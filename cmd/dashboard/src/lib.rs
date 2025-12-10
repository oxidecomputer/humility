// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility dashboard`
//!
//! Provides a captive dashboard that graphs sensor values over time.  (The
//! `sensor` task is required for operation; see the documentation for
//! `humility sensors` for more details.)
//!
//! If `-o` is provided, it specifies an output file for any raw sensor data
//! graphed by the dashboard.
//!

use anyhow::{Result, anyhow, bail};
use clap::{CommandFactory, Parser};
use crossterm::{
    event::{
        self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode,
        KeyModifiers,
    },
    execute,
    terminal::{
        EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode,
        enable_raw_mode,
    },
};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility::reflect::{self, Load};
use humility_cli::{ExecutionContext, Subcommand};
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};
use humility_doppel as doppel;
use humility_hiffy::*;
use humility_idol::{self as idol, HubrisIdol};
use ratatui::{
    Frame, Terminal,
    backend::{Backend, CrosstermBackend},
    layout::{Alignment, Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    symbols,
    text::{Line, Span},
    widgets::{
        Axis, Block, Borders, Chart, Dataset, List, ListItem, ListState,
        Paragraph,
    },
};
use std::fs::File;
use std::io;
use std::io::Write;
use std::time::{Duration, Instant};

#[derive(Parser, Debug)]
#[clap(name = "dashboard", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DashboardArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// CSV output file
    #[clap(long, short)]
    output: Option<String>,

    /// use the given backend to read dashboard data
    ///
    /// By default, this will try to make sensible choices:
    /// - On live systems, it tries to use `hiffy`, falling back to `readmem` if
    ///   the `hiffy`-based backend fails (e.g. on network-connected targets
    ///   without the `udprpc` task)
    /// - On dumps, it will use the `readmem` backend
    #[clap(long, short, value_enum)]
    backend: Option<DashboardBackend>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, clap::ValueEnum)]
#[clap(rename_all = "kebab-case")]
enum DashboardBackend {
    /// Use the Hiffy task to read dashboard data
    Hiffy,
    /// Use the RAM-reading primitive to read dashboard data
    Readmem,
}

struct StatefulList {
    state: ListState,
    n: usize,
}

impl StatefulList {
    fn next(&mut self) {
        self.state.select(match self.state.selected() {
            Some(ndx) => Some((ndx + 1) % self.n),
            None => Some(0),
        });
    }

    fn previous(&mut self) {
        self.state.select(match self.state.selected() {
            Some(0) => Some(self.n - 1),
            Some(ndx) => Some(ndx - 1),
            None => Some(0),
        });
    }

    fn unselect(&mut self) {
        self.state.select(None);
    }
}

struct Series {
    name: String,
    color: Color,
    data: Vec<(f64, f64)>,
    raw: Vec<Option<f32>>,
}

trait Attributes {
    fn label(&self) -> &'static str;
    fn legend_label(&self) -> &'static str;
    fn x_axis_label(&self) -> &'static str {
        "Time"
    }
    fn y_axis_label(&self) -> &'static str;
    fn axis_value(&self, val: f64) -> String;
    fn legend_value(&self, val: f64) -> String;

    fn increase(&mut self, _ndx: usize) -> Option<u8> {
        None
    }

    fn decrease(&mut self, _ndx: usize) -> Option<u8> {
        None
    }
}

struct TempGraph;

impl Attributes for TempGraph {
    fn label(&self) -> &'static str {
        "Temperature"
    }
    fn legend_label(&self) -> &'static str {
        "Sensors"
    }

    fn y_axis_label(&self) -> &'static str {
        "Degrees Celsius"
    }

    fn axis_value(&self, val: f64) -> String {
        format!("{:2.0}°", val)
    }

    fn legend_value(&self, val: f64) -> String {
        format!("{:4.2}°", val)
    }
}

struct FanGraph(Vec<u8>);

impl FanGraph {
    fn new(len: usize) -> Self {
        let v = vec![0; len];
        FanGraph(v)
    }
}

impl Attributes for FanGraph {
    fn label(&self) -> &'static str {
        "Fan speed"
    }
    fn legend_label(&self) -> &'static str {
        "Fans"
    }

    fn y_axis_label(&self) -> &'static str {
        "RPM"
    }

    fn axis_value(&self, val: f64) -> String {
        format!("{:3.1}K", val / 1000.0)
    }

    fn legend_value(&self, val: f64) -> String {
        format!("{:.0}", val)
    }

    fn increase(&mut self, ndx: usize) -> Option<u8> {
        let current = self.0[ndx];
        let nval = current + 20;

        self.0[ndx] = if nval <= 100 { nval } else { 100 };
        Some(self.0[ndx])
    }

    fn decrease(&mut self, ndx: usize) -> Option<u8> {
        let current = self.0[ndx];
        let nval = current as i8 - 20;

        self.0[ndx] = if nval >= 0 { nval as u8 } else { 0 };
        Some(self.0[ndx])
    }
}

struct CurrentGraph;

impl Attributes for CurrentGraph {
    fn label(&self) -> &'static str {
        "Output current"
    }
    fn legend_label(&self) -> &'static str {
        "Regulators"
    }

    fn y_axis_label(&self) -> &'static str {
        "Amperes"
    }

    fn axis_value(&self, val: f64) -> String {
        format!("{:2.2}A", val)
    }

    fn legend_value(&self, val: f64) -> String {
        format!("{:3.2}A", val)
    }
}

struct Graph {
    series: Vec<Series>,
    legend: StatefulList,
    time: usize,
    width: usize,
    interpolate: usize,
    bounds: [f64; 2],
    attributes: Box<dyn Attributes>,
}

impl Graph {
    fn new(all: &[String], attr: Box<dyn Attributes>) -> Result<Self> {
        let mut series = vec![];

        let colors = [
            Color::Yellow,
            Color::Green,
            Color::Magenta,
            Color::White,
            Color::Red,
            Color::LightRed,
            Color::Blue,
            Color::LightMagenta,
            Color::LightYellow,
            Color::LightCyan,
            Color::LightGreen,
            Color::LightBlue,
            Color::LightRed,
        ];

        for (ndx, s) in all.iter().enumerate() {
            series.push(Series {
                name: s.to_string(),
                color: colors[ndx % colors.len()],
                data: Vec::new(),
                raw: Vec::new(),
            })
        }

        Ok(Graph {
            series,
            legend: StatefulList { state: ListState::default(), n: all.len() },
            time: 0,
            width: 600,
            interpolate: 0,
            bounds: [20.0, 120.0],
            attributes: attr,
        })
    }

    fn data(&mut self, data: &[Option<f32>]) {
        for (ndx, s) in self.series.iter_mut().enumerate() {
            s.raw.push(data[ndx]);
        }

        self.time += 1;
    }

    fn update_data(&mut self) {
        for s in &mut self.series {
            s.data = Vec::new();
        }

        for i in 0..self.width {
            if self.time < self.width - i {
                continue;
            }

            let offs = self.time - (self.width - i);

            for (_ndx, s) in &mut self.series.iter_mut().enumerate() {
                if let Some(datum) = s.raw[offs] {
                    let point = (i as f64, datum as f64);

                    if self.interpolate != 0
                        && let Some(last) = s.data.last()
                    {
                        let x_delta = point.0 - last.0;
                        let slope = (point.1 - last.1) / x_delta;
                        let x_inc = x_delta / self.interpolate as f64;

                        for x in 0..self.interpolate {
                            s.data.push((
                                point.0 + x as f64 * x_inc,
                                point.1 + (slope * x_inc),
                            ));
                        }
                    }

                    s.data.push((i as f64, datum as f64));
                }
            }
        }

        self.update_bounds();
    }

    fn update_bounds(&mut self) {
        let selected = self.legend.state.selected();
        let mut min = None;
        let mut max = None;

        for (ndx, s) in self.series.iter().enumerate() {
            if let Some(selected) = selected
                && ndx != selected
            {
                continue;
            }

            for (_, datum) in &s.data {
                min = match min {
                    Some(min) if datum < min => Some(datum),
                    None => Some(datum),
                    _ => min,
                };

                max = match max {
                    Some(max) if datum > max => Some(datum),
                    None => Some(datum),
                    _ => max,
                };
            }
        }

        if let Some(min) = min {
            self.bounds[0] = ((min * 0.85) / 2.0) * 2.0;
        }

        if self.bounds[0] < 0.0 {
            self.bounds[0] = 0.0;
        }

        if let Some(max) = max {
            self.bounds[1] = ((max * 1.15) / 2.0) * 2.0;
        }
    }

    fn previous(&mut self) {
        self.legend.previous();
    }

    fn next(&mut self) {
        self.legend.next();
    }

    fn unselect(&mut self) {
        self.legend.unselect();
    }

    fn set_interpolate(&mut self) {
        let interpolate = (1000.0 - self.width as f64) / self.width as f64;

        if interpolate >= 1.0 {
            self.interpolate = interpolate as usize;
        } else {
            self.interpolate = 0;
        }
    }

    fn zoom_in(&mut self) {
        self.width = (self.width as f64 * 0.8) as usize;
        self.set_interpolate();
    }

    fn zoom_out(&mut self) {
        self.width = (self.width as f64 * 1.25) as usize;
        self.set_interpolate();
    }
}

struct DashboardData {
    /// Power state string, or None if unavailable
    power_state: Option<String>,
    /// Sensor readings (temperature, fans, current, etc.)
    sensor_values: Vec<Option<f32>>,
}

trait DashboardReader {
    /// Reads dashboard data (power state and sensors)
    fn read(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<DashboardData>;

    /// Returns true if this reader can control power state
    fn can_control_power(&self) -> bool;

    /// Sets the power state (only works if can_control_power() returns true)
    fn set_power_state(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        state: &str,
    ) -> Result<()>;

    /// Controls fans (only works if can_control_power() returns true)
    fn set_fans(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        on: bool,
    ) -> Result<()>;

    /// Sets fan PWM (only works if can_control_power() returns true)
    fn set_fan_pwm(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        index: usize,
        pwm: u8,
    ) -> Result<()>;
}

struct HiffyDashboardReader<'a> {
    context: HiffyContext<'a>,
    ops: Vec<Op>,
    status_op: idol::IdolOperation<'a>,
    num_status_items: usize,
}

impl<'a> HiffyDashboardReader<'a> {
    fn new(
        hubris: &'a HubrisArchive,
        mut context: HiffyContext<'a>,
    ) -> Result<Self> {
        let mut ops = vec![];

        let status_op = sequencer_state_ops(hubris, &mut context, &mut ops)?;

        sensor_ops(hubris, &mut context, &mut ops, |s| {
            s.kind == HubrisSensorKind::Temperature
        })?;

        sensor_ops(hubris, &mut context, &mut ops, |s| {
            s.kind == HubrisSensorKind::Speed
        })?;

        sensor_ops(hubris, &mut context, &mut ops, |s| {
            s.kind == HubrisSensorKind::Current
        })?;

        ops.push(Op::Done);

        Ok(Self {
            context,
            ops,
            status_op,
            num_status_items: 1,
        })
    }

    fn deserialize_status(&self, hubris: &HubrisArchive, val: &[u8]) -> Result<String> {
        use humility::reflect::Format;

        let ty = hubris.lookup_type(self.status_op.ok).unwrap();
        let v = match self.status_op.operation.encoding {
            ::idol::syntax::Encoding::Zerocopy => {
                humility::reflect::load_value(hubris, val, ty, 0)?
            }
            ::idol::syntax::Encoding::Ssmarshal
            | ::idol::syntax::Encoding::Hubpack => {
                humility::reflect::deserialize_value(hubris, val, ty)?.0
            }
        };

        let fmt = HubrisPrintFormat {
            newline: false,
            hex: true,
            ..HubrisPrintFormat::default()
        };

        let mut dumped = vec![];
        v.format(hubris, fmt, &mut dumped)?;
        Ok(std::str::from_utf8(&dumped)?.to_string())
    }
}

impl<'a> DashboardReader for HiffyDashboardReader<'a> {
    fn read(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<DashboardData> {
        let results = self.context.run(core, self.ops.as_slice(), None)?;

        let power_state = if let Ok(val) = &results[0] {
            Some(self.deserialize_status(hubris, val)?)
        } else {
            None
        };

        let mut sensor_values = vec![];
        for r in &results[self.num_status_items..] {
            sensor_values.push(if let Ok(val) = r {
                Some(f32::from_le_bytes(val[0..4].try_into()?))
            } else {
                None
            });
        }

        Ok(DashboardData { power_state, sensor_values })
    }

    fn can_control_power(&self) -> bool {
        true
    }

    fn set_power_state(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        state: &str,
    ) -> Result<()> {
        let ops = power_ops(hubris, &mut self.context, state)?;
        let _results = self.context.run(core, ops.as_slice(), None)?;
        Ok(())
    }

    fn set_fans(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        on: bool,
    ) -> Result<()> {
        let ops = fan_ops(hubris, &mut self.context, on)?;
        let _results = self.context.run(core, ops.as_slice(), None)?;
        Ok(())
    }

    fn set_fan_pwm(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
        index: usize,
        pwm: u8,
    ) -> Result<()> {
        let ops = pwm_ops(hubris, &mut self.context, index, pwm)?;
        let _results = self.context.run(core, ops.as_slice(), None)?;
        Ok(())
    }
}

#[derive(Copy, Clone, reflect::Load)]
enum LastReading {
    DataOnly,
    ErrorOnly,
    Data,
    Error,
}

struct RamDashboardReader {
    /// Position of `LAST_READING` in global RAM
    last_reading: HubrisVariable,
    last_reading_buf: Vec<u8>,

    /// Position of `DATA_VALUE` in global RAM
    data_value: HubrisVariable,
    data_value_buf: Vec<u8>,

    /// Indexes of sensors that we care about
    sensor_indices: Vec<usize>,
}

impl RamDashboardReader {
    fn new(
        hubris: &HubrisArchive,
        temps: &[String],
        fans: &[String],
        current: &[String],
    ) -> Result<Self> {
        let find_var = |name| {
            hubris
                .qualified_variables()
                .find(|&(n, _)| n == format!("task_sensor::main::{name}"))
                .ok_or_else(|| anyhow!("could not find {name}"))
                .map(|(_, v)| *v)
        };

        let last_reading = find_var("LAST_READING")?;
        let data_value = find_var("DATA_VALUE")?;

        // Collect sensor indices in the order: temps, fans, current
        let mut sensor_indices = vec![];

        for temp_name in temps {
            if let Some((i, _)) = hubris.manifest.sensors.iter().enumerate()
                .find(|(_, s)| &s.name == temp_name && s.kind == HubrisSensorKind::Temperature)
            {
                sensor_indices.push(i);
            }
        }

        for fan_name in fans {
            if let Some((i, _)) = hubris.manifest.sensors.iter().enumerate()
                .find(|(_, s)| &s.name == fan_name && s.kind == HubrisSensorKind::Speed)
            {
                sensor_indices.push(i);
            }
        }

        for current_name in current {
            if let Some((i, _)) = hubris.manifest.sensors.iter().enumerate()
                .find(|(_, s)| &s.name == current_name && s.kind == HubrisSensorKind::Current)
            {
                sensor_indices.push(i);
            }
        }

        Ok(Self {
            last_reading,
            last_reading_buf: vec![0u8; last_reading.size],
            data_value,
            data_value_buf: vec![0u8; data_value.size],
            sensor_indices,
        })
    }

    fn unpack_array<T: Load + Copy>(
        &self,
        hubris: &HubrisArchive,
        buf: &[u8],
        var: HubrisVariable,
    ) -> Result<Vec<T>> {
        let v =
            reflect::load_value(hubris, buf, hubris.lookup_type(var.goff)?, 0)?;
        let out = doppel::MaybeUninit::<Vec<T>>::from_value(&v)?;
        Ok(self.sensor_indices.iter().map(|i| out.value[*i]).collect())
    }
}

impl DashboardReader for RamDashboardReader {
    fn read(
        &mut self,
        hubris: &HubrisArchive,
        core: &mut dyn Core,
    ) -> Result<DashboardData> {
        core.read_8(self.data_value.addr, &mut self.data_value_buf)?;
        core.read_8(self.last_reading.addr, &mut self.last_reading_buf)?;

        let data_values = self.unpack_array::<f32>(
            hubris,
            &self.data_value_buf,
            self.data_value,
        )?;

        let last_reading = self.unpack_array::<Option<LastReading>>(
            hubris,
            &self.last_reading_buf,
            self.last_reading,
        )?;

        // Clear data values that aren't valid (according to `last_reading`)
        let sensor_values = data_values
            .into_iter()
            .zip(last_reading)
            .map(|(data, valid)| match valid {
                Some(LastReading::Data | LastReading::DataOnly) => Some(data),
                _ => None,
            })
            .collect::<Vec<_>>();

        // Power state is unknown when using RAM reader
        Ok(DashboardData {
            power_state: None,
            sensor_values,
        })
    }

    fn can_control_power(&self) -> bool {
        false
    }

    fn set_power_state(
        &mut self,
        _hubris: &HubrisArchive,
        _core: &mut dyn Core,
        _state: &str,
    ) -> Result<()> {
        bail!("Cannot control power state with readmem backend")
    }

    fn set_fans(
        &mut self,
        _hubris: &HubrisArchive,
        _core: &mut dyn Core,
        _on: bool,
    ) -> Result<()> {
        bail!("Cannot control fans with readmem backend")
    }

    fn set_fan_pwm(
        &mut self,
        _hubris: &HubrisArchive,
        _core: &mut dyn Core,
        _index: usize,
        _pwm: u8,
    ) -> Result<()> {
        bail!("Cannot control fan PWM with readmem backend")
    }
}

struct Dashboard<'a> {
    hubris: &'a HubrisArchive,
    reader: Box<dyn DashboardReader + 'a>,
    graphs: Vec<Graph>,
    current: usize,
    last: Instant,
    interval: u32,
    status: String,
    output: Option<File>,
}

impl<'a> Dashboard<'a> {
    fn new(
        hubris: &'a HubrisArchive,
        core: &mut dyn Core,
        subargs: &DashboardArgs,
    ) -> Result<Dashboard<'a>> {
        // Collect sensor names for each category
        let mut temps = vec![];
        let mut fans = vec![];
        let mut current_sensors = vec![];

        for s in &hubris.manifest.sensors {
            match s.kind {
                HubrisSensorKind::Temperature => temps.push(s.name.clone()),
                HubrisSensorKind::Speed => fans.push(s.name.clone()),
                HubrisSensorKind::Current => current_sensors.push(s.name.clone()),
                _ => {}
            }
        }

        // Create the appropriate reader based on backend option
        let reader: Box<dyn DashboardReader> = match subargs.backend {
            Some(DashboardBackend::Hiffy) => {
                let context = HiffyContext::new(hubris, core, subargs.timeout)?;
                Box::new(HiffyDashboardReader::new(hubris, context)?)
            }
            Some(DashboardBackend::Readmem) => {
                Box::new(RamDashboardReader::new(
                    hubris,
                    &temps,
                    &fans,
                    &current_sensors,
                )?)
            }
            None => {
                // Try hiffy first, fall back to readmem
                match HiffyContext::new(hubris, core, subargs.timeout) {
                    Ok(context) => match HiffyDashboardReader::new(hubris, context) {
                        Ok(r) => Box::new(r),
                        Err(_) => Box::new(RamDashboardReader::new(
                            hubris,
                            &temps,
                            &fans,
                            &current_sensors,
                        )?),
                    },
                    Err(_) => Box::new(RamDashboardReader::new(
                        hubris,
                        &temps,
                        &fans,
                        &current_sensors,
                    )?),
                }
            }
        };

        let output = if let Some(output) = &subargs.output {
            let mut f = File::create(output)?;
            writeln!(
                &mut f,
                "Time,{},{},{},Power",
                temps.join(","),
                fans.join(","),
                current_sensors.join(",")
            )?;
            Some(f)
        } else {
            None
        };

        let graphs = vec![
            Graph::new(&temps, Box::new(TempGraph))?,
            Graph::new(&fans, Box::new(FanGraph::new(fans.len())))?,
            Graph::new(&current_sensors, Box::new(CurrentGraph))?,
        ];

        Ok(Dashboard {
            hubris,
            reader,
            graphs,
            current: 0,
            last: Instant::now(),
            interval: 1000,
            status: "".to_string(),
            output,
        })
    }

    fn status(&self) -> Vec<(&str, &str)> {
        vec![("Power state", &self.status)]
    }

    fn need_update(&mut self, core: &mut dyn Core) -> Result<bool> {
        if self.last.elapsed().as_millis() > self.interval.into() {
            let data = self.reader.read(self.hubris, core)?;

            self.status = data.power_state.unwrap_or_else(|| "unknown".to_string());

            let mut offs = 0;
            for graph in self.graphs.iter_mut() {
                graph.data(&data.sensor_values[offs..]);
                offs += graph.series.len();
            }

            if let Some(output) = &mut self.output {
                use std::time::SystemTime;

                let now = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)?;
                write!(output, "{},", now.as_secs())?;

                for val in &data.sensor_values {
                    if let Some(val) = val {
                        write!(output, "{:.2},", val)?;
                    } else {
                        write!(output, ",")?;
                    }
                }
                writeln!(output, "{}", self.status)?;
            }

            self.last = Instant::now();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn update_data(&mut self) {
        for graph in self.graphs.iter_mut() {
            graph.update_data();
        }
    }

    fn up(&mut self) {
        self.graphs[self.current].previous();
    }

    fn down(&mut self) {
        self.graphs[self.current].next();
    }

    fn esc(&mut self) {
        self.graphs[self.current].unselect();
    }

    fn tab(&mut self) {
        self.current = (self.current + 1) % self.graphs.len();
    }

    fn increase(&mut self, core: &mut dyn Core) {
        let graph = &mut self.graphs[self.current];

        if let Some(selected) = graph.legend.state.selected()
            && let Some(pwm) = graph.attributes.increase(selected)
        {
            self.fan_to(core, selected, pwm).unwrap();
        }
    }

    fn decrease(&mut self, core: &mut dyn Core) {
        let graph = &mut self.graphs[self.current];

        if let Some(selected) = graph.legend.state.selected()
            && let Some(pwm) = graph.attributes.decrease(selected)
        {
            self.fan_to(core, selected, pwm).unwrap();
        }
    }

    fn enter(&mut self) {}

    fn set_a0(&mut self, core: &mut dyn Core) -> Result<()> {
        if !self.reader.can_control_power() {
            return Ok(());
        }
        self.reader.set_power_state(self.hubris, core, "A0")?;
        Ok(())
    }

    fn set_a2(&mut self, core: &mut dyn Core) -> Result<()> {
        if !self.reader.can_control_power() {
            return Ok(());
        }
        self.reader.set_power_state(self.hubris, core, "A2")?;
        Ok(())
    }

    fn fans_on(&mut self, core: &mut dyn Core) -> Result<()> {
        if !self.reader.can_control_power() {
            return Ok(());
        }
        self.reader.set_fans(self.hubris, core, true)?;
        Ok(())
    }

    fn fans_off(&mut self, core: &mut dyn Core) -> Result<()> {
        if !self.reader.can_control_power() {
            return Ok(());
        }
        self.reader.set_fans(self.hubris, core, false)?;
        Ok(())
    }

    fn fan_to(
        &mut self,
        core: &mut dyn Core,
        index: usize,
        pwm: u8,
    ) -> Result<()> {
        if !self.reader.can_control_power() {
            return Ok(());
        }
        self.reader.set_fan_pwm(self.hubris, core, index, pwm)?;
        Ok(())
    }

    fn zoom_in(&mut self) {
        for graph in self.graphs.iter_mut() {
            graph.zoom_in();
        }
    }

    fn zoom_out(&mut self) {
        for graph in self.graphs.iter_mut() {
            graph.zoom_out();
        }
    }
}

fn run_dashboard<B: Backend>(
    terminal: &mut Terminal<B>,
    mut dashboard: Dashboard,
    core: &mut dyn Core,
) -> Result<()> {
    let mut last_tick = Instant::now();
    let tick_rate = Duration::from_millis(100);

    loop {
        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        let update = if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                match key.code {
                    KeyCode::Char('q') => return Ok(()),
                    KeyCode::Char('2') => dashboard.set_a2(core)?,
                    KeyCode::Char('0') => dashboard.set_a0(core)?,
                    KeyCode::Char('F') => dashboard.fans_on(core)?,
                    KeyCode::Char('f') => dashboard.fans_off(core)?,
                    KeyCode::Char('+') => dashboard.zoom_in(),
                    KeyCode::Char('-') => dashboard.zoom_out(),
                    KeyCode::Char('>') => dashboard.increase(core),
                    KeyCode::Char('<') => dashboard.decrease(core),
                    KeyCode::Char('l') => {
                        //
                        // ^L -- form feed -- is historically used to clear and
                        // redraw the screen.  And, notably, it is what dtach(1)
                        // will send when attaching to a dashboard.  If we
                        // see ^L, clear the terminal to force a total redraw.
                        //
                        if key.modifiers == KeyModifiers::CONTROL {
                            terminal.clear()?;
                        }
                    }
                    KeyCode::Up => dashboard.up(),
                    KeyCode::Down => dashboard.down(),
                    KeyCode::Esc => dashboard.esc(),
                    KeyCode::Tab => dashboard.tab(),
                    KeyCode::Enter => dashboard.enter(),
                    _ => {}
                }
            }
            true
        } else {
            dashboard.need_update(core)?
        };

        if update {
            dashboard.update_data();
            terminal.draw(|f| draw(f, &mut dashboard))?;
        }

        last_tick = Instant::now();
    }
}

fn dashboard(context: &mut ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let hubris = context.archive.as_ref().unwrap();

    let core = &mut **context.core.as_mut().unwrap();

    let subargs = DashboardArgs::try_parse_from(subargs)?;
    let dashboard = Dashboard::new(hubris, core, &subargs)?;

    // setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let res = run_dashboard(&mut terminal, dashboard, core);

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    res?;

    Ok(())
}

pub fn init() -> Command {
    Command {
        app: DashboardArgs::command(),
        name: "dashboard",
        run: dashboard,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::Any,
            validate: Validate::Booted,
        },
    }
}

fn sensor_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    ops: &mut Vec<Op>,
    capture: impl Fn(&HubrisSensor) -> bool,
) -> Result<Vec<String>> {
    let mut sensors = vec![];
    let op = hubris.get_idol_command("Sensor.get")?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if (ok.encoding, ok.size) != (HubrisEncoding::Float, 4) {
        bail!("expected return value of Sensor.get() to be a float");
    }

    for (i, s) in hubris.manifest.sensors.iter().enumerate() {
        if !capture(s) {
            continue;
        }

        let payload =
            op.payload(&[("id", idol::IdolArgument::Scalar(i as u64))])?;
        context.idol_call_ops(&op, &payload, ops)?;
        sensors.push(s.name.clone());
    }

    Ok(sensors)
}

fn sequencer_state_ops<'a>(
    hubris: &'a HubrisArchive,
    context: &mut HiffyContext,
    ops: &mut Vec<Op>,
) -> Result<idol::IdolOperation<'a>> {
    // Sidecar and Gimlet have different names for this operation -- and the
    // PSC doesn't have a sequencer at all, so fall back on Jefe.get_state()
    // if we can't find either of their power state entry points.
    let op =
        ["Sequencer.tofino_seq_state", "Sequencer.get_state", "Jefe.get_state"]
            .iter()
            .map(|name| hubris.get_idol_command(name))
            .find_map(Result::ok)
            .ok_or_else(|| {
                anyhow!(
                    "Could not find Sequencer.get_state or \
                 Sequencer.tofino_seq_state"
                )
            })?;

    context.idol_call_ops(&op, &[], ops)?;
    Ok(op)
}

fn power_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    state: &str,
) -> Result<Vec<Op>> {
    let mut ops = vec![];

    if let Ok(op) = hubris.get_idol_command("Sequencer.set_tofino_seq_policy") {
        // Translate from Gimlet-style power states to Tofino-style policies
        let policy = match state {
            "A0" => {
                // If we're trying to go to A0, then _also_ clear Tofino faults
                // before changing the power policy.
                let op = hubris
                    .get_idol_command("Sequencer.clear_tofino_seq_error")?;
                let payload = op.payload(&[])?;
                context.idol_call_ops(&op, &payload, &mut ops)?;

                // Then, return the policy which will get us to power up
                "LatchOffOnFault"
            }
            "A2" => "Disabled",
            state => bail!("Unknown state {state}"),
        };

        context.idol_call_ops(
            &op,
            &op.payload(&[("policy", idol::IdolArgument::String(policy))])?,
            &mut ops,
        )?;
    } else if let Ok(op) = hubris.get_idol_command("Sequencer.set_state") {
        context.idol_call_ops(
            &op,
            &op.payload(&[("state", idol::IdolArgument::String(state))])?,
            &mut ops,
        )?;
    } else {
        bail!(
            "Could not find Sequencer.set_state or Sequencer.set_tofino_seq_policy"
        );
    };
    ops.push(Op::Done);

    Ok(ops)
}

fn fan_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    on: bool,
) -> Result<Vec<Op>> {
    let mut ops = vec![];
    let op = hubris.get_idol_command(if on {
        "Sequencer.fans_on"
    } else {
        "Sequencer.fans_off"
    })?;

    let payload = vec![];
    context.idol_call_ops(&op, &payload, &mut ops)?;
    ops.push(Op::Done);

    Ok(ops)
}

fn pwm_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    index: usize,
    pwm: u8,
) -> Result<Vec<Op>> {
    let mut ops = vec![];
    let op = hubris.get_idol_command("Thermal.set_fan_pwm")?;

    let payload = op.payload(&[
        ("index", idol::IdolArgument::Scalar(index as u64)),
        ("pwm", idol::IdolArgument::Scalar(pwm as u64)),
    ])?;

    context.idol_call_ops(&op, &payload, &mut ops)?;
    ops.push(Op::Done);

    Ok(ops)
}

fn draw_graph<B: Backend>(f: &mut Frame<B>, parent: Rect, graph: &mut Graph) {
    //
    // We want the right panel to be 30 characters wide (a left-justified 20
    // and a right justified 8 + margins), but we don't want it to consume
    // more than 80%; calculate accordingly.
    //
    let r = std::cmp::min((30u16 * 100).div_ceil(parent.width), 80);

    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [Constraint::Percentage(100 - r), Constraint::Percentage(r)]
                .as_ref(),
        )
        .split(parent);

    let x_labels = vec![
        Span::styled(
            format!("t-{}", graph.width),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        Span::styled(
            format!("t-{}", 1),
            Style::default().add_modifier(Modifier::BOLD),
        ),
    ];

    let mut datasets = vec![];
    let selected = graph.legend.state.selected();

    for (ndx, s) in graph.series.iter().enumerate() {
        if let Some(selected) = selected
            && ndx != selected
        {
            continue;
        }

        datasets.push(
            Dataset::default()
                .name(&s.name)
                .marker(symbols::Marker::Braille)
                .style(Style::default().fg(s.color))
                .data(&s.data),
        );
    }

    let chart = Chart::new(datasets)
        .block(
            Block::default()
                .title(Span::styled(
                    graph.attributes.label(),
                    Style::default()
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::BOLD),
                ))
                .borders(Borders::ALL),
        )
        .x_axis(
            Axis::default()
                .title(graph.attributes.x_axis_label())
                .style(Style::default().fg(Color::Gray))
                .labels(x_labels)
                .bounds([0.0, graph.width as f64]),
        )
        .y_axis(
            Axis::default()
                .title(graph.attributes.y_axis_label())
                .style(Style::default().fg(Color::Gray))
                .labels(vec![
                    Span::styled(
                        graph.attributes.axis_value(graph.bounds[0]),
                        Style::default().add_modifier(Modifier::BOLD),
                    ),
                    Span::styled(
                        graph.attributes.axis_value(graph.bounds[1]),
                        Style::default().add_modifier(Modifier::BOLD),
                    ),
                ])
                .bounds(graph.bounds),
        );

    f.render_widget(chart, chunks[0]);

    let mut rows = vec![];

    for s in &graph.series {
        let val = match s.raw.last() {
            None | Some(None) => "-".to_string(),
            Some(Some(val)) => graph.attributes.legend_value((*val).into()),
        };

        rows.push(ListItem::new(Line::from(vec![
            Span::styled(
                format!("{:<20}", s.name),
                Style::default().fg(s.color),
            ),
            Span::styled(format!("{:>8}", val), Style::default().fg(s.color)),
        ])));
    }

    let list = List::new(rows)
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(graph.attributes.legend_label()),
        )
        .highlight_style(
            Style::default()
                .bg(Color::LightGreen)
                .fg(Color::Black)
                .add_modifier(Modifier::BOLD),
        );

    // We can now render the item list
    f.render_stateful_widget(list, chunks[1], &mut graph.legend.state);
}

fn draw_graphs<B: Backend>(
    f: &mut Frame<B>,
    parent: Rect,
    dashboard: &mut Dashboard,
) {
    let screen = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Ratio(1, 2),
                Constraint::Ratio(1, 4),
                Constraint::Ratio(1, 4),
            ]
            .as_ref(),
        )
        .split(parent);

    draw_graph(f, screen[0], &mut dashboard.graphs[0]);
    draw_graph(f, screen[1], &mut dashboard.graphs[1]);
    draw_graph(f, screen[2], &mut dashboard.graphs[2]);
}

fn draw_status<B: Backend>(
    f: &mut Frame<B>,
    parent: Rect,
    status: &[(&str, &str)],
) {
    let mut bar = vec![];

    for i in 0..status.len() {
        let s = &status[i];

        bar.push(Span::styled(
            s.0,
            Style::default().add_modifier(Modifier::BOLD),
        ));

        bar.push(Span::styled(
            ": ",
            Style::default().add_modifier(Modifier::BOLD),
        ));

        bar.push(Span::raw(s.1));

        if i < status.len() - 1 {
            bar.push(Span::raw(" | "));
        }
    }

    let text = vec![Line::from(bar)];

    let para = Paragraph::new(text)
        .alignment(Alignment::Right)
        .style(Style::default().fg(Color::White).bg(Color::Black));

    f.render_widget(para, parent);
}

fn draw<B: Backend>(f: &mut Frame<B>, dashboard: &mut Dashboard) {
    let size = f.size();

    let screen = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(1), Constraint::Length(1)].as_ref())
        .split(size);

    draw_graphs(f, screen[0], dashboard);
    draw_status(f, screen[1], &dashboard.status());
}
