// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{
        disable_raw_mode, enable_raw_mode, EnterAlternateScreen,
        LeaveAlternateScreen,
    },
};
use hif::*;
use humility::core::Core;
use humility::hubris::*;
use humility_cmd::hiffy::*;
use humility_cmd::idol;
use humility_cmd::{Archive, Args, Attach, Command, Validate};
use std::fs::File;
use std::io;
use std::io::Write;
use std::time::{Duration, Instant};
use structopt::clap::App;
use structopt::StructOpt;
use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color, Modifier, Style},
    symbols,
    text::{Span, Spans},
    widgets::{
        Axis, Block, Borders, Chart, Dataset, List, ListItem, ListState,
    },
    Frame, Terminal,
};

#[derive(StructOpt, Debug)]
#[structopt(name = "dashboard", about = env!("CARGO_PKG_DESCRIPTION"))]
struct DashboardArgs {
    /// sets timeout
    #[structopt(
        long, short = "T", default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// CSV output file
    #[structopt(long, short)]
    output: Option<String>,
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
            Some(ndx) if ndx == 0 => Some(self.n - 1),
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

struct Graph {
    series: Vec<Series>,
    legend: StatefulList,
    time: usize,
    width: usize,
    interpolate: usize,
    bounds: [f64; 2],
}

impl Graph {
    fn new(all: &[String]) -> Result<Self> {
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
        })
    }

    fn data(&mut self, data: &[Option<f32>]) {
        for (ndx, r) in data.iter().enumerate() {
            self.series[ndx].raw.push(*r);
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

            let offs = (self.time - (self.width - i)) as usize;

            for (_ndx, s) in &mut self.series.iter_mut().enumerate() {
                if let Some(datum) = s.raw[offs] {
                    let point = (i as f64, datum as f64);

                    if self.interpolate != 0 {
                        if let Some(last) = s.data.last() {
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
            if let Some(selected) = selected {
                if ndx != selected {
                    continue;
                }
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

struct Dashboard<'a> {
    hubris: &'a HubrisArchive,
    context: HiffyContext<'a>,
    ops: Vec<Op>,
    temps: Graph,
    work: Vec<Vec<Op>>,
    last: Instant,
    interval: u32,
    outstanding: bool,
    output: Option<File>,
}

impl<'a> Dashboard<'a> {
    fn new(
        hubris: &'a HubrisArchive,
        core: &mut dyn Core,
        subargs: &DashboardArgs,
    ) -> Result<Dashboard<'a>> {
        let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;

        let (ops, all) = thermal_ops(hubris, &mut context)?;

        context.start(core, ops.as_slice(), None)?;

        let output = if let Some(output) = &subargs.output {
            let mut f = File::create(output)?;
            writeln!(&mut f, "{}", all.join(","))?;
            Some(f)
        } else {
            None
        };

        Ok(Dashboard {
            hubris,
            context,
            ops,
            temps: Graph::new(&all)?,
            outstanding: true,
            last: Instant::now(),
            interval: 1000,
            work: Vec::new(),
            output,
        })
    }

    fn dequeue_work(&mut self, core: &mut dyn Core) -> Result<()> {
        for w in &self.work {
            let _results = self.context.run(core, w.as_slice(), None)?;
        }

        self.work = vec![];
        Ok(())
    }

    fn enqueue_work(
        &mut self,
        core: &mut dyn Core,
        ops: Vec<Op>,
    ) -> Result<()> {
        if self.outstanding {
            self.work.push(ops);
            Ok(())
        } else {
            let _results = self.context.run(core, ops.as_slice(), None)?;
            Ok(())
        }
    }

    fn need_update(&mut self, core: &mut dyn Core) -> Result<bool> {
        if self.outstanding {
            if self.context.done(core)? {
                let results = self.context.results(core)?;
                let mut raw = vec![];

                for r in &results {
                    raw.push(if let Ok(val) = r {
                        Some(f32::from_le_bytes(val[0..4].try_into()?))
                    } else {
                        None
                    });
                }

                self.temps.data(&raw);

                if let Some(output) = &mut self.output {
                    for val in raw {
                        if let Some(val) = val {
                            write!(output, "{:.2},", val)?;
                        } else {
                            write!(output, ",")?;
                        }
                    }
                    writeln!(output)?;
                }

                self.outstanding = false;
                self.dequeue_work(core)?;
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            if self.last.elapsed().as_millis() > self.interval.into() {
                self.context.start(core, self.ops.as_slice(), None)?;
                self.last = Instant::now();
                self.outstanding = true;
            }

            Ok(false)
        }
    }

    fn update_data(&mut self) {
        self.temps.update_data();
    }

    fn up(&mut self) {
        self.temps.previous();
    }

    fn down(&mut self) {
        self.temps.next();
    }

    fn esc(&mut self) {
        self.temps.unselect();
    }

    fn enter(&mut self) {}

    fn set_a0(&mut self, core: &mut dyn Core) -> Result<()> {
        let ops = power_ops(self.hubris, &mut self.context, "A0")?;
        self.enqueue_work(core, ops)?;
        Ok(())
    }

    fn set_a2(&mut self, core: &mut dyn Core) -> Result<()> {
        let ops = power_ops(self.hubris, &mut self.context, "A2")?;
        self.enqueue_work(core, ops)?;
        Ok(())
    }

    fn fans_on(&mut self, core: &mut dyn Core) -> Result<()> {
        let ops = fan_ops(self.hubris, &mut self.context, true)?;
        self.enqueue_work(core, ops)?;
        Ok(())
    }

    fn fans_off(&mut self, core: &mut dyn Core) -> Result<()> {
        let ops = fan_ops(self.hubris, &mut self.context, false)?;
        self.enqueue_work(core, ops)?;
        Ok(())
    }

    fn zoom_in(&mut self) {
        self.temps.zoom_in();
    }

    fn zoom_out(&mut self) {
        self.temps.zoom_out();
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
                    KeyCode::Char('}') => dashboard.temps.interpolate += 1,
                    KeyCode::Char('{') => {
                        if dashboard.temps.interpolate != 0 {
                            dashboard.temps.interpolate -= 1;
                        }
                    }
                    KeyCode::Up => dashboard.up(),
                    KeyCode::Down => dashboard.down(),
                    KeyCode::Esc => dashboard.esc(),
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

fn dashboard(
    hubris: &mut HubrisArchive,
    core: &mut dyn Core,
    _args: &Args,
    subargs: &[String],
) -> Result<()> {
    let subargs = DashboardArgs::from_iter_safe(subargs)?;
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

pub fn init<'a, 'b>() -> (Command, App<'a, 'b>) {
    (
        Command::Attached {
            name: "dashboard",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: dashboard,
        },
        DashboardArgs::clap(),
    )
}

fn thermal_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
) -> Result<(Vec<Op>, Vec<String>)> {
    let mut ops = vec![];
    let mut sensors = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Sensor", "get", None)?;

    let ok = hubris.lookup_basetype(op.ok)?;

    if ok.encoding != HubrisEncoding::Float {
        bail!("expected return value of read_sensor() to be a float");
    }

    if ok.size != 4 {
        bail!("expected return value of read_sensor() to be an f32");
    }

    for (i, s) in hubris.manifest.sensors.iter().enumerate() {
        if s.kind != HubrisSensorKind::Temperature {
            continue;
        }

        let payload =
            op.payload(&[("id", idol::IdolArgument::Scalar(i as u64))])?;
        context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
        sensors.push(s.name.clone());
    }

    ops.push(Op::Done);

    Ok((ops, sensors))
}

fn power_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    state: &str,
) -> Result<Vec<Op>> {
    let mut ops = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(hubris, "Sequencer", "set_state", None)?;

    let payload =
        op.payload(&[("state", idol::IdolArgument::String(state))])?;
    context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    ops.push(Op::Done);

    Ok(ops)
}

fn fan_ops(
    hubris: &HubrisArchive,
    context: &mut HiffyContext,
    on: bool,
) -> Result<Vec<Op>> {
    let mut ops = vec![];
    let funcs = context.functions()?;
    let op = idol::IdolOperation::new(
        hubris,
        "Sequencer",
        if on { "fans_on" } else { "fans_off" },
        None,
    )?;

    let payload = vec![];
    context.idol_call_ops(&funcs, &op, &payload, &mut ops)?;
    ops.push(Op::Done);

    Ok(ops)
}

fn draw<B: Backend>(f: &mut Frame<B>, dashboard: &mut Dashboard) {
    let size = f.size();
    let screen = Layout::default()
        .direction(Direction::Vertical)
        .constraints(
            [
                Constraint::Ratio(2, 3),
                Constraint::Ratio(1, 6),
                Constraint::Ratio(1, 6),
            ]
            .as_ref(),
        )
        .split(size);

    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints(
            [Constraint::Ratio(4, 5), Constraint::Ratio(1, 5)].as_ref(),
        )
        .split(screen[0]);

    let x_labels = vec![
        Span::styled(
            format!("t-{}", dashboard.temps.width),
            Style::default().add_modifier(Modifier::BOLD),
        ),
        Span::styled(
            format!("t-{}", 1),
            Style::default().add_modifier(Modifier::BOLD),
        ),
    ];

    let mut datasets = vec![];
    let selected = dashboard.temps.legend.state.selected();

    for (ndx, s) in dashboard.temps.series.iter().enumerate() {
        if let Some(selected) = selected {
            if ndx != selected {
                continue;
            }
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
                    "Temperature",
                    Style::default()
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::BOLD),
                ))
                .borders(Borders::ALL),
        )
        .x_axis(
            Axis::default()
                .title("Time")
                .style(Style::default().fg(Color::Gray))
                .labels(x_labels)
                .bounds([0.0, dashboard.temps.width as f64]),
        )
        .y_axis(
            Axis::default()
                .title("Degrees Celsius")
                .style(Style::default().fg(Color::Gray))
                .labels(vec![
                    Span::styled(
                        format!("{:2.0}°", dashboard.temps.bounds[0]),
                        Style::default().add_modifier(Modifier::BOLD),
                    ),
                    Span::styled(
                        format!("{:2.0}°", dashboard.temps.bounds[1]),
                        Style::default().add_modifier(Modifier::BOLD),
                    ),
                ])
                .bounds(dashboard.temps.bounds),
        );

    f.render_widget(chart, chunks[0]);

    let mut rows = vec![];

    for s in &dashboard.temps.series {
        let val = match s.raw.last() {
            None | Some(None) => "-".to_string(),
            Some(Some(val)) => format!("{:4.2}°", val),
        };

        rows.push(ListItem::new(Spans::from(vec![
            Span::styled(
                format!("{:<15}", s.name),
                Style::default().fg(s.color),
            ),
            Span::styled(val, Style::default().fg(s.color)),
        ])));
    }

    let list = List::new(rows)
        .block(Block::default().borders(Borders::ALL).title("Sensors"))
        .highlight_style(
            Style::default()
                .bg(Color::LightGreen)
                .fg(Color::Black)
                .add_modifier(Modifier::BOLD),
        );

    //        .highlight_symbol(">> ");

    // We can now render the item list
    f.render_stateful_widget(
        list,
        chunks[1],
        &mut dashboard.temps.legend.state,
    );

    /*
    let table = Table::new(rows)
        .header(
            Row::new(vec!["Sensor", "Temp"])
                .style(Style::default().fg(Color::Gray))
        )
        .block(
            Block::default()
                .title(Span::styled(
                    "Sensors",
                    Style::default()
                        .fg(Color::Cyan)
                        .add_modifier(Modifier::BOLD),
                ))
                .borders(Borders::ALL),
        )
        .widths(&[
            Constraint::Ratio(1, 2),
            Constraint::Ratio(1, 2),
        ]);

    f.render_widget(table, chunks[1]);
    */
}
