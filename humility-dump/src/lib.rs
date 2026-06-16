// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Library to support hubris dumps
//!
//! These functions can be used to collect a system dump or extract
//! individual task crash dumps

#![warn(missing_docs)]
use humility::core::Core;
use humility::hubris::{HubrisArchive, HubrisTask};
use humility::log::{Logger, warn};
use humility::mem::InMemoryCore;
use humility_dump_agent::{
    DumpAgent, DumpAgentExt, DumpArea, HiffyDumpAgent, UdpDumpAgent, task_areas,
};
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Error returned from dumping
#[derive(Debug, thiserror::Error)]
pub enum DumpError {
    /// Tried to take a dump when there were pending dumps. The
    /// dumps should be extracted first before trying to take a dump.
    #[error("cannot take a dump while one is already in progress")]
    DumpAlreadyInProgress,
    /// Error creating UDP agent
    #[error("error creating UDP agent")]
    UdpAgent(#[source] anyhow::Error),
    /// Error creating hiffy agent
    #[error("error creating hiffy agent")]
    HiffyAgent(#[source] anyhow::Error),
    /// Error reading dump headers
    #[error("error reading dump headers")]
    DumpHeaders(#[source] anyhow::Error),
    /// Tried to extract a system dump when a task dump was expected
    #[error("found system dump, expected task dump(s)")]
    SystemDump,
    /// Error creating in-memory core
    #[error("Error creating in-memory core")]
    InMemoryCore(#[source] anyhow::Error),
    /// Error reading dump via agent
    #[error("error reading dump")]
    ReadDump(#[source] anyhow::Error),
    /// Error writing dump
    #[error("error writing dump")]
    HubrisDump(#[source] anyhow::Error),
    /// Error taking a dump
    #[error("error taking a dump")]
    TakeDump(#[source] anyhow::Error),
    /// Dump has indicated failure but this is likely due to space
    /// exhaustion. The dump can be extracted but may be incomplete
    #[error("dump space exhausted")]
    DumpSpaceExhaustion(#[source] anyhow::Error),
    /// Dump has indicated failure but some dump contents appear to
    /// have been written. This can be extractec but may be incomplete.
    #[error("dump incomplete")]
    IncompleteDump(#[source] anyhow::Error),
    /// Expected to get a task
    #[error("expected task")]
    ExpectedTask,
    /// Error getting dump segments
    #[error("error getting dump segments")]
    DumpSegments(#[source] anyhow::Error),
    /// Error initializing dump
    #[error("error initializing dump")]
    InitializeDump(#[source] anyhow::Error),
    /// Error initializing segments
    #[error("error initialziing segments")]
    InitializeSegments(#[source] anyhow::Error),
    /// Error setting timeout
    #[error("error setting timeout")]
    SetTimeout(#[source] anyhow::Error),
    /// Unknown dump contents found
    #[error("unknown contents type {0}")]
    UnknownContents(u8),
    /// Error opening dump file for writing
    #[error("opening dump file for writing")]
    OpenError(#[source] std::io::Error),
    /// Error finding task name
    #[error("error finding task name")]
    TaskName(#[source] anyhow::Error),
}

/// Opens a file for use in a system dump. This uses the naming scheme
/// previously found in humility core (hubris.core.0, hubris.core.1 etc.)
/// Return the file name along with the file handle
pub fn open_dump_file(
    hubris: &HubrisArchive,
    task: Option<humpty::DumpTask>,
    default: Option<&Path>,
) -> Result<(PathBuf, std::fs::File), DumpError> {
    let filename = match default {
        Some(filename) => filename.to_owned(),
        None => {
            let prefix = match task {
                Some(task) => {
                    let t = HubrisTask::Task(task.id as u32);
                    format!(
                        "hubris.core.{}.",
                        hubris
                            .lookup_module(t)
                            .map_err(DumpError::TaskName)?
                            .name
                    )
                }
                None => "hubris.core.".to_string(),
            };

            (0..)
                .map(|i| PathBuf::from(format!("{prefix}{i}")))
                .find(|f| !f.exists())
                .unwrap()
        }
    };

    let f = OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(&filename)
        .map_err(DumpError::OpenError)?;

    Ok((filename, f))
}

fn get_dump_agent<'a>(
    hubris: &'a HubrisArchive,
    core: &'a mut dyn Core,
    force_hiffy_agent: bool,
    log: &Logger,
) -> Result<Box<dyn DumpAgent + 'a>, DumpError> {
    // Find the dump agent task name.  This is usually `dump_agent`, but that's
    // not guaranteed; what *is* guaranteed is that it implements the DumpAgent
    // interface.
    let dump_agent_task =
        hubris.lookup_module_by_iface("DumpAgent").map(|t| t.task);

    if core.is_net()
        && !force_hiffy_agent
        && dump_agent_task
            .map(|t| hubris.does_task_have_feature(t, "net").unwrap())
            .unwrap_or(false)
    {
        let imageid = hubris.image_id();

        Ok(Box::new(
            UdpDumpAgent::new(core, imageid, log)
                .map_err(DumpError::UdpAgent)?,
        ))
    } else {
        // This is a mostly uselss timeout because it only gets updated with
        // net cores.
        let timeout = std::time::Duration::from_millis(5000);
        Ok(Box::new(
            HiffyDumpAgent::new(hubris, core, timeout, log)
                .map_err(DumpError::HiffyAgent)?,
        ))
    }
}

/// Extract all task dumps
pub fn extract_all_task_dumps(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    use_hiffy: bool,
    log: &Logger,
) -> Result<(), DumpError> {
    let mut agent = get_dump_agent(hubris, core, use_hiffy, log)?;
    let headers =
        agent.read_dump_headers(false).map_err(DumpError::DumpHeaders)?;
    if headers.is_empty() || headers[0].0.dumper == humpty::DUMPER_NONE {
        return Ok(());
    }

    if headers[0].1.is_none() {
        return Err(DumpError::SystemDump);
    }

    let areas = task_areas(&headers);
    for (area, (task, headers)) in &areas {
        let task_name =
            match hubris.lookup_module(HubrisTask::Task(task.id.into())) {
                Ok(module) => match headers[0].contents {
                    humpty::DUMP_CONTENTS_SINGLETASK => module.name.to_owned(),
                    humpty::DUMP_CONTENTS_TASKREGION => {
                        format!("{}.region", module.name.to_owned())
                    }
                    c => return Err(DumpError::UnknownContents(c)),
                },
                _ => "<unknown>".to_owned(),
            };

        let filename = (0..)
            .map(|i| PathBuf::from(format!("hubris.core.{task_name}.{i}")))
            .find(|f| !f.exists())
            .unwrap();

        let mut dumpfile = OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&filename)
            .map_err(DumpError::OpenError)?;

        let mut out = InMemoryCore::from_archive(hubris)
            .map_err(DumpError::InMemoryCore)?;
        let started = Some(Instant::now());

        let task = agent
            .read_dump(Some(DumpArea::ByIndex(*area)), &mut out, false, log)
            .map_err(DumpError::ReadDump)?;

        if task.0.is_none() {
            return Err(DumpError::ExpectedTask);
        }
        hubris
            .dump(&mut out, task.0, &mut dumpfile, started, log)
            .map_err(DumpError::HubrisDump)?;
    }

    Ok(())
}

/// Extract an existing system dump
pub fn extract_system_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    file: impl std::io::Write,
    use_hiffy: bool,
    log: &Logger,
) -> Result<(), DumpError> {
    let started = Instant::now();
    let mut agent = get_dump_agent(hubris, core, use_hiffy, log)?;
    extract_system_dump_via_agent(hubris, &mut *agent, file, started, log)
}

fn extract_system_dump_via_agent(
    hubris: &HubrisArchive,
    mut agent: &mut dyn DumpAgent,
    file: impl std::io::Write,
    started: std::time::Instant,
    log: &Logger,
) -> Result<(), DumpError> {
    let mut out =
        InMemoryCore::from_archive(hubris).map_err(DumpError::InMemoryCore)?;

    let _ = agent
        .read_dump(None, &mut out, false, log)
        .map_err(DumpError::ReadDump)?;

    // Explicitly reset the dump state so we can take more
    agent.initialize_dump().map_err(DumpError::InitializeDump)?;

    hubris
        .dump(&mut out, None, file, Some(started), log)
        .map_err(DumpError::HubrisDump)?;

    Ok(())
}

/// Take and collect a dump via the agent. The dump will
/// be written to `file`.
pub fn take_system_dump(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    file: impl std::io::Write,
    use_hiffy: bool,
    force_overwrite: bool,
    log: &Logger,
) -> Result<(), DumpError> {
    let segments = hubris
        .dump_segments(core, None, false)
        .map_err(DumpError::DumpSegments)?;
    let mut agent = get_dump_agent(hubris, core, use_hiffy, log)?;
    let header = agent.read_dump_header().map_err(DumpError::DumpHeaders)?;

    if header.dumper != humpty::DUMPER_NONE && !force_overwrite {
        return Err(DumpError::DumpAlreadyInProgress);
    }

    agent.initialize_dump().map_err(DumpError::InitializeDump)?;

    agent
        .initialize_segments(&segments)
        .map_err(DumpError::InitializeSegments)?;

    //
    // We are about to disappear for -- as the kids say -- a minute.
    // Set our timeout to be a literal minute so we don't prematurely
    // give up.
    //

    let started = Instant::now();
    agent
        .core()
        .set_timeout(std::time::Duration::new(60, 0))
        .map_err(DumpError::SetTimeout)?;

    let r = agent.take_dump().map_err(|err| {
        if let Ok(all) = agent.read_dump_headers(true) {
            let c = all
                .iter()
                .filter(|&&(h, _)| h.dumper != humpty::DUMPER_NONE)
                .count();
            if c == all.len() {
                DumpError::DumpSpaceExhaustion(err)
            } else if c != 0 {
                DumpError::IncompleteDump(err)
            } else {
                DumpError::TakeDump(err)
            }
        } else {
            DumpError::TakeDump(err)
        }
    });

    // Not all these are fatal at the moment. Just log a warning for a
    // few of these.
    match r {
        Ok(_) => (),
        Err(DumpError::DumpSpaceExhaustion(err)) => {
            warn!(
                log,
                "dump has indicated failure ({err:#}), but this is \
                likely due to space exhaustion; \
                dump will be extracted but may be incomplete!"
            );
        }
        Err(DumpError::IncompleteDump(err)) => {
            warn!(
                log,
                "dump has indicated failure ({err:#}), but some dump \
                contents appear to have been written; \
                dump will be extracted but may be incomplete!"
            );
        }
        _ => {
            return r;
        }
    }

    extract_system_dump_via_agent(hubris, &mut *agent, file, started, log)?;

    Ok(())
}

/// Representation of a task name from a dump
pub enum TaskDumpName {
    /// A single task (e.g. `hf`)
    SingleTask(String),
    /// A task region
    TaskRegion(String),
    /// Unknown region
    Unknown,
}

impl std::fmt::Display for TaskDumpName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SingleTask(s) => s.fmt(f),
            Self::TaskRegion(s) => write!(f, "{s} [region]"),
            Self::Unknown => "<unknown>".fmt(f),
        }
    }
}

/// Meta information about an available task dump
pub struct TaskDumpData {
    /// Area number
    pub area: usize,
    /// Total size of dump
    pub size: u32,
    /// Task name or region
    pub task_name: TaskDumpName,
    /// Time the task was dumped
    pub time: u64,
}

/// Available dump types
pub enum DumpType {
    /// No dumps
    None,
    /// A whole system dump
    System {
        /// Size of the system dump
        size: u32,
    },
    /// Individual Hubris tasks
    Tasks(Vec<TaskDumpData>),
}

/// List all available dumps
pub fn list_dumps(
    hubris: &HubrisArchive,
    use_hiffy: bool,
    core: &mut dyn Core,
    log: &Logger,
) -> Result<DumpType, DumpError> {
    let mut agent = get_dump_agent(hubris, core, use_hiffy, log)?;

    let headers =
        agent.read_dump_headers(false).map_err(DumpError::DumpHeaders)?;

    // raw: false should mean we never get humpty::DUMPER_NONE here but
    // double check this later?
    if headers.is_empty() || headers[0].0.dumper == humpty::DUMPER_NONE {
        return Ok(DumpType::None);
    }

    if headers[0].1.is_none() {
        let size = headers
            .iter()
            .filter(|&(h, _)| h.dumper != humpty::DUMPER_NONE)
            .fold(0, |ttl, (h, _)| ttl + h.written);

        return Ok(DumpType::System { size });
    }

    let areas = task_areas(&headers);

    let mut tasks = vec![];

    for (area, (task, headers)) in &areas {
        let size = headers.iter().fold(0, |ttl, h| ttl + h.written);

        let task_name =
            match hubris.lookup_module(HubrisTask::Task(task.id.into())) {
                Ok(module) => match headers[0].contents {
                    humpty::DUMP_CONTENTS_SINGLETASK => {
                        TaskDumpName::SingleTask(module.name.to_owned())
                    }
                    humpty::DUMP_CONTENTS_TASKREGION => {
                        TaskDumpName::TaskRegion(module.name.to_owned())
                    }
                    c => return Err(DumpError::UnknownContents(c)),
                },
                _ => TaskDumpName::Unknown,
            };

        tasks.push(TaskDumpData {
            area: *area,
            size,
            task_name,
            time: task.time,
        });
    }

    Ok(DumpType::Tasks(tasks))
}
