// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! Mirror (doppelganger) versions of types from Hubris.
//!
//! We do this instead of directly including the Hubris abi crate, say, because
//! we have to support many versions of Hubris. Defining our own types lets us
//! be more flexible, take advantage of `std`, and use host-size values for
//! things like `usize` if we want.
//!
//! Is this duplicative? Yes! It sure is. However, if we didn't have these
//! types, this same information would be _spread throughout the debugger
//! codebase_, which is both duplicative and fragile.
//!
//! Also note that doing things this way means the layout and in-memory
//! representation of these types is totally independent from those used in
//! Hubris. For instance, enums can be freely reordered.
//!
//! This module is intended to be imported and used by name in the event of
//! conflicts, e.g. `doppel::TaskId`, so the types herein don't have prefixes on
//! their names.
//!
//! # Compatibility
//!
//! Compatibility between these types and the types used in the Hubris
//! application is maintained through the `Load` impls. Generally speaking,
//!
//! - enums here can be any _superset_ of the ones in the application,
//! - struct fields can be any _subset._
//!
//! That is, we can add enum cases the application doesn't understand without
//! breaking compatibility, and the application can have struct fields we don't
//! interpret.

use anyhow::{anyhow, bail, Result};
use humility::reflect::{Load, Ptr, Value};
use std::convert::TryInto;
use zerocopy::{AsBytes, LittleEndian, U16, U64};

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct TaskDesc {
    pub entry_point: u32,
    pub initial_stack: u32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct Task {
    pub state: TaskState,
    pub generation: GenOrRestartCount,
    pub priority: Priority,
    pub descriptor: Ptr,
    pub timer: TimerState,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct Generation(pub u8);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct Priority(pub u8);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct Timestamp(pub u64);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct NotificationSet(pub u32);

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub struct TimerState {
    pub deadline: Option<Timestamp>,
    pub to_post: NotificationSet,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum TaskState {
    /// Task is healthy and can be scheduled subject to the `SchedState`
    /// requirements.
    Healthy(SchedState),
    /// Task has been stopped by a fault and must not be scheduled without
    /// intervention.
    Faulted {
        /// Information about the fault.
        fault: FaultInfo,
        /// Record of the previous healthy state at the time the fault was
        /// taken.
        original_state: SchedState,
    },
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum SchedState {
    /// This task is ignored for scheduling purposes.
    Stopped,
    /// This task could be scheduled on the CPU.
    Runnable,
    /// This task is blocked waiting to deliver a message to the given task.
    InSend(TaskId),
    /// This task is blocked waiting for a reply from the given task.
    InReply(TaskId),
    /// This task is blocked waiting for messages, either from any source
    /// (`None`) or from a particular sender only.
    InRecv(Option<TaskId>),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum FaultInfo {
    /// The task has violated memory access rules. This may have come from a
    /// memory protection fault while executing the task (in the case of
    /// `source` `User`), from overflowing a stack, or from checks on kernel
    /// syscall arguments (`source` `Kernel`).
    MemoryAccess {
        /// Problematic address that the task accessed, or asked the kernel to
        /// access. This is `Option` because there are cases of processor
        /// protection faults that don't provide a precise address.
        address: Option<u32>,
        /// Origin of the fault.
        source: FaultSource,
    },
    /// A task has overflowed its stack. We can always determine the bad
    /// stack address, but we can't determine the PC
    StackOverflow { address: u32 },
    /// A task has induced a bus error
    BusError { address: Option<u32>, source: FaultSource },
    /// Divide-by-zero
    DivideByZero,
    /// Attempt to execute non-executable memory
    IllegalText,
    /// Execution of an illegal instruction
    IllegalInstruction,
    /// Other invalid operation, with 32-bit code
    InvalidOperation(u32),
    /// Arguments passed to a syscall were invalid. TODO: this should become
    /// more descriptive, it's a placeholder.
    SyscallUsage(UsageError),
    /// A task has explicitly aborted itself with a panic.
    Panic,
    /// A fault has been injected into this task by another task
    Injected(TaskId),
    /// A fault has been delivered by a server task.
    FromServer(TaskId, ReplyFaultReason),
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum UsageError {
    /// A program used an undefined syscall number.
    BadSyscallNumber,
    /// A program specified a slice as a syscall argument, but the slice is
    /// patently invalid: it is either unaligned for its type, or it is
    /// expressed such that it would wrap around the end of the address space.
    /// Neither of these conditions is ever legal, so this represents a
    /// malfunction in the caller.
    InvalidSlice,
    /// A program named a task ID that will never be valid, as it's out of
    /// range.
    TaskOutOfRange,
    /// A program named a valid task ID, but attempted to perform an operation
    /// on it that is illegal or otherwise forbidden.
    IllegalTask,
    LeaseOutOfRange,
    OffsetOutOfRange,
    NoIrq,
    BadKernelMessage,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum FaultSource {
    /// User code did something that was intercepted by the processor.
    User,
    /// User code asked the kernel to do something bad on its behalf.
    Kernel,
}

/// Reasons a server might cite when using the `REPLY_FAULT` syscall.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Load)]
pub enum ReplyFaultReason {
    /// The message indicated some operation number that is unknown to the
    /// server -- which almost certainly indicates that the client intended the
    /// message for a different kind of server.
    UndefinedOperation = 0,
    /// The message sent by the client had the wrong size to even attempt
    /// parsing by the server -- either too short or too long. (Because most
    /// messages are fixed size, it currently doesn't seem useful to distinguish
    /// between too-short and too-long.)
    BadMessageSize = 1,
    /// The server attempted to parse the message, and couldn't. This may
    /// indicate an enum with an illegal value, or a more nuanced error on
    /// operations that use serde encoding.
    BadMessageContents = 2,
    /// The client did not provide the leases required for the operation, or
    /// provided them with the wrong attributes.
    BadLeases = 3,
    /// The client did not provide a reply buffer large enough to receive the
    /// server's reply, despite this information being implied by the IPC
    /// protocol.
    ReplyBufferTooSmall = 4,

    /// Application-defined: The client attempted to operate on a resource that
    /// is not available to them due to mandatory access control or other type
    /// of access validation.
    AccessViolation = 5,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GenOrRestartCount {
    Gen(Generation),
    RestartCount(u32),
}

impl From<GenOrRestartCount> for u32 {
    fn from(g: GenOrRestartCount) -> Self {
        match g {
            GenOrRestartCount::Gen(g) => u32::from(g.0),
            GenOrRestartCount::RestartCount(x) => x,
        }
    }
}

impl humility::reflect::Load for GenOrRestartCount {
    fn from_value(v: &Value) -> Result<Self> {
        // At the time of this writing, we're in the midst of transitioning from
        // storing 6-bit generations in the kernel to storing 32-bit restart
        // counts, where the bottom 6 bits give the generation. Handle both
        // shapes here.
        if let Ok(g) = Generation::from_value(v) {
            Ok(Self::Gen(g))
        } else if let Ok(n) = u32::from_value(v) {
            Ok(Self::RestartCount(n))
        } else {
            bail!("unexpected generation/restart count shape: {:?}", v);
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct TaskId(pub u16);

impl TaskId {
    // TODO: these assume structure of the task ID that should, ideally, come
    // from debug info... but it's currently not in there.

    pub const KERNEL: Self = Self(0xFFFF);

    pub fn index(self) -> usize {
        usize::from(self.0 & 0x3FF)
    }

    pub fn generation(self) -> u8 {
        (self.0 >> 10) as u8
    }
}

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "#{}/gen{}", self.index(), self.generation())
    }
}

impl humility::reflect::Load for TaskId {
    fn from_value(v: &Value) -> Result<Self> {
        // In some older kernel cores there's a single case where a task ID is
        // represented as a u32 rather than a tuple struct. Handle this by
        // allowing a tuple struct or a basetype here.
        let v = v
            .as_1tuple()
            .and_then(|v| v.as_base())
            .or_else(|_| v.as_base())
            .map_err(|_| anyhow!("unexpected form for TaskId: {:?}", v))?;

        // Accept either a u16, or a u32 that fits into a u16.
        let bits = if let Some(bits) = v.as_u16() {
            bits
        } else if let Some(bits) = v.as_u32() {
            bits.try_into()?
        } else {
            bail!("task ID not 16 bits");
        };
        Ok(Self(bits))
    }
}

/// Double of the struct from `ringbuf`.
///
/// The Hubris equivalent has a type parameter. We're dynamic instead: the
/// `payload` is just read in as a generic `Value`.
#[derive(Clone, Debug, Load)]
pub struct RingbufEntry {
    pub line: u16,
    pub generation: u16,
    pub count: u32,
    pub payload: Value,
}

/// Double of the struct from `ringbuf`.
///
/// The Hubris equivalent has a type parameter. We're dynamic instead; see
/// `RingbufEntry`.
#[derive(Clone, Debug, Load)]
pub struct Ringbuf {
    pub last: Option<u32>,
    pub buffer: Vec<RingbufEntry>,
}

#[derive(Clone, Debug, Load)]
pub struct StaticCell {
    pub cell: UnsafeCell,
}

#[derive(Clone, Debug, Load)]
pub struct UnsafeCell {
    pub value: Value,
}

/// Double of the struct from `udprpc`
#[derive(Copy, Clone, Debug, AsBytes)]
#[repr(C)]
pub struct RpcHeader {
    pub image_id: U64<LittleEndian>,
    pub task: U16<LittleEndian>,
    pub op: U16<LittleEndian>,
    pub nreply: U16<LittleEndian>,
    pub nbytes: U16<LittleEndian>,
}
