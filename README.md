
# Humility

Humility is the debugger for
<a href="https://github.com/oxidecomputer/hubris">Hubris</a>.

## Guiding principles

### Production disposition

Hubris is the artifact that actually runs, and it must be allowed to be
optimized in terms of size, space, and run-time; in as much as contortions are
required for debuggability, they should be borne by Humility, not Hubris.  As a
concrete example of this, Humility operates exclusively on `release` builds of
Hubris, relying on (unloaded) DWARF information in the binaries to make sense of
the system.

### Hubris-specific

Humility is Hubris-specific:  it is not trying to be a generic *in situ*
debugger, but rather specifically focused on debugging Hubris-based systems.
Humility is therefore willing to encode Hubris-specific concepts like
archives and tasks.

### Microcontroller-specific

Debuggers must cut through abstractions, which
often requires knowledge of underlying
implementation detail.  For Humility, this means being willing to take
advantage of microcontroller-specific debug facilities where applicable.
While Hubris may make decisions in the name of greater portability,
Humility will generally make decisions to maximize debuggability, even
where these facilities are highly specific to a particular MCU.

### Device-specific

Humility is unafraid to offer device-specific functionality where that
functionality is useful for system debuggability.  (For example,
`humility i2c` functions as an I^2^C analyzer.)  That said, all device
interaction uses Hubris as a proxy to actually communicate with the devices
themselves (e.g., via [HIF](https://github.com/oxidecomputer/hif));
Humility does not seek to create second I/O path.

### Pragmatic

Humility seeks to be *useful*, and therefore seeks to offer all manners of
debugging: in situ, postmortem, dynamic instrumentation, static
instrumentation, etc.

## Operation

Humility operates by specifying a subcommand.  There are options that
are Humility-wide (that is, applying to every subcommand), as well as
options that are specific to particular subcommands.

### Chip

While some autodetection is possible, Humility regrettably may need to be made
aware of the specifics of the target chip.  Supported chips include:

- `STM32F407VGTx` (default): STM32F407 as found on the reference Discovery board
- `LPC55S69JBD100`: LPC55S69 as found on the LPCXpresso55S69
- `STM32H7B3LIHxQ`: STM32H7B3 as found on the STM32H7B3I-DK

The target chip can be specified via the `-c` option or the `HUMILITY_CHIP`
environment variable.

### Probe

Humility needs to have some way of extracting information and/or controlling
the microcontroller running Hubris.  This is done through some variant of a
debug *probe*, a separate microcontroller that speaks to debug-specific
functionality on the target microcontroller.  (For details of the mechanics
of these probes on ARM parts, see <a
href="https://65.rfd.oxide.computer">RFD 65</a>.)

For most evaluation boards, this debug probe is available on the board, and
is connected to a host via USB, e.g. ST's STLink/V2 on the STM32F407
Discovery or the LPC-Link2 present on the LPCXpresso55S69.
(On the Gemini board, there are two SWD headers, one for the LPC55S28
and the other for the STM32H753.)
While Humility allows for direct attachment to an on-chip debugger, doing so
precludes other connections (from, for example, OpenOCD), making it to
disruptive to development workflows. To allow for easier development
workflows, Humility also has the option to attach via OpenOCD.

The debug probe to use is specified to Humility via
the `-p` option (long form `--probe`), which can have the following values:

- `auto` (default): Automatically determine how to attach to the
  microcontroller.

- `ocd`: Attach via OpenOCD, which is presumed to have the TCL interface
  available on localhost on port 6666 (its default).

- `jlink`: Attach via Segger JLink, which is presumed to have the GDB
  interface available on localhost on port 2331 (its default).  Note that
  when semihosting is being used by Hubris, the Segger JLink GDB server
  will become confused when Humility attaches to it -- and subsequent
  calls to semihosting will cause a halt.  A subsequent Humility invocation
  will resume the target (directing semihosting output correctly to the
  running GDB instance), but subsequent semihosting output will again cause
  a halt. To recover from this condition, send an explicit ^C to the
  running GDB and continue from the resulting stop.

- `usb`: Attach directly via USB to a debug probe.  When multiple probes
  are plugged in via USB, a probe index must be specified as a suffix
  (e.g., `usb-0`, `usb-1`, etc.)  To determine which probe is which,
  examine the serial number in the output of `humility probe`.

- `vid:pid[:serial]`: In some cases, the automatic algorithm may either find
  the wrong thing, or timeout attempting to search for non-existent probes.
  In these cases, it can help to explicitly set the vendor ID (VID) and
  product ID (PID) of the debug probe, which should be colon-delimited, e.g.,
  `0483:374e`.  (Determining the VID and PID of an attached probe is
  platform-specific; on Linux one can use the `lsusb` command.)  In cases
  where there are multiple probes with the same VID and PID, the serial number
  of the probe (as reported via `humility probe` can be postpended, also
  delimited by a colon, e.g. `0483:374e:004000343137510939383538`.

### Archive

Many Humility commands require the complete Hubris archive.  This is a ZIP
archive created by the build process, and includes all binaries as well as the
`app.toml` file used to configure the Hubris archive.  The Hubris archive is
specified via the `-a` option or the `HUMILITY_ARCHIVE` environment variable.

### Dump

Many Humility commands are able to operate *postmortem* on a Hubris dump,
an ELF core file generated by the `humility dump` command.
Dumps are offered in lieu of a probe and an archive and specified via
the `-d` option (long form `--dump`) or the `HUMILITY_DUMP` environment
variable.

## Commands

- [humility apptable](#humility-apptable): print Hubris apptable
- [humility dump](#humility-dump): generate Hubris dump
- [humility i2c](#humility-i2c): scan for and read I<sup>2</sup>C devices
- [humility jefe](#humility-jefe): control tasks exernally via jefe
- [humility manifest](#humility-manifest): print archive manifest
- [humility map](#humility-map): print memory map, with association of regions to tasks
- [humility probe](#humility-probe): probe attached devices
- [humility readmem](#humility-readmem): read and display memory region
- [humility readvar](#humility-readvar): read and display a specified Hubris variable
- [humility ringbuf](#humility-ringbuf): read and display any ring buffers
- [humility stackmargin](#humility-stackmargin): calculate and print stack
  margins by task
- [humility tasks](#humility-tasks): list Hubris tasks
- [humility test](#humility-test): run Hubris test suite and parse results

### `humility manifest`

`humility manifest` displays information about the Hubris archive.  It
does not connect at all to a Hubris target to operate.  In addition to
archive-wide attributes, `humility manifest` displays the features enabled
for each task, e.g.:

```console
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip manifest
humility:      version => hubris build archive v1.0.0
humility:      git rev => 049db2ef2a24eabf3fef3f8a6634b36e9d371ad6-dirty
humility:        board => stm32h7b3i-dk
humility:       target => thumbv7em-none-eabihf
humility:     features => itm, h7b3
humility:   total size => 65K
humility:  kernel size => 19K
humility:        tasks => 8
humility:                 ID TASK                SIZE FEATURES
humility:                  0 jefe                8.2K itm
humility:                  1 rcc_driver          6.0K
humility:                  2 gpio_driver         6.3K
humility:                  3 usart_driver        6.5K h7b3
humility:                  4 user_leds           6.1K stm32h7
humility:                  5 pong                6.1K
humility:                  6 ping                6.1K uart
humility:                  7 idle                0.1K
```

`humility manifest` can operate on either an archive or on a dump.

### `humility probe`

`humility probe` attempts to infer as much about the hardware state as it
can, e.g.:

```console
% humility probe
humility: attached via ST-Link
humility:        probe => STLink V3, VID 0483, PID 374e
humility: probe serial => 003700303137511139383538
humility:         core => Cortex-M7
humility: manufacturer => STMicroelectronics
humility:         chip => STM32H7, revision 0x2003
humility:       status => executing
humility:  debug units => CSTF(x2) CTI(x2) DWT ETM FPB ITM SCS SWO TMC TPIU
humility:         CSTF => 0x5c004000, 0x5c013000
humility:          CTI => 0x5c011000, 0xe0043000
humility:          DWT => 0xe0001000
humility:          ETM => 0xe0041000
humility:          FPB => 0xe0002000
humility:          ITM => 0xe0000000
humility:          SCS => 0xe000e000
humility:          SWO => 0x5c003000
humility:          TMC => 0x5c014000
humility:         TPIU => 0x5c015000
humility:   ITM status => TRCENA enabled, TCR disabled, TER=0x0
humility:           R0 => 0x20006000
humility:           R1 => 0x20006000
humility:           R2 => 0x0
humility:           R3 => 0x0
humility:           R4 => 0x0
humility:           R5 => 0x0
humility:           R6 => 0x0
humility:           R7 => 0x0
humility:           R8 => 0x0
humility:           R9 => 0x0
humility:          R10 => 0x0
humility:          R11 => 0x0
humility:          R12 => 0x0
humility:           SP => 0x20006100
humility:           LR => 0x802404f
humility:           PC => 0x8024052
humility:         xPSR => 0x61000000
humility:          MSP => 0x20000f48
humility:          PSP => 0x20006100
humility:          SPR => 0x7000000
```

If provided a Hubris archive, `humility probe` will display any register
contents symbolically, e.g.:

```console
% humility -a ~/hubris/target/demo/dist/build-demo.zip probe
humility: attached via ST-Link
humility:        probe => STLink V2-1, VID 0483, PID 374b
humility: probe serial => 066DFF383032534E43132614
humility:         core => Cortex-M4
humility: manufacturer => STMicroelectronics
humility:         chip => STM32F40x/STM32F41x, revision 0x1007
humility:  debug units => DWT ETM FPB ITM SCS TPIU
humility:       status => executing
humility:          ITM => TRCENA enabled, TCR enabled, TER=0x3
humility:           R0 => 0x0
humility:           R1 => 0x0
humility:           R2 => 0x1
humility:           R3 => 0x20001bd4
humility:           R4 => 0x20001bd4
humility:           R5 => 0x801d988
humility:           R6 => 0xb004
humility:           R7 => 0x20001bf0
humility:           R8 => 0x40004400
humility:           R9 => 0x1
humility:          R10 => 0x0
humility:          R11 => 0xffff
humility:          R12 => 0x0
humility:           SP => 0x20001ba8
humility:           LR => 0x801c12b   <- main+0xef
humility:           PC => 0x801d290   <- sys_recv_stub+0x1e
humility:         xPSR => 0x61000000
humility:          MSP => 0x20000f48
humility:          PSP => 0x20001ba8
humility:          SPR => 0x7000000
```

### `humility tasks`

`humility tasks` offers a ps-like view of a system, e.g.:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip tasks
humility: attached via STLink
system time = 83329
ID TASK            GEN PRI STATE
 0 jefe              0   0 FAULT: stack overflow; sp=0x20000fa0 (was: ready)
 1 rcc_driver        0   1 recv
 2 gpio_driver       0   2 recv
 3 usart_driver      0   2 recv, notif: bit0(irq39)
 4 i2c_driver        0   2 recv
 5 spd               0   2 notif: bit0(irq33/irq34)
 6 spi_driver        0   2 RUNNING
 7 spi               0   3 wait: reply from 0x0006
 8 user_leds         0   2 recv
 9 pong              0   3 FAULT: killed by task 0x0000 (was: recv, notif: bit0)
10 i2c_debug         0   3 notif: bit0(T+671)
11 thermal           0   3 notif: bit0(T+552)
12 idle              0   5 ready
```

To see every field in each task, you can use the `-v` flag:

```console
% humility -d hubris.core.4 tasks -v
...
 9 pong              0   3 FAULT: killed by task 0x0000 (was: recv, notif: bit0)
          |
          +---->  {
                    save: SavedState {
                        r4: 0x20002bbc,
                        r5: 0x10,
                        r6: 0x1,
                        r7: 0x0,
                        r8: 0xa007,
                        r9: 0x20002bcc,
                        r10: 0x1951d,
                        r11: 0x1,
                        psp: 0x20002b28,
                        exc_return: 0xffffffed
                    },
                    priority: Priority(0x3),
            ...
...
```

To see a task's registers, use the `-r` flag:

```console
% humility tasks -r user_leds
...
ID TASK            GEN PRI STATE
 8 user_leds         0   2 recv
   |
   +--->   R0 = 0x200023d8   R1 = 0x00000004   R2 = 0x00000000   R3 = 0x200023dc
           R4 = 0x200023d8   R5 = 0x00000004   R6 = 0x00000000   R7 = 0x00000000
           R8 = 0x080215c8   R9 = 0x40020c00  R10 = 0x00000000  R11 = 0x00000001
          R12 = 0x00000000   SP = 0x200023b0   LR = 0x080200ab   PC = 0x08021052
```

To see a task's stack backtrace, use the `-s` flag:

```console
% humility tasks -s user_leds
...
ID TASK            GEN PRI STATE
 8 user_leds         0   2 recv
   |
   +--->  0x200023d0 0x08021052 sys_recv_stub()
          0x20002400 0x0802009c sys_recv()
          0x20002400 0x080200aa main()
```

To additionally see line number information on a stack backtrace, also provide
`-l` flag:

```console
% humility tasks -sl user_leds
...
ID TASK            GEN PRI STATE
 8 user_leds         0   2 recv
   |
   +--->  0x200023d0 0x08021052 sys_recv_stub()
                     @ /home/bmc/hubris/userlib/src/lib.rs:261
          0x20002400 0x0802009c sys_recv()
                     @ /home/bmc/hubris/userlib/src/lib.rs:208
          0x20002400 0x080200aa main()
                     @ /home/bmc/hubris/drv/user-leds/src/main.rs:69
```

### `humility jefe`

Humility allows for some (well-defined) manipulation of tasks via `jefe`,
the Hubris supervisor.  By default, `jefe` will restart any faulting task;
when debugging, it can be useful to hold a task in the faulted state. This
is done via the `-h` option:

```console
% humility jefe -h ping
humility: attached via ST-Link
humility: successfully changed disposition for ping
```

This does not change the task, but rather `jefe`'s restart disposition; when
the task next faults, it will not be restarted, e.g.:

```console
% humility tasks ping
ID ADDR     TASK               GEN PRI STATE
 4 200003c8 ping                35   4 Faulted { fault: DivideByZero, original_state: Runnable }
```

This can be particularly useful to couple with either `humility dump` or
`humility tasks -sl`:

```console
% humility tasks -sl ping
humility: attached via ST-Link
ID ADDR     TASK               GEN PRI STATE
 4 200003c8 ping                35   4 Faulted { fault: DivideByZero, original_state: Runnable }
   |
   +--->  0x200025b0 0x0802405e divzero()
                     @ /home/bmc/hubris/task-ping/src/main.rs:28
          0x20002600 0x080240f2 sys_panic()
                     @ /home/bmc/hubris/userlib/src/lib.rs:642
          0x20002600 0x080240f2 main()
                     @ /home/bmc/hubris/task-ping/src/main.rs:39
```

To change the disposition of a task (back) to be restarted, use the `-r` flag:

```console
% humility jefe -r ping
humility: attached via ST-Link
humility: successfully changed disposition for ping
```

To inject a fault into a task, use the `-f` flag, which will change the
disposition to injecting a fault.  The injected fault has a dedicated type to
make clear that the fault originated outside of the task:

```console
% humility jefe -f ping
humility: attached via ST-Link
humility: successfully changed disposition for ping
% humility tasks ping
ID ADDR     TASK               GEN PRI STATE
 4 200003c8 ping                 7   4 Faulted { fault: Injected(0x0), original_state: InReply(TaskId(0x5)) }
```

To restart a task that has had a fault injected, again use the `-r` flag to
change its disposition back to restart.

Finally, to start a task that is not started by default, use the `-s` flag.

### `humility map`

One common pathology in Hubris tasks is a fault induced when a task attempts
to access a memory address outside of its designated regions.  (This can
happen, for example, because the task attempted to access device memory that
was not allocated to it in the build description, because it exceeded the
device memory allocated to it, or because it allocated the memory allocated to
it, e.g., because of a stack overflow.) `humility tasks` can be useful to see
this happening; a non-zero generation count will indicate that a task has been
restarted -- and the log message from `jefe` will indicate the specific
address, e.g.:

```console
% humility itm -ea
humility: attached via OpenOCD
humility: core halted
humility: core resumed
humility: TPIU sync packet found at offset 1
humility: ITM synchronization packet found at offset 12
Task #7 Memory fault at address 0x200028fc
Task #7 Memory fault at address 0x200028fc
^C
```

To better understand the memory that a task is trying to access, one can
run the `humility map` command, which shows the memory regions that
have been mapped into tasks, in address order:

```console
% humility -a ~/hubris/target/demo/dist/build-demo.zip map
humility: attached via OpenOCD
DESC       LOW          HIGH          SIZE ATTR  ID TASK
0x08004864 0x08010000 - 0x08017fff   32KiB r-x--  0 jefe
0x08004884 0x08018000 - 0x08019fff    8KiB r-x--  1 rcc_driver
0x080048a4 0x0801c000 - 0x0801ffff   16KiB r-x--  2 usart_driver
0x080048c4 0x08020000 - 0x08023fff   16KiB r-x--  3 user_leds
0x080048e4 0x08024000 - 0x08025fff    8KiB r-x--  4 ping
0x08004904 0x08026000 - 0x08027fff    8KiB r-x--  5 pong
0x08004924 0x08028000 - 0x080280ff     256 r-x--  6 idle
0x08004944 0x0802a000 - 0x0802bfff    8KiB r-x--  7 oh_no
0x08004964 0x0802c000 - 0x0802dfff    8KiB r-x--  8 oh_no2
0x08004874 0x20001000 - 0x200013ff    1KiB rwx--  0 jefe
0x08004894 0x20001400 - 0x200017ff    1KiB rwx--  1 rcc_driver
0x080048b4 0x20001800 - 0x20001bff    1KiB rwx--  2 usart_driver
0x080048d4 0x20001c00 - 0x20001fff    1KiB rwx--  3 user_leds
0x080048f4 0x20002000 - 0x200021ff     512 rwx--  4 ping
0x08004914 0x20002400 - 0x200027ff    1KiB rwx--  5 pong
0x08004934 0x20002800 - 0x200028ff     256 rwx--  6 idle
0x08004954 0x20002900 - 0x200029ff     256 rwx--  7 oh_no
0x08004974 0x20002a00 - 0x20002aff     256 rwx--  8 oh_no2
0x08004824 0x40004400 - 0x400047ff    1KiB rw-d-  2 usart_driver
0x08004844 0x40020000 - 0x400203ff    1KiB rw-d-  2 usart_driver
0x08004854 0x40020c00 - 0x40020fff    1KiB rw-d-  3 user_leds
0x08004834 0x40023800 - 0x40023bff    1KiB rw-d-  1 rcc_driver
```

(In this case, task 7, `oh_no`, has overflowed its stack -- which
we can see from the `map` output has been sized to only 256 bytes.)

### `humility readmem`

`humility readmem` allows one to read a specified range of memory:

```console
% humility readmem 0x00011b00
humility: attached via DAPLink
humility: reading at 0x11b00 for 256 bytes
             \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
0x00011b00 | 00 0f 1a bf 81 54 bc f1 01 0f d0 bd 02 44 bc f1 | .....T.......D..
0x00011b10 | 02 0f 51 70 00 d1 d0 bd 91 70 d0 bd 69 00 01 00 | ..Qp.....p..i...
0x00011b20 | 04 00 00 00 04 00 00 00 17 01 01 00 6b 00 01 00 | ............k...
0x00011b30 | f1 00 01 00 65 78 70 6c 69 63 69 74 20 70 61 6e | ....explicit pan
0x00011b40 | 69 63 00 00 3c 1f 01 00 49 00 00 00 0a 00 00 00 | ic..<...I.......
0x00011b50 | 09 00 00 00 76 69 76 61 20 65 6c 20 6a 65 66 65 | ....viva el jefe
0x00011b60 | 0a 54 61 73 6b 20 23 20 50 61 6e 69 63 21 0a 00 | .Task # Panic!..
0x00011b70 | 61 1b 01 00 06 00 00 00 67 1b 01 00 08 00 00 00 | a.......g.......
0x00011b80 | 20 42 61 64 20 53 79 73 63 61 6c 6c 20 55 73 61 |  Bad Syscall Usa
0x00011b90 | 67 65 20 0a 61 1b 01 00 06 00 00 00 80 1b 01 00 | ge .a...........
0x00011ba0 | 13 00 00 00 93 1b 01 00 01 00 00 00 20 53 74 61 | ............ Sta
0x00011bb0 | 63 6b 20 6f 76 65 72 66 6c 6f 77 20 61 74 20 61 | ck overflow at a
0x00011bc0 | 64 64 72 65 73 73 20 30 78 00 00 00 61 1b 01 00 | ddress 0x...a...
0x00011bd0 | 06 00 00 00 ac 1b 01 00 1d 00 00 00 93 1b 01 00 | ................
0x00011be0 | 01 00 00 00 20 4d 65 6d 6f 72 79 20 66 61 75 6c | .... Memory faul
0x00011bf0 | 74 20 61 74 20 61 64 64 72 65 73 73 20 30 78 00 | t at address 0x.
```

If an argument is present, it is the number of bytes to read:

```console
$ humility readmem 0x00011d00 100
humility: attached via DAPLink
humility: reading at 0x11d00 for 100 bytes
             \/  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
0x00011d00 | 20 62 6f 75 6e 64 73 3a 20 74 68 65 20 6c 65 6e |  bounds: the len
0x00011d10 | 20 69 73 20 20 62 75 74 20 74 68 65 20 69 6e 64 |  is  but the ind
0x00011d20 | 65 78 20 69 73 20 30 30 30 31 30 32 30 33 30 34 | ex is 0001020304
0x00011d30 | 30 35 30 36 30 37 30 38 30 39 31 30 31 31 31 32 | 0506070809101112
0x00011d40 | 31 33 31 34 31 35 31 36 31 37 31 38 31 39 32 30 | 1314151617181920
0x00011d50 | 32 31 32 32 32 33 32 34 32 35 32 36 32 37 32 38 | 2122232425262728
0x00011d60 | 32 39 33 30                                     | 2930
```

Both arguments can be in either hex, decimal, octal or binary (addresses
and contents will always be printed in hex):

```console
humility readmem 0o216401 0b110
humility: attached via DAPLink
humility: reading at 0x11d01 for 6 bytes
              0 \/  2  3  4  5  6  7  8  9  a  b  c  d  e  f
0x00011d00 |    62 6f 75 6e 64 73                            |  bounds
```

To display as half-words (16-bits) use `-h`; to display as words (32-bits)
use `-w`.  (The addresses must be 2-byte and 4-byte aligned, respectively.)

```console
$ humility readmem -w 0x20000000 0x40
humility: attached via DAPLink
humility: reading at 0x20000000 for 64 bytes
                   \/        4        8        c
0x20000000 | 00000001 20000180 0000000b 00005020 | ....... .... P..
0x20000010 | 00000002 200001f0 00838042 00000000 | ....... B.......
0x20000020 | 00004db8 00004dc8 00004d28 00004d28 | .M...M..(M..(M..
0x20000030 | 00004d28 00004d28 00004d28 00004d28 | (M..(M..(M..(M..
```

A frequent use of `readmem` is to read peripheral memory; as a
convenience, a peripheral name can be used in lieu of an address, provided
that an archive or dump is also specified:

```console
$ humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip readmem -w i2c4
0x30
humility: attached via ST-Link
                   \/        4        8        c
0x58001c00 | 000000f7 00020490 00008000 00008000 | ................
0x58001c10 | 10c0ecff 00000000 00000021 00000000 | ........!.......
0x58001c20 | 00000000 00000080 00000000 00000000 | ................
```

**Note that reading some peripheral memory may have side effects!**

It can also be useful to interpret memory contents symbolically; to do this,
provide a dump or achive and specify the `-s` option, e.g.:

```console
$ humility  -a ~/hubris/target/gemini-bu-rot/dist/build-gemini-bu-rot.zip readmem -s 0x20004b30 0x40
humility: attached via DAPLink
0x20004b30 | 0x20004bdc
0x20004b34 | 0x0000000a <- __EXCEPTIONS+0x2
0x20004b38 | 0x80000000
0x20004b3c | 0x00002e2e <- syscall_entry+0xcf6
0x20004b40 | 0x00000000
0x20004b44 | 0x0003c2e3 <- spi:main+0x5b
0x20004b48 | 0x0003ceda <- spi:sys_send_stub+0xe
0x20004b4c | 0x01000000
0x20004b50 | 0x00000000
0x20004b54 | 0x00000000
0x20004b58 | 0x00000000
0x20004b5c | 0x00000000
0x20004b60 | 0x00000000
0x20004b64 | 0x00000000
0x20004b68 | 0x00000000
0x20004b6c | 0x00000000
```

### `humility readvar`

`humility readvar` allows one to read a global static variable.
To list all such variables, use the `-l` option:

```console
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip readvar -l
humility: MODULE             VARIABLE                       ADDR       SIZE
humility: kernel             CORE_PERIPHERALS               0x20000000 1
humility: kernel             CURRENT_TASK_PTR               0x20000018 4
humility: kernel             DEVICE_PERIPHERALS             0x20000001 1
humility: kernel             FAULT_NOTIFICATION             0x20000004 4
humility: kernel             IRQ_TABLE_BASE                 0x20000010 4
humility: kernel             IRQ_TABLE_SIZE                 0x20000014 4
humility: kernel             TASK_TABLE_BASE                0x20000008 4
humility: kernel             TASK_TABLE_SIZE                0x2000000c 4
humility: kernel             __EXCEPTIONS                   0x08000008 56
humility: kernel             __INTERRUPTS                   0x08000040 620
humility: kernel             __RESET_VECTOR                 0x08000004 4
humility: adt7420            TEMPS_BYMINUTE                 0x2000b848 17288
humility: adt7420            TEMPS_BYSECOND                 0x20008000 14408
```

To read a variable, specify it:

```console
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip readvar CURRENT_TASK_PTR
humility: attached via ST-Link
CURRENT_TASK_PTR (0x20000018) = Some(NonNull<kern::task::Task> {
        pointer: 0x20000558 (*const kern::task::Task)
    })
```

### `humility ringbuf`

`humility ringbuf` reads and displays any Hubris ring buffers (as created
via the `ringbuf!` macro in the Hubris `ringbuf` crate).  e.g.:

```console
% humility -d ./hubris.core.5 ringbuf
ADDR        NDX LINE  GEN    COUNT PAYLOAD
0x2000a288  552   92    1        5 (21.5, 70.69999694824219)
0x2000a298  553   92    1        1 (21.4375, 70.58749389648438)
0x2000a2a8  554   92    1        1 (21.5, 70.69999694824219)
0x2000a2b8  555   92    1        1 (21.4375, 70.58749389648438)
0x2000a2c8  556   92    1        5 (21.5, 70.69999694824219)
0x2000a2d8  557   92    1        1 (21.5625, 70.8125)
0x2000a2e8  558   92    1       15 (21.5, 70.69999694824219)
0x2000a2f8  559   92    1        1 (21.4375, 70.58749389648438)
0x2000a308  560   92    1       10 (21.5, 70.69999694824219)
0x2000a318  561   92    1        2 (21.4375, 70.58749389648438)
0x2000a328  562   92    1        2 (21.5, 70.69999694824219)
0x2000a338  563   92    1        1 (21.4375, 70.58749389648438)
0x2000a348  564   92    1        9 (21.5, 70.69999694824219)
0x2000a358  565   92    1        3 (21.4375, 70.58749389648438)
0x2000a368  566   92    1        4 (21.5, 70.69999694824219)
0x2000a378  567   92    1        1 (21.4375, 70.58749389648438)
...
```

See the `ringbuf` documentation for more details.

### `humility stackmargin`

`humility stackmargin` calculates and print stack margins by task. The
margin is determined by walking up each stack, looking for the first
word that does not contain the uninitialized pattern (`0xbaddcafe`),
from which it infers maximum depth, and therefore margin:

```console
% humility -d ./hubris.core.10 stackmargin
humility: attached to dump
ID TASK                STACKBASE  STACKSIZE   MAXDEPTH     MARGIN
 0 jefe               0x20001000       1024        768        256
 1 rcc_driver         0x20001400       1024        176        848
 2 usart_driver       0x20001800       1024        216        808
 3 user_leds          0x20001c00       1024        208        816
 4 ping               0x20002000        512        224        288
 5 pong               0x20002400       1024        208        816
 6 idle               0x20002800        256        104        152
```

Note that the margin is only valid for the task's lifetime -- and in
particular, will not be correct if the task has restarted due to a
stack overflow!

### `humility dump`

`humility dump` takes a dump of the attached system, writing out an ELF
core file:

```console
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip dump
humility: attached via ST-Link
humility: core halted
humility: dumping to hubris.core.0
humility: dumped 1.12MB in 24 seconds
humility: core resumed
```

A dump file name may also be specified:

```console
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip dump hubris.core.`date +%s`
humility: attached via ST-Link
humility: core halted
humility: dumping to hubris.core.1600718079
humility: dumped 1.12MB in 24 seconds
humility: core resumed
```

The resulting dump can be used with many commands (including `manifest`,
`map`, `readvar`, and `tasks`) -- and need not be run on the same machine
as the debugged MCU, e.g.:

```console
% humility -d hubris.core.0 tasks
humility: attached to dump
ID ADDR     TASK               GEN STATE
 0 20000168 jefe                 0 Healthy(InRecv(None))
 1 200001d8 rcc_driver           0 Healthy(InRecv(None))
 2 20000248 gpio_driver          0 Healthy(InRecv(None))
 3 200002b8 usart_driver         0 Healthy(InRecv(None))
 4 20000328 i2c_driver           0 Healthy(InRecv(None))
 5 20000398 user_leds            0 Healthy(InRecv(None))
 6 20000408 pong                 0 Healthy(InRecv(None))
 7 20000478 ping                40 Healthy(InReply(TaskId(0x3)))
 8 200004e8 adt7420              0 Healthy(InRecv(Some(TaskId(0xffff))))
 9 20000558 idle                 0 Healthy(Runnable)          <-
```

### `humility apptable`

Hubris encodes the applications at build time by creating a
`.hubris_app_table` section in the kernel ELF binary.  `humility apptable`
allows this table to be printed and formatted.  As with other Humility
commands, `humility apptable` can run on an archive (or dump):

```console
% humility -a ~/hubris/target/tests-lpc55/dist/build-tests-lpc55.zip apptable
App = {
        magic: 0x1defa7a1,
        task_count: 0x4,
        region_count: 0x9,
        irq_count: 0x0,
        fault_notification: 0x1,
        zeroed_expansion_space: [
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0
        ]
    }

RegionDesc[0x0] = {
        base: 0x0,
        size: 0x20,
        attributes: RegionAttributes {
            bits: 0x0
        },
        reserved_zero: 0x0
    }
...
```

`humility apptable` can also operate directly on a kernel in lieu of an
archive or dump by providing the kernel ELF file as an argument:

```console
% humility apptable ~/hubris/target/demo/dist/kernel
App = {
        magic: 0x1defa7a1,
        task_count: 0x7,
        region_count: 0x13,
        irq_count: 0x1,
        fault_notification: 0x1,
        zeroed_expansion_space: [
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0,
            0x0
        ]
    }
...
```

### `humility test`

When run against a test archive, `humility test` kicks off the test suite
and parses its results via ITM.

```console
humility: attached via ST-Link
humility: ITM synchronization packet found at offset 6
humility: expecting 22 cases
humility: running test_send ... ok
humility: running test_recv_reply ... ok
humility: running test_fault_badmem ... ok
humility: running test_fault_stackoverflow ... ok
humility: running test_fault_execdata ... ok
humility: running test_fault_illop ... ok
humility: running test_fault_nullexec ... ok
humility: running test_fault_textoob ... ok
humility: running test_fault_stackoob ... ok
humility: running test_fault_buserror ... ok
humility: running test_fault_illinst ... ok
humility: running test_fault_divzero ... ok
humility: running test_panic ... ok
humility: running test_restart ... ok
humility: running test_restart_taskgen ... ok
humility: running test_borrow_info ... ok
humility: running test_borrow_read ... ok
humility: running test_borrow_write ... ok
humility: running test_supervisor_fault_notification ... ok
humility: running test_timer_advance ... ok
humility: running test_timer_notify ... ok
humility: running test_timer_notify_past ... ok
humility: tests completed: pass
```

If a test fails, this will also create a complete report, e.g.:

```console
humility: attached via ST-Link
humility: ITM synchronization packet found at offset 6
humility: expecting 22 cases
humility: running test_send ... ok
humility: running test_recv_reply ... fail
humility: running test_fault_badmem ... ok
...
humility: running test_timer_notify_past ... ok
humility: tests completed: fail
humility: test output dumped to hubris.testout.15
```

This output file will have (among other things) a section that has
complete test run information.  For details on a failing test, look
for `result: Fail`:

```console
$ cat hubris.testout.15
...
==== Test results
[
    ...
    TestCompletion {
        case: "test_recv_reply",
        result: Fail,
        log: [
            (
                UserLog,
                "assistant starting",
            ),
            (
                KernelLog,
                "task @1 panicked: panicked at \'assertion failed: false\', test/test-suite/src/main.rs:124:5",
            ),
            (
                UserLog,
                "Task #1 Panic!",
            ),
            (
                UserLog,
                "assistant starting",
            ),
        ],
    },
    ...
```

This shows the sequential ordering of all log messages while running the
test.  The test report can also be useful even when tests pass; to always
dump a test report, use the `-d` option to `humility test`.

Note that `humility test` relies on the ability to keep up with ITM data,
which can be lossy.  In the event ITM data is lost, the failure mode is
unlikely to be a failing test, but rather a fatal error due to a misframed
packet:

```console
humility: running test_fault_nullexec ... ok
humility: running test_fault_textoob ... ok
humility: running test_fault_stackoob ... humility: test output dumped to hubris.testout.21
humility: test failed: malformed datum: 0x74
Error: test failed
```

All received packet data will be dumped to the resulting output file, allowing
these transient failures to be differentiated from deeper issues.

### `humility i2c`

On platforms that have I<sup>2</sup>C support, `humility i2c` can be used to
scan a bus, scan a device, read a register, or write a register.  Its usage
will be specific to the board being examined; to specify a controller use the
`-c`, to specify a port (if necessary), use `-p`; to specify a mux and
segment (if necessary), use `-m`.

For example, on a Gimlet-let, here is a scan of controller I2C3, revealing
one device at address `0x48`:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -s -c 3
humility: attached via ST-Link

Device scan on controller I2C3:

    R = Reserved   - = No device   \o/ = Device found   X = Timed out

ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00       R   R   R   R   R   R   R   R   -   -   -   -   -   -   -   -
0x10       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x20       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x30       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x40       -   -   -   -   -   -   -   - \o/   -   -   -   -   -   -   -
0x50       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x60       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x70       -   -   -   -   -   -   -   -   -   -   -   -   R   R   R   R
```

To scan that device, specify its address via `-d`:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -s -c 3 -d 0x48
humility: attached via ST-Link

Register scan for device 0x48 on I2C3:

      - = No register        ! = No device        X = Timed out

ADDR  0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00   0b  c8  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0x10   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0x20   0b  c8  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0x30   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0x40   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0x50   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0x60   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0x70   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0x80   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0x90   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0xa0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0xb0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0xc0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0xd0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
0xe0   00  00  80  00  20  00  05  00  49  80  05  cb  00  00  00  00
0xf0   00  00  00  00  00  00  00  00  6c  00  06  00  00  00  00  00
```

(This device is an ADT7420 temp sensor.)  To look at a particular register,
elide `-s` and specify the register of interest via `-r`:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 3 -d 0x48 -r 0xb
humility: attached via ST-Link
Controller I2C3, device 0x48, register 0xb = 0xcb
```

To write a value to a register, specify the `-w` flag, along with the value to
write, e.g. (for the ADT7420), the MSB of the T<sub>HIGH</sub> register:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 3 -d 0x48 -r 0x4 -w 0x1f
humility: attached via ST-Link
Controller I2C3, device 0x48, register 0x4 = 0x1f
```

Note that if registers are not writable, the write will (generally) be silently
discarded by the device; it can be useful to read the register after writing
it to confirm that the value is as expected:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 3 -d 0x48 -r 0x4
humility: attached via ST-Link
Controller I2C3, device 0x48, register 0x4 = 0x1f
```

#### Errors

If there are errors in executing the requested `i2c` operation(s), these will
appear in the output, e.g.:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 5 -s
humility: attached via ST-Link

Device scan on controller I2C5:

    R = Reserved   - = No device   \o/ = Device found   X = Timed out

ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00       R   R   R   R   R   R   R   R Err Err Err Err Err Err Err Err
0x10     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x20     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x30     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x40     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x50     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x60     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x70     Err Err Err Err Err Err Err Err Err Err Err Err   R   R   R   R
```

More details about these errors can be found by reading the `I2C_DEBUG_RESULTS`
variable:

```console
humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip readvar I2C_DEBUG_RESULTS
humility: attached via ST-Link
I2C_DEBUG_RESULTS (0x20004400) = [
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(BadController)),
    Some(Err(BadController)),
    Some(Err(BadController)),
    Some(Err(BadController)),
    Some(Err(BadController)),
    Some(Err(BadController)),
    ...
```

Note that a common source of errors will be not specifying a port when it is
required, e.g. on I2C4 on the Gemini bringup board:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip i2c -c 4 -s
humility: attached via ST-Link

Device scan on controller I2C4:

    R = Reserved   - = No device   \o/ = Device found   X = Timed out

ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00       R   R   R   R   R   R   R   R Err Err Err Err Err Err Err Err
0x10     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x20     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x30     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x40     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x50     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x60     Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err Err
0x70     Err Err Err Err Err Err Err Err Err Err Err Err   R   R   R   R
```

In this case, reading `I2C_DEBUG_RESULTS` shows that a port must be provided:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip readvar I2C_DEBUG_RESULTS | head -20
humility: attached via ST-Link
I2C_DEBUG_RESULTS (0x20006400) = [
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(ReservedAddress)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    Some(Err(BadDefaultPort)),
    ...
```

Correcting this, to (e.g.) scan port F:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip i2c -c 4 -p f -s
humility: attached via ST-Link

Device scan on controller I2C4:

    R = Reserved   - = No device   \o/ = Device found   X = Timed out

ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00       R   R   R   R   R   R   R   R   -   -   -   -   -   -   -   -
0x10       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x20       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x30       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x40       -   -   -   - \o/   -   -   -   -   -   -   -   -   -   -   -
0x50       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x60       -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
0x70       -   -   -   -   -   -   -   -   -   -   -   -   R   R   R   R
```

In some cases (particularly with bad input), a command may fail to execute
entirely.  In these cases, use the `-l` option to also enable ITM-based logging:

```console
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 10 -s
humility: attached via ST-Link
humility: i2c failed: i2c command failed on target; run with -l for more detail
% humility -a ~/hubris/target/gimletlet/dist/build-gimletlet.zip i2c -c 10 -s -l
humility: attached via ST-Link
humility: ITM synchronization packet found at offset 6
i2c_debug: invalid controller value 10
humility: i2c failed: i2c command failed on target
%
```

The cohort in the Hubris image responsible for executing the operations
requested by the `i2c` command is the `i2c_debug` task; if this task is
incapacitated, the operation may time out:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip i2c -c 4 -p f -s
humility: attached via ST-Link
humility: operation timed out

Device scan on controller I2C4:

    R = Reserved   - = No device   \o/ = Device found   X = Timed out

ADDR     0x0 0x1 0x2 0x3 0x4 0x5 0x6 0x7 0x8 0x9 0xa 0xb 0xc 0xd 0xe 0xf
0x00       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x10       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x20       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x30       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x40       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x50       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x60       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
0x70       X   X   X   X   X   X   X   X   X   X   X   X   X   X   X   X
```

In this case, check this task:

```console
% humility -a ~/hubris/target/gemini-bu/dist/build-gemini-bu.zip tasks -sl i2c_debug
humility: attached via ST-Link
ID ADDR     TASK               GEN PRI STATE
10 200008b0 i2c_debug            0   3 Healthy(InSend(TaskId(0x4)))
   |
   +--->  0x20006330 0x08028ea0 sys_send_stub()
                     @ /home/bmc/hubris/userlib/src/lib.rs:125
          0x20006388 0x080280be read<u8>()
                     @ /home/bmc/hubris/drv/i2c-api/src/lib.rs:350
          0x20006400 0x08028644 scan_controller()
                     @ /home/bmc/hubris/task-i2c/src/main.rs:77
          0x20006400 0x08028668 main()
                     @ /home/bmc/hubris/task-i2c/src/main.rs:183

```

(In this case, the stack trace shows that it is blocked on the `i2c_driver`
itself; the next step in debugging this would be determining the state of
that task.)

### `humility stmsecure`

Humility has support to manage the Root Security Services (RSS) and various
flash options bits

A typical sequence to set the secure region at 0x08000000

```
humility stmsecure set-secure-bit
humility stmsecure set-secure-region 0x08000000 0xa000
```

To undo the secure region:

```
humility stmsecure unset-secure-region
humility stmsecure unset-secure-bit
```

The STM32 has support for flash bank swapping as well

```
humility stmsecure bank-swap
```

