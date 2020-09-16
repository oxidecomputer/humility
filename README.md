
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
is connected to a host via USB, e.g.  ST's STLink/V2 on the STM32F407
Discovery or the LPC-Link2 present on the LPCXpresso55S69.  (When we deploy
Hubris on our own board, we will debug via either JTAG or a SWD header.)
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

- `usb`: Attach directly via USB to a debug probe

### Archive

Many Humility commands require the complete Hubris archive.  This is a ZIP
archive created by the build process, and includes all binaries as well as the
`app.toml` file used to configure the Hubris archive.  The Hubris archive is
specified via the `-a` option or the `HUMILITY_ARCHIVE` environment variable.

## Commands

### `humility manifest`

`humility manifest` displays information about the Hubris archive.  It
does not connect at all to a Hubris target to operate.  In addition to
archive-wide attributes, `humility manifest` displays the features enabled
for each task, e.g.:

```
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

### `humility probe`

`humility probe` attempts to infer as much about the hardware state as it
can, e.g.:

```
% humility probe
humility: attached via ST-Link
humility:         core => Cortex-M7
humility: manufacturer => STMicroelectronics
humility:         chip => STM32H7A3/STM32H7B3/STM32H7B0, revision 0x1001
humility:  debug units => CSTF CTI DWT ETM FPB ITM SCS SWO TMC TPIU
humility:       status => executing
humility:          ITM => TRCENA enabled, TCR enabled, TER=0x3
humility:           R0 => 0x20002a00 
humility:           R1 => 0x8018038  
humility:           R2 => 0x20002a00 
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
humility:           SP => 0x20002b00 
humility:           LR => 0xffffffff 
humility:           PC => 0x8018020  
humility:         xPSR => 0x61000000 
humility:          MSP => 0x20000f48 
humility:          PSP => 0x20002b00 
humility:          SPR => 0x7000000  
```

If provided a Hubris archive, `humility probe` will display any register
contents symbolically, e.g.:

```
% humility -a ~/hubris/target/demo/dist/build-demo.zip probe
humility: attached via ST-Link
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

```
% humility -a ~/hubris/target/lpc55/dist/build-lpc55.zip tasks
humility: attached via DAPLink
ID ADDR     TASK               GEN STATE
 0 20000160 jefe                 0 Healthy(InRecv(None))
 1 200001d0 idle                 0 Healthy(Runnable)          <-
 2 20000240 syscon_driver        0 Healthy(InRecv(None))
 3 200002b0 gpio_driver          0 Healthy(InRecv(None))
 4 20000320 user_leds            0 Healthy(InRecv(None))
 5 20000390 usart_driver         0 Healthy(InRecv(None))
 6 20000400 i2c_driver           0 Healthy(InRecv(None))
 7 20000470 ping                11 Healthy(InReply(5))
 8 200004e0 pong                 0 Healthy(InRecv(None))
 9 20000550 spam                 0 Healthy(Stopped)
```

To see every field in each task, you can use the `-v` flag:

```
% humility -a ~/hubris/target/demo/dist/build-demo.zip tasks -v
...
 4 200002c8 pong                 0 Healthy(InRecv(None))
          |
          +----> {
                    save: {
                        r4: 0x200023bc,
                        r5: 0x10,
                        r6: 0x1,
                        r7: 0x200023f0,
                        r8: 0x40020c00,
                        r9: 0x2491b8,
                        r10: 0x200023bc,
                        r11: 0x1,
                        psp: 0x20002328,
                        exc_return: 0xffffffed
                    },
                    priority: 0x2,
                    state: Healthy(InRecv(None)),
                    timer: {
                        deadline: Some(0x2491b8),
                        to_post: 0x1
                    },
                    generation: 0x0,
                    region_table: {
                        data_ptr: 0x200000a8 (*const &abi::RegionDesc),
                        length: 0x8
                    },
                    notifications: 0x0,
                    descriptor: 0x80047c4 (&abi::TaskDesc)
                }
...
```

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

```
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

```
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

### `humility readvar`

`humility readvar` allows one to read a global static variable.
To list all such variables, use the `-l` option:

```
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

```
% humility -a ~/hubris/target/demo-stm32h7b3/dist/build-demo-stm32h7b3.zip readvar CURRENT_TASK_PTR
humility: attached via ST-Link
CURRENT_TASK_PTR (0x20000018) = Some(NonNull<kern::task::Task> {
        pointer: 0x20000558 (*const kern::task::Task)
    })
```

### `humility itm`

### `humility etm`

## Future work

### `humility log`

### `humility core`

### `humility stacks`

### `humility test`

