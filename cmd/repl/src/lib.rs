// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility repl`
//!
//! `humility repl` is an interactive prompt that you can use with humility.
//!
//! This allows you to run several commands in succession without needing to
//! type in some core settings over and over again.
//!
//! `humility repl` takes the same top level arguments as any other subcommand,
//! and will remember them inside of the prompt. For example:
//!
//! ```console
//! $ humility -a ../path/to/hubris/archive.zip repl
//! humility: attached via ST-Link V2-1
//! Welcome to the humility REPL! Try out some subcommands, or 'quit' to quit!
//! humility> tasks
//! system time = 7209837
//! ID TASK                 GEN PRI STATE
//!  0 jefe                   0   0 recv, notif: bit0 bit1(T+63)
//!  1 rcc_driver             0   1 recv
//!  2 usart_driver           0   2 RUNNING
//!  3 user_leds              0   2 recv
//!  4 ping               47524   4 wait: reply from usart_driver/gen0
//!  5 pong                   0   3 recv, notif: bit0(T+163)
//!  6 hiffy                  0   3 notif: bit31(T+121)
//!  7 idle                   0   5 ready
//!
//! humility> tasks
//! system time = 7212972
//! ID TASK                 GEN PRI STATE
//!  0 jefe                   0   0 recv, notif: bit0 bit1(T+28)
//!  1 rcc_driver             0   1 recv
//!  2 usart_driver           0   2 recv, notif: bit0(irq38)
//!  3 user_leds              0   2 recv
//!  4 ping               47544   4 RUNNING
//!  5 pong                   0   3 recv, notif: bit0(T+28)
//!  6 hiffy                  0   3 notif: bit31(T+252)
//!  7 idle                   0   5 ready
//!
//! humility> quit
//! Quitting!
//! ```
//!
//! As you can see, we can run the `tasks` subcommand twice, without passing our
//! archive each time. In the output above, you can see the ping task faulting
//! in the background; your code is still running in the background while you
//! use the repl!
//!
//! Finally, as you can see, `quit` will quit the repl. There is also a
//! `history` command, which will show you recent commands you've put into the
//! prompt.
//!
//! The repl is still very early days! We'll be adding more features in the
//! future.
