// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::arch::ARMRegister;
use humility::hubris::*;

#[derive(Debug, Default)]
pub struct StackPrinter {
    /// Indentation
    pub indent: usize,

    /// Print line information
    pub line: bool,

    /// Caller will hang additional output below stack
    pub additional: bool,
}

impl StackPrinter {
    pub fn print(&self, hubris: &HubrisArchive, stack: &[HubrisStackFrame]) {
        let indent = self.indent;
        print!("{:indent$}|\n{:indent$}+--->  ", "", "");

        let bar = if self.additional { "|" } else { " " };
        let right = 6;

        let print_indent = || {
            print!("{:indent$}{}{:right$}", "", bar, "");
        };

        for (ndx, frame) in stack.iter().enumerate() {
            let pc = frame.registers.get(&ARMRegister::PC).unwrap();

            if let Some(ref inlined) = frame.inlined {
                for inline in inlined {
                    println!(
                        "0x{:08x} 0x{:08x} {}",
                        frame.cfa, inline.addr, inline.name
                    );

                    print_indent();

                    if self.line {
                        if let Some(src) = hubris.lookup_src(inline.origin) {
                            println!(
                                "{:11}@ {}:{}",
                                "",
                                src.fullpath(),
                                src.line
                            );
                            print_indent();
                        }
                    }
                }
            }

            if let Some(sym) = frame.sym {
                println!(
                    "0x{:08x} 0x{:08x} {}",
                    frame.cfa, *pc, sym.demangled_name
                );

                if self.line {
                    if let Some(src) = hubris.lookup_src(sym.goff) {
                        print_indent();
                        println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                    }
                }
            } else {
                println!("0x{:08x} 0x{:08x}", frame.cfa, *pc);
            }

            if ndx + 1 < stack.len() {
                print_indent();
            }
        }

        if self.additional {
            println!("{:indent$}{}", "", bar);
        } else {
            println!();
        }
    }
}
