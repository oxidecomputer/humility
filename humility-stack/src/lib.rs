// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use humility::hubris::*;
use humility_arch_arm::ARMRegister;

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

        // Generated code has long paths; we'll patch them up here
        let pattern =
            regex::Regex::new(r"/hubris/target/[a-zA-Z0-9\-]+/release/build/")
                .unwrap();
        let fixup_file = |input| pattern.replace_all(input, "/build/");

        for (ndx, frame) in stack.iter().enumerate() {
            let pc = frame.registers.get(&ARMRegister::PC).unwrap();

            if let Some(pos) = &frame.pos {
                for (i, p) in pos.iter().enumerate() {
                    println!("0x{:08x} 0x{:08x} {}", frame.cfa, *pc, p.func);
                    if self.line {
                        print_indent();
                        println!(
                            "{:11}@ {}:{}:{}",
                            "",
                            fixup_file(&p.file),
                            p.line,
                            p.col
                        );
                    }
                    if ndx + 1 < stack.len() || i + 1 < pos.len() {
                        print_indent();
                    }
                }
            } else {
                if let Some(ref inlined) = frame.inlined {
                    for inline in inlined {
                        println!(
                            "0x{:08x} 0x{:08x} {}",
                            frame.cfa, inline.addr, inline.name
                        );
                        print_indent();

                        if self.line {
                            if let Some(src) = hubris.lookup_src(inline.origin)
                            {
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

                if let Some(sym) = &frame.sym {
                    println!("0x{:08x} 0x{:08x} {}", frame.cfa, *pc, sym.name);
                } else {
                    println!("0x{:08x} 0x{:08x}", frame.cfa, *pc);
                }

                if self.line {
                    if let Some(src) = frame
                        .sym
                        .as_ref()
                        .and_then(|s| s.goff)
                        .and_then(|g| hubris.lookup_src(g))
                    {
                        print_indent();
                        println!("{:11}@ {}:{}", "", src.fullpath(), src.line);
                    }
                }
                if ndx + 1 < stack.len() {
                    print_indent();
                }
            }
        }

        if self.additional {
            println!("{:indent$}{}", "", bar);
        } else {
            println!();
        }
    }
}
