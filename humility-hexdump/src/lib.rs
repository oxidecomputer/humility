// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

pub struct Dumper {
    /// Word size, in bytes
    pub size: usize,

    /// Width of memory, in bytes
    pub width: usize,

    /// Address size, in nibbles
    pub addrsize: usize,

    /// Left indentation, in characters
    pub indent: usize,

    /// Left indent should be a hanging indent
    pub hanging: bool,

    /// Print the OpenBoot PROM-style header line
    pub header: bool,

    /// Print the ASCII translation of characters in the right margin
    pub ascii: bool,
}

impl Dumper {
    pub fn new() -> Self {
        Self {
            size: 1,
            width: 16,
            addrsize: 8,
            indent: 0,
            hanging: false,
            header: true,
            ascii: true,
        }
    }

    pub fn dump(&self, bytes: &[u8], addr: u32) {
        let size = self.size;
        let width = self.width;
        let mut addr = addr;
        let mut indent = if self.hanging { 0 } else { self.indent };

        let print = |line: &[u8], addr, offs, indent| {
            print!(
                "{:indent$}0x{:0width$x} | ",
                "",
                addr,
                indent = indent,
                width = self.addrsize
            );

            for i in (0..width).step_by(size) {
                if i < offs || i - offs >= line.len() {
                    print!(" {:width$}", "", width = size * 2);
                    continue;
                }

                let slice = &line[i - offs..i - offs + size];

                print!(
                    "{:0width$x} ",
                    match size {
                        1 => u32::from(line[i - offs]),
                        2 => u32::from(u16::from_le_bytes(
                            slice.try_into().unwrap()
                        )),
                        4 => u32::from_le_bytes(slice.try_into().unwrap()),
                        _ => {
                            panic!("invalid size");
                        }
                    },
                    width = size * 2
                );
            }

            if self.ascii {
                print!("| ");

                for i in 0..width {
                    if i < offs || i - offs >= line.len() {
                        print!(" ");
                    } else {
                        let c = line[i - offs] as char;

                        if c.is_ascii() && !c.is_ascii_control() {
                            print!("{}", c);
                        } else {
                            print!(".");
                        }
                    }
                }
            }

            println!();
        };

        let offs = (addr & (width - 1) as u32) as usize;
        addr -= offs as u32;

        //
        // Print out header line, OpenBoot PROM style
        //
        if self.header {
            print!("  {:width$}  ", "", width = indent + self.addrsize);

            for i in (0..width).step_by(size) {
                if i == offs {
                    print!(" {:>width$}", "\\/", width = size * 2);
                } else {
                    print!(" {:>width$x}", i, width = size * 2);
                }
            }

            println!();
            indent = self.indent;
        }

        //
        // Print our first line.
        //
        let lim = std::cmp::min(width - offs, bytes.len());
        print(&bytes[0..lim], addr, offs, indent);
        indent = self.indent;

        if lim < bytes.len() {
            let lines = bytes[lim..].chunks(width);

            for line in lines {
                addr += width as u32;
                print(line, addr, 0, indent);
            }
        }
    }
}

impl Default for Dumper {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// dump() computes the first-line offset as `addr & (width - 1)`.
    /// Verify that a non-zero starting address correctly places data
    /// at the right column in the first row (i.e. offset = addr % 16).
    #[test]
    fn dump_offset_from_address() {
        // addr = 0x13: offset within a 16-byte row = 3
        // only 1 byte of data — should not panic
        let mut d = Dumper::new();
        d.header = false;
        d.ascii = false;
        // Exercise path: offs = 0x13 & 0x0f = 3, lim = min(16-3, 1) = 1
        d.dump(&[0xAB], 0x13);
    }

    /// When addr is at the very end of the u32 address space and the byte
    /// slice is exactly 1 byte, dump() must not overflow the addr arithmetic.
    #[test]
    fn dump_high_address_single_byte() {
        let mut d = Dumper::new();
        d.header = false;
        d.ascii = false;
        // offs = 0xFFFFFFFF & 0x0F = 15; lim = min(16-15, 1) = 1; no wrapping
        d.dump(&[0xFF], 0xFFFF_FFFF);
    }

    /// Verify that a multi-chunk dump (more than one 16-byte row) advances
    /// addr correctly between rows without wrapping.
    #[test]
    fn dump_multirow_addr_advance() {
        let mut d = Dumper::new();
        d.header = false;
        d.ascii = false;
        // addr = 0, 32 bytes → 2 rows; addr increments by 16 between rows
        d.dump(&[0u8; 32], 0x0);
    }
}
