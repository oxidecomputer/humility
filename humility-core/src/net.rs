// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Result};

pub fn decode_iface(iface: &str) -> Result<u32> {
    #[cfg(not(windows))]
    use libc::if_nametoindex;
    #[cfg(windows)]
    use winapi::shared::netioapi::if_nametoindex;

    // Work around https://github.com/rust-lang/rust/issues/65976 by manually
    // converting from scopeid to numerical value.  I'm not any happier about
    // this than you are!
    let iface_c = std::ffi::CString::new(iface).unwrap();
    let scopeid: u32 = iface
        .parse()
        .unwrap_or_else(|_| unsafe { if_nametoindex(iface_c.as_ptr()) });
    if scopeid == 0 {
        bail!("Could not find interface for {}", iface);
    }
    Ok(scopeid)
}
