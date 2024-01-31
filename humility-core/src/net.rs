// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use anyhow::{bail, Context, Result};
use std::{fmt, net::Ipv6Addr, str::FromStr};

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct ScopedV6Addr {
    pub ip: Ipv6Addr,
    pub scopeid: u32,
}

const SCOPE_DELIM: char = '%';

impl FromStr for ScopedV6Addr {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let s = s.trim();
        let Some((ip, scope)) = s.rsplit_once(SCOPE_DELIM) else {
            bail!(
                "missing scope ID (e.g. \"{SCOPE_DELIM}en0\") in IPv6 address"
            )
        };
        let ip = ip
            .parse()
            .with_context(|| format!("{ip:?} is not a valid IPv6 address"))?;
        let scopeid = decode_iface(scope)?;
        Ok(ScopedV6Addr { ip, scopeid })
    }
}

impl fmt::Display for ScopedV6Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self { ip, scopeid } = self;
        write!(f, "{ip}{SCOPE_DELIM}{scopeid}")
    }
}

impl fmt::Debug for ScopedV6Addr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

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
