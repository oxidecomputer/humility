// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility net`
//! `humility net` exposes commands to interact with the management network from
//! the client's perspective.
//!
//! (It is the counterpart to `humility monorail`, which interacts with the
//!  management network from the _switch_'s perspective.)
//!
//! It is fully functional on
//! - Gimlet
//! - Sidecar
//! - PSC
//! - `gimletlet-mgmt`
//!
//! These PCAs have the KSZ8463 switch + VSC85x2 PHY which is our standard
//! management network interface.
//!
//! ### `humility net ip`
//! The `ip` subcommand works on any image with network functionality, and
//! prints the MAC and IPv6 address of the board.  Note that on boards with
//! multiple ports, this is the lowest MAC / IPv6 address.
//!
//! ### `humility net mac`
//! This subcommand prints the MAC table from the KSZ8463 switch.  It is
//! functional on the boards listed above *and* the Gimletlet (when the NIC
//! daughterboard is installed)
//!
//! ### `humility net status`
//! This subcommand shows the status of the management network links.  It is
//! only functional on the fully supported boards listed above.
//!
//! ### `humility net counters`
//! This subcommand shows the internal counters on the management network links.
//! It is only functional on the fully supported boards listed above.  In
//! addition, the MAC-side counters are only supported on the VSC8562, not the
//! VSC8552; this is indicated with `--` in the relevant table positions.
use std::collections::BTreeMap;

use anyhow::Result;
use clap::Parser;
use colored::Colorize;

use humility::core::Core;
use humility::hubris::HubrisArchive;
use humility::reflect::*;
use humility_cli::{ExecutionContext, humility_cmd};
use humility_hiffy::HiffyContext;
use humility_idol::HubrisIdol;

#[derive(Parser, Debug)]
enum NetCommand {
    /// Dump the KSZ8463's MAC table
    Mac,
    /// Print the IP and MAC address
    Ip,
    /// Print the link status
    Status,
    /// Print the counters
    ///
    /// This is a destructive operation that clears counter values, resetting
    /// them to zero.
    ///
    /// If no flags are provided, both the table and diagram are shown.
    Counters {
        /// Show a table of raw counter values
        #[clap(short, long)]
        table: bool,
        /// Show a diagram of good packet counts in context
        #[clap(short, long)]
        diagram: bool,
    },
}

#[derive(Parser, Debug)]
#[clap(name = "net", about = env!("CARGO_PKG_DESCRIPTION"))]
pub struct NetArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        value_parser = parse_int::parse::<u64>
    )]
    timeout: u64,

    #[clap(subcommand)]
    cmd: NetCommand,
}

fn net_ip(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut hiffy_context: HiffyContext,
) -> Result<()> {
    let op = hubris.get_idol_command("Net.get_mac_address")?;

    let v = humility_hiffy::hiffy_call::<humility::reflect::Tuple>(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
        None,
    )?;
    assert_eq!(v.name(), "MacAddress");
    let mac = v.field::<[u8; 6]>(0)?;
    print!("{}:  ", "MAC address".bold());
    for (i, byte) in mac.iter().enumerate() {
        if i > 0 {
            print!(":");
        }
        print!("{byte:02x}");
    }
    println!();

    // MAC to IPv6 link-local address
    print!("{}: {}", "IPv6 address".bold(), mac_to_ip6(mac));
    println!();
    Ok(())
}

pub fn mac_to_ip6(mac: [u8; 6]) -> String {
    let mut out = "fe80::".to_string();
    for (i, byte) in mac.iter().enumerate() {
        if i == 2 || i == 4 {
            out += ":";
        }
        out +=
            &format!("{:02x}", if i == 0 { *byte ^ (1 << 1) } else { *byte });
        if i == 2 {
            out += "ff:fe";
        }
    }
    out
}

fn net_mac_table(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut hiffy_context: HiffyContext,
) -> Result<()> {
    let op_mac_count = hubris.get_idol_command("Net.read_ksz8463_mac_count")?;

    // We need to make two HIF calls:
    // - Read the number of entries in the MAC table
    // - Loop over the table that many times, reading entries
    let mac_count = humility_hiffy::hiffy_call::<u32>(
        hubris,
        core,
        &mut hiffy_context,
        &op_mac_count,
        &[],
        None,
        None,
    )?;

    // Due to HIF limitations (lack of Expand16 / Collect16), we're going to
    // limit ourselves to 255 MAC table entries.
    let (mac_count, clipped) = if mac_count > u8::MAX as u32 {
        (u8::MAX, true)
    } else {
        (mac_count as u8, false)
    };

    humility::msg!("Reading {mac_count} MAC addresses...");

    let op = hubris.get_idol_command("Net.read_ksz8463_mac")?;
    let send = hiffy_context.get_function("Send", 4)?;

    use hif::*;
    let label = Target(0);

    let ops = vec![
        Op::Push32(op.task.task()),
        Op::Push16(op.code),
        Op::Push(0), // LSB of u16
        Op::Push(0), // Dumy counter byte
        Op::Label(label),
        // BEGIN LOOP
        Op::Drop,
        Op::Push(0), // MSB of u16
        Op::Push(2), // Payload length
        Op::Push(8), // Return size
        Op::Call(send.id),
        Op::DropN(3), // Drop everything until the LSB
        // Loop iteration is now at the top of the stack
        Op::Push(1), // Prepare to increment
        Op::Add,     // i = i + 1
        Op::Push(mac_count),
        Op::BranchGreaterThan(label),
        // END LOOP
        Op::DropN(3),
        Op::Done,
    ];

    let results = hiffy_context.run(core, ops.as_slice(), None)?;
    let results = results
        .into_iter()
        .map(move |r| {
            humility_hiffy::hiffy_decode::<humility::reflect::Struct>(
                hubris, &op, r,
            )
        })
        .collect::<Vec<_>>();

    let mut mac_table: BTreeMap<u16, Vec<[u8; 6]>> = BTreeMap::new();
    for r in results {
        if let Ok(s) = r {
            assert_eq!(s.name(), "KszMacTableEntry");
            let port = s.field::<u16>("port")?;
            let mac = s.field::<[u8; 6]>("mac")?;
            if mac == [0; 6] && port == 0xFFFF {
                humility::msg!("Skipping empty MAC address");
            } else {
                mac_table.entry(port).or_default().push(mac);
            }
        } else {
            // Log the error but keep going for other entries in the table
            humility::msg!("Got error result: {r:?}");
        }
    }
    println!(" {} |        {}", "PORT".bold(), "MAC".bold());
    println!("------|-------------------");
    for (port, macs) in &mac_table {
        for (i, mac) in macs.iter().enumerate() {
            if i == 0 {
                print!("{:>5} | ", port);
            } else {
                print!("      | ");
            }
            for (i, m) in mac.iter().enumerate() {
                if i > 0 {
                    print!(":");
                }
                print!("{:02x}", m);
            }
            println!();
        }
    }
    if clipped {
        println!(" ...  | ...");
    }

    Ok(())
}

fn net_status(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut hiffy_context: HiffyContext,
) -> Result<()> {
    let op = hubris.get_idol_command("Net.management_link_status")?;

    let s = humility_hiffy::hiffy_call::<humility::reflect::Struct>(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
        None,
    )?;
    assert_eq!(s.name(), "ManagementLinkStatus");

    let ksz_100base_fx: Vec<bool> = s.field("ksz8463_100base_fx_link_up")?;
    let vsc_100base_fx: Vec<bool> = s.field("vsc85x2_100base_fx_link_up")?;
    let vsc_sgmii: Vec<bool> = s.field("vsc85x2_sgmii_link_up")?;

    let up_down = |b| {
        if b { " UP ".green() } else { "DOWN".red() }
    };

    // ASCII-art drawing of the network
    println!(
        "          {}              {}",
        "------------------".dimmed(),
        "---------------------".dimmed(),
    );
    println!(
        "          {}           {} 1 <----------> 0 {}         {} 0 <------>",
        "|".dimmed(),
        up_down(ksz_100base_fx[0]),
        up_down(vsc_100base_fx[0]),
        up_down(vsc_sgmii[0])
    );
    println!(
        "  {1} <--> 3  {2}       {0}              {0}      {3}      {0}",
        "|".dimmed(),
        "SP".bold(),
        "KSZ8463".bold(),
        "VSC85x2".bold()
    );
    println!(
        "     RMII {}           {} 2 <----------> 1 {}         {} 1 <------>",
        "|".dimmed(),
        up_down(ksz_100base_fx[1]),
        up_down(vsc_100base_fx[1]),
        up_down(vsc_sgmii[1])
    );
    println!(
        "          {}  100BASE-FX  {}  SGMII",
        "------------------".dimmed(),
        "---------------------".dimmed()
    );

    Ok(())
}

fn net_counters(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    mut hiffy_context: HiffyContext,
    table: bool,
    diagram: bool,
) -> Result<()> {
    let op = hubris.get_idol_command("Net.management_counters")?;

    let s = humility_hiffy::hiffy_call::<humility::reflect::Struct>(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
        None,
    )?;
    assert_eq!(s.name(), "ManagementCounters");

    if !table && !diagram {
        net_counters_table(&s)?;
        println!();
        net_counters_diagram(&s)?;
    } else {
        if table {
            net_counters_table(&s)?;
        }
        if diagram {
            net_counters_diagram(&s)?;
        }
    }
    Ok(())
}

fn net_counters_table(s: &Struct) -> Result<()> {
    let k_tx = s.field::<[_; 3]>("ksz8463_tx")?;
    let k_rx = s.field::<[_; 3]>("ksz8463_rx")?;
    let value = |k: &Struct, s: &str| {
        let k = k.field::<u32>(s).unwrap();
        let out = format!("{:>6}", k);
        if k > 0 {
            if s.contains("ERR") { out.red() } else { out.green() }
        } else {
            out.normal()
        }
    };
    println!(
        " -------------------------------------------------------------------"
    );
    println!(
        " |  {}  |         Transmit         |         Receive          |",
        "KSZ8463".bold(),
    );
    println!(
        " |           |   UC   |   BC   |   MC   |   UC   |   BC   |   MC   |"
    );
    println!(
        " |-----------|--------|--------|--------|--------|--------|--------|"
    );
    for i in 0..3 {
        let k_tx = &k_tx[i];
        let k_rx = &k_rx[i];
        println!(
            " | Port {}    | {} | {} | {} | {} | {} | {} |",
            i + 1,
            value(k_tx, "unicast"),
            value(k_tx, "broadcast"),
            value(k_tx, "multicast"),
            value(k_rx, "unicast"),
            value(k_rx, "broadcast"),
            value(k_rx, "multicast"),
        );
    }
    println!(
        " -------------------------------------------------------------------"
    );

    println!();

    let v_tx = s.field::<[_; 2]>("vsc85x2_tx")?;
    let v_rx = s.field::<[_; 2]>("vsc85x2_rx")?;
    let v_mac_valid = s.field::<bool>("vsc85x2_mac_valid")?;

    let value = |v: &Struct, s: &str| {
        let v = v.field::<u16>(s).unwrap();
        let out = format!("{:>6}", v);
        if v > 0 {
            if s.contains("good") { out.green() } else { out.red() }
        } else {
            out.normal()
        }
    };
    println!(" -------------------------------------------------------");
    println!(
        " |     {}     |     Transmit    |     Receive     |",
        if v_mac_valid { "VSC8562" } else { "VSC8552" }.bold()
    );
    println!(" |                 |  Good  |   Bad  |  Good  |   Bad  |");
    println!(" |-----------------------------------------------------|");
    for i in 0..2 {
        let v_tx = &v_tx[i];
        let v_rx = &v_rx[i];
        if v_mac_valid {
            println!(
                " | Port {} | MAC    | {} | {} | {} | {} |",
                i,
                value(v_tx, "mac_good"),
                value(v_tx, "mac_bad"),
                value(v_rx, "mac_good"),
                value(v_rx, "mac_bad"),
            );
        } else {
            println!(
                " | Port {0} | MAC    | {1:>6} | {1:>6} | {1:>6} | {1:>6} |",
                i,
                "--".dimmed(),
            );
        }
        println!(
            " |        | Media  | {} | {} | {} | {} |",
            value(v_tx, "media_good"),
            value(v_tx, "media_bad"),
            value(v_rx, "media_good"),
            value(v_rx, "media_bad"),
        );
    }
    println!(" -------------------------------------------------------");

    Ok(())
}

fn net_counters_diagram(s: &Struct) -> Result<()> {
    let k_tx = s.field::<[Struct; 3]>("ksz8463_tx")?;
    let k_rx = s.field::<[Struct; 3]>("ksz8463_rx")?;

    let mut ksz_tx = [0; 3];
    let mut ksz_rx = [0; 3];
    for port in 0..3 {
        let k_tx = &k_tx[port];
        let k_rx = &k_rx[port];
        for t in ["unicast", "broadcast", "multicast"] {
            ksz_tx[port] += k_tx.field::<u32>(t)?;
            ksz_rx[port] += k_rx.field::<u32>(t)?;
        }
    }

    let v_tx = s.field::<[Struct; 2]>("vsc85x2_tx")?;
    let v_rx = s.field::<[Struct; 2]>("vsc85x2_rx")?;
    let v_mac_valid = s.field::<bool>("vsc85x2_mac_valid")?;

    let mut v_mac_tx = [0; 2];
    let mut v_mac_rx = [0; 2];
    let mut v_media_tx = [0; 2];
    let mut v_media_rx = [0; 2];
    for port in 0..2 {
        let v_tx = &v_tx[port];
        let v_rx = &v_rx[port];
        if v_mac_valid {
            v_mac_tx[port] = v_tx.field::<u16>("mac_good")?;
            v_mac_rx[port] = v_rx.field::<u16>("mac_good")?;
        }
        v_media_tx[port] = v_tx.field::<u16>("media_good")?;
        v_media_rx[port] = v_rx.field::<u16>("media_good")?;
    }

    let mac = |i: u16| {
        if v_mac_valid {
            i.to_string().green()
        } else {
            "--".to_string().dimmed()
        }
    };

    println!(
        "            ┌──────────────────┐            ┌───────────────────┐
            │ {}          │            │ {}           │
            │                  │tx        rx│                   │tx
            │            {:>6}├───────────►│{:<6}       {:>6}├───────►
            │                  │1          0│                   │0
┌────┐    rx│            {:>6}│◄───────────┤{:<6}       {:>6}│◄───────
│    ├─────►│{:<6}            │rx        tx│                   │rx
│ SP │     3│                  │            │                   │
│    │◄─────┤{:<6}            │tx        rx│                   │tx
└────┘    tx│            {:>6}├───────────►│{:<6}       {:>6}├───────►
            │                  │2          1│                   │1
            │            {:>6}│◄───────────┤{:<6}       {:>6}│◄───────
            │                  │rx        tx│                   │rx
            └──────────────────┘            └───────────────────┘
                                             {}           {}
    ",
        "KSZ8463".bold(),
        "VSC85x2".bold(),
        ksz_tx[0].to_string().green(),
        v_media_rx[0].to_string().green(),
        mac(v_mac_tx[0]),
        ksz_rx[0].to_string().green(),
        v_media_tx[0].to_string().green(),
        mac(v_mac_rx[0]),
        ksz_rx[2].to_string().green(),
        ksz_tx[2].to_string().green(),
        ksz_tx[1].to_string().green(),
        v_media_rx[1].to_string().green(),
        mac(v_mac_tx[1]),
        ksz_rx[1].to_string().green(),
        v_media_tx[1].to_string().green(),
        mac(v_mac_rx[1]),
        "MEDIA".dimmed(),
        "MAC".dimmed()
    );
    Ok(())
}

fn net(subargs: NetArgs, context: &mut ExecutionContext) -> Result<()> {
    let hubris = &context.cli.archive()?;
    let core = &mut *context.cli.attach_live_booted(hubris)?;
    let timeout = std::time::Duration::from_millis(subargs.timeout);
    let hiffy_context = HiffyContext::new(hubris, core, timeout)?;

    match subargs.cmd {
        NetCommand::Mac => net_mac_table(hubris, core, hiffy_context)?,
        NetCommand::Ip => net_ip(hubris, core, hiffy_context)?,
        NetCommand::Status => net_status(hubris, core, hiffy_context)?,
        NetCommand::Counters { table, diagram } => {
            net_counters(hubris, core, hiffy_context, table, diagram)?
        }
    }
    Ok(())
}

humility_cmd!(NetArgs, net);
