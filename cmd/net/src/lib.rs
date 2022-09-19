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

use anyhow::{bail, Result};
use clap::{CommandFactory, Parser};
use colored::Colorize;

use cmd_hiffy as humility_cmd_hiffy;

use humility::cli::Subcommand;
use humility::reflect::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::HubrisIdol;
use humility_cmd::{Archive, Attach, Command, CommandKind, Validate};

#[derive(Parser, Debug)]
enum NetCommand {
    /// Dump the KSZ8463's MAC table
    Mac,
    /// Print the IP and MAC address
    Ip,
    /// Print the link status
    Status,
    /// Print the counters
    Counters,
}

#[derive(Parser, Debug)]
#[clap(name = "net", about = env!("CARGO_PKG_DESCRIPTION"))]
struct NetArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: NetCommand,
}

fn net_ip(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = NetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let mut hiffy_context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let op = hubris.get_idol_command("Net.get_mac_address")?;

    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
    )?;
    let v = match value {
        Ok(v) => v,
        Err(e) => bail!("Got Hiffy error: {}", e),
    };
    let v = v.as_tuple()?;
    assert_eq!(v.name(), "MacAddress");
    let mut mac = [0; 6];
    for (i, byte) in v[0].as_array()?.iter().enumerate() {
        mac[i] = byte.as_base()?.as_u8().unwrap();
    }
    print!("{}:  ", "MAC address".bold());
    for (i, byte) in mac.iter().enumerate() {
        if i > 0 {
            print!(":");
        }
        print!("{:02x}", byte);
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

fn net_mac_table(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = NetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let mut hiffy_context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let op_mac_count = hubris.get_idol_command("Net.read_ksz8463_mac_count")?;

    // We need to make two HIF calls:
    // - Read the number of entries in the MAC table
    // - Loop over the table that many times, reading entries
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut hiffy_context,
        &op_mac_count,
        &[],
        None,
    )?;
    let mac_count = match value {
        Ok(v) => {
            if let Value::Base(Base::U32(v)) = v {
                v
            } else {
                bail!("Got bad reflected value: expected U32, got {:?}", v);
            }
        }
        Err(e) => {
            bail!("Got error: {}", e);
        }
    };

    // Due to HIF limitations (lack of Expand16 / Collect16), we're going to
    // limit ourselves to 255 MAC table entries.
    let (mac_count, clipped) = if mac_count > u8::MAX as u32 {
        (u8::MAX, true)
    } else {
        (mac_count as u8, false)
    };

    humility::msg!("Reading {} MAC addresses...", mac_count);

    let op = hubris.get_idol_command("Net.read_ksz8463_mac")?;
    let funcs = hiffy_context.functions()?;
    let send = funcs.get("Send", 4)?;

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
        .map(move |r| humility_cmd_hiffy::hiffy_decode(hubris, &op, r))
        .collect::<Result<Vec<Result<_, _>>>>()?;

    let mut mac_table: BTreeMap<u16, Vec<[u8; 6]>> = BTreeMap::new();
    for r in results {
        if let Ok(r) = r {
            let s = r.as_struct()?;
            assert_eq!(s.name(), "KszMacTableEntry");
            let port = s["port"].as_base().unwrap().as_u16().unwrap();
            let mut mac = [0; 6];
            for (i, m) in s["mac"].as_array().unwrap().iter().enumerate() {
                mac[i] = m.as_base().unwrap().as_u8().unwrap()
            }
            if mac == [0; 6] && port == 0xFFFF {
                humility::msg!("Skipping empty MAC address");
            } else {
                mac_table.entry(port).or_default().push(mac);
            }
        } else {
            // Log the error but keep going for other entries in the table
            humility::msg!("Got error result: {:?}", r);
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

fn net_status(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = NetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let mut hiffy_context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let op = hubris.get_idol_command("Net.management_link_status")?;

    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
    )?;
    let v = match value {
        Ok(v) => v,
        Err(e) => bail!("Got Hiffy error: {}", e),
    };
    let s = v.as_struct()?;
    assert_eq!(s.name(), "ManagementLinkStatus");

    let to_bool_vec = |name| -> Result<Vec<bool>> {
        Ok(s[name]
            .as_array()?
            .iter()
            .map(|i| i.as_base().unwrap().as_bool().unwrap())
            .collect())
    };
    let ksz_100base_fx = to_bool_vec("ksz8463_100base_fx_link_up")?;
    let vsc_100base_fx = to_bool_vec("vsc85x2_100base_fx_link_up")?;
    let vsc_sgmii = to_bool_vec("vsc85x2_sgmii_link_up")?;

    let up_down = |b| {
        if b {
            " UP ".green()
        } else {
            "DOWN".red()
        }
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

fn net_counters(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = NetArgs::try_parse_from(subargs)?;

    let hubris = context.archive.as_ref().unwrap();
    let core = &mut **context.core.as_mut().unwrap();

    let mut hiffy_context = HiffyContext::new(hubris, core, subargs.timeout)?;

    let op = hubris.get_idol_command("Net.management_counters")?;

    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        &mut hiffy_context,
        &op,
        &[],
        None,
    )?;
    let v = match value {
        Ok(v) => v,
        Err(e) => bail!("Got Hiffy error: {}", e),
    };
    let s = v.as_struct()?;
    assert_eq!(s.name(), "ManagementCounters");

    let k_tx = s["ksz8463_tx"].as_array()?;
    let k_rx = s["ksz8463_rx"].as_array()?;
    let value = |k: &Struct, s: &str| {
        let k = k[s].as_base().unwrap().as_u32().unwrap();
        let out = format!("{:>6}", k);
        if k > 0 {
            if s.contains("ERR") {
                out.red()
            } else {
                out.green()
            }
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
        let k_tx = k_tx[i].as_struct()?;
        let k_rx = k_rx[i].as_struct()?;
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

    let v_tx = s["vsc85x2_tx"].as_array()?;
    let v_rx = s["vsc85x2_rx"].as_array()?;
    let v_mac_valid = s["vsc85x2_mac_valid"].as_base()?.as_bool().unwrap();

    let value = |v: &Struct, s: &str| {
        let v = v[s].as_base().unwrap().as_u16().unwrap();
        let out = format!("{:>6}", v);
        if v > 0 {
            if s.contains("good") {
                out.green()
            } else {
                out.red()
            }
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
        let v_tx = v_tx[i].as_struct()?;
        let v_rx = v_rx[i].as_struct()?;
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

fn net(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = NetArgs::try_parse_from(subargs)?;

    match subargs.cmd {
        NetCommand::Mac => net_mac_table(context)?,
        NetCommand::Ip => net_ip(context)?,
        NetCommand::Status => net_status(context)?,
        NetCommand::Counters => net_counters(context)?,
    }
    Ok(())
}

pub fn init() -> Command {
    Command {
        app: NetArgs::command(),
        name: "net",
        run: net,
        kind: CommandKind::Attached {
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
        },
    }
}
