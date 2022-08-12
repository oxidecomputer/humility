// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use std::collections::BTreeMap;

use anyhow::{bail, Context, Result};
use clap::{Command as ClapCommand, CommandFactory, Parser};
use colored::Colorize;

use humility::core::Core;
use humility::hubris::*;
use humility::reflect::*;
use humility_cmd::hiffy::HiffyContext;
use humility_cmd::idol::IdolOperation;
use humility_cmd::{Archive, Attach, Command, Run, Validate};

#[derive(Parser, Debug)]
enum NetCommand {
    /// Dump the KSZ8463's MAC table
    Mac,
    /// Print the IP and MAC address
    Ip,
    /// Print the link status
    Status,
}

#[derive(Parser, Debug)]
#[clap(name = "monorail", about = env!("CARGO_PKG_DESCRIPTION"))]
struct NetArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value = "5000", value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    #[clap(subcommand)]
    cmd: NetCommand,
}

fn net_ip(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<()> {
    let op = IdolOperation::new(hubris, "Net", "get_mac_address", None)
        .context(
            "Could not find `get_mac_address`, \
                  is your Hubris archive new enough?",
        )?;

    let value =
        humility_cmd_hiffy::hiffy_call(hubris, core, context, &op, &[])?;
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
    print!("{}: ", "IPv6 address".bold());
    print!("fe80::");
    for (i, byte) in mac.iter().enumerate() {
        if i == 2 || i == 4 {
            print!(":");
        }
        print!("{:02x}", if i == 0 { *byte ^ (1 << 1) } else { *byte });
        if i == 2 {
            print!("ff:fe");
        }
    }
    println!();
    Ok(())
}

fn net_mac_table(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    context: &mut HiffyContext,
) -> Result<()> {
    let op_mac_count =
        IdolOperation::new(hubris, "Net", "read_ksz8463_mac_count", None)
            .context(
                "Could not find `read_ksz8463_mac_count`, \
                  is your Hubris archive new enough?",
            )?;

    // We need to make two HIF calls:
    // - Read the number of entries in the MAC table
    // - Loop over the table that many times, reading entries
    let value = humility_cmd_hiffy::hiffy_call(
        hubris,
        core,
        context,
        &op_mac_count,
        &[],
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

    println!("Reading {} MAC addresses...", mac_count);

    let op = IdolOperation::new(hubris, "Net", "read_ksz8463_mac", None)?;
    let funcs = context.functions()?;
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

    let results = context.run(core, ops.as_slice(), None)?;
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
                println!("Skipping empty MAC address");
            } else {
                mac_table.entry(port).or_default().push(mac);
            }
        } else {
            // Log the error but keep going for other entries in the table
            println!("Got error result: {:?}", r);
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
    context: &mut HiffyContext,
) -> Result<()> {
    let op = IdolOperation::new(hubris, "Net", "management_link_status", None)
        .context(
            "Could not find `management_link_status`, \
                  is your Hubris archive new enough?",
        )?;

    let value =
        humility_cmd_hiffy::hiffy_call(hubris, core, context, &op, &[])?;
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
        "VSC8552".bold()
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

fn net(
    hubris: &HubrisArchive,
    core: &mut dyn Core,
    subargs: &[String],
) -> Result<()> {
    let subargs = NetArgs::try_parse_from(subargs)?;
    let mut context = HiffyContext::new(hubris, core, subargs.timeout)?;
    match subargs.cmd {
        NetCommand::Mac => net_mac_table(hubris, core, &mut context)?,
        NetCommand::Ip => net_ip(hubris, core, &mut context)?,
        NetCommand::Status => net_status(hubris, core, &mut context)?,
    }
    Ok(())
}

pub fn init() -> (Command, ClapCommand<'static>) {
    (
        Command::Attached {
            name: "net",
            archive: Archive::Required,
            attach: Attach::LiveOnly,
            validate: Validate::Booted,
            run: Run::Subargs(net),
        },
        NetArgs::command(),
    )
}
