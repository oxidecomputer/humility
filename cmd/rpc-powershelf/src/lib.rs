// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

//! ## `humility rpc-powershelf`
//!
//! TODO docs

use std::time::Duration;

use cmd_hiffy as humility_cmd_hiffy;
use cmd_rpc as humility_cmd_rpc;

use anyhow::{anyhow, Context, Result};
use clap::App;
use clap::IntoApp;
use clap::Parser;
use humility::cli::Subcommand;
use humility::hubris::HubrisArchive;
use humility::hubris::HubrisEnum;
use humility_cmd::idol::{self, HubrisIdol};
use humility_cmd::{Archive, Command};
use humility_cmd_rpc::RpcClient;

#[derive(Parser, Debug)]
#[clap(name = "rpc-powershelf", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RpcArgs {
    /// sets timeout
    #[clap(
        long, short = 'T', default_value_t = 5000, value_name = "timeout_ms",
        parse(try_from_str = parse_int::parse)
    )]
    timeout: u32,

    /// Index of the power shelf to inspect.
    #[clap(long, default_value_t = 0)]
    index: u64,

    /// Rail of the power shelf to inspect.
    #[clap(long, default_value_t = 0)]
    rail: u64,

    /// IPv6 address, e.g. `fe80::0c1d:9aff:fe64:b8c2%en0`
    #[clap(long, env = "HUMILITY_RPC_IP")]
    ip: String,
}

// Find the `task_power_api::Operation` enum.
fn lookup_operation_enum(hubris: &HubrisArchive) -> Result<&HubrisEnum> {
    let power_task = hubris.lookup_task("power").ok_or_else(|| {
        anyhow!("missing `power` task - is this a PSC archive?")
    })?;

    let module = hubris.lookup_module(*power_task)?;

    // `Operation` matches multiple enums, and `task_power_api::Operation`
    // doesn't show up alone, but `Option<task_power_api::Operation>` does! Find
    // that, then drill down to get the enum we want out.
    let opt_operation = module
        .lookup_enum_byname(hubris, "Option<task_power_api::Operation>")
        .context("Could not look up task_power_api::Operation")?;
    let some_operation_goff = opt_operation
        .variants
        .iter()
        .find_map(|v| if v.name == "Some" { v.goff } else { None })
        .expect("Option<_> missing `Some(_)` variant");

    let some_operation = hubris.lookup_struct(some_operation_goff)?;
    let operation_goff = some_operation
        .newtype()
        .ok_or_else(|| anyhow!("Some(_) variant is not a newtype struct"))?;

    let operation = hubris.lookup_enum(operation_goff)?;

    Ok(operation)
}

fn rpc_powershelf_run(context: &mut humility::ExecutionContext) -> Result<()> {
    let Subcommand::Other(subargs) = context.cli.cmd.as_ref().unwrap();
    let subargs = RpcArgs::try_parse_from(subargs)?;
    let hubris = context.archive.as_ref().unwrap();

    let operation = lookup_operation_enum(&hubris)?;

    let mut client = RpcClient::new(
        hubris,
        &subargs.ip,
        Duration::from_millis(u64::from(subargs.timeout)),
    )?;

    let idol_cmd = hubris
        .get_idol_command("Power.pmbus_read")
        .context("missing `Power.pmbus_read` - is this a PSC archive?")?;

    // 3 out of 4 args are the same for every variant; build a vec of the 4 args
    // here, and we'll modify the first arg in the loop below.
    let mut args = vec![
        ("op", idol::IdolArgument::String("PLACEHOLDER")),
        ("dev", idol::IdolArgument::String("PowerShelf")),
        ("rail", idol::IdolArgument::Scalar(subargs.rail)),
        ("index", idol::IdolArgument::Scalar(subargs.index)),
    ];

    for variant in &operation.variants {
        args[0].1 = idol::IdolArgument::String(&variant.name);
        let result = client.call(&idol_cmd, &args)?;
        println!(
            "{:<20} => {}",
            variant.name,
            humility_cmd_hiffy::hiffy_format_result(hubris, result)
        );
    }

    Ok(())
}

pub fn init() -> (Command, App<'static>) {
    (
        Command::Unattached {
            name: "rpc-powershelf",
            archive: Archive::Required,
            run: rpc_powershelf_run,
        },
        RpcArgs::into_app(),
    )
}
