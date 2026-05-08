use clap::{CommandFactory, Parser};
use crate::RingbufArgs as MainRingbufArgs;
use crate::TotalsOptions as MainTotalsOptions;
use humility_cli::ExecutionContext;
use humility_cmd::Command;
use anyhow::Result;

#[derive(Parser, Debug)]
#[clap(name = "ringbuf", about = env!("CARGO_PKG_DESCRIPTION"))]
struct RingbufArgs {
    /// list variables
    #[clap(long, short)]
    list: bool,
    /// print full errors
    #[clap(long, short)]
    verbose: bool,
    /// print integer values in decimal rather than hex
    #[clap(long, short)]
    decimal: bool,
    /// expand de-duplicated entries
    #[clap(long, short)]
    expand: bool,
    /// print only a single ringbuffer by substring of name
    #[clap(conflicts_with = "list")]
    name: Option<String>,
    #[clap(flatten)]
    totals: TotalsOptions,
}
impl From<RingbufArgs> for MainRingbufArgs {
    fn from(args: RingbufArgs) -> Self {
        Self {
            list: args.list,
            verbose: args.verbose,
            decimal: args.decimal,
            expand: args.expand,
            name: args.name,
            totals: args.totals.into()
        }
    }
}

#[derive(Parser, Debug, Clone, Copy)]
#[clap(next_help_heading = "DISPLAYING ENTRY TOTALS")]
struct TotalsOptions {
    /// when displaying totals, don't skip entry variants which have
    /// not been recorded
    #[clap(long, short, conflicts_with = "list")]
    full_totals: bool,
    /// skip displaying total counts for ringbuf entry variants
    #[clap(
        long,
        short,
        conflicts_with_all = &[
            "full_totals",
            "list"
        ],
    )]
    no_totals: bool,
}

impl From<TotalsOptions> for MainTotalsOptions {
    fn from(args: TotalsOptions) -> Self {
        Self {
            full_totals: args.full_totals,
            no_totals: args.no_totals,
        }
    }
}

// this allow is meant for the header println! in the body but you cannot apply
// an attribute to a macro invoction, so we have to put it here instead.
#[allow(clippy::print_literal)]
fn ringbuf_entry(context: &mut ExecutionContext) -> Result<()> {
    let subargs = RingbufArgs::try_parse_from(&context.cli.cmd)?;

    let hubris = &context.cli.archive()?;

    let core = &mut *context.cli.attach_live_or_dump_match(hubris)?;

    crate::ringbuf(&subargs.into(), hubris, core)
}

pub fn init() -> Command {
    Command { app: RingbufArgs::command(), name: "ringbuf", run: ringbuf_entry }
}

