mod backend;
mod cli;
mod common;
mod frontend;
mod middle;

fn main() -> anyhow::Result<()> {
    cli::run()
}
