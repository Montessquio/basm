
extern crate clap;
#[macro_use] extern crate log;
extern crate fern;
extern crate chrono;
extern crate term_grid;

pub mod assembler;

use clap::{Arg, ArgMatches, App};
use term_grid::{Grid, GridOptions, Direction, Filling, Cell};

use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    let args = process_arguments();
    initialize_logging(args.occurrences_of("verbose"));

    debug!("Arguments:\n\tVerbosity: {}\n\tPreprocess Only: {}\n\tOutfile: {}\n\tInfile: {}",
        match args.occurrences_of("verbose") {
            0 => log::LevelFilter::Error.to_string(),
            1 => log::LevelFilter::Warn.to_string(),
            2 => log::LevelFilter::Info.to_string(),
            3 | _ => log::LevelFilter::Debug.to_string(),
        },
        args.is_present("preprocess"),
        args.value_of("output").unwrap_or("None"),
        args.value_of("INPUT").unwrap()
    );

    let ifile = args.value_of("INPUT").unwrap();
    // Read the specified input file.
    let ipath = Path::new(ifile);

    // Open the path in read-only mode, returns `io::Result<File>`
    let ifile = match File::open(&ipath) {
        Err(err) => {
            error!("fatal: unable to open input file `{}`: {}", ipath.display(), err);
            std::process::exit(1);
        },
        Ok(file) => file,
    };

    let parser = assembler::parser::Parser::new(
        assembler::lexer::tokenize(Box::new(ifile))
    );

    let ast = parser.run();

    if args.is_present("print-debug") {
        let mut grid = Grid::new(GridOptions {
            filling:     Filling::Spaces(1),
            direction:   Direction::LeftToRight,
        });

        for (idx, ins) in ast.iter().enumerate() {
            grid.add(Cell::from(format!("0x{:04X}:", idx)));
            grid.add(Cell::from(format!("{}", ins)));
            grid.add(Cell::from("=>".to_string()));
            grid.add(Cell::from(format!("0x{:04X}", ins.assemble())));
        }
        
        println!("{}", grid.fit_into_columns(4));
    }

    let opath = if let Some(filename) = args.value_of("output") {
        Path::new(filename)
    } else {
        Path::new(ipath.file_stem().unwrap())
    };

    let mut ofile = match File::create(&opath) {
        Err(err) => {
            error!("fatal: unable to open output file `{}`: {}", opath.display(), err);
            std::process::exit(1);
        },
        Ok(file) => file,
    };

    for dword in ast.iter() {
        if let Err(err) = ofile.write_all(&dword.assemble().to_be_bytes()) {
            error!("fatal: unable to write to output file `{}`: {}", opath.display(), err);
            std::process::exit(1);
        }
    }
}

fn process_arguments() -> ArgMatches<'static> {
    App::new(option_env!("CARGO_PKG_NAME").unwrap())
        .version(option_env!("CARGO_PKG_VERSION").unwrap())
        .author(option_env!("CARGO_PKG_AUTHORS").unwrap())
        .about(option_env!("CARGO_PKG_DESCRIPTION").unwrap())
        .arg(Arg::with_name("INPUT")
            .help("Sets the input file to use")
            .required(true)
            .multiple(false)
            .index(1))
        .arg(Arg::with_name("verbose")
            .short("v")
            .multiple(true)
            .takes_value(false)
            .help("Sets the level of verbosity"))
        .arg(Arg::with_name("output")
            .short("o")
            .takes_value(true)
            .help("write output to an outfile"))
        .arg(Arg::with_name("preprocess")
            .short("e")
            .takes_value(false)
            .help("preprocess only"))
        .arg(Arg::with_name("print-debug")
            .short("d")
            .alias("show")
            .alias("s")
            .takes_value(false)
            .help("prints the debug information alongside the assembly to STDOUT"))
        .get_matches()
}

fn initialize_logging(verbosity: u64) {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}[{}][{}] {}",
                chrono::Local::now().format("[%Y-%m-%d][%H:%M:%S]"),
                record.target(),
                record.level(),
                message
            ))
        })
        .level(match verbosity {
            0 => log::LevelFilter::Error,
            1 => log::LevelFilter::Warn,
            2 => log::LevelFilter::Info,
            3 | _ => log::LevelFilter::Debug,
        })
        .chain(std::io::stdout())
        .apply().ok();
}