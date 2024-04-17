mod interp;
mod graphics;

use std::fs::File;
use std::io::Read;
use interp::parser;

fn help() {
    println!("Usage:
              progname file_with_program file_with_map")
}

fn main() -> Result<(), std::io::Error> {
    let mut args = std::env::args();
    let mut file_with_prog : File;
    let mut program = String::new();
    let mut file_with_map : File;
    let mut map = String::new();

    if args.len() != 3 {
        help();
        return Ok(());
    }

    file_with_prog = File::open(args.nth(1).unwrap())?;
    file_with_prog.read_to_string(&mut program)?;
    if !program.is_ascii() {
        println!("Go and fix your program, I dont know UTF-8!");
        return Ok(());
    }

    let mut parser = parser::Parser::new(&program);
    /* build AST here */
    parser.build_ast();
    file_with_map = File::open(args.nth(2).unwrap())?;
    file_with_map.read_to_string(&mut map)?;
    Ok(())
}
