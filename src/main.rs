use crate::compiler::Compiler;
use clap::{App, Arg};
use std::fs;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::process::{exit, Command};
use std::str;
use x86asm::Instruction;

#[macro_use]
extern crate log;
#[macro_use]
extern crate lalrpop_util;
#[macro_use]
extern crate lazy_static;

mod ast;
mod compiler;
#[allow(dead_code)]
mod parser;
mod tester;
mod tests;
#[allow(dead_code)]
mod x86asm;

const WRAPPER: &'static str = include_str!("../lib/wrapper.c");
const GC_H: &'static str = include_str!("../lib/gc.h");
const GC_C: &'static str = include_str!("../lib/gc.c");

fn main() {
    let matches = App::new("fer-de-lance")
        .version("0.1")
        .about("Compiler for the fer-de-lance language")
        .arg(
            Arg::with_name("run")
                .short("r")
                .long("run")
                .help("Run the program after compiling"),
        )
        .arg(
            Arg::with_name("assembly")
                .long("assembly")
                .help("Output assembly file instead of compiling"),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .help("Output file")
                .value_name("FILE"),
        )
        .arg(
            Arg::with_name("debug")
                .long("debug")
                .help("outputs a debug project")
                .value_name("DIR"),
        )
        .arg(
            Arg::with_name("INPUT")
                .required(true)
                .help("Source file to compile")
                .index(1),
        )
        .arg(
            Arg::with_name("verbose")
                .long("--verbose")
                .short("-v")
                .multiple(true)
                .help("Provides verbose output on stderr"),
        )
        .get_matches();

    let run = matches.is_present("run");
    let assembly = matches.is_present("assembly");
    let file = matches.value_of("INPUT").unwrap();

    let default_output = Path::new(file).with_extension(if assembly { "s" } else { "out" });

    let output = matches
        .value_of("output")
        .unwrap_or(default_output.to_str().unwrap());
    let virtualize = matches.is_present("virtualize");

    let debug_output = matches.value_of("debug");

    let verbose = matches.occurrences_of("verbose");
    stderrlog::new().verbosity(verbose as usize).init().unwrap();

    let contents = fs::read_to_string(file).expect("Failed to read file");

    let binary = compile(&contents, file, output, virtualize, assembly, debug_output)
        .map_err(|code| exit(code));

    if run {
        eprintln!("Running...\n---------");
        let output = Command::new(binary.unwrap())
            .status()
            .expect("Failed to run");

        eprintln!("---------\nExited with {}", output.code().unwrap());
    }
}

pub fn compile(
    contents: &str,
    file: &str,
    output_path: &str,
    _interpret: bool,
    assembly: bool,
    debug_output: Option<&str>,
) -> Result<PathBuf, i32> {
    let mut compiler = Compiler::new();
    let program = compiler.parse(contents);
    let errors = compiler.validate(&program);

    for error in &errors {
        error!("{}", error);
    }
    if !errors.is_empty() {
        return Err(2);
    }

    let instructions = compiler.compile(program);

    let mut asm = "section .text\nextern error\nextern print\nextern input\nextern try_gc\n\
        extern HEAP_END\nextern STACK_BOTTOM\n\
        global our_code_starts_here\n\n"
        .to_string();

    for instr in instructions {
        asm.push_str(&instr.to_asm());
        asm.push('\n');
    }

    debug!("ASM:\n{}", asm);

    if assembly {
        let asm_file = fs::File::create(output_path).unwrap();
        write!(&asm_file, "{}", asm).unwrap();
        return Ok(PathBuf::from(output_path));
    }

    let path = Path::new(file);
    let dir = path.parent().unwrap().to_path_buf();
    let asm_path = path.with_extension("s");
    let asm_file = fs::File::create(&asm_path).expect("Failed to open file for writing");

    write!(
        &asm_file,
        "; {}:\n;\n",
        path.file_name().unwrap().to_str().unwrap()
    )
    .unwrap();
    for line in contents.lines() {
        write!(&asm_file, "; {}\n", line).unwrap();
    }

    write!(&asm_file, "\n{}", asm).unwrap();

    let wrapper_path = path.with_extension("c");
    let wrapper_file = fs::File::create(&wrapper_path).expect("Failed to open file for writing");
    write!(&wrapper_file, "{}", WRAPPER).unwrap();

    let mut gc_path = dir.clone();
    gc_path.push("gc");
    for (contents, extension) in vec![(GC_C, "c"), (GC_H, "h")] {
        let f = fs::File::create(&gc_path.with_extension(extension))
            .expect(&format!("Failed to open gc.{} file for writing", extension));
        write!(&f, "{}", contents).expect(&format!("Failed to write gc.{}", extension));
    }

    let output = Command::new("nasm")
        .arg("-g")
        .arg("-f")
        .arg("elf64")
        .arg("-w+error")
        .arg("-o")
        .arg(path.with_extension("o"))
        .arg(&asm_path)
        .output()
        .expect("Failed to assemble");

    if !output.status.success() {
        panic!(
            "Failed to assemble\n{}",
            str::from_utf8(&output.stderr).unwrap()
        );
    }

    let output = Command::new("clang")
        .arg("-g")
        .arg("-mstackrealign")
        .arg(&wrapper_path)
        .arg(gc_path.with_extension("c"))
        .arg(path.with_extension("o"))
        .arg("-o")
        .arg(output_path)
        .output()
        .expect("Failed to compile");

    if !output.status.success() {
        panic!(
            "Failed to compile\n{}",
            str::from_utf8(&output.stderr).unwrap()
        );
    }

    let files = vec![
        asm_path.clone(),
        asm_path.with_extension("o"),
        wrapper_path.clone(),
        gc_path.with_extension("c"),
        gc_path.with_extension("h"),
    ];

    if let Some(debug) = debug_output {
        let mut debug_path = Path::new(debug).to_path_buf();
        debug_path.push("file");

        fs::copy(&asm_path, debug_path.with_file_name("source.asm")).unwrap();
        fs::copy(&wrapper_path, debug_path.with_file_name("main.c")).unwrap();
        fs::copy(
            &gc_path.with_extension("c"),
            debug_path.with_file_name("gc.c"),
        )
        .unwrap();
        fs::copy(
            &gc_path.with_extension("h"),
            debug_path.with_file_name("gc.h"),
        )
        .unwrap();
    }

    for f in files {
        fs::remove_file(f).expect("Failed to clean up");
    }

    Ok(PathBuf::from(output_path))
}
