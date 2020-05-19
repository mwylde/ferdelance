use crate::ast::Program;
lalrpop_mod!(pub grammar); // synthesized by LALRPOP

pub fn parse(i: &str) -> Program {
    match grammar::ProgramParser::new().parse(i) {
        Ok(p) => p,
        Err(e) => panic!("Failed to parse: {:#?}", e),
    }
}
