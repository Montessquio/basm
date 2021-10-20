//! The Assembler module is in charge of taking a
//! BASM file and producing a Vec<Instruction> from the
//! AST submodule.
//! 
//! It does this by implementing a simple tokenizer
//! and non-lookahead recursive descent parser.

pub mod ast;
pub mod lexer;
pub mod parser;