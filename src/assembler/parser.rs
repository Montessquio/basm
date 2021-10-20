//! The Parser module takes a token stream (Vec<Token>) from the Tokenizer
//! and converts it into an AST.
use std::collections::VecDeque;
use std::convert::TryFrom;
use super::lexer::Token;
use super::ast::*;

pub struct Parser {
    tokens: VecDeque<Token>,
    ast:    VecDeque<Instruction>,
    error_count: usize
}

impl Parser {
    pub fn new(tokens: VecDeque<Token>) -> Self {
        let capacity = tokens.capacity();
        Parser{tokens, ast: VecDeque::with_capacity(capacity), error_count: 0}
    }

    /// Run the parser, consuming itself and returning a list of instructions.
    pub fn run(mut self) -> VecDeque<Instruction> {
        'mainloop: loop {
            match self.instruction() {
                Ok(Some(ins)) => self.ast.push_back(ins),
                Ok(None) => break 'mainloop,
                Err(e) => error!("{}", e),
            };
        }

        if self.error_count > 0 {
            error!("Stopped assembly due to {} parsing error(s).", self.error_count);
            std::process::exit(1);
        }

        self.ast
    }

    /// Recursively consumes tokens to produce an Instruction.
    fn instruction(&mut self) -> Result<Option<Instruction>, String> {
        let cur_tok = self.consume();

        match cur_tok {
            Some(Token::Op(op, line)) => match self.operation(op) {
                Ok(ins) => Ok(Some(ins)),
                Err(e) => {
                    Err(format!("error parsing operation on line {}: {}", line, e))
                }
            }
            Some(Token::Reg(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected register parameter on line {}", line))
            },
            Some(Token::Val(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected numeric literal on line {}", line))
            },
            // No tokens left to parse means we're out of instructions.
            None => Ok(None),
        }
    }

/*
LDI (rega, value) |
SUBI(rega, value) |
IN  (rega, value) |
OUT (rega, value)
             */

    fn operation(&mut self, op: String) -> Result<Instruction, String> {
        match op.as_str() {
            "NOP" => Ok(Instruction::NOP),

            // Jump is the only opcode with a single,
            // register operand, so it's just handled
            // inline.
            "J" => match self.register() {
                Ok(reg) => Ok(Instruction::J(reg)),
                Err(e) => Err(e),
            },

            "ADD" | "SUB" | "OR" |
            "XOR" | "MOVE"| "SR" |
            "SL"  | "JZ"  | "JLT"|
            "AND"               => self.op_dual_reg(op),

            "LDI" | "SUBI" |
            "IN"  | "OUT"       => self.op_reg_immediate(op),
            _ => Err(format!("lexer returned invalid opcode {}", op)),
        }
    }

    /// Parses ops with dual register operands.
    /// This function assumes the input op is valid.
    fn op_dual_reg(&mut self, op: String) -> Result<Instruction, String> {
        let a = match self.register() {
            Ok(reg) => reg,
            Err(e) => return Err(e),
        };

        let b = match self.register() {
            Ok(reg) => reg,
            Err(e) => return Err(e),
        };

        match op.as_str() {
            "ADD" => Ok(Instruction::ADD(a, b)),
            "SUB" => Ok(Instruction::SUB(a, b)),
            "AND" => Ok(Instruction::AND(a, b)),
            "OR"  => Ok(Instruction::OR(a, b)),
            "XOR" => Ok(Instruction::XOR(a, b)),
            "MOVE"=> Ok(Instruction::MOVE(a, b)),
            "SR"  => Ok(Instruction::SR(a, b)),
            "SL"  => Ok(Instruction::SL(a, b)),
            "JZ"  => Ok(Instruction::JZ(a, b)),
            "JLT" => Ok(Instruction::JLT(a, b)),
            _ => Err(format!("op_dual_reg got opcode {} - this function should only get valid values as input - check all callers", op)),
        }
    }

    // Parses ops with one operand a register and another
    // an immediate value.
    fn op_reg_immediate(&mut self, op: String) -> Result<Instruction, String> {
        let a = match self.register() {
            Ok(reg) => reg,
            Err(e) => return Err(e),
        };

        let v = match self.immediate() {
            Ok(reg) => reg,
            Err(e) => return Err(e),
        };

        match op.as_str() {
            "LDI" => Ok(Instruction::LDI(a, v)),
            "SUBI"=> Ok(Instruction::SUBI(a, v)),
            "IN"  => if v > 0x0F {
                Err(format!("immediate parameter to IN must be 4-bits (max 0x0F)"))
            } else {
                Ok(Instruction::IN(a, v))
            },
            "OUT" => 
            if v > 0x0F {
                Err(format!("immediate parameter to OUT must be 4-bits (max 0x0F)"))
            } else {
                Ok(Instruction::OUT(a, v))
            },
            _ => Err(format!("op_reg_immediate got opcode {} - this function should only get valid values as input - check all callers", op)),
        }
    }

    fn register(&mut self) -> Result<Register, String> {
        let cur_tok = self.consume();

        match cur_tok {
            Some(Token::Op(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected instruction token on line {}", line))
            },
            Some(Token::Reg(id, _)) => Register::try_from(id),
            Some(Token::Val(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected numeric literal on line {}", line))
            },
            None => Err(format!("expected a register operand, got EOF.", )),
        }
    }

    fn immediate(&mut self) -> Result<Immediate, String> {
        let cur_tok = self.consume();

        match cur_tok {
            Some(Token::Op(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected instruction token on line {}", line))
            },
            Some(Token::Reg(_, line)) => {
                self.error_count += 1;
                Err(format!("unexpected register parameter on line {}", line))
            },
            Some(Token::Val(val, _)) => Ok(Immediate::from(val)),
            None => Err(format!("expected a numeric literal, got EOF.", )),
        }
    }
    
    /// Pops a token off the input stream and returns it.
    /// Returns None if no tokens are left.
    #[inline]
    fn consume(&mut self) -> Option<Token> {
        self.tokens.pop_front()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::lexer::Token;
    use super::super::ast;

    #[test]
    fn test_register() {
        // Test- Valid
        let mut tokens: VecDeque<Token> = VecDeque::new();
        for i in 0..=u16::MAX {
            tokens.push_back(Token::Reg(i, 0));
        }
        let mut parser : Parser = Parser::new(tokens);

        for i in 0..=15 {
            let out = parser.register();
            assert!(out.is_ok());
            assert_eq!(out.ok().unwrap(), ast::Register::try_from(i).ok().unwrap());
        }

        for _ in 16..=u16::MAX {
            let out = parser.register();
            assert!(out.is_err());
        }
        assert!(parser.tokens.is_empty());

        // Test- Invalid

        let mut tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("Strike the Earth".to_string(), 0)
        ]);
        for i in 0..=u8::MAX {
            tokens.push_back(Token::Val(i, 0));
        }
        let mut parser : Parser = Parser::new(tokens);

        for _ in 15..=u8::MAX as u16 + 1 {
            assert!(parser.register().is_err());
        }
    }

    #[test]
    fn test_immediate() {
                // Test- Valid
                let mut tokens: VecDeque<Token> = VecDeque::new();
                for i in 0..=u8::MAX {
                    tokens.push_back(Token::Val(i, 0));
                }
                let mut parser : Parser = Parser::new(tokens);
        
                for i in 0..=u8::MAX {
                    let out = parser.immediate();
                    assert!(out.is_ok());
                    assert_eq!(out.ok().unwrap(), ast::Immediate::from(i));
                }
                assert!(parser.tokens.is_empty());
        
                // Test- Invalid
        
                let mut tokens: VecDeque<Token> = VecDeque::from(vec![
                    Token::Op("Strike the Earth".to_string(), 0)
                ]);
                for i in 0..=u16::MAX {
                    tokens.push_back(Token::Reg(i, 0));
                }
                let mut parser : Parser = Parser::new(tokens);
        
                for _ in 15..=u16::MAX as u32 + 1 {
                    assert!(parser.immediate().is_err());
                }
    }

    #[test]
    fn test_op_reg_immediate() {
        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
            Token::Val(128, 0),
            Token::Reg(0, 0),
            Token::Val(128, 0),
            Token::Reg(0, 0),
            Token::Val(0x0F, 0),
            Token::Reg(0, 0),
            Token::Val(0x0F, 0),
            Token::Val(0, 0),
            Token::Reg(128, 0),
            Token::Val(0, 0),
            Token::Reg(128, 0),
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert_eq!(parser.op_reg_immediate("LDI".to_string()), Ok(ast::Instruction::LDI(ast::Register::R0, 128)));
        assert_eq!(parser.op_reg_immediate("SUBI".to_string()), Ok(ast::Instruction::SUBI(ast::Register::R0, 128)));
        assert_eq!(parser.op_reg_immediate("IN".to_string()), Ok(ast::Instruction::IN(ast::Register::R0, 0x0F)));
        assert_eq!(parser.op_reg_immediate("OUT".to_string()), Ok(ast::Instruction::OUT(ast::Register::R0, 0x0F)));
        assert!(parser.op_reg_immediate("IN".to_string()).is_err());
        assert!(parser.op_reg_immediate("OUT".to_string()).is_err());
    }

    #[test]
    fn test_op_dual_reg() {
        let mut tokens: VecDeque<Token> = VecDeque::new();
        for _ in 0..=10 {
            tokens.push_back(Token::Reg(0, 0));
            tokens.push_back(Token::Reg(15, 0));
        }
        let mut parser : Parser = Parser::new(tokens);

        assert_eq!(parser.op_dual_reg("ADD".to_string()), Ok(ast::Instruction::ADD(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("SUB".to_string()), Ok(ast::Instruction::SUB(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("AND".to_string()), Ok(ast::Instruction::AND(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("OR".to_string()), Ok(ast::Instruction::OR(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("XOR".to_string()), Ok(ast::Instruction::XOR(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("MOVE".to_string()), Ok(ast::Instruction::MOVE(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("SR".to_string()), Ok(ast::Instruction::SR(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("SL".to_string()), Ok(ast::Instruction::SL(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("JZ".to_string()), Ok(ast::Instruction::JZ(ast::Register::R0, ast::Register::R15)));
        assert_eq!(parser.op_dual_reg("JLT".to_string()), Ok(ast::Instruction::JLT(ast::Register::R0, ast::Register::R15)));
    }

    #[test]
    fn test_operation() {
        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
            Token::Val(128, 0),
            Token::Reg(1, 0),
            Token::Reg(2, 0),
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert_eq!(parser.operation("LDI".to_string()), Ok(ast::Instruction::LDI(ast::Register::R0, 128)));
        assert_eq!(parser.operation("MOVE".to_string()), Ok(ast::Instruction::MOVE(ast::Register::R1, ast::Register::R2)));

        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
            Token::Reg(1, 0), // Invalid in this position
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert!(parser.operation("LDI".to_string()).is_err());

        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
            Token::Val(128, 0), // Invalid in this position
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert!(parser.operation("MOVE".to_string()).is_err());

        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
            Token::Val(128, 0), // Invalid in this position
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert!(parser.operation("INVALID_OPCODE".to_string()).is_err());
    }

    #[test]
    fn test_instruction() {
        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_string(), 0),
            Token::Reg(0, 0),
            Token::Val(128, 0),
            Token::Op("MOVE".to_string(), 0),
            Token::Reg(1, 0),
            Token::Reg(2, 0),
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert_eq!(parser.instruction(), Ok(Some(ast::Instruction::LDI(ast::Register::R0, 128))));
        assert_eq!(parser.instruction(), Ok(Some(ast::Instruction::MOVE(ast::Register::R1, ast::Register::R2))));
        assert_eq!(parser.instruction(), Ok(None));

        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_string(), 0),
            Token::Reg(0, 0),
            Token::Reg(1, 0), // Invalid in this position
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert!(parser.instruction().is_err());

        let tokens: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("MOVE".to_string(), 0),
            Token::Reg(0, 0),
            Token::Val(128, 0), // Invalid in this position
        ]);
        let mut parser : Parser = Parser::new(tokens);

        assert!(parser.instruction().is_err());
    }
}