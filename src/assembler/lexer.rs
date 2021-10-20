//! This lexer tokenizes BASM.
use std::collections::VecDeque;
use std::io::{BufReader, BufRead, Read};

// Tokens are Tuples of the token value and the line they appear on.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    Op(String, usize),
    Reg(u16, usize),
    Val(u8, usize),
}

/// BASM only supports a single instruction per line.
/// The expected sequence of tokens is OPCODE REG REG
/// or OPCODE REG IMMEDIATE.
pub fn tokenize<T: Read + ?Sized>(reader: Box<T>) -> VecDeque<Token> {
    let mut tokens: VecDeque<Token> = VecDeque::with_capacity(256);
    let mut error_count: usize = 0;

    for (index, line) in BufReader::new(reader).lines().enumerate() {
        match line {
            Ok(s) => match tokenize_line(s, index) {
                Ok(mut toks) => tokens.append(&mut toks),
                Err(e) => {
                    error_count += 1;
                    error!("Error: Invalid token on line {}: {}", index, e.to_string());
                },
            },
            Err(e) => {
                error_count += 1;
                error!("Error reading line {}: {}", index, e.to_string());
            },
        }
    }

    if error_count > 0 {
        error!("Stopped assembly due to {} lexer error(s).", error_count);
        std::process::exit(1);
    }
    tokens
}

fn tokenize_line(line: String, line_num: usize) -> Result<VecDeque<Token>, String> {
    let mut out : VecDeque<Token> = VecDeque::with_capacity(3);

    let mut sb = String::new();
    'mainloop: for c in line.chars() {
        match c {
            ';' => break 'mainloop,
            // Ignore commas, tabs, linefeeds, vertical tabs, form feeds, and carriage returns.
            // However, they are Transition characters - they bound tokens. This means that
            // When we see one of these we need to check if the string builder variable
            // has a valid token.
            '\t' | '\n' | '\x0B' | '\x0C' | '\x0D' | ' ' | ',' => {
                match process_token(sb.clone(), line_num) {
                    Ok(Some(tok)) => {
                        out.push_back(tok);
                        sb.clear();
                    },
                    Ok(None) => {},
                    Err(_) => return Err(sb),
                }
            }

            // All other characters can be added to the string
            _ => sb.push(c),
        };
    }
    // Process any final token that may be in the buffer.
    if !sb.is_empty() {
        match process_token(sb.clone(), line_num) {
            Ok(Some(tok)) => out.push_back(tok),
            Ok(None) => {},
            Err(_) => return Err(sb),
        };
    }

    Ok(out)
}

fn process_token(sb: String, line: usize) -> Result<Option<Token>, ()> {
    // Short-circuit if there's nothing to process.
    if sb.is_empty() {
        return Ok(None);
    }

    // Check if it's an Opcode
    if let Some(op) = tokenize_op(sb.clone(), line) {
        return Ok(Some(op));
    }

    // Check if it's a register.
    if let Some(op) = tokenize_reg(sb.clone(), line) {
        return Ok(Some(op));
    }

    if let Some(op) = tokenize_const(sb.clone(), line) {
        return Ok(Some(op));
    }

    return Err(());
}

fn tokenize_op(sb: String, line: usize) -> Option<Token> {
    match sb.as_str() {
        "NOP" | "ADD" | "LDI" |
        "SUB" | "SUBI"| "AND" |
        "OR"  | "XOR" | "MOVE"|
        "MOV" | "SR"  | "SL"  |
        "IN"  | "OUT" | "JZ"  |
        "JLT" | "J"   => Some(Token::Op(sb, line)),

        _ => None
    }
}

fn tokenize_reg(mut sb: String, line: usize) -> Option<Token> {
    if sb.starts_with("R") {
        sb.remove(0);
        // Check if the remaining string is entirely numeric.
        // If it is, we can treat this as a Reg token.
        match u16::from_str_radix(&sb, 10) {
            Ok(val) => Some(Token::Reg(val, line)),
            Err(_) => None,
        }
    } else {
        None
    }
}

fn tokenize_const(sb: String, line: usize) -> Option<Token> {
    // Short-circuit in the ambiguous case: 0 is always 0.
    if sb == "0" { return Some(Token::Val(0,line)); }
    if sb.starts_with("0x") || sb.starts_with("0X") {
        match u8::from_str_radix((sb.chars().skip(2).collect::<String>()).as_str(), 16) {
            Ok(val) => Some(Token::Val(val, line)),
            Err(_) => None,
        }
    } else if sb.starts_with("0b") || sb.starts_with("0B") {
        match u8::from_str_radix((sb.chars().skip(2).collect::<String>()).as_str(), 2) {
            Ok(val) => Some(Token::Val(val, line)),
            Err(_) => None,
        }
    } else if sb.starts_with("0") {
        match u8::from_str_radix((sb.chars().skip(1).collect::<String>()).as_str(), 8) {
            Ok(val) => Some(Token::Val(val, line)),
            Err(_) => None,
        }
    } else {
        match u8::from_str_radix(sb.as_str(), 10) {
            Ok(val) => Some(Token::Val(val, line)),
            Err(_) => None,
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tokenize_op() {
        assert_eq!(tokenize_op("NOP".to_owned(), 0), Some(Token::Op("NOP".to_owned(), 0)));
        assert_eq!(tokenize_op("ADD".to_owned(), 0), Some(Token::Op("ADD".to_owned(), 0)));
        assert_eq!(tokenize_op("LDI".to_owned(), 0), Some(Token::Op("LDI".to_owned(), 0)));
        assert_eq!(tokenize_op("SUB".to_owned(), 0), Some(Token::Op("SUB".to_owned(), 0)));
        assert_eq!(tokenize_op("SUBI".to_owned(), 0), Some(Token::Op("SUBI".to_owned(), 0)));
        assert_eq!(tokenize_op("AND".to_owned(), 0), Some(Token::Op("AND".to_owned(), 0)));
        assert_eq!(tokenize_op("OR".to_owned(), 0), Some(Token::Op("OR".to_owned(), 0)));
        assert_eq!(tokenize_op("XOR".to_owned(), 0), Some(Token::Op("XOR".to_owned(), 0)));
        assert_eq!(tokenize_op("MOVE".to_owned(), 0), Some(Token::Op("MOVE".to_owned(), 0)));
        assert_eq!(tokenize_op("SR".to_owned(), 0), Some(Token::Op("SR".to_owned(), 0)));
        assert_eq!(tokenize_op("SL".to_owned(), 0), Some(Token::Op("SL".to_owned(), 0)));
        assert_eq!(tokenize_op("IN".to_owned(), 0), Some(Token::Op("IN".to_owned(), 0)));
        assert_eq!(tokenize_op("OUT".to_owned(), 0), Some(Token::Op("OUT".to_owned(), 0)));
        assert_eq!(tokenize_op("JZ".to_owned(), 0), Some(Token::Op("JZ".to_owned(), 0)));
        assert_eq!(tokenize_op("JLT".to_owned(), 0), Some(Token::Op("JLT".to_owned(), 0)));
        assert_eq!(tokenize_op("J".to_owned(), 0), Some(Token::Op("J".to_owned(), 0)));

        assert_eq!(tokenize_op("nop".to_owned(), 0), None);
        assert_eq!(tokenize_op(" NOP ".to_owned(), 0), None);
        assert_eq!(tokenize_op("%".to_owned(), 0), None);
        assert_eq!(tokenize_op("\n \t".to_owned(), 0), None);
    }

    #[test]
    fn test_tokenize_reg() {
        // Iterate through all values in a u16.
        // At the tokenizing stage all u16 values are allowed.
        for i in 0..=u16::MAX {
            assert_eq!(tokenize_reg(format!("R{}", i.to_string()), 0), Some(Token::Reg(i, 0)));
        }

        for i in 0..=u16::MAX {
            assert_eq!(tokenize_reg(format!("{}", i.to_string()), 0), None);
        }

        for i in 0..=u16::MAX {
            assert_eq!(tokenize_reg(format!("{}R", i.to_string()), 0), None);
        }

        assert_eq!(tokenize_reg("RABCDEFG".to_owned(), 0), None);
    }

    #[test]
    fn test_tokenize_const() {
        // A const value can be all values of a u8.
        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("{}", i.to_string()), 0), Some(Token::Val(i, 0)));
        }

        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("0x{:x}", i), 0), Some(Token::Val(i, 0)));
        }

        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("0X{:x}", i), 0), Some(Token::Val(i, 0)));
        }

        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("0b{:b}", i), 0), Some(Token::Val(i, 0)));
        }

        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("0B{:b}", i), 0), Some(Token::Val(i, 0)));
        }

        for i in 0..=u8::MAX {
            assert_eq!(tokenize_const(format!("0{:o}", i), 0), Some(Token::Val(i, 0)));
        }
    }

    #[test]
    fn test_process_token() {
        assert_eq!(process_token("".to_string(), 0), Ok(None));
        assert_eq!(process_token("LDI".to_string(), 0), Ok(Some(Token::Op("LDI".to_string(), 0))));
        assert_eq!(process_token("NONSENSE".to_string(), 0), Err(()));
        assert_eq!(process_token("R6".to_string(), 0), Ok(Some(Token::Reg(6, 0))));
        assert_eq!(process_token("R12".to_string(), 0), Ok(Some(Token::Reg(12, 0))));
        assert_eq!(process_token("R146".to_string(), 0), Ok(Some(Token::Reg(146, 0))));
        assert_eq!(process_token("R1A".to_string(), 0), Err(()));
        assert_eq!(process_token("146".to_string(), 0), Ok(Some(Token::Val(146, 0))));
        assert_eq!(process_token("0146".to_string(), 0), Ok(Some(Token::Val(102, 0))));
        assert_eq!(process_token("0x46".to_string(), 0), Ok(Some(Token::Val(70, 0))));
        assert_eq!(process_token("0X46".to_string(), 0), Ok(Some(Token::Val(70, 0))));
        assert_eq!(process_token("0b10010010".to_string(), 0), Ok(Some(Token::Val(146, 0))));
        assert_eq!(process_token("0B10010010".to_string(), 0), Ok(Some(Token::Val(146, 0))));
        assert_eq!(process_token("0b1001A010".to_string(), 0), Err(()));
        assert_eq!(process_token("0f10010010".to_string(), 0), Err(()));
    }

    #[test]
    fn test_tokenize_line() {
        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 0),
        ]);
        assert_eq!(tokenize_line("LDI".to_string(), 0), Ok(v));

        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Reg(0, 0),
        ]);
        assert_eq!(tokenize_line("R0".to_string(), 0), Ok(v));

        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Val(0x56, 0),
        ]);
        assert_eq!(tokenize_line("0x56".to_string(), 0), Ok(v));

        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 0), 
            Token::Reg(0, 0),
            Token::Val(0x56, 0),
        ]);
        assert_eq!(tokenize_line("LDI R0, 0x56".to_string(), 0), Ok(v));

        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 0), 
            Token::Reg(0, 0),
            Token::Val(0x56, 0),
        ]);
        assert_eq!(tokenize_line("LDI \t\t\t\rR0, \t\n,, \t0x56".to_string(), 0), Ok(v));

        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 0), 
            Token::Reg(0, 0),
            Token::Val(0x56, 0),
            Token::Reg(1, 0),
        ]);
        // Invalid configurations of valid tokens should be allowed - that's
        // the parser's job, not the lexer's.
        assert_eq!(tokenize_line("LDI R0, 0x56, R1".to_string(), 0), Ok(v));
        
        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 0), 
            Token::Reg(0, 0),
            Token::Val(0x56, 0),
        ]);
        assert_eq!(tokenize_line("LDI R0, 0x56 ; this is a comment LDI R1 0x56".to_string(), 0), Ok(v));
    }

    #[test]
    fn test_tokenize() {
        let asm_input = "
        LDI R0 0x56
        LDI R15 8
        SL  R0 R15
        LDI R0 0x56 ; R0 should now contain 0x5656
        ".to_string();
        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 1), 
            Token::Reg(0, 1),
            Token::Val(0x56, 1),
            Token::Op("LDI".to_owned(), 2),
            Token::Reg(15, 2),
            Token::Val(8, 2),
            Token::Op("SL".to_owned(), 3),
            Token::Reg(0, 3),
            Token::Reg(15, 3),
            Token::Op("LDI".to_owned(), 4),
            Token::Reg(0, 4),
            Token::Val(0x56, 4),
        ]);

        assert_eq!(tokenize::<>(Box::new(asm_input.as_str().as_bytes())), v);

        let asm_input = "
        LDI R0 0x56
        LDI R15 8
        SL  R0 R15
        LDI R0 0x56 ; R0 should now contain 0x5656
        J  0b00000000
        ".to_string();
        let v: VecDeque<Token> = VecDeque::from(vec![
            Token::Op("LDI".to_owned(), 1), 
            Token::Reg(0, 1),
            Token::Val(0x56, 1),
            Token::Op("LDI".to_owned(), 2),
            Token::Reg(15, 2),
            Token::Val(8, 2),
            Token::Op("SL".to_owned(), 3),
            Token::Reg(0, 3),
            Token::Reg(15, 3),
            Token::Op("LDI".to_owned(), 4),
            Token::Reg(0, 4),
            Token::Val(0x56, 4),
            Token::Op("J".to_owned(), 5),
            Token::Val(0, 5),
        ]);

        assert_eq!(tokenize::<>(Box::new(asm_input.as_str().as_bytes())), v);
    }
}
