//! This AST describes a parsed pure BASM file (no preprocessor functions).
//! 
//! Execution begins with the first instruction in the file.
//! Comments are prefixed with semicolons (;) and are single-line only.
//! Instructions are delimited by newlines.
//! 
//! Supported Instructions:
//! 
//! ```nasm
//! NOP ; no-op
//! LDI RA, CONST  ; Store the 8-bit immediate value
//!               ; into the lower 8 bits of register RA.
//!               ; Upper bits will not be affected.
//! 
//! ADD RA, RB     ; RA <= RA + RB
//! SUB RA, RB     ; RA <= RA - RB
//! SUBI RA, CONST ; RA <= RA - CONST (8bit)
//! AND RA, RB     ; RA <= RA & RB (bitwise)
//! OR RA, RB      ; RA <= RA | RB (bitwise)
//! XOR RA, RB     ; RA <= RA ^ RB (bitwise)
//! SR RA, RB      ; RA <= RA >> RB
//! SL RA, RB      ; RA <= RA << RB
//! IN RA, CONST    ; RA gets value of CONST port
//! OUT RA, CONST   ; CONST port gets value of RA.
//! JZ             ; Jump to R1 if R2 is zero
//! JLT            ; Jump to R1 if R2 is less than zero
//! J              ; Unconditionally jump to R1
//! ```
//! 
//! Example (but nonsense) source file:
//! 
//! ```nasm
//! NOP
//! NOP
//! LDI R3 32           ; Inputs may be in decimal
//! LDI R4 071          ; Octal
//! LDI R5 0b10101000   ; Binary
//! LDI R6 0xDE         ; Hexadecimal
//!                     ; All values must be 8-bit.
//! 
//! ADD R1 R2 ; Instruction arguments are delimited by spaces...
//! ADD R3,R4 ; or a comma. If a comma is used, spaces are optional.
//! 
//! // The following are two invalid instructions.
//! Add R5 R6
//! add r1, R2 ; Instructions and register names are case-sensitive
//! ```

use std::fmt;

#[derive(Copy, Clone,  PartialEq, Eq, Debug)]
pub enum Instruction {
    NOP,
    ADD (Register, Register),
    LDI (Register, Immediate),
    SUB (Register, Register),
    SUBI(Register, Immediate),
    AND (Register, Register),
    OR  (Register, Register),
    XOR (Register, Register),
    MOVE(Register, Register),
    SR  (Register, Register),
    SL  (Register, Register),
    IN  (Register, Immediate),
    OUT (Register, Immediate),
    JZ  (Register, Register),
    JLT (Register, Register),
    J   (Register),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Instruction {
    /// Assembles the given instruction to binary machine instructions.
    /// All unused fields will assemble to undefined values.
    pub fn assemble(&self) -> u16 {
        use Instruction::*;
        match self {
            NOP                                   => 0x0000,
            ADD (rega, regb) |
            SUB (rega, regb) |
            AND (rega, regb) |
            OR  (rega, regb) |
            XOR (rega, regb) |  
            MOVE(rega, regb) |  
            SL  (rega, regb) |
            SR  (rega, regb) |  
            JZ  (rega, regb) |
            JLT (rega, regb)    => (self.opcode() << 12) | (rega.to_u16() << 8) | (regb.to_u16() << 4),

            LDI (rega, value) |
            SUBI(rega, value) |
            IN  (rega, value) |
            OUT (rega, value)        => (self.opcode() << 12) | (rega.to_u16() << 8) | (((*value as u16) & 0x00FF)),
            J   (rega)                   => (self.opcode() << 12) | (rega.to_u16() << 8),
        }
    }

    /// Returns the opcode of the instruction.
    fn opcode(&self) -> u16 {
        use Instruction::*;
        match self {
            NOP        => 0b0000,
            ADD(_, _)  => 0b0001,
            LDI(_, _)  => 0b0010,
            SUB(_, _)  => 0b0011,
            SUBI(_, _) => 0b0100,
            AND(_, _)  => 0b0101,
            OR(_, _)   => 0b0110,
            XOR(_, _)  => 0b0111,
            MOVE(_, _) => 0b1000,
            SR(_, _)   => 0b1001,
            SL(_, _)   => 0b1010,
            IN(_, _)   => 0b1011,
            OUT(_, _)  => 0b1100,
            JZ(_, _)   => 0b1101,
            JLT(_, _)  => 0b1110,
            J(_)       => 0b1111,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", *self as u8)
    }
}

impl std::convert::TryFrom<u16> for Register {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        use Register::*;
        // Value is unsigned so it may never be less than zero.
        // Register ID must be between 0 and 15.
        match value {
            0  => Ok(R0),
            1  => Ok(R1),
            2  => Ok(R2),
            3  => Ok(R3),
            4  => Ok(R4),
            5  => Ok(R5),
            6  => Ok(R6),
            7  => Ok(R7),
            8  => Ok(R8),
            9  => Ok(R9),
            10 => Ok(R10),
            11 => Ok(R11),
            12 => Ok(R12),
            13 => Ok(R13),
            14 => Ok(R14),
            15 => Ok(R15),
            _  => Err("registers may only have values from 0-15 inclusive".to_owned())
        }
    }
}

impl Register {
    /// Convert the register to its binary assembly equivalent.
    pub fn to_u16(&self) -> u16 {
        use Register::*;
        match self {
            R0  => 0b0000,
            R1  => 0b0001,
            R2  => 0b0010,
            R3  => 0b0011,
            R4  => 0b0100,
            R5  => 0b0101,
            R6  => 0b0110,
            R7  => 0b0111,
            R8  => 0b1000,
            R9  => 0b1001,
            R10 => 0b1010,
            R11 => 0b1011,
            R12 => 0b1100,
            R13 => 0b1101,
            R14 => 0b1110,
            R15 => 0b1111,
        }
    }
}

pub type Immediate = u8;