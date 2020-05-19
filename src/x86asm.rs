use regex::Regex;
use std::str::FromStr;
use Register::*;
use Register64::*;
use Size::{BytePtr, DwordPtr, QwordPtr, WordPtr};

#[cfg(test)]
mod test {
    use super::Arg;
    use super::Register::*;
    use super::Size::DwordPtr;
    use super::{AsmProgram, Instruction};

    #[test]
    fn test_parse_arg() {
        assert_eq!(Some(Arg::Reg(EAX)), Arg::from_str("eax"));
        assert_eq!(Some(Arg::Const(-20)), Arg::from_str("-20"));
        assert_eq!(
            Some(Arg::Sized(DwordPtr, Box::new(Arg::Reg(EAX)))),
            Arg::from_str("dword eax")
        );
        assert_eq!(Some(Arg::Address(1000)), Arg::from_str("[1000]"));
        assert_eq!(
            Some(Arg::Label("temp_end_1".to_string())),
            Arg::from_str("temp_end_1")
        );

        assert_eq!(
            Some(Arg::RegOffset(EAX, -32)),
            Arg::from_str("[eax + (-32)]")
        );

        assert_eq!(
            Some(Arg::RegOffsetReg(ECX, EAX, 2, 3)),
            Arg::from_str("[ecx + eax * (2) + (3)]")
        )
    }

    #[test]
    fn test_parse_instr() {
        use super::Register::*;
        use Arg::*;
        use Instruction::*;
        assert_eq!(
            Some(Mov(Reg(EAX), Const(10))),
            Instruction::from_str("mov eax, 10")
        );

        assert_eq!(
            Some(Instruction::Je(Arg::Label("blah_test_1".to_string()))),
            Instruction::from_str("  je blah_test_1")
        );

        assert_eq!(
            Some(Instruction::Label("blah_test_1".to_string())),
            Instruction::from_str("blah_test_1:")
        );

        assert_eq!(
            Some(Instruction::Comment(
                "this is my comment [] it ; includes stuff!".to_string()
            )),
            Instruction::from_str("  ;; this is my comment [] it ; includes stuff!")
        );
    }

    #[test]
    fn test_parse_program() {
        use super::Register::*;
        use Arg::*;
        use Instruction::*;

        let asm = r#"
  mov eax, [esp + (-4)]
  add eax, [esp + (-8)]
  jo error_overflow
ret

error_overflow:
  mov eax, 3
  push eax
  call error
ret
        "#;

        assert_eq!(
            AsmProgram {
                instructions: vec![
                    Mov(Reg(EAX), RegOffset(ESP, -4)),
                    Add(Reg(EAX), RegOffset(ESP, -8)),
                    Jo(Arg::Label("error_overflow".to_string())),
                    Ret,
                    Instruction::Label("error_overflow".to_string()),
                    Mov(Reg(EAX), Const(3)),
                    Push(Reg(EAX)),
                    Call("error".to_string()),
                    Ret
                ]
            },
            AsmProgram::from_str(asm).unwrap()
        );
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Register {
    EAX,
    EBX,
    ECX,

    ESP,
    EIP,
}

impl Register {
    fn from_str(s: &str) -> Option<Register> {
        match s {
            "eax" => Some(EAX),
            "ebx" => Some(EBX),
            "ecx" => Some(ECX),
            "eip" => Some(EIP),
            "esp" => Some(ESP),
            _ => None,
        }
    }

    pub fn to_asm(&self) -> &'static str {
        match self {
            Register::EAX => "eax",
            Register::EBX => "ebx",
            Register::ECX => "ecx",

            Register::ESP => "esp",
            Register::EIP => "eip",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
pub enum Register64 {
    // general purpose
    RAX,
    RBX,

    // stack pointer
    RSP,

    // parameter in c calling convention
    RDI,
    RSI,
    RDX,
    RCX,
    R8,
    R9,

    // extra 64-bit registers
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl Register64 {
    fn from_str(s: &str) -> Option<Register64> {
        match s {
            "rax" => Some(RAX),
            "rbx" => Some(RBX),

            "rsp" => Some(RSP),

            "rdi" => Some(RDI),
            "rsi" => Some(RSI),
            "rdx" => Some(RDX),
            "rcx" => Some(RCX),
            "r8" => Some(R8),
            "r9" => Some(R9),

            "r10" => Some(R10),
            "r11" => Some(R11),
            "r12" => Some(R12),
            "r13" => Some(R13),
            "r14" => Some(R14),
            "r15" => Some(R15),

            _ => None,
        }
    }

    pub fn to_asm(&self) -> &'static str {
        match self {
            RAX => "rax",
            RBX => "rbx",

            RSP => "rsp",

            RDI => "rdi",
            RSI => "rsi",
            RDX => "rdx",
            RCX => "rcx",
            R8 => "r8",
            R9 => "r9",
            R10 => "r10",
            R11 => "r11",
            R12 => "r12",
            R13 => "r13",
            R14 => "r14",
            R15 => "r15",
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum Size {
    QwordPtr,
    DwordPtr,
    WordPtr,
    BytePtr,
}

impl Size {
    pub fn from_str(s: &str) -> Option<Size> {
        match s {
            "qword" => Some(QwordPtr),
            "dword" => Some(DwordPtr),
            "word" => Some(WordPtr),
            "byte" => Some(BytePtr),
            _ => None,
        }
    }

    pub fn to_asm(&self) -> &'static str {
        match self {
            Size::QwordPtr => "QWORD",
            Size::DwordPtr => "DWORD",
            Size::WordPtr => "WORD",
            Size::BytePtr => "BYTE",
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Size::QwordPtr => 8,
            Size::DwordPtr => 4,
            Size::WordPtr => 2,
            Size::BytePtr => 1,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Arg {
    Const(i64),
    Address(u64),
    Reg(Register),
    Reg64(Register64),
    RegOffset(Register, i32),
    Reg64Offset(Register64, i64),
    Reg64OffsetReg64(Register64, Register64, i64, i64),
    RegOffsetReg(Register, Register, i32, i32),
    Sized(Size, Box<Arg>),
    Label(String),
    LabelContents(String),
}

impl Arg {
    pub fn to_asm(&self) -> String {
        match self {
            Arg::Const(a) => format!("{}", *a),
            Arg::Address(addr) => format!("[{}]", *addr),
            Arg::Reg(reg) => reg.to_asm().to_string(),
            Arg::RegOffset(reg, offset) => format!("[{} + ({})]", reg.to_asm(), offset),
            Arg::RegOffsetReg(reg1, reg2, mul, off) => format!(
                "[{} + {} * ({}) + ({})]",
                reg1.to_asm(),
                reg2.to_asm(),
                mul,
                off
            ),
            Arg::Sized(size, arg) => format!("{} {}", size.to_asm(), arg.to_asm()),
            Arg::Label(name) => name.clone(),
            Arg::LabelContents(name) => format!("[{}]", name),

            Arg::Reg64(reg) => reg.to_asm().to_string(),
            Arg::Reg64Offset(reg, offset) => format!("[{} + ({})]", reg.to_asm(), offset),
            Arg::Reg64OffsetReg64(reg1, reg2, mul, off) => format!(
                "[{} + {} * ({}) + ({})]",
                reg1.to_asm(),
                reg2.to_asm(),
                mul,
                off
            ),
        }
    }

    pub fn from_str(s: &str) -> Option<Arg> {
        lazy_static! {
            static ref ADDRESS_RE: Regex = Regex::new(r"^\[(\d+)]$").unwrap();
            static ref SIZED_RE: Regex = Regex::new(r"^(qword|dword|word|byte)\s+(.+)$").unwrap();
            static ref LABEL_CONTENTS_RE: Regex = Regex::new(r"^\[([a-z0-9_]+)\]$").unwrap();
            static ref LABEL_RE: Regex = Regex::new(r"^[a-z0-9_]+$").unwrap();
            static ref REG_OFFSET_RE: Regex = Regex::new(r"^\[(\w+)\s*\+\s*\((-?\d+)\)]$").unwrap();
            static ref REG_OFFSET_REG_RE: Regex =
                Regex::new(r"^\[(\w+)\s*\+\s*(\w+)\s*\*\s*\((-?\d+)\)\s*\+\s*\((-?\d+)\)]$")
                    .unwrap();
        }
        // [ecx + eax * (2) + (3)]

        if let Some(x) = i32::from_str(s).ok() {
            Some(Arg::Const(x as i64))
        } else if ADDRESS_RE.is_match(s) {
            Some(Arg::Address(
                u64::from_str(ADDRESS_RE.captures(s).unwrap().get(1).unwrap().as_str()).unwrap(),
            ))
        } else if let Some(reg) = Register::from_str(s) {
            Some(Arg::Reg(reg))
        } else if let Some(captures) = SIZED_RE.captures(s) {
            let size = Size::from_str(captures.get(1).unwrap().as_str()).unwrap();
            Some(Arg::Sized(
                size,
                Box::new(Self::from_str(captures.get(2).unwrap().as_str())?),
            ))
        } else if LABEL_RE.is_match(s) {
            Some(Arg::Label(s.to_string()))
        } else if let Some(captures) = LABEL_CONTENTS_RE.captures(s) {
            Some(Arg::LabelContents(
                captures.get(1).unwrap().as_str().to_string(),
            ))
        } else if let Some(captures) = REG_OFFSET_RE.captures(s) {
            let r_s = captures.get(1).unwrap().as_str();
            let o_s = captures.get(2).unwrap().as_str();
            if let Some(reg) = Register::from_str(r_s) {
                let offset = i32::from_str(o_s).unwrap();
                Some(Arg::RegOffset(reg, offset))
            } else if let Some(reg) = Register64::from_str(r_s) {
                let offset = i64::from_str(o_s).unwrap();
                Some(Arg::Reg64Offset(reg, offset))
            } else {
                None
            }
        } else if let Some(captures) = REG_OFFSET_REG_RE.captures(s) {
            let r1_s = captures.get(1).unwrap().as_str();
            let r2_s = captures.get(2).unwrap().as_str();
            let o_s = captures.get(3).unwrap().as_str();
            let m_s = captures.get(4).unwrap().as_str();

            if let Some(reg1) = Register::from_str(r1_s) {
                let reg2 = Register::from_str(r2_s)?;
                let offset = i32::from_str(o_s).unwrap();
                let mul = i32::from_str(m_s).unwrap();
                Some(Arg::RegOffsetReg(reg1, reg2, offset, mul))
            } else if let Some(reg1) = Register64::from_str(r2_s) {
                let reg2 = Register64::from_str(r2_s)?;
                let offset = i64::from_str(o_s).unwrap();
                let mul = i64::from_str(m_s).unwrap();
                Some(Arg::Reg64OffsetReg64(reg1, reg2, offset, mul))
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Instruction {
    Mov(Arg, Arg),

    Add(Arg, Arg),
    Sub(Arg, Arg),
    IMul(Arg, Arg),
    IDiv(Arg),
    Neg(Arg),

    Sar(Arg, Arg),
    Sal(Arg, Arg),

    And(Arg, Arg),
    Or(Arg, Arg),
    Xor(Arg, Arg),

    Label(String),
    Cmp(Arg, Arg),
    Jne(Arg),
    Je(Arg),
    Jl(Arg),
    Jle(Arg),
    Jg(Arg),
    Jge(Arg),
    Jo(Arg),
    Jmp(Arg),

    Push(Arg),
    Pop(Arg),
    Call(String),

    Comment(String),

    Ret,
    Cqo,
}

impl Instruction {
    pub fn to_asm(&self) -> String {
        match self {
            Instruction::Mov(a, b) => format!("  mov {}, {}", a.to_asm(), b.to_asm()),

            Instruction::Add(a, b) => format!("  add {}, {}", a.to_asm(), b.to_asm()),
            Instruction::Sub(a, b) => format!("  sub {}, {}", a.to_asm(), b.to_asm()),
            Instruction::IMul(a, b) => format!("  imul {}, {}", a.to_asm(), b.to_asm()),
            Instruction::IDiv(a) => format!("  idiv {}", a.to_asm()),
            Instruction::Neg(a) => format!("  neg {}", a.to_asm()),

            Instruction::Sar(a, b) => format!("  sar {}, {}", a.to_asm(), b.to_asm()),
            Instruction::Sal(a, b) => format!("  sal {}, {}", a.to_asm(), b.to_asm()),

            Instruction::And(a, b) => format!("  and {}, {}", a.to_asm(), b.to_asm()),
            Instruction::Or(a, b) => format!("  or {}, {}", a.to_asm(), b.to_asm()),
            Instruction::Xor(a, b) => format!("  xor {}, {}", a.to_asm(), b.to_asm()),

            Instruction::Label(a) => format!("\n{}:", a),
            Instruction::Cmp(a, b) => format!("  cmp {}, {}", a.to_asm(), b.to_asm()),
            Instruction::Jne(a) => format!("  jne {}", a.to_asm()),
            Instruction::Je(a) => format!("  je {}", a.to_asm()),
            Instruction::Jl(a) => format!("  jl {}", a.to_asm()),
            Instruction::Jle(a) => format!("  jle {}", a.to_asm()),
            Instruction::Jg(a) => format!("  jg {}", a.to_asm()),
            Instruction::Jge(a) => format!("  jge {}", a.to_asm()),
            Instruction::Jmp(a) => format!("  jmp {}", a.to_asm()),
            Instruction::Jo(a) => format!("  jo {}", a.to_asm()),

            Instruction::Push(a) => format!("  push {}", a.to_asm()),
            Instruction::Pop(a) => format!("  pop {}", a.to_asm()),
            Instruction::Call(a) => format!("  call {}", a),

            Instruction::Comment(c) => format!(";; {}", c),

            Instruction::Ret => "ret".to_string(),

            Instruction::Cqo => "  cqo".to_string(),
        }
    }

    pub fn from_str(s: &str) -> Option<Instruction> {
        use Instruction::*;

        let comment_re = Regex::new(r"^\s+;;\s*(.+)$").unwrap();
        let label_re = Regex::new(r"^\s*([a-z0-9_]+):\s*$").unwrap();
        let re = Regex::new(r"^\s*([a-z]+)\s+([^,]+(,[^,]+)*)$").unwrap();

        let lowercase = s.to_lowercase();

        if &lowercase == "ret" {
            return Some(Ret);
        }

        if &lowercase == "cqo" {
            return Some(Cqo);
        }

        if let Some(captures) = comment_re.captures(&lowercase) {
            return Some(Comment(captures.get(1).unwrap().as_str().to_string()));
        }

        if let Some(captures) = label_re.captures(&lowercase) {
            return Some(Label(captures.get(1).unwrap().as_str().to_string()));
        }

        let captures = re.captures(&lowercase)?;

        let args: Vec<&str> = captures
            .get(2)
            .unwrap()
            .as_str()
            .split(",")
            .map(|a| a.trim())
            .collect();

        let one_arg = |con: fn(Arg) -> Instruction| -> Option<Instruction> {
            Some(con(Arg::from_str(args.get(0)?)?))
        };

        let two_arg = |con: fn(Arg, Arg) -> Instruction| -> Option<Instruction> {
            Some(con(
                Arg::from_str(args.get(0)?)?,
                Arg::from_str(args.get(1)?)?,
            ))
        };

        let label = |con: fn(String) -> Instruction| -> Option<Instruction> {
            Some(con(args.get(0)?.to_string()))
        };

        match captures.get(1).unwrap().as_str() {
            "mov" => two_arg(Mov),
            "add" => two_arg(Add),
            "sub" => two_arg(Sub),
            "imul" => two_arg(IMul),
            "idiv" => one_arg(IDiv),
            "neg" => one_arg(Neg),

            "sar" => two_arg(Sar),
            "sal" => two_arg(Sal),

            "and" => two_arg(And),
            "or" => two_arg(Or),
            "xor" => two_arg(Xor),

            "cmp" => two_arg(Cmp),

            "jne" => one_arg(Jne),
            "je" => one_arg(Je),
            "jl" => one_arg(Jl),
            "jle" => one_arg(Jle),
            "jg" => one_arg(Jg),
            "jge" => one_arg(Jge),
            "jmp" => one_arg(Jmp),
            "jo" => one_arg(Jo),

            "push" => one_arg(Push),
            "pop" => one_arg(Pop),
            "call" => label(Call),

            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct AsmProgram {
    pub instructions: Vec<Instruction>,
}

impl AsmProgram {
    pub fn from_str(s: &str) -> Result<AsmProgram, String> {
        let whitespace = Regex::new(r"^\s+$").unwrap();

        let mut instructions = vec![];
        for (i, l) in s.lines().enumerate() {
            if l == "" || whitespace.is_match(l) {
                continue;
            }

            if let Some(instr) = Instruction::from_str(l) {
                instructions.push(instr);
            } else {
                return Err(format!("Failed to parse instruction on line {}", i));
            }
        }

        Ok(AsmProgram { instructions })
    }
}
