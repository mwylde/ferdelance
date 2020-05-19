use crate::ast::Expr::Id;
use crate::ast::{BinOp, Decl, Expr, MonOp, Program};
use crate::x86asm::Arg::*;
use crate::x86asm::Instruction::*;
use crate::x86asm::Register64::*;
use crate::x86asm::Size::QwordPtr;
use crate::x86asm::{Arg, Instruction};
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

const TRUE: Arg = Arg::Const(0xffffffff);
const FALSE: Arg = Arg::Const(0x7fffffff);

const ERROR_OVERFLOW: &str = "error_overflow";
const ERROR_NON_INT: &str = "error_nonint";
const ERROR_NON_BOOL: &str = "error_nonbool";
const ERROR_NON_TUPLE: &str = "error_nontuple";
const ERROR_INDEX_TOO_SMALL: &str = "error_indextoosmall";
const ERROR_INDEX_TOO_LARGE: &str = "error_indextoolarge";
const ERROR_NON_LAMBDA: &str = "error_nonlambda";

#[derive(Copy, Clone)]
enum EnvVal {
    Stack(i64),
    Heap(i64),
}

impl EnvVal {
    fn arg(&self) -> Arg {
        match self {
            EnvVal::Stack(si) => Reg64Offset(RSP, *si),
            EnvVal::Heap(hi) => Reg64Offset(R15, *hi as i64),
        }
    }
}

type Env = Vec<(String, EnvVal)>;

fn find_env(env: &Env, var: &str) -> Option<EnvVal> {
    env.iter()
        .rev()
        .find(|(name, _)| name == var)
        .map(|(_, val)| *val)
}

pub struct Compiler {
    label_count: u64,
}

fn find_duplicates<I, T: Eq + Hash + Clone>(v: I) -> Vec<T>
where
    I: Iterator<Item = T>,
{
    let mut map = HashMap::new();
    for x in v {
        let count = map.entry(x.clone()).or_insert(0);
        *count += 1;
    }

    map.into_iter()
        .filter(|(_, v)| *v > 1)
        .map(|(k, _)| k)
        .collect()
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { label_count: 0 }
    }

    fn validate_expr(
        expr: &Expr,
        decls: &[Decl],
        variables: &mut Vec<String>,
        errors: &mut Vec<String>,
    ) {
        match expr {
            Expr::Id(name) => {
                if !variables.contains(name) && !decls.iter().find(|d| d.name == *name).is_some() {
                    errors.push(format!("Variable identifier {} unbounded", name))
                }
            }
            Expr::Let(bindings, body) => {
                let names: Vec<&String> = find_duplicates(bindings.iter().map(|(k, _)| k));
                for name in &names {
                    errors.push(format!("Multiple bindings for variable {}", name));
                }

                for (name, e) in bindings {
                    Compiler::validate_expr(e, decls, variables, errors);
                    variables.push(name.clone());
                }

                Compiler::validate_expr(body, decls, variables, errors);

                for _ in 0..bindings.len() {
                    variables.pop();
                }
            }

            Expr::If(cond, lh, rh) => {
                Compiler::validate_expr(cond, decls, variables, errors);
                Compiler::validate_expr(lh, decls, variables, errors);
                Compiler::validate_expr(rh, decls, variables, errors);
            }
            Expr::Prim1(_, expr) => {
                Compiler::validate_expr(expr, decls, variables, errors);
            }
            Expr::Prim2(_, lh, rh) => {
                Compiler::validate_expr(lh, decls, variables, errors);
                Compiler::validate_expr(rh, decls, variables, errors);
            }
            Expr::App(func, args) => {
                Compiler::validate_expr(func, decls, variables, errors);

                for arg in args {
                    Compiler::validate_expr(arg, decls, variables, errors);
                }

                if let Id(id) = &**func {
                    // this may be a normal function call, not a lambda
                    if let Some(decl) = decls.iter().find(|d| d.name == *id) {
                        // if we find it, validate the args
                        if decl.params.len() != args.len() {
                            errors.push(format!(
                                "Invalid Arity for function '{}'; expected {} \
                        parameters but found {}",
                                id,
                                decl.params.len(),
                                args.len()
                            ));
                        }
                    }
                }
            }

            Expr::Tuple(es) => {
                for e in es {
                    Compiler::validate_expr(e, decls, variables, errors);
                }
            }
            Expr::GetItem(tuple, index) => {
                Compiler::validate_expr(tuple, decls, variables, errors);
                Compiler::validate_expr(index, decls, variables, errors);
            }
            Expr::SetItem(tuple, index, new_val) => {
                Compiler::validate_expr(tuple, decls, variables, errors);
                Compiler::validate_expr(index, decls, variables, errors);
                Compiler::validate_expr(new_val, decls, variables, errors);
            }
            Expr::Block(es) => {
                for e in es {
                    Compiler::validate_expr(e, decls, variables, errors);
                }
            }
            Expr::Lambda(params, e) => {
                // check for duplicate parameters
                let duplicates: Vec<&String> = find_duplicates(params.iter());
                for name in &duplicates {
                    errors.push(format!("Duplicate parameter '{}' in lambda", name));
                }

                for param in params {
                    variables.push(param.clone());
                }

                Self::validate_expr(e, decls, variables, errors);

                for _ in params {
                    variables.pop();
                }
            }
            // terminal, nothing to do here
            Expr::Number(_) => {}
            Expr::Bool(_) => {}
        }
    }

    fn validate_decl(decl: &Decl, decls: &[Decl], errors: &mut Vec<String>) {
        // check for duplicate parameters
        let duplicates: Vec<&String> = find_duplicates(decl.params.iter());
        for name in &duplicates {
            errors.push(format!(
                "Duplicate parameter '{}' in function '{}'",
                name, decl.name
            ));
        }

        let mut variables = vec![];
        for param in &decl.params {
            variables.push(param.clone());
        }

        Self::validate_expr(&decl.body, decls, &mut variables, errors);

        for _ in &decl.params {
            variables.pop().unwrap();
        }
    }

    fn validate_program(program: &Program, errors: &mut Vec<String>) {
        let dup_decls: Vec<&String> = find_duplicates(program.decls.iter().map(|d| &d.name));
        for name in dup_decls {
            errors.push(format!("Duplicate function '{}'", name));
        }

        let mut decls = program.decls.clone();
        decls.push(Decl {
            name: "print".to_string(),
            params: vec!["value".to_string()],
            body: Expr::Number(1),
        });
        decls.push(Decl {
            name: "input".to_string(),
            params: vec!["x".to_string()],
            body: Expr::Number(1),
        });

        for decl in &program.decls {
            Self::validate_decl(decl, &decls, errors);
        }

        Self::validate_expr(&program.init, &decls, &mut vec![], errors);
    }

    pub fn find_free_vars(
        env: &mut Vec<String>,
        decls: &[Decl],
        e: &Expr,
        free: &mut HashSet<String>,
    ) {
        match e {
            // interesting cases
            Id(v) => {
                // is this in our param list?
                if !env.contains(v)
                    && !decls.iter().any(|d| d.name == *v)
                    && *v != "print"
                    && *v != "input"
                {
                    free.insert(v.clone());
                }
            }
            Expr::Let(bindings, body) => {
                // add bindings
                for (name, e) in bindings {
                    Self::find_free_vars(env, decls, e, free);
                    env.push(name.clone());
                }

                // evaluate body
                Self::find_free_vars(env, decls, body, free);

                // remove bindings
                for _ in bindings {
                    env.pop();
                }
            }
            Expr::Lambda(params, body) => {
                for p in params {
                    env.push(p.clone());
                }

                Self::find_free_vars(env, decls, body, free);

                for _ in params {
                    env.pop();
                }
            }

            // recursion cases
            Expr::If(cond, lh, rh) => {
                Self::find_free_vars(env, decls, cond, free);
                Self::find_free_vars(env, decls, lh, free);
                Self::find_free_vars(env, decls, rh, free);
            }
            Expr::Prim1(_, e) => {
                Self::find_free_vars(env, decls, e, free);
            }
            Expr::Prim2(_, lh, rh) => {
                Self::find_free_vars(env, decls, lh, free);
                Self::find_free_vars(env, decls, rh, free);
            }
            Expr::App(f, args) => {
                Self::find_free_vars(env, decls, f, free);
                for arg in args {
                    Self::find_free_vars(env, decls, arg, free);
                }
            }
            Expr::Tuple(tuple) => {
                for e in tuple {
                    Self::find_free_vars(env, decls, e, free);
                }
            }
            Expr::GetItem(tuple, idx) => {
                Self::find_free_vars(env, decls, tuple, free);
                Self::find_free_vars(env, decls, idx, free);
            }
            Expr::SetItem(tuple, idx, val) => {
                Self::find_free_vars(env, decls, tuple, free);
                Self::find_free_vars(env, decls, idx, free);
                Self::find_free_vars(env, decls, val, free);
            }
            Expr::Block(es) => {
                for e in es {
                    Self::find_free_vars(env, decls, e, free);
                }
            }

            // no-op cases
            Expr::Number(_) => {}
            Expr::Bool(_) => {}
        }
    }

    fn temp_label(&mut self, base: &str) -> String {
        self.label_count += 1;
        format!("temp_{}_{}", base, self.label_count)
    }

    fn throw_error(code: i64) -> Vec<Instruction> {
        //vec![Instruction::Or(Arg::Sized(Size::WORD_PTR, Box::new(Arg::Address(0))), Const(0))]
        vec![
            Mov(Arg::Reg64(RDI), Const(code)),
            Call("error".to_string()),
            Ret,
        ]
    }

    fn stack_loc(si: i64) -> Arg {
        Arg::Reg64Offset(RSP, -8 * si)
    }

    fn save_to_stack(si: i64) -> Instruction {
        Instruction::Mov(Self::stack_loc(si), Arg::Reg64(RAX))
    }

    fn restore_from_stack(si: i64) -> Instruction {
        Instruction::Mov(Arg::Reg64(RAX), Self::stack_loc(si))
    }

    // assumes you have already called Cmp
    fn compile_if(
        &mut self,
        label: &str,
        jmp_op: fn(Arg) -> Instruction,
        if_true: &[Instruction],
        if_false: &[Instruction],
    ) -> Vec<Instruction> {
        use crate::Instruction::*;

        let mut asm = vec![];

        let true_label = self.temp_label(label);
        let end_label = self.temp_label(&format!("{}_end", label));

        asm.push(jmp_op(Arg::Label(true_label.clone())));
        asm.extend_from_slice(if_false);
        asm.push(Jmp(Arg::Label(end_label.clone())));

        asm.push(Label(true_label));
        asm.extend_from_slice(if_true);
        asm.push(Label(end_label));
        asm
    }

    fn type_check(label: &str, and: i32, value: i32, arg: Arg, si: i64) -> Vec<Instruction> {
        use crate::x86asm::Arg::*;
        use crate::x86asm::Instruction::*;

        let mut asm = vec![];

        let stored = Self::stack_loc(si);
        // first, save the current RAX
        asm.push(Mov(stored.clone(), Reg64(RAX)));
        // move our value into RAX
        if arg != Reg64(RAX) {
            // but don't bother if it's already RAX
            asm.push(Mov(Reg64(RAX), arg));
        }

        // AND it with 1
        asm.push(And(Reg64(RAX), Const(and as i64)));
        // CMP
        asm.push(Cmp(Reg64(RAX), Const(value as i64)));
        // restore the old RAX
        asm.push(Mov(Reg64(RAX), stored));
        // jump if not equal to the error label
        asm.push(Jne(Arg::Label(label.to_string())));

        asm
    }

    // 0b1111
    fn bool_check(arg: Arg, si: i64) -> Vec<Instruction> {
        Self::type_check(ERROR_NON_BOOL, 3, 3, arg, si)
    }

    // 0bXXX0
    fn int_check(arg: Arg, si: i64) -> Vec<Instruction> {
        Self::type_check(ERROR_NON_INT, 1, 0, arg, si)
    }

    // 0bX001
    fn tuple_check(arg: Arg, si: i64) -> Vec<Instruction> {
        Self::type_check(ERROR_NON_TUPLE, 0b101, 1, arg, si)
    }

    // 0bX101
    fn lambda_check(arg: Arg, si: i64) -> Vec<Instruction> {
        Self::type_check(ERROR_NON_LAMBDA, 0b101, 5, arg, si)
    }

    fn binop(&mut self, op: BinOp, rh: Arg) -> Vec<Instruction> {
        use Arg::*;
        use BinOp::*;
        use Instruction::*;

        let mut asm = vec![];
        match op {
            Plus => {
                asm.push(Add(Reg64(RAX), rh));
                asm.push(Jo(Arg::Label(ERROR_OVERFLOW.to_string())));
            }
            Minus => {
                asm.push(Sub(Reg64(RAX), rh));
                asm.push(Jo(Arg::Label(ERROR_OVERFLOW.to_string())));
            }
            Times => {
                asm.push(Sar(Reg64(RAX), Const(1)));
                asm.push(IMul(Reg64(RAX), rh));
                asm.push(Jo(Arg::Label(ERROR_OVERFLOW.to_string())));
            }
            Divide => {
                asm.push(Mov(Reg64(RCX), rh));
                asm.push(Sar(Reg64(RAX), Const(1)));
                asm.push(Sar(Reg64(RCX), Const(1)));
                asm.push(Cqo);
                asm.push(IDiv(Reg64(RCX)));
                asm.push(Sal(Reg64(RAX), Const(1)));
            }
            Less => {
                asm.push(Cmp(Reg64(RAX), rh));
                asm.extend_from_slice(&self.compile_if(
                    "lt",
                    Jl,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ));
            }
            Greater => {
                asm.push(Cmp(Reg64(RAX), rh));
                asm.extend_from_slice(&self.compile_if(
                    "gt",
                    Jg,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ));
            }
            Mod => {
                asm.push(Mov(Reg64(RCX), rh));
                asm.push(Sar(Reg64(RCX), Const(1)));
                asm.push(Cqo);
                asm.push(IDiv(Reg64(RCX)));
                asm.push(Mov(Reg64(RAX), Reg64(RDX)));
            }
            Equal => unreachable!(),
        };
        asm
    }

    fn compile_prim1(
        &mut self,
        op: MonOp,
        e: &Expr,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        let mut asm = self.compile_expr(e, si, env, decls, false);
        match op {
            MonOp::Add1 => {
                asm.extend_from_slice(&Self::int_check(Reg64(RAX), si));
                asm.push(Add(Reg64(RAX), Const(2)));
            }
            MonOp::Sub1 => {
                asm.extend_from_slice(&Self::int_check(Reg64(RAX), si));
                asm.push(Sub(Reg64(RAX), Const(2)));
            }
            MonOp::IsNum => {
                asm.push(And(Reg64(RAX), Const(1)));
                asm.push(Cmp(Reg64(RAX), Const(0)));
                asm.extend_from_slice(&self.compile_if(
                    "isnum",
                    Je,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ))
            }
            MonOp::IsBool => {
                asm.push(And(Reg64(RAX), Const(3)));
                asm.push(Cmp(Reg64(RAX), Const(3)));
                asm.extend_from_slice(&self.compile_if(
                    "isbool",
                    Je,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ))
            }
            MonOp::IsTuple => {
                asm.push(And(Reg64(RAX), Const(3)));
                asm.push(Cmp(Reg64(RAX), Const(1)));
                asm.extend_from_slice(&self.compile_if(
                    "isbool",
                    Je,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ))
            }
            MonOp::Neg => {
                asm.extend_from_slice(&Self::int_check(Reg64(RAX), si));
                asm.push(Neg(Reg64(RAX)));
            }
        }

        asm
    }

    fn save_to_heap(arg: Arg, asm: &mut Vec<Instruction>) {
        asm.push(Mov(Sized(QwordPtr, Box::new(Reg64Offset(RBX, 0))), arg));
        asm.push(Add(Reg64(RBX), Const(8)));
    }

    // assumes you've already set up arguments
    fn call_c(name: &str, si: i64) -> Vec<Instruction> {
        let mut asm = vec![];

        // move the RSP to the top of the stack (which we track via si)
        asm.push(Sub(Reg64(RSP), Const(si * 8)));
        // call the c function
        asm.push(Call(name.to_string()));
        // restore RSP
        asm.push(Add(Reg64(RSP), Const(si * 8)));

        asm
    }

    fn reserve(&mut self, bytes: usize, si: i64) -> Vec<Instruction> {
        let mut asm = vec![];
        // check if we have space
        asm.push(Mov(Reg64(RAX), LabelContents("HEAP_END".to_string())));
        // end - cur
        asm.push(Sub(Reg64(RAX), Reg64(RBX)));
        // CMP (memory left, memory needed)
        asm.push(Cmp(Reg64(RAX), Const(bytes as i64)));
        let done_label = self.temp_label("after_reserve");
        asm.push(Jge(Arg::Label(done_label.clone())));

        // call to try_gc

        // arg1: alloc pointer
        asm.push(Mov(Reg64(RDI), Reg64(RBX)));
        // arg2: amount_needed
        asm.push(Mov(Reg64(RSI), Const(bytes as i64)));
        // arg3: current_esp
        asm.push(Mov(Reg64(RDX), Reg64(RSP)));
        // arg4: stack top
        asm.push(Mov(Reg64(RCX), Reg64(RSP)));
        asm.push(Sub(Reg64(RCX), Const(si * 8)));

        asm.extend_from_slice(&Self::call_c("try_gc", si));

        // update our RBX given result of try_gc
        asm.push(Mov(Reg64(RBX), Reg64(RAX)));

        asm.push(Instruction::Label(done_label));
        asm
    }

    // tuples are stored with the most significant dword (4 bytes) reserved for GC, and
    // least-significant dword storing the length
    fn compile_tuple(
        &mut self,
        es: &Vec<Expr>,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        let mut asm = vec![];
        // eval all of the args
        for (i, e) in es.iter().enumerate() {
            asm.extend_from_slice(&self.compile_expr(e, si + i as i64, env, decls, false));
            asm.push(Self::save_to_stack(si + i as i64));
        }

        let padding = (es.len() + 1) % 2;
        let size = es.len() + padding + 1;
        assert_eq!(size % 2, 0, "tuple is not aligned");

        asm.extend_from_slice(&self.reserve(size * 8, si + es.len() as i64 - 1));

        // Then save our tuple to the heap, starting with the length (not including padding)
        Self::save_to_heap(Const(es.len() as i64 * 2), &mut asm);

        // then each of the arguments
        for i in 0..es.len() {
            asm.push(Self::restore_from_stack(si + i as i64));
            Self::save_to_heap(Reg64(RAX), &mut asm);
        }

        if padding != 0 {
            Self::save_to_heap(Const(0xffffffe), &mut asm);
        }

        // finally, return the address of the header
        asm.push(Mov(Reg64(RAX), Reg64(RBX)));
        asm.push(Sub(Reg64(RAX), Const((size as i64) * 8)));
        // and tag it as a pointer
        asm.push(Or(Reg64(RAX), Const(1)));
        asm
    }

    // after this executes, the tuple address will be in RCX and the index will be on the stack at si + 1
    fn compile_tuple_access(
        &mut self,
        tuple: &Expr,
        index: &Expr,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        // compile the tuple
        let mut asm = self.compile_expr(tuple, si, env, decls, false);
        asm.push(Self::save_to_stack(si));
        // compile the index
        asm.extend_from_slice(&self.compile_expr(index, si + 1, env, decls, false));

        // check the tuple is a tuple
        asm.extend_from_slice(&Self::tuple_check(Self::stack_loc(si), si + 2));

        // and the index is a number
        asm.extend_from_slice(&Self::int_check(Reg64(RAX), si + 1));

        // get the value
        asm.push(Mov(Reg64(RCX), Self::stack_loc(si)));

        // move the length into RAX
        asm.push(Mov(Reg64(RAX), Reg64Offset(RCX, -1)));
        // clear the high bits (gc info)
        asm.push(Mov(Reg64(RDX), Const(0xFFFFFFFF)));
        asm.push(And(Reg64(RAX), Reg64(RDX)));

        // first, is the idx less than 0?
        asm.push(Cmp(
            Sized(QwordPtr, Box::new(Self::stack_loc(si + 1))),
            Const(0),
        ));
        asm.push(Jl(Arg::Label(ERROR_INDEX_TOO_SMALL.to_string())));
        // then, is it more than our element count?
        asm.push(Cmp(Reg64(RAX), Self::stack_loc(si + 1)));
        asm.push(Jle(Arg::Label(ERROR_INDEX_TOO_LARGE.to_string())));

        asm
    }

    fn compile_get_item(
        &mut self,
        tuple: &Expr,
        index: &Expr,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        let mut asm = self.compile_tuple_access(tuple, index, si, env, decls);
        // move the index into RAX
        asm.push(Self::restore_from_stack(si + 1));

        // [RCX + RAX * 2 + 3]
        asm.push(Mov(
            Reg64(RAX),
            Reg64OffsetReg64(
                RCX,
                RAX,
                4, // it's 4, not 8, because constants are stored shifted
                -1 // remove pointer tag
                                                + 8, // skip header
            ),
        ));

        asm
    }

    fn compile_set_item(
        &mut self,
        tuple: &Expr,
        index: &Expr,
        new_val: &Expr,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        let mut asm = self.compile_tuple_access(tuple, index, si, env, decls);

        // move the index into RDX
        asm.push(Instruction::Mov(Arg::Reg64(RDX), Self::stack_loc(si + 1)));

        // evaluate the new value
        asm.extend_from_slice(&self.compile_expr(new_val, si + 2, env, decls, false));

        // [RCX + RDX * 2 + 3]
        asm.push(Mov(
            Reg64OffsetReg64(
                RCX,
                RDX,
                4, // it's 4, not 8, because constants are stored shifted
                -1 // remove pointer tag
                                                  + 8, // skip header
            ),
            Reg64(RAX),
        ));

        asm
    }

    fn compile_app(
        &mut self,
        func: &Box<Expr>,
        args: &Vec<Expr>,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
        is_tco: bool,
    ) -> Vec<Instruction> {
        let after_call = self.temp_label("after_call");
        let mut asm = vec![];

        let is_lambda = if let Id(id) = &**func {
            if env.iter().find(|(name, _)| name == id).is_some() {
                true
            } else if decls.iter().find(|d| d.name == *id).is_some() {
                false
            } else {
                unreachable!(format!(
                    "found function id '{}' which is neither a variable nor decl",
                    id
                ));
            }
        } else {
            true
        };

        let jump_loc = if is_lambda {
            Reg64Offset(RAX, 0)
        } else {
            if let Id(id) = &**func {
                Arg::Label(id.clone())
            } else {
                unreachable!();
            }
        };

        let argc = args.len() as i64;
        // evaluate our args onto the stack
        for (i, arg) in args.iter().enumerate() {
            asm.extend_from_slice(&self.compile_expr(arg, si + i as i64, env, decls, false));
            asm.push(Mov(Self::stack_loc(si + i as i64), Reg64(RAX)));
        }

        // if this is a lambda, evaluate the heap pointer to RAX
        if is_lambda {
            asm.extend_from_slice(&self.compile_expr(func, si + argc + 1, env, decls, false));

            // check that it's a lambda
            asm.extend_from_slice(&Self::lambda_check(Reg64(RAX), si + argc + 1));

            // untag it
            asm.push(Sub(Reg64(RAX), Const(5)));
            // skip the header
            asm.push(Add(Reg64(RAX), Const(8)));

            // move this into R15
            asm.push(Mov(Reg64(R15), Reg64(RAX)));
        }

        if is_tco && !is_lambda {
            // if is_lambda {
            //     unimplemented!("lambdas don't work yet with tco... need to think more about this");
            // }

            // copy args to param position
            for i in 0..argc {
                asm.push(Mov(Reg64(RCX), Self::stack_loc(si + i)));
                asm.push(Mov(Reg64Offset(RSP, -(i * 8 + 16)), Reg64(RCX)));
            }
            // jump
            asm.push(Jmp(jump_loc));
        } else {
            // save our return pointer
            asm.push(Mov(
                Arg::Sized(QwordPtr, Box::new(Self::stack_loc(si + argc))),
                Arg::Label(after_call.clone()),
            ));

            // save the current RSP
            asm.push(Mov(Reg64Offset(RSP, -(si + argc + 1) * 8), Reg64(RSP)));

            // copy args to top of stack
            for i in 0..argc {
                asm.push(Mov(Reg64(RCX), Self::stack_loc(si + i)));
                asm.push(Mov(Reg64Offset(RSP, -(si + argc + 2 + i) * 8), Reg64(RCX)));
            }

            // move RSP to point to return pointer
            asm.push(Sub(Reg64(RSP), Const(((si + argc) * 8) as i64)));

            // call the function
            asm.push(Jmp(jump_loc));

            // set up the return
            asm.push(Instruction::Label(after_call));

            // restore RSP
            asm.push(Mov(Reg64(RSP), Reg64Offset(RSP, -16)));
        }

        asm
    }

    fn compile_lambda(
        &mut self,
        params: &[String],
        body: &Expr,
        env: &mut Env,
        si: i64,
        decls: &[Decl],
    ) -> Vec<Instruction> {
        let mut asm = vec![];
        let label = self.temp_label("lambda");
        let after_label = self.temp_label("lambda_done");

        // compute and store free variables
        let mut free = HashSet::new();
        Self::find_free_vars(&mut params.to_vec(), decls, body, &mut free);

        // reserve space on the heap for our variables
        let padding = free.len() % 2;
        let size = free.len() + 2 + padding;
        asm.extend_from_slice(&self.reserve(size * 8, si - 1));

        // store the size
        Self::save_to_heap(Const((free.len() + 1) as i64 * 2), &mut asm);
        // store the address of our label
        Self::save_to_heap(Arg::Label(label.clone()), &mut asm);
        // store each variable
        for (i, v) in free.iter().enumerate() {
            let val = find_env(env, v).expect(&format!("No binding for {}", v));

            asm.push(Mov(Reg64(RCX), val.arg()));
            Self::save_to_heap(Reg64(RCX), &mut asm);
            env.push((v.clone(), EnvVal::Heap((i as i64 + 1) * 8)));
        }

        if padding == 1 {
            Self::save_to_heap(Const(0xFF00F0), &mut asm);
        }

        // compile the body of the function, along with a jump to skip it here
        asm.push(Jmp(Arg::Label(after_label.clone())));
        asm.push(Instruction::Label(label.clone()));

        let mut si = 2;
        for p in params {
            env.push((p.clone(), EnvVal::Stack(-si * 8)));
            si += 1;
        }

        asm.extend_from_slice(&self.compile_expr(body, si, env, decls, true));
        asm.push(Ret);

        for _ in params {
            env.pop();
        }
        for _ in free {
            env.pop();
        }

        asm.push(Instruction::Label(after_label));

        // return a pointer to our heap location, tagged as a lambda
        asm.push(Mov(Reg64(RAX), Reg64(RBX)));
        // +5 for the pointer tagging, -size*8 to move it back to our header
        asm.push(Add(Reg64(RAX), Const(5 - size as i64 * 8)));

        asm
    }

    fn compile_expr(
        &mut self,
        e: &Expr,
        si: i64,
        env: &mut Env,
        decls: &[Decl],
        is_tco: bool,
    ) -> Vec<Instruction> {
        use Expr::*;

        match e {
            Number(n) => vec![Mov(Reg64(RAX), Const(*n as i64 * 2))],
            Bool(b) => vec![Mov(Reg64(RAX), if *b { TRUE } else { FALSE })],
            Prim1(op, expr) => self.compile_prim1(*op, expr, si, env, decls),
            Prim2(BinOp::Equal, lh, rh) => {
                // evaluate first arg to RAX
                let mut asm = self.compile_expr(lh, si, env, decls, false);
                // store on stack
                asm.push(Self::save_to_stack(si));
                // evaluate second arg TO RAX
                asm.extend_from_slice(&self.compile_expr(rh, si + 1, env, decls, false));
                // Compare them
                asm.push(Cmp(Self::stack_loc(si), Reg64(RAX)));
                // return true or false, depending
                asm.extend_from_slice(&self.compile_if(
                    "eq",
                    Je,
                    &vec![Mov(Reg64(RAX), TRUE)],
                    &vec![Mov(Reg64(RAX), FALSE)],
                ));

                asm
            }
            Prim2(op, lh, rh) => {
                // evaluate the first arg, to RAX
                let mut asm = self.compile_expr(lh, si, env, decls, false);
                // move it to the top of our stack
                let lh_addr = -si * 8;
                asm.push(Mov(Reg64Offset(RSP, lh_addr), Reg64(RAX)));

                match **rh {
                    Number(x) => {
                        // check the left-side is an int
                        asm.extend_from_slice(&Self::int_check(Reg64(RAX), si));
                        // move the arg back to RAX
                        asm.push(Mov(Reg64(RAX), Reg64Offset(RSP, lh_addr)));
                        // perform the op
                        asm.extend_from_slice(&self.binop(*op, Const(x as i64 * 2)));
                    }
                    _ => {
                        // evaluate the second arg, to RAX
                        asm.extend_from_slice(&self.compile_expr(rh, si + 1, env, decls, false));
                        // move the second arg to the top of the stack
                        let rh_addr = -(si + 1) * 8;
                        asm.push(Mov(Reg64Offset(RSP, rh_addr), Reg64(RAX)));

                        // check both are ints
                        asm.extend_from_slice(&Self::int_check(Reg64Offset(RSP, lh_addr), si + 2));
                        asm.extend_from_slice(&Self::int_check(Reg64Offset(RSP, rh_addr), si + 2));

                        // move the first arg to RAX
                        asm.push(Mov(Reg64(RAX), Reg64Offset(RSP, lh_addr)));

                        // perform op between RAX and top of stack
                        asm.extend_from_slice(&self.binop(*op, Reg64Offset(RSP, rh_addr)));
                    }
                }

                asm
            }
            Id(id) => {
                let val = find_env(env, id).expect(&format!("No binding for variable '{}'", id));

                vec![Mov(Reg64(RAX), val.arg())]
            }
            Let(bindings, body) => {
                let mut si = si;
                let mut asm = vec![];
                // first, add the bindings to the stack
                for (name, value) in bindings {
                    asm.extend_from_slice(&self.compile_expr(value, si, env, decls, false));
                    asm.push(Mov(Self::stack_loc(si), Reg64(RAX)));
                    env.push((name.clone(), EnvVal::Stack(-si * 8)));
                    si += 1;
                }

                // next, evaluate the body
                asm.extend_from_slice(&self.compile_expr(body, si, env, decls, is_tco));

                // finally, pop the bindings off the stack
                for _ in bindings {
                    env.pop().unwrap();
                }

                asm
            }
            If(cond, true_expr, false_expr) => {
                let mut asm = self.compile_expr(cond, si, env, decls, false);

                asm.extend_from_slice(&Self::bool_check(Reg64(RAX), si));

                let true_asm = self.compile_expr(true_expr, si, env, decls, is_tco);
                let false_asm = self.compile_expr(false_expr, si, env, decls, is_tco);

                asm.push(Mov(Reg64(RCX), TRUE));
                asm.push(Cmp(Reg64(RAX), Reg64(RCX)));
                asm.extend_from_slice(&self.compile_if("if", Je, &true_asm, &false_asm));
                asm
            }
            App(func, args) => {
                match &**func {
                    Id(id) if id == "print" => {
                        let mut asm = self.compile_expr(&args[0], si, env, decls, false);
                        // move our argument to RDI
                        asm.push(Mov(Sized(QwordPtr, Box::new(Reg64(RDI))), Reg64(RAX)));
                        asm.extend_from_slice(&Self::call_c("print", si));
                        asm
                    }
                    Id(id) if id == "input" => {
                        let mut asm = self.compile_expr(&args[0], si, env, decls, false);
                        // check this is an int
                        asm.extend_from_slice(&Self::int_check(Reg64(RAX), si));
                        // move our argument to RDI
                        asm.push(Mov(Sized(QwordPtr, Box::new(Reg64(RDI))), Reg64(RAX)));
                        asm.extend_from_slice(&Self::call_c("input", si));
                        asm
                    }
                    _ => self.compile_app(func, args, si, env, decls, is_tco),
                }
            }
            Tuple(es) => self.compile_tuple(es, si, env, decls),
            GetItem(tuple, e) => self.compile_get_item(tuple, e, si, env, decls),
            SetItem(tuple, idx, new_val) => {
                self.compile_set_item(tuple, idx, new_val, si, env, decls)
            }
            Block(exprs) => {
                let mut asm = vec![];
                let (last, rest) = exprs.split_last().unwrap();
                for e in rest {
                    asm.extend_from_slice(&self.compile_expr(e, si, env, decls, false));
                }
                asm.extend_from_slice(&self.compile_expr(last, si, env, decls, is_tco));
                asm
            }
            Lambda(params, body) => self.compile_lambda(params, body, env, si, decls),
        }
    }

    fn compile_decl(&mut self, decl: &Decl, decls: &[Decl]) -> Vec<Instruction> {
        let mut asm = vec![];
        asm.push(Instruction::Label(decl.name.clone()));

        let mut si = 2;

        let mut env = vec![];
        for p in &decl.params {
            env.push((p.clone(), EnvVal::Stack(-si * 8)));
            si += 1;
        }

        asm.extend_from_slice(&self.compile_expr(&decl.body, si, &mut env, decls, true));
        asm.push(Ret);
        asm
    }

    fn compile_program(&mut self, program: &Program) -> Vec<Instruction> {
        let mut asm = vec![
            Instruction::Label("our_code_starts_here".to_string()),
            // store the heap address in RBX, and align it to 16 bytes
            Mov(Reg64(RBX), Reg64(RDI)),
            //Add(Reg64(RBX), Const(8)),
            Mov(Reg64(RCX), Const(0xFFF_FFFF_FFFF_FFF0)),
            And(Reg64(RBX), Reg64(RCX)),
            // store the highest stack address in STACK_BOTTOM (return address to C)
            Mov(LabelContents("STACK_BOTTOM".to_string()), Reg64(RSP)),
            //Mov(RegOffset(-4, ESP), Sized(DWORD_PTR, Const 0)), // set old ESP to 0
        ];

        // start at si 3 so that the first stack frame looks like other stack frames in our lang
        asm.extend_from_slice(&self.compile_expr(
            &program.init,
            3,
            &mut vec![],
            &program.decls,
            false,
        ));
        asm.push(Instruction::Ret);
        for decl in &program.decls {
            asm.extend_from_slice(&self.compile_decl(decl, &program.decls));
        }

        for (i, e) in [
            ERROR_NON_INT,
            ERROR_NON_BOOL,
            ERROR_OVERFLOW,
            ERROR_NON_TUPLE,
            ERROR_INDEX_TOO_SMALL,
            ERROR_INDEX_TOO_LARGE,
            ERROR_NON_LAMBDA,
        ]
        .iter()
        .enumerate()
        {
            asm.push(Instruction::Label(e.to_string()));
            asm.extend_from_slice(&Compiler::throw_error(i as i64 + 1));
        }

        asm
    }

    pub fn parse(&mut self, s: &str) -> Program {
        crate::parser::parse(s)
    }

    pub fn validate(&mut self, program: &Program) -> Vec<String> {
        let mut errors = vec![];
        Compiler::validate_program(program, &mut errors);
        errors
    }

    pub fn compile(&mut self, program: Program) -> Vec<Instruction> {
        debug!("{:?}", program);
        self.compile_program(&program)
    }
}
