use std::collections::HashMap;

/// The types of values the VM can handle
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    None,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Str(s) => write!(f, "{}", s),
            Value::None => write!(f, "None"),
        }
    }
}

/// The bytecode instructions
#[derive(Debug, Clone)]
pub enum Bytecode {
    // Stack operations
    PushConst(usize), // index into constants
    Pop,
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    // Comparison
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    // Logic
    Not,
    // Variables
    LoadVar(String),
    StoreVar(String),
    // Control flow
    Jump(usize),           // absolute jump
    JumpIfFalse(usize),    // jump if top of stack is false
    PushLoopExit(usize),
    // Functions
    Call(String, usize),   // function name, arg count
    Return,
    // Print
    Print,
    // Scoping
    EnterScope,
    ExitScope,
    // Halt
    Halt,
    Break,
    WhileStart(usize),
    WhileEnd,
}

/// A chunk of bytecode and its constants
#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<Bytecode>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk { code: Vec::new(), constants: Vec::new() }
    }
    pub fn add_const(&mut self, val: Value) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }
}

/// The VM struct
pub struct VM {
    pub chunk: Chunk,
    pub ip: usize, // instruction pointer
    pub stack: Vec<Value>,
    pub scopes: Vec<HashMap<String, Value>>, // stack of variable scopes
    pub call_stack: Vec<(Chunk, usize, Vec<HashMap<String, Value>>)>,
    pub functions: HashMap<String, Chunk>,
    pub loop_stack: Vec<usize>, // stack of loop exit addresses
}

impl VM {
    pub fn new(chunk: Chunk) -> Self {
        VM {
            chunk,
            ip: 0,
            stack: Vec::new(),
            scopes: vec![HashMap::new()],
            call_stack: Vec::new(),
            functions: HashMap::new(),
            loop_stack: Vec::new(),
        }
    }

    pub fn with_functions(chunk: Chunk, functions: HashMap<String, Chunk>) -> Self {
        let mut vm = VM::new(chunk);
        vm.functions = functions;
        vm
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn get_var(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(val) = scope.get(name) {
                return Some(val.clone());
            }
        }
        None
    }
    fn set_var(&mut self, name: &str, val: Value) {
        // Update the first scope (from innermost to outermost) where the variable exists
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), val);
                return;
            }
        }
        // If not found in any scope, insert as a new variable in the innermost scope
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), val);
        }
    }

    fn pop_stack_or_error(&mut self, op: &str) -> Value {
        self.stack.pop().unwrap_or_else(|| {
            eprintln!("Runtime error: Stack underflow on {}", op);
            std::process::exit(1);
        })
    }

    pub fn run(&mut self) -> Value {
        self.run_internal(false)
    }

    fn run_internal(&mut self, is_function: bool) -> Value {
        loop {
            let instr = if self.ip < self.chunk.code.len() {
                self.chunk.code[self.ip].clone()
            } else {
                Bytecode::Halt
            };
            self.ip += 1;
            match instr {
                Bytecode::PushConst(idx) => {
                    let val = self.chunk.constants[idx].clone();
                    self.stack.push(val);
                }
                Bytecode::Pop => { self.pop_stack_or_error("Pop"); }
                Bytecode::Add => {
                    let b = self.pop_stack_or_error("Add (rhs)");
                    let a = self.pop_stack_or_error("Add (lhs)");
                    self.stack.push(arith_binop(a, b, |x, y| x + y, |x, y| x + y));
                }
                Bytecode::Sub => {
                    let b = self.pop_stack_or_error("Sub (rhs)");
                    let a = self.pop_stack_or_error("Sub (lhs)");
                    self.stack.push(arith_binop(a, b, |x, y| x - y, |x, y| x - y));
                }
                Bytecode::Mul => {
                    let b = self.pop_stack_or_error("Mul (rhs)");
                    let a = self.pop_stack_or_error("Mul (lhs)");
                    self.stack.push(arith_binop(a, b, |x, y| x * y, |x, y| x * y));
                }
                Bytecode::Div => {
                    let b = self.pop_stack_or_error("Div (rhs)");
                    let a = self.pop_stack_or_error("Div (lhs)");
                    self.stack.push(arith_binop(a, b, |x, y| x / y, |x, y| x / y));
                }
                Bytecode::Neg => {
                    let a = self.pop_stack_or_error("Neg");
                    self.stack.push(match a {
                        Value::Int(x) => Value::Int(-x),
                        Value::Float(x) => Value::Float(-x),
                        _ => panic!("Negation on non-number"),
                    });
                }
                Bytecode::Eq => {
                    let b = self.pop_stack_or_error("Eq (rhs)");
                    let a = self.pop_stack_or_error("Eq (lhs)");
                    self.stack.push(Value::Bool(a == b));
                }
                Bytecode::Neq => {
                    let b = self.pop_stack_or_error("Neq (rhs)");
                    let a = self.pop_stack_or_error("Neq (lhs)");
                    self.stack.push(Value::Bool(a != b));
                }
                Bytecode::Lt => {
                    let b = self.pop_stack_or_error("Lt (rhs)");
                    let a = self.pop_stack_or_error("Lt (lhs)");
                    self.stack.push(cmp_binop(a, b, |x, y| x < y, |x, y| x < y));
                }
                Bytecode::Lte => {
                    let b = self.pop_stack_or_error("Lte (rhs)");
                    let a = self.pop_stack_or_error("Lte (lhs)");
                    self.stack.push(cmp_binop(a, b, |x, y| x <= y, |x, y| x <= y));
                }
                Bytecode::Gt => {
                    let b = self.pop_stack_or_error("Gt (rhs)");
                    let a = self.pop_stack_or_error("Gt (lhs)");
                    self.stack.push(cmp_binop(a, b, |x, y| x > y, |x, y| x > y));
                }
                Bytecode::Gte => {
                    let b = self.pop_stack_or_error("Gte (rhs)");
                    let a = self.pop_stack_or_error("Gte (lhs)");
                    self.stack.push(cmp_binop(a, b, |x, y| x >= y, |x, y| x >= y));
                }
                Bytecode::Not => {
                    let a = self.pop_stack_or_error("Not");
                    self.stack.push(match a {
                        Value::Bool(b) => Value::Bool(!b),
                        _ => panic!("Not on non-bool"),
                    });
                }
                Bytecode::LoadVar(name) => {
                    let name_cloned = name.clone();
                    let val = self.get_var(&name_cloned).unwrap_or(Value::None);
                    self.stack.push(val);
                }
                Bytecode::StoreVar(name) => {
                    let name_cloned = name.clone();
                    let val = self.pop_stack_or_error("StoreVar");
                    self.set_var(&name_cloned, val);
                }
                Bytecode::Jump(addr) => {
                    self.ip = addr;
                }
                Bytecode::JumpIfFalse(addr) => {
                    let cond = if self.stack.is_empty() {
                        Value::None
                    } else {
                        self.pop_stack_or_error("JumpIfFalse")
                    };
                    let is_false = match cond {
                        Value::Bool(false) => true,
                        Value::Int(0) => true,
                        Value::None => true,
                        _ => false,
                    };
                    if is_false {
                        if matches!(self.chunk.code.get(self.ip), Some(Bytecode::EnterScope)) {
                            self.ip += 1; // skip EnterScope for else/elif
                        }
                        self.ip = addr;
                    }
                }
                Bytecode::PushLoopExit(addr) => {
                    self.loop_stack.push(addr);
                }
                Bytecode::Call(func, argc) => {
                    let func_cloned = func.clone();
                    if func_cloned == "print" {
                        let val = self.pop_stack_or_error("print");
                        println!("{}", val);
                        self.stack.push(Value::None);
                        continue;
                    }
                    // User-defined function call
                    let fn_chunk = if let Some(chunk) = self.functions.get(&func_cloned) {
                        chunk.clone()
                    } else {
                        panic!("Function not found: {}", func_cloned);
                    };
                    // Pop arguments in reverse order and assign to local variables (by convention: arg0, arg1, ...)
                    let mut arg_names: Vec<String> = (0..argc).map(|i| format!("Call arg{}", argc - i - 1)).collect();
                    let mut args = Vec::with_capacity(argc);
                    for name in &arg_names {
                        args.push(self.pop_stack_or_error(name));
                    }
                    args.reverse();
                    // Save current state
                    self.call_stack.push((self.chunk.clone(), self.ip, self.scopes.clone()));
                    self.chunk = fn_chunk;
                    self.ip = 0;
                    self.enter_scope();
                    for (i, arg) in args.into_iter().enumerate() {
                        if let Some(scope) = self.scopes.last_mut() {
                            scope.insert(format!("arg{}", i), arg);
                        }
                    }
                    let ret = self.run_internal(true);
                    self.exit_scope();
                    let (prev_chunk, prev_ip, prev_scopes) = self.call_stack.pop().unwrap();
                    self.chunk = prev_chunk;
                    self.ip = prev_ip;
                    self.scopes = prev_scopes;
                    self.stack.push(ret);
                }
                Bytecode::Return => {
                    // Only return from function context
                    if is_function {
                        return if self.stack.is_empty() { Value::None } else { self.pop_stack_or_error("Return") };
                    } else {
                        // In main chunk, treat Return as Halt
                        return if self.stack.is_empty() { Value::None } else { self.pop_stack_or_error("Return") };
                    }
                }
                Bytecode::Print => {
                    let val = self.pop_stack_or_error("Print");
                    println!("{}", val);
                }
                Bytecode::EnterScope => {
                    self.enter_scope();
                }
                Bytecode::ExitScope => {
                    self.exit_scope();
                }
                Bytecode::Halt => {
                    break;
                }
                Bytecode::Break => {
                    if let Some(exit_ip) = self.loop_stack.pop() {
                        self.ip = exit_ip;
                    } else {
                        eprintln!("Runtime error: 'break' used outside of loop");
                        std::process::exit(1);
                    }
                }
                Bytecode::WhileStart(loop_exit_ip) => {
                    self.loop_stack.push(loop_exit_ip);
                }
                Bytecode::WhileEnd => {
                    if self.loop_stack.pop().is_none() {
                        eprintln!("Runtime error: WhileEnd without matching WhileStart (loop stack underflow)");
                        std::process::exit(1);
                    }
                }
            }
        }
        Value::None
    }
}

// Helper for arithmetic binary ops
fn arith_binop(a: Value, b: Value, int_op: fn(i64, i64) -> i64, float_op: fn(f64, f64) -> f64) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(int_op(x, y)),
        (Value::Float(x), Value::Float(y)) => Value::Float(float_op(x, y)),
        (Value::Int(x), Value::Float(y)) => Value::Float(float_op(x as f64, y)),
        (Value::Float(x), Value::Int(y)) => Value::Float(float_op(x, y as f64)),
        (Value::Str(x), Value::Str(y)) => Value::Str(x + &y),
        (a, b) => {
            eprintln!("Runtime error: Invalid operands for arithmetic: {} and {}", a, b);
            std::process::exit(1);
        }
    }
}

// Helper for comparison binary ops
fn cmp_binop(a: Value, b: Value, int_op: fn(i64, i64) -> bool, float_op: fn(f64, f64) -> bool) -> Value {
    match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(int_op(x, y)),
        (Value::Float(x), Value::Float(y)) => Value::Bool(float_op(x, y)),
        (Value::Int(x), Value::Float(y)) => Value::Bool(float_op(x as f64, y)),
        (Value::Float(x), Value::Int(y)) => Value::Bool(float_op(x, y as f64)),
        (a, b) => {
            eprintln!("Runtime error: Invalid operands for comparison: {} and {}", a, b);
            std::process::exit(1);
        }
    }
} 