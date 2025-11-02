# Babbage Intermediate Representation (IR) Specification

**Date**: 2025-10-31  
**Phase**: Week 7 Foundation (Critical Path)  
**Purpose**: Language-agnostic intermediate representation for all 8 language compilers targeting Babbage Analytical Engine  
**Scope**: IR design, semantics, memory model, code examples

---

## Executive Summary

The Babbage Intermediate Representation (BIR) is a simple, low-level IR designed as the compilation target for 7 programming languages (C, Python, Haskell, IDRIS2, LISP, Java, System F) to execute on Babbage Analytical Engine.

**Key Design Principles**:
1. **Simplicity**: Direct mapping to 32-instruction Babbage ISA
2. **Register Awareness**: Explicit 4-register (A, B, C, D) architecture
3. **Memory Explicit**: All memory operations explicit (no hidden register spills)
4. **Type Agnostic**: Language-independent (all types flatten to 50-digit decimal)
5. **Control Flow Simple**: Direct jumps, branches, calls/returns
6. **Decimal Arithmetic**: All arithmetic on 50-digit fixed-point decimals

**Compilation Pipeline**:
```
[Language] 
    ↓ (Parse + Semantic Analysis)
[AST]
    ↓ (Lowering)
[Babbage IR]
    ↓ (Register Allocation + Assembly Emission)
[Babbage Machine Code]
    ↓ (Execution)
[Babbage Emulator/Hardware]
```

---

## Part 1: IR Structure and Components

### 1.1 Program Representation

A Babbage IR program consists of:
- **Global declarations** (functions, variables)
- **Function definitions** (control flow graph)
- **Basic blocks** (sequences of IR instructions)

```
Program ::= GlobalDecl* Function*

GlobalDecl ::= GlobalVar | GlobalFunc

GlobalVar ::= 
  "global" name "=" initial_value
  | "global" name ":=" size

GlobalFunc ::=
  "function" name "(" params ")" "{" BasicBlock+ "}"

Function ::= {
  name: string
  params: Parameter[]
  basic_blocks: BasicBlock[]
  local_vars: LocalVar[]
  return_type: Type
}
```

### 1.2 Data Types

All data in Babbage IR ultimately represents 50-digit fixed-point decimals. However, the IR tracks logical types for documentation and validation:

```
Type ::=
  | "i64"              -- 64-bit integer (compile-time only)
  | "f64"              -- 64-bit float (compile-time only)
  | "dec50"            -- 50-digit decimal (Babbage native)
  | "ptr"              -- Pointer (memory address 0-2047)
  | "array" Type Size  -- Array (contiguous memory)
  | "record" {fields}  -- Record/struct (contiguous memory)
  | "void"             -- No value (function return, discard operations)
```

**Type Lowering Rules**:
- `i64`, `f64` → `dec50` (50-digit decimal representation)
- `ptr` → memory address (integer 0-2047)
- `array` → contiguous memory blocks with layout metadata
- `record` → contiguous memory with field offsets

### 1.3 Values and Operands

```
Value ::=
  | Constant(dec50)          -- Decimal constant
  | Register(reg)            -- One of A, B, C, D
  | Memory(address)          -- Direct memory access
  | Variable(name)           -- Local variable (resolved to register/memory)
  | UndefValue               -- Undefined (optimization barrier)

Register ::= "a" | "b" | "c" | "d"

Operand ::=
  | Value
  | Label(name)              -- Jump target
```

### 1.4 Basic Block Structure

A basic block is a straight-line sequence of IR instructions ending with a terminator.

```
BasicBlock ::= {
  label: string              -- Unique label within function
  instructions: Instruction[]
  terminator: Terminator
}

Instruction ::=
  | Assignment
  | Call
  | Memory Operation
  | Arithmetic Operation

Terminator ::=
  | Return(value)
  | Branch(condition, true_label, false_label)
  | Jump(label)
  | Call + Return (tail call)
```

---

## Part 2: Instruction Set

### 2.1 Assignment Instructions

```
Instruction ::= 
  | target = value
  | target = constant
  | target = source_register
```

Examples:
```
a = 42              -- Load constant 42 into register A
b = a               -- Copy register A to B
c = memory[100]     -- Load from memory address 100 into C
```

### 2.2 Arithmetic Instructions

```
Instruction ::=
  | target = add source1, source2
  | target = sub source1, source2
  | target = mul source1, source2
  | target = div source1, source2
  | target = sqrt source
  | target = abs source
  | target = neg source
  | target = min source1, source2
  | target = max source1, source2
```

**Semantics**:
- All arithmetic is 50-digit decimal fixed-point
- Division by zero → exception (handled by emulator)
- Overflow → truncation to 50 digits (no saturation)
- All operations are blocking (implicit timing model in emulator)

Examples:
```
a = add b, c        -- A ← B + C (timing: 8s on Babbage)
d = mul a, b        -- D ← A × B (timing: 400s on Babbage)
a = sqrt d          -- A ← √D (timing: 250s on Babbage)
```

### 2.3 Memory Instructions

```
Instruction ::=
  | target = load address
  | store value, address
  | target = lea variable       -- Load effective address (address of variable)
```

**Memory Model**:
- Linear 2000-word memory (addresses 0-2047)
- Each word holds 50-digit decimal
- Load: memory[addr] → register (timing: 15s)
- Store: register → memory[addr] (timing: 15s)
- LEA: variable address → register (timing: 4s)

Examples:
```
a = load 100        -- Load memory[100] into A
store a, 200        -- Store A into memory[200]
b = lea x           -- Load address of variable x into B
```

### 2.4 Control Flow Instructions

#### 2.4.1 Unconditional Jump

```
Instruction ::= jump label
```

Example:
```
jump loop_start      -- Unconditional jump to loop_start
```

#### 2.4.2 Conditional Branch

```
Instruction ::= 
  | branch_eq source1, source2, true_label, false_label
  | branch_ne source1, source2, true_label, false_label
  | branch_lt source1, source2, true_label, false_label
  | branch_gt source1, source2, true_label, false_label
  | branch_le source1, source2, true_label, false_label
  | branch_ge source1, source2, true_label, false_label
  | branch_zero source, true_label, false_label
  | branch_nonzero source, true_label, false_label
```

**Semantics**:
- Compare source1 with source2 (or source with 0)
- Jump to true_label or false_label based on condition
- No implicit status register; all comparisons explicit

Examples:
```
branch_lt a, 10, loop_continue, loop_exit
branch_zero d, handle_zero, continue
```

#### 2.4.3 Function Call

```
Instruction ::= 
  | target = call function_name, arguments
  | call function_name, arguments  -- Discard return value
```

**Semantics**:
- Push current function state onto stack
- Jump to function
- Function executes, returns to caller
- Return value in implicit register (TBD: A or dedicated register)
- Timing: 8s to call, 4s to return

Examples:
```
a = call factorial, b             -- Call factorial(b), store result in A
call print_number, a              -- Call print_number(a), discard result
```

#### 2.4.4 Return

```
Instruction ::=
  | return value
  | return                         -- Return void
```

**Semantics**:
- Return from current function
- Return value in implicit register (copied from source)
- Control transfers to caller

Examples:
```
return a              -- Return value of A
return                -- Return void
```

### 2.5 I/O Instructions

```
Instruction ::=
  | target = read_card              -- Read from punch card
  | write_printer value             -- Print decimal number
  | write_punch value               -- Write to punch card
```

**Semantics**:
- I/O is blocking
- Timing: 30s (card read/write), 2s (printer)
- Read returns 50-digit decimal
- Write outputs decimal in machine-readable format

Examples:
```
a = read_card        -- Read value from punch card into A
write_printer a      -- Print value of A
write_punch b        -- Punch value of B to output card
```

---

## Part 3: Memory Layout and Variable Allocation

### 3.1 Memory Map

```
Address Range    | Usage           | Size
-----------------+-----------------+------------
0-255            | Global variables | 256 words
256-511          | Stack frame      | 256 words (per call)
512-2047         | Heap / Program   | 1536 words
```

**Constraints**:
- Stack grows downward (from 511 toward 256)
- Heap grows upward (from 512 toward 2047)
- Stack overflow → exception
- Heap overflow → exception
- Recursion depth limited by stack size (~4 levels for typical function)

### 3.2 Local Variable Allocation

Local variables are allocated in the stack frame:

```
Function "foo" {
  local_vars: [
    x: dec50 @ offset 0
    y: dec50 @ offset 1
    z: dec50 @ offset 2
  ]
  
  block entry {
    a = load (256 + 0)      -- Load x from stack
    b = load (256 + 1)      -- Load y from stack
    c = add a, b            -- c = x + y
    store c, (256 + 2)      -- Store z on stack
    ...
  }
}
```

### 3.3 Array and Record Layout

**Array Layout** (contiguous memory):
```
array[10] of dec50
  address: base_addr
  element 0: base_addr + 0
  element 1: base_addr + 1
  ...
  element 9: base_addr + 9
```

**Record Layout** (field-based):
```
record {
  x: dec50        @ offset 0
  y: dec50        @ offset 1
  flag: dec50     @ offset 2
}
```

---

## Part 4: Register Allocation Constraints

### 4.1 Four-Register Architecture

Babbage provides 4 registers: A, B, C, D

**Register Roles** (conventional usage):
- **A (Accumulator)**: Primary arithmetic register
- **B (Secondary)**: Secondary arithmetic, loop counter
- **C (Counter/Address)**: Loop counter, address calculations
- **D (Destination)**: Division result, output register

**Compiler Responsibility**:
- Allocate all local variables to registers or memory
- Explicit spill/fill operations for overflow
- Register allocation is **NOT** implicit (unlike LLVM)

### 4.2 Register Assignment Strategy

For each function:
1. Count distinct values needed simultaneously (liveness analysis)
2. If ≤ 4: assign to registers (no spills)
3. If > 4: some values live in memory (stack), spill/fill on demand
4. All register assignments explicit in IR

Example (liveness of 3 values):
```
function sum(a, b, c) {
  block entry {
    -- Register assignment: a→A, b→B, c→C
    d = add a, b        -- D ← A + B (result in D, but not needed yet)
    result = add d, c   -- Result ← D + C (D still live, C live, need temp)
    return result       -- Return
  }
}
```

Example (liveness of 5 values → needs spill):
```
function complex(a, b, c, d, e) {
  block entry {
    -- Register assignment: a→A, b→B, c→C, d→memory[256]
    v1 = add a, b           -- v1 in D
    spill v1, 256           -- Store D to memory[256]
    v2 = add c, d_loaded    -- Need to load d from 256 first
    fill d_loaded, 256      -- Load from memory to temp
    ...
  }
}
```

---

## Part 5: Function Call Convention

### 5.1 Calling Convention

```
Caller Setup:
  1. Push arguments onto stack (in reverse order)
  2. Execute CALL instruction
  3. Control transfers to function
  
Callee Setup:
  1. Function executes
  2. Result in implicit register (register A)
  
Caller Cleanup:
  1. Function returns
  2. Caller pops arguments from stack
  3. Caller uses result from register A
```

### 5.2 Stack Frame Layout

```
Frame pointer (FP) = 256 + (stack_depth * frame_size)

Stack Frame:
  [FP - 0]:   Return address (implicit)
  [FP - 1]:   Saved registers (implicit)
  [FP - k]:   Local variables
  [FP - m]:   Spilled temporaries
  
Stack grows downward:
  Frame N at offset 256
  Frame N+1 at offset (256 + frame_size)
  ...
```

### 5.3 Example: Function Call

```
function caller() {
  block entry {
    a = 10              -- Argument for callee
    b = call add, a     -- Call add(10)
    return b            -- Return result
  }
}

function add(x) {
  block entry {
    -- x loaded from argument (implicit)
    c = add x, 5        -- c = x + 5
    return c            -- Return in A
  }
}
```

Execution:
```
caller: push 10
caller: call add (control to add, push return address)
add:    c = add x, 5        (x in some register)
add:    return c            (c in A)
caller: (return address restored, control back to caller)
caller: b = A               (result from A into b)
caller: return b
```

---

## Part 6: Code Examples

### 6.1 Example 1: Factorial (Simple Recursion)

**C Code**:
```c
long factorial(long n) {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
}
```

**Babbage IR**:
```
function factorial(n) {
  block entry {
    -- Check if n <= 1
    branch_le n, 1, base_case, recursive_case
  }
  
  block base_case {
    a = 1               -- return 1
    return a
  }
  
  block recursive_case {
    -- Compute n - 1
    b = sub n, 1        -- b = n - 1
    -- Recursive call
    c = call factorial, b  -- c = factorial(n - 1)
    -- Multiply n * factorial(n - 1)
    a = mul n, c        -- a = n * c
    return a
  }
}
```

### 6.2 Example 2: Factorial (Iterative)

**Babbage IR**:
```
function factorial(n) {
  local_vars: [result: dec50 @ offset 0]
  
  block entry {
    result = 1          -- result = 1
    b = 2               -- loop counter = 2
    jump loop_condition
  }
  
  block loop_condition {
    branch_le b, n, loop_body, loop_exit
  }
  
  block loop_body {
    result = mul result, b   -- result *= b
    b = add b, 1             -- b++
    jump loop_condition
  }
  
  block loop_exit {
    a = result          -- a = result
    return a
  }
}
```

### 6.3 Example 3: Array Sum

**Babbage IR**:
```
function sum_array(arr_addr, length) {
  local_vars: [sum: dec50 @ offset 0, i: dec50 @ offset 1]
  
  block entry {
    sum = 0             -- sum = 0
    i = 0               -- i = 0
    jump loop_condition
  }
  
  block loop_condition {
    branch_lt i, length, loop_body, loop_exit
  }
  
  block loop_body {
    -- Load arr[i]
    b = add arr_addr, i     -- b = arr_addr + i
    elem = load b           -- elem = arr[i]
    -- Add to sum
    sum = add sum, elem     -- sum += arr[i]
    -- Increment i
    i = add i, 1            -- i++
    jump loop_condition
  }
  
  block loop_exit {
    a = sum
    return a
  }
}
```

### 6.4 Example 4: Simple Record

**Babbage IR** (struct with two fields):
```
record Point {
  x: dec50 @ offset 0
  y: dec50 @ offset 1
}

function distance(p_addr) {
  local_vars: [x: dec50, y: dec50, x_sq: dec50, y_sq: dec50, sum: dec50]
  
  block entry {
    -- Load p.x
    x_addr = add p_addr, 0
    x = load x_addr
    -- Load p.y
    y_addr = add p_addr, 1
    y = load y_addr
    -- Compute x^2 + y^2
    x_sq = mul x, x
    y_sq = mul y, y
    sum = add x_sq, y_sq
    -- Return sqrt(x^2 + y^2)
    a = sqrt sum
    return a
  }
}
```

---

## Part 7: Semantic Rules

### 7.1 Type Safety

- **No implicit type conversions**: All conversions explicit in IR
- **Pointer arithmetic**: `ptr + int` allowed (address calculation)
- **Division by zero**: Runtime exception

### 7.2 Liveness and Dead Code

- **Liveness analysis**: Compiler must determine which values are live simultaneously
- **Dead code**: Compiler must eliminate unreachable blocks
- **Uninitialized variables**: Compiler must ensure all variables defined before use

### 7.3 Control Flow Correctness

- **All paths return**: All code paths must explicitly return (if function has non-void return type)
- **No unreachable code**: All blocks must be reachable from function entry or explicitly dead

### 7.4 Memory Safety

- **Bounds checking**: Not enforced by IR; compiler must insert explicit checks (if desired)
- **Use-after-free**: Not detected (memory is global, no deallocation)
- **Stack overflow**: Caught by emulator (stack exhaustion exception)

---

## Part 8: IR Validation Rules

Before code generation, IR must satisfy:

1. **All variables defined before use**: Liveness analysis
2. **All code paths return**: Control flow analysis
3. **All blocks reachable**: Dominance analysis
4. **Register constraints met**: Register allocation feasible
5. **Memory layout consistent**: No overlapping allocations

---

## Part 9: Example: Translation from C to IR

### Input: Fibonacci in C

```c
long fibonacci(long n) {
  if (n == 0) return 0;
  if (n == 1) return 1;
  return fibonacci(n - 1) + fibonacci(n - 2);
}
```

### Output: Babbage IR

```
function fibonacci(n) {
  block entry {
    branch_eq n, 0, base_case_0, check_case_1
  }
  
  block base_case_0 {
    a = 0
    return a
  }
  
  block check_case_1 {
    branch_eq n, 1, base_case_1, recursive_case
  }
  
  block base_case_1 {
    a = 1
    return a
  }
  
  block recursive_case {
    -- fib(n-1)
    b = sub n, 1
    fib_n_minus_1 = call fibonacci, b
    -- fib(n-2)
    c = sub n, 2
    fib_n_minus_2 = call fibonacci, c
    -- return fib(n-1) + fib(n-2)
    a = add fib_n_minus_1, fib_n_minus_2
    return a
  }
}
```

---

## Part 10: Compiler Requirements

Each language compiler (C, Python, Haskell, etc.) must:

1. **Parse** source code into language-specific AST
2. **Semantic analysis** (type checking, scope resolution)
3. **Lower to IR**:
   - Convert language-specific constructs to IR instructions
   - Perform register allocation (or spillage)
   - Linearize control flow to basic blocks
4. **Validate IR** against semantic rules (Part 8)
5. **Emit Babbage machine code** (via code generator)

**IR Validation Checklist**:
- [ ] All variables defined before use
- [ ] All code paths return (if non-void)
- [ ] All blocks reachable from entry
- [ ] Register allocation satisfies 4-register constraint
- [ ] Memory layout non-overlapping
- [ ] All function calls target defined functions
- [ ] All jumps target defined labels within function

---

## Part 11: Future Extensions

Potential IR extensions (not in Phase 1):

- **Exception handling**: `try`, `catch`, `throw`
- **Concurrency**: (N/A on Babbage; defer)
- **SIMD operations**: (N/A on Babbage; defer)
- **Inline assembly**: Direct Babbage instruction insertion
- **Optimization hints**: Likely inline, unroll, vectorize (limited value)

---

## Part 12: Reference Implementation

### Pseudocode: IR Builder API

```python
class IRBuilder:
    def __init__(self, function_name: str, params: List[str]):
        self.function = Function(function_name, params)
        self.current_block = None
    
    def new_block(self, label: str) -> BasicBlock:
        block = BasicBlock(label)
        self.function.add_block(block)
        self.current_block = block
        return block
    
    def emit_add(self, target: str, op1: Value, op2: Value):
        instr = BinaryOp("add", target, op1, op2)
        self.current_block.add_instruction(instr)
    
    def emit_load(self, target: str, address: Value):
        instr = Load(target, address)
        self.current_block.add_instruction(instr)
    
    def emit_branch(self, condition, op1: Value, op2: Value, 
                    true_label: str, false_label: str):
        terminator = Branch(condition, op1, op2, true_label, false_label)
        self.current_block.set_terminator(terminator)
    
    def emit_return(self, value: Value):
        terminator = Return(value)
        self.current_block.set_terminator(terminator)
    
    def emit_call(self, target: str, function_name: str, args: List[Value]):
        instr = Call(target, function_name, args)
        self.current_block.add_instruction(instr)
    
    def finalize(self) -> Function:
        return self.function
```

---

## Part 13: Testing the IR

### Test Case 1: Simple Addition

**IR**:
```
function test_add(a, b) {
  block entry {
    c = add a, b
    return c
  }
}
```

**Validation**:
- [x] All variables defined before use
- [x] All code paths return
- [x] All blocks reachable
- [x] Register allocation feasible (2 inputs + 1 output = 3 registers)

### Test Case 2: Factorial with Recursion

**Validation** (from Part 6.1):
- [x] All variables defined before use
- [x] Both base case and recursive case return
- [x] Both blocks reachable from entry
- [x] Register allocation feasible (n lives in register, recursion on stack)

---

## Summary

The Babbage IR is a minimal, language-independent intermediate representation designed to express computation in terms of Babbage's 4-register, 2000-word-memory architecture. It provides:

- **Simple instruction set**: 13 basic operations
- **Explicit register allocation**: No hidden register spills
- **Memory-aware**: Direct memory addressing
- **Function call convention**: Stack-based calling
- **Type flexibility**: All types lower to 50-digit decimals

This IR serves as the compilation target for all 8 language services, enabling each language to express its semantics in Babbage-compatible terms.

---

**Next Document**: `BABBAGE_CODE_GENERATOR_SPECIFICATION.md` will define how IR converts to Babbage machine code.

**Status**: IR Specification COMPLETE. Ready for code generator design and implementation.

**Effort Remaining (Week 7)**:
- [ ] Babbage Code Generator (25 hours)
- [ ] Babbage Assembler (15 hours)
- [ ] Babbage Assembly Service (10 hours)
- **Total**: 50 hours remaining in Week 7
