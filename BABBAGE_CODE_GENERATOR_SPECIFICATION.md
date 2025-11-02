# Babbage Code Generator Specification

> **⚠️ NOTE**: This file has been reorganized. The authoritative version is now at:  
> **[BABBAGE_ANALYTICAL_ENGINE/specifications/BABBAGE_CODE_GENERATOR_SPECIFICATION.md](./BABBAGE_ANALYTICAL_ENGINE/specifications/BABBAGE_CODE_GENERATOR_SPECIFICATION.md)**  
> See [BABBAGE_FILES_MOVED_README.md](./BABBAGE_FILES_MOVED_README.md) for more information.

**Date**: 2025-10-31  
**Phase**: Week 7 Foundation (Critical Path)  
**Purpose**: Transform Babbage IR to Babbage machine code (32-instruction ISA)  
**Scope**: Register allocation, instruction selection, code generation strategy

---

## Executive Summary

The Babbage Code Generator converts from Babbage Intermediate Representation (IR) to executable Babbage machine code. It performs three critical transformations:

1. **Register Allocation**: Map IR values to 4 physical registers (A, B, C, D)
2. **Instruction Selection**: IR operations → Babbage ISA instructions
3. **Code Emission**: Generate 50-bit machine code words

**Inputs**: Valid Babbage IR (from `BABBAGE_IR_SPECIFICATION.md`)  
**Outputs**: Executable Babbage machine code (2000-word binary format)  
**Constraints**: 4 registers, 2000-word memory, 50-bit instructions, 50-digit decimal

---

## Part 1: Babbage ISA Review (Quick Reference)

### 1.1 Instruction Format

```
50-bit instruction word:
[8-bit opcode] [2-bit register] [11-bit address] [29-bit immediate]

Bits:  0-7        8-9         10-20            21-49
       opcode     reg         addr             immed
```

### 1.2 32 Babbage Instructions

| Opcode | Mnemonic | Format | Semantics | Timing |
|--------|----------|--------|-----------|--------|
| 0x00 | NOP | - | No operation | 0s |
| 0x01 | ADD | reg,reg | A ← A + B | 8s |
| 0x02 | SUB | reg,reg | A ← A - B | 8s |
| 0x03 | MULT | reg,reg | A ← A × B | 400s |
| 0x04 | DIV | reg,reg | A ← A ÷ B | 750s |
| 0x05 | SQRT | reg | A ← √A | 250s |
| 0x06 | LOAD | reg,addr | reg ← memory[addr] | 15s |
| 0x07 | STOR | reg,addr | memory[addr] ← reg | 15s |
| 0x08 | JMP | addr | PC ← addr | 4s |
| 0x09 | JZ | reg,addr | if reg==0 then PC ← addr | 4s |
| 0x0A | JNZ | reg,addr | if reg!=0 then PC ← addr | 4s |
| 0x0B | JLT | reg,addr | if reg<0 then PC ← addr | 4s |
| 0x0C | JGT | reg,addr | if reg>0 then PC ← addr | 4s |
| 0x0D | JLE | reg,addr | if reg<=0 then PC ← addr | 4s |
| 0x0E | JGE | reg,addr | if reg>=0 then PC ← addr | 4s |
| 0x0F | CMP | reg,reg | A ← A - B (for flags) | 8s |
| 0x10 | CALL | addr | Push return, PC ← addr | 8s |
| 0x11 | RET | - | Pop return, PC ← return_addr | 4s |
| 0x12 | PUSH | reg | Push reg onto stack | 4s |
| 0x13 | POP | reg | Pop stack into reg | 4s |
| 0x14 | RDCRD | reg | Read punch card into reg | 30s |
| 0x15 | WRPCH | reg | Write reg to punch | 30s |
| 0x16 | WRPRN | reg | Print reg | 2s |
| 0x17 | MOV | reg,reg | reg1 ← reg2 | 4s |
| 0x18 | NEG | reg | reg ← -reg | 4s |
| 0x19 | ABS | reg | reg ← |reg| | 4s |
| 0x1A | SHL | reg,immed | reg ← reg << immed | 4s |
| 0x1B | SHR | reg,immed | reg ← reg >> immed | 4s |
| ... | ... | ... | (Reserved for future) | ... |

### 1.3 Register Mapping

```
Register Code | Name | Convention | Purpose
0            | A    | Accum      | Primary arithmetic
1            | B    | Secondary  | Secondary arithmetic, loop counter
2            | C    | Counter    | Address calculations
3            | D    | Dest       | Division result, temporary
```

---

## Part 2: Register Allocation

### 2.1 Linear Scan Register Allocation

Goal: Assign IR values to 4 physical registers, minimizing spill/fill.

**Algorithm**:
1. Compute liveness intervals for each IR value
2. Sort intervals by start position
3. Allocate registers greedily (first-fit)
4. Spill values that don't fit (store to memory)

**Pseudocode**:
```
function linear_scan_allocate(IR_function):
  intervals = compute_liveness(IR_function)
  sorted_intervals = sort(intervals, by_start)
  
  active = []              -- Currently allocated registers
  register_pool = [A, B, C, D]
  spill_location = 256     -- Memory starts at offset 256
  
  for interval in sorted_intervals:
    -- Expire old intervals
    for active_interval in active:
      if active_interval.end < interval.start:
        free_register(active_interval)
        register_pool.push(active_interval.register)
    
    -- Allocate new interval
    if register_pool is not empty:
      reg = register_pool.pop()
      allocate_register(interval, reg)
      active.push(interval)
    else:
      -- Spill: choose victim (furthest use)
      victim = choose_spill_victim(active)
      spill_memory = allocate_stack_slot()
      spill(victim, spill_memory)
      allocate_register(interval, victim.register)
      active.push(interval)
  
  return allocation_map
```

### 2.2 Liveness Analysis

Compute which IR values are simultaneously live (needed for simultaneous use).

**Liveness = range from definition to last use**

Example:
```
IR instruction 1: a = 5
IR instruction 2: b = a + 3
IR instruction 3: c = b * 2
IR instruction 4: d = a + c

Live ranges:
  a: [1, 4]     -- defined at 1, last used at 4
  b: [2, 3]     -- defined at 2, last used at 3
  c: [3, 4]     -- defined at 3, last used at 4
  d: [4, 4]     -- defined and used at 4

Simultaneous liveness at instruction 4:
  a, c are live together
  Need 2 registers for {a, c}
```

### 2.3 Register Allocation Example

**IR Function** (Fibonacci):
```
function fibonacci(n):
  block entry:
    branch_eq n, 0, base_case_0, check_case_1
  
  block base_case_0:
    a = 0               -- Instruction 1
    return a            -- Instruction 2
  
  block check_case_1:
    branch_eq n, 1, base_case_1, recursive_case
  
  block base_case_1:
    a = 1               -- Instruction 3
    return a            -- Instruction 4
  
  block recursive_case:
    b = sub n, 1        -- Instruction 5
    c = call fib, b     -- Instruction 6
    d = sub n, 2        -- Instruction 7
    e = call fib, d     -- Instruction 8
    a = add c, e        -- Instruction 9
    return a            -- Instruction 10
```

**Liveness Analysis**:
```
n:  [entry, 10]   -- parameter, used throughout
b:  [5, 6]        -- defined at 5, used at 6
c:  [6, 9]        -- defined at 6, used at 9
d:  [7, 8]        -- defined at 7, used at 8
e:  [8, 9]        -- defined at 8, used at 9
a:  [9, 10]       -- defined at 9, used at 10

Max simultaneous liveness:
  At instruction 9: {n, c, d, e} = 4 values
  At instruction 10: {a} = 1 value
```

**Register Allocation**:
```
n → A          (parameter in A)
b → B          (temporary in B)
c → C          (temporary in C)
d → D          (temporary in D)
e → memory[256] (spill to stack)

Spill code for e:
  -- Before use:
    load e, 256    -- Load from memory into register (need to recycle register)
  -- After define:
    store e, 256   -- Store to memory after defining
```

### 2.4 Spill and Fill Code Generation

When a value is spilled (doesn't fit in 4 registers):

**Spill** (value → memory):
```
store value_register, spill_address
```

**Fill** (memory → value):
```
load temp_register, spill_address
-- Use temp_register instead of original
```

Example with spill:
```
IR:                          Generated Code:
e = 10           →           a = 10            (using register A)
                             store a, 256      (spill to memory)
f = e + 5        →           load b, 256       (fill from memory)
                             c = add b, 5      (c = e + 5)
```

---

## Part 3: Instruction Selection

### 3.1 IR to Babbage ISA Mapping

**Arithmetic Operations**:
```
IR: a = add b, c          →  ADD: a ← b + c
IR: a = sub b, c          →  SUB: a ← b - c
IR: a = mul b, c          →  MULT: a ← b × c
IR: a = div b, c          →  DIV: a ← b ÷ c
IR: a = sqrt b            →  SQRT: a ← √b
IR: a = abs b             →  ABS: a ← |b|
IR: a = neg b             →  NEG: a ← -b
```

**Memory Operations**:
```
IR: a = load addr         →  LOAD: a ← memory[addr]
IR: store val, addr       →  STOR: memory[addr] ← val
IR: a = lea var           →  (computed as address offset)
```

**Control Flow**:
```
IR: jump label            →  JMP: PC ← label_address
IR: branch_eq a, b, ...   →  (CMP then conditional jump)
IR: branch_lt a, b, ...   →  (CMP then conditional jump)
...
```

**Function Calls**:
```
IR: a = call func, args   →  (spill registers, CALL, restore registers)
IR: return val            →  RET: PC ← return_address
```

### 3.2 Register-to-Register Mapping

All IR registers map to Babbage registers via allocation:

```
IR Register    Allocated To    Babbage Register
param_n        A               A (parameter convention)
temp_1         B               B
temp_2         C               C
temp_3         D               D
temp_4         memory[256]     (spilled)
```

**Instruction Emission** (example):
```
IR:   a = add b, c
Allocation: a→A, b→B, c→C
Emitted: ADD A B   (A ← B + C in Babbage ISA)
```

---

## Part 4: Memory Address Calculation

### 4.1 Static Address Assignment

Global variables and stack frames assigned static addresses:

```
Memory Layout:
0-255:        Global variables (pre-allocated)
256-511:      Stack frame for current call
512-2047:     Heap / program data

Variable Address Calculation:
  global_var x → address 100 (statically assigned)
  local_var y  → address 256 + offset_y (stack-relative)
  array a[i]   → base_addr + i (dynamic)
```

### 4.2 Address Operands in Instructions

Babbage instructions can accept:
- **Direct address** (11-bit field): `LOAD a, 100`
- **Register + offset** (computed): `c = add array_base, i; LOAD a, c`

### 4.3 Load Effective Address (LEA)

IR operation: `a = lea var`

Generates: `a ← address_of(var)` (computation of address)

Example:
```
IR:   a = lea array_x
Compiled: a = 512    (address of array_x is 512)
          LOAD d, a  (later loads value from address in a)
```

---

## Part 5: Function Call Code Generation

### 5.1 Call Sequence

**Before CALL** (caller):
```
-- Push arguments onto stack (reverse order)
store arg1, 256 + 0
store arg2, 256 + 1
...

-- Jump to function
CALL function_address
```

**Function prologue** (callee):
```
-- Callee receives arguments on stack
-- Load arguments from stack into registers (if needed)
LOAD a, 256 + 0    (load first argument)
...
```

**Function epilogue** (callee):
```
-- Prepare return value in A
a = result_value
-- Return to caller
RET
```

**After CALL** (caller):
```
-- Return value in register A
result = a
-- Clean up stack
(pop arguments if needed)
```

### 5.2 Register Preservation

**Volatile registers** (caller-saved):
- A, B, C, D (all caller-saved; callee may clobber)

**Callee-saved registers**:
- (None in Babbage; all are caller-saved)

**Consequence**: Caller must save/restore registers before/after call.

### 5.3 Example: Call Sequence

**IR**:
```
function caller(x) {
  block entry {
    a = call add, x        -- Call add(x)
    return a
  }
}

function add(y) {
  block entry {
    b = add y, 5           -- b = y + 5
    return b
  }
}
```

**Generated Code** (Babbage ISA pseudo-assembly):
```
caller:
  LOAD a, <x_address>      -- Load parameter x
  PUSH a                   -- Push argument
  CALL add_func            -- Call function
  POP a                    -- Pop return value (or use implicit convention)
  RET

add_func:
  POP b                    -- Pop argument into b (or load from stack)
  MOV c, b                 -- c = b (for add operation)
  MOV d, 5                 -- d = 5
  ADD c, d                 -- c = c + d
  MOV a, c                 -- a = c (return value)
  RET
```

---

## Part 6: Branching and Labels

### 6.1 Label Resolution

IR labels map to instruction addresses:

```
IR Label      Address in Memory
loop_start:   256
loop_body:    260
loop_exit:    300
```

**Label table** (built during IR parsing):
```
label_map = {
  "loop_start": 256,
  "loop_body": 260,
  "loop_exit": 300,
}
```

### 6.2 Branch Instruction Emission

**IR branch**:
```
IR: branch_lt a, b, true_label, false_label
```

**Generated code**:
```
CMP a, b          -- A ← A - B (sets flags)
JLT true_label    -- if A < 0, jump to true_label
JMP false_label   -- else jump to false_label
```

### 6.3 Address Fixup

When emitting code, forward references require fixup:

```
IR:
  loop_start: nop
  jump loop_end       -- Address of loop_end not yet known
  ...
  loop_end: nop

Fixup phase:
  1. First pass: collect all labels and addresses
  2. Second pass: emit instructions with resolved addresses
```

---

## Part 7: Code Generation Algorithm

### 7.1 Overall Strategy

```
function generate_code(IR_program):
  1. Register allocation (linear scan)
  2. Label resolution (map labels to addresses)
  3. Instruction emission (IR → machine code)
  4. Fixup pass (resolve forward references)
  5. Output binary

  return executable_machine_code
```

### 7.2 Two-Pass Code Generation

**Pass 1: Address calculation**
- Assign addresses to all instructions (some may be variable-length)
- Build label map

**Pass 2: Code emission**
- Generate machine code with resolved addresses
- Emit binary output

### 7.3 Pseudocode: Code Generator

```python
class CodeGenerator:
    def __init__(self, ir_program, register_allocation):
        self.ir = ir_program
        self.allocation = register_allocation
        self.label_map = {}
        self.code = []          # Sequence of machine code words
        self.address = 0
    
    def generate(self):
        # Pass 1: Label resolution
        self._resolve_labels()
        
        # Pass 2: Code emission
        for function in self.ir.functions:
            for block in function.basic_blocks:
                for instruction in block.instructions:
                    self._emit_instruction(instruction)
                self._emit_terminator(block.terminator)
        
        return self.code
    
    def _resolve_labels(self):
        """Collect all labels and their addresses"""
        for function in self.ir.functions:
            for block in function.basic_blocks:
                self.label_map[block.label] = self.address
                # Each instruction is typically 1 word; account for multiple if needed
                self.address += len(block.instructions) + 1  # +1 for terminator
    
    def _emit_instruction(self, instr):
        """Emit single IR instruction as Babbage machine code"""
        if instr.op == "add":
            self._emit_add(instr)
        elif instr.op == "sub":
            self._emit_sub(instr)
        elif instr.op == "mul":
            self._emit_mul(instr)
        # ... etc
    
    def _emit_add(self, instr):
        """Emit ADD instruction: a = add b, c"""
        target_reg = self._get_register(instr.target)
        op1_reg = self._get_register(instr.op1)
        op2_reg = self._get_register(instr.op2)
        
        # Babbage ADD: A ← A + B (only specific register combos)
        # If target is not A, need intermediate moves
        if target_reg != "A":
            self._emit_mov(target_reg, "A")    # A ← target
        if op1_reg != "A":
            self._emit_mov("A", op1_reg)       # A ← op1
        if op2_reg != "B":
            self._emit_mov("B", op2_reg)       # B ← op2
        
        # Now emit ADD A, B
        machine_word = self._encode_instruction(opcode=0x01, reg1=0, reg2=1)
        self.code.append(machine_word)
        
        # Move result back to target if needed
        if target_reg != "A":
            self._emit_mov(target_reg, "A")    # target ← A
    
    def _get_register(self, ir_value):
        """Get Babbage register for IR value"""
        return self.allocation[ir_value]
    
    def _encode_instruction(self, opcode, reg1, reg2, addr=0, immed=0):
        """Encode 50-bit instruction word"""
        word = (opcode << 42) | (reg1 << 40) | (reg2 << 38) | (addr << 27) | immed
        return word
```

---

## Part 8: Code Generation Examples

### 8.1 Example 1: Simple Addition

**IR**:
```
function add_numbers(a, b) {
  block entry {
    c = add a, b
    return c
  }
}
```

**Register allocation**:
```
a → A (parameter)
b → B (parameter)
c → A (result, overwrite a)
```

**Generated Babbage Code**:
```
Instruction 1: MOV B, B         (ensure b in B)
Instruction 2: ADD A, B         (a = a + b)
Instruction 3: RET              (return a in A)

Machine code (pseudo):
0x17 0 1 <no-addr> <no-immed>   (MOV B ← B)
0x01 0 1 <no-addr> <no-immed>   (ADD A ← A + B)
0x11 <no-reg> <no-addr>         (RET)
```

### 8.2 Example 2: Loop (Array Sum)

**IR**:
```
function sum_array(arr_addr, length) {
  local_vars: [sum, i]
  
  block entry {
    sum = 0
    i = 0
    jump loop_condition
  }
  
  block loop_condition {
    branch_lt i, length, loop_body, loop_exit
  }
  
  block loop_body {
    elem = load (arr_addr + i)   -- Simplified
    sum = add sum, elem
    i = add i, 1
    jump loop_condition
  }
  
  block loop_exit {
    return sum
  }
}
```

**Register allocation**:
```
arr_addr → A (parameter)
length → B (parameter)
sum → C (local, result)
i → D (local, loop counter)
elem → (spilled to memory[256])
```

**Generated Babbage Code** (simplified):
```
entry:
  MOV C, 0        -- sum = 0 (C ← 0)
  MOV D, 0        -- i = 0 (D ← 0)
  JMP loop_cond

loop_cond:
  CMP D, B        -- compare i with length
  JGE loop_exit   -- if i >= length, exit
  JMP loop_body

loop_body:
  ADD D, C        -- (intermediate: D + arr_base to get address)
  LOAD C, D       -- elem = arr[i] (load from address)
  ADD C, C        -- sum += elem
  ADD D, 1        -- i += 1
  JMP loop_cond

loop_exit:
  RET             -- return sum (in C)
```

---

## Part 9: Optimization Opportunities

### 9.1 Peephole Optimization

After code emission, simple optimizations:

```
Redundant move elimination:
  MOV A, B
  MOV A, B     ← eliminate second move

Constant folding:
  LOAD a, 100
  ADD a, 5     ← compute 105 at compile time if 100+5 is constant
```

### 9.2 Register Allocation Improvement

Better spill selection:
- Spill register with furthest next use
- Prefer spilling loop-invariant values outside loops

### 9.3 Code Size Reduction

Instruction combining:
```
MOV A, B
MOV B, C
ADD A, B
→
MOV A, C       (if only used once)
ADD A, B
```

---

## Part 10: Error Handling

### 10.1 Register Allocation Failure

If linear scan cannot allocate within 4 registers AND memory is exhausted:

```
Error: Cannot allocate registers for function 'foo'
  Simultaneously live: 5 values
  Available: 4 registers + limited stack
  Solution: Refactor function to reduce live values (split into multiple functions)
```

### 10.2 Label Resolution Failure

If label referenced but not defined:

```
Error: Undefined label 'loop_start' in function 'factorial'
  Likely cause: Typo in IR generation
  Solution: Check IR generation code
```

### 10.3 Stack Overflow

If stack allocation exceeds available memory:

```
Error: Stack frame too large for function 'matrix_mult'
  Stack frame size: 2000 words
  Available: 256 words (addresses 256-511)
  Solution: Reduce local variables or recursion depth
```

---

## Part 11: Integration with Backend

### 11.1 Code Generator in Pipeline

```
[Language Compiler] 
      ↓ (generates)
[Babbage IR] 
      ↓ (inputs to)
[Code Generator]
      ↓ (generates)
[Babbage Machine Code (2000-word binary)]
      ↓ (inputs to)
[Babbage Emulator / Hardware]
```

### 11.2 Code Generator API

```python
class CodeGenerator:
    def __init__(self, ir_program: IRProgram):
        """Initialize with IR program"""
    
    def generate(self) -> BabbageCode:
        """Generate executable Babbage code"""
    
    def get_label_map(self) -> Dict[str, int]:
        """Return mapping from labels to addresses"""
    
    def get_code_size(self) -> int:
        """Return total code size in words"""
```

### 11.3 Output Format

Babbage code is output as:
- **Binary**: 2000 50-bit words (10,000 bytes)
- **Hex**: Human-readable hex dump
- **Assembly**: Babbage assembly listing

---

## Part 12: Testing Code Generation

### Test Case 1: Simple Register Allocation

**IR**:
```
function test(a, b) {
  c = add a, b
  d = mul c, 2
  return d
}
```

**Expected**:
- Allocate a→A, b→B, c→C, d→A (reuse)
- Emit ADD, MULT, RET
- No spills needed (3 simultaneous values, 4 registers)

### Test Case 2: Spill Handling

**IR**:
```
function test(a, b, c, d, e) {
  f = add a, b
  g = add c, d
  h = add f, g
  i = add e, h
  return i
}
```

**Expected**:
- Allocate a→A, b→B, c→C, d→D
- Spill e to memory[256]
- Generate fill instruction before using e
- Total live values at peak: 4 (fits in 4 registers)

### Test Case 3: Loop Code Generation

**IR** (for loop):
```
function sum_loop(n) {
  local_vars: [sum, i]
  
  block entry {
    sum = 0
    i = 0
    jump cond
  }
  
  block cond {
    branch_lt i, n, body, exit
  }
  
  block body {
    sum = add sum, i
    i = add i, 1
    jump cond
  }
  
  block exit {
    return sum
  }
}
```

**Expected**:
- Labels: entry=0, cond=2, body=4, exit=10 (example addresses)
- Branch at instruction 2 jumps to address 4 or 10
- Jump at instruction 7 jumps back to address 2

---

## Part 13: Debugging and Diagnostics

### 13.1 Code Generation Tracing

Enable verbose output:
```
$ codegen --verbose input.ir output.babbage

[CODEGEN] Input: test.ir
[CODEGEN] Parsing IR...
[CODEGEN] Computing liveness...
[CODEGEN] Allocating registers...
  a → A (live: 0-10)
  b → B (live: 0-10)
  c → C (live: 2-8)
  d → memory[256] (live: 3-7, spilled)
[CODEGEN] Emitting code...
[CODEGEN] Label map:
  entry: 0
  loop_cond: 2
  loop_body: 4
  loop_exit: 10
[CODEGEN] Output: 10 instructions, 500 bytes
[CODEGEN] Done.
```

### 13.2 Instruction Listing

Human-readable output:
```
Address   Instruction              Mnemonic      Comment
0         0x17 0 1 0 0            MOV A, B      -- entry: parameter setup
1         0x17 1 2 0 0            MOV B, C      -- parameter setup
2         0x01 0 1 0 0            ADD A, B      -- c = a + b
3         0x03 0 1 0 0            MULT A, B     -- d = c * 2
4         0x11 0 0 0 0            RET           -- return d
```

### 13.3 Register Allocation Report

```
Register Allocation Report
===========================

Function: fibonacci(n)

Value       Register   Live Range   Spilled?
n           A          [0, 10]      No
b           B          [2, 3]       No
c           C          [3, 8]       No
d           D          [4, 6]       No
e           memory     [5, 7]       Yes (256 bytes)

Peak simultaneous liveness: 4 values
Register pressure: 100% (4/4 registers used)
Spill count: 1
```

---

## Summary

The Babbage Code Generator transforms IR to machine code through:

1. **Register allocation** (linear scan with spilling)
2. **Label resolution** (map IR labels to instruction addresses)
3. **Instruction emission** (IR operations → Babbage ISA)
4. **Code emission** (generate executable binary)

**Key Constraints**:
- 4 registers (A, B, C, D)
- 2000-word memory
- 50-bit instructions
- 50-digit decimal arithmetic

**Effort**: Code Generator implementation ~25 hours

**Status**: Code Generator Specification COMPLETE. Ready for implementation.

---

**Next**: Babbage Assembler Specification (converts assembly to machine code)
