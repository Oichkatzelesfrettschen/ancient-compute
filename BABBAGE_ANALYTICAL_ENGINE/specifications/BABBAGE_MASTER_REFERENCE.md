# Babbage Analytical Engine - Master Reference & Best Practices

**Document Type**: Comprehensive Technical Reference
**Version**: 1.0 (2025-10-31)
**Audience**: Compiler developers, language service implementers, system architects
**Scope**: Complete Babbage ISA specification, compiler infrastructure, and implementation patterns

---

## PART 1: BABBAGE ARCHITECTURE FUNDAMENTALS

### 1.1 Historical Context

The Babbage Analytical Engine (1837) represents humanity's first attempt at a programmable computing machine, designed by Charles Babbage with crucial contributions from Ada Lovelace.

**Key Characteristics**:
- Decimal-based arithmetic (not binary)
- Mechanical implementation
- Fixed-point 50-digit numbers
- Simple 4-register architecture
- Memory addressing via column positions
- No floating-point (precision via 50 digits)

**Educational Value**:
- Demonstrates core computation concepts independent of modern electronics
- Shows that computation fundamentals transcend implementation technology
- Reveals design constraints that drive modern compiler optimization

### 1.2 Target ISA Overview

**Processor Model**:
- 4 physical registers: A (accumulator), B (secondary), C (counter), D (destination)
- 2,000-word memory (0-2047)
- 32-instruction ISA
- 50-bit instruction format
- Mechanical timing model (NOP=0s, ADD/SUB=8s, MULT=400s, DIV=750s)

**Memory Layout**:
```
[0-255]       Global variables and constants
[256-511]     Stack frame (per function invocation)
[512-2047]    Heap (program data, arrays, records)
```

**Number System**:
- 50-digit signed fixed-point decimal
- Range: -10^50 to +10^50
- Precision: exact (no floating-point rounding errors)
- Operations: addition, subtraction, multiplication, division, square root

**Instruction Format (50 bits)**:
```
Bits 49-42:   Opcode (8 bits)
Bits 41-40:   Register 1 (2 bits: A=0, B=1, C=2, D=3)
Bits 39-38:   Register 2 or flag (2 bits)
Bits 37-0:    Address/Immediate (38 bits, up to ~274 billion)
```

---

## PART 2: COMPLETE 32-INSTRUCTION ISA

### 2.1 Arithmetic Instructions (7 instructions)

**ADD - Addition**
```
Syntax: ADD reg1, reg2          (reg1 ← reg1 + reg2)
Opcode: 0x01
Timing: 8 seconds (mechanical)
Semantics: Add reg2 to reg1, store result in reg1
Flags: None (sets no condition flags)
Operands: 2 registers
Example: ADD A, B               (A ← A + B)
```

**SUB - Subtraction**
```
Syntax: SUB reg1, reg2          (reg1 ← reg1 - reg2)
Opcode: 0x02
Timing: 8 seconds
Semantics: Subtract reg2 from reg1, store result in reg1
Example: SUB A, B               (A ← A - B)
```

**MULT - Multiplication**
```
Syntax: MULT reg1, reg2         (reg1 ← reg1 × reg2)
Opcode: 0x03
Timing: 400 seconds (mechanical - very slow!)
Semantics: Multiply reg1 by reg2, store result in reg1
Overflow: Silently wraps (Babbage had no overflow detection)
Example: MULT A, B              (A ← A × B)
```

**DIV - Division**
```
Syntax: DIV reg1, reg2          (reg1 ← reg1 ÷ reg2)
Opcode: 0x04
Timing: 750 seconds (very slow)
Semantics: Divide reg1 by reg2, integer division, store in reg1
Division by zero: Undefined behavior (no runtime check)
Example: DIV A, B               (A ← A / B)
```

**SQRT - Square Root**
```
Syntax: SQRT reg                (reg ← √reg)
Opcode: 0x05
Timing: 250 seconds
Semantics: Compute square root of reg, store in reg
Operands: 1 register
Example: SQRT A                 (A ← √A)
```

**ABS - Absolute Value**
```
Syntax: ABS reg                 (reg ← |reg|)
Opcode: 0x19
Timing: ~1 second (fast)
Semantics: If reg < 0, negate it
Example: ABS A                  (A ← |A|)
```

**NEG - Negation**
```
Syntax: NEG reg                 (reg ← -reg)
Opcode: 0x18
Timing: ~1 second
Semantics: Negate value in register
Example: NEG A                  (A ← -A)
```

### 2.2 Memory Instructions (3 instructions)

**LOAD - Load from Memory**
```
Syntax: LOAD reg, addr          (reg ← memory[addr])
Opcode: 0x06
Timing: ~2 seconds
Operands: Register (reg A-D), Address (0-2047)
Semantics: Load 50-digit value from memory address into register
Example: LOAD A, 256            (A ← memory[256])
Example: LOAD B, 512            (B ← memory[512])
```

**STOR - Store to Memory**
```
Syntax: STOR reg, addr          (memory[addr] ← reg)
Opcode: 0x07
Timing: ~2 seconds
Operands: Register (reg A-D), Address (0-2047)
Semantics: Store 50-digit value from register into memory
Example: STOR A, 256            (memory[256] ← A)
```

**MOV - Move Register to Register**
```
Syntax: MOV reg1, reg2          (reg1 ← reg2)
Opcode: 0x17
Timing: ~1 second
Operands: 2 registers
Semantics: Copy value from reg2 to reg1
Example: MOV A, B               (A ← B)
Example: MOV C, A               (C ← A)
```

### 2.3 Control Flow Instructions (8 instructions)

**JMP - Unconditional Jump**
```
Syntax: JMP addr or JMP label   (PC ← addr)
Opcode: 0x08
Timing: ~1 second
Operands: Address (0-2047) or label
Semantics: Set program counter to address, continue execution there
Example: JMP 100                (jump to instruction 100)
Example: JMP loop_start         (jump to label)
```

**JZ - Jump if Zero**
```
Syntax: JZ reg, addr            (if reg == 0 then PC ← addr)
Opcode: 0x09
Timing: ~1 second
Operands: Register, Address
Semantics: If register is zero, jump; otherwise continue
Example: JZ A, exit_label       (if A == 0, jump to exit_label)
```

**JNZ - Jump if Not Zero**
```
Syntax: JNZ reg, addr           (if reg != 0 then PC ← addr)
Opcode: 0x0A
Timing: ~1 second
Operands: Register, Address
Semantics: If register is non-zero, jump; otherwise continue
Example: JNZ B, loop_start      (if B != 0, loop)
```

**JLT - Jump if Less Than Zero**
```
Syntax: JLT reg, addr           (if reg < 0 then PC ← addr)
Opcode: 0x0B
Timing: ~1 second
Operands: Register, Address
Semantics: If register is negative, jump
Example: JLT A, handle_negative
```

**JGT - Jump if Greater Than Zero**
```
Syntax: JGT reg, addr           (if reg > 0 then PC ← addr)
Opcode: 0x0C
Timing: ~1 second
Operands: Register, Address
Semantics: If register is positive, jump
Example: JGT A, positive_branch
```

**JLE - Jump if Less Than or Equal to Zero**
```
Syntax: JLE reg, addr           (if reg <= 0 then PC ← addr)
Opcode: 0x0D
Timing: ~1 second
Operands: Register, Address
Semantics: If register is <= 0, jump
```

**JGE - Jump if Greater Than or Equal to Zero**
```
Syntax: JGE reg, addr           (if reg >= 0 then PC ← addr)
Opcode: 0x0E
Timing: ~1 second
Operands: Register, Address
Semantics: If register is >= 0, jump
```

**CMP - Compare (Subtract for Comparison)**
```
Syntax: CMP reg1, reg2          (reg1 ← reg1 - reg2, for comparison)
Opcode: 0x0F
Timing: ~1 second
Operands: 2 registers
Semantics: Subtract reg2 from reg1 (result indicates relationship)
Note: Used before conditional jumps; result in reg1 determines jump
Example: CMP A, B; JZ zero_label (if A == B, jump)
```

### 2.4 Function Call Instructions (4 instructions)

**CALL - Function Call**
```
Syntax: CALL addr or CALL label (push return addr, PC ← addr)
Opcode: 0x10
Timing: ~2 seconds
Operands: Address or label
Semantics: Push current PC+1 onto stack, jump to function
Stack: Uses stack memory starting at 256
Example: CALL factorial_func
```

**RET - Return from Function**
```
Syntax: RET                     (PC ← pop from stack)
Opcode: 0x11
Timing: ~2 seconds
Operands: None
Semantics: Pop return address from stack, jump to it
Stack: Pops from stack memory
Return value: In register A (by convention)
Example: RET
```

**PUSH - Push Register onto Stack**
```
Syntax: PUSH reg                (stack[SP] ← reg; SP++)
Opcode: 0x12
Timing: ~2 seconds
Operands: 1 register
Semantics: Push value onto stack (stack grows upward)
Stack pointer: Implicit (SP starts at 256, grows to 511)
Example: PUSH A                 (save A on stack)
```

**POP - Pop Stack into Register**
```
Syntax: POP reg                 (SP--; reg ← stack[SP])
Opcode: 0x13
Timing: ~2 seconds
Operands: 1 register
Semantics: Pop value from stack into register
Stack pointer: Implicit (SP decrements)
Example: POP B                  (restore B from stack)
```

### 2.5 I/O Instructions (3 instructions)

**RDCRD - Read Card**
```
Syntax: RDCRD reg               (reg ← read input card)
Opcode: 0x14
Timing: Variable (user input)
Operands: 1 register
Semantics: Read 50-digit number from input punch card into register
Historical: Babbage's input device was punch cards
Modern: Reads from stdin or input buffer
Example: RDCRD A
```

**WRPCH - Write Punch Card**
```
Syntax: WRPCH reg               (write reg to output card)
Opcode: 0x15
Timing: Variable (mechanical printing)
Operands: 1 register
Semantics: Write 50-digit number from register to output punch card
Historical: Output was generated as punch cards
Modern: Writes to stdout or output buffer
Example: WRPCH A
```

**WRPRN - Write Print (Display)**
```
Syntax: WRPRN reg               (print reg)
Opcode: 0x16
Timing: Variable (mechanical printing)
Operands: 1 register
Semantics: Display 50-digit number from register (human-readable)
Output format: Decimal number with sign
Example: WRPRN A                (print A in decimal)
```

### 2.6 Bit Shift Instructions (2 instructions)

**SHL - Shift Left**
```
Syntax: SHL reg, immed          (reg ← reg << immed)
Opcode: 0x1A
Timing: ~1-2 seconds
Operands: Register, Immediate (0-49)
Semantics: Left shift register by immediate positions
Equivalent to: Multiply by 10^immed
Example: SHL A, 3               (A ← A × 1000)
```

**SHR - Shift Right**
```
Syntax: SHR reg, immed          (reg ← reg >> immed)
Opcode: 0x1B
Timing: ~1-2 seconds
Operands: Register, Immediate (0-49)
Semantics: Right shift register by immediate positions
Equivalent to: Divide by 10^immed (integer division)
Example: SHR A, 2               (A ← A / 100)
```

### 2.7 Special Instructions (1 instruction)

**NOP - No Operation**
```
Syntax: NOP                     (do nothing)
Opcode: 0x00
Timing: 0 seconds (truly does nothing)
Operands: None
Semantics: No operation; used for padding or alignment
Example: NOP
```

---

## PART 3: INTERMEDIATE REPRESENTATION (IR)

### 3.1 IR Type System

All types flatten to 50-digit decimal at runtime. IR preserves type information for analysis and optimization.

**Scalar Types**:
- `i64`: 64-bit signed integer (-2^63 to 2^63-1)
  - Stored as 50-digit decimal
  - Safe range: ±10^18 (well within 50 digits)

- `f64`: 64-bit floating-point (IEEE 754)
  - Converted to 50-digit fixed-point
  - Decimal representation: mantissa + exponent

- `dec50`: 50-digit signed decimal (native Babbage type)
  - Exact representation, no rounding
  - Used for financial calculations, precise arithmetic

- `ptr`: Pointer (32-bit address)
  - References memory locations [0-2047]
  - Can be dereferenced to load/store values

- `void`: No value (used for functions that don't return)

### 3.2 IR Value Hierarchy

**Constant**:
- Literal values embedded in code
- Examples: `Constant(42)`, `Constant(-17)`, `Constant(3.14)`
- Compiled to: LOAD reg, immediate

**RegisterValue**:
- Reference to physical register (A, B, C, D)
- Examples: `RegisterValue('A')`, `RegisterValue('B')`
- Already in hardware register

**MemoryValue**:
- Reference to memory location
- Examples: `MemoryValue(256)`, `MemoryValue(512)`
- Must LOAD before use, STOR after modification

**VariableValue**:
- Named variable (resolved to address by code generator)
- Examples: `VariableValue('x')`, `VariableValue('sum')`
- Allocated to [0-255] globals or [256-511] stack

**UndefValue**:
- Undefined value (error case)
- Used when value not yet computed

### 3.3 IR Instruction Types

**Assignment**:
```
target = source
Semantics: Copy source to target
IR: Assignment(target, source)
Example: x = 42
```

**BinaryOp**:
```
target = left op right
Semantics: Compute operation on two values
IR: BinaryOp('+', target, left, right)
Operations: '+', '-', '*', '/', '%', 'sqrt', 'abs'
Example: sum = a + b
```

**Load**:
```
target = memory[address]
Semantics: Load from memory
IR: Load(target, address)
Example: x = array[10]
```

**Store**:
```
memory[address] = value
Semantics: Store to memory
IR: Store(address, value)
Example: array[10] = x
```

**LoadEffectiveAddress**:
```
target = &variable
Semantics: Get address of variable
IR: LoadEffectiveAddress(target, variable)
Example: ptr = &array[0]
```

**Jump**:
```
goto label
Semantics: Unconditional branch
IR: Jump(label)
Terminator: Ends basic block
Example: goto loop_end
```

**ConditionalBranch**:
```
if condition goto true_label else goto false_label
Semantics: Conditional branch
IR: ConditionalBranch(condition, true_label, false_label)
Terminator: Ends basic block
Example: if x > 0 goto positive else goto negative
```

**Call**:
```
target = call function(arg1, arg2, ...)
Semantics: Function call
IR: Call(target, function, [arg1, arg2, ...])
Calling convention: Arguments in stack, return value in A
Example: result = call factorial(5)
```

**Return**:
```
return value
Semantics: Return from function
IR: Return(value)
Terminator: Ends basic block and function
Convention: Return value in register A
Example: return sum
```

### 3.4 IR Structure

**BasicBlock**:
- Sequence of instructions with single entry and exit
- Terminates with Jump, ConditionalBranch, Call, or Return
- No branches in middle (branches only at end)

**Function**:
- Collection of basic blocks
- Entry block: execution starts here
- Parameters: function arguments
- Local variables: allocate on stack

**Program**:
- Collection of functions
- Global variables: memory [0-255]
- Entry point: 'main' function

### 3.5 IRBuilder API

```python
# Create function
builder = IRBuilder('factorial', parameters=['n'])

# Create block
block = builder.new_block('entry')

# Emit instructions
builder.emit_assignment('a', Constant(1))
builder.emit_assignment('b', VariableValue('n'))

# Emit binary operation
builder.emit_binary_op('*', 'result', VariableValue('a'), VariableValue('b'))

# Emit return
builder.emit_return(VariableValue('result'))

# Finalize function
func = builder.finalize()
```

---

## PART 4: CODE GENERATION PIPELINE

### 4.1 Pipeline Overview

Complete transformation from IR to machine code:

```
IR (language-agnostic)
    ↓ Phase 1: Liveness Analysis
Live intervals (which values simultaneously alive)
    ↓ Phase 2: Register Allocation
Register assignments (map values to A, B, C, D or memory)
    ↓ Phase 3: Instruction Selection
Babbage ISA operations (IR ops → Babbage mnemonics)
    ↓ Phase 4: Code Emission
Assembly text (human-readable)
    ↓ Assembler
Machine code (50-bit words)
    ↓
Babbage Execution
```

### 4.2 Phase 1: Liveness Analysis

**Purpose**: Determine which values are simultaneously needed in registers

**Algorithm**:
1. Compute live range for each value (first definition to last use)
2. Build conflict graph (values with overlapping live ranges)
3. Track peak simultaneous liveness

**Output**: LiveIntervals
```python
for_name='n': LiveInterval(
    name='n',
    start=0,      # instruction 0
    end=10,       # instruction 10
    uses={0, 5, 10},
    definitions={0}
)
```

**Critical Insight**: Peak simultaneous liveness drives spilling
- If 5 values live at same instruction, but only 4 registers exist
- Must spill 1 value to memory (address 256-511)

**Output Example**:
```
n: [0-10] (5 uses)
b: [3-15] (3 uses)
c: [7-20] (4 uses)
Max simultaneous: 3
```

### 4.3 Phase 2: Register Allocation

**Purpose**: Assign IR values to physical registers (A, B, C, D) or memory

**Algorithm**: Linear scan register allocation
```
for each live interval (sorted by start):
    1. Expire intervals where end < current start
       (remove completed values from registers)
    2. Find free register
       if available: allocate to register
       else: spill interval with furthest next use to memory
```

**Spill Strategy**: Furthest next use
- Compute next use distance for each allocated value
- Spill value with largest next use distance
- Rationale: Minimize memory accesses for frequently used values

**Output**: AllocationMap
```python
allocations = {
    'n': 'A',           # parameter n in register A
    'b': 'B',           # variable b in register B
    'c': 'C',           # variable c in register C
    'd': memory[256]    # variable d spilled to memory
}
```

**Register Pressure Metric**:
- Track percentage of time all 4 registers full
- Example: 80% pressure means 4 registers full 80% of execution
- Guides optimization decisions

### 4.4 Phase 3: Instruction Selection

**Purpose**: Map IR operations to Babbage ISA mnemonics

**Key Challenge**: Babbage stores results in first operand
- IR: `target = left + right`
- Babbage: `ADD A, B` means `A ← A + B`
- Solution: Generate MOV instructions to establish register convention

**IR to Babbage Mapping**:

```
IR: target = left + right

Solution:
  1. MOV A, left       (load left into accumulator)
  2. MOV B, right      (load right into secondary)
  3. ADD A, B          (add: A ← A + B)
  4. MOV target, A     (store result)
```

**All Operations Mapped**:

```
Arithmetic:
  ADD: IR op '+' → Babbage ADD (8 seconds)
  SUB: IR op '-' → Babbage SUB (8 seconds)
  MULT: IR op '*' → Babbage MULT (400 seconds)
  DIV: IR op '/' → Babbage DIV (750 seconds)
  SQRT: IR op 'sqrt' → Babbage SQRT (250 seconds)
  ABS: IR op 'abs' → Babbage ABS
  NEG: IR op 'neg' → Babbage NEG

Memory:
  Load: IR Load → LOAD reg, addr
  Store: IR Store → STOR reg, addr
  Assignment: IR Assignment → MOV reg, reg or LOAD reg, immed

Control Flow:
  Jump: IR Jump → JMP label
  ConditionalBranch: IR ConditionalBranch → CMP + conditional JMP
  Call: IR Call → CALL label
  Return: IR Return → (result in A) RET
```

### 4.5 Phase 4: Code Emission

**Purpose**: Generate assembly text with proper label resolution

**Output Format**:
```asm
.global main
.text

main:
  MOV A, 10          # a = 10
  MOV B, 5           # b = 5
  ADD A, B           # a = a + b
  WRPRN A            # print a
  RET                # return
```

**Label Resolution**:
- Map labels to instruction addresses
- Example: `main → 0`, `loop_start → 5`, `loop_end → 20`
- Enables jumps to correct instruction positions

---

## PART 5: BABBAGE ASSEMBLER

### 5.1 Two-Pass Algorithm

**Pass 1: Symbol Resolution**
- Scan all instructions
- Record label → address mappings
- Build symbol table
- Detect duplicate symbols

**Pass 2: Code Emission**
- Process each instruction
- Resolve operands (registers, labels, immediates)
- Encode to 50-bit machine word
- Output machine code array

### 5.2 50-Bit Instruction Encoding

**Format**:
```
Bits 49-42:   Opcode (8 bits)
Bits 41-40:   Register 1 (2 bits)
Bits 39-38:   Register 2 or flag (2 bits)
Bits 37-0:    Address/Immediate (38 bits)
```

**Register Encoding**:
```
A: 0  (binary 00)
B: 1  (binary 01)
C: 2  (binary 10)
D: 3  (binary 11)
```

**Encoding Examples**:

```
LOAD A, 42
  Opcode: 0x06 (LOAD)
  Register 1: A (0)
  Register 2: unused (0)
  Immediate: 42
  Result: (0x06 << 42) | (0 << 40) | 42
         = 0x18000000002a

ADD A, B
  Opcode: 0x01 (ADD)
  Register 1: A (0)
  Register 2: B (1)
  Immediate: 0 (unused)
  Result: (0x01 << 42) | (0 << 40) | (1 << 38)
         = 0x04000000000

RET
  Opcode: 0x11 (RET)
  Registers: unused
  Immediate: unused
  Result: (0x11 << 42)
         = 0x440000000000
```

### 5.3 Complete Instruction Set Encoding

All 32 instructions with encoding details:

| Instruction | Opcode | Format | Encoding |
|------------|--------|--------|----------|
| NOP | 0x00 | - | (0x00 << 42) |
| ADD | 0x01 | reg, reg | (0x01 << 42) \| (r1 << 40) \| (r2 << 38) |
| SUB | 0x02 | reg, reg | (0x02 << 42) \| (r1 << 40) \| (r2 << 38) |
| MULT | 0x03 | reg, reg | (0x03 << 42) \| (r1 << 40) \| (r2 << 38) |
| DIV | 0x04 | reg, reg | (0x04 << 42) \| (r1 << 40) \| (r2 << 38) |
| SQRT | 0x05 | reg | (0x05 << 42) \| (r1 << 40) |
| LOAD | 0x06 | reg, addr | (0x06 << 42) \| (r1 << 40) \| addr |
| STOR | 0x07 | reg, addr | (0x07 << 42) \| (r1 << 40) \| addr |
| JMP | 0x08 | addr | (0x08 << 42) \| addr |
| JZ | 0x09 | reg, addr | (0x09 << 42) \| (r1 << 40) \| addr |
| JNZ | 0x0A | reg, addr | (0x0A << 42) \| (r1 << 40) \| addr |
| JLT | 0x0B | reg, addr | (0x0B << 42) \| (r1 << 40) \| addr |
| JGT | 0x0C | reg, addr | (0x0C << 42) \| (r1 << 40) \| addr |
| JLE | 0x0D | reg, addr | (0x0D << 42) \| (r1 << 40) \| addr |
| JGE | 0x0E | reg, addr | (0x0E << 42) \| (r1 << 40) \| addr |
| CMP | 0x0F | reg, reg | (0x0F << 42) \| (r1 << 40) \| (r2 << 38) |
| CALL | 0x10 | addr | (0x10 << 42) \| addr |
| RET | 0x11 | - | (0x11 << 42) |
| PUSH | 0x12 | reg | (0x12 << 42) \| (r1 << 40) |
| POP | 0x13 | reg | (0x13 << 42) \| (r1 << 40) |
| RDCRD | 0x14 | reg | (0x14 << 42) \| (r1 << 40) |
| WRPCH | 0x15 | reg | (0x15 << 42) \| (r1 << 40) |
| WRPRN | 0x16 | reg | (0x16 << 42) \| (r1 << 40) |
| MOV | 0x17 | reg, reg | (0x17 << 42) \| (r1 << 40) \| (r2 << 38) |
| NEG | 0x18 | reg | (0x18 << 42) \| (r1 << 40) |
| ABS | 0x19 | reg | (0x19 << 42) \| (r1 << 40) |
| SHL | 0x1A | reg, immed | (0x1A << 42) \| (r1 << 40) \| immed |
| SHR | 0x1B | reg, immed | (0x1B << 42) \| (r1 << 40) \| immed |

---

## PART 6: CALLING CONVENTION & MEMORY LAYOUT

### 6.1 Register Convention

**Purpose**: Consistent function interface across all compilers

**Registers**:
- **A (Accumulator)**: Return value, primary computation
- **B (Secondary)**: Temporary, second argument
- **C (Counter)**: Loop counter, third argument
- **D (Destination)**: Destination, fourth argument

**Function Call Order**:
1. Caller loads arguments into B, C, D (or stack)
2. Caller CALL function (pushes return address)
3. Function executes with arguments available
4. Function returns value in A
5. Caller continues with result in A

### 6.2 Memory Layout

```
[0-255]         Global variables and constants
                - Statically allocated
                - Initialized at program start
                - Shared across functions

[256]           Stack pointer base (grows toward 511)
[256-511]       Stack frame (locals, return address, spills)
                - Per function invocation
                - Grows upward as functions nest
                - Maximum 256 words per frame

[512-2047]      Heap (program data)
                - Dynamically allocated
                - Arrays, records, complex data structures
                - Grows upward as program runs
```

### 6.3 Function Stack Frame

For function `foo(x, y, z)` with locals `a, b`:

```
Before CALL foo:
  [256] A=1, B=x, C=y, D=z (arguments)
  [257-258] (free)

After CALL foo:
  [256] return_address (pushed by CALL)
  [257] local_a
  [258] local_b
  [259] saved_A (if needed)
  [260] saved_B (if needed)
  ...
  [511] (stack limit)

After RET:
  [256] back to caller's state
  A = return value
```

---

## PART 7: BEST PRACTICES FOR LANGUAGE COMPILERS

### 7.1 IR Generation (C → IR, Python → IR, etc.)

**Step 1: Parse Source to AST**
```python
# For C: use pycparser or similar
ast = parse_c_code(source_code)

# For Python: use ast module
ast = ast.parse(source_code)
```

**Step 2: Build Symbol Table**
```python
symbols = {
    'x': VariableValue('x', address=0),      # global
    'foo': FunctionValue('foo', address=512) # function
}
```

**Step 3: Generate IR from AST**
```python
def generate_ir(node, builder):
    if isinstance(node, BinaryOpNode):
        left = generate_ir(node.left, builder)
        right = generate_ir(node.right, builder)
        target = builder.new_temp()
        builder.emit_binary_op(node.op, target, left, right)
        return target
    elif isinstance(node, VarNode):
        return VariableValue(node.name)
    # ... handle all node types
```

**Step 4: Type Lowering**
```python
# Map language types to IR types
int x;      → VariableValue('x', ir_type='i64')
float y;    → VariableValue('y', ir_type='f64')
double z;   → VariableValue('z', ir_type='f64')

# All become 50-digit decimal at IR level
# Type information preserved for analysis
```

**Step 5: Generate Intermediate Functions**
```python
# Create IR function
ir_func = builder.finalize()

# Validate IR
assert all(isinstance(i, Instruction) for i in ir_func.instructions)

# Prepare for code generation
return ir_func
```

### 7.2 Integration with Code Generator

**Seamless Integration**:
```python
from backend.src.codegen import CodeGenerator

# Compile function to Babbage assembly
generator = CodeGenerator()
ir_function = compile_c_to_ir(c_code)
result = generator.generate_function(ir_function)

# Get assembly
assembly = result.get_assembly_text()

# Assemble to machine code
assembler = Assembler(assembly)
asm_result = assembler.assemble()

# Get machine code
machine_code = asm_result.machine_code
symbol_table = asm_result.symbol_table
```

### 7.3 Error Classification

**Compile-Time Errors**:
- Syntax errors (malformed code)
- Type errors (incompatible operations)
- Undefined symbols (variable not declared)
- Security violations (forbidden patterns)

**Runtime Errors**:
- Division by zero
- Stack overflow (too much recursion)
- Memory access (address out of range)
- Undefined behavior (used uninitialized variable)

**Timeout Errors**:
- Infinite loops
- Excessive computation (MULT/DIV chain)
- Resource exhaustion

### 7.4 Testing Each Compiler

**Test Matrix**:
```
Basic Functionality:
  ✓ Hello world (simple output)
  ✓ Arithmetic (add, subtract, multiply, divide)
  ✓ Variables (assign, read, modify)
  ✓ Conditionals (if/else, comparisons)
  ✓ Loops (while, for)
  ✓ Functions (define, call, return)
  ✓ Arrays (allocate, access, modify)
  ✓ Records/Structs (field access)

Type System:
  ✓ Integer arithmetic (no overflow)
  ✓ Floating-point conversion
  ✓ Type checking (catch type errors)
  ✓ Implicit conversions (if allowed)

Edge Cases:
  ✓ Deeply nested expressions
  ✓ Recursive functions
  ✓ Large numbers (up to 10^50)
  ✓ Negative numbers
  ✓ Zero division (error handling)
  ✓ Empty programs

Performance:
  ✓ Typical program < 5 seconds
  ✓ Code generation < 1 second
  ✓ Assembly < 100ms
```

---

## PART 8: COMPREHENSIVE EXAMPLES

### 8.1 Example 1: Factorial (Recursive)

**C Code**:
```c
int factorial(int n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}

int main() {
    printf("%d\n", factorial(5));
    return 0;
}
```

**IR (intermediate)**:
```
function factorial(n):
  cmp_result = CMP(n, 1)
  if_branch: if cmp_result <= 0 goto base_case else goto recursive_case

  base_case:
    return 1

  recursive_case:
    n_minus_1 = SUB(n, 1)
    recursive_result = CALL(factorial, n_minus_1)
    result = MULT(n, recursive_result)
    return result

function main():
  result = CALL(factorial, 5)
  WRPRN(result)
  return 0
```

**Assembly**:
```asm
.global factorial
.text

factorial:
  LOAD A, 1            # A = 1 (for comparison)
  CMP B, A             # B (parameter n) vs 1
  JLE base_case        # if n <= 1, goto base_case

recursive_case:
  LOAD A, 1
  SUB B, A             # B = B - 1 (n_minus_1 in B)
  MOV A, B             # move n_minus_1 to A (argument)
  CALL factorial       # recursive call
  MOV B, A             # B = result from A
  MOV A, C             # A = n (saved in C before recursion)
  MULT A, B            # A = A * B
  RET

base_case:
  LOAD A, 1            # A = 1 (return value)
  RET

main:
  LOAD A, 5            # load 5 as argument
  CALL factorial       # call factorial(5)
  WRPRN A              # print result (in A)
  LOAD A, 0            # load 0 for success
  RET
```

**Machine Code** (hex dump):
```
Address  Instruction
0        LOAD A, 1       → 0x18000000001
1        CMP B, A        → 0x3c000000000
2        JLE base_case   → 0x34000000006
3        LOAD A, 1       → 0x18000000001
4        SUB B, A        → 0x84000000000
5        MOV A, B        → 0x5c000000000
6        CALL factorial  → 0x40000000000
7        MOV B, A        → 0x5c400000000
...
```

### 8.2 Example 2: Fibonacci (Iterative)

**C Code**:
```c
int fibonacci(int n) {
    int a = 0, b = 1;
    for (int i = 0; i < n; i++) {
        int temp = a + b;
        a = b;
        b = temp;
    }
    return a;
}
```

**IR**:
```
function fibonacci(n):
  a = 0
  b = 1
  i = 0

loop_condition:
  cmp_result = CMP(i, n)
  if cmp_result >= 0 goto loop_end

loop_body:
  temp = ADD(a, b)
  a = b
  b = temp
  i = ADD(i, 1)
  goto loop_condition

loop_end:
  return a
```

**Assembly**:
```asm
fibonacci:
  LOAD A, 0            # a = 0
  STOR A, 256          # store a at memory[256]
  LOAD A, 1            # b = 1
  STOR A, 257          # store b at memory[257]
  LOAD A, 0            # i = 0
  STOR A, 258          # store i at memory[258]

loop_condition:
  LOAD A, 258          # load i
  CMP A, B             # compare i with n
  JGE loop_end         # if i >= n, exit loop

loop_body:
  LOAD A, 256          # load a
  LOAD C, 257          # load b
  ADD A, C             # temp = a + b
  STOR A, 259          # store temp
  LOAD A, 257          # load b
  STOR A, 256          # a = b
  LOAD A, 259          # load temp
  STOR A, 257          # b = temp
  LOAD A, 258          # load i
  LOAD C, 1            # load 1
  ADD A, C             # i = i + 1
  STOR A, 258          # store i
  JMP loop_condition   # continue loop

loop_end:
  LOAD A, 256          # load a (return value)
  RET
```

---

## PART 9: IMPLEMENTATION ROADMAP

### 9.1 Language Compilers (Week 8)

**C Compiler** (40 hours)
- Parser: pycparser or hand-rolled
- Symbol table: variable scopes, function prototypes
- IR generation: statement → IR instruction sequence
- Type system: int, float, pointers
- Challenges: pointer arithmetic, struct layout, array indexing

**Python Compiler** (60 hours)
- Parser: ast.parse() or custom parser
- Symbol table: name resolution, scope chains
- IR generation: expression → IR value sequence
- Type system: infer types from runtime behavior
- Challenges: dynamic types, generator expressions, comprehensions

**Haskell Compiler** (50 hours)
- Parser: pattern matching syntax
- Symbol table: function definitions, type signatures
- IR generation: lazy evaluation → strict evaluation
- Type system: polymorphic types, type classes
- Challenges: higher-order functions, pattern matching, lazy evaluation

### 9.2 Advanced Compilers (Weeks 8-9)

**IDRIS2 Compiler** (35 hours)
- Dependent type checking
- Proof tactics compilation
- Challenges: dependent patterns, universe levels

**LISP Compiler** (30 hours)
- S-expression parsing
- Macro expansion
- Challenges: dynamic typing, closure capture

**Java Compiler** (30 hours)
- JVM bytecode interpretation → Babbage
- Object-oriented features
- Challenges: inheritance, polymorphism, exceptions

**System F Compiler** (65 hours)
- Polymorphic lambda calculus
- Type inference (Hindley-Milner)
- Challenges: universal quantification, rank-N types, higher-order functions

---

## PART 10: QUICK REFERENCE TABLES

### Opcode Summary
```
0x00: NOP     0x01: ADD     0x02: SUB     0x03: MULT
0x04: DIV     0x05: SQRT    0x06: LOAD    0x07: STOR
0x08: JMP     0x09: JZ      0x0A: JNZ     0x0B: JLT
0x0C: JGT     0x0D: JLE     0x0E: JGE     0x0F: CMP
0x10: CALL    0x11: RET     0x12: PUSH    0x13: POP
0x14: RDCRD   0x15: WRPCH   0x16: WRPRN   0x17: MOV
0x18: NEG     0x19: ABS     0x1A: SHL     0x1B: SHR
```

### Register Codes
```
A: 0 (binary 00)
B: 1 (binary 01)
C: 2 (binary 10)
D: 3 (binary 11)
```

### Timing (in seconds, mechanical)
```
NOP: 0       ADD/SUB: 8       MULT: 400     DIV: 750
SQRT: 250    LOAD/STOR: 2     Jumps: 1      CALL/RET: 2
MOV: 1       NEG/ABS: 1       Shifts: 2     I/O: variable
```

### Memory Regions
```
[0-255]:      Global variables (256 words)
[256-511]:    Stack (256 words, grows toward 511)
[512-2047]:   Heap (1536 words)
Total:        2048 words of memory
```

---

## CONCLUSION

This Babbage Master Reference provides complete specification for:

1. **Architecture**: 4 registers, 2000-word memory, 32 instructions
2. **Type System**: 50-digit decimal with type preservation in IR
3. **Code Generation**: 4-phase pipeline (liveness → allocation → selection → emission)
4. **Assembly**: 2-pass algorithm with 50-bit instruction encoding
5. **Calling Convention**: Register-based with stack frames
6. **Best Practices**: For building language compilers targeting Babbage

Use this reference when:
- Implementing new language compilers
- Debugging compiled code
- Optimizing performance
- Understanding IR generation
- Validating machine code output

---

**Document Version**: 1.0
**Last Updated**: 2025-10-31
**Status**: COMPLETE AND COMPREHENSIVE
**Scope**: All Babbage components consolidated and harmonized
