# Babbage Assembler Specification

**Date**: 2025-10-31  
**Phase**: Week 7 Foundation (Critical Path)  
**Purpose**: Convert Babbage assembly language to machine code  
**Scope**: Assembly syntax, parser, assembler algorithm, symbol resolution

---

## Executive Summary

The Babbage Assembler converts human-readable Babbage assembly language to executable 50-bit machine code. It serves two purposes:

1. **Development**: Programmers write assembly directly (without IR)
2. **Code Generation**: Babbage Code Generator outputs assembly (as intermediate step)

**Design**: Single-pass assembler with forward reference handling.

**Syntax**: Inspired by x86 assembly but adapted for Babbage's 4-register, 50-digit decimal architecture.

---

## Part 1: Babbage Assembly Language Syntax

### 1.1 Program Structure

```
[Labels and directives]
[Instruction mnemonics]

Example:
  .global main
  .data
  x: 42
  
  .text
  main:
    LOAD A, x      -- Load variable x
    ADD A, 5       -- Add 5
    WRPRN A        -- Print result
    RET
```

### 1.2 Instruction Syntax

**General format**:
```
[label:] mnemonic [operand1] [operand2] [# comment]
```

**Examples**:
```
ADD A, B           -- Add B to A
LOAD A, 100        -- Load from memory address 100
JMP loop_start     -- Jump to label
MOV A, B           -- Move B to A
```

### 1.3 Operand Types

```
Operand ::=
  | Register        -- A, B, C, D
  | Decimal Literal -- 42, -17, 3.14159...
  | Label           -- loop_start, func_end
  | Variable        -- x, y, sum
  | Address         -- 256, 512, etc.
```

**Examples**:
```
MOV A, B           -- Register to register
ADD A, 42          -- Immediate constant
LOAD A, x          -- Load from variable
JMP loop_start     -- Jump to label
```

### 1.4 Labels

Labels mark positions for jumps and data:

```
label_name:
  instruction1
  instruction2
  ...
```

**Naming**: Alphanumeric + underscore, not starting with digit.

**Examples**:
```
loop_start:
  ADD A, 1
  JMP loop_start

func_return:
  RET
```

### 1.5 Directives

Directives are assembler commands (not Babbage instructions):

```
.global name       -- Export symbol as global
.data              -- Enter data section
.text              -- Enter code section
.align N           -- Align to N-byte boundary
.space N           -- Reserve N bytes
```

**Examples**:
```
.global main
.data
  array: 100 0 0 0 0    -- 5 words initialized to 0
.text
  main:
    LOAD A, array
    ...
```

---

## Part 2: Babbage Assembly Instruction Set

All 32 Babbage instructions with assembly syntax:

### 2.1 Arithmetic Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
ADD      | ADD a, b        | a ← a + b
SUB      | SUB a, b        | a ← a - b
MULT     | MULT a, b       | a ← a × b
DIV      | DIV a, b        | a ← a ÷ b
SQRT     | SQRT a          | a ← √a
ABS      | ABS a           | a ← |a|
NEG      | NEG a           | a ← -a
```

### 2.2 Memory Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
LOAD     | LOAD a, addr    | a ← memory[addr]
STOR     | STOR a, addr    | memory[addr] ← a
MOV      | MOV a, b        | a ← b
```

### 2.3 Control Flow Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
JMP      | JMP label       | PC ← label
JZ       | JZ a, label     | if a == 0 then PC ← label
JNZ      | JNZ a, label    | if a != 0 then PC ← label
JLT      | JLT a, label    | if a < 0 then PC ← label
JGT      | JGT a, label    | if a > 0 then PC ← label
JLE      | JLE a, label    | if a <= 0 then PC ← label
JGE      | JGE a, label    | if a >= 0 then PC ← label
CMP      | CMP a, b        | a ← a - b (for comparison)
```

### 2.4 Function Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
CALL     | CALL label      | push return, PC ← label
RET      | RET             | pop return, PC ← return_addr
PUSH     | PUSH a          | push a onto stack
POP      | POP a           | pop stack into a
```

### 2.5 I/O Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
RDCRD    | RDCRD a         | a ← read punch card
WRPCH    | WRPCH a         | write a to punch card
WRPRN    | WRPRN a         | print a
```

### 2.6 Special Instructions

```
Mnemonic | Syntax          | Semantics
---------|-----------------|---------------------------
NOP      | NOP             | no operation
SHL      | SHL a, immed    | a ← a << immed
SHR      | SHR a, immed    | a ← a >> immed
```

---

## Part 3: Assembly Examples

### 3.1 Example 1: Simple Program

**Assembly**:
```
.global main
.text

main:
  LOAD A, 10       -- Load 10
  LOAD B, 5        -- Load 5
  ADD A, B         -- Add: A = A + B = 15
  WRPRN A          -- Print 15
  RET              -- Return
```

**Assembled**:
```
Address   Machine Code           Mnemonic
0         0x06 0 0 0 10        LOAD A, 10
1         0x06 1 0 0 5         LOAD B, 5
2         0x01 0 1 0 0         ADD A, B
3         0x16 0 0 0 0         WRPRN A
4         0x11 0 0 0 0         RET
```

### 3.2 Example 2: Loop

**Assembly**:
```
.global sum
.text

sum:
  LOAD A, 0        -- sum = 0
  LOAD B, 1        -- i = 1
loop_start:
  CMP B, 10        -- Compare i with 10
  JGT loop_end     -- If i > 10, exit
  ADD A, B         -- sum += i
  ADD B, 1         -- i++
  JMP loop_start   -- Loop back
loop_end:
  RET              -- Return
```

### 3.3 Example 3: Function Call

**Assembly**:
```
.global main
.text

main:
  LOAD A, 5        -- Argument
  CALL add_five    -- Call add_five(5)
  WRPRN A          -- Print result
  RET

add_five:
  ADD A, 5         -- A = A + 5
  RET
```

---

## Part 4: Assembler Algorithm

### 4.1 Two-Pass Assembly

**Pass 1: Symbol Resolution**
1. Scan all labels and symbols
2. Assign addresses to each
3. Build symbol table

**Pass 2: Code Emission**
1. Process each instruction
2. Resolve references using symbol table
3. Emit machine code

### 4.2 Symbol Table

Tracks label addresses and variable locations:

```
Symbol Table:
  main:        address 0
  loop_start:  address 3
  loop_end:    address 8
  x:           address 256 (variable)
  y:           address 257 (variable)
```

### 4.3 Pass 1 Algorithm

```
function pass1(assembly_lines):
  symbol_table = {}
  address = 0
  
  for each line in assembly_lines:
    if line is empty or comment:
      continue
    
    if line is directive:
      process_directive(line)
      continue
    
    if line contains label:
      label = extract_label(line)
      symbol_table[label] = address
    
    if line contains instruction:
      address += 1  -- Each instruction is 1 word
  
  return symbol_table
```

### 4.4 Pass 2 Algorithm

```
function pass2(assembly_lines, symbol_table):
  machine_code = []
  address = 0
  
  for each line in assembly_lines:
    if line is empty or comment:
      continue
    
    if line contains instruction:
      instruction = parse_instruction(line)
      -- Resolve operands using symbol_table
      operand1 = resolve_operand(instruction.op1, symbol_table)
      operand2 = resolve_operand(instruction.op2, symbol_table)
      
      -- Encode instruction
      machine_word = encode(instruction.mnemonic, operand1, operand2)
      machine_code[address] = machine_word
      address += 1
  
  return machine_code
```

### 4.5 Pseudocode: Assembler

```python
class Assembler:
    def __init__(self, assembly_text: str):
        self.lines = assembly_text.strip().split('\n')
        self.symbol_table = {}
        self.machine_code = []
    
    def assemble(self) -> List[int]:
        """Two-pass assembly"""
        self._pass1_resolve_symbols()
        self._pass2_emit_code()
        return self.machine_code
    
    def _pass1_resolve_symbols(self):
        """First pass: collect labels and variables"""
        address = 0
        for line in self.lines:
            line = self._strip_comment(line).strip()
            if not line:
                continue
            
            # Check for label
            if ':' in line:
                label, rest = line.split(':', 1)
                label = label.strip()
                self.symbol_table[label] = address
                line = rest.strip()
            
            # Check for instruction
            if line and not line.startswith('.'):
                address += 1
    
    def _pass2_emit_code(self):
        """Second pass: emit machine code"""
        for line in self.lines:
            line = self._strip_comment(line).strip()
            if not line or line.startswith('.'):
                continue
            
            # Remove label if present
            if ':' in line:
                _, line = line.split(':', 1)
                line = line.strip()
            
            if line:
                machine_word = self._assemble_instruction(line)
                self.machine_code.append(machine_word)
    
    def _assemble_instruction(self, instr_line: str) -> int:
        """Assemble single instruction to machine code"""
        parts = instr_line.split()
        mnemonic = parts[0]
        operands = parts[1:] if len(parts) > 1 else []
        
        # Resolve operands
        resolved = [self._resolve_operand(op) for op in operands]
        
        # Encode instruction
        return self._encode(mnemonic, resolved)
    
    def _resolve_operand(self, operand: str) -> int:
        """Resolve operand to numeric value"""
        operand = operand.rstrip(',')
        
        # Register
        if operand.upper() in ['A', 'B', 'C', 'D']:
            return {'A': 0, 'B': 1, 'C': 2, 'D': 3}[operand.upper()]
        
        # Label
        if operand in self.symbol_table:
            return self.symbol_table[operand]
        
        # Decimal literal
        try:
            return int(operand)
        except ValueError:
            pass
        
        # Variable (address)
        if operand in self.symbol_table:
            return self.symbol_table[operand]
        
        raise AssemblyError(f"Undefined operand: {operand}")
    
    def _encode(self, mnemonic: str, operands: List[int]) -> int:
        """Encode instruction to 50-bit machine word"""
        opcodes = {
            'NOP': 0x00, 'ADD': 0x01, 'SUB': 0x02, 'MULT': 0x03,
            'DIV': 0x04, 'SQRT': 0x05, 'LOAD': 0x06, 'STOR': 0x07,
            'JMP': 0x08, 'JZ': 0x09, 'JNZ': 0x0A, 'JLT': 0x0B,
            'JGT': 0x0C, 'JLE': 0x0D, 'JGE': 0x0E, 'CMP': 0x0F,
            'CALL': 0x10, 'RET': 0x11, 'PUSH': 0x12, 'POP': 0x13,
            'RDCRD': 0x14, 'WRPCH': 0x15, 'WRPRN': 0x16, 'MOV': 0x17,
            'NEG': 0x18, 'ABS': 0x19, 'SHL': 0x1A, 'SHR': 0x1B,
        }
        
        if mnemonic not in opcodes:
            raise AssemblyError(f"Unknown mnemonic: {mnemonic}")
        
        opcode = opcodes[mnemonic]
        
        # Encode based on instruction type
        if len(operands) == 0:
            return opcode << 42
        elif len(operands) == 1:
            reg = operands[0]
            return (opcode << 42) | (reg << 40)
        elif len(operands) == 2:
            reg1, reg2_or_addr = operands[0], operands[1]
            if reg2_or_addr <= 15:  # Register
                return (opcode << 42) | (reg1 << 40) | (reg2_or_addr << 38)
            else:  # Address
                return (opcode << 42) | (reg1 << 40) | reg2_or_addr
        
        raise AssemblyError(f"Invalid operand count for {mnemonic}")
    
    def _strip_comment(self, line: str) -> str:
        """Remove comments from line"""
        if '#' in line:
            return line.split('#')[0]
        return line
```

---

## Part 5: Error Detection

### 5.1 Syntax Errors

```
Error: Invalid instruction 'ADDD A, B'
  Line 5: ADDD A, B
  Suggestion: Did you mean 'ADD'?

Error: Too many operands for 'JMP'
  Line 10: JMP label, extra_operand
  Instruction 'JMP' takes 1 operand, got 2

Error: Invalid register 'E'
  Line 7: MOV A, E
  Valid registers: A, B, C, D
```

### 5.2 Semantic Errors

```
Error: Undefined label 'loop_end'
  Line 15: JMP loop_end
  Label not found in symbol table

Error: Duplicate label 'main'
  Line 5: main:
  Line 12: main:
  Labels must be unique

Error: Invalid operand type
  Line 8: ADD A, ABC
  Expected: register, label, or decimal literal
```

### 5.3 Error Recovery

The assembler should:
1. Report first error with context
2. Attempt to continue for more errors
3. Prevent cascading errors
4. Suggest fixes if possible

---

## Part 6: Assembly Output Formats

### 6.1 Binary Output

Raw 50-bit words in machine-readable format:

```
00000000000000001010...  (2000 50-bit words)
...
```

### 6.2 Hex Dump

Human-readable hex format:

```
Address    Hex Code              Mnemonic    Comment
0          0x06000000a         LOAD A, 10
1          0x06010000005       LOAD B, 5
2          0x0100000000        ADD A, B
3          0x16000000000       WRPRN A
4          0x11000000000       RET
```

### 6.3 Symbol Map

Maps labels to addresses:

```
Symbol Table
=============
main:      0
loop_start: 3
loop_end:   8
x:         256
y:         257
```

---

## Part 7: Integration with Code Generator

The Babbage Code Generator outputs assembly, which the Assembler converts to machine code:

```
[Language Code]
    ↓
[Language Compiler → IR]
    ↓
[Code Generator → Assembly]
    ↓
[Assembler → Machine Code]
    ↓
[Babbage Emulator]
```

### 7.1 Assembly Intermediate Format

Code Generator outputs assembly text:

```
.global fibonacci
.text

fibonacci:
  # ... instructions ...
```

Assembler reads and converts to machine code.

### 7.2 API Integration

```python
class Assembler:
    def assemble(self, assembly_text: str) -> BabbageCode:
        """Assemble text to machine code"""
```

---

## Part 8: Testing the Assembler

### Test 1: Simple Instruction

**Input**:
```
LOAD A, 42
```

**Expected Output**:
```
Machine code: 0x06 0 0 0 42
              (LOAD opcode=0x06, reg A=0, addr=42)
```

### Test 2: Label Resolution

**Input**:
```
loop_start:
  ADD A, 1
  JMP loop_start
```

**Expected**:
- Label 'loop_start' resolves to address 0
- JMP at address 1 jumps back to address 0

### Test 3: Forward Reference

**Input**:
```
JMP loop_end
ADD A, 1
loop_end:
  RET
```

**Expected**:
- Forward reference to 'loop_end' resolved in pass 2
- JMP at address 0 jumps to address 2

### Test 4: Error Detection

**Input**:
```
ADDD A, B      -- Typo: ADDD instead of ADD
```

**Expected Error**:
```
Error: Unknown mnemonic 'ADDD'
Suggestion: Did you mean 'ADD'?
```

---

## Part 9: Future Extensions

### 9.1 Macro Support

```
.macro add_five
  ADD A, 5
.endmacro

main:
  LOAD A, 10
  add_five      -- Expands to ADD A, 5
```

### 9.2 Conditional Assembly

```
.ifdef DEBUG
  WRPRN A       -- Only assembled if DEBUG defined
.endif
```

### 9.3 Include Files

```
.include "lib.asm"    -- Include external assembly file
```

---

## Summary

The Babbage Assembler:

1. **Parses** assembly syntax (mnemonics, operands, labels)
2. **Resolves symbols** (two-pass assembly)
3. **Encodes** instructions to 50-bit machine code
4. **Detects errors** (syntax, semantic, undefined symbols)
5. **Produces output** (binary, hex dump, symbol map)

**Key Features**:
- Single-pass (after symbol resolution) code emission
- Support for labels and forward references
- Error detection with suggestions
- Multiple output formats

**Implementation Effort**: ~15 hours

**Status**: Assembler Specification COMPLETE. Ready for implementation.

---

**Next Document**: Babbage Assembly Service (language service wrapper)
