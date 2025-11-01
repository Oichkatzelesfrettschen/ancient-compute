# Week 7: Babbage Foundation - Complete Architecture Summary

**Date**: 2025-10-31  
**Phase**: Week 7 Foundation (Critical Path)  
**Status**: Specifications COMPLETE - Implementation Ready  
**Scope**: Integration of IR, Code Generator, and Assembler

---

## Executive Overview

The Babbage Foundation comprises three integrated components that enable all 8 language compilers to target the Babbage Analytical Engine:

```
┌─────────────────────────────────────────────────────────────────┐
│                    LANGUAGE COMPILER PIPELINE                   │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  [C Code]  [Python] [Haskell] [IDRIS2] [LISP] [Java] [Sys-F]  │
│      ↓         ↓        ↓        ↓        ↓       ↓      ↓     │
│  [AST Parser & Type Check]  (per-language)                     │
│      ↓                                                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  BABBAGE INTERMEDIATE REPRESENTATION (IR)                │  │
│  │  - Language-agnostic, Babbage-aware                      │  │
│  │  - 4 registers (A,B,C,D), 2000-word memory              │  │
│  │  - 50-digit decimal arithmetic                           │  │
│  │  - Liveness analysis, register allocation ready         │  │
│  └──────────────────────────────────────────────────────────┘  │
│      ↓                                                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  BABBAGE CODE GENERATOR                                  │  │
│  │  - Linear scan register allocation                       │  │
│  │  - Instruction selection (IR → Babbage ISA)             │  │
│  │  - Code emission (memory layout, addressing)             │  │
│  │  - Generates human-readable assembly                     │  │
│  └──────────────────────────────────────────────────────────┘  │
│      ↓                                                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  BABBAGE ASSEMBLY LANGUAGE (Text)                        │  │
│  │  - Mnemonics (ADD, SUB, MULT, DIV, LOAD, STOR, etc.)   │  │
│  │  - Labels and jumps                                      │  │
│  │  - Symbol table and directives                           │  │
│  └──────────────────────────────────────────────────────────┘  │
│      ↓                                                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  BABBAGE ASSEMBLER                                       │  │
│  │  - Two-pass assembly (symbol resolution, code emit)     │  │
│  │  - Encodes 50-bit instructions                           │  │
│  │  - Label resolution with forward references              │  │
│  │  - Error detection & recovery                            │  │
│  └──────────────────────────────────────────────────────────┘  │
│      ↓                                                          │
│  ┌──────────────────────────────────────────────────────────┐  │
│  │  BABBAGE MACHINE CODE (Binary)                           │  │
│  │  - 2000 50-bit instruction words                         │  │
│  │  - Ready for Babbage Emulator or Hardware                │  │
│  └──────────────────────────────────────────────────────────┘  │
│      ↓                                                          │
│  [Babbage Emulator / Hardware Execution]                        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

---

## Part 1: Three-Component Architecture

### Component 1: Babbage Intermediate Representation (BIR)

**Purpose**: Language-independent intermediate form that all compilers target.

**Key Features**:
- **IR Instructions**: 13 basic operations (assignment, arithmetic, memory, control flow, I/O, function calls)
- **Memory Model**: Explicit linear memory (0-2047 words), each word holds 50-digit decimal
- **Registers**: 4 explicit registers (A, B, C, D) with compiler responsibility for allocation
- **Type System**: Language-agnostic (all types flatten to 50-digit decimals at IR level)
- **Control Flow**: Direct jumps, branches, function calls with calling convention

**Specification**: `BABBAGE_IR_SPECIFICATION.md` (600+ lines)

**Key Sections**:
- IR structure and components (programs, functions, basic blocks)
- Data types and values
- Assignment, arithmetic, memory, control flow instructions
- Memory layout and variable allocation
- Function call convention
- Code examples (factorial, loop, array, record)

**Status**: Specification COMPLETE ✓

---

### Component 2: Babbage Code Generator

**Purpose**: Transform IR to Babbage assembly (intermediate step before machine code).

**Key Features**:
- **Register Allocation**: Linear scan algorithm with spill/fill
- **Liveness Analysis**: Compute which values simultaneously live
- **Instruction Selection**: Map IR operations to Babbage ISA instructions
- **Memory Address Calculation**: Static assignment for globals, stack-relative for locals
- **Code Emission**: Generate assembly with resolved addresses

**Specification**: `BABBAGE_CODE_GENERATOR_SPECIFICATION.md` (500+ lines)

**Key Sections**:
- Linear scan register allocation algorithm
- Liveness analysis
- Instruction selection mapping (IR → Babbage ISA)
- Memory layout and addressing
- Function call code generation
- Label resolution and forward references
- Code generation examples

**Status**: Specification COMPLETE ✓

**Implementation Pseudocode**: Includes full `CodeGenerator` class with methods for instruction emission, register allocation, and encoding.

---

### Component 3: Babbage Assembler

**Purpose**: Convert human-readable assembly to executable machine code.

**Key Features**:
- **Assembly Syntax**: Mnemonics, operands, labels, directives
- **Two-Pass Assembly**: Pass 1 (symbol resolution), Pass 2 (code emission)
- **32 Instructions**: Full Babbage ISA supported (arithmetic, memory, control, I/O, function)
- **Error Detection**: Undefined labels, invalid mnemonics, syntax errors
- **Multiple Outputs**: Binary, hex dump, symbol map

**Specification**: `BABBAGE_ASSEMBLER_SPECIFICATION.md` (400+ lines)

**Key Sections**:
- Assembly language syntax (instructions, operands, labels, directives)
- All 32 Babbage instructions with assembly syntax
- Assembly examples (simple program, loop, function calls)
- Two-pass algorithm with pseudocode
- Error detection and recovery
- Output formats (binary, hex, symbol map)

**Status**: Specification COMPLETE ✓

**Implementation Pseudocode**: Includes full `Assembler` class with methods for two-pass assembly, operand resolution, instruction encoding, and error reporting.

---

## Part 2: Data Flow Through Pipeline

### Step-by-Step Example: Factorial in C

#### Step 1: C Source Code
```c
long factorial(long n) {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
}
```

#### Step 2: Parse to IR (via C Compiler)
```
function factorial(n) {
  block entry {
    branch_le n, 1, base_case, recursive_case
  }
  
  block base_case {
    a = 1
    return a
  }
  
  block recursive_case {
    b = sub n, 1
    c = call factorial, b
    a = mul n, c
    return a
  }
}
```

#### Step 3: Register Allocation & Code Generation
```
Register Allocation:
  n → A (parameter)
  b → B (temporary)
  c → C (temporary)
  a → A (return, reuse)
```

#### Step 4: Code Generator Output (Assembly)
```asm
factorial:
  branch_le a, 1, base_case    (compare n with 1)
  
recursive_case:
  sub b, a, 1                  (b = n - 1)
  call factorial, b            (c = factorial(n - 1))
  mov c, a                     (c now in A from call)
  mul a, a, c                  (a = n * c)
  ret                          (return in a)
  
base_case:
  mov a, 1                     (a = 1)
  ret                          (return in a)
```

#### Step 5: Assembler Converts to Machine Code
```
Address   Machine Code           Instruction
0         0x0D 0 1 0 5         branch_le a, 1, base_case
1         0x02 1 0 1           sub b, a, 1
2         0x10 0 (addr)        call factorial
3         0x17 2 0             mov c, a
4         0x03 0 2             mul a, c
5         0x11                 ret
6         0x17 0 1             mov a, 1
7         0x11                 ret
```

#### Step 6: Execution on Babbage
```
Input: n = 5
1. branch_le 5, 1 → false, go to recursive_case
2. sub b, 5, 1 → b = 4
3. call factorial(4) → pushes return address, recursively computes
... (recursion unwinding)
Output: 120 (5! = 120)
```

---

## Part 3: Integration Points

### 3.1 Language Compiler Integration

Each language compiler must:
1. **Parse** source code to AST
2. **Semantic Analysis** (type checking, scope)
3. **Lower to IR** (language-specific AST → BIR)
4. **Validate IR** (liveness, control flow correctness)
5. **Pass to Code Generator**

**Interface**:
```python
class LanguageCompiler:
    def compile_to_ir(self, source_code: str) -> IRProgram:
        """Convert source to IR"""
    
    def validate_ir(self, ir: IRProgram) -> bool:
        """Verify IR correctness"""

class CodeGenerator:
    def generate(self, ir: IRProgram) -> str:
        """IR → Babbage assembly"""

class Assembler:
    def assemble(self, assembly_text: str) -> bytes:
        """Assembly → machine code"""
```

### 3.2 Babbage Emulator Integration

Code Generator → Assembly → Assembler → Machine Code → Emulator

**Emulator Input**: 2000-word binary (50-bit instructions)

**Emulator Output**: Program execution results (stdout, final register states)

---

## Part 4: Implementation Schedule (Week 7)

### Phase 1: Code Generator (25 hours)
**Status**: Specification COMPLETE, ready for implementation

**Tasks**:
1. Implement `LinearScanAllocator` class
2. Implement `LivenessAnalyzer` class
3. Implement `InstructionSelector` class
4. Implement `CodeEmitter` class
5. Implement `CodeGenerator` orchestrator
6. Write unit tests (10+ test cases)

**Deliverable**: Working code generator (IR → assembly)

**Effort**: 25 hours
**Timeline**: 3-4 days (with parallel work)

---

### Phase 2: Assembler (15 hours)
**Status**: Specification COMPLETE, ready for implementation

**Tasks**:
1. Implement assembly lexer/parser
2. Implement pass 1 (symbol resolution)
3. Implement pass 2 (code emission)
4. Implement instruction encoder
5. Implement error detection
6. Write unit tests (8+ test cases)

**Deliverable**: Working assembler (assembly → machine code)

**Effort**: 15 hours
**Timeline**: 2 days (can parallelize with code generator)

---

### Phase 3: Babbage Assembly Service (10 hours)
**Status**: Ready (extends BaseExecutor)

**Tasks**:
1. Create `BabbageAssemblyService` class
2. Implement `execute()` method
3. Integrate with Assembler
4. Error handling and reporting
5. Docker container (lightweight, just assembler)
6. Write unit tests (5+ test cases)

**Deliverable**: Language service for direct assembly programming

**Effort**: 10 hours
**Timeline**: 1 day

---

### Total Week 7 Foundation: 50 hours
- Specifications: 20 hours ✓ COMPLETE
- Implementation: 50 hours (in progress)
- **Total available**: 70 hours planned for Week 7

**Remaining**: 20 hours slack for testing, debugging, integration

---

## Part 5: Critical Success Factors

### 5.1 Register Allocation Correctness
**Why**: If register allocation fails, ALL language compilers fail.

**Validation**:
- [ ] Linear scan algorithm correctly computes liveness
- [ ] Spill/fill operations generated correctly
- [ ] No register conflicts (use liveness overlap checking)
- [ ] Test with 2, 3, 4, and 5+ simultaneous values

### 5.2 Instruction Encoding
**Why**: If instructions encode incorrectly, execution fails silently.

**Validation**:
- [ ] Opcode mapping correct (0x00-0x1B match ISA)
- [ ] Register codes correct (A=0, B=1, C=2, D=3)
- [ ] Address field correct (11 bits, values 0-2047)
- [ ] Immediate field correct (29 bits)
- [ ] Round-trip test: encode → decode → original

### 5.3 Symbol Resolution
**Why**: Forward references and labels must resolve correctly.

**Validation**:
- [ ] Pass 1 collects all labels and variables
- [ ] Pass 2 resolves all references using symbol table
- [ ] Forward references work (jump to later label)
- [ ] Error on undefined label

### 5.4 Memory Layout Consistency
**Why**: Global variables, locals, stack, heap must not overlap.

**Validation**:
- [ ] Globals: 0-255
- [ ] Stack frame: 256-511 (per function call)
- [ ] Heap: 512-2047
- [ ] No overlaps or out-of-bounds accesses

---

## Part 6: Testing Strategy

### Unit Tests (Code Generator)

```
Test Set 1: Register Allocation
  test_allocate_simple_2_values()          -- 2 values → 2 registers
  test_allocate_simple_4_values()          -- 4 values → 4 registers
  test_allocate_spill_5_values()           -- 5 values → 4 regs + spill
  test_liveness_simple_sequence()          -- Linear liveness
  test_liveness_branches()                 -- Branches split liveness
  test_liveness_function_call()            -- Function call splits liveness

Test Set 2: Instruction Selection
  test_select_arithmetic_add()             -- ADD instruction
  test_select_arithmetic_sqrt()            -- SQRT instruction
  test_select_memory_load()                -- LOAD instruction
  test_select_memory_store()               -- STOR instruction
  test_select_control_jump()               -- JMP instruction
  test_select_function_call()              -- CALL instruction

Test Set 3: Code Emission
  test_emit_simple_program()               -- 3-instruction program
  test_emit_loop()                         -- Loop with backward jump
  test_emit_function_call()                -- Call sequence
  test_emit_spilled_values()               -- Spill/fill code
  test_label_resolution()                  -- Label addresses correct
```

### Unit Tests (Assembler)

```
Test Set 1: Parsing
  test_parse_simple_instruction()          -- Single line
  test_parse_label()                       -- Label definition
  test_parse_comment()                     -- Comment stripping
  test_parse_directive()                   -- .global, .data, etc.

Test Set 2: Symbol Resolution
  test_resolve_forward_reference()         -- Jump to later label
  test_resolve_backward_reference()        -- Jump to earlier label
  test_error_undefined_label()             -- Undefined label error

Test Set 3: Instruction Encoding
  test_encode_arithmetic()                 -- ADD, SUB, MULT, etc.
  test_encode_memory()                     -- LOAD, STOR
  test_encode_control()                    -- JMP, CALL, RET
  test_encode_roundtrip()                  -- Encode → decode

Test Set 4: Error Detection
  test_error_invalid_mnemonic()            -- Unknown instruction
  test_error_too_many_operands()           -- Operand count mismatch
  test_error_invalid_register()            -- Register E, F, etc.
  test_error_duplicate_label()             -- Two definitions
```

---

## Part 7: Documentation Artifacts

### Completed Specifications (Week 7)

1. **BABBAGE_IR_SPECIFICATION.md** (600+ lines)
   - Language-independent intermediate form
   - Complete instruction set with semantics
   - Memory model and variable allocation
   - Function calling convention
   - Type system (lowering rules)

2. **BABBAGE_CODE_GENERATOR_SPECIFICATION.md** (500+ lines)
   - Linear scan register allocation algorithm
   - Liveness analysis
   - Instruction selection (IR → Babbage ISA)
   - Code generation examples (factorial, loop, array, record)
   - Pseudocode for `CodeGenerator` class

3. **BABBAGE_ASSEMBLER_SPECIFICATION.md** (400+ lines)
   - Assembly language syntax
   - All 32 Babbage instructions with mnemonics
   - Two-pass assembly algorithm with pseudocode
   - Error detection and recovery
   - Multiple output formats

4. **WEEK_7_BABBAGE_FOUNDATION_SUMMARY.md** (this document)
   - Architecture overview
   - Data flow through pipeline
   - Integration points
   - Testing strategy

**Total Documentation**: 1,700+ lines of specification
**Quality**: Publication-grade, pseudocode-heavy, implementation-ready

---

## Part 8: Implementation Checklist

### Code Generator Implementation
- [ ] Create `babbage_codegen.py` (main module)
- [ ] Implement `LivenessAnalyzer` class
- [ ] Implement `LinearScanAllocator` class
- [ ] Implement `InstructionSelector` class
- [ ] Implement `CodeEmitter` class
- [ ] Implement `CodeGenerator` orchestrator
- [ ] Write comprehensive unit tests
- [ ] Validate against reference IR examples
- [ ] Benchmark register allocation on complex functions

### Assembler Implementation
- [ ] Create `babbage_assembler.py` (main module)
- [ ] Implement lexer (tokenize assembly)
- [ ] Implement parser (parse instructions, labels)
- [ ] Implement pass 1 (symbol resolution)
- [ ] Implement pass 2 (code emission)
- [ ] Implement instruction encoder
- [ ] Implement error detection
- [ ] Write comprehensive unit tests
- [ ] Validate against reference assembly examples

### Assembly Service Implementation
- [ ] Create `BabbageAssemblyService` class (extends `BaseExecutor`)
- [ ] Implement `execute()` method
- [ ] Integrate with Assembler
- [ ] Create Dockerfile for assembly service
- [ ] Error handling and reporting
- [ ] Write unit tests
- [ ] Test with FastAPI endpoint

### Integration & Testing
- [ ] End-to-end test: IR → assembly → machine code
- [ ] Verify machine code executes correctly
- [ ] Test with Babbage Emulator (integration)
- [ ] Performance benchmarking
- [ ] Documentation and examples

---

## Part 9: Risk Assessment

### Risk 1: Register Allocation Complexity
**Severity**: HIGH  
**Likelihood**: MEDIUM

**Problem**: Linear scan algorithm may have edge cases with complex control flow.

**Mitigation**:
- Comprehensive unit tests before implementation
- Reference implementation from literature (Poletto & Sarkar)
- Validation against known good results
- Early testing with complex functions (recursive, loops)

### Risk 2: Instruction Encoding Errors
**Severity**: CRITICAL  
**Likelihood**: LOW

**Problem**: Incorrect instruction encoding = silent failures in execution.

**Mitigation**:
- Unit tests for every opcode (0x00-0x1B)
- Round-trip testing (encode → decode → original)
- Bit-level verification
- Validation against Babbage ISA spec

### Risk 3: Memory Layout Conflicts
**Severity**: HIGH  
**Likelihood**: MEDIUM

**Problem**: Global, stack, heap overlapping = memory corruption.

**Mitigation**:
- Static memory map verification
- Unit tests with different stack frame sizes
- Runtime bounds checking in emulator
- Documentation of memory layout per function

### Risk 4: Forward Reference Resolution
**Severity**: MEDIUM  
**Likelihood**: LOW

**Problem**: Forward references to undefined labels = incorrect jumps.

**Mitigation**:
- Pass 1 symbol table exhaustive
- Pass 2 verification all references in symbol table
- Error on undefined label
- Test with forward jumps, loops, function calls

---

## Part 10: Next Steps (After Week 7)

### Week 8: Language Compiler Implementation (Weeks 8-13)

**Critical Path** (Week 8):
1. **C Compiler** (40 hours)
   - C AST → BIR (arithmetic, control flow, functions, arrays)
   - Type system mapping (int, float → dec50)
   - Pointer arithmetic (addresses 0-2047)

2. **Haskell Compiler** (50 hours)
   - Haskell type system → BIR
   - Lazy evaluation → strict evaluation
   - Pattern matching → case statements

3. **LISP Compiler** (55 hours)
   - S-expressions → BIR
   - List structures as memory blocks
   - Closures and scoping

**Parallel Track** (Weeks 8-9):
- IDRIS2, Python, Java, System F compilers

### Week 14: Integration & Testing
- End-to-end compilation pipeline
- Unit tests for all 8 languages
- Performance profiling
- Documentation

---

## Summary

The Babbage Foundation (Week 7) provides the infrastructure for all language compilers:

1. **IR**: Language-independent, Babbage-aware intermediate form
2. **Code Generator**: Register allocation, instruction selection, assembly emission
3. **Assembler**: Two-pass assembly, machine code generation

**Status**: Specifications COMPLETE (1,700+ lines), implementation ready

**Next**: Begin Code Generator implementation (25 hours, highest priority)

**Critical Success**: Register allocation must work flawlessly (affects all languages)

---

**Document Status**: SUMMARY COMPLETE  
**Ready for**: Implementation Phase (Week 7 Phase 2)  
**Effort Remaining**: 50 hours (implementation + testing)
