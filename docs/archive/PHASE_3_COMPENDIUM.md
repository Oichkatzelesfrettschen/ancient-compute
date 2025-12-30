================================================================================
PHASE 3 COMPENDIUM: BABBAGE MECHANICAL COMPUTERS EMULATOR
Ancient Compute Project - Complete Integration Guide
================================================================================

**Status**: Phase 3 Week 1.2 (Analytical Engine Integration Complete)
**Date**: November 2025
**Target Completion**: 9 weeks (end of Phase 3)
**Test Coverage**: 17/17 AE tests passing (100%)

================================================================================
EXECUTIVE SUMMARY
================================================================================

Phase 3 implements a faithful mechanical simulation of Charles Babbage's
computing machines, grounded in primary SMG (Science Museum Group) documentation
and working hardware (1991 DE2 calculator, 2002 printer/stereotyper).

**Two distinct computational engines**:

1. **Analytical Engine (AE)** - Programmable universal computer
   - Completed and integrated (762 lines, 17 tests)
   - 50-digit decimal arithmetic
   - 2,000 word memory, 4 registers
   - 25-instruction ISA with full arithmetic
   - Punch card I/O and debugger support

2. **Difference Engine No. 2 (DE2)** - Specialized difference table calculator
   - Under development (Week 1 roadmap)
   - 8 columns × 31 decimal digits
   - Anticipating carriage mechanism
   - Mechanical timing model (0-360° shaft rotation)
   - Printer and stereotyper output apparatus

**Unified subsystems**:

- **Printer Apparatus**: Type setter, inking, hammer, platen
- **Stereotyper Frame**: 50-line mold creation and extraction
- **Debugger**: Symbol table, breakpoints, stepping (mechanical and instruction-level)

================================================================================
PHASE 3 ARCHITECTURE OVERVIEW
================================================================================

### High-Level Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                      BABBAGE MECHANICAL SYSTEMS                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌──────────────────────────┐      ┌──────────────────────────┐ │
│  │ ANALYTICAL ENGINE (AE)   │      │ DIFFERENCE ENGINE (DE2)  │ │
│  │                          │      │                          │ │
│  │ - 50-digit BabbageNumber │      │ - 31-digit DigitColumn   │ │
│  │ - 2K word memory (store) │      │ - 8 columns (sectors)    │ │
│  │ - 4 registers (A,B,C,D)  │      │ - AnticipatingCarriage   │ │
│  │ - 25 opcodes (mill)      │      │ - TimingController       │ │
│  │ - CALL/RET stacks        │      │ - Phase-based execution  │ │
│  │ - Punch card I/O         │      │ - 0-360° shaft model     │ │
│  └──────────────────────────┘      └──────────────────────────┘ │
│           │                                     │                 │
│           └─────────────────┬───────────────────┘                 │
│                             │                                     │
│  ┌──────────────────────────▼───────────────────────────────┐   │
│  │        UNIFIED I/O SUBSYSTEM                             │   │
│  │                                                           │   │
│  │  ┌──────────────┐         ┌──────────────┐             │   │
│  │  │   Printer    │         │ Stereotyper  │             │   │
│  │  │  Apparatus   │         │    Frame     │             │   │
│  │  │              │         │              │             │   │
│  │  │ - Type wheel │         │ - Mold image │             │   │
│  │  │ - Inking     │         │ - X/Y motion │             │   │
│  │  │ - Hammer     │         │ - Extraction │             │   │
│  │  │ - Platen     │         │ - 50-line cd │             │   │
│  │  └──────────────┘         └──────────────┘             │   │
│  └───────────────────────────────────────────────────────────┘   │
│           │                                                       │
│  ┌────────▼────────────────────────────────────────────────┐    │
│  │       UNIFIED DEBUGGER SUBSYSTEM                        │    │
│  │                                                          │    │
│  │  - Symbol table (IR vars → emulator state)             │    │
│  │  - Breakpoint engine (address, time, register, memory) │    │
│  │  - Stepper (instruction for AE, angle for DE2)        │    │
│  │  - State snapshots and trace logging                   │    │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

### Module Structure

```
backend/src/emulator/
├── __init__.py                   (module initialization, exports)
├── types.py                      (type definitions: DebugSnapshot, etc.)
├── analytical_engine.py          (COMPLETE: BabbageNumber, Engine, 762 lines)
├── columns.py                    (TODO Week 1: DigitColumn, ColumnBank)
├── carry.py                      (TODO Week 1: AnticipatingCarriage)
├── timing.py                     (TODO Week 1: TimingController)
├── core.py                       (TODO Week 2: DEMachine orchestrator)
├── printer.py                    (TODO Week 3: PrinterApparatus, Frame)
├── cards.py                      (TODO Week 3: CardReader, AECard)
└── debugger.py                   (TODO Week 5: Debugger, SymbolTable)

backend/tests/unit/
├── test_analytical_engine.py     (COMPLETE: 17/17 tests passing ✓)
├── test_digit_column.py          (TODO Week 1: 80 tests)
├── test_column_bank.py           (TODO Week 1: 40 tests)
├── test_anticipating_carriage.py (TODO Week 1: 50 tests)
├── test_timing_controller.py     (TODO Week 1: 60 tests)
├── test_printer.py               (TODO Week 3: 40 tests)
└── test_cards.py                 (TODO Week 3: 30 tests)
```

================================================================================
ANALYTICAL ENGINE IMPLEMENTATION REFERENCE
================================================================================

### Location and Status

**File**: `backend/src/emulator/analytical_engine.py`
**Lines of Code**: 762 (fully integrated)
**Test Status**: 17/17 PASSING (100%)
**Original Location**: `BABBAGE_ANALYTICAL_ENGINE/babbage_emulator.py`

### Architecture: Babbage's "Mill" (Arithmetic) and "Store" (Memory)

The emulator faithfully implements two core Babbage concepts:

**STORE (Memory)**:
- 2,000 cells, each holding a 50-digit decimal number
- Addressed 0-1999
- Persistent across instruction execution
- Supports LOAD (read) and STOR (write) operations

**MILL (Arithmetic)**:
- 4 working registers: A, B, C, D
- Full arithmetic: +, -, ×, ÷, √
- All operations produce 50-digit results
- Overflow detection on all operations

**INGRESS/EGRESS**:
- Input: RDCRD (read punch card into register)
- Output: WRPCH (write punch card), WRPRN (print)

### BabbageNumber: 50-Digit Fixed-Point Arithmetic

```python
class BabbageNumber:
    """Internal representation: value × 10^(-40)"""

    # Constructor
    def __init__(self, value: float | int) -> BabbageNumber
        # Scales input by 10^40: 123 → 123 × 10^40
        # Supports Python floats and ints
        # Immutable-style semantics

    # Conversion
    def to_decimal() -> float          # Returns Python float
    def to_card_format() -> str        # Returns 50-character string

    # Arithmetic (all with overflow checking)
    def __add__, __sub__, __mul__, __truediv__()

    # Comparison
    def __eq__, __lt__, __gt__, __le__, __ge__, __ne__()
```

**Design Rationale**:
- 50-digit decimal matches Babbage's historical specification
- Fixed-point (10^40 scaling) avoids floating-point errors
- All operations return BabbageNumber to preserve precision
- Overflow flag set but truncated (wraps at 10^50)

**Example Use**:
```python
a = BabbageNumber(3.14159)
b = BabbageNumber(2.71828)
c = a + b  # BabbageNumber(5.85987)

# Test overflow
big = BabbageNumber(10**50)  # exceeds maximum
big._overflow_flag  # True (internally set)
```

### Instruction Set: 25 Opcodes

All opcodes follow Babbage's mechanical notebook. Timing values represent
abstract time units (cycles) needed to execute each operation.

#### Arithmetic Operations (Mill)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| NOP | - | 0 | No operation |
| ADD | reg, operand | 8 | reg += operand |
| SUB | reg, operand | 8 | reg -= operand |
| MULT | reg, operand | 400 | A, D = reg × operand (100-digit result split) |
| DIV | reg, operand | 750 | reg /= operand (fixed-point) |
| SQRT | reg | 250 | reg = √reg (Newton's method, 100 iterations) |
| CMP | op1, op2 | 4 | Compare; set GREATER, LESS, EQUAL flags |

**Special Behavior**:
- **MULT**: Produces 100-digit product; stores upper 50 in A, lower 50 in D
- **DIV**: Fixed-point division with 10^40 scaling
- **SQRT**: Uses Newton-Raphson iteration with convergence check
- **CMP**: Sets condition flags without modifying registers

#### Memory Operations (Store)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| LOAD | reg, address | 15 | reg = memory[address] OR immediate |
| STOR | reg, address | 15 | memory[address] = reg |

**Address Modes**:
- Memory address: `[0]`, `[999]`, `[1999]` (brackets required)
- Immediate: `100`, `42`, `-5` (plain integers)

#### Control Flow (Sequencing)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| JMP | address | 4 | PC = address (unconditional) |
| JZ | address | 4 | PC = address if ZERO flag |
| JNZ | address | 4 | PC = address if ZERO clear |
| JLT | address | 4 | PC = address if LESS flag |
| JGT | address | 4 | PC = address if GREATER flag |
| JLE | address | 4 | PC = address if LESS or EQUAL |
| JGE | address | 4 | PC = address if GREATER or EQUAL |

**Conditional Flags Set By**:
- CMP: GREATER, LESS, EQUAL
- Arithmetic: ZERO (result == 0), SIGN (result < 0), OVERFLOW

#### Subroutines (Return Stack, max 16 levels)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| CALL | address | 8 | Push return address; PC = address |
| RET | - | 4 | Pop return address; PC = return address |

**Implementation**:
```python
self.return_stack.append(self.PC + 1)  # CALL saves next instruction
self.PC = int(address)
```

#### Stack Operations (Data Stack, unlimited)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| PUSH | register | 4 | Push register value onto stack |
| POP | register | 4 | Pop stack value into register |

#### I/O (Punch Cards and Printer)

| Opcode | Operands | Timing | Description |
|--------|----------|--------|-------------|
| RDCRD | register [, value] | 30 | Read punch card into register |
| WRPCH | register | 30 | Write register to punch card (output) |
| WRPRN | register | 2 | Write register to printer |

### Engine: Complete Execution Environment

```python
class Engine:
    def __init__(self):
        self.memory: List[BabbageNumber]              # 2,000 cells
        self.registers: Dict[str, BabbageNumber]      # A, B, C, D
        self.PC: int                                  # Program Counter
        self.clock_time: int                          # Simulated time

        # Condition flags (set by operations)
        self.flags: Dict[str, bool]                   # ZERO, SIGN, etc.

        # Stacks
        self.return_stack: List[int]                  # CALL/RET (max 16)
        self.data_stack: List[BabbageNumber]          # PUSH/POP

        # Debugging
        self.breakpoints: List[Dict]                  # Address, time, register
        self.trace_enabled: bool                      # Log execution

        # Program
        self.instruction_cards: List[Instruction]     # Loaded program
        self.result_cards: List[Dict]                 # Output results

    # Execution
    def execute_instruction(instr: Instruction) -> None
        """Execute single instruction, update clock and PC"""

    def load_program(filename: str) -> None
        """Load and assemble program with label resolution"""

    def run() -> None
        """Execute until program end or error"""

    # Debugging
    def set_breakpoint(type: str, target: Any) -> None
    def check_breakpoints() -> None
    def dump_state() -> str
    def save_trace(filename: str) -> None
```

### Test Suite: 17 Comprehensive Tests

**File**: `backend/tests/unit/test_analytical_engine.py`
**Status**: 17/17 PASSING (100%)

#### BabbageNumber Tests (4 tests)

```python
test_babbage_number_init()           # Initialization
test_babbage_number_add()            # Addition
test_babbage_number_sub()            # Subtraction
test_babbage_number_comparison()     # All comparison operators
```

#### Arithmetic Tests (4 tests)

```python
test_engine_add_immediate()          # ADD with immediate operand
test_engine_load_immediate()         # LOAD immediate value
test_engine_stor_to_memory()         # STOR to memory location
test_engine_load_from_memory()       # LOAD from memory location
```

#### Control Flow Tests (3 tests)

```python
test_engine_jmp()                    # JMP (unconditional jump)
test_engine_jz_true()                # JZ with flag set
test_engine_jz_false()               # JZ with flag clear
```

#### Subroutine Tests (2 tests)

```python
test_engine_call_ret()               # CALL and RET with return stack
test_engine_push_pop()               # PUSH and POP with data stack
```

#### I/O Tests (4 tests)

```python
test_engine_rdcrd()                  # RDCRD input
test_engine_wrpch()                  # WRPCH output to punch card
test_engine_wrprn()                  # WRPRN output to printer
test_engine_nop()                    # NOP (no operation)
```

### Timing Model: Instruction Costs

Each instruction has a cost in abstract time units. Accumulated in `clock_time`.

```python
TIMING_TABLE = {
    'NOP':      0,      # No operation
    'ADD':      8,      # Basic addition
    'SUB':      8,      # Basic subtraction
    'CMP':      4,      # Compare
    'JMP':      4,      # Jump
    'JZ':       4,      # Conditional jump
    'LOAD':    15,      # Memory read
    'STOR':    15,      # Memory write
    'MULT':   400,      # Multiplication (slow!)
    'DIV':    750,      # Division (very slow!)
    'SQRT':   250,      # Square root
    'RDCRD':   30,      # Read punch card
    'WRPCH':   30,      # Write punch card
    'WRPRN':    2,      # Write to printer
    'CALL':     8,      # Subroutine call
    'RET':      4,      # Return from subroutine
    'PUSH':     4,      # Push to stack
    'POP':      4,      # Pop from stack
}
```

**Historical Accuracy**:
- These timing values are loosely based on Babbage's mechanical notebook
- Used for performance analysis and historical simulation
- Not critical to correctness (all operations complete eventually)

================================================================================
INTEGRATION WITH PHASE 2 COMPILER IR
================================================================================

### Compiler-to-Emulator Pipeline

```
Source Code (Lisp/IDRIS2/Java)
    ↓
[Phase 2] Language Lexer/Parser
    ↓
[Phase 2] Type System & IR Generation
    ↓
Babbage IR (intermediate representation)
    ↓
[Phase 3] Symbol Table Resolution
    ↓
[Phase 3] IR → Babbage ISA Translation
    ↓
Analytical Engine Program (25 opcodes)
    ↓
Engine.execute_instruction() ← You are here
    ↓
Results: registers, memory, output cards
```

### Symbol Table Integration (TODO Week 5)

The debugger's symbol table will bridge Phase 2 IR variables to Phase 3
register/memory locations:

```python
class SymbolTable:
    """Maps compiler IR variables to emulator state"""

    # IR variable → Engine location
    def add_symbol(var_name: str, location: str)
        # location: "register.A", "memory[0]", "memory[1000]"

    # Get variable value during execution
    def get_value(var_name: str, engine: Engine) -> BabbageNumber
        # Looks up location and reads from engine state

    # Breakpoint on variable change
    def watch_variable(var_name: str) -> Breakpoint
        # Creates breakpoint triggered when variable changes
```

================================================================================
WEEK-BY-WEEK IMPLEMENTATION ROADMAP
================================================================================

### Week 1-2: Emulator Core (Target: 17 + 260 = 277 tests)

**Current Status**: ✓ AE Integration (17 tests passing)

**Remaining**:

```
W1.1: DigitColumn (150 lines, 80 tests)
  - Digit storage: 31 positions × 0-9 each
  - add_difference() with carry propagation
  - get_value_as_int() / set_value_from_int()
  - Carry state management
  - Est. 4 hours development

W1.2: ColumnBank (200 lines, 40 tests)
  - Unified state for 8 columns
  - add_difference_row() for synchronized addition
  - get_all_values() for result extraction
  - state_snapshot() for debugging
  - Est. 3 hours development

W1.3: AnticipatingCarriage (300 lines, 50 tests)
  - Look-ahead carry logic (2-position depth)
  - evaluate_carry_at_position()
  - execute_carry_cycle() for full propagation
  - Reduces 8-16 cycles to 2 (Babbage's innovation)
  - Est. 5 hours development

W1.4: TimingController (400 lines, 60 tests)
  - Main shaft angle (0-360°) tracking
  - Phase-to-event mapping (from SMG timing diagram)
  - advance_shaft() for angle increments
  - execute_full_cycle() for complete rotation
  - Event logging and dispatch
  - Est. 6 hours development

W1.5: DEMachine (200 lines, 30 tests)
  - Top-level orchestrator
  - Integration of all components
  - run_cycle() and run_n_cycles()
  - State snapshot capture
  - Est. 4 hours development

W2.1: Integration Test (1 integration test, 1 hour)
  - Test case: x² + x + 1 for x ∈ [0, 5]
  - Validate difference table generation
  - Verify output matches hand calculation
```

**Week 1-2 Deliverables**:
- 1,250 lines of new code
- 260+ new unit tests
- 1 integration test (polynomial evaluation)
- All tests passing
- SMG timing compliance validated

### Week 3-4: I/O Subsystem (Target: 277 + 105 = 382 tests)

**PrinterApparatus** (300 lines, 40 tests):
- Type wheel positioning (8-digit output)
- Inking roller engagement
- Hammer strike mechanism
- Platen advance (line by line)

**StereotypeFrame** (300 lines, 35 tests):
- Mold image creation (2D grid)
- X/Y positioning (8 columns × 50 lines)
- Mold extraction and cadence validation
- Metal type conversion

**CardReader + Enhanced AE** (200 lines, 30 tests):
- Punch card format parsing
- AE reduced store (20 columns for card deck execution)
- Lovelace Bernoulli sequence card deck

### Week 5-6: Debugger (Target: 382 + 90 = 472 tests)

**SymbolTable** (150 lines):
- IR variable → engine location mapping
- Value inspection during execution

**BreakpointEngine** (200 lines, 45 tests):
- Enhanced breakpoints for mechanical events
- Register, memory, and phase conditions

**Stepper** (150 lines, 45 tests):
- Instruction-level stepping for AE
- Angle/phase stepping for DE2
- Breakpoint triggering

### Week 7-8: Validation (Target: 472 + 200+ = 672+ tests)

- DE2 + printer + stereotyper pipeline
- AE + card deck + printer pipeline
- Debugger + symbol table integration
- 90%+ code coverage

### Week 9: Documentation

- User guide for running both engines
- Mechanism reference (all components)
- API documentation
- SMG cross-reference guide
- Troubleshooting guide

================================================================================
CRITICAL DESIGN DECISIONS
================================================================================

### 1. Dual Computation Models

**Decision**: Keep separate AE and DE2 implementations
**Rationale**:
- AE: 50-digit precision, programmable ISA, universal
- DE2: 31-digit simple, mechanical phases, specialized
- Both are historically accurate distinct machines
- Better testing and validation

**Integration**: Unified I/O and debugger bridge them

### 2. Number Representation

**AE**: BabbageNumber (50-digit, 10^40 scaling)
**DE2**: DigitColumn (31-digit, simple positional)
**Rationale**: Each matches historical specification exactly

### 3. Timing Model

**Phase Model**: Not instruction count, but mechanical angles (0-360°)
**Rationale**: Matches SMG technical description timing diagram
**Advantages**: Enables period counting, phase-based debugging

### 4. I/O Unification

Both engines share:
- PrinterApparatus (type wheel, hammer, platen)
- StereotypeFrame (mold creation, 50-line cadence)

**Rationale**: Physically accurate (both had printer option)

### 5. Debugger Unification

Single debugger system:
- Symbol table maps IR variables to locations
- Breakpoints work on instructions AND mechanical phases
- Stepper granularity: instruction vs angle

**Rationale**: Enables high-level debugging despite low-level execution

================================================================================
VALIDATION STRATEGY
================================================================================

### Unit Testing (600+ tests target)

```
AE:              17/17 ✓
DigitColumn:      80
ColumnBank:       40
Carry:            50
Timing:           60
DEMachine:        30
Printer:          40
Stereotyper:      35
Debugger:         90
Integration:     200+
─────────────────────
TOTAL:          642+
```

### Integration Testing

**Polynomial Evaluation** (Week 1 end):
- Compute x² + x + 1 for x ∈ [0, 5]
- Verify 6-cycle execution
- Check output matches hand calculation

**Bernoulli Sequence** (Week 3 end):
- AE with Lovelace card deck
- Verify computation of Bernoulli numbers
- Check printed output format

**Cross-System** (Week 7 end):
- DE2 + printer: verify type output
- DE2 + stereotyper: verify mold extraction
- AE + debugger: set breakpoints, step through

### SMG Documentation Compliance

**Timing** (Week 1 end):
- Phase events match SMG timing diagram
- Carry propagation timing accurate

**Mechanics** (Week 3 end):
- Printer output format matches historical specimens
- Stereotyper mold size (50 lines × 8 columns)

**Functionality** (Week 7 end):
- AE ISA matches Lovelace/Menabrea
- DE2 difference engine matches SMG specs

================================================================================
KEY FILES AND LOCATIONS
================================================================================

### Specification Documents

- **PHASE_3_OVERVIEW.md**: Executive summary (200 lines)
- **PHASE_3_ARCHITECTURE_SPECIFICATION.md**: Complete technical design (1,800 lines)
- **PHASE_3_IMPLEMENTATION_ROADMAP.md**: 9-week granular breakdown (1,000 lines)
- **PHASE_3_STATUS_SUMMARY.md**: Current progress and readiness (300 lines)
- **PHASE_3_IMPLEMENTATION_SUMMARY.md**: Integration strategy (400 lines)
- **PHASE_3_COMPENDIUM.md**: This file (master reference)

### Code

**Module**: `backend/src/emulator/`
- `__init__.py`: Module initialization, exports
- `types.py`: Type definitions
- `analytical_engine.py`: ✓ COMPLETE (762 lines, 17 tests)
- `columns.py`: TODO
- `carry.py`: TODO
- `timing.py`: TODO
- `core.py`: TODO
- `printer.py`: TODO
- `cards.py`: TODO
- `debugger.py`: TODO

**Tests**: `backend/tests/unit/`
- `test_analytical_engine.py`: ✓ COMPLETE (17/17 passing)
- `test_digit_column.py`: TODO
- `test_column_bank.py`: TODO
- `test_anticipating_carriage.py`: TODO
- `test_timing_controller.py`: TODO
- `test_de_machine.py`: TODO
- `test_printer.py`: TODO
- `test_cards.py`: TODO
- `test_debugger.py`: TODO

================================================================================
QUICK START GUIDE
================================================================================

### Running the Analytical Engine

```python
from backend.src.emulator import Engine, Instruction, BabbageNumber

# Create engine
engine = Engine()

# Load program from file
engine.load_program('my_program.aea')

# Or add instructions directly
engine.instruction_cards = [
    Instruction('LOAD', ['A', '10']),
    Instruction('ADD', ['A', '20']),
    Instruction('WRPRN', ['A']),
]

# Execute
engine.run()

# Check results
print(engine.dump_state())
for card in engine.result_cards:
    print(f"Output: {card['value'].to_decimal()}")
```

### AEA Program Format

```
# Factorial of 5 (demo program)
start:  LOAD A 5      # Load 5 into A
        LOAD D 1      # Load 1 into D (accumulator)

loop:   MULT D A      # D *= A
        SUB A 1       # A -= 1
        JNZ loop      # Jump if A != 0

        WRPRN D       # Print result
```

### Running Tests

```bash
# Run all AE tests
python -m pytest backend/tests/unit/test_analytical_engine.py -v

# Run with coverage
pytest --cov=backend.src.emulator --cov-report=term-missing

# Run specific test
pytest backend/tests/unit/test_analytical_engine.py::test_engine_mult -v
```

================================================================================
COMMON ISSUES AND SOLUTIONS
================================================================================

### Issue: Overflow in Multiplication

**Symptom**: Product exceeds 50 digits, MULT truncates

**Solution**: MULT splits 100-digit product into A (high 50) and D (low 50)
- Check both registers after MULT
- High precision: use D for remainder

### Issue: Breakpoint Never Triggered

**Symptom**: set_breakpoint() doesn't pause execution

**Solution**: Call check_breakpoints() during instruction execution
- Engine.step_one_instruction() does this automatically
- Manual execution: call check_breakpoints() between instructions

### Issue: Return Stack Overflow

**Symptom**: RuntimeError "Return stack overflow"

**Solution**: CALL limit is 16 levels (historical constraint)
- Design recursion to use less than 16 levels
- Use data stack for state instead of deep recursion

### Issue: Division by Zero

**Symptom**: ZeroDivisionError on DIV instruction

**Solution**: Check for zero operand before division
- DIV raises exception (halt execution)
- Use CMP + JZ to skip division when needed

### Issue: Labels Not Resolved

**Symptom**: JMP to label gives "invalid address"

**Solution**: Load program with load_program() (does label resolution)
- Hand-assembled programs must use numeric addresses
- Label resolution: two-pass assembly in load_program()

================================================================================
APPENDIX: SMG REFERENCES
================================================================================

### Primary Sources

- **SMG Technical Description**: Charles Babbage's Difference Engine No. 2
  - 232 pages, comprehensive mechanical specifications
  - Timing diagrams, phase maps, component designs
  - Available from Science Museum Group

- **Menabrea, L.F. & Lovelace, A.A. (1843)**: "Note on the Analytical Engine"
  - Original description of Analytical Engine
  - First computer program (Bernoulli number sequence)
  - Published in Taylor's Scientific Memoirs

- **Babbage, C. (1826)**: "Mechanical Notation"
  - Formal notation for describing mechanical systems
  - Royal Society Philosophical Transactions
  - Still used for machine documentation

### Referenced Sections in This Document

- **Timing Table** (TIMING_TABLE): Based on SMG timing diagram, Plans 27-28
- **Anticipating Carriage** (Week 1.3): SMG Technical Description, p. 89-95
- **Printer Apparatus** (Week 3): SMG Technical Description, p. 150-165
- **Stereotyper** (Week 3): SMG Technical Description, p. 175-185
- **Phase Timing** (TimingController): SMG Timing Diagram, p. 45-60

================================================================================
NEXT IMMEDIATE ACTIONS
================================================================================

**Remaining this week**:

1. ✓ Move babbage_emulator.py → backend/src/emulator/analytical_engine.py
2. ✓ Create test_analytical_engine.py with 17 tests
3. ✓ Verify all 17 tests passing
4. ✓ Create PHASE_3_COMPENDIUM.md (this document)

**Next week (Week 1.1-1.2)**:

1. Create DigitColumn class (backend/src/emulator/columns.py)
2. Create 80 unit tests for DigitColumn
3. Create ColumnBank class
4. Create 40 unit tests for ColumnBank
5. Begin AnticipatingCarriage implementation

**Success Criteria**:

- [ ] 17 AE tests still passing (regression check)
- [ ] 80 DigitColumn tests passing
- [ ] 40 ColumnBank tests passing
- [ ] Code coverage > 80% on new code
- [ ] All compiler warnings resolved
- [ ] SMG timing compliance documented

================================================================================
CONCLUSION
================================================================================

Phase 3 is now in **Week 1 Implementation** with the Analytical Engine fully
integrated and documented. This compendium serves as the master reference for
all Phase 3 work, grounding our implementation in historical accuracy while
maintaining modern software engineering standards.

The dual-engine architecture (AE for general computation, DE2 for difference
tables) reflects Babbage's actual design philosophy: specialized and universal
machines working in concert.

**Estimated Completion**: 9 weeks with current plan
**Risk Level**: LOW (solid foundation, specifications validated, tests driving)
**Quality Gate**: 90%+ coverage, all warnings as errors, SMG compliance verified

Next commit: After Week 1.1-1.2 DigitColumn + ColumnBank completion

================================================================================
END COMPENDIUM
================================================================================
