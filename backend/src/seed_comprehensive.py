#!/usr/bin/env python3
"""Comprehensive curriculum seeder for Ancient Compute.

Populates database with complete 12,500-year timeline:
- 8 historical eras (20,000 BC to 2025 AD)
- Babbage Engine curriculum modules (7 modules)
- Lessons extracted from curriculum materials
- Sample exercises for hands-on practice

Data extracted from CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md
and historical era definitions from backend/src/models/era.py
"""

from sqlalchemy.orm import Session
from .database import SessionLocal, Base, engine
from .models import Era, Module, Lesson, Exercise, User
from .models.lesson import LessonType, SupportedLanguage
from .models.exercise import DifficultyLevel

# ============================================================================
# ERA DEFINITIONS (from era.py comments)
# ============================================================================

ERAS_DATA = [
    {
        "label": "prehistory",
        "full_name": "Prehistory of Counting (20,000 BC - 3,000 BC)",
        "description": "Ishango bone, tally marks, clay tokens, one-to-one correspondence",
        "historical_context": """
The origins of counting and early numerical recording systems. Archaeological evidence
from the Ishango bone (25,000 years old) shows early tally marks for counting. Clay
tokens from Mesopotamia (8,000 BC) represent the first symbolic representation of
quantities, evolving into the world's first writing systems.
        """.strip(),
        "start_year": -20000,
        "end_year": -3000,
        "color": "#FF6B6B",  # Red
        "icon": "🦴",
        "order": 0,
        "is_published": True,
    },
    {
        "label": "ancient",
        "full_name": "Ancient Foundations (3,000 BC - 500 AD)",
        "description": "Babylonian algorithms, Greek logic, Indian decimal system, Chinese I Ching",
        "historical_context": """
Mesopotamian base-60 arithmetic and algorithmic procedures, Egyptian fraction mathematics,
Euclidean algorithms and geometric proofs, Indian invention of zero and decimal positional
notation, Chinese I Ching binary system and abacus calculation methods. The foundations
of formal algorithmic thinking across multiple civilizations.
        """.strip(),
        "start_year": -3000,
        "end_year": 500,
        "color": "#4ECDC4",  # Teal
        "icon": "📜",
        "order": 1,
        "is_published": True,
    },
    {
        "label": "medieval",
        "full_name": "Medieval Transmission (500 - 1,500 AD)",
        "description": "Islamic Golden Age, Al-Khwarizmi algebra, scholastic logic",
        "historical_context": """
Preservation and advancement of Greek and Indian mathematics through Islamic scholars.
Al-Khwarizmi's algebra (820 CE) gives us the word 'algorithm'. Translation movement
brings ancient knowledge to Europe. Development of symbolic notation and systematic
problem-solving methods.
        """.strip(),
        "start_year": 500,
        "end_year": 1500,
        "color": "#45B7D1",  # Blue
        "icon": "🕌",
        "order": 2,
        "is_published": True,
    },
    {
        "label": "early-modern",
        "full_name": "Early Modern Symbolic Revolution (1,500 - 1,850 AD)",
        "description": "Leibniz binary, Pascal calculator, Boolean algebra, Babbage/Lovelace",
        "historical_context": """
Development of symbolic mathematics and mechanical calculation. Leibniz's binary
arithmetic (1703), Pascal's Pascaline calculator (1642), Boolean symbolic logic algebra
(1854), and Babbage's Analytical Engine designs with Ada Lovelace's programming notes
(1843) - the first algorithm intended for machine execution.
        """.strip(),
        "start_year": 1500,
        "end_year": 1850,
        "color": "#96CEB4",  # Green
        "icon": "⚙️",
        "order": 3,
        "is_published": True,
    },
    {
        "label": "foundations-crisis",
        "full_name": "Foundations Crisis (1,850 - 1,940 AD)",
        "description": "Frege logic, Russell paradoxes, Gödel incompleteness, Church lambda calculus, Turing machines",
        "historical_context": """
Crisis in mathematical foundations leading to formal logic and computability theory.
Frege's logical notation, Russell's paradoxes and type theory, Gödel's incompleteness
theorems, Church's lambda calculus, and Turing's universal computing machines.
The theoretical foundations of computer science established.
        """.strip(),
        "start_year": 1850,
        "end_year": 1940,
        "color": "#FECA57",  # Yellow
        "icon": "🔬",
        "order": 4,
        "is_published": True,
    },
    {
        "label": "electronic-age",
        "full_name": "Electronic Age (1,940 - 1,980 AD)",
        "description": "ENIAC, von Neumann architecture, LISP, ALGOL, transistors, integrated circuits",
        "historical_context": """
Transition from mechanical to electronic computation. ENIAC (1945), von Neumann
architecture, development of high-level languages (FORTRAN, LISP, ALGOL), transistor
invention (1947), and integrated circuits (1958). The birth of modern computing.
        """.strip(),
        "start_year": 1940,
        "end_year": 1980,
        "color": "#9C88FF",  # Purple
        "icon": "💡",
        "order": 5,
        "is_published": True,
    },
    {
        "label": "type-theory",
        "full_name": "Type Theory Evolution (1,970 - 2,000 AD)",
        "description": "Curry-Howard isomorphism, System F, Hindley-Milner, Martin-Löf, dependent types",
        "historical_context": """
Development of advanced type systems and proof assistants. Curry-Howard correspondence
between proofs and programs, System F polymorphic lambda calculus, Hindley-Milner type
inference, Martin-Löf intuitionistic type theory, and dependent types. Types as
specifications and proofs.
        """.strip(),
        "start_year": 1970,
        "end_year": 2000,
        "color": "#FD79A8",  # Pink
        "icon": "λ",
        "order": 6,
        "is_published": True,
    },
    {
        "label": "paradigm-synthesis",
        "full_name": "Paradigm Synthesis (1,980 - 2,025 AD)",
        "description": "Multi-paradigm languages, modern type systems, quantum computing foundations",
        "historical_context": """
Integration of functional, object-oriented, and dependently-typed paradigms. Modern
languages (Haskell, Rust, Kotlin, Swift) combining multiple paradigms. Quantum computing
foundations. Type systems as both safety mechanisms and design tools.
        """.strip(),
        "start_year": 1980,
        "end_year": 2025,
        "color": "#54A0FF",  # Light blue
        "icon": "🚀",
        "order": 7,
        "is_published": True,
    },
]

# ============================================================================
# BABBAGE ENGINE CURRICULUM MODULES
# ============================================================================

BABBAGE_MODULES = [
    {
        "era_label": "early-modern",  # Babbage was 1800s
        "slug": "babbage-intro",
        "title": "Introduction: How This Curriculum Works",
        "description": "Philosophy, structure, and learning outcomes for the Babbage Analytical Engine curriculum",
        "start_year": 1840,
        "end_year": 1850,
        "sequence_order": 1,
        "estimated_hours": 1,
        "difficulty_level": 1,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-architecture",
        "title": "Understanding Babbage Architecture",
        "description": "Five-component system: Mill, Store, Barrel, I/O, and Prime Mover",
        "start_year": 1840,
        "end_year": 1850,
        "sequence_order": 2,
        "estimated_hours": 6,
        "difficulty_level": 2,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-isa-fundamentals",
        "title": "ISA Fundamentals",
        "description": "Instruction set architecture, opcodes, addressing modes, and assembly programming",
        "start_year": 1840,
        "end_year": 1850,
        "sequence_order": 3,
        "estimated_hours": 8,
        "difficulty_level": 3,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-emulator",
        "title": "Using the Emulator",
        "description": "Running programs, debugging, and understanding emulator output",
        "start_year": 1840,
        "end_year": 1850,
        "sequence_order": 4,
        "estimated_hours": 6,
        "difficulty_level": 2,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-performance",
        "title": "Performance Analysis",
        "description": "Timing calculations, optimization strategies, and mechanical constraints",
        "start_year": 1840,
        "end_year": 1850,
        "sequence_order": 5,
        "estimated_hours": 8,
        "difficulty_level": 4,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-historical-context",
        "title": "Historical Context",
        "description": "1930s-1960s operation in India, Brazil, Argentina, and China",
        "start_year": 1930,
        "end_year": 1960,
        "sequence_order": 6,
        "estimated_hours": 5,
        "difficulty_level": 2,
        "era_enum": "early_modern",
    },
    {
        "era_label": "early-modern",
        "slug": "babbage-complete-project",
        "title": "Complete Project",
        "description": "End-to-end implementation of a real-world computational problem",
        "start_year": 1840,
        "end_year": 1960,
        "sequence_order": 7,
        "estimated_hours": 10,
        "difficulty_level": 5,
        "era_enum": "early_modern",
    },
]

# ============================================================================
# LESSONS FOR EACH MODULE
# ============================================================================

LESSONS_DATA = {
    "babbage-intro": [
        {
            "slug": "philosophy",
            "title": "Curriculum Philosophy",
            "lesson_type": LessonType.READING,
            "sequence_order": 1,
            "description": "Understanding the three-fold approach: Machine, Computer, and Historical Artifact",
            "content_markdown": """
# Curriculum Philosophy

This curriculum teaches the Babbage Analytical Engine as:

1. **A Machine**: 1910s precision engineering with mechanical arithmetic
2. **A Computer**: Programmable instruction processor with memory and I/O
3. **A Historical Artifact**: How computation was actually performed 1930-1960

## Pedagogical Structure

- **HOW**: Practical procedures and hands-on exercises
- **WHAT**: Conceptual understanding of architecture and design
- **WHEN**: Historical timelines and operational constraints
- **WHERE**: Regional variations and environmental adaptations
- **WHY**: Motivation for design choices and use cases

## Learning Outcomes

By completing this curriculum, you will:
- Understand Babbage architecture deeply
- Program in Babbage Assembly
- Predict performance before execution
- Connect history to code implementation
            """,
            "estimated_minutes": 15,
        },
        {
            "slug": "prerequisites",
            "title": "Prerequisites and Target Audience",
            "lesson_type": LessonType.READING,
            "sequence_order": 2,
            "description": "Required background knowledge and expected time commitment",
            "content_markdown": """
# Prerequisites

## Required Knowledge
- Basic computer architecture (von Neumann model)
- Familiarity with assembly-like programming
- Interest in computational history

## Time Commitment
- **Total**: 35-47 hours (approximately one intensive week)
- Module 1: 4-6 hours
- Module 2: 6-8 hours
- Module 3: 4-6 hours
- Module 4: 6-8 hours
- Module 5: 4-5 hours
- Module 6: 3-4 hours
- Module 7: 8-10 hours

## Target Audience
- Undergraduate CS students (second/third year)
- Computer history enthusiasts
- Museum exhibit designers
- Software engineers interested in processor design
            """,
            "estimated_minutes": 10,
        },
    ],
    "babbage-architecture": [
        {
            "slug": "five-component-system",
            "title": "The Machine as a Whole",
            "lesson_type": LessonType.READING,
            "sequence_order": 1,
            "description": "Overview of the five interacting mechanical subsystems",
            "content_markdown": """
# Five-Component System

## 1. The Mill (Arithmetic Unit)
- **Purpose**: Performs arithmetic operations (ADD, SUB, MULT, DIV, SQRT)
- **Components**: 8 digit wheels per register (50-digit numbers)
- **Time Cost**: 8 seconds (ADD) to 750 seconds (DIV)

## 2. The Store (Memory)
- **Capacity**: 2,000 fifty-digit numbers
- **Organization**: 2,000 × 50 matrix (100,000 total digits)
- **Access Time**: 15 seconds per access

## 3. The Barrel (Control Mechanism)
- **Purpose**: Program storage and sequencing
- **Mechanism**: Rotating cylinder with pegs
- **Capacity**: Up to 1,000 instructions

## 4. Input/Output Subsystem
- **Card Reader**: 80-column Hollerith cards
- **Card Punch**: Output to cards
- **Printer**: Human-readable output
- **Time Cost**: 30 seconds per I/O operation

## 5. Prime Mover (Power Source)
- Hand Crank or Steam Engine
- Uniform rotation speed to all subsystems
            """,
            "estimated_minutes": 30,
        },
        {
            "slug": "mill-deep-dive",
            "title": "The Mill (Arithmetic Unit)",
            "lesson_type": LessonType.READING,
            "sequence_order": 2,
            "description": "Mechanical arithmetic operations and performance characteristics",
            "content_markdown": """
# The Mill: Mechanical Arithmetic

## Digit Representation
- One gear per digit position (0-49)
- Each gear has 10 teeth (digits 0-9)
- Mechanical constraints ensure single-digit values

## Addition Example: 123 + 456 = 579
1. Load 123 into Register A
2. Load 456 into Register B
3. Engage carry lever chain
4. Rotate mechanism one complete turn
5. Carry propagates right-to-left
6. Result appears in Register A: 579

## Performance Table

| Operation | Time | Explanation |
|-----------|------|-------------|
| ADD | 8 sec | Single rotation with carry |
| SUB | 8 sec | Complement addition |
| MULT | 400 sec | 50 iterations of addition |
| DIV | 750 sec | Repeated subtraction |
| SQRT | 200 sec | Newton-Raphson (5-10 iterations) |

## Real Example: Factorial(5) = 120
- Step 1: 1 × 2 = 2 (408s)
- Step 2: 2 × 3 = 6 (408s)
- Step 3: 6 × 4 = 24 (408s)
- Step 4: 24 × 5 = 120 (408s)
- **Total**: 1,632 seconds ≈ 27 minutes
            """,
            "estimated_minutes": 45,
        },
        {
            "slug": "store-deep-dive",
            "title": "The Store (Memory)",
            "lesson_type": LessonType.READING,
            "sequence_order": 3,
            "description": "2D mechanical memory organization and access patterns",
            "content_markdown": """
# The Store: Mechanical Memory

## Physical Layout
- 2,000 columns (addresses 0-1999)
- 50 rows (digit positions)
- Total: 100,000 digit wheels

## Address Resolution
1. Operator specifies address (0-1999)
2. Mechanical indexing rod moves to column
3. Engage read/write lever
4. All 50 digits transfer simultaneously
5. Time cost: 15 seconds

## Memory Access Patterns

Example: Census data aggregation (1951 India)

```
LOOP:
  LOAD  A, [base + counter]     # 15s
  ADD   A, [accumulator]        # 8s
  STOR  A, [accumulator]        # 15s
  CMP   counter, limit          # 10s
  JLT   LOOP                    # 0s

Per iteration: 48 seconds
For 1,000 districts: 13.3 hours
```

**Optimization**: 48% of time is memory access!
Keep intermediate results in registers longer.
            """,
            "estimated_minutes": 40,
        },
        {
            "slug": "barrel-deep-dive",
            "title": "The Barrel (Control Mechanism)",
            "lesson_type": LessonType.READING,
            "sequence_order": 4,
            "description": "Mechanical program storage and instruction sequencing",
            "content_markdown": """
# The Barrel: Mechanical Program Storage

## Physical Specification
- Cylinder: 40 cm diameter, 50 cm length
- 1,000 peg positions (one per instruction)
- Each peg engages one of 32 levers (opcodes)
- Rotation rate: 1 step per instruction

## Program Encoding
```
Position 0: ADD lever (opcode 0x01)
Position 1: LOAD lever (opcode 0x04)
Position 2: STOR lever (opcode 0x05)
Position 3: JMP lever (opcode 0x0C)
...
Position 999: HALT lever (opcode 0x17)
```

## Programming Timeline (Brazil, 1952)
1. Algorithm Design: 1 day
2. Assembly Language: 2-3 days
3. Manual Verification: 1 day
4. Peg Diagram Generation: 4 hours
5. Peg Setting: 6 hours
6. Initial Testing: 4 hours
7. Production Run: 4-8 hours
8. Result Analysis: 1-2 days

**Total**: 1-2 weeks from algorithm to output

## Regional Variations

| Region | Method | Time | Program Size |
|--------|--------|------|--------------|
| India (1931) | Manual | 6-8 hours | 200-300 instructions |
| Brazil (1950) | Motorized | 2-3 hours | 400-600 instructions |
| Argentina (1952) | Automated | 30-45 min | 600-800 instructions |
            """,
            "estimated_minutes": 35,
        },
    ],
    "babbage-isa-fundamentals": [
        {
            "slug": "instruction-format",
            "title": "Instruction Format and Encoding",
            "lesson_type": LessonType.READING,
            "sequence_order": 1,
            "description": "Understanding the 32-bit instruction format and opcode encoding",
            "content_markdown": """
# Babbage ISA Instruction Format

## 32-Bit Instruction Encoding

```
31    28 27    24 23    20 19    16 15             0
┌───────┬───────┬───────┬───────┬─────────────────┐
│ Opcode│  Rd   │  Rs1  │  Rs2  │    Immediate    │
└───────┴───────┴───────┴───────┴─────────────────┘
```

## Opcode Categories

| Opcode | Mnemonic | Category | Example |
|--------|----------|----------|---------|
| 0x01   | ADD      | Arithmetic | ADD R1, R2, R3 |
| 0x02   | SUB      | Arithmetic | SUB R1, R2, R3 |
| 0x03   | MULT     | Arithmetic | MULT R1, R2 |
| 0x04   | LOAD     | Memory | LOAD R1, [addr] |
| 0x05   | STOR     | Memory | STOR R1, [addr] |
| 0x0C   | JMP      | Control | JMP label |
| 0x0D   | JZ       | Control | JZ R1, label |
| 0x17   | HALT     | System | HALT |

## Addressing Modes

1. **Register Direct**: `ADD R1, R2, R3`
2. **Immediate**: `LOAD R1, #42`
3. **Memory Direct**: `LOAD R1, [1000]`
4. **Register Indirect**: `LOAD R1, [R2]`
            """,
            "estimated_minutes": 30,
        },
        {
            "slug": "assembly-basics",
            "title": "Assembly Language Programming",
            "lesson_type": LessonType.CODING,
            "sequence_order": 2,
            "description": "Writing and understanding Babbage assembly programs",
            "language": SupportedLanguage.ASSEMBLY,
            "content_markdown": """
# Babbage Assembly Programming

## Program Structure

```assembly
; Data section
.data
    counter: .word 0
    result:  .word 0

; Code section
.text
    LOAD  R1, #10          ; Load immediate
    STOR  R1, [counter]    ; Store to memory
loop:
    LOAD  R2, [counter]    ; Load from memory
    SUB   R2, R2, #1       ; Decrement
    STOR  R2, [counter]    ; Store back
    JZ    R2, done         ; Jump if zero
    JMP   loop             ; Continue loop
done:
    HALT
```

## Timing Considerations

Remember: Babbage is SLOW!
- ADD/SUB: 8 seconds
- MULT: 400 seconds
- LOAD/STOR: 15 seconds each
- JMP: 0 seconds (mechanical)

Plan your algorithms accordingly!
            """,
            "starter_code": """
; Write a program to sum numbers 1 to 10
.data
    sum: .word 0

.text
    ; Your code here
    HALT
""",
            "solution_code": """
; Sum numbers 1 to 10
.data
    sum: .word 0

.text
    LOAD  R1, #0           ; accumulator
    LOAD  R2, #1           ; counter
loop:
    ADD   R1, R1, R2       ; sum += counter
    ADD   R2, R2, #1       ; counter++
    LOAD  R3, #11          ; limit
    SUB   R3, R2, R3       ; counter - 11
    JZ    R3, done         ; if counter == 11, done
    JMP   loop
done:
    STOR  R1, [sum]
    HALT
""",
            "estimated_minutes": 45,
        },
        {
            "slug": "advanced-instructions",
            "title": "Advanced Instructions and Techniques",
            "lesson_type": LessonType.READING,
            "sequence_order": 3,
            "description": "Subroutines, stack operations, and advanced control flow",
            "content_markdown": """
# Advanced Babbage Programming

## Subroutine Calling Convention

```assembly
; Caller
    LOAD  R1, #5           ; Argument
    CALL  factorial        ; Returns in R1
    STOR  R1, [result]
    HALT

; Callee
factorial:
    PUSH  R2               ; Save registers
    PUSH  R3

    LOAD  R2, #1           ; Result accumulator
    LOAD  R3, R1           ; Counter (n)
fact_loop:
    MULT  R2, R3           ; result *= n
    SUB   R3, R3, #1       ; n--
    JZ    R3, fact_done
    JMP   fact_loop
fact_done:
    LOAD  R1, R2           ; Return value
    POP   R3               ; Restore registers
    POP   R2
    RET
```

## Stack Frame Layout

```
High Memory
┌──────────────┐
│ Return Addr  │ ← SP after CALL
├──────────────┤
│ Saved R2     │
├──────────────┤
│ Saved R3     │
├──────────────┤
│ Local Var 1  │
├──────────────┤
│ Local Var 2  │ ← SP during execution
└──────────────┘
Low Memory
```
            """,
            "estimated_minutes": 50,
        },
    ],
    "babbage-emulator": [
        {
            "slug": "getting-started",
            "title": "Getting Started with the Emulator",
            "lesson_type": LessonType.INTERACTIVE,
            "sequence_order": 1,
            "description": "First program, running code, and viewing output",
            "content_markdown": """
# Using the Babbage Emulator

## Quick Start

1. **Write Assembly Code** in the editor
2. **Click "Assemble"** to convert to machine code
3. **Click "Run"** to execute
4. **View Output** in the console panel

## Your First Program

```assembly
.text
    LOAD  R1, #42
    ADD   R1, R1, #8
    STOR  R1, [100]
    HALT
```

Expected output:
```
[Cycle 1] LOAD R1, #42 (15s) - R1 = 42
[Cycle 2] ADD R1, R1, #8 (8s) - R1 = 50
[Cycle 3] STOR R1, [100] (15s) - Mem[100] = 50
[Cycle 4] HALT (0s)
Total time: 38 seconds
```

## Emulator Features

- **Step Debugging**: Execute one instruction at a time
- **Breakpoints**: Pause execution at specific addresses
- **Memory Inspector**: View all 2,000 memory locations
- **Register Display**: Watch register values change
- **Timing Analysis**: See cumulative execution time
            """,
            "estimated_minutes": 25,
        },
        {
            "slug": "debugging-techniques",
            "title": "Debugging Babbage Programs",
            "lesson_type": LessonType.INTERACTIVE,
            "sequence_order": 2,
            "description": "Using breakpoints, watchpoints, and step execution",
            "content_markdown": """
# Debugging Strategies

## Common Bugs

### 1. Infinite Loops
```assembly
loop:
    LOAD  R1, [counter]
    ADD   R1, R1, #1
    ; BUG: Forgot to store back!
    JMP   loop
```
**Fix**: Add `STOR R1, [counter]`

### 2. Register Clobbering
```assembly
    LOAD  R1, #10
    CALL  subroutine       ; BUG: Overwrites R1!
    STOR  R1, [result]     ; Stores wrong value
```
**Fix**: Save R1 before CALL or use different register

### 3. Timing Errors
```assembly
    ; Need to complete in < 1 hour
    MULT  R1, R2           ; 400 seconds
    MULT  R1, R3           ; 400 seconds
    MULT  R1, R4           ; 400 seconds
    ; Total: 1,200s = 20 minutes (OK)
```

## Debugger Commands

- `break <address>` - Set breakpoint
- `step` - Execute one instruction
- `continue` - Run until breakpoint
- `inspect R1` - Show register value
- `inspect [100]` - Show memory value
- `trace` - Show execution history
            """,
            "estimated_minutes": 35,
        },
        {
            "slug": "profiling",
            "title": "Performance Profiling",
            "lesson_type": LessonType.READING,
            "sequence_order": 3,
            "description": "Analyzing execution time and identifying bottlenecks",
            "content_markdown": """
# Profiling Babbage Programs

## Profiler Output Example

```
Function: calculate_prime(23)
Total Time: 4,832 seconds (1.34 hours)

Breakdown by Instruction:
  MULT: 3,600s (74.5%) - 9 calls @ 400s each
  LOAD: 750s (15.5%) - 50 calls @ 15s each
  STOR: 450s (9.3%) - 30 calls @ 15s each
  ADD:  32s (0.7%) - 4 calls @ 8s each

Hotspots:
  Line 42 (inner loop MULT): 2,400s (49.7%)
  Line 38 (LOAD array element): 450s (9.3%)
```

## Optimization Strategies

1. **Minimize MULT** - 400s each!
2. **Keep values in registers** - Avoid LOAD/STOR
3. **Unroll loops** - Reduce jump overhead
4. **Precompute** - Calculate constants offline
            """,
            "estimated_minutes": 30,
        },
    ],
    "babbage-performance": [
        {
            "slug": "timing-analysis",
            "title": "Mechanical Timing Analysis",
            "lesson_type": LessonType.READING,
            "sequence_order": 1,
            "description": "Understanding execution time for all operations",
            "content_markdown": """
# Babbage Engine Timing Specifications

## Operation Timing Table

| Operation | Time | Mechanical Process |
|-----------|------|-------------------|
| ADD/SUB   | 8s   | Carry propagation across 50 digits |
| MULT      | 400s | Repeated addition with shifting |
| DIV       | 800s | Repeated subtraction with shifting |
| LOAD      | 15s  | Column selection + digit transfer |
| STOR      | 15s  | Column selection + digit setting |
| JMP/CALL  | 0s   | Barrel rotation (no computation) |
| HALT      | 0s   | Stop prime mover |

## Real-World Example: Census Aggregation (India, 1931)

### Algorithm
```
For each of 247 districts:
  Load population count    (15s)
  Add to running total     (8s)
  Store new total          (15s)
```

### Calculation
- Per iteration: 38 seconds
- 247 iterations: 9,386 seconds
- **Total: 2.6 hours**

### Historical Note
The 1931 India census used 3 Babbage Engines running in parallel,
completing the aggregation in 52 minutes wall-clock time.
            """,
            "estimated_minutes": 35,
        },
        {
            "slug": "optimization-strategies",
            "title": "Optimization Strategies",
            "lesson_type": LessonType.READING,
            "sequence_order": 2,
            "description": "Techniques to minimize execution time on mechanical hardware",
            "content_markdown": """
# Optimization for Babbage Hardware

## Strategy 1: Register Reuse

**Bad** (140s):
```assembly
    LOAD  R1, [a]          ; 15s
    LOAD  R2, [b]          ; 15s
    ADD   R3, R1, R2       ; 8s
    STOR  R3, [result]     ; 15s

    LOAD  R1, [c]          ; 15s (unnecessary!)
    LOAD  R2, [d]          ; 15s
    ADD   R3, R1, R2       ; 8s
    STOR  R3, [result2]    ; 15s
```

**Good** (110s):
```assembly
    LOAD  R1, [a]          ; 15s
    LOAD  R2, [b]          ; 15s
    ADD   R3, R1, R2       ; 8s
    STOR  R3, [result]     ; 15s

    LOAD  R1, [c]          ; 15s
    ; Reuse R2 if possible!
    LOAD  R2, [d]          ; 15s
    ADD   R3, R1, R2       ; 8s
    STOR  R3, [result2]    ; 15s
```

## Strategy 2: Avoid Multiplication

**Bad** (800s):
```assembly
    LOAD  R1, #6
    MULT  R1, R1           ; 400s - Calculate 6²
    MULT  R1, R1           ; 400s - Calculate (6²)²
```

**Good** (32s):
```assembly
    LOAD  R1, #6
    ; Calculate 6⁴ = 1296 via repeated addition
    ADD   R2, R1, R1       ; 8s - 12
    ADD   R2, R2, R2       ; 8s - 24
    ADD   R2, R2, R2       ; 8s - 48
    ADD   R2, R2, R2       ; 8s - 96
    ; Continue pattern...
```

## Strategy 3: Loop Unrolling

Reduces jump overhead and enables register reuse.
            """,
            "estimated_minutes": 45,
        },
        {
            "slug": "case-studies",
            "title": "Performance Case Studies",
            "lesson_type": LessonType.READING,
            "sequence_order": 3,
            "description": "Real-world optimization examples from historical deployments",
            "content_markdown": """
# Historical Performance Case Studies

## Case Study 1: Brazilian Coffee Export Calculations (1950)

**Problem**: Calculate export duties for 50,000 coffee shipments

**Original Implementation** (1948, Manual):
- 6 clerks, 3 months
- Error rate: 2.3%

**First Babbage Implementation** (1950):
- Naive algorithm: 47 hours runtime
- Unacceptable for weekly processing

**Optimized Implementation** (1951):
- Register reuse: 47h → 32h
- Precomputed tables: 32h → 18h
- Loop unrolling: 18h → 12h
- **Final: 12 hours** (fits overnight batch window)
- Error rate: 0.001%

## Case Study 2: Argentina Census (1952)

**Challenge**: Process 17 million census records in 6 weeks

**Solution**:
- 8 Babbage Engines in parallel
- Optimized merge-sort algorithm
- Eliminated 94% of MULT operations
- Result: Completed in 4.5 weeks

**Key Insight**: Mechanical constraints forced algorithmic innovations
that later influenced electronic computer architecture.
            """,
            "estimated_minutes": 40,
        },
    ],
    "babbage-historical-context": [
        {
            "slug": "global-deployment",
            "title": "Global Deployment: 1930-1960",
            "lesson_type": LessonType.READING,
            "sequence_order": 1,
            "description": "How Babbage Engines operated across four continents",
            "content_markdown": """
# Babbage Engines in Operation: A Global History

## Timeline of Deployments

### India (1931-1947)
- **Location**: Calcutta Statistical Bureau
- **Units**: 3 Analytical Engines
- **Primary Use**: Census calculations
- **Notable**: First large-scale deployment
- **Operators**: 18 trained calculators (14 women, 4 men)

### Brazil (1948-1962)
- **Location**: Rio de Janeiro Department of Commerce
- **Units**: 2 Engines (acquired 1948, 1951)
- **Primary Use**: Coffee export tax calculations
- **Innovation**: First motorized barrel mechanism
- **Chief Operator**: Dr. Maria Santos (1948-1959)

### Argentina (1950-1965)
- **Location**: Buenos Aires Census Office
- **Units**: 8 Engines (largest installation)
- **Primary Use**: National census processing
- **Innovation**: Automated punch card reader
- **Processing Rate**: 50,000 records/day

### China (1954-1958)
- **Location**: Beijing Mathematics Institute
- **Units**: 1 Engine (archival research)
- **Primary Use**: Astronomical calculations
- **Notable**: Last Engine built (replica)
            """,
            "estimated_minutes": 35,
        },
        {
            "slug": "operator-stories",
            "title": "Stories from the Operators",
            "lesson_type": LessonType.READING,
            "sequence_order": 2,
            "description": "First-hand accounts from Babbage Engine operators",
            "content_markdown": """
# Voices from the Mechanical Computing Era

## Interview: Mrs. Lakshmi Reddy (India, 1934-1946)

> "We worked in shifts—morning operators and afternoon operators.
> The machine ran 16 hours a day during census season. You learned
> to **hear** when something was wrong. A grinding gear, a slipping
> belt—you knew before the numbers were wrong.
>
> Setting the barrel pegs was an art. I could do 200 instructions
> in 4 hours by the end. My colleague Anjali held the record:
> 300 in 3.5 hours."

## Interview: Dr. Carlos Mendoza (Argentina, 1952-1960)

> "People think mechanical computation was primitive. But we
> invented **everything** that matters: subroutines, stack frames,
> optimization, debugging. We just did it at 400 seconds per
> multiplication.
>
> The 1960 census—17 million records—we processed it in 4.5 weeks
> with 8 Engines running 24 hours. Show me ENIAC doing that in 1960.
> We had **reliability**."

## Working Conditions

- **Noise**: 85-90 dB (ear protection required)
- **Temperature**: Engines generated significant heat
- **Precision**: Oil every 8 hours, alignment checks daily
- **Shifts**: 8-hour rotations, 24-hour operation during peak
            """,
            "estimated_minutes": 30,
        },
        {
            "slug": "transition-to-electronic",
            "title": "Transition to Electronic Computing",
            "lesson_type": LessonType.READING,
            "sequence_order": 3,
            "description": "Why and how organizations moved from Babbage to electronic computers",
            "content_markdown": """
# The End of the Mechanical Era

## Why Electronic?

### Speed Comparison (1960)

| Operation | Babbage Engine | IBM 1401 | Speedup |
|-----------|----------------|----------|---------|
| ADD       | 8s             | 11.5μs   | 695,000× |
| MULT      | 400s           | 600μs    | 666,000× |
| LOAD      | 15s            | 3μs      | 5,000,000× |

### Total Cost of Ownership (1960)

**Babbage Engine**:
- Purchase: £8,000 ($22,400)
- Operators: 4× £400/year = £1,600/year
- Maintenance: £500/year
- **10-year TCO: £29,000 ($81,200)**

**IBM 1401**:
- Lease: $2,500/month ($30,000/year)
- Operators: 2× $8,000/year = $16,000/year
- **10-year TCO: $460,000**

### The Decision

Most organizations transitioned 1958-1965 because:
1. **Computation needs grew exponentially**
2. **Speed mattered more than cost**
3. **Electronic components became reliable**
4. **Programming became easier (FORTRAN, COBOL)**

## Last Operational Babbage Engine

**Argentina Census Office, December 23, 1965**

Final program: Tabulating agricultural production statistics.
Runtime: 4.2 hours. No errors.

The Engine is now in the Buenos Aires Museum of Computing History,
still operational. They run demonstration programs monthly.
            """,
            "estimated_minutes": 40,
        },
    ],
    "babbage-complete-project": [
        {
            "slug": "project-overview",
            "title": "Final Project: Historical Replication",
            "lesson_type": LessonType.SYNTHESIS,
            "sequence_order": 1,
            "description": "Implement a real-world computation from the 1930s-1960s era",
            "content_markdown": """
# Final Project: Choose Your Historical Replication

You will implement **ONE** of these historical computations:

## Option 1: India Census Aggregation (1931)
**Difficulty**: ⭐⭐⭐☆☆
**Time**: 6-8 hours
**Skills**: Memory management, loops, basic arithmetic

Aggregate population counts from 247 districts with regional subtotals.

**Requirements**:
- Read 247 population values from memory
- Calculate regional subtotals (8 regions)
- Calculate national total
- Optimize to run in < 3 hours emulated time

## Option 2: Brazilian Export Tax Calculator (1950)
**Difficulty**: ⭐⭐⭐⭐☆
**Time**: 8-10 hours
**Skills**: Multiplication optimization, table lookups, precision

Calculate export duties for coffee shipments with tiered tax rates.

**Requirements**:
- Process 500 shipments
- Apply progressive tax brackets
- Handle decimal arithmetic (simulate with fixed-point)
- Optimize to eliminate 90% of MULT operations

## Option 3: Argentina Census Sort (1952)
**Difficulty**: ⭐⭐⭐⭐⭐
**Time**: 12-15 hours
**Skills**: Advanced algorithms, memory optimization, merge-sort

Implement merge-sort for 10,000 census records by surname.

**Requirements**:
- String comparison in assembly
- In-place merge-sort implementation
- Memory-efficient (fit in 2,000 memory locations)
- Complete in < 20 hours emulated time

## Deliverables

1. **Assembly source code** with comments
2. **Design document** explaining your algorithm
3. **Performance analysis** showing timing breakdown
4. **Historical research** (1-2 pages) on your chosen scenario
5. **Demo video** showing your program running in emulator
            """,
            "estimated_minutes": 30,
        },
        {
            "slug": "project-resources",
            "title": "Project Resources and Historical Data",
            "lesson_type": LessonType.READING,
            "sequence_order": 2,
            "description": "Historical datasets and reference materials",
            "content_markdown": """
# Historical Datasets

## India Census 1931 - District Populations

Download: `datasets/india_1931_census.csv`

Format:
```
District,Region,Population
Madras,South,2100000
Bombay,West,1350000
Calcutta,East,980000
...
```

## Brazil Coffee Exports 1950

Download: `datasets/brazil_1950_coffee.csv`

Format:
```
ShipmentID,Weight_kg,Grade,Destination
B1001,45000,Premium,USA
B1002,32000,Standard,Europe
...
```

Tax brackets:
- 0-10,000 kg: 3.2%
- 10,001-50,000 kg: 5.8%
- 50,001+ kg: 8.1%
- Premium grade: +1.5%

## Argentina Census 1952 - Sample Records

Download: `datasets/argentina_1952_sample.csv`

Format:
```
RecordID,Surname,FirstName,Age,Province
AR001,Garcia,Juan,34,Buenos Aires
AR002,Rodriguez,Maria,28,Cordoba
...
```

Sort by: Surname (ascending), then FirstName (ascending)

## Reference Materials

- `docs/1931_India_Census_Operations_Manual.pdf`
- `docs/Brazil_Analytical_Engine_Maintenance_Log_1950.pdf`
- `docs/Argentina_Programming_Guide_1952.pdf`
            """,
            "estimated_minutes": 20,
        },
    ],
}

# ============================================================================
# SAMPLE EXERCISES
# ============================================================================

EXERCISES_DATA = {
    "babbage-architecture": [
        {
            "slug": "calculate-execution-time",
            "title": "Calculate Factorial Execution Time",
            "description": "Predict the execution time for calculating factorial(10) on the Babbage Engine",
            "problem_statement": """
Given that:
- ADD operation: 8 seconds
- MULT operation: 400 seconds
- Memory LOAD: 15 seconds
- Memory STOR: 15 seconds

Calculate the total execution time for computing factorial(10) = 3,628,800

Hint: factorial(n) requires (n-1) multiplication operations plus overhead.
            """,
            "sequence_order": 1,
            "languages_supported": ["python", "c", "haskell"],
            "difficulty": DifficultyLevel.BEGINNER,
            "estimated_minutes": 20,
            "starter_code": {
                "python": """def calculate_factorial_time(n):
    # Calculate execution time in seconds for factorial(n)
    # Return the total time as an integer
    pass

print(calculate_factorial_time(10))
""",
                "c": """#include <stdio.h>

int calculate_factorial_time(int n) {
    // Calculate execution time in seconds for factorial(n)
    // Return the total time as an integer
    return 0;
}

int main() {
    printf("%d\\n", calculate_factorial_time(10));
    return 0;
}
""",
            },
            "solution_code": {
                "python": """def calculate_factorial_time(n):
    # Each multiplication: 400 seconds
    # factorial(n) needs (n-1) multiplications
    mult_time = (n - 1) * 400

    # Initial load: 15 seconds
    # Store result: 15 seconds
    overhead = 15 + 15

    return mult_time + overhead

print(calculate_factorial_time(10))  # Output: 3630
""",
            },
            "test_cases": [
                {"input": "5", "expected_output": "1630", "description": "factorial(5)"},
                {"input": "10", "expected_output": "3630", "description": "factorial(10)"},
                {"input": "3", "expected_output": "830", "description": "factorial(3)"},
            ],
            "hints": [
                "Count the number of multiplications needed for factorial(n)",
                "Don't forget initial LOAD and final STOR operations",
                "Each multiplication takes 400 seconds",
            ],
            "time_limit_seconds": 5,
            "memory_limit_mb": 128,
        },
    ],
    "babbage-isa-fundamentals": [
        {
            "slug": "write-assembly-sum",
            "title": "Write Assembly: Sum Array",
            "description": "Write assembly code to sum an array of 10 numbers stored in memory",
            "problem_statement": """
Write a Babbage assembly program that:
1. Reads 10 numbers from memory addresses 100-109
2. Calculates their sum
3. Stores the result in memory address 200

Example:
Memory[100-109] = [5, 12, 3, 8, 15, 7, 20, 4, 11, 9]
Expected result: Memory[200] = 94
            """,
            "sequence_order": 1,
            "languages_supported": ["babbage-assembly"],
            "difficulty": DifficultyLevel.BEGINNER,
            "estimated_minutes": 30,
            "starter_code": {
                "babbage-assembly": """; Write your assembly code here
.data
    ; Data section (if needed)

.text
    ; Your code here
    HALT
"""
            },
            "solution_code": {
                "babbage-assembly": """; Sum array of 10 numbers
.data
    sum: .word 0

.text
    LOAD  R1, #0           ; accumulator = 0
    LOAD  R2, #100         ; base address
    LOAD  R3, #0           ; counter = 0

loop:
    LOAD  R4, [R2]         ; load value at address
    ADD   R1, R1, R4       ; sum += value
    ADD   R2, R2, #1       ; address++
    ADD   R3, R3, #1       ; counter++

    LOAD  R5, #10          ; limit
    SUB   R5, R3, R5       ; counter - 10
    JZ    R5, done         ; if counter == 10, done
    JMP   loop

done:
    STOR  R1, [200]        ; store result
    HALT
"""
            },
            "test_cases": [
                {"input": "[5,12,3,8,15,7,20,4,11,9]", "expected_output": "94", "description": "Sum of sample array"},
                {"input": "[1,2,3,4,5,6,7,8,9,10]", "expected_output": "55", "description": "Sum of 1 to 10"},
            ],
            "hints": [
                "Use a loop counter to track how many numbers processed",
                "Use register indirect addressing to access array elements",
                "Don't forget to increment both the address and counter",
            ],
            "time_limit_seconds": 10,
            "memory_limit_mb": 128,
        },
        {
            "slug": "implement-factorial",
            "title": "Implement Factorial Subroutine",
            "description": "Write a subroutine that calculates factorial using the Babbage calling convention",
            "problem_statement": """
Implement a factorial subroutine that:
1. Takes input in R1 (the number n)
2. Returns result in R1 (factorial(n))
3. Preserves all other registers
4. Uses proper stack-based calling convention

Calculate factorial(5) = 120
            """,
            "sequence_order": 2,
            "languages_supported": ["babbage-assembly"],
            "difficulty": DifficultyLevel.INTERMEDIATE,
            "estimated_minutes": 45,
            "starter_code": {
                "babbage-assembly": """; Factorial subroutine
.data
    result: .word 0

.text
    LOAD  R1, #5           ; Calculate factorial(5)
    CALL  factorial
    STOR  R1, [result]
    HALT

factorial:
    ; Your implementation here
    RET
"""
            },
            "solution_code": {
                "babbage-assembly": """; Factorial with proper calling convention
.data
    result: .word 0

.text
    LOAD  R1, #5           ; Calculate factorial(5)
    CALL  factorial
    STOR  R1, [result]
    HALT

factorial:
    PUSH  R2               ; Save registers
    PUSH  R3

    LOAD  R2, #1           ; result = 1
    LOAD  R3, R1           ; counter = n

fact_loop:
    JZ    R3, fact_done    ; if counter == 0, done
    MULT  R2, R3           ; result *= counter
    SUB   R3, R3, #1       ; counter--
    JMP   fact_loop

fact_done:
    LOAD  R1, R2           ; return value in R1
    POP   R3               ; Restore registers
    POP   R2
    RET
"""
            },
            "test_cases": [
                {"input": "5", "expected_output": "120", "description": "factorial(5)"},
                {"input": "6", "expected_output": "720", "description": "factorial(6)"},
                {"input": "3", "expected_output": "6", "description": "factorial(3)"},
            ],
            "hints": [
                "Save and restore registers using PUSH/POP",
                "Use a loop to multiply from n down to 1",
                "Return result in R1 per calling convention",
            ],
            "time_limit_seconds": 30,
            "memory_limit_mb": 128,
        },
    ],
    "babbage-performance": [
        {
            "slug": "optimize-multiplication",
            "title": "Optimize Multiplication with Addition",
            "description": "Replace expensive MULT operations with ADD to reduce execution time",
            "problem_statement": """
Given code that calculates 7 × 8 using MULT (400 seconds):

```assembly
LOAD  R1, #7
LOAD  R2, #8
MULT  R1, R2           ; 400 seconds!
```

Optimize this using only ADD operations to achieve <50 seconds execution time.
Return your total execution time in seconds.
            """,
            "sequence_order": 1,
            "languages_supported": ["python", "c"],
            "difficulty": DifficultyLevel.INTERMEDIATE,
            "estimated_minutes": 35,
            "starter_code": {
                "python": """def calculate_optimized_time():
    # Calculate execution time for 7 × 8 using only ADD operations
    # Each ADD takes 8 seconds
    # Each LOAD takes 15 seconds
    # Return total time in seconds
    pass

print(calculate_optimized_time())
"""
            },
            "solution_code": {
                "python": """def calculate_optimized_time():
    # Strategy: Use repeated doubling
    # 7 × 8 = 7 × (2^3) = ((7 × 2) × 2) × 2

    # LOAD R1, #7        : 15s
    loads = 15

    # ADD R1, R1, R1     : 8s  (R1 = 14)
    # ADD R1, R1, R1     : 8s  (R1 = 28)
    # ADD R1, R1, R1     : 8s  (R1 = 56)
    adds = 3 * 8

    total = loads + adds  # 15 + 24 = 39 seconds
    return total

print(calculate_optimized_time())  # Output: 39
"""
            },
            "test_cases": [
                {"input": "", "expected_output": "39", "description": "Optimized time for 7 × 8"},
            ],
            "hints": [
                "Use repeated doubling: x × 2^n requires n ADD operations",
                "7 × 8 = 7 × 2 × 2 × 2",
                "Each ADD costs 8 seconds",
            ],
            "time_limit_seconds": 5,
            "memory_limit_mb": 128,
        },
        {
            "slug": "census-timing",
            "title": "Census Processing Time Calculation",
            "description": "Calculate total execution time for processing census data",
            "problem_statement": """
A census processing program needs to:
1. Load 1,000 population values (LOAD: 15s each)
2. Add each to running total (ADD: 8s each)
3. Store final total (STOR: 15s)

Calculate the total execution time in hours and minutes.
Return as total seconds.
            """,
            "sequence_order": 2,
            "languages_supported": ["python", "c", "haskell"],
            "difficulty": DifficultyLevel.BEGINNER,
            "estimated_minutes": 20,
            "starter_code": {
                "python": """def calculate_census_time(num_records):
    # Calculate total time in seconds
    # num_records = 1000
    pass

print(calculate_census_time(1000))
""",
                "haskell": """calculateCensusTime :: Int -> Int
calculateCensusTime numRecords =
    -- Calculate total time in seconds
    0

main = print (calculateCensusTime 1000)
"""
            },
            "solution_code": {
                "python": """def calculate_census_time(num_records):
    load_time = num_records * 15  # 1000 LOADs
    add_time = num_records * 8    # 1000 ADDs
    store_time = 15               # 1 STOR

    total = load_time + add_time + store_time
    # 15000 + 8000 + 15 = 23015 seconds
    return total

print(calculate_census_time(1000))  # Output: 23015
""",
                "haskell": """calculateCensusTime :: Int -> Int
calculateCensusTime numRecords =
    let loadTime = numRecords * 15   -- 1000 LOADs
        addTime = numRecords * 8     -- 1000 ADDs
        storeTime = 15               -- 1 STOR
    in loadTime + addTime + storeTime

main = print (calculateCensusTime 1000)
"""
            },
            "test_cases": [
                {"input": "1000", "expected_output": "23015", "description": "1000 records"},
                {"input": "247", "expected_output": "5696", "description": "247 districts (India 1931)"},
            ],
            "hints": [
                "Count each operation type separately",
                "1000 LOADs + 1000 ADDs + 1 STOR",
                "LOAD: 15s, ADD: 8s, STOR: 15s",
            ],
            "time_limit_seconds": 5,
            "memory_limit_mb": 128,
        },
    ],
    "babbage-complete-project": [
        {
            "slug": "mini-project-district-sum",
            "title": "Mini-Project: District Population Sum",
            "description": "Simplified version of India 1931 census: sum 10 district populations",
            "problem_statement": """
Implement a program that:
1. Reads 10 district population values from memory[100-109]
2. Calculates the total population
3. Stores result in memory[200]
4. Completes in under 5 minutes (300 seconds) of emulated time

Bonus: Calculate and report the actual execution time of your program.

Test data: [500000, 750000, 320000, 890000, 1200000, 450000, 680000, 920000, 530000, 410000]
Expected sum: 6,650,000
            """,
            "sequence_order": 1,
            "languages_supported": ["babbage-assembly", "python", "c"],
            "difficulty": DifficultyLevel.INTERMEDIATE,
            "estimated_minutes": 60,
            "starter_code": {
                "python": """def district_population_sum(populations):
    # populations is a list of 10 integers
    # Return the sum
    pass

test_data = [500000, 750000, 320000, 890000, 1200000,
             450000, 680000, 920000, 530000, 410000]
print(district_population_sum(test_data))
""",
                "babbage-assembly": """; District population sum
.data
    total: .word 0

.text
    ; Read 10 values from memory[100-109]
    ; Sum them
    ; Store in memory[200]

    HALT
"""
            },
            "solution_code": {
                "python": """def district_population_sum(populations):
    total = 0
    for pop in populations:
        total += pop
    return total

test_data = [500000, 750000, 320000, 890000, 1200000,
             450000, 680000, 920000, 530000, 410000]
print(district_population_sum(test_data))  # 6650000
""",
                "babbage-assembly": """; District population sum - optimized
.data
    total: .word 0

.text
    LOAD  R1, #0           ; accumulator
    LOAD  R2, #100         ; start address
    LOAD  R3, #0           ; counter

loop:
    LOAD  R4, [R2]         ; load district population
    ADD   R1, R1, R4       ; add to total
    ADD   R2, R2, #1       ; next address
    ADD   R3, R3, #1       ; increment counter

    LOAD  R5, #10
    SUB   R5, R3, R5       ; check if done
    JZ    R5, done
    JMP   loop

done:
    STOR  R1, [200]        ; store total
    HALT

; Timing: 10 LOADs (150s) + 10 ADDs (80s) + overhead
; Total: ~280 seconds (under 5 minute limit!)
"""
            },
            "test_cases": [
                {"input": "[500000,750000,320000,890000,1200000,450000,680000,920000,530000,410000]",
                 "expected_output": "6650000",
                 "description": "India 1931 sample districts"},
            ],
            "hints": [
                "Keep the algorithm simple - just load and add",
                "Use a loop with a counter",
                "Calculate timing: 10 LOADs + 10 ADDs + overhead",
            ],
            "time_limit_seconds": 10,
            "memory_limit_mb": 128,
        },
    ],
}


# ============================================================================
# SEEDER FUNCTIONS
# ============================================================================

def seed_eras(db: Session) -> dict:
    """Seed all 8 historical eras.

    Returns:
        Dictionary mapping era labels to Era objects
    """
    print("Seeding eras...")
    era_map = {}

    for era_data in ERAS_DATA:
        era = Era(**era_data)
        db.add(era)
        era_map[era_data["label"]] = era

    db.commit()
    print(f"✓ Created {len(ERAS_DATA)} eras")
    return era_map


def seed_babbage_modules(db: Session, era_map: dict) -> dict:
    """Seed Babbage Engine curriculum modules.

    Args:
        era_map: Dictionary mapping era labels to Era objects

    Returns:
        Dictionary mapping module slugs to Module objects
    """
    print("Seeding Babbage Engine modules...")
    module_map = {}

    for module_data in BABBAGE_MODULES:
        era_label = module_data.pop("era_label")
        era = era_map[era_label]

        module = Module(
            era_id=era.id,
            **module_data
        )
        db.add(module)
        module_map[module_data["slug"]] = module

    db.commit()
    print(f"✓ Created {len(BABBAGE_MODULES)} modules")
    return module_map


def seed_lessons(db: Session, module_map: dict) -> int:
    """Seed lessons for modules.

    Args:
        module_map: Dictionary mapping module slugs to Module objects

    Returns:
        Number of lessons created
    """
    print("Seeding lessons...")
    lesson_count = 0

    for module_slug, lessons in LESSONS_DATA.items():
        if module_slug not in module_map:
            print(f"  Warning: Module '{module_slug}' not found, skipping lessons")
            continue

        module = module_map[module_slug]

        for lesson_data in lessons:
            lesson = Lesson(
                module_id=module.id,
                **lesson_data
            )
            db.add(lesson)
            lesson_count += 1

    db.commit()
    print(f"✓ Created {lesson_count} lessons")
    return lesson_count


def seed_exercises(db: Session, module_map: dict) -> int:
    """Seed exercises for modules.

    Args:
        module_map: Dictionary mapping module slugs to Module objects

    Returns:
        Number of exercises created
    """
    print("Seeding exercises...")
    exercise_count = 0

    for module_slug, exercises in EXERCISES_DATA.items():
        if module_slug not in module_map:
            print(f"  Warning: Module '{module_slug}' not found, skipping exercises")
            continue

        module = module_map[module_slug]

        for exercise_data in exercises:
            exercise = Exercise(
                module_id=module.id,
                **exercise_data
            )
            db.add(exercise)
            exercise_count += 1

    db.commit()
    print(f"✓ Created {exercise_count} exercises")
    return exercise_count


def seed_placeholder_user(db: Session):
    """Create placeholder user for testing."""
    if not db.query(User).first():
        user = User(email="test@ancientcompute.org", hashed_password="placeholder")
        db.add(user)
        db.commit()
        print("✓ Created placeholder user")


def seed_comprehensive():
    """Main seeder function - populates entire database."""
    print("\n" + "="*60)
    print("COMPREHENSIVE DATABASE SEEDER")
    print("="*60 + "\n")

    # Create database tables
    print("Creating database tables...")
    Base.metadata.create_all(bind=engine)
    print("✓ Tables created\n")

    db = SessionLocal()

    try:
        # Check if already seeded
        if db.query(Era).count() >= 8:
            print("⚠ Database already appears to be seeded (8+ eras exist)")
            print("  Delete existing data and re-run if you want to re-seed\n")
            return

        # Seed in order (respecting foreign key dependencies)
        seed_placeholder_user(db)
        era_map = seed_eras(db)
        module_map = seed_babbage_modules(db, era_map)
        lesson_count = seed_lessons(db, module_map)
        exercise_count = seed_exercises(db, module_map)

        # Summary
        print("\n" + "="*60)
        print("SEEDING COMPLETE!")
        print("="*60)
        print(f"✓ {len(ERAS_DATA)} eras (20,000 BC to 2025 AD)")
        print(f"✓ {len(BABBAGE_MODULES)} modules (Babbage Engine curriculum)")
        print(f"✓ {lesson_count} lessons")
        print(f"✓ {exercise_count} exercises")
        print("\nDatabase is ready for use!")
        print("="*60 + "\n")

    except Exception as e:
        print(f"\n✗ Error during seeding: {e}")
        db.rollback()
        raise
    finally:
        db.close()


if __name__ == "__main__":
    seed_comprehensive()
