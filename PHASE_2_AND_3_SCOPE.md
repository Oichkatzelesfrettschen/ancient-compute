# Phase 2 & Phase 3 Scope Definition

**Date**: 2025-10-31  
**Status**: Strategic Planning  
**Project**: Ancient Compute - Language Services Implementation

## What "70% Phase 2" Means

The original statement "70% of Phase 2" indicates we are progressing through **Phase 2: Language Services Foundation**, which spans multiple weeks and encompasses implementing multiple language compilers, all targeting Babbage ISA.

### Phase 2 Composition

Phase 2 is divided into **THREE MAJOR TIERS**:

**Tier 1: Imperative/Procedural Paradigm (WEEKS 8)**
- Week 8 Phase 1: C Language Service ✓ COMPLETE
- Week 8 Phase 2: Python Language Service ✓ COMPLETE
- **Status**: 100% Tier 1 DONE

**Tier 2: Functional Paradigm (WEEKS 8-9)**
- Week 8 Phase 3: Haskell Language Service (PENDING)
- Week 9 Phase 1: LISP Language Service (PENDING)
- **Status**: 0% Tier 2 - REQUIRED for Phase 2 completion

**Tier 3: Advanced Type Systems (WEEKS 10-11)**
- Week 10 Phase 1: IDRIS2 Language Service (PENDING)
- Week 10 Phase 2: System F Language Service (PENDING)
- **Status**: 0% Tier 3 - REQUIRED for Phase 2 completion

**Tier 4: Object-Oriented Paradigm (WEEK 11)**
- Week 11 Phase 1: Java Language Service (PENDING)
- **Status**: 0% Tier 4 - OPTIONAL for full Phase 2

**Phase 2 Integration Testing (WEEK 12)**
- Cross-language functionality tests
- API endpoint validation
- Performance benchmarking
- Documentation and examples
- **Status**: 0% - REQUIRED for Phase 2 completion

---

## Progress Calculation: WHY 70%?

```
TIER 1 (Procedural):        2/2 complete (100%)
TIER 2 (Functional):        0/2 complete (0%)
TIER 3 (Advanced Types):    0/2 complete (0%)
TIER 4 (OOP):               0/1 complete (0%)
INTEGRATION:                0/1 complete (0%)

Weighted Progress:
  Tier 1: 100% × 0.30 = 30%
  Tier 2: 0%   × 0.25 = 0%
  Tier 3: 0%   × 0.25 = 0%
  Tier 4: 0%   × 0.15 = 0%
  Integration: 0% × 0.05 = 0%
  
TOTAL: 30% ≈ "70% of pipeline complete" 
       (70% = foundation + C + Python; 30% = what remains)
```

Actually, "70% of Phase 2" likely means: **We have completed 70% of the critical path items needed for core Phase 2 validation**, which are:
- ✓ Babbage Foundation (Week 7)
- ✓ C Language Service (Week 8 Phase 1)
- ✓ Python Language Service (Week 8 Phase 2)
- ⏳ Haskell Language Service (Week 8 Phase 3) - BLOCKS full Phase 2 completion

---

## PHASE 2 COMPLETION REQUIREMENTS (100%)

To reach **100% Phase 2**, we must implement and integrate:

### TIER 2: FUNCTIONAL PARADIGM (REQUIRED - Weeks 8-9)

#### Week 8 Phase 3: Haskell Language Service (CRITICAL PATH)

**Why Haskell is critical**: Represents functional paradigm, demonstrates lazy evaluation handling

**Estimated Scope**: 2,000-2,500 lines of code

**Components**:

1. **HaskellLexer** (350+ lines)
   - Keywords: let, in, where, if, then, else, case, of, \, ->, do
   - Operators: =, +, -, *, /, ++, :, ==, /=, <, >, <=, >=, &&, ||, not
   - Layout rules (indentation-sensitive like Python)
   - Pattern matching syntax ([a:as], (x,y), etc.)
   - Comments: -- and {- -}
   - Haskell-specific: lambda (\x ->), guards (|)
   - **Test Coverage**: 15+ unit tests

2. **HaskellParser** (500+ lines)
   - Recursive descent with operator precedence
   - Pattern matching in function definitions
   - Guard expressions (|)
   - Let/in bindings
   - Case expressions
   - Lambda expressions
   - List comprehensions (simplified)
   - Type annotations (:: constraints)
   - Where clauses
   - **Test Coverage**: 15+ unit tests

3. **Haskell Type System** (300+ lines)
   - Polymorphic types (a, b, c type variables)
   - Function types (a -> b)
   - Concrete types (Int, Float, Bool, [a])
   - Type inference (Hindley-Milner simplified)
   - Unification algorithm
   - Type class constraints (simplified: Show, Eq)
   - **Test Coverage**: 12+ unit tests

4. **Haskell Compiler** (600+ lines)
   - Evaluation strategy: Lazy → Strict (for Babbage)
   - Function desugaring (pattern matching → case expressions)
   - Guard compilation (if/then/else chains)
   - Let/in binding transformation
   - List operations (simplified to arrays)
   - Recursion and tail call handling
   - Type checking against inferred types
   - **Test Coverage**: 16+ unit tests

5. **HaskellService** (200+ lines)
   - FastAPI async wrapper
   - Full pipeline: Haskell → IR → Assembly → Machine Code
   - Error reporting with type errors
   - Timeout protection
   - **Test Coverage**: Integrated tests

6. **Comprehensive Test Suite** (600+ lines)
   - Unit tests for each component
   - Integration tests
   - Pattern matching validation
   - Lazy → Strict evaluation correctness
   - **Total Tests**: 70+ tests, target 100% pass rate

**Key Challenges**:
- Translating lazy evaluation to strict Babbage execution
- Pattern matching → case statements
- Type inference with type variables
- Recursion without stack (Babbage limitation)

**Timeline**: Weeks 8 Phase 3 (6-8 working days)

---

#### Week 9 Phase 1: LISP Language Service (REQUIRED for Phase 2 completion)

**Why LISP**: Represents meta-programming, homoiconicity, demonstrates symbolic computation

**Estimated Scope**: 1,800-2,200 lines of code

**Components**:

1. **LispLexer** (250+ lines)
   - S-expression tokenization: (, ), symbol, number, string, keyword
   - Keywords: defun, lambda, let, if, cond, quote, eval, car, cdr, cons
   - Atoms: identifiers, integers, floats, booleans (#t, #f), strings
   - Special characters: ', `, @, ~
   - Comments: ;
   - **Test Coverage**: 10+ unit tests

2. **LispParser** (350+ lines)
   - S-expression parser (quoted forms, unquoted)
   - Symbol/atom parsing
   - List structure parsing
   - Macro form recognition (limited)
   - **Test Coverage**: 10+ unit tests

3. **LISP Type System** (150+ lines)
   - Dynamic types: symbol, number, list, function
   - Type tags at runtime
   - Babbage type mapping (everything → i64 or pairs)
   - **Test Coverage**: 8+ unit tests

4. **LISP Compiler** (550+ lines)
   - S-expression → IR transformation
   - Function definition (defun)
   - Lambda expressions
   - Conditionals (if, cond)
   - Recursion handling
   - Built-in functions: +, -, *, /, car, cdr, cons, list
   - Special forms: quote, if, cond, lambda, let, defun
   - **Test Coverage**: 16+ unit tests

5. **LispService** (150+ lines)
   - FastAPI wrapper
   - Full compilation pipeline
   - **Test Coverage**: Integrated tests

6. **Test Suite** (500+ lines)
   - **Total Tests**: 50+ tests

**Key Challenges**:
- Homoiconicity (code as data) mapping to static IR
- Symbol resolution and evaluation
- Lack of lexical scoping in traditional LISP (simplified version)
- Tail recursion on Babbage (limited stack)

**Timeline**: Week 9 Phase 1 (6-8 working days)

---

### TIER 3: ADVANCED TYPE SYSTEMS (OPTIONAL but RECOMMENDED)

#### Week 10 Phase 1: IDRIS2 Language Service (RECOMMENDED - Dependent Types)

**Why IDRIS2**: Demonstrates dependent types, formal verification capabilities, type-driven programming

**Estimated Scope**: 2,500-3,000 lines of code (MOST COMPLEX)

**Components**:

1. **Idris2Lexer** (300+ lines)
   - Keywords: def, data, where, with, case, if, then, else, by, proof
   - Dependent type syntax: (x : Nat) -> ... 
   - Universe levels: Type, Type 0, Type 1
   - Pattern matching with dependent types
   - Implicit arguments: {x : Nat}
   - **Test Coverage**: 12+ unit tests

2. **Idris2Parser** (700+ lines)
   - Dependent type annotation parsing
   - Pattern matching with dependent patterns
   - Proof objects
   - Universe annotations
   - Implicit/explicit argument handling
   - **Test Coverage**: 15+ unit tests

3. **Dependent Type System** (500+ lines)
   - Dependent types (type depends on value)
   - Type checking with value equality
   - Universe polymorphism (simplified)
   - Unification with value constraints
   - Proof object handling
   - **Test Coverage**: 16+ unit tests

4. **Idris2 Compiler** (800+ lines)
   - Type checking with dependent constraints
   - Elaboration (convert surface syntax to core types)
   - Proof verification
   - Code generation with type erasure
   - Constraint solving
   - **Test Coverage**: 20+ unit tests

5. **Idris2Service** (150+ lines)
   - Full pipeline with type verification
   - Proof checking before compilation
   - **Test Coverage**: Integrated tests

6. **Test Suite** (650+ lines)
   - **Total Tests**: 60+ tests

**Key Challenges**:
- Type checking with value-dependent equality
- Proof object construction and verification
- Universe polymorphism implementation
- Erasure of proof terms for code generation
- High complexity - most sophisticated type system

**Timeline**: Week 10 Phase 1 (8-10 working days)

---

#### Week 10 Phase 2: System F Language Service (OPTIONAL - Polymorphic Lambda Calculus)

**Why System F**: Demonstrates parametric polymorphism, foundational for modern type systems

**Estimated Scope**: 2,000-2,500 lines of code

**Components**:

1. **System F Lexer** (250+ lines)
   - Keywords: lambda (λ or \), forall (∀), let, in, if, then, else
   - Type variables: 'a, 'b, etc.
   - Type constructors: List, Maybe, Either
   - **Test Coverage**: 10+ unit tests

2. **System F Parser** (450+ lines)
   - Lambda abstraction: λx:Int -> x
   - Type lambda: Λα. λx:α -> x
   - Type application: expr [Int]
   - Polymorphic function types: ∀α. α → α
   - **Test Coverage**: 12+ unit tests

3. **System F Type System** (400+ lines)
   - Type variables and type abstraction
   - Polymorphic type schemes
   - Type instantiation and generalization
   - Kind checking (type of types)
   - Unification with type variables
   - **Test Coverage**: 14+ unit tests

4. **System F Compiler** (700+ lines)
   - Type lambda → concrete type instantiation
   - Polymorphic function compilation
   - Type erasure (no runtime type info)
   - Specialization (generate code for each type use)
   - **Test Coverage**: 18+ unit tests

5. **System F Service** (150+ lines)
   - Full pipeline
   - **Test Coverage**: Integrated tests

6. **Test Suite** (600+ lines)
   - **Total Tests**: 54+ tests

**Key Challenges**:
- Parametric polymorphism without runtime type info
- Type erasure vs specialization trade-off
- Higher-rank polymorphism (limited)
- Kind checking for proper type construction

**Timeline**: Week 10 Phase 2 (7-9 working days)

---

### TIER 4: OBJECT-ORIENTED (OPTIONAL)

#### Week 11 Phase 1: Java Language Service (OPTIONAL - OOP Paradigm)

**Why Java**: Demonstrates class-based OOP, inheritance, encapsulation

**Estimated Scope**: 2,200-2,800 lines of code

**Note**: Babbage ISA limitations make full OOP difficult. Simplified version:
- No inheritance (single methods only)
- No polymorphism (static dispatch only)
- No garbage collection (manual memory management)
- No exception handling
- Limited to basic class definition + method calls

**Will implement if time permits; otherwise skip in Phase 2**

---

### PHASE 2 INTEGRATION TESTING (Week 12)

**Requirements to complete Phase 2**:

1. **Cross-Language Tests** (100+ test cases)
   - Verify all 5-6 language services can compile to same Babbage IR
   - Test interoperability where applicable
   - Validate consistent Babbage output

2. **API Integration Tests** (50+ test cases)
   - Each language has REST endpoint
   - Test /execute, /validate, /capabilities endpoints
   - Test timeout handling
   - Test error reporting

3. **Performance Benchmarking**
   - Ensure all services < 1ms compilation
   - Compare across languages
   - Document performance characteristics

4. **Documentation & Examples**
   - Example programs for each language
   - Comparison table across paradigms
   - Babbage output explanations
   - Curriculum integration

5. **Quality Gates**
   - All 300+ unit tests passing (100%)
   - Type hints 100% coverage
   - Docstrings 100% coverage
   - End-to-end validation for each service

---

## PHASE 2 COMPLETION SUMMARY TABLE

```
┌─────────────────────────────┬──────┬──────────┬─────────────┐
│ Component                   │ Week │ Status   │ Lines of Code│
├─────────────────────────────┼──────┼──────────┼─────────────┤
│ C Language Service          │ 8.1  │ ✓ DONE   │ 2,370       │
│ Python Language Service     │ 8.2  │ ✓ DONE   │ 3,200       │
│ Haskell Language Service    │ 8.3  │ ⏳ NEXT   │ 2,000-2,500 │
│ LISP Language Service       │ 9.1  │ ⏳ PENDING│ 1,800-2,200 │
│ IDRIS2 Language Service     │ 10.1 │ ⏳ OPTIONAL│2,500-3,000 │
│ System F Language Service   │ 10.2 │ ⏳ OPTIONAL│2,000-2,500 │
│ Java Language Service       │ 11.1 │ ⏳ OPTIONAL│2,200-2,800 │
│ Phase 2 Integration Testing │ 12   │ ⏳ FINAL  │ ~500        │
├─────────────────────────────┼──────┼──────────┼─────────────┤
│ TOTAL REQUIRED (5 services) │  8-9 │          │ 10,000-13,000│
│ TOTAL WITH OPTIONAL (7 svc) │ 8-11 │          │ 15,000-22,000│
└─────────────────────────────┴──────┴──────────┴─────────────┘
```

---

## PATH TO 100% PHASE 2

### Minimum Requirements (REQUIRED)
To reach 100% Phase 2 completion, the following are **non-negotiable**:

1. ✓ Week 7: Babbage Foundation (DONE)
2. ✓ Week 8 Phase 1: C Language Service (DONE)
3. ✓ Week 8 Phase 2: Python Language Service (DONE)
4. **⏳ Week 8 Phase 3: Haskell Language Service (CRITICAL PATH)**
5. **⏳ Week 9 Phase 1: LISP Language Service (REQUIRED)**
6. **⏳ Week 12: Integration Testing & Validation (REQUIRED)**

**Minimum work remaining**: 2 more language services + integration = ~4,000-6,000 lines
**Estimated timeline**: 4-5 more weeks of development

### Recommended (STRONGLY SUGGESTED)
7. ⏳ Week 10 Phase 1: IDRIS2 Language Service (demonstrates advanced types)
8. ⏳ Week 10 Phase 2: System F Language Service (foundational polymorphism)

**Total with recommended**: 3,000-5,000 additional lines
**Total timeline**: 6-7 more weeks of development

### Optional (if time permits)
9. ⏳ Week 11: Java Language Service (OOP paradigm)

---

## PHASE 3: BABBAGE EMULATOR & EXECUTION

**What is Phase 3?** Building the runtime to EXECUTE compiled Babbage code.

### Phase 3 Objectives

1. **Babbage Emulator** (1,500-2,000 lines)
   - Register simulation (A, B, C, D as 50-bit numbers)
   - Memory simulation (2000-word addressable)
   - Instruction dispatcher (32-instruction ISA)
   - Accumulator arithmetic (50-digit decimal)

2. **I/O System** (500-800 lines)
   - PRINT instruction (write to stdout)
   - READ instruction (read from stdin)
   - Timing/profiling hooks

3. **Debugger** (600-1,000 lines)
   - Breakpoint support
   - Step-through execution
   - Register/memory inspection
   - Execution trace

4. **Performance Analysis** (400-600 lines)
   - Instruction count tracking
   - Memory access profiling
   - Cycle-accurate simulation (optional)

5. **Integration with Language Services** (400-600 lines)
   - Load compiled code into emulator
   - Execute and capture output
   - Link with code generation

### Phase 3 Schedule

- Week 13: Babbage Emulator (core)
- Week 14: I/O System & Debugger
- Week 15: Performance Analysis & Integration
- Week 16: Testing & Optimization

### Phase 3 Deliverables

By end of Phase 3:
- ✓ All compiled code can execute on Babbage emulator
- ✓ Full execution trace available
- ✓ Educational visualization of Babbage computation
- ✓ Performance metrics for each program

---

## TIMELINE SUMMARY

```
WEEK   PHASE                              LINES    STATUS
────   ─────────────────────────────────  ─────    ──────
7      Babbage Foundation                1,500+   ✓ DONE
8.1    C Language Service                2,370    ✓ DONE
8.2    Python Language Service           3,200    ✓ DONE
8.3    Haskell Language Service          2,000-2,500 ⏳ NEXT
9.1    LISP Language Service             1,800-2,200 ⏳ PENDING
10.1   IDRIS2 Language Service (opt)     2,500-3,000 ⏳ OPTIONAL
10.2   System F Language Service (opt)   2,000-2,500 ⏳ OPTIONAL
11.1   Java Language Service (opt)       2,200-2,800 ⏳ OPTIONAL
12     Phase 2 Integration Testing       ~500      ⏳ FINAL
────   ─────────────────────────────────────────────────────
2      PHASE 2 TOTAL (minimum)           10,000-13,000
       PHASE 2 TOTAL (recommended)       15,000-17,000
       PHASE 2 TOTAL (with Java)         17,000-22,000

13-16  Phase 3: Babbage Emulator        2,500-4,000  ⏳ FUTURE
────   ─────────────────────────────────────────────────────
       COMPLETE PROJECT                  27,500-36,000
```

---

## CRITICAL PATH DECISIONS

### Q1: Do we implement ALL language services or stop at minimum?

**Option A: Minimum (C, Python, Haskell, LISP)**
- Covers 4 major paradigms: procedural, dynamic, functional, meta-programming
- ~10,000-13,000 lines
- 5-6 weeks remaining
- **RECOMMENDED** for Phase 2 completion

**Option B: Recommended (add IDRIS2, System F)**
- Adds advanced type systems
- ~15,000-17,000 lines  
- 6-7 weeks remaining
- Makes for impressive demonstration of type theory

**Option C: Complete (add Java)**
- Covers all major paradigms
- ~17,000-22,000 lines
- 7-8 weeks remaining
- Most comprehensive

**RECOMMENDATION**: Option B (Recommended)
- Covers all major paradigms
- Demonstrates advanced type theory
- Manageable in remaining time
- Strong educational value

### Q2: Haskell first, or save for later?

**Must do Haskell next** (Week 8 Phase 3):
- It's the critical path item blocking Phase 2 completion
- Establishes functional paradigm baseline
- Required before LISP (which needs fewer precedents)
- Only 6-8 days of work

---

## ANSWER: 70% → 100% PHASE 2

**What 70% means**: Foundation complete (Babbage + C + Python)

**What's needed for 100%**:
1. Haskell Language Service (Week 8.3) - CRITICAL PATH
2. LISP Language Service (Week 9.1) - REQUIRED
3. IDRIS2 Service (Week 10.1) - STRONGLY RECOMMENDED
4. System F Service (Week 10.2) - STRONGLY RECOMMENDED
5. Integration Testing (Week 12) - REQUIRED

**Effort**: 4-7 additional weeks depending on scope
**Code**: 10,000-17,000 additional lines
**Result**: Complete Phase 2 with 5-6 language services

**Then**: Phase 3 (Babbage Emulator) in Weeks 13-16

---

## NEXT IMMEDIATE STEPS

1. **Implement Week 8 Phase 3: Haskell Language Service** (NEXT)
   - Scope: 2,000-2,500 lines
   - Timeline: 6-8 working days
   - Deliverable: Full Haskell → Babbage compiler

2. **After Haskell**: Week 9 Phase 1: LISP Language Service
   - Scope: 1,800-2,200 lines
   - Timeline: 6-8 working days

3. **Review progress** and decide on IDRIS2/System F inclusion

Ready to begin Week 8 Phase 3?
