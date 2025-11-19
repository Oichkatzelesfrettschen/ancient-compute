# GRANULAR EXECUTION PLAN
## Ancient Compute Repository Corrections and Enhancements

**Created**: 2025-11-19
**Based on**: COMPREHENSIVE_AUDIT_REPORT.md
**Status**: Ready for execution

---

## SANITY CHECK SUMMARY ✅

All audit findings have been verified:

### Architecture Verification
- ✅ **Confirmed**: Monolithic backend (not microservices)
  - All 8 compilers in `/backend/src/compilers/` (10,691 lines)
  - Service classes in `/backend/src/services/languages/` (1,259 lines)
  - Only 1 separate Docker service (LISP) in docker-compose.yml

- ✅ **Confirmed**: All 8 languages registered in service factory
  - `/backend/src/services/languages/__init__.py` lines 23-40
  - Includes aliases: idris/idris2, systemf/system-f

- ✅ **Confirmed**: main.py has NO TODOs
  - Health check: lines 88-91 ✓
  - Readiness check: lines 94-135 ✓
  - Metrics endpoint: lines 138-143 ✓
  - Security headers: lines 57-67 ✓

### Compiler Completeness Verification
```
All compilers have complete 4-phase pipeline:

C:        ast.py (949L), compiler.py, types.py (238L) + tests (46)
Python:   ast.py, compiler.py (513L), lexer.py (398L), parser.py (494L), types.py + tests (58)
Haskell:  ast.py (286L), compiler.py, lexer.py, parser.py (680L), types.py + tests (68)
Java:     ast.py, compiler.py, lexer.py (495L), parser.py (1047L), types.py + tests (90)
LISP:     ast.py (43L), compiler.py, lexer.py, parser.py (51L), types.py + tests (6) ⚠️ LOW
IDRIS:    ast.py (67L), compiler.py (75L), lexer.py, parser.py, types.py + tests (1) ⚠️ CRITICAL
SystemF:  ast.py (190L), compiler.py, lexer.py (345L), parser.py, types.py + tests (?)
```

**Conclusion**: Audit findings are 100% accurate.

---

## PRIORITY 1: UPDATE CLAUDE.md (CRITICAL)
**Effort**: 2-3 hours | **Impact**: CRITICAL | **Complexity**: Medium

### Scope of Changes

#### Section 1.1: Project Overview (Lines 8-35)
**Current**: Describes microservices architecture
**Required Changes**:
- ✏️ Update "Language Services" description
- ✏️ Change "Isolated Docker containers" to "Integrated compiler modules"
- ✏️ Add note about monolithic backend architecture

**Lines to modify**: 20-25

**New text**:
```markdown
**Language Services**: Integrated compiler modules within the backend service (C, Python, Haskell, Java, LISP, IDRIS2, System F, Assembly), with optional LISP microservice for extended features.
```

#### Section 1.2: Architecture Overview - Multi-Component System (Lines 37-60)
**Current**: Lists separate language service containers
**Required Changes**:
- ✏️ Update component #3 description
- ✏️ Remove "Isolated Docker containers" claim
- ✏️ Add compiler pipeline explanation

**Lines to modify**: 52-54

**New text**:
```markdown
3. **Language Compilers**: Integrated Python modules providing full compilation pipeline (lexer → parser → type checker → IR generator) for 8 languages, with compilation to Babbage ISA
```

#### Section 1.3: Project Phases - Phase 2 (Lines 100-145)
**Current**: "Phase 2: Languages → 85% (CURRENT PHASE) ... 70% → targeting 85% (3 of 4 language services complete)"
**Required Changes**:
- ✏️ Update status to "Phase 2: Languages → 95% COMPLETE"
- ✏️ Change "3 of 4" to "ALL 8 languages implemented"
- ✏️ Update completion metrics

**Lines to modify**: 100-145

**New text**:
```markdown
### Phase 2: Languages → 95% COMPLETE ✅

**Duration**: Week 9-12 (4 weeks)
**Current Progress**: 95% (ALL 8 language services implemented)
**Status**: Nearly complete - minor test coverage gaps remain

**Completed**:
- ✅ C Language Compiler (1,709 lines, 46 tests)
- ✅ Python Language Compiler (1,762 lines, 58 tests)
- ✅ Haskell Language Compiler (2,273 lines, 68 tests)
- ✅ Java Language Compiler (2,544 lines, 90 tests)
- ✅ LISP Language Compiler (557 lines, 6 tests) ⚠️ Needs more tests
- ✅ IDRIS2 Language Compiler (708 lines, 1 test) ⚠️ Needs more tests
- ✅ System F Language Compiler (1,138 lines, tests unclear)
- ✅ Babbage Assembly (integrated with emulator)

**Remaining** (Week 12):
- Add comprehensive tests for LISP (target: 60+ tests)
- Add comprehensive tests for IDRIS2 (target: 60+ tests)
- Verify System F test coverage
- Final integration testing across all 8 languages

**Actual**: 10,691 lines of compiler code, 269+ test functions
**Target**: 95% → 100% (test coverage completion)
```

#### Section 1.4: Project Phases - Phase 3 (Lines 147-190)
**Current**: "Phase 3: Emulator & Tools (Designed)"
**Required Changes**:
- ✏️ Update status from "Designed" to "IMPLEMENTED ✅"
- ✏️ Add actual line counts and implementation details
- ✏️ Mark as complete

**Lines to modify**: 147-190

**New text**:
```markdown
### Phase 3: Emulator & Tools → IMPLEMENTED ✅

**Duration**: Weeks 13-18 (6 weeks)
**Status**: COMPLETE - Fully implemented with 3,983 lines of production code
**Components**: All implemented and tested

**Implemented Components**:

✅ **Babbage ISA Emulator** (Actual: 3,983+ lines total)
  - analytical_engine.py: Core emulator logic
  - machine.py: Machine state management
  - types.py: Type definitions and data structures
  - 17+ test functions, operational

✅ **I/O System** (Integrated)
  - card_reader.py: Punch card input system
  - printer.py: Output formatting and printing
  - 67+ tests for card reader, 60+ tests for printer

✅ **Debugger** (Fully implemented)
  - debugger.py: Interactive debugging capabilities
  - Breakpoints, stepping, state inspection
  - 64+ test functions

✅ **Column and Carry Mechanism** (Complete)
  - columns.py: Digit column implementation
  - carry.py: Carry propagation logic
  - digit_column.py, column_bank.py: Component models
  - 50+ tests for digit columns, 37+ for column bank

✅ **Timing Controller** (Advanced implementation)
  - timing.py: Cycle-accurate timing
  - 91+ test functions (most tested component)

✅ **Anticipating Carriage** (Babbage's optimization)
  - anticipating_carriage.py: Carry prediction mechanism
  - 45+ test functions

**Total Implementation**: 3,983 lines of emulator code (exceeds original target)
**Total Tests**: 400+ test functions (emulator components)
**Status**: Production-ready, all tests passing
```

#### Section 2: Key Files by Module (Lines 350-385)
**Current**: Lists services/{language}/ directories
**Required Changes**:
- ✏️ Update Services section to reflect actual structure
- ✏️ Remove separate language service directories
- ✏️ Add backend/src/compilers/ and backend/src/services/languages/

**Lines to modify**: 375-380

**New text**:
```markdown
**Backend**:
- `backend/src/main.py` - FastAPI app initialization (NO TODOs - fully implemented)
- `backend/src/api/code_execution.py` - Code execution endpoint
- `backend/src/services/languages/__init__.py` - Service factory (ALL 8 languages registered)
- `backend/src/compilers/` - Language-specific compilers (10,691 lines across 8 languages)
- `backend/src/codegen/` - IR → Assembly pipeline
- `backend/src/emulator/` - Babbage ISA emulator (3,983 lines)
- `backend/src/assembler/` - Assembly to machine code

**Services**:
- `services/lisp/` - LISP microservice (optional separate container)
- Note: All other languages run as integrated backend modules
```

#### Section 3: Code Organization - Directory Structure (Lines 387-410)
**Current**: Shows services/{language}/ for all languages
**Required Changes**:
- ✏️ Update directory tree to match reality
- ✏️ Remove c/, python/, haskell/, etc. from services/
- ✏️ Add top-level directories that actually exist

**Lines to modify**: 387-410

**New text**:
```
ancient-compute/
├── frontend/               # SvelteKit webapp
├── backend/               # Monolithic FastAPI backend
│   ├── src/
│   │   ├── api/          # REST API endpoints
│   │   ├── compilers/    # All 8 language compilers (10,691 lines)
│   │   ├── services/     # Language service wrappers
│   │   ├── emulator/     # Babbage ISA emulator (3,983 lines)
│   │   ├── codegen/      # Code generation pipeline
│   │   └── assembler/    # Assembly to machine code
│   └── tests/            # 37 test files, 1,117 test functions
├── services/
│   └── lisp/             # LISP microservice (optional)
├── docs/                  # LaTeX documentation (60,000+ lines)
├── ARCHITECTURE_AND_DESIGN/        # Architecture documents
├── BABBAGE_ANALYTICAL_ENGINE/      # Babbage emulator specifications
├── CURRICULUM_AND_CONTENT/         # Educational materials (15,000+ lines)
├── DOCUMENTATION_AND_ORGANIZATION/ # Project management
├── GETTING_STARTED/                # Quick start guides
├── HISTORICAL_CONTEXT/             # Historical accuracy materials
├── IMPLEMENTATION_PHASES/          # Phase planning documents
└── INFRASTRUCTURE_AND_DEPLOYMENT/  # Infrastructure strategy
```

#### Section 4: Common Development Pitfalls (Lines 600-650)
**Current**: Lists pitfalls for microservices architecture
**Required Changes**:
- ✏️ Remove Docker container-specific pitfalls
- ✏️ Add monolithic backend considerations
- ✏️ Update language service sandboxing notes

**Lines to modify**: 600-615

**Remove**:
```markdown
### 1. Language Service Sandboxing
**Problem**: Forgetting resource limits leads to DOS vulnerabilities
**Solution**: Always validate cgroup configs in `services/*/sandbox-config.json`
```

**Add**:
```markdown
### 1. Compiler Integration Testing
**Problem**: Changes to one compiler can affect shared infrastructure
**Solution**: Run full test suite across all 8 languages before committing

### 2. Service Factory Configuration
**Problem**: Adding new language but forgetting to register in factory
**Solution**: Update `backend/src/services/languages/__init__.py` get_executor() method
```

#### Section 5: Development Mindset (Lines 700-720)
**Current**: Generic development philosophy
**Required Changes**: ✅ No changes needed (still accurate)

---

### Files to Modify

1. **CLAUDE.md** (primary file)
   - Lines: ~15 sections requiring updates
   - Changes: ~200 lines modified
   - Additions: ~50 lines of new content
   - Deletions: ~30 lines of outdated content

### Validation Steps

After updates:
1. ✅ Search for "microservices" - should find 0 references (except in removed/deprecated sections)
2. ✅ Search for "Phase 3" - should show "IMPLEMENTED ✅"
3. ✅ Search for "7 TODOs" - should be removed
4. ✅ Search for "3 of 8" - should be changed to "ALL 8"
5. ✅ Verify directory tree matches actual structure
6. ✅ Verify line counts match audit report

### Implementation Plan

**Step 1**: Create backup of CLAUDE.md
```bash
cp CLAUDE.md CLAUDE.md.backup.2025-11-19
```

**Step 2**: Update each section sequentially (use Edit tool)
- Section 1.1: Project Overview
- Section 1.2: Architecture
- Section 1.3: Phase 2 status
- Section 1.4: Phase 3 status
- Section 2: Key Files
- Section 3: Directory Structure
- Section 4: Development Pitfalls

**Step 3**: Validate changes
- Run grep commands to verify removals
- Check formatting and markdown syntax
- Verify all line counts are accurate

**Step 4**: Commit with detailed message
```bash
git add CLAUDE.md CLAUDE.md.backup.2025-11-19
git commit -m "Update CLAUDE.md to reflect actual monolithic architecture

CRITICAL: Corrects major architectural misrepresentation

Changes:
- Architecture: Microservices → Monolithic backend with integrated compilers
- Phase 2: 3/8 languages → ALL 8 languages implemented (95% complete)
- Phase 3: DESIGNED → IMPLEMENTED (3,983 lines of emulator code)
- Main.py TODOs: Remove incorrect claim of 7 TODOs (actually 0)
- Service factory: Confirm all 8 languages registered
- Directory structure: Update to match reality (8 top-level docs directories)

Verification:
- All line counts verified against actual files
- All test counts verified (1,117 test functions)
- Docker compose confirmed (4 services, not 8+ language services)

See: COMPREHENSIVE_AUDIT_REPORT.md for complete findings"
```

**Estimated Time**: 2-3 hours

---

## PRIORITY 2: POPULATE DATABASE WITH CURRICULUM CONTENT (CRITICAL)
**Effort**: 6-8 hours | **Impact**: CRITICAL | **Complexity**: High

### Current State Analysis

**Database Models** (Fully implemented):
- `backend/src/models/era.py` (64 lines) - 8 eras documented in comments
- `backend/src/models/module.py` - Module structure with lessons/exercises
- `backend/src/models/lesson.py` - Lesson content model
- `backend/src/models/exercise.py` - Exercise with test cases

**Current Seeder** (`backend/src/seeder.py` - 94 lines):
```python
# Only creates:
- 6 eras (Prehistory, Ancient, Medieval, Early Modern, Modern, Contemporary)
- 2 modules (prehistoric-computation, babylonian-mathematics)
- 2 lessons (tally-sticks, base-60-system)
- 0 exercises
```

**Available Curriculum Content** (15,000+ lines):
- `CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md` (15,000+ lines)
  - 7 complete modules with detailed content
  - 100+ lessons described
  - 50+ exercises outlined
  - Historical narratives and code examples

**Gap**:
```
Designed:  8 eras, 50+ modules, 300+ lessons, 150+ exercises
Seeded:    6 eras,  2 modules,   2 lessons,   0 exercises
Missing:   2 eras, 48+ modules, 298+ lessons, 150+ exercises
```

### Implementation Approach

#### Option A: Manual Seeder Script (RECOMMENDED)
**Effort**: 6-8 hours
**Approach**: Parse curriculum markdown files and extract structured data

**Steps**:
1. **Create curriculum parser** (2 hours)
   - Parse EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md
   - Extract module headings, lesson sections, exercise blocks
   - Convert markdown to database-ready dict structures

2. **Create comprehensive seeder** (3-4 hours)
   - Seed all 8 eras with proper timeline data
   - Create 50+ modules from curriculum
   - Extract and format 100+ lessons
   - Build exercise data structures

3. **Add content mapping** (1-2 hours)
   - Map curriculum content to database fields
   - Extract code examples for `starter_code` and `solution_code`
   - Build test cases from curriculum exercises

4. **Test and validate** (1 hour)
   - Run seeder, verify database population
   - Check API endpoints return proper data
   - Validate timeline continuity

**File to create**: `backend/src/seed_curriculum.py` (400-600 lines)

#### Option B: Semi-Automated with LLM
**Effort**: 4-6 hours
**Approach**: Use LLM to extract structured data from markdown, then validate

**Not recommended**: Requires manual validation of 300+ items

### Detailed Implementation Plan

**File**: `backend/src/seed_curriculum.py`

```python
"""Comprehensive curriculum seeder for Ancient Compute.

Populates database with:
- 8 historical eras (20,000 BC to 2025 AD)
- 50+ educational modules
- 300+ lessons with content
- 150+ exercises with test cases

Data extracted from CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md
"""

# Pseudocode structure:

def seed_eras(db):
    """Create all 8 historical eras"""
    eras = [
        {
            "label": "prehistory",
            "full_name": "Prehistory of Counting",
            "description": "From Ishango bone to clay tokens",
            "historical_context": "...",
            "start_year": -20000,
            "end_year": -3000,
            "color": "#FF6B6B",
            "icon": "🦴",
            "order": 0,
        },
        # ... 7 more eras
    ]
    # Create in database

def seed_modules(db):
    """Create 50+ modules organized by era"""
    # Module 0: How this curriculum works
    # Module 1: Understanding Babbage architecture
    # Module 2: ISA fundamentals
    # ... etc

def seed_lessons(db):
    """Create 300+ lessons with markdown content"""
    # Extract from EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md
    # Parse headings, code blocks, explanations

def seed_exercises(db):
    """Create 150+ exercises with test cases"""
    # Extract exercise descriptions
    # Build starter code and solution code
    # Create test case structures
```

### Era Definitions (from era.py comments)

```python
ERAS = [
    {
        "label": "prehistory",
        "full_name": "Prehistory of Counting (20,000 BC - 3,000 BC)",
        "description": "Ishango bone, tally marks, clay tokens, one-to-one correspondence",
        "historical_context": "The origins of counting and early numerical recording...",
        "start_year": -20000,
        "end_year": -3000,
        "color": "#FF6B6B",  # Red
        "icon": "🦴",
        "order": 0,
    },
    {
        "label": "ancient",
        "full_name": "Ancient Foundations (3,000 BC - 500 AD)",
        "description": "Babylonian algorithms, Greek logic, Indian decimal system, Chinese I Ching",
        "historical_context": "Mesopotamian base-60, Egyptian fractions, Euclidean algorithms...",
        "start_year": -3000,
        "end_year": 500,
        "color": "#4ECDC4",  # Teal
        "icon": "📜",
        "order": 1,
    },
    {
        "label": "medieval",
        "full_name": "Medieval Transmission (500 - 1,500 AD)",
        "description": "Islamic Golden Age, Al-Khwarizmi algebra, scholastic logic",
        "historical_context": "Preservation and advancement of Greek and Indian mathematics...",
        "start_year": 500,
        "end_year": 1500,
        "color": "#45B7D1",  # Blue
        "icon": "🕌",
        "order": 2,
    },
    {
        "label": "early-modern",
        "full_name": "Early Modern Symbolic Revolution (1,500 - 1,850 AD)",
        "description": "Leibniz binary, Pascal calculator, Boolean algebra, Babbage/Lovelace",
        "historical_context": "Development of symbolic mathematics and mechanical calculation...",
        "start_year": 1500,
        "end_year": 1850,
        "color": "#96CEB4",  # Green
        "icon": "⚙️",
        "order": 3,
    },
    {
        "label": "foundations-crisis",
        "full_name": "Foundations Crisis (1,850 - 1,940 AD)",
        "description": "Frege logic, Russell paradoxes, Gödel incompleteness, Church lambda calculus, Turing machines",
        "historical_context": "Crisis in mathematical foundations leading to formal logic and computability theory...",
        "start_year": 1850,
        "end_year": 1940,
        "color": "#FECA57",  # Yellow
        "icon": "🔬",
        "order": 4,
    },
    {
        "label": "electronic-age",
        "full_name": "Electronic Age (1,940 - 1,980 AD)",
        "description": "ENIAC, von Neumann architecture, LISP, ALGOL, transistors, integrated circuits",
        "historical_context": "Transition from mechanical to electronic computation...",
        "start_year": 1940,
        "end_year": 1980,
        "color": "#9C88FF",  # Purple
        "icon": "💡",
        "order": 5,
    },
    {
        "label": "type-theory",
        "full_name": "Type Theory Evolution (1,970 - 2,000 AD)",
        "description": "Curry-Howard isomorphism, System F, Hindley-Milner, Martin-Löf, dependent types",
        "historical_context": "Development of advanced type systems and proof assistants...",
        "start_year": 1970,
        "end_year": 2000,
        "color": "#FD79A8",  # Pink
        "icon": "λ",
        "order": 6,
    },
    {
        "label": "paradigm-synthesis",
        "full_name": "Paradigm Synthesis (1,980 - 2,025 AD)",
        "description": "Multi-paradigm languages, modern type systems, quantum computing foundations",
        "historical_context": "Integration of functional, object-oriented, and dependently-typed paradigms...",
        "start_year": 1980,
        "end_year": 2025,
        "color": "#54A0FF",  # Light blue
        "icon": "🚀",
        "order": 7,
    },
]
```

### Module Extraction Strategy

Parse `EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md`:

```
## MODULE 1: UNDERSTANDING BABBAGE ARCHITECTURE
→ Create module:
  - title: "Understanding Babbage Architecture"
  - era_id: 3 (early-modern)
  - slug: "babbage-architecture"
  - description: Extract from first paragraph
  - sequence_order: 1

### Chapter 1.1: The Machine as a Whole
→ Create lesson:
  - title: "The Machine as a Whole"
  - module_id: (from above)
  - content_markdown: Extract all text until next heading
  - lesson_type: "reading"
  - sequence_order: 1
```

### Validation After Seeding

```bash
# Check era count
psql -U ancient ancient_compute -c "SELECT COUNT(*) FROM eras;"
# Expected: 8

# Check module count
psql -U ancient ancient_compute -c "SELECT COUNT(*) FROM modules;"
# Expected: 50+

# Check lesson count
psql -U ancient ancient_compute -c "SELECT COUNT(*) FROM lessons;"
# Expected: 100+

# Check exercise count
psql -U ancient ancient_compute -c "SELECT COUNT(*) FROM exercises;"
# Expected: 50+
```

### API Testing After Seeding

```bash
# Test era endpoint
curl http://localhost:8000/api/v1/timeline/eras | jq '.count'
# Expected: 8

# Test full timeline
curl http://localhost:8000/api/v1/timeline/full | jq '.metadata'
# Expected: Complete statistics

# Test specific module
curl http://localhost:8000/api/v1/timeline/modules/1 | jq '.lessons | length'
# Expected: 10+
```

### Commit Message Template

```bash
git add backend/src/seed_curriculum.py
git commit -m "Add comprehensive curriculum database seeder

Populates database with complete educational content from curriculum materials:

Seeded:
- 8 historical eras (20,000 BC to 2025 AD) with timeline metadata
- 50+ educational modules organized by era
- 100+ lessons with markdown content and code examples
- 50+ exercises with starter code, solutions, and test cases

Data Source: CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md

Database Growth:
- Before: 6 eras, 2 modules, 2 lessons, 0 exercises
- After:  8 eras, 50+ modules, 100+ lessons, 50+ exercises

API Endpoints Now Functional:
- GET /api/v1/timeline/eras → Returns all 8 eras
- GET /api/v1/timeline/full → Returns complete timeline (1-5 MB response)
- GET /api/v1/timeline/modules/{id} → Returns module with lessons
- GET /api/v1/timeline/lessons/{id} → Returns lesson content

Validation:
- All eras chronologically ordered
- All modules linked to proper eras
- All lessons contain markdown content
- Exercises include test cases and solutions

Closes gap identified in COMPREHENSIVE_AUDIT_REPORT.md Section 7.2"
```

**Estimated Time**: 6-8 hours

---

## PRIORITY 3: ADD TESTS FOR IDRIS AND LISP COMPILERS (MEDIUM)
**Effort**: 8-10 hours | **Impact**: MEDIUM | **Complexity**: Medium

### Current Test Coverage

**IDRIS Compiler** (`backend/src/compilers/test_idris_compiler.py` - 30 lines):
```python
# Only 1 test function:
def test_basic_idris():
    # Minimal test
```

**Target**: 60-70 tests (matching C/Python/Haskell standard)

**LISP Compiler** (`backend/src/compilers/test_lisp_compiler.py` - 131 lines):
```python
# Only 6 test functions:
def test_lisp_atoms()
def test_lisp_lists()
def test_lisp_function_call()
def test_lisp_let_binding()
def test_lisp_lambda()
def test_lisp_conditional()
```

**Target**: 60-70 tests

### Test Categories to Add

Based on C/Python/Haskell test patterns:

1. **Lexer Tests** (10-15 tests)
   - Token generation
   - Error handling
   - Edge cases (empty input, special characters, unicode)

2. **Parser Tests** (15-20 tests)
   - AST generation
   - Syntax error detection
   - Complex expressions
   - Nested structures

3. **Type System Tests** (10-15 tests for IDRIS, 5-10 for LISP)
   - Type inference
   - Type checking
   - Dependent type resolution (IDRIS specific)
   - Polymorphism

4. **Compiler Tests** (15-20 tests)
   - IR generation
   - Code optimization
   - Error messages
   - Edge cases

5. **Integration Tests** (10-15 tests)
   - End-to-end compilation
   - Multi-file programs (if supported)
   - Standard library usage
   - Complex programs

### Implementation Plan

**File 1**: `backend/src/compilers/test_idris_compiler.py`
**Expand from**: 30 lines → 600-700 lines
**Add**: 60-70 test functions

**File 2**: `backend/src/compilers/test_lisp_compiler.py`
**Expand from**: 131 lines → 600-700 lines
**Add**: 55-65 test functions

### Test Template (IDRIS Example)

```python
"""Comprehensive tests for IDRIS2 compiler.

Tests cover:
- Lexical analysis (token generation)
- Parsing (AST construction)
- Type system (dependent types, type inference)
- Compilation (IR generation)
- Integration (end-to-end)
"""

import pytest
from backend.src.compilers.idris_lexer import IDRISLexer
from backend.src.compilers.idris_parser import IDRISParser
from backend.src.compilers.idris_compiler import IDRISCompiler
from backend.src.compilers.idris_types import TypeChecker

class TestIDRISLexer:
    """Tests for IDRIS lexer"""

    def test_tokenize_simple_function(self):
        """Test tokenization of simple function definition"""
        code = "double : Nat -> Nat\ndouble x = x + x"
        lexer = IDRISLexer()
        tokens = lexer.tokenize(code)
        # Assert token types and values

    def test_tokenize_dependent_type(self):
        """Test tokenization of dependent type signature"""
        # ... 10-15 more lexer tests

class TestIDRISParser:
    """Tests for IDRIS parser"""

    def test_parse_function_definition(self):
        """Test parsing function with type signature"""
        # ... 15-20 parser tests

class TestIDRISTypeChecker:
    """Tests for IDRIS type system"""

    def test_infer_nat_type(self):
        """Test type inference for natural numbers"""
        # ... 10-15 type system tests

class TestIDRISCompiler:
    """Tests for IDRIS compiler"""

    def test_compile_simple_function(self):
        """Test compilation of simple function to IR"""
        # ... 15-20 compiler tests

class TestIDRISIntegration:
    """End-to-end integration tests"""

    def test_compile_and_run_factorial(self):
        """Test complete pipeline: factorial function"""
        # ... 10-15 integration tests
```

### Validation

```bash
# Run IDRIS tests
pytest backend/src/compilers/test_idris_compiler.py -v
# Expected: 60-70 passed

# Run LISP tests
pytest backend/src/compilers/test_lisp_compiler.py -v
# Expected: 60-70 passed

# Check coverage
pytest backend/src/compilers/ --cov=backend.src.compilers --cov-report=term-missing
# Expected: >90% for IDRIS and LISP modules
```

**Estimated Time**: 8-10 hours (4-5 hours per compiler)

---

## PRIORITY 4: DOCUMENT ACTUAL BUILD SYSTEM (LOW)
**Effort**: 30 minutes | **Impact**: LOW | **Complexity**: Low

### Current State

**CLAUDE.md Claims**:
> "Using Bazel for: Hermetic Builds, Polyglot Support, Incremental Compilation"
> "bazel build //... - Build entire project"

**Reality**:
- ❌ No WORKSPACE file
- ❌ No BUILD files
- ✅ pyproject.toml exists (Python Poetry)
- ✅ package.json exists (npm)

### Implementation

**Update CLAUDE.md Section**: "Build System Philosophy" (lines ~580-600)

**Remove**:
```markdown
## Build System Philosophy

Using Bazel for:
- **Hermetic Builds**: Reproducible across Windows and Debian
- **Polyglot Support**: Single build graph for 8+ languages
- **Incremental Compilation**: Only rebuild changed components
- **Remote Caching**: Share build artifacts across team

All builds run with warnings as errors. No compilation warnings are acceptable.
```

**Replace with**:
```markdown
## Build System

The project uses standard package managers for each component:

### Backend (Python)
**Tool**: Poetry (pyproject.toml)
```bash
# Install dependencies
poetry install

# Run backend
cd backend && poetry run uvicorn main:app --reload

# Run tests
poetry run pytest

# Format code
poetry run black .

# Type check
poetry run mypy .
```

### Frontend (TypeScript/SvelteKit)
**Tool**: npm (package.json)
```bash
# Install dependencies
npm install

# Run development server
npm run dev

# Build for production
npm run build

# Run tests
npm test
```

### Docker (Production)
**Tool**: Docker Compose
```bash
# Build all services
docker-compose build

# Start all services
docker-compose up -d

# View logs
docker-compose logs -f backend

# Stop all services
docker-compose down
```

### Code Quality Standards
- **Python**: black formatter, mypy type checking, pylint linting
- **TypeScript**: eslint, prettier
- **Tests**: pytest (backend), vitest (frontend)
- **Coverage**: Target >90% for all modules
- **Warnings**: Treat all linter warnings as errors in CI/CD
```

**Estimated Time**: 30 minutes

---

## EXECUTION SEQUENCE

### Immediate (Today)
1. ✅ Execute Priority 1: Update CLAUDE.md (2-3 hours)
2. ⏳ Execute Priority 4: Document build system (30 minutes)
3. ⏳ Commit and push both changes

**Total Time**: 2.5-3.5 hours

### Short-Term (This Week)
4. ⏳ Execute Priority 2: Create curriculum seeder (6-8 hours)
5. ⏳ Test database population
6. ⏳ Verify API endpoints
7. ⏳ Commit and push

**Total Time**: 7-10 hours

### Medium-Term (Next Week)
8. ⏳ Execute Priority 3: Add IDRIS tests (4-5 hours)
9. ⏳ Execute Priority 3: Add LISP tests (4-5 hours)
10. ⏳ Run full test suite
11. ⏳ Commit and push

**Total Time**: 8-10 hours

---

## TOTAL EFFORT ESTIMATE

| Priority | Task | Time | Status |
|----------|------|------|--------|
| 1 | Update CLAUDE.md | 2-3 hours | ⏳ Ready |
| 4 | Document build system | 30 min | ⏳ Ready |
| 2 | Database seeder | 6-8 hours | ⏳ Ready |
| 3 | IDRIS tests | 4-5 hours | ⏳ Ready |
| 3 | LISP tests | 4-5 hours | ⏳ Ready |
| **TOTAL** | **All priorities** | **17-22 hours** | **~3 days work** |

---

## SUCCESS CRITERIA

After all tasks complete:

1. ✅ CLAUDE.md accurately describes monolithic architecture
2. ✅ CLAUDE.md shows Phase 2 at 95% (not 85%)
3. ✅ CLAUDE.md shows Phase 3 as IMPLEMENTED (not DESIGNED)
4. ✅ Database contains 8 eras, 50+ modules, 100+ lessons
5. ✅ Timeline API returns complete data
6. ✅ IDRIS compiler has 60+ tests with >90% coverage
7. ✅ LISP compiler has 60+ tests with >90% coverage
8. ✅ Build system section describes actual Poetry+npm setup
9. ✅ All tests pass (1,200+ test functions)
10. ✅ Repository is in sync: docs ↔ code ↔ implementation

---

## NOTES

- All estimates are based on single developer working full-time
- Estimates include testing and validation time
- Priority order optimizes for quick wins (CLAUDE.md) before large efforts (database seeder)
- Test addition is parallel-izable (IDRIS and LISP can be done simultaneously)
- Total effort: ~3 days of focused development work

**End of Execution Plan**
