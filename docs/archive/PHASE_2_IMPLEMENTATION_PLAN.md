# Phase 2 Implementation Plan: Language Services & Simulator

## Archive Metadata

- Archive Status: archived
- Archive Reason: phase_completed_snapshot
- Canonical Successor: docs/general/MASTER_ROADMAP.md; docs/general/TODO_TRACKER.md
- Novel Content Integrated: yes
- Merged Into: docs/general/TODO_TRACKER.md
- Last Validated: 2026-02-24


**Date Created**: November 1, 2025
**Status**: READY FOR EXECUTION
**Duration**: Weeks 5-8 (plus simulator/BSD work in parallel)
**Overall Timeline**: Phase 2 = 8 weeks + Simulator = 10 weeks + BSD = 13 weeks (can parallelize)

---

## Executive Summary

Phase 2 implements the complete language execution infrastructure for ancient_compute. Three parallel workstreams execute simultaneously:

1. **Language Services** (Weeks 5-8): Implement 8 programming language sandboxes
2. **Simulator Development** (Weeks 1-10): Fork, enhance, and integrate cakenggt/analytical-engine
3. **BSD Integration** (Weeks 1-13): Device driver for 2.11BSD and DiscoBSD (concurrent track)

**Critical Path**: Language Services (Weeks 5-8) must complete before Week 9 (Content Management System)

---

## Part 1: Language Services Implementation (Weeks 5-8)

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│         FastAPI Backend (async orchestration)               │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  POST /execute ──→ ExecutionRouter                         │
│       ├─→ [C Service]        (Docker container 1)         │
│       ├─→ [Python Service]   (Docker container 2)         │
│       ├─→ [Haskell Service]  (Docker container 3)         │
│       ├─→ [IDRIS2 Service]   (Docker container 4)         │
│       ├─→ [LISP Service]     (Docker container 5)         │
│       ├─→ [Assembly Service] (Docker container 6)         │
│       ├─→ [Java Service]     (Docker container 7)         │
│       └─→ [System F Service] (Docker container 8)         │
│                                                             │
│  Each service returns: ExecutionResult {                   │
│    status: ExecutionStatus (enum)                          │
│    stdout: str                                             │
│    stderr: str                                             │
│    compile_output: Optional[str]                           │
│    execution_time: float                                   │
│    memory_used: int                                        │
│    exit_code: int                                          │
│  }                                                          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Week 5: C Language Service

**Status**: Needs implementation
**Dependencies**: docker-compose, base_executor framework
**Effort**: ~40 hours

#### Implementation Checklist

- [ ] **Docker Container Setup**
  - [ ] Create `services/c/Dockerfile` with GCC, Make, Valgrind
  - [ ] Install development headers (glibc-dev, linux-headers)
  - [ ] Setup seccomp-bpf profiles
  - [ ] Test container builds and runs

- [ ] **C Service Implementation** (`backend/src/services/languages/c_service.py`)
  - [ ] Extend `BaseExecutor` for C-specific logic
  - [ ] Implement compile phase (gcc with sanitizers)
  - [ ] Implement seccomp sandboxing
  - [ ] Add memory limit enforcement (via cgroups)
  - [ ] Add timeout handling (SIGALRM)
  - [ ] Implement Valgrind integration for memory checking
  - [ ] Test with sample C programs

- [ ] **Security Configuration**
  - [ ] Create seccomp profile (`services/c/seccomp.json`)
  - [ ] Block dangerous syscalls: open (except /tmp), network, ptrace
  - [ ] Allow safe syscalls: read, write, mmap, brk, exit
  - [ ] Test with malicious programs (fork bombs, etc.)

- [ ] **Testing**
  - [ ] Unit tests: compilation errors, runtime errors, timeouts
  - [ ] Memory leak detection (Valgrind)
  - [ ] Sandbox escape attempts (negative tests)
  - [ ] Performance benchmarks

**Code Location**: `/backend/src/services/languages/c_service.py`
**Docker Location**: `/services/c/Dockerfile`
**Tests Location**: `/backend/tests/unit/test_c_service.py`

---

### Week 6: Python & Haskell Services

#### Python Service (RestrictedPython)

**Status**: 17K partial implementation exists
**Effort**: ~25 hours (complete existing work)

Implementation checklist:

- [ ] **RestrictedPython Sandbox** (Already 70% done)
  - [ ] Verify safe_globals configuration
  - [ ] Test 50+ safe built-in functions
  - [ ] Verify import restrictions (only whitelist modules)
  - [ ] Test infinite loop prevention (via signal)
  - [ ] Test recursion limits

- [ ] **Python Service Completion**
  - [ ] Add mypy integration for type checking
  - [ ] Add pytest integration for solution validation
  - [ ] Implement output capture and sandboxing
  - [ ] Add timeout handling
  - [ ] Complete error reporting

- [ ] **Testing**
  - [ ] Run example programs from EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md
  - [ ] Test type checking accuracy
  - [ ] Test sandbox escape attempts (import, __import__, eval)
  - [ ] Performance tests

**Code Location**: `/backend/src/services/languages/python_service.py`
**Expected Result**: Full compliance with ExecutionResult interface

#### Haskell Service

**Status**: 1.3K stub
**Effort**: ~30 hours

Implementation checklist:

- [ ] **Docker Container**
  - [ ] Setup Haskell Platform or GHC + Cabal
  - [ ] Configure Stack build system
  - [ ] Pre-compile common dependencies (base, containers, etc.)
  - [ ] Setup sandboxing

- [ ] **Haskell Service Implementation**
  - [ ] Extend BaseExecutor for Haskell
  - [ ] Implement compile phase (ghc)
  - [ ] Add type checking (via compiler messages)
  - [ ] Implement QuickCheck property testing
  - [ ] Add memory limits
  - [ ] Timeout handling

- [ ] **Testing**
  - [ ] Test simple functions (fibonacci, factorial)
  - [ ] Test type errors (compile errors)
  - [ ] Test property-based testing
  - [ ] Test module imports

**Code Location**: `/backend/src/services/languages/haskell_service.py`

---

### Week 7: Advanced Language Services

#### IDRIS2 Service

**Status**: 1.3K stub
**Effort**: ~35 hours
**Note**: IDRIS2 enables formal correctness proofs

Implementation checklist:

- [ ] **Docker Container**
  - [ ] Install IDRIS2 compiler
  - [ ] Setup proof checking environment
  - [ ] Configure type checking

- [ ] **IDRIS2 Service**
  - [ ] Implement compilation and type-checking
  - [ ] Track proof obligations
  - [ ] Generate proof feedback
  - [ ] Implement resource limits

#### LISP Service (SBCL)

**Status**: 1.3K stub
**Effort**: ~30 hours

Implementation checklist:

- [ ] **Docker Container**
  - [ ] Install SBCL (Steel Bank Common Lisp)
  - [ ] Configure SLIME or similar
  - [ ] Setup sandboxing

- [ ] **LISP Service**
  - [ ] Implement REPL interface
  - [ ] Add macro expansion tracking
  - [ ] Implement output capture
  - [ ] Resource limits

#### Assembly Service (x86-64 via QEMU)

**Status**: 1.3K stub
**Effort**: ~35 hours

Implementation checklist:

- [ ] **Docker Container**
  - [ ] Install NASM assembler
  - [ ] Install QEMU user mode emulator
  - [ ] Setup minimal kernel for execution (or just raw execution)

- [ ] **Assembly Service**
  - [ ] Implement NASM compilation
  - [ ] Setup QEMU execution environment
  - [ ] Implement memory inspection (registers, memory layout)
  - [ ] Add instruction counting

#### Java Service

**Status**: 1.3K stub
**Effort**: ~30 hours

Implementation checklist:

- [ ] **Docker Container**
  - [ ] Install OpenJDK
  - [ ] Configure security manager

- [ ] **Java Service**
  - [ ] Compile .java files
  - [ ] Execute with sandboxing
  - [ ] Implement resource limits

---

### Week 8: System F & Orchestration

#### System F Service

**Status**: 1.3K stub (custom interpreter)
**Effort**: ~40 hours (complex language theory)

Implementation checklist:

- [ ] **Interpreter Implementation**
  - [ ] Implement type checking (System F with universal quantification)
  - [ ] Implement lambda reduction (beta reduction)
  - [ ] Add type inference
  - [ ] Performance optimization

- [ ] **Service Integration**
  - [ ] Expose via standard ExecutionResult
  - [ ] Implement visualization of type derivations
  - [ ] Add formal proof integration (if time permits)

#### Language Service Orchestration

**Status**: Partially implemented (`docker_manager.py` exists, needs completion)
**Effort**: ~20 hours

Implementation checklist:

- [ ] **Service Discovery**
  - [ ] Implement service registry (which containers are running?)
  - [ ] Health check for each container
  - [ ] Automatic restart on failure

- [ ] **Load Balancing**
  - [ ] Implement request queueing
  - [ ] Route requests to least-loaded service
  - [ ] Timeout handling if service down

- [ ] **Error Handling**
  - [ ] Unified error format across all languages
  - [ ] Translation of language-specific errors to ExecutionResult
  - [ ] Detailed error messages and stack traces

- [ ] **Performance Monitoring**
  - [ ] Track execution time per language
  - [ ] Monitor memory usage
  - [ ] Log slow requests (> 1 second)

- [ ] **Testing**
  - [ ] Chaos testing (stop containers, restart)
  - [ ] Load testing (10+ concurrent requests)
  - [ ] Integration testing (verify all 8 languages work end-to-end)

**Code Location**: `/backend/src/services/docker_manager.py`
**Test Location**: `/backend/tests/integration/test_orchestration.py`

---

## Part 2: Simulator Development (Weeks 1-10, Parallel Track)

### Phase 1: Repository Forking & Setup (Week 1-2)

**Task**: Clone, analyze, and prepare cakenggt/analytical-engine for enhancement

Checklist:

- [ ] Fork repository to ancient_compute/analytical-engine
- [ ] Review cakenggt's codebase structure
- [ ] Run existing tests and verify functionality
- [ ] Document existing instruction set (32 operations)
- [ ] Create development branch: `enhance/tier2-extensions`
- [ ] Setup build system (npm scripts, test framework)

**Effort**: ~16 hours

### Phase 2: Assembly Language Parser (Weeks 3-4)

**Task**: Enable assembly language instead of raw card format

Example assembly:
```assembly
LOAD [100] → A        # Load memory location 100 to Accumulator
ADD [101] → A         # Add location 101 to Accumulator
STORE A → [102]       # Store result to location 102
PUNCH [102]           # Output result to punch card
HALT                  # Stop
```

Checklist:

- [ ] Design assembly language grammar (EBNF)
- [ ] Implement parser (JavaScript/TypeScript)
- [ ] Implement assembler (assembly → card format)
- [ ] Add syntax validation
- [ ] Create test programs (fibonacci, factorial, prime)
- [ ] Unit tests for parser

**Effort**: ~40 hours
**Files**:
- `analytical-engine/src/AssemblyParser.js`
- `analytical-engine/src/Assembler.js`
- `analytical-engine/test/assembler.test.js`

### Phase 3: Debugger & Visualization (Weeks 5-6)

**Task**: Enable interactive debugging of Babbage programs

Features:

- [ ] **Breakpoints**: Set on instruction address or register value
- [ ] **Step Execution**: Single step, step over, run to breakpoint
- [ ] **Memory Watch**: Monitor specific memory addresses
- [ ] **Register Inspection**: Real-time display of Mill state
- [ ] **Execution Trace**: Log all operations
- [ ] **UI Dashboard**: WebSocket-based live updates

Checklist:

- [ ] Extend Engine class with debug hooks
- [ ] Create DebugUI component
- [ ] Implement memory inspector
- [ ] Implement breakpoint management
- [ ] Add WebSocket for live updates
- [ ] Create visualization dashboard

**Effort**: ~50 hours
**Files**:
- `analytical-engine/src/Debugger.js`
- `analytical-engine/web/debugger.html`
- `analytical-engine/web/debugger.js`

### Phase 4: Process/Scheduler Simulation (Weeks 7)

**Task**: Enable multi-process execution

Features:

- [ ] Multiple processes
- [ ] Context switching (save/restore state)
- [ ] Round-robin scheduling
- [ ] Process table management
- [ ] Inter-process communication (pipes)

Checklist:

- [ ] Create Scheduler class
- [ ] Implement process state management
- [ ] Implement context switching
- [ ] Add timing for context switches
- [ ] Test process switching

**Effort**: ~30 hours
**Files**:
- `analytical-engine/src/Scheduler.js`
- `analytical-engine/src/ProcessTable.js`

### Phase 5: I/O & Signal Support (Week 8)

**Task**: Expand I/O capabilities beyond cards

Features:

- [ ] File read/write
- [ ] Pipe simulation
- [ ] Signal handling (interrupts)
- [ ] Inter-process I/O

Checklist:

- [ ] Create FileIO module
- [ ] Implement pipe buffer
- [ ] Add signal queue
- [ ] Integrate with Node.js filesystem (web version uses IndexedDB)

**Effort**: ~30 hours

### Phase 6: Performance Profiler (Week 9)

**Task**: Timing analysis and bottleneck detection

Features:

- [ ] Operation timing (match Babbage specifications)
- [ ] Cycle counting
- [ ] Performance visualization
- [ ] Bottleneck detection

**Effort**: ~20 hours

### Phase 7: Testing & Validation (Week 10)

**Task**: Cross-validate against FourmiLab emulator and create test suite

Checklist:

- [ ] Create 10 standard test programs
- [ ] Run on both our simulator and FourmiLab
- [ ] Compare outputs (should be bit-identical)
- [ ] Compare timing (should be ±5%)
- [ ] Document any discrepancies
- [ ] Create comprehensive test suite

**Effort**: ~25 hours

**Timeline Summary**:
- Week 1-2: Forking & setup (16 hours)
- Week 3-4: Assembly parser (40 hours)
- Week 5-6: Debugger (50 hours)
- Week 7: Scheduler (30 hours)
- Week 8: I/O (30 hours)
- Week 9: Profiler (20 hours)
- Week 10: Testing (25 hours)
- **Total**: 211 hours (~10 weeks, 1 engineer)

---

## Part 3: BSD Integration (Weeks 1-13, Parallel Track)

### Implementation Timeline

| Phase | Task | Duration | Status |
|-------|------|----------|--------|
| 1 | Device driver skeleton | 2 weeks | READY |
| 2 | Babbage ALU implementation | 2 weeks | READY |
| 3 | Instruction decoder | 2 weeks | READY |
| 4 | Device ioctl operations | 1 week | READY |
| 5 | Testing & debugging | 2 weeks | READY |
| 6 | DiscoBSD 2.5 porting | 2 weeks | READY |
| 7 | Performance optimization | 1 week | READY |
| 8 | Documentation | 1 week | READY |
| **TOTAL** | | **13 weeks** | |

### Key Deliverables

- [ ] Device driver (`/dev/babbage`)
- [ ] System calls (load, run, status, reset)
- [ ] In-kernel ALU (50-digit BCD arithmetic)
- [ ] DiscoBSD 2.5 port (STM32F4 Discovery board)
- [ ] User-space C/C++ bindings
- [ ] Test suite
- [ ] Documentation

**Resources**: See `INFRASTRUCTURE_AND_DEPLOYMENT/BSD_INTEGRATION_SPEC.md` for complete specification and code examples.

---

## Critical Dependency Graph

```
Phase 2 Language Services (Weeks 5-8)
  ↓
  └─ Required for: Phase 3 (Content Management System, Week 9)
     Required for: Phase 4 (Interactive Components, Week 13)

Simulator Development (Weeks 1-10)
  ↓
  └─ Required for: SvelteKit timeline visualization (Week 14)
     Required for: Educational curriculum content (Weeks 9-12)

BSD Integration (Weeks 1-13)
  ↓
  └─ Optional for Weeks 1-12 (nice-to-have, doesn't block main features)
     Required for: Demo on physical hardware
```

**Critical Path**: Language Services MUST complete by end of Week 8 to unblock content management system.

---

## Resource Allocation (Assuming 1-2 Engineers)

### Parallel Execution Strategy

**Option A: Single Engineer** (13+ weeks)
- Weeks 5-8: Language Services (sequential)
- Weeks 9-10: Simulator (sequential)
- Weeks 11-13: BSD Integration (sequential)

**Option B: Two Engineers** (8 weeks)
- Engineer 1: Language Services (Weeks 5-8)
- Engineer 2: Simulator + BSD (parallel, Weeks 1-8)
- Week 8: Both engineers focus on integration and testing

**Recommended**: Option B for parallel execution and faster timeline

---

## Success Criteria

### Phase 2 Complete When:

**Language Services**:
- [ ] All 8 language services implement ExecutionResult interface
- [ ] All services have working Docker containers
- [ ] All services pass sandbox security tests
- [ ] Integration tests show all 8 languages working end-to-end
- [ ] Performance: Single request < 1 second (including Docker startup)

**Simulator**:
- [ ] cakenggt fork with all 5 tiers of enhancements
- [ ] Assembly language parser working
- [ ] Debugger UI functional
- [ ] Cross-validation with FourmiLab passed
- [ ] Comprehensive test suite (>90% code coverage)

**BSD Integration**:
- [ ] Device driver compiles without warnings
- [ ] Device file `/dev/babbage` functional
- [ ] DiscoBSD 2.5 port boots and runs Babbage programs
- [ ] System calls working (load, run, status, reset)
- [ ] Security tests passed (sandbox validated)

---

## Testing Strategy

### Phase 2A: Language Services Unit Tests

```python
# backend/tests/unit/test_c_service.py
def test_c_hello_world():
    service = CService()
    result = service.execute('#include <stdio.h>\nint main() { printf("hello"); return 0; }')
    assert result.status == ExecutionStatus.SUCCESS
    assert result.stdout == "hello"

def test_c_compile_error():
    service = CService()
    result = service.execute('int x = invalid;')
    assert result.status == ExecutionStatus.COMPILE_ERROR
    assert 'invalid' in result.stderr
```

### Phase 2B: Integration Tests

```python
# backend/tests/integration/test_all_languages.py
def test_fibonacci_across_all_languages():
    """Same algorithm in C, Python, Haskell, IDRIS2, LISP, Assembly, Java, System F"""
    programs = {
        'c': 'int fib(int n) { ... }',
        'python': 'def fib(n): ...',
        'haskell': 'fib n = ...',
        # ... etc for all 8
    }

    for lang, code in programs.items():
        result = execute_code(lang, code)
        assert result.status == ExecutionStatus.SUCCESS
        # All should produce identical output
```

### Phase 2C: Security Tests

```python
# backend/tests/security/test_sandbox_escape.py
def test_c_fork_bomb():
    """Verify fork bomb is prevented"""
    result = CService().execute('while(1) fork();')
    assert result.status in [ExecutionStatus.TIMEOUT, ExecutionStatus.SECURITY_VIOLATION]

def test_python_import_os():
    """Verify os module import blocked"""
    result = PythonService().execute('import os; os.system("rm -rf /")')
    assert 'not allowed' in result.stderr or result.status == ExecutionStatus.SECURITY_VIOLATION
```

---

## Milestones & Review Points

**End of Week 5**:
- C service implementation complete and tested
- All other services have stubbed Docker containers

**End of Week 6**:
- Python and Haskell services complete
- Assembly and IDRIS2 services 50% complete

**End of Week 7**:
- All 7 services complete (except System F)
- System F 50% complete

**End of Week 8**:
- All 8 services complete
- All pass integration tests
- Ready for content system (Phase 3)

**End of Week 10**:
- Simulator complete with all 5 tiers
- Cross-validation with FourmiLab passed

**End of Week 13**:
- BSD device driver complete
- DiscoBSD 2.5 port functional

---

## Agent Troubleshooting Notes

**Issue**: Agent API returned "tools: Tool names must be unique" error
**Root Cause**: Unknown (possible MCP configuration issue)
**Resolution**: Bypassed agents and proceeded with direct implementation planning
**Impact**: None - comprehensive specifications available in existing documents
**Recommendation**: Retry agent usage once Phase 2 implementation begins (may have been transient issue)

---

## Next Immediate Steps (Week 5 Begins)

1. **Monday**: Start C Language Service implementation
   - Create `/services/c/Dockerfile` with GCC + seccomp
   - Extend `BaseExecutor` for C-specific logic
   - Begin Docker container testing

2. **Wednesday**: Parallel work
   - Python service: complete RestrictedPython integration
   - Simulator: fork cakenggt/analytical-engine

3. **Friday**: First integration test
   - Can we execute a simple C program end-to-end?
   - Does Docker sandbox work?

---

**Document Status**: READY FOR EXECUTION
**Prepared By**: Claude Code (Multi-Document Analysis)
**Date**: November 1, 2025
**Approval Status**: Ready for Phase 2 implementation commencement
