# Language Readiness Matrix

**Date**: 2026-03-20
**Status**: Updated after Gate 3 Language Promotion completion

## Gate Definitions

| Gate | Name | Scope | Criteria |
|------|------|-------|----------|
| Gate 0 | Assembler Stability | Babbage Assembly | Golden corpus deterministic, hex output matches |
| Gate 1 | ABI Conformance | Assembly + C | Register preservation, return convention, stack balance |
| Gate 2 | Freestanding C | C subset | Scalar core compiles; unsupported features rejected with diagnostics |
| Gate 3 | Language Promotion | All non-Java languages | Compile + execute round-trip; paradigm-specific contract |

## Language Readiness

| Language | Service | Compiler | Gate 0 | Gate 1 | Gate 2 | Gate 3 | Overall |
|----------|---------|----------|--------|--------|--------|--------|---------|
| Babbage Assembly | FULL | FULL | GREEN | GREEN | N/A | N/A | READY |
| C (Freestanding) | FULL | FULL | N/A | GREEN | GREEN | GREEN | READY |
| Python (Babbage IR) | FULL | FULL | N/A | N/A | N/A | GREEN | READY |
| Haskell | FULL | FULL | N/A | N/A | N/A | GREEN | READY |
| LISP (sbcl) | FULL | FULL | N/A | N/A | N/A | GREEN | READY |
| IDRIS2 (Babbage IR stub) | FULL | FULL | N/A | N/A | N/A | GREEN | READY* |
| System F (Babbage IR) | FULL | FULL | N/A | N/A | N/A | GREEN | READY |
| ALGOL68 (a68g 3.10.12) | FULL | N/A | N/A | N/A | N/A | GREEN | READY |
| C++ (g++ 15.2.1) | FULL | N/A | N/A | N/A | N/A | GREEN | READY |
| MicroPython 1.27.0 | FULL | N/A | N/A | N/A | N/A | GREEN | READY |
| Java | STUB | FULL | N/A | N/A | N/A | BLOCKED | NOT READY |

*IDRIS2: real execution (idris2 binary) deferred until AUR build completes;
currently uses Babbage IR compilation path.

## Gate Test Files

| Gate | Test File | Status | Count |
|------|-----------|--------|-------|
| Gate 0 | test_babbage_assembler_golden.py | PASSING | parametrized |
| Gate 1 | test_babbage_abi_contract.py | PASSING | 3 tests |
| Gate 2 | test_c_freestanding_subset.py | PASSING | 6 tests |
| Gate 2 | test_c_gate2_programs.py | PASSING | 20+ tests |
| Gate 3 | test_gate3_language_contracts.py | PASSING | 10 tests |
| Gate 3 | test_c_gate3_contract.py | PASSING | 6 tests |
| Gate 3 | test_python_gate3_contract.py | PASSING | 6 tests |
| Gate 3 | test_haskell_service.py | PASSING | 8 tests |
| Gate 3 | test_algol68_gate3_contract.py | PASSING | 6 tests |
| Gate 3 | test_cpp_gate3_contract.py | PASSING | 6 tests |
| Gate 3 | test_micropython_gate3_contract.py | PASSING | 6 tests |

## CI Tier Mapping

| Tier | Purpose | Command |
|------|---------|---------|
| A | Pre-merge smoke | `make test-active` |
| B | Scheduled regression | `pytest backend/tests/ --ignore=...` |
| C | Full regression | `pytest backend/tests/` |

## Notes

- **Java**: Compiler exists (java_compiler.py) but service returns stub message.
  JVM seccomp profile complexity defers this past Gate 3.  See DEFERRED_WORK.md.
- **LISP**: Now uses real SBCL 2.6.2 execution (sbcl --script). Upgraded from
  Babbage IR compilation path on 2026-03-20. Gate 3: 11/11 tests pass.
  Note: function names that clash with SB-ALIEN (e.g. 'double') must be avoided.
- **IDRIS2**: Babbage IR path is complete and Gate 3 passes.  The idris2 binary
  (AUR build) when available will upgrade to real execution in idris2_service.py.
- **Gate 0/1 are GREEN**: Assembler and ABI conformance tests pass.
- **Gate 2 is GREEN**: C freestanding subset end-to-end; 4 code-gen bugs fixed
  (emitter label addresses, trailing labels, comparison encoding, return terminator).
- **Gate 3 is GREEN**: 9 of 10 languages; Java deferred per DEFERRED_WORK.md.
