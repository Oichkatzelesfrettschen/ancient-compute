# Language Readiness Matrix

**Date**: 2026-02-26
**Status**: Updated after Phase 10 audit

## Gate Definitions

| Gate | Name | Scope | Criteria |
|------|------|-------|----------|
| Gate 0 | Assembler Stability | Babbage Assembly | Golden corpus deterministic, hex output matches |
| Gate 1 | ABI Conformance | Assembly + C | Register preservation, return convention, stack balance |
| Gate 2 | Freestanding C | C subset | Scalar core compiles; unsupported features rejected with diagnostics |
| Gate 3 | Language Promotion | Haskell, LISP, IDRIS2, SystemF, Java | Compile + execute round-trip; paradigm-specific contract |

## Language Readiness

| Language | Service | Compiler | Gate 0 | Gate 1 | Gate 2 | Gate 3 | Overall |
|----------|---------|----------|--------|--------|--------|--------|---------|
| Babbage Assembly | FULL | FULL | GREEN | GREEN | N/A | N/A | READY |
| C (Freestanding) | FULL | FULL | N/A | GREEN | GREEN | N/A | READY |
| Python | FULL | FULL | N/A | N/A | N/A | DEFERRED | READY |
| Haskell | FULL | FULL | N/A | N/A | N/A | DEFERRED | PARTIAL |
| LISP | FULL | FULL | N/A | N/A | N/A | DEFERRED | PARTIAL |
| IDRIS2 | FULL | FULL | N/A | N/A | N/A | DEFERRED | PARTIAL |
| System F | FULL | FULL | N/A | N/A | N/A | DEFERRED | PARTIAL |
| Java | STUB | FULL | N/A | N/A | N/A | BLOCKED | NOT READY |

## Gate Test Files

| Gate | Test File | Status | Count |
|------|-----------|--------|-------|
| Gate 0 | test_babbage_assembler_golden.py | PASSING | parametrized |
| Gate 1 | test_babbage_abi_contract.py | PASSING | 3 tests |
| Gate 2 | test_c_freestanding_subset.py | PASSING | 6 tests |
| Gate 3 | (not yet created) | DEFERRED | -- |

## CI Tier Mapping

| Tier | Purpose | Command |
|------|---------|---------|
| A | Pre-merge smoke | `make test-active` |
| B | Scheduled regression | `pytest backend/tests/ --ignore=...` |
| C | Full regression | `pytest backend/tests/` |

## Notes

- **Java**: Compiler exists (java_compiler.py) but service returns stub message.
  Service layer needs to be wired to the compiler before Gate 3 can proceed.
- **Haskell/LISP/IDRIS2/SystemF**: All have working compilers and services.
  Gate 3 tests need to be written to validate paradigm-specific contracts:
  - Haskell: pattern matching, type inference, higher-order functions
  - LISP: S-expression evaluation, defun, recursive functions
  - IDRIS2: dependent types, pattern matching, type erasure
  - System F: type abstraction/application, parametric polymorphism
- **Gate 0/1 are GREEN**: Assembler and ABI conformance tests pass.
- **Gate 2 is GREEN**: C freestanding subset compiles scalar programs and
  rejects unsupported features with diagnostic messages.
