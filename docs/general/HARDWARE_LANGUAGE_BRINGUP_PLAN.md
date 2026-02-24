# Hardware and Language Bring-up Plan

**Date**: 2026-02-24  
**Status**: Active  
**Owner**: Core compiler/runtime workstream  
**Scope**: Make the Babbage target usable by stabilizing machine-code bring-up before expanding language surface area.

## Decision Summary

We will use an assembly-first bring-up path:

1. Stabilize assembler and ISA behavior as the ground truth.
2. Define and enforce a deterministic ABI and runtime contract.
3. Bring up a freestanding C subset on top of that ABI.
4. Add higher-level language frontends only after C subset gates are green.

This avoids building frontends on unstable backend assumptions.

## Reality Baseline (2026-02-24)

Language service capability snapshot from `backend/src/services/languages/__init__.py`:

- `implemented`: 1 (`assembly`)
- `partial`: 4 (`c`, `python`, `haskell`, `lisp`)
- `stub`: 3 (`idris`, `systemf`, `java`)

Implication: backend roadmap and docs must treat assembly as the single stable execution anchor right now.

## Phase Gates

## Gate 0: ISA + Assembler Stability

Deliverables:

1. Instruction encoding/decoding invariants documented and tested.
2. Golden assembly corpus with deterministic hex outputs.
3. Error taxonomy for assembler diagnostics.

Exit criteria:

1. Assembler integration tests pass on every run.
2. No nondeterministic output for golden corpus.

## Gate 1: ABI and Runtime Contract

Deliverables:

1. Calling convention spec (args, return values, stack, preserved registers).
2. Minimal runtime model (startup, memory layout, syscall/IO boundary).
3. ABI conformance tests in backend unit/integration suites.

Exit criteria:

1. Assembly routines can call each other using documented ABI rules.
2. ABI tests fail on intentional contract violations.

## Gate 2: Freestanding C Subset

Deliverables:

1. Narrow C subset matrix (supported syntax/semantics).
2. C-to-IR-to-assembly pipeline working for subset programs.
3. No dependency on hosted libc assumptions.

Exit criteria:

1. Subset programs compile and execute under ABI/runtime contract.
2. Unsupported C features fail with explicit diagnostics.

## Gate 3: Structured Expansion

Deliverables:

1. Promote mature partial frontends one-by-one (`python`, `haskell`, `lisp`).
2. Keep `idris`, `systemf`, `java` marked as stubs until real execution paths exist.
3. Publish language readiness matrix in API and docs.

Exit criteria:

1. Each promoted service has contract tests and stable API behavior.
2. Documentation and API metadata stay synchronized.

## Implementation Rules

1. Do not mark a language as implemented without runtime-path evidence.
2. Keep `/api/v1/execute/languages` as the canonical machine-readable truth.
3. Favor small, verifier-backed increments over large speculative rewrites.
4. Archive superseded planning docs with successor mapping before deprecating.

## References (Primary Bring-up Guidance)

1. LLVM backend bring-up: <https://llvm.org/docs/WritingAnLLVMBackend.html>
2. LLVM codegen pipeline: <https://llvm.org/docs/CodeGenerator.html>
3. GCC backend internals: <https://gcc.gnu.org/onlinedocs/gccint/Back-End.html>
4. GCC machine descriptions: <https://gcc.gnu.org/onlinedocs/gccint/Machine-Desc.html>
5. GNU assembler guidance: <https://sourceware.org/binutils/docs/as/>
