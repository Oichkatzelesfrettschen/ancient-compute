# Freestanding C Subset Profile (Gate 2)

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Define the currently supported C subset for Babbage bring-up and enforce explicit diagnostics for unsupported features.

## Profile Scope

This profile applies to `backend/src/services/languages/c_service.py` in bring-up mode.

Goals:

1. Keep C target deterministic and ABI-aligned.
2. Reject unsupported hosted/advanced features with explicit messages.
3. Preserve a clear migration path for future expansion.

## Supported Subset (Current)

Based on current compiler/service behavior and tests:

1. Function definitions with scalar parameters.
2. Local scalar declarations and assignments.
3. Integer and float literals.
4. Arithmetic expressions (`+`, `-`, `*`, `/`).
5. Comparisons and conditionals (`if`).
6. Loops (`while`, `for`).
7. Function calls and scalar returns.

## Unsupported Features (Explicitly Diagnosed)

The service rejects these with deterministic diagnostics:

1. Preprocessor directives (`#include`, `#define`, etc.).
2. `struct`, `union`, `enum` declarations/usages.
3. `switch` statements.
4. `goto` statements.
5. Array declarators in C declarations.
6. Pointer declarators in C declarations.

## Diagnostic Contract

Unsupported input must fail with:

1. `compile_error` status.
2. Error text containing `Unsupported C feature(s) for freestanding subset`.
3. Feature-specific labels.
4. Pointer to this profile document.

## Gate 2 Notes

This profile is a bring-up boundary, not the final language target.
Any expansion must be staged behind tests and ABI/runtime compatibility checks.
