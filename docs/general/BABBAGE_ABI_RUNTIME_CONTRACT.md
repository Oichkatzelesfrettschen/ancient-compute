# Babbage ABI and Runtime Contract (Gate 1)

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Define the deterministic call/runtime contract that all frontends must target before broader language expansion.

## Scope

This contract is the Gate 1 baseline for the current Babbage bring-up path:

1. Calling convention and register roles.
2. Stack and control-flow behavior.
3. Runtime memory model.
4. Conformance test requirements.

## Register Contract

Registers exposed by the emulator/runtime:

1. `A`: primary accumulator and return-value register.
2. `B`: caller-saved scratch/secondary operand register.
3. `C`: callee-saved general register.
4. `D`: callee-saved general register (also used as remainder register for division paths).

Contract rules:

1. Callers must assume `A` and `B` are clobbered by callees.
2. Callees must preserve `C` and `D` when they modify them.
3. Function return values are delivered in `A`.

## Calling Convention

Control instructions:

1. `CALL target` pushes `PC + 1` to the return stack and transfers control.
2. `RET` pops one return address and transfers control back to caller.

Call-frame discipline:

1. Any callee use of `C` or `D` must be wrapped with save/restore (`PUSH`/`POP`) or equivalent.
2. Return stack depth is bounded (max 16), and overflow/underflow is a runtime contract violation.

## Runtime Memory Model

Current runtime/compiler memory partition (from allocator + emulator assumptions):

1. `0-255`: global/static data.
2. `256-511`: spill/stack working region.
3. `512-2047`: heap/scratch expansion region.

Notes:

1. The emulator also maintains a dedicated control return stack and data stack for `CALL`/`RET` and `PUSH`/`POP`.
2. Frontends targeting this ABI must not assume hosted libc/process services.

## Conformance Requirements

Gate 1 is considered passing when:

1. Positive ABI tests verify caller/callee interaction and register preservation.
2. Intentional ABI violations are detected by tests (register clobber, stack misuse, or return-stack misuse).
3. Tier A includes ABI conformance coverage.

## Reference Tests

Primary conformance suite:

1. `backend/tests/integration/test_babbage_abi_contract.py`
