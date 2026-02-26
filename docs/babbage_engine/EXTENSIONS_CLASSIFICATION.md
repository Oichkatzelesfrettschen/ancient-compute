# Babbage Extensions Classification

Purpose: distinguish historically documented capabilities from plausible extensions and anachronisms.
Sources: docs/babbage_engine/BABBAGE_CRITICAL_REVIEW.md and whitepaper notes.

-------------------------------------------------------------------------------

Tier A: Historical (documented in Babbage/Lovelace)
- Arithmetic: ADD, SUB, MUL, DIV
- Compare and conditional branching (CMP + conditional jumps)
- Memory load/store (Store <-> Mill)
- Card I/O: operation cards, variable cards, number cards; read/punch/print
- Program control via barrel (looping via peg patterns)

Tier B: Plausible 1910s mechanical extensions (IMPLEMENTED)
- CALL/RET (subroutine sequencing by barrel position stack)
- PUSH/POP (data stack in store)
- SHL/SHR (decimal digit shifts via gear train)
- CHECKSUMS (simple error detection using arithmetic)
- CLEAR/NEG/ABS/MOD (basic arithmetic convenience ops)

Tier C: Speculative / anachronistic (PARTIALLY IMPLEMENTED)
- Bitwise logic ops (AND/OR/XOR) as built-in instructions (IMPLEMENTED)
- Lovelace Music Engine (PLAY) (IMPLEMENTED)
- Symbolic Mode (SETMODE) (IMPLEMENTED)
- Unix-like processes, scheduling, and context switching (PLANNED)
- Pipes and IPC mechanisms (PLANNED)
- Filesystem abstraction, file metadata table, and syscalls (PLANNED)
- Signals/interrupts, device drivers (PLANNED)

-------------------------------------------------------------------------------

Policy for emulator implementation
- Tier A: always enabled in core emulator
- Tier B: enabled behind a feature flag ("plausible extensions")
- Tier C: isolated into an optional module with explicit anachronism disclaimer

Rationale
- Tier A matches primary sources (Menabrea/Lovelace Notes, Babbage notebooks).
- Tier B is mechanically feasible with 1910s tooling but not explicitly documented.
- Tier C corresponds to 20th-century OS abstractions and should be treated as speculative.
