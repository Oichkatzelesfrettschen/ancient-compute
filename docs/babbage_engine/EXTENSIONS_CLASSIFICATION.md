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

Tier D: Historical Contemporaries (IMPLEMENTED)
Independent reinventions of programmable computation, each with novel features.
- Scheutz Difference Engine (Sweden, 1851): 7-register 6th-order finite differences,
  first commercially manufactured difference engine. (backend/src/emulator/scheutz.py)
- Ludgate Analytical Machine (Ireland, 1909): 192-column store, "Irish logarithms"
  for multiplication, perforated cylinder control. (backend/src/emulator/ludgate.py)
- Torres y Quevedo (Spain, 1914-1920): first floating-point proposal, electromechanical
  relay registers, remote typewriter I/O. (backend/src/emulator/torres_quevedo.py)
- Zuse Z1 (Germany, 1938): first mechanical binary computer, 22-bit FP, 64-word
  memory, punched tape control. (backend/src/emulator/zuse_z1.py)

-------------------------------------------------------------------------------

Policy for emulator implementation
- Tier A: always enabled in core emulator
- Tier B: enabled behind a feature flag ("plausible extensions")
- Tier C: isolated into an optional module with explicit anachronism disclaimer
- Tier D: separate emulator modules in backend/src/emulator/; each follows the
  reset/state/step/run pattern and has a MachineAdapter in adapter.py

Rationale
- Tier A matches primary sources (Menabrea/Lovelace Notes, Babbage notebooks).
- Tier B is mechanically feasible with 1910s tooling but not explicitly documented.
- Tier C corresponds to 20th-century OS abstractions and should be treated as speculative.
