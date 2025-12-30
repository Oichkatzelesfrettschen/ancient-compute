Emulator Physics Validation
- Timing: verify Engine.clock_time increments match TIMING_TABLE per opcode.
- Arithmetic: MULT split into upper/lower digits; SQRT correctness; LOAD/STOR roundtrip.
- Run: make physics-validate; CI should fail on mismatch.
