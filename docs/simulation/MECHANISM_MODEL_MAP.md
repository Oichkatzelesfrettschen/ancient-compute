# Mechanism -> Model Map

Purpose: map each mechanism to a concrete emulator model and testable outputs.

Format
- State: core variables that define the mechanism at any time
- Ops: minimal operations required for deterministic simulation
- Output: expected observable result for tests

Tally marks / tally sticks
- State: multiset of marks per ledger line
- Ops: add_mark, remove_mark, sum
- Output: integer totals, diff between ledgers

Clay tokens / bullae
- State: sealed bag (token multiset) + imprint ledger
- Ops: add_token, seal, open, verify_imprint
- Output: verification pass/fail, count by type

Counting rods / abacus
- State: columnar digit array, base-10 carry flags
- Ops: add, sub, mul (repeated add), div (repeated sub)
- Output: digit columns + carry history

Quipu / khipu
- State: cord tree, knot type/position encoding
- Ops: encode_number, decode_number, aggregate
- Output: decoded values and ledger totals

Antikythera mechanism
- State: gear angles per train, dial pointers
- Ops: advance_days, read_dial
- Output: expected dial positions for known dates

Astrolabe
- State: rete + plate rotation, latitude setting
- Ops: set_latitude, rotate_time, read_altitude
- Output: altitude/time readings vs reference table

Slide rule
- State: log-scale slider positions
- Ops: multiply, divide (via add/sub logs)
- Output: approximate results with error bounds

Pascaline
- State: wheel digits + carry levers
- Ops: increment_digit, add
- Output: displayed digits, carry trace

Leibniz stepped reckoner
- State: stepped drum position, carriage offset, accumulator
- Ops: add, sub, mul (repeated add with carriage shift), div
- Output: accumulator + cycle count

Jacquard loom
- State: card deck, hook states, row output
- Ops: read_card, weave_row
- Output: pattern rows (binary grid)

Babbage Analytical Engine (base)
- State: store columns, mill registers, barrel/counter
- Ops: execute_instruction, load/store, branch
- Output: register/store values + trace

Ada Lovelace Bernoulli (Note G)
- State: Analytical Engine + Note G deck counters
- Ops: program run with looped deck
- Output: Bernoulli numbers B1..Bn

Difference Engine No. 2 (DE2)
- State: column values, carry flags, crank step
- Ops: step, propagate_carry
- Output: tabulated polynomial values

Anachronistic / unconventional
- Cellular automata: grid + step rule, output next grid
- DNA computing: reaction multiset, output concentrations
- Reaction-diffusion: grid, output field after n steps
- Slime mold: graph, output path reinforcement
- Reservoir computing: state vector, output readout
- Membrane computing: multiset + membranes, output halting config
- In-materio: black-box response, output transfer function
