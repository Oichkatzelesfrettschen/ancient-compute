# Logic Graphs: Ancient Computing Mechanisms

Purpose: define logic-only models for emulator design.
Scope: logic graphs describe state, operations, and outputs only. UI and visuals are layered later.

Conventions
- Inputs -> State -> Operations -> Outputs
- Graph notation uses ASCII: [Input] -> (Op) -> {State} -> (Op) -> [Output]
- Each graph is deterministic and testable with discrete steps, even for analog devices.

Schema (canonical)
- Node types: input, state, op, output
- Edge: directed, labeled with data or control flow
- Minimum fields:
  - id: stable identifier (snake_case)
  - type: input|state|op|output
  - label: short human-readable name
  - deps: list of upstream node ids
  - notes: constraints or invariants (optional)

Example (pseudo-YAML)
graph:
  - id: input_event
    type: input
    label: Event
  - id: add_mark
    type: op
    label: add_mark
    deps: [input_event]
  - id: marks
    type: state
    label: marks
    deps: [add_mark]
  - id: tally_string
    type: output
    label: tally_string
    deps: [marks]

-------------------------------------------------------------------------------

1) Tally Sticks / Tally Marks

Core logic
- Inputs: increment events, decrement events, query
- State: mark groups (1-4) + group separators (5)
- Operations: add_mark, remove_mark, group_by_five, count
- Outputs: tally string, numeric count

Logic graph
[Event] -> (add_mark) -> {marks} -> (group_by_five) -> {groups} -> (render) -> [tally_string]
[Query] -> (count) -> {marks} -> [number]

Suggested tests
- 0 -> ""; 4 -> "||||"; 5 -> "|||||" or "||||-" (choose one standard)
- remove_mark underflow handled (no negative)

-------------------------------------------------------------------------------

2) Clay Tokens / Bullae (accounting)

Core logic
- Inputs: commodity, quantity, transaction_type (deposit/withdraw), seal/unseal
- State: token multiset, bulla sealed flag
- Operations: add_token, remove_token, seal, impress, audit
- Outputs: token inventory, impression ledger

Logic graph
[Transaction] -> (add/remove token) -> {token_bag} -> (optional seal) -> {sealed_bulla}
{sealed_bulla} -> (impress) -> [impression_ledger]
[Audit] -> (compare tokens vs ledger) -> [match/mismatch]

Suggested tests
- Deposit 3 sheep, 2 grain -> ledger shows 3/2
- Seal then attempt edit -> rejected

-------------------------------------------------------------------------------

3) Counting Rods / Abacus

Core logic
- Inputs: number set, operation (add/sub/mul/div)
- State: digit columns (base 10), carry flags
- Operations: set_value, add, subtract, carry_propagate, multiply, divide
- Outputs: digit column values, numeric result

Logic graph
[Number] -> (set_value) -> {columns}
[Op + Operand] -> (apply op) -> {columns} -> (carry_propagate) -> {columns} -> [result]

Suggested tests
- 789 + 456 = 1245 with two carry steps
- 1000 - 1 = 999 (borrow chain)

-------------------------------------------------------------------------------

4) Antikythera Mechanism (astronomical cycles)

Core logic
- Inputs: crank angle or elapsed days
- State: gear train angles, dial pointers, phase offsets
- Operations: rotate, gear_ratio_propagate, differential_sum
- Outputs: calendar date, zodiac position, lunar phase, eclipse cycle indicators

Logic graph
[Crank angle] -> (rotate) -> {primary_gear}
{primary_gear} -> (gear_ratio_propagate) -> {gear_train}
{gear_train} -> (differential_sum) -> {dial_positions} -> [dial_readouts]

Suggested tests
- 1 synodic month step updates lunar phase
- 1 Metonic cycle (235 months) returns calendar alignment
- Draconic pointer ratio sanity: s1 rotations per b1 rotation matches sourced ratio (arXiv:2104.06181)

-------------------------------------------------------------------------------

5) Quipu / Khipu (decimal knot encoding)

Core logic
- Inputs: category, numeric value, record id
- State: cord tree (main cord + pendant cords), knot positions and types
- Operations: encode_value, decode_value, sum_by_category
- Outputs: decoded ledger entries, totals

Logic graph
[Record] -> (encode_value) -> {cord_tree}
[Query] -> (decode_value) -> {cord_tree} -> [ledger_entry]
[Audit] -> (sum_by_category) -> {cord_tree} -> [category_totals]

Suggested tests
- Encode 123 (hundreds/tens/ones) and decode to 123
- Two cords in same category sum correctly

-------------------------------------------------------------------------------

6) Pascaline (wheel adder)

Core logic
- Inputs: digit wheel rotations per place
- State: wheel positions, carry pawls
- Operations: rotate_wheel, carry_forward
- Outputs: displayed digits

Logic graph
[Digit input] -> (rotate_wheel) -> {wheels} -> (carry_forward) -> {wheels} -> [display]

Suggested tests
- 59 + 1 = 60 (single carry)
- 99 + 1 = 100 (carry chain)

-------------------------------------------------------------------------------

7) Leibniz Stepped Reckoner (add/mul/div via stepped drum)

Core logic
- Inputs: multiplicand, multiplier, rotation count, carriage shift
- State: stepped drum, accumulator, carriage position
- Operations: add_cycle, subtract_cycle, shift_carriage, repeat
- Outputs: accumulator value, quotient/remainder for division

Logic graph
[Set multiplicand] -> (load drum) -> {drum}
[Rotate N times] -> (add_cycle) -> {accumulator}
[Shift] -> (shift_carriage) -> {position} -> (add_cycle) -> {accumulator} -> [result]

Suggested tests
- 123 * 45 via repeated add and shift
- 100 / 4 via repeated subtract and count

-------------------------------------------------------------------------------

8) Jacquard Loom (card-programmed patterning)

Core logic
- Inputs: card deck, warp threads, weft insert events
- State: hook selection, lifted warp set
- Operations: read_card, select_hooks, lift_warp, insert_weft
- Outputs: woven pattern row

Logic graph
[Card] -> (read_card) -> {hook_selection} -> (lift_warp) -> {warp_state}
{warp_state} -> (insert_weft) -> [pattern_row]

Suggested tests
- Repeating two-card deck produces alternating pattern

-------------------------------------------------------------------------------

9) Babbage Analytical Engine (base + Ada program deck)

Core logic
- Inputs: operation cards, variable cards, number cards
- State: store, mill registers, barrel position, flags
- Operations: fetch, decode, execute, store, branch, I/O
- Outputs: printed numbers, punched cards, trace

Logic graph
[Card deck] -> (fetch) -> (decode) -> (execute) -> {store/mill}
{store/mill} -> (branch?) -> (fetch) -> ... -> [output]

Suggested tests
- Add/multiply program produces correct output
- Ada Lovelace Bernoulli program (Note G) matches known values

-------------------------------------------------------------------------------

9a) Ada Lovelace Note G (Bernoulli numbers, Table A.2)

Core logic
- Inputs: n (target index), initial constants for V1..V7, card deck (Ops 1..25)
- State: V1..V24 columns, A0/A1/A3 accumulators, B-series results
- Operations: Op1-12 init, Op13-23 repeat loop, Op24 output, Op25 variable-card reset
- Outputs: B1, B3, B5, ... B(2n-1)

Logic graph (high level)
[n + initial V] -> (Ops 1..12 init) -> {A0, A1, B1}
{A0,A1,B1} -> (Ops 13..23 loop) -> {A3, B3, ...}
{loop state} -> (Op24 output) -> [B(2n-1)]
{state} -> (Op25 reset) -> {next iteration}

Suggested tests
- For n=1, output B1 = -1/2
- For n=2, output B3 = 1/6 (note: see errata references)
- Cross-check Op21/Op24 placement against docs/simulation/NOTE_G_TRANSCRIPTION.md

-------------------------------------------------------------------------------

10) Slide Rule (logarithmic scale analog computation)

Core logic
- Inputs: align scales at x and y
- State: scale offsets
- Operations: translate (add logs), read (convert back)
- Outputs: approximate product/quotient

Logic graph
[Align scales] -> (offset = log(x)) -> {scale_state}
{scale_state} -> (read at y) -> [approx result]

Suggested tests
- 2 * 3 approximates 6 within tolerance

-------------------------------------------------------------------------------

11) Astrolabe (astronomical time/position solver)

Core logic
- Inputs: date, time, latitude, target body (e.g., Sun)
- State: declination model and local hour angle
- Operations: compute_declination, compute_hour_angle, spherical_altitude
- Outputs: altitude (or inferred time/position in inverse mode)

Logic graph
[Date/time] -> (rotate_rete) -> {rete_state}
[Altitude] -> (align_alidade) -> {alignment}
{alignment} -> (read_scale) -> [time/position]

Suggested tests
- Equinox at equator, local noon -> near-zenith altitude
- (Future) extracted primary tables cross-check computed model

-------------------------------------------------------------------------------

Anachronistic / Unconventional Computing Extensions (non-ancient, optional scope)
- These are modern or speculative models found in ~/Documents/AGL_Library for future expansion.

12) Cellular Automata (Game of Life)

Core logic
- Inputs: initial grid, rule set (B3/S23)
- State: cell grid (alive/dead)
- Operations: count_neighbors, apply_rule, step
- Outputs: grid state after N steps

Logic graph
[Seed] -> (apply_rule) -> {grid} -> (step) -> {grid} -> [snapshot]

Suggested tests
- Blinker oscillates with period 2
- Block remains stable

-------------------------------------------------------------------------------

13) DNA Computing (strand displacement)

Core logic
- Inputs: strand set, reaction rules, target output strand
- State: strand pool with concentrations
- Operations: hybridize, displace, ligate, cleave
- Outputs: strand concentration profile or boolean output

Logic graph
[Strands + Rules] -> (react) -> {pool} -> (measure) -> [output]

Suggested tests
- AND gate strand set yields signal only when both inputs present

-------------------------------------------------------------------------------

14) Reaction-Diffusion Computing (chemical patterns)

Core logic
- Inputs: initial concentrations, reaction rates
- State: spatial concentration field
- Operations: diffuse, react, threshold
- Outputs: pattern class or computed signal

Logic graph
[Initial field] -> (diffuse/react) -> {field} -> (threshold) -> [pattern]

Suggested tests
- Stable spot/stripe pattern emerges from reference parameters

-------------------------------------------------------------------------------

15) Slime Mold Computing (Physarum pathfinding)

Core logic
- Inputs: nutrient nodes, repellents, time steps
- State: network graph with edge thickness
- Operations: grow_toward, retract, reinforce
- Outputs: optimized path network

Logic graph
[Nutrients] -> (grow) -> {network} -> (reinforce) -> {network} -> [path graph]

Suggested tests
- Two food nodes produce a shortest connecting path

-------------------------------------------------------------------------------

16) Reservoir Computing (neuromorphic/physical reservoirs)

Core logic
- Inputs: time series, readout weights
- State: reservoir activations
- Operations: drive_input, update_state, linear_readout
- Outputs: predicted sequence or classification

Logic graph
[Input] -> (drive) -> {reservoir} -> (readout) -> [output]

Suggested tests
- Echo-state memory task matches reference accuracy

-------------------------------------------------------------------------------

17) Membrane Computing (P systems)

Core logic
- Inputs: object multiset, rewrite rules, membrane tree
- State: membranes with local multisets
- Operations: apply_rules, move_objects, dissolve
- Outputs: halting configuration or object count

Logic graph
[Objects + Rules] -> (apply_rules) -> {membranes} -> (halt?) -> [output]

Suggested tests
- Simple multiset rewrite reaches expected halting state

-------------------------------------------------------------------------------

18) In-materio Computing (material substrate)

Core logic
- Inputs: stimuli patterns, measurement points
- State: material state vector
- Operations: stimulate, settle, read
- Outputs: classification or analog value

Logic graph
[Stimulus] -> (stimulate) -> {material_state} -> (read) -> [output]

Suggested tests
- Consistent classification for fixed stimulus pattern

-------------------------------------------------------------------------------

Notes
- These graphs define the logic core only. UI interactions and 3D/visual models are deferred.
- Each emulator should expose deterministic step functions plus a batch runner for testing.
