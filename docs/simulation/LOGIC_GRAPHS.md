# Logic Graphs: Ancient Computing Mechanisms

Purpose: define logic-only models for emulator design.
Scope: logic graphs describe state, operations, and outputs only. UI and visuals are layered later.

Conventions
- Inputs -> State -> Operations -> Outputs
- Graph notation uses ASCII: [Input] -> (Op) -> {State} -> (Op) -> [Output]
- Each graph is deterministic and testable with discrete steps, even for analog devices.

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
- Inputs: date, time, observed altitude
- State: rete rotation, plate alignment
- Operations: rotate_rete, align_alidade, read_scale
- Outputs: time or celestial position

Logic graph
[Date/time] -> (rotate_rete) -> {rete_state}
[Altitude] -> (align_alidade) -> {alignment}
{alignment} -> (read_scale) -> [time/position]

Suggested tests
- Known date/time -> expected star position (from reference table)

-------------------------------------------------------------------------------

Notes
- These graphs define the logic core only. UI interactions and 3D/visual models are deferred.
- Each emulator should expose deterministic step functions plus a batch runner for testing.
