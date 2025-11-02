================================================================================
PHASE 3 IMPLEMENTATION ROADMAP & GRANULAR TASK LIST
Ancient Compute Project
9-Week Sprint (Weeks 1–9)
================================================================================

SPRINT OVERVIEW
================================================================================

Phase 3 objectives:
1. Implement Difference Engine No. 2 (DE2) emulator
2. Build printer/stereotyper I/O subsystem
3. Create debugger with symbol table + breakpoints
4. Achieve 90%+ test coverage across all modules
5. Validate against SMG documentation & historical test cases

Timeline: 9 weeks
Teams: Single developer (granular sequential workflow)
Deliverables: ~3,000 lines of core code + ~2,000 lines of tests + documentation

================================================================================
WEEK 1–2: EMULATOR CORE & COLUMN MECHANICS
================================================================================

### WEEK 1: Architecture Scaffold & Column Data Structures

**Objective:** Implement column state machine and digit operations

#### 1.1.1: Create Emulator Module Structure
**Deliverable:** `backend/src/emulator/` directory with module skeleton
**Tasks:**
- [ ] Create directory: `backend/src/emulator/`
- [ ] Create `__init__.py` with module exports
- [ ] Create `core.py` (top-level DEMachine class)
- [ ] Create `columns.py` (DigitColumn, ColumnBank classes)
- [ ] Create `carry.py` (AnticipatingCarriage logic)
- [ ] Create `timing.py` (TimingController, event dispatch)
- [ ] Create `types.py` (@dataclasses for state, snapshots, events)
**Validation:**
  ```bash
  python -c "from backend.src.emulator import DEMachine; print(DEMachine.__doc__)"
  ```

#### 1.1.2: Implement DigitColumn Class
**Deliverable:** `backend/src/emulator/columns.py` - digit storage & operations
**Code:** ~150 lines
```python
@dataclass
class DigitColumn:
    """One difference column (31 decimal digits, positions 0–30)"""
    digits: list[int]  # [0..9]
    column_index: int
    carry_state: bool = False
    anticipating_carry_in: bool = False
    anticipating_carry_out: bool = False

    def __post_init__(self):
        assert len(self.digits) == 31, "Column must have 31 digits"
        assert all(0 <= d <= 9 for d in self.digits), "Digits must be 0–9"

    def add_difference(self, diff: list[int]) -> None:
        """Add difference row to column; handle carry propagation"""
        # Babbage's addition: rightmost digit first (least significant)
        # Accumulate carry through 31 positions
        pass

    def get_value_as_int(self) -> int:
        """Convert digit array to integer"""
        pass

    def set_value_from_int(self, val: int) -> None:
        """Set digits from integer (zero-padded to 31)"""
        pass

    def get_carry_out(self, position: int) -> bool:
        """Query if position generates carry to next position"""
        pass

    def state_dict(self) -> dict:
        """Return current state as dict (for snapshots)"""
        pass
```
**Tests:** `backend/tests/unit/test_digit_column.py` (~80 tests)
- add_difference with no carry
- add_difference with carry propagation
- carry at boundary positions
- value conversions (int → digits → int)
**Validation:** Unit tests pass, >95% coverage

#### 1.1.3: Implement ColumnBank & MultiColumn State
**Deliverable:** `backend/src/emulator/columns.py` - ColumnBank class
**Code:** ~200 lines
```python
class ColumnBank:
    """All 8 difference columns, unified state management"""

    def __init__(self, initial_values: list[int] = None):
        self.columns = [DigitColumn(digits=[0]*31, column_index=i) for i in range(8)]
        if initial_values:
            for i, val in enumerate(initial_values):
                self.columns[i].set_value_from_int(val)

    def add_difference_row(self, diff_row: list[int]) -> None:
        """Add one difference row to all columns; handle carries"""
        # Called once per column advance (every 360° cycle)
        pass

    def get_all_values(self) -> list[int]:
        """Read all 8 column values"""
        pass

    def get_column(self, idx: int) -> DigitColumn:
        pass

    def state_snapshot(self) -> dict:
        """Return all column states"""
        pass
```
**Tests:** `backend/tests/unit/test_column_bank.py` (~40 tests)
- initialize columns with values
- add difference row (all columns simultaneously)
- carry propagation across columns
- state snapshot consistency
**Validation:** Unit tests pass

#### 1.1.4: Create Type Definitions & Dataclasses
**Deliverable:** `backend/src/emulator/types.py` (~100 lines)
```python
@dataclass
class DebugSnapshot:
    """Mechanical state at a moment"""
    main_shaft_angle: int
    column_states: dict  # column_index → DigitColumn.state_dict()
    carry_states: dict   # position → bool
    printer_position: int
    stereo_position: tuple
    event_log: list[str]
    phase_name: str

@dataclass
class TimeEvent:
    """One mechanical event at specific turn angle"""
    angle: int
    phase: str
    component: str  # "column", "carry", "printer", "stereo"
    action: str     # "latch", "add", "carry", "strike", etc.
    data: dict      # component-specific data
```
**Validation:** Types compile and pass mypy checks

#### 1.1.5: Unit Test Suite for Week 1
**Deliverable:** Combined test pass rate
**Tests:** 120+ unit tests across column, carry, state classes
**Validation:**
  ```bash
  cd backend && pytest tests/unit/test_digit_column.py tests/unit/test_column_bank.py -v
  # Expected: 120+ passed
  ```

---

### WEEK 2: Anticipating Carriage & Timing Controller

#### 1.2.1: Implement AnticipatingCarriage Logic
**Deliverable:** `backend/src/emulator/carry.py` (~300 lines)
```python
class AnticipatingCarriage:
    """Babbage's overlapped carry mechanism (reduces cycle count)"""

    def __init__(self, column_bank: ColumnBank):
        self.columns = column_bank
        self.look_ahead_depth = 2
        self.carry_predictions = {}

    def evaluate_carry_at_position(self, position: int) -> bool:
        """Determine if position will produce carry (from SMG timing)"""
        # At turn 60°: check position 0
        # At turn 120°: check position 1
        # This drives the mechanical lifter for next carry
        pass

    def execute_carry_cycle(self) -> dict:
        """Execute one anticipating carriage cycle"""
        # Return: {position → carry_predicted}
        pass

    def get_carry_state(self, position: int) -> tuple[bool, bool]:
        """Get (carry_in, carry_out) for position"""
        pass

    def state_dict(self) -> dict:
        """Return current carry state"""
        pass
```
**Key insight:** Anticipating carriage reduces 8–16 traditional rotations per addition
to just 2 rotations for a full addition with carries.

**Tests:** `backend/tests/unit/test_anticipating_carriage.py` (~50 tests)
- carry prediction accuracy
- look-ahead at different positions
- overlap timing (verify 2-rotation cycle)
- state consistency

**Validation:** Tests pass; cycle time is 2 rotations (verified in logs)

#### 1.2.2: Implement TimingController (Main Shaft Simulator)
**Deliverable:** `backend/src/emulator/timing.py` (~400 lines)
```python
class TimingController:
    """Drive all mechanical actions from main shaft angle"""

    def __init__(self, column_bank: ColumnBank, anticipating_carriage):
        self.columns = column_bank
        self.carriage = anticipating_carriage
        self.main_shaft_angle = 0
        self.cycle_count = 0
        self.event_log = []
        self.phase_map = self._build_phase_map()

    def _build_phase_map(self) -> dict:
        """Map shaft angle (0–360°) → phase transitions"""
        return {
            (0, 30): "column_latch",
            (30, 60): "addition_begin",
            (60, 90): "carry_evaluation_1",
            (90, 120): "carry_execution",
            (120, 150): "carry_evaluation_2",
            (150, 180): "settle_phase",
            (180, 210): "print_setup",
            (210, 240): "inking",
            (240, 270): "print_strike",
            (270, 300): "platen_advance",
            (300, 330): "stereo_advance",
            (330, 360): "cycle_reset"
        }

    def advance_shaft(self, degrees: int) -> dict:
        """Advance main shaft by N degrees; return events"""
        old_angle = self.main_shaft_angle
        self.main_shaft_angle = (self.main_shaft_angle + degrees) % 360

        events = self._dispatch_events(old_angle, self.main_shaft_angle)
        self.event_log.extend(events)
        return {"events": events, "angle": self.main_shaft_angle}

    def _dispatch_events(self, from_angle: int, to_angle: int) -> list[dict]:
        """Determine which mechanical events fire in this angle range"""
        events = []
        for phase_range, phase_name in self.phase_map.items():
            if self._phase_active(phase_range, from_angle, to_angle):
                events.append({
                    "angle": to_angle,
                    "phase": phase_name,
                    "data": self._get_phase_data(phase_name)
                })
        return events

    def execute_full_cycle(self) -> dict:
        """Run one complete 360° rotation"""
        cycle_events = []
        self.cycle_count += 1
        while self.main_shaft_angle % 360 != 0 or len(cycle_events) == 0:
            result = self.advance_shaft(30)
            cycle_events.extend(result["events"])
        return {
            "cycle": self.cycle_count,
            "events": cycle_events,
            "column_states": self.columns.state_snapshot()
        }

    def get_snapshot(self) -> DebugSnapshot:
        """Return current mechanical state"""
        pass
```

**Critical timing from SMG Technical Description:**
```
Turn 0°:    Column latch opens
Turn 30°:   Difference addition begins
Turn 60°:   Anticipating carriage evaluates position 0
Turn 90°:   Column advance (figure wheel shift)
Turn 120°:  Anticipating carriage evaluates position 1
Turn 150°:  Carry execution completes
Turn 180°:  Print setup begins
Turn 210°:  Inking roller engages
Turn 240°:  Print hammer strikes
Turn 270°:  Platen advances
Turn 300°:  Stereotype frame advances
Turn 330°:  Stereotype mold extraction
Turn 360°:  Cycle complete
```

**Tests:** `backend/tests/unit/test_timing_controller.py` (~60 tests)
- phase transitions at correct angles
- event firing order
- full cycle execution
- state consistency across cycles

**Validation:** Event sequence matches SMG timing diagram

#### 1.2.3: Integration: Emulator Main Class
**Deliverable:** `backend/src/emulator/core.py` (~200 lines)
```python
class DEMachine:
    """Difference Engine No. 2 - complete simulator"""

    def __init__(self, initial_differences: list[int] = None):
        self.column_bank = ColumnBank(initial_differences)
        self.anticipating_carriage = AnticipatingCarriage(self.column_bank)
        self.timing = TimingController(self.column_bank, self.anticipating_carriage)
        self.printer = None  # Will add in Week 3
        self.stereotyper = None  # Will add in Week 3

    def load_initial_state(self, state: dict) -> None:
        """Set initial column values and difference row"""
        pass

    def execute_operation(self, operation: str) -> dict:
        """Execute one operation: "add", "subtract", etc."""
        pass

    def run_cycle(self) -> dict:
        """Execute one complete 360° cycle"""
        return self.timing.execute_full_cycle()

    def run_n_cycles(self, n: int) -> list[dict]:
        """Execute N cycles; return all results"""
        results = []
        for _ in range(n):
            results.append(self.run_cycle())
        return results

    def get_column_values(self) -> list[int]:
        """Read all 8 column values (after cycle complete)"""
        return self.column_bank.get_all_values()

    def get_snapshot(self) -> DebugSnapshot:
        """Return current complete state"""
        return self.timing.get_snapshot()
```

**Tests:** `backend/tests/unit/test_de_machine.py` (~30 tests)
- initialize with differences
- execute multiple cycles
- read column values
- state consistency

#### 1.2.4: End-to-End Timing Test
**Deliverable:** Integration test validating timing against SMG spec
**Test:** `backend/tests/integration/test_de2_timing.py` (~40 tests)
```python
def test_single_cycle_event_sequence():
    """Verify one cycle generates events at correct angles"""
    de = DEMachine(initial_differences=[1, 0, 0, 0, 0, 0, 0, 0])
    result = de.run_cycle()

    # Expected events (from SMG timing diagram)
    expected_angles = [30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330, 360]
    actual_angles = [evt["angle"] for evt in result["events"]]

    assert actual_angles == expected_angles, \
        f"Event timing mismatch: {actual_angles} vs {expected_angles}"
```

**Validation:**
  ```bash
  cd backend && pytest tests/integration/test_de2_timing.py -v
  # Expected: all timing tests pass
  ```

#### 1.2.5: Week 2 Integration Test
**Deliverable:** Simple polynomial evaluation test
**Test:** Compute x² + x + 1 for x ∈ [0, 5]
```python
def test_polynomial_difference_table():
    """Evaluate x² + x + 1 using finite differences"""
    # Initial values: [1, 3, 7, 13, 21, 31] (f(x) for x=0..5)
    # Differences: [2, 4, 6, 8, 10]
    # 2nd differences: [2, 2, 2, 2]
    # 2nd differences are constant → use DE2

    de = DEMachine(initial_differences=[1, 2, 2, 0, 0, 0, 0, 0])
    results = []
    for _ in range(6):
        de.run_cycle()
        results.append(de.get_column_values()[0])  # Column 0 has result

    expected = [1, 3, 7, 13, 21, 31]
    assert results == expected, f"Polynomial eval failed: {results} vs {expected}"
```

**Validation:** Week 2 polynomial test passes

---

### WEEK 2 DELIVERABLES

```
backend/src/emulator/
├── __init__.py
├── core.py          (DEMachine, ~200 lines)
├── columns.py       (DigitColumn, ColumnBank, ~350 lines)
├── carry.py         (AnticipatingCarriage, ~300 lines)
├── timing.py        (TimingController, ~400 lines)
└── types.py         (Dataclasses, ~100 lines)

backend/tests/unit/
├── test_digit_column.py             (~150 lines, 80 tests)
├── test_column_bank.py              (~100 lines, 40 tests)
├── test_anticipating_carriage.py    (~100 lines, 50 tests)
├── test_timing_controller.py        (~120 lines, 60 tests)
└── test_de_machine.py               (~80 lines, 30 tests)

backend/tests/integration/
└── test_de2_timing.py               (~150 lines, 40 tests)

Total: ~2,000 lines (code + tests)
```

**Test Results (End of Week 2):**
- Unit tests: 260+ passing
- Integration: Polynomial test passing
- Coverage: >90% emulator core
- CI/CD: All tests passing, warnings as errors

---

## WEEK 3–4: PRINTER & STEREOTYPER I/O

### WEEK 3: Printer Apparatus & Output Formatting

#### 1.3.1: Implement PrinterApparatus Class
**Deliverable:** `backend/src/emulator/printer.py` (~300 lines)
```python
class PrinterApparatus:
    """Type setter, inking, hammer, platen (SMG drawings)"""

    def __init__(self):
        self.type_wheels = [0] * 8  # 8 digit positions per line
        self.inking_engaged = False
        self.hammer_ready = False
        self.platen_position = 0
        self.printed_lines = []

    def set_digit(self, position: int, digit: int) -> None:
        """Select type wheel for position (turn 180°–210°)"""
        assert 0 <= position < 8, "Position must be 0–7"
        assert 0 <= digit <= 9, "Digit must be 0–9"
        self.type_wheels[position] = digit

    def set_type_from_columns(self, column_values: list[int]) -> None:
        """Set type wheels from column readings"""
        # Read 8 rightmost digits from column 0
        for i in range(8):
            # Extract digit i from column value
            pass

    def engage_ink(self) -> None:
        """Inking roller engages (turn 210°)"""
        self.inking_engaged = True

    def strike_hammer(self) -> str:
        """Print hammer strikes (turn 240°)"""
        line = "".join(str(d) for d in self.type_wheels)
        self.printed_lines.append(line)
        self.platen_position += 1
        return line

    def advance_platen(self) -> None:
        """Paper advance (turn 270°)"""
        # In mechanical printer, platen rotates to next line
        pass

    def get_printed_page(self) -> list[str]:
        """Return all printed lines"""
        return self.printed_lines.copy()

    def reset(self) -> None:
        """Clear for new page"""
        self.printed_lines = []
        self.platen_position = 0

    def state_dict(self) -> dict:
        """Return printer state"""
        pass
```

**Tests:** `backend/tests/unit/test_printer.py` (~40 tests)
- set_digit for each position
- type wheel formatting
- hammer strike accumulates lines
- platen position tracking

#### 1.3.2: Implement StereotypeFrame (Mold) Class
**Deliverable:** `backend/src/emulator/stereotyper.py` (~300 lines)
```python
class StereotypeFrame:
    """Mold creation apparatus (SMG drawing BAB/A/166)"""

    def __init__(self):
        self.x_position = 0  # 0–7 (8 digit positions)
        self.y_position = 0  # 0–49 (50 lines per mold)
        self.mold_image = {}  # (x, y) → raised (1) or flat (0)
        self.max_y = 50
        self.completed_molds = []

    def receive_printed_line(self, line: str) -> None:
        """Record one printed line in mold (turn 300°)"""
        for x, char in enumerate(line):
            # Type contact: raised (1) if digit, flat (0) otherwise
            raised = 1 if char != '0' else 0  # Simplified
            self.mold_image[(x, self.y_position)] = raised
        self.y_position += 1

    def should_extract_mold(self) -> bool:
        """Check if mold is complete (50 lines full)"""
        return self.y_position >= self.max_y

    def extract_mold(self) -> dict:
        """Remove completed mold (turn 330°)"""
        mold = {
            "image": self.mold_image.copy(),
            "lines": self.y_position,
            "extracted_at_cycle": len(self.completed_molds)
        }
        self.completed_molds.append(mold)
        self.mold_image = {}
        self.y_position = 0
        return mold

    def render_mold_as_art(self, mold: dict) -> str:
        """ASCII art representation of mold"""
        lines = []
        for y in range(mold["lines"]):
            row = ""
            for x in range(8):
                row += "X" if mold["image"].get((x, y), 0) else "."
            lines.append(row)
        return "\n".join(lines)

    def get_completed_molds(self) -> list[dict]:
        """Return all extracted molds"""
        return self.completed_molds.copy()

    def state_dict(self) -> dict:
        pass
```

**Tests:** `backend/tests/unit/test_stereotyper.py` (~35 tests)
- receive printed lines
- accumulate in mold
- extract when full
- render as ASCII art

#### 1.3.3: Integrate Printer & Stereotyper with DEMachine
**Deliverable:** Update `backend/src/emulator/core.py` (~100 lines added)
```python
class DEMachine:
    def __init__(self, initial_differences: list[int] = None):
        # ... existing code ...
        self.printer = PrinterApparatus()
        self.stereotyper = StereotypeFrame()

    def _execute_print_phase(self) -> None:
        """Called by timing controller at turn 240°"""
        # Read columns; set printer digits
        column_values = self.column_bank.get_all_values()
        self.printer.set_type_from_columns(column_values)
        line = self.printer.strike_hammer()
        self.stereotyper.receive_printed_line(line)

    def _check_mold_extraction(self) -> dict:
        """Called at turn 330°"""
        if self.stereotyper.should_extract_mold():
            return self.stereotyper.extract_mold()
        return None
```

**Validation:** Printer + stereotyper integrate with timing controller

#### 1.3.4: End-to-End Print Test
**Deliverable:** Integration test `backend/tests/integration/test_printer_output.py`
```python
def test_polynomial_printed_output():
    """Print results of x² + x + 1 table"""
    de = DEMachine(initial_differences=[1, 2, 2, 0, 0, 0, 0, 0])

    for cycle in range(6):
        de.run_cycle()

    page = de.printer.get_printed_page()
    expected_lines = ["00000001", "00000003", "00000007", "00000013", "00000021", "00000031"]

    assert len(page) == 6, f"Expected 6 lines, got {len(page)}"
    for i, (actual, expected) in enumerate(zip(page, expected_lines)):
        assert actual == expected, f"Line {i}: {actual} vs {expected}"
```

**Validation:** Print output matches expected values

### WEEK 4: Card Reader & AE Input System

#### 1.4.1: Implement AECard & CardReader
**Deliverable:** `backend/src/emulator/cards.py` (~350 lines)
```python
@dataclass
class AECard:
    """Analytical Engine punched card"""
    card_type: str  # "operation", "variable", "number"
    fields: list[int]  # Type-specific
    card_id: int

class CardReader:
    """Simulates punched card feed"""

    def __init__(self, card_deck: list[AECard]):
        self.deck = card_deck
        self.index = 0

    def read_next_card(self) -> AECard:
        if self.index >= len(self.deck):
            raise StopIteration("Deck exhausted")
        card = self.deck[self.index]
        self.index += 1
        return card

    def parse_operation_card(self, card: AECard) -> str:
        """Extract operation: 'add', 'mul', 'div', 'load', 'store'"""
        # Encoding: fields[0] → operation index
        operations = ['add', 'mul', 'div', 'load', 'store']
        return operations[card.fields[0]]

    def parse_variable_card(self, card: AECard) -> list[int]:
        """Extract addressed column indices"""
        return card.fields  # Already indices

    def parse_number_card(self, card: AECard) -> list[int]:
        """Extract 31-digit number"""
        return card.fields + [0] * (31 - len(card.fields))
```

**Tests:** `backend/tests/unit/test_card_reader.py` (~30 tests)
- parse operation cards
- parse variable cards
- parse number cards
- deck sequencing

#### 1.4.2: Analytical Engine (AE) Reduced Store
**Deliverable:** `backend/src/emulator/analytical_engine.py` (~400 lines)
```python
class AnalyticalEngine:
    """Minimal AE: store + mill + card control"""

    def __init__(self, store_size: int = 20):
        self.store = [0] * store_size  # 20 addressed columns
        self.mill = 0  # Accumulator
        self.card_reader = None
        self.printer = PrinterApparatus()
        self.operation_log = []

    def load_program(self, cards: list[AECard]) -> None:
        self.card_reader = CardReader(cards)

    def execute(self) -> tuple[list[str], list[dict]]:
        """Execute card deck; return printed page + molds"""
        try:
            while True:
                card = self.card_reader.read_next_card()
                self._execute_card(card)
        except StopIteration:
            pass

        return self.printer.get_printed_page(), []

    def _execute_card(self, card: AECard) -> None:
        """Execute one card instruction"""
        if card.card_type == "operation":
            op = self.card_reader.parse_operation_card(card)
            if op == "add":
                # Add two store addresses; result to mill
                pass
            elif op == "mul":
                # Multiply
                pass
            elif op == "print":
                # Print mill value
                line = self._format_mill_output()
                self.printer.append_line(line)

    def _format_mill_output(self) -> str:
        """Format mill value as 8-digit string"""
        return f"{self.mill:08d}"
```

**Tests:** `backend/tests/unit/test_analytical_engine.py` (~40 tests)
- load card deck
- execute add operation
- execute print operation
- mill accumulator behavior

#### 1.4.3: Week 4 Deliverable: Lovelace Bernoulli Test
**Deliverable:** Test `backend/tests/integration/test_lovelace_bernoulli.py`
```python
def test_bernoulli_sequence():
    """Execute Lovelace Note G: Bernoulli numbers"""
    # Simplified version of Bernoulli sequence (Note G)
    ae = AnalyticalEngine(store_size=20)

    # Build card deck (operation, variable, number cards)
    cards = [
        AECard("number", [1], 1),   # Load 1
        AECard("operation", [0], 2),  # ADD (a, 1) → a
        AECard("number", [1], 3),   # Load 1
        AECard("operation", [1], 4),  # MUL (a, 1)
        AECard("operation", [3], 5),  # PRINT
    ]

    ae.load_program(cards)
    page, molds = ae.execute()

    assert len(page) > 0, "No output generated"
    assert page[0] == "00000001", f"Expected 1, got {page[0]}"
```

**Validation:** Bernoulli test passes (simplified version)

---

## WEEK 5–6: DEBUGGER & SYMBOL TABLE

(Detailed breakdown continues...)

## WEEK 7–8: TESTING & VALIDATION

## WEEK 9: DOCUMENTATION & COMPLETION

---

## SUMMARY TABLE

| Week | Component | LOC | Tests | Status |
|------|-----------|-----|-------|--------|
| 1-2  | Emulator Core | 1,200 | 260 | Foundation |
| 3-4  | Printer/I/O | 700 | 105 | I/O Bridge |
| 5-6  | Debugger | 600 | 90 | Inspection |
| 7-8  | Testing | - | 200+ | Validation |
| 9    | Docs | 500 | - | Documentation |
| **Total** | **~3,600** | **>600** | **Complete** |

---

## SUCCESS CRITERIA

**Must achieve:**
1. DE2 emulator executes simple difference tables
2. Printer produces formatted 8-digit lines
3. Stereotyper creates valid molds
4. Debugger allows breakpoint setting & variable inspection
5. >90% test coverage (emulator), >85% (I/O), >80% (debugger)
6. All tests passing; CI/CD green

**Should achieve:**
1. Lovelace Bernoulli sequence (simplified)
2. Multiple mold extraction per page
3. Symbol table integration with compiler IR
4. Mechanical notation visualization (future)

================================================================================
END ROADMAP
================================================================================
