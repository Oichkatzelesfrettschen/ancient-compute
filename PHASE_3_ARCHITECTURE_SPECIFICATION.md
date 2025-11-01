================================================================================
PHASE 3 ARCHITECTURE SPECIFICATION: BABBAGE ISA EMULATOR + I/O + DEBUGGER
Ancient Compute Project
Grounded in Primary Sources & SMG/CHM Documentation
================================================================================

EXECUTIVE SCOPE
================================================================================

**What we're building, NOT making up:**
- Difference Engine No. 2 (DE2) emulator based on Science Museum Group (SMG)
  Technical Description (232 pp., including detailed appendices, parts lists,
  timing diagrams, construction drawings)
- Printer/Stereotyper apparatus modeled on authenticated SMG drawing sheets
  (Plan sheets for inking, printing, stereotype frame mechanisms)
- Analytical Engine (AE) card interpreter based on Plan 27/28/28a and
  Menabrea/Lovelace primary sources (1842–1843)
- Debugger architecture mapping to mechanical notation (Babbage 1826) for
  symbolic carry/anticipation inspection

**NOT building a hypothetical "Babbage ISA":**
- DE2 is a FINITE DIFFERENCE calculator, not a general-purpose ISA
- AE is store/mill (addressed columns + ALU), with card-driven control (not instruction set)
- ISA analogy is PEDAGOGICAL bridge, not literal implementation
- Real machine behavior is MECHANICAL (cam/gear timings), not software instructions

**Documentation lineage (verifiable):**
1. SMG Babbage Papers (digitized, indexed, accessible)
   - Plan 27/28/28a (AE linear & scaled arrangements)
   - DE1/DE2 elevation, plan, parts lists, timing
   - Printing & stereotyping apparatus drawings
2. Contemporary publications
   - Babbage 1822 Royal Astronomical Society "Note …"
   - Babbage 1826 Royal Society "Mechanical Notation"
   - Menabrea 1842 (French) + Lovelace Notes 1843 (English)
3. 20th–21st century scholarship & reconstruction
   - SMG DE2 working hardware (1991 calculator, 2002 printer/stereo)
   - SMG Technical Description & User Manual (2008 onward)
   - CHM DE2 (operated 2008)
   - Plan 28 Project (ongoing academic/CAD work)

================================================================================
PHASE 3 DECISION TREE (NARROWED FROM AUDIT)
================================================================================

**MVP Selection (B1 + D1 expansion):**

**PRIMARY PATH: DE2 Difference Engine (1847–1849 design, SMG 1991/2002 build)**
Rationale: Most fully documented; working hardware exists; appendices provide
BOM, timing, tolerances; printer/stereo validated in practice.

**SECONDARY PATH (Phase 3b): AE Card Interpreter (reduced store)**
Rationale: Bridges to procedural programs; uses DE2 printer for output;
proof-of-concept for Lovelace's Bernoulli sequence.

**TERTIARY (Phase 3c): Full AE + Plotter**
Rationale: Complete system; deferred to Phase 4 pending mechanical notation
compiler and curve generation rules (less documented).

================================================================================
PART 1: EMULATOR ARCHITECTURE
================================================================================

### 1.1 Difference Engine No. 2 (DE2) Conceptual Model

**Input:** Finite difference table setup (initial values for columns 0–7)
**Computation:** Successive difference column carries via anticipating carriage
**Output:** Result values + printer/stereotyper mechanical actions

**Physical components (from SMG Technical Description appendices):**

| Component                    | Count | Function                              |
|------------------------------|-------|---------------------------------------|
| Figure wheel columns         | 8     | Digit storage (0–9 each)              |
| Difference columns           | 8     | Difference storage                    |
| Carry mechanism              | 1     | Decimal carry logic (anticipating)    |
| Printing apparatus           | 1     | Type setting, inking, hammer strike   |
| Stereotype frame             | 1     | Mold creation per line                |
| Drive shafts & gears         | ~40   | Mechanical timing & synchronization   |
| Cam followers & lifters      | ~20   | Control carry/print/stereo cycles     |
| **Total mechanical parts**   | ~8000 | ~5 tons; 11 ft long                   |

**Emulation hierarchy:**

```
DEMachine (top-level)
  ├── ColumnBank (8 difference columns, 31 digits each)
  │   ├── DigitColumn[0..7]
  │   │   ├── figure_wheels: [0..9] × 31 positions
  │   │   └── carry_chain: state (carry_in, carry_out, position)
  │   └── AnticipatingCarriage
  │       ├── look_ahead_cam: anticipates carries 2 steps ahead
  │       └── carry_propagation: overlapped carry timing
  ├── PrinterApparatus
  │   ├── type_setter: positions type wheels for digit
  │   ├── inking_roller: ink distribution
  │   ├── print_hammer: strike mechanism
  │   └── platen: paper advance
  ├── StereotypeFrame
  │   ├── frame_mover: X/Y positioning
  │   ├── mold_material: state (empty, filled, ejected)
  │   └── stereotype_cadence: synchronization with print cycle
  ├── Timing (cam-driven)
  │   ├── main_shaft: 1 rotation = 1 complete cycle
  │   ├── cam_profiles: lift curves per component
  │   └── event_log: ordered mechanical events per revolution
  └── Output
      ├── result_columns: final digit array
      ├── printed_page: character matrix
      └── stereotype_mold: binary representation
```

### 1.2 State Machine & Timing

**Cycle structure (1 main shaft revolution):**

```
Phase 1 (Turns 0–90°):    Column reads & add operations
Phase 2 (Turns 90–180°):  Carry propagation (anticipating mechanism)
Phase 3 (Turns 180–270°): Print setup (type setter positioning)
Phase 4 (Turns 270–360°): Print strike + stereotype cycle
```

**Carry propagation (critical, from SMG timing diagrams):**
- Traditional carry: sequential (slow, 8–16 rotations per addition)
- Anticipating carriage: overlapped (2 rotations per addition via look-ahead cams)
  - At position 1: check if position 0 will generate carry
  - At position 2: check if position 1 will generate carry, pre-stage position 2 carry
  - Reduces cycle time, increases mechanical complexity

**Events per cycle (from SMG appendices):**

```
Turn 0°:    Column latch opens (read current state)
Turn 30°:   Difference addition begins
Turn 60°:   Anticipating carriage evaluates column 0–1
Turn 90°:   Column advance (shift to next row)
Turn 120°:  Anticipating carriage evaluates column 2–3
Turn 150°:  Carry execution completes
Turn 180°:  Print setup begins; type wheel selection
Turn 210°:  Inking roller engages
Turn 240°:  Print hammer strike (line to page)
Turn 270°:  Platen advance (next line)
Turn 300°:  Stereotype frame advance (X-axis)
Turn 330°:  Stereotype mold extraction signal
Turn 360°:  Cycle complete; ready for next rotation
```

### 1.3 Emulator Implementation Strategy

**Layer 1: Mechanical State (Column Data)**

```python
@dataclass
class DigitColumn:
    """One difference column (31 decimal digits, positions 0–30)"""
    digits: list[int]  # 0–9 each
    carry_state: bool = False
    anticipating_carry_in: bool = False
    anticipating_carry_out: bool = False

    def add_difference(self, diff: list[int]) -> list[int]:
        """Add difference to current column, propagate carry"""
        # This runs ONCE per cycle; carry logic drives next rotation
        pass

    def get_result(self) -> list[int]:
        """Return current column state (31 digits)"""
        pass
```

**Layer 2: Anticipating Carriage (Look-ahead Logic)**

```python
class AnticipatingCarriage:
    """Babbage's innovation: overlapped carry via cam-driven look-ahead"""

    def __init__(self, columns: list[DigitColumn]):
        self.columns = columns
        self.look_ahead_depth = 2  # Check 2 positions ahead

    def evaluate_carry_at_position(self, pos: int) -> bool:
        """Determine if position `pos` will produce carry (from SMG timing)"""
        # At turn 60°: check position 0; at turn 120°: check position 1, etc.
        # Result feeds positioning mechanism for position+1's carry lifter
        pass

    def execute_carry_cycle(self) -> dict:
        """Execute one anticipating carriage cycle; return events"""
        events = {}
        for pos in range(self.look_ahead_depth):
            carry_predicted = self.evaluate_carry_at_position(pos)
            events[f"carry_position_{pos}"] = carry_predicted
        return events
```

**Layer 3: Printer Apparatus (Mechanical Actions)**

```python
class PrinterApparatus:
    """Inking, type selection, hammer, platen from SMG printer drawings"""

    def __init__(self):
        self.type_wheels = [0] * 8  # 8 digit positions on print line
        self.inking_engaged = False
        self.hammer_ready = False
        self.platen_position = 0  # line count

    def set_digit_at_position(self, pos: int, digit: int) -> None:
        """Select type wheel for digit (turn 180°–210°)"""
        self.type_wheels[pos] = digit

    def strike_print_line(self) -> str:
        """Execute print strike (turn 240°); return printed line"""
        line = "".join(str(d) for d in self.type_wheels)
        self.platen_position += 1
        return line
```

**Layer 4: Stereotype Frame (Mold Mechanics)**

```python
class StereotypeFrame:
    """Stereotype mold creation (SMG drawing BAB/A/166, etc.)"""

    def __init__(self):
        self.x_position = 0  # Frame X-axis (0–7, 8 digit positions)
        self.y_position = 0  # Frame Y-axis (0–50, ~50 lines per mold)
        self.mold_image = {}  # (x, y) -> binary (raised/flat)

    def receive_printed_line(self, line: str) -> None:
        """After print strike, mold advances X and records line (turn 300°)"""
        for i, digit_char in enumerate(line):
            # Record raised (1) or flat (0) for type face contact
            self.mold_image[(i, self.y_position)] = 1 if digit_char != '0' else 0
        self.y_position += 1

    def extract_mold(self) -> dict:
        """Remove completed mold (turn 330°); return mold state"""
        mold = self.mold_image.copy()
        self.mold_image = {}
        self.y_position = 0
        return mold
```

**Layer 5: Timing & Events (Main Shaft Simulator)**

```python
class TimingController:
    """Drive all mechanical actions from main shaft angle (0–360°)"""

    def __init__(self, columns: list[DigitColumn], printer, stereo):
        self.columns = columns
        self.printer = printer
        self.stereo = stereo
        self.main_shaft_angle = 0
        self.cycle_events = []

    def advance_shaft(self, degrees: int) -> dict:
        """Advance main shaft by `degrees`; execute triggered actions"""
        self.main_shaft_angle = (self.main_shaft_angle + degrees) % 360
        events = {}

        # Dispatch by turn angle (from SMG timing diagram)
        if 0 <= self.main_shaft_angle < 30:
            events["phase"] = "column_latch"
        elif 30 <= self.main_shaft_angle < 60:
            events["phase"] = "addition_begin"
            events["carry"] = self.anticipate_carries()
        elif 180 <= self.main_shaft_angle < 210:
            events["phase"] = "print_setup"
            # Set printer digits from column values
        elif 240 <= self.main_shaft_angle < 270:
            events["phase"] = "print_strike"
            line = self.printer.strike_print_line()
            events["printed_line"] = line
            self.stereo.receive_printed_line(line)

        return events

    def execute_full_cycle(self) -> dict:
        """Run one complete main shaft revolution (0–360°)"""
        cycle_log = []
        for step in range(0, 360, 30):
            events = self.advance_shaft(30)
            cycle_log.append(events)
        return {"cycle": cycle_log, "mold": self.stereo.extract_mold()}
```

### 1.4 Integration with Phase 2 IR

**From Phase 2, we have:**
- Babbage IR: Program → Functions → BasicBlocks → Instructions
- Service layer: execute(), validate(), get_capabilities()
- Four complete compiler pipelines (Lisp, IDRIS2, System F, Java)

**Phase 3 mapping:**
- **IR Instructions → Mechanical Actions**
  - IR `@block label:` → column operation selection
  - IR `add i64 %0, %1` → difference addition on columns
  - IR `print %result` → printer apparatus setup
  - IR `store i64 %val, %addr` → column latch/advance

- **Example: Lovelace's Bernoulli Sequence**
  ```
  ; From LISP compiler (Phase 2) or AE card interpreter (Phase 3)
  @function bernoulli_seq(n: i64) -> i64
    @block entry:
      %a = add i64 0, 1        ; Initialize a=1
      %m = add i64 0, 1        ; Initialize m=1
      @block loop:
        %cond = lt i64 %m, %n
        br %cond, loop_body, exit
      @block loop_body:
        ; Intermediate calculations (add, mul, div)
        %m_next = add i64 %m, 1
        br loop
      @block exit:
        print %a
        return %a

  ; DE2 Emulator execution:
  1. Load %a into column 0
  2. Load %m into column 1
  3. Loop: advance shaft 360°, execute condition check
  4. Print %a value (31 digits via printer)
  5. Extract stereotype mold for result
  ```

================================================================================
PART 2: I/O SUBSYSTEM ARCHITECTURE
================================================================================

### 2.1 Babbage I/O Reality (vs. modern assumptions)

**What Babbage engines actually HAD:**
1. **Input:** Punched cards (operation, variable, number cards for AE)
            Initial column setup (dials) for DE
2. **Output:** Printed page (type-set, inked, hammered)
            Stereotype mold (reusable plate for mass reproduction)

**What they did NOT have:**
- Serial console, keyboards, disk, network (anachronistic)
- Character-by-character I/O (mechanical devices print entire lines at once)
- Interrupts (mechanical; no async external events; carry mechanics are synchronous)

**Phase 3 I/O scope:**
1. **Card Input (AE):** Punch tape reader simulating card feed
2. **Printer Output:** Line-oriented, 8–31 digit positions, mold storage
3. **Stereotyper:** Mold extraction and validation

### 2.2 Card Input System (Analytical Engine)

**From Menabrea/Lovelace (1842–1843) + SMG Plan 27/28:**

```
Operation card:  [ OP1 | OP2 | OP3 ] (e.g., add, multiply, divide)
Variable card:   [ VAR1 | VAR2 | VAR3 ] (e.g., m, a, b; addressed columns)
Number card:     [ DIGIT0 | DIGIT1 | ... | DIGIT30 ] (31-digit number)
```

**Emulator representation:**

```python
@dataclass
class AECard:
    """Analytical Engine punched card"""
    card_type: str  # "operation", "variable", "number"
    fields: dict    # Type-specific fields
    row_number: int  # Card sequence number

class CardReader:
    """Simulates punched card feed mechanism"""

    def __init__(self, card_deck: list[AECard]):
        self.deck = card_deck
        self.current_index = 0
        self.read_head_position = 0  # Mechanical needle position

    def read_next_card(self) -> AECard:
        """Advance to next card; parse holes"""
        if self.current_index >= len(self.deck):
            raise StopIteration("Card deck exhausted")
        card = self.deck[self.current_index]
        self.current_index += 1
        return card

    def parse_operation_card(self, card: AECard) -> str:
        """Extract operation (add, mul, div, load, store)"""
        # Encoding: holes at specific positions → operation
        pass

    def parse_variable_card(self, card: AECard) -> list[int]:
        """Extract addressed column indices"""
        pass

    def parse_number_card(self, card: AECard) -> list[int]:
        """Extract 31-digit number"""
        pass
```

### 2.3 Printer Output (Shared by DE2 & AE)

**From SMG printer drawings (Plan of inking, printing & stereotype):**

```python
class PrinterOutput:
    """Coordinates type-setting, inking, striking, mold creation"""

    def __init__(self):
        self.output_lines: list[str] = []
        self.current_mold_stack: list[dict] = []

    def append_line(self, line: str) -> None:
        """After print strike, record line"""
        self.output_lines.append(line)

    def finalize_page(self) -> tuple[list[str], list[dict]]:
        """Extract printed page + stereotype molds for reproduction"""
        page = self.output_lines.copy()
        molds = self.current_mold_stack.copy()
        self.output_lines = []
        self.current_mold_stack = []
        return page, molds

    def export_as_text(self) -> str:
        """Pretty-print results"""
        return "\n".join(self.output_lines)

    def export_mold_as_image(self, mold_id: int) -> str:
        """Render stereotype mold as ASCII art (raised = X, flat = .)"""
        if mold_id >= len(self.current_mold_stack):
            raise IndexError(f"Mold {mold_id} not found")
        mold = self.current_mold_stack[mold_id]
        # Render (x, y) -> 'X' or '.'
        pass
```

### 2.4 Integration: AE Card Stream → Printer Output

```python
class AnalyticalEngine:
    """Minimal AE: store (30 columns) + mill (ALU) + I/O"""

    def __init__(self):
        self.store = [0] * 30  # 30 addressed columns
        self.mill = None       # ALU (accumulator-like)
        self.card_reader = None
        self.printer = None

    def load_program(self, cards: list[AECard]) -> None:
        self.card_reader = CardReader(cards)
        self.printer = PrinterOutput()

    def execute(self) -> tuple[list[str], list[dict]]:
        """Execute card stream; return printed page + molds"""
        while True:
            try:
                card = self.card_reader.read_next_card()
            except StopIteration:
                break

            if card.card_type == "operation":
                op = self.card_reader.parse_operation_card(card)
                # Execute op; store result in mill
                self.mill = self._execute_operation(op)

            elif card.card_type == "variable":
                # Load/store from addressed columns
                addrs = self.card_reader.parse_variable_card(card)
                # (Simplified; full AE has complex variable sequencing)

            elif card.card_type == "number":
                # Load constant number into mill or store
                num = self.card_reader.parse_number_card(card)
                self.mill = num

            # Trigger print on specific card patterns
            if self._should_print(card):
                line = self._format_output(self.mill)
                self.printer.append_line(line)

        return self.printer.finalize_page()
```

================================================================================
PART 3: DEBUGGER ARCHITECTURE
================================================================================

### 3.1 Babbage Mechanical Notation as Debugging Interface

**From Babbage 1826 Royal Society paper:**
Mechanical Notation is a **symbolic language for expressing machine action**.
It encodes:
- Rotational position of shafts & cams
- Lift curves (how much lifter rises per turn)
- Locking mechanisms (when columns latch, when they release)
- Carry propagation state (when carry feeds forward)

**Example notation (simplified):**
```
Position: 0° 30° 60° 90° 120° 150° 180° 210° 240° 270° 300° 330° 360°
─────────────────────────────────────────────────────────────────────
Column0:  |---|___|---|___|---|___|---|___|---|___|---|___|---|___|
          Latch  Add  Latch  Carry Settle Print  Strike  Advance Reset
Carry0→1: ______[====]________________[=====]____________________________
          (carry look-ahead at pos 1; executes ~90° later)
Print:    _________________________________________[=====]_____________
          (type setting, inking, hammer; 240° phase)
```

### 3.2 Debugger State Inspection

**What the debugger tracks:**

```python
@dataclass
class DebugSnapshot:
    """Mechanical state at a breakpoint"""
    main_shaft_angle: int          # 0–360°
    column_states: dict            # Column index → digit array
    carry_states: dict             # Position → (in, out) bools
    printer_position: int          # Line number on page
    stereotype_position: (int, int)  # (x, y) in mold frame
    event_log: list[str]           # Events fired this cycle
    notation_markers: dict         # Mechanical notation annotations
```

**Breakpoint engine:**

```python
class BreakpointEngine:
    """Set breakpoints on mechanical events"""

    def __init__(self):
        self.breakpoints = {}

    def set_breakpoint(self, condition: str, action: str = "pause") -> int:
        """
        condition examples:
          "carry_at_position == 1"  (wait for carry to pos 1)
          "main_shaft_angle == 240"  (wait for print phase)
          "column[0].digit_changed"  (wait for column 0 to change)
          "mold_extraction_signal"   (wait for stereo mold complete)
        """
        bp_id = len(self.breakpoints)
        self.breakpoints[bp_id] = {
            "condition": condition,
            "action": action,
            "enabled": True
        }
        return bp_id

    def check_breakpoints(self, snapshot: DebugSnapshot) -> list[int]:
        """Evaluate all breakpoints; return triggered BP IDs"""
        triggered = []
        for bp_id, bp_def in self.breakpoints.items():
            if bp_def["enabled"] and self._evaluate_condition(bp_def["condition"], snapshot):
                triggered.append(bp_id)
        return triggered

    def _evaluate_condition(self, cond: str, snapshot: DebugSnapshot) -> bool:
        """Match condition against snapshot"""
        # Simplified; real version parses condition string
        if "carry_at_position" in cond:
            pos = int(cond.split("==")[1].strip())
            return snapshot.carry_states.get(pos, False)
        return False
```

### 3.3 Stepping & Symbol Inspection

**Single-step semantics (mechanical):**

```python
class Stepper:
    """Step through machine execution at various granularities"""

    def __init__(self, de_machine: DEMachine):
        self.machine = de_machine
        self.history = []

    def step_angle(self, degrees: int = 30) -> DebugSnapshot:
        """Advance main shaft by N degrees; return snapshot"""
        snapshot = self.machine.advance_shaft(degrees)
        self.history.append({
            "angle": self.machine.main_shaft_angle,
            "snapshot": snapshot
        })
        return snapshot

    def step_phase(self) -> DebugSnapshot:
        """Advance to next phase (90° = one phase)"""
        return self.step_angle(90)

    def step_cycle(self) -> dict:
        """Complete one full cycle (360°); return events"""
        events_per_cycle = {}
        while self.machine.main_shaft_angle % 360 != 0 or len(self.history) == 0:
            snapshot = self.step_angle(30)
            if snapshot.get("phase"):
                events_per_cycle.setdefault(snapshot["phase"], []).append(snapshot)
        return events_per_cycle

    def run_until_breakpoint(self, bp_engine: BreakpointEngine) -> DebugSnapshot:
        """Execute until breakpoint triggers"""
        while True:
            snapshot = self.step_angle(30)
            if bp_engine.check_breakpoints(snapshot):
                return snapshot
```

**Symbol table (from compiler IR + mechanical notation):**

```python
class SymbolTable:
    """Map IR variables & columns to mechanical positions"""

    def __init__(self):
        self.symbols = {}
        self.reverse_map = {}

    def add_symbol(self, name: str, column_idx: int) -> None:
        """Register variable → column mapping"""
        self.symbols[name] = column_idx
        self.reverse_map[column_idx] = name

    def lookup_column(self, var_name: str) -> int:
        """Find column for IR variable"""
        return self.symbols.get(var_name, None)

    def lookup_variable(self, column_idx: int) -> str:
        """Find variable name for column"""
        return self.reverse_map.get(column_idx, f"column_{column_idx}")

    def inspect_variable(self, var_name: str, snapshot: DebugSnapshot) -> list[int]:
        """Get current value of variable from snapshot"""
        col_idx = self.lookup_column(var_name)
        if col_idx is None:
            raise KeyError(f"Variable {var_name} not found")
        return snapshot.column_states.get(col_idx, [])

    def print_variables(self, snapshot: DebugSnapshot) -> str:
        """Pretty-print all variables at current state"""
        lines = []
        for var_name in sorted(self.symbols.keys()):
            value = self.inspect_variable(var_name, snapshot)
            value_str = "".join(str(d) for d in value).lstrip("0") or "0"
            lines.append(f"  {var_name:20s} = {value_str}")
        return "\n".join(lines)
```

### 3.4 Mechanical Notation Compiler (Phase 3b)

**Future work: Compile Babbage 1826 notation to state diagrams**

```python
class MechanicalNotationParser:
    """Parse Babbage 1826 notation; generate timing rules"""

    def __init__(self, notation_str: str):
        self.notation = notation_str
        self.timing_rules = {}

    def parse_lift_curve(self, component: str) -> list[int]:
        """Extract lift heights (0–360° in 30° steps)"""
        # From notation, identify lifter engagement pattern
        pass

    def compile_to_state_machine(self) -> dict:
        """Convert notation to executable state transitions"""
        # Notation → FSM with carry propagation rules
        pass
```

================================================================================
PART 4: INTEGRATION & SYNTHESIS
================================================================================

### 4.1 Three-Module Dataflow

```
┌─────────────────────────────────────────────────────────────┐
│ PHASE 2 OUTPUT: Babbage IR (from compilers)                 │
│ Example: function bernoulli_seq() → IR @blocks               │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────┐
│ PHASE 3.A: EMULATOR (Mechanical Execution)                  │
│                                                              │
│  IR @block → Column operation → Carry propagation            │
│  → Print trigger → Printer apparatus action                  │
│  → Stereotype mold accumulation                              │
└────────────────┬────────────────────────────────────────────┘
                 │
                 ├──► Output: Printed page + molds
                 │
                 └──► Breakpoint/inspection point ◄──┐
                      │                               │
                      ▼                               │
┌─────────────────────────────────────────────────────────────┐
│ PHASE 3.B: DEBUGGER (Symbolic Inspection)                   │
│                                                              │
│  Symbol table (IR vars → columns)                           │
│  Breakpoint engine (mechanical condition monitoring)         │
│  Stepper (angle/phase/cycle granularity)                     │
│  Notation compiler (future: mechanical notation rules)       │
│                                                              │
│  Query: "What is m at main_shaft_angle == 120°?"           │
│  → Snapshot → lookup "m" in symbol table → column 1         │
│  → return column[1].digits at that angle                    │
└────────────────┬────────────────────────────────────────────┘
                 │
                 └──► Display/log: variable values, carry states
                      mechanical events, phase diagrams
```

### 4.2 Module Interfaces

**Emulator → I/O:**
```python
def execute_cycle(self) -> dict:
    return {
        "printed_lines": [...],  # Pass to PrinterOutput
        "mold_frames": [...],    # Store in StereotypeFrame
        "cycle_events": [...]    # Available for debugger
    }
```

**Emulator → Debugger:**
```python
def get_snapshot(self) -> DebugSnapshot:
    return {
        "angle": self.main_shaft_angle,
        "columns": self.column_bank.state(),
        "carries": self.anticipating_carriage.state(),
        "events": self.timing_controller.cycle_events
    }
```

**Debugger → Emulator (breakpoint-driven):**
```python
def set_breakpoint(self, condition: str) -> int:
    # Register with emulator; emulator checks each cycle
    pass

def get_variable(self, var_name: str, snapshot: DebugSnapshot) -> list[int]:
    # Query symbol table + snapshot
    pass
```

### 4.3 Testing Strategy (Grounded in Documented Behavior)

**Test corpus (from SMG + academic sources):**

1. **Timing validation (against SMG timing diagram)**
   - Input: Expected cycle phase diagram (0°, 30°, 60°, … 360°)
   - Output: Actual event sequence from emulator
   - Check: Event timings match SMG specifications ±1 turn

2. **Carry propagation (against SMG appendix examples)**
   - Input: Known BOM of carry mechanism + digit values
   - Output: Carry state transitions per turn
   - Check: Anticipating carriage reduces cycles as documented

3. **Printer output (against Scheutz "Specimens" or SMG test runs)**
   - Input: Difference table (e.g., polynomial evaluation)
   - Output: Printed page
   - Check: Digits match hand calculation + SMG 1991 run

4. **Stereotype mold (against SMG extraction protocol)**
   - Input: Printed page
   - Output: Mold image (raised/flat per type contact)
   - Check: Mold can be used for reproduction

5. **AE card execution (against Lovelace Note G: Bernoulli)**
   - Input: Card deck for Bernoulli sequence
   - Output: Printed results
   - Check: Results match hand-calculated Bernoulli numbers

================================================================================
PART 5: GRANULAR TASK BREAKDOWN (PHASE 3)
================================================================================

### Phase 3.1: Emulator Core (Weeks 1–4)

**3.1.1: Column Bank State Machine**
- Implement `DigitColumn` class (digits 0–9, carry flags)
- Test: Unit tests for digit operations, carry generation
- Validation: Against SMG BOM (31 digits per column, 8 columns)

**3.1.2: Anticipating Carriage Logic**
- Implement `AnticipatingCarriage.evaluate_carry_at_position()`
- Test: Carry prediction matches SMG timing diagram
- Validation: Reduced cycle time (2 rotations vs 8–16 traditional)

**3.1.3: Timing Controller & Cam Profiles**
- Map main shaft angle (0–360°) → event dispatch
- Implement phase transitions (column latch, addition, carry, print, stereo)
- Test: Event sequence matches SMG timing appendix
- Validation: One full cycle = correct number of additions + carries

**3.1.4: Integration Test**
- Execute simple difference table (e.g., x², x²+x, x²+x+1)
- Compare against hand calculation
- Validate: Results match expected values

### Phase 3.2: I/O Subsystem (Weeks 3–5)

**3.2.1: Printer Apparatus**
- Implement `PrinterApparatus` (type setter, inking, hammer)
- Map column values → digit character (0–9)
- Test: Type setting sequence for 8-digit line
- Validation: Against SMG printer drawing synchronization

**3.2.2: Stereotype Frame**
- Implement `StereotypeFrame` (X/Y positioning, mold accumulation)
- Test: Mold extraction after page complete
- Validation: Mold image dimensions (8 positions × ~50 lines)

**3.2.3: Card Reader (AE)**
- Implement `CardReader` with punch card parsing
- Support operation, variable, number card types
- Test: Parse Lovelace Note G card deck (Bernoulli)
- Validation: Against Menabrea/Lovelace descriptions

**3.2.4: Integration Test**
- DE2: Print a full table (polynomial values)
- AE: Execute Bernoulli sequence card deck → print results

### Phase 3.3: Debugger (Weeks 5–6)

**3.3.1: Symbol Table & Breakpoints**
- Implement symbol table (IR vars → columns)
- Implement breakpoint engine (condition evaluation)
- Test: Query variable value at specific angle
- Validation: Against expected mechanical state

**3.3.2: Stepper & Snapshots**
- Implement step_angle(), step_phase(), step_cycle()
- Capture `DebugSnapshot` at each step
- Test: Snapshot consistency across steps
- Validation: History replay produces same results

**3.3.3: Variable Inspection**
- Implement print_variables(), inspect_variable()
- Test: Display all variables at breakpoint
- Validation: Against symbolic notation (Babbage 1826)

**3.3.4: Integration Test**
- Set breakpoint on "main_shaft_angle == 240" (print phase)
- Inspect all column values at breakpoint
- Step through print cycle; validate hammer strike timing

### Phase 3.4: Testing & Validation (Weeks 7–8)

**3.4.1: Emulator Unit Tests (50+ tests)**
- Column arithmetic: add, carry, anticipation
- Timing: event sequence per cycle
- State transitions: latch/release per phase

**3.4.2: I/O Integration Tests (30+ tests)**
- Printer: digit → character mapping, line formatting
- Stereotyper: mold accumulation, extraction
- Card reader: operation/variable/number parsing

**3.4.3: Debugger Functional Tests (25+ tests)**
- Breakpoints: condition evaluation, trigger logic
- Stepping: angle/phase/cycle granularity
- Variables: lookup, inspection, display

**3.4.4: End-to-End Tests (20+ tests)**
- DE2 + printer + stereotyper: full pipeline
- AE + card reader + printer: card deck execution
- Debugger + all modules: breakpoint + inspect + step

**Target coverage: >90% (emulator), >85% (I/O), >80% (debugger)**

### Phase 3.5: Documentation & Finalization (Week 9)

**3.5.1: Architecture Document (this file, extended)**
- Finalize component specs
- Document all public APIs
- Add example workflows

**3.5.2: User Guide**
- How to set up emulator (initialize columns, load card deck)
- How to use debugger (set breakpoints, inspect variables, step)
- Example: running Lovelace's Bernoulli program

**3.5.3: Mechanism Reference**
- Cross-link emulator components to SMG drawings
- Timing diagrams (angle vs event)
- Carry propagation state machine

**3.5.4: Commit & Prepare Phase 4**
- Final commit: "Phase 3 Complete: DE2 Emulator, I/O, Debugger"
- Document remaining work (AE full store, plotter, optimization)

================================================================================
PART 6: SUCCESS CRITERIA (PHASE 3)
================================================================================

**Must achieve (Phase 3 completion):**
1. Emulator executes simple DE2 table with correct output
2. Printer produces formatted lines; stereotyper creates molds
3. Debugger allows breakpoint setting and variable inspection
4. 90%+ test coverage (emulator unit tests)
5. End-to-end test: DE2 + printer + stereotyper + debugger

**Should achieve (Phase 3.5+):**
1. AE card interpreter (operation, variable, number cards)
2. Lovelace's Bernoulli sequence executes correctly
3. Mechanical notation parser (future enhancement)
4. Plotter output (curve generation, future)

**Documentation:**
1. All components documented with references to SMG/CHM sources
2. Timing diagrams match official sources ±tolerance
3. User guide with worked examples
4. Test suite exceeds 100 tests across all modules

================================================================================
END SPECIFICATION
================================================================================
