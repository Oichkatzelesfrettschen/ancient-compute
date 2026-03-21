# CARD_FORMATS.md

Card format specification for all three historical punch card families
emulated in ancient_compute.

---

## Overview

Three distinct punch card families are implemented:

| Family | Machines | Format | Designer Class |
|--------|----------|--------|---------------|
| Hollerith | `hollerith-tabulator` | 12 rows x 80 cols, hole-per-attribute | `HollerithCardDesigner` |
| Jacquard | `jacquard-loom` | N-hook binary rows | `JacquardCardDesigner` |
| AE | `analytical-engine` | Assembly source text | `AECardDesigner` |

All designers live in `backend/src/emulator/card_designer.py`.
The CLI tool is at `tools/card_designer.py`.

---

## 1. Hollerith Card Format

### History

Herman Hollerith invented the electromechanical punched-card tabulator for the
1890 US Census. The original card was 12 rows x 24 columns; the IBM 026/029
standard expanded this to 12 rows x 80 columns.

### Row / Column Numbering

Rows are numbered by the Hollerith zone/digit scheme:

| Index | Name | Typical Use |
|-------|------|-------------|
| 0 | Zone 12 | Upper zone punch (A-I) |
| 1 | Zone 11 | Middle zone punch (J-R) |
| 2 | Zone 0 | Lower zone punch (S-Z, digit 0) |
| 3 | Digit 1 | Numeric row 1 |
| 4 | Digit 2 | Numeric row 2 |
| 5 | Digit 3 | Numeric row 3 |
| 6 | Digit 4 | Numeric row 4 |
| 7 | Digit 5 | Numeric row 5 |
| 8 | Digit 6 | Numeric row 6 |
| 9 | Digit 7 | Numeric row 7 |
| 10 | Digit 8 | Numeric row 8 |
| 11 | Digit 9 | Numeric row 9 |

Columns are 0-indexed from left to right (0..79 for a standard 80-col card).

### Character Encoding (IBM 026/029)

| Char | Rows | Char | Rows |
|------|------|------|------|
| A | 12, 1 | J | 11, 1 |
| B | 12, 2 | K | 11, 2 |
| C | 12, 3 | L | 11, 3 |
| D | 12, 4 | M | 11, 4 |
| E | 12, 5 | N | 11, 5 |
| F | 12, 6 | O | 11, 6 |
| G | 12, 7 | P | 11, 7 |
| H | 12, 8 | Q | 11, 8 |
| I | 12, 9 | R | 11, 9 |
| S | 0, 2 | 0 | 0 |
| T | 0, 3 | 1 | 1 |
| U | 0, 4 | 2 | 2 |
| V | 0, 5 | 3 | 3 |
| W | 0, 6 | 4 | 4 |
| X | 0, 7 | 5 | 5 |
| Y | 0, 8 | 6 | 6 |
| Z | 0, 9 | 7 | 7 |
|   |      | 8 | 8 |
|   |      | 9 | 9 |

Rows in the table above use Hollerith names (12, 11, 0, 1-9), not 0-based
indices. Use the index table above to convert.

### Payload JSON Schema

```json
{
  "payload": {
    "cards": [
      {"row": <col_index>, "columns": [<row_index>, ...]},
      ...
    ]
  }
}
```

Note: the field name `row` in the payload refers to the card's *column* index
(a historical naming quirk in the tabulator implementation). `columns` lists
the row indices within that card column that are punched.

Only non-empty columns appear in the payload.

### Python API

```python
from backend.src.emulator.card_designer import HollerithCardDesigner

d = HollerithCardDesigner(cols=80)
d.encode_text("HELLO WORLD", start_col=0)
payload = d.to_payload()
# -> {"cards": [{"row": 0, "columns": [0, 10]}, ...]}

print(d.render_ascii())  # 12-row ASCII grid
print(d.to_json())       # {"payload": {"cards": [...]}}
```

### CLI

```
python3 tools/card_designer.py hollerith "HELLO WORLD" --ascii
python3 tools/card_designer.py hollerith "HELLO" --cols 40 --out card.json
```

### Round-Trip Example

```python
from backend.src.emulator.card_designer import HollerithCardDesigner

d = HollerithCardDesigner()
d.encode_text("AB")
payload = d.to_payload()

# Load into Hollerith tabulator via API:
# POST /machines/hollerith-tabulator/load
# Body: {"payload": <payload>}
```

---

## 2. Jacquard Card Format

### History

Joseph Marie Jacquard (1752-1834) introduced punched-card control of loom
warp threads in 1804. Each card row has N holes, one per hook: a hole = 1
(hook raised, thread lifted); no hole = 0 (hook lowered).

### Format

Each card is a binary vector of length `hooks`. A deck is a sequence of cards
processed one per weft row. Hooks are indexed 0..N-1 from left to right.

### Payload JSON Schema

```json
{
  "payload": {
    "cards": [
      [1, 0, 1, 0, 1, 0, 1, 0],
      [0, 1, 0, 1, 0, 1, 0, 1],
      ...
    ]
  }
}
```

Values are integers 0 or 1.

### Python API

```python
from backend.src.emulator.card_designer import JacquardCardDesigner

d = JacquardCardDesigner(hooks=8)
d.add_card([1, 0, 1, 0, 1, 0, 1, 0])
d.add_card([0, 1, 0, 1, 0, 1, 0, 1])
d.stripe(4)           # add 4 alternating twill cards
d.add_row_from_string("11001100")  # from binary string

payload = d.to_payload()
# -> {"cards": [[1,0,1,0,1,0,1,0], ...]}
```

### CLI

```
python3 tools/card_designer.py jacquard "10101010" "01010101" --ascii
python3 tools/card_designer.py jacquard --stripe 8 --hooks 16 --out deck.json
```

### Round-Trip Example

```python
from backend.src.emulator.card_designer import JacquardCardDesigner
from backend.src.emulator.jacquard import JacquardLoom

d = JacquardCardDesigner(hooks=8)
d.add_card([1, 0, 1, 0, 1, 0, 1, 0])
d.add_card([0, 1, 0, 1, 0, 1, 0, 1])

loom = JacquardLoom(num_hooks=8)
loom.load_deck(d.to_payload()["cards"])
loom.step()  # weaves first card row
assert loom.get_current_card() == [1, 0, 1, 0, 1, 0, 1, 0]
```

---

## 3. AE Card Format

### History

The Analytical Engine (Babbage/Lovelace, 1843) used three classes of punch
cards: Operation cards, Variable cards, and Number cards. Ada Lovelace's
Note G (1843) contains the first algorithm written for the AE.

### Format

The AE API accepts human-readable assembly source text. The card compiler
(`tools/card_compiler.py`) can round-trip between source text and binary card
encodings, but the API load handler (`program_input_type: assembly`) takes
source text directly.

### Payload JSON Schema

```json
{
  "payload": {
    "source": "<assembly source text>"
  }
}
```

### Assembly Syntax Reference

See `docs/specifications/OPCODES.yaml` for the full opcode list. Core opcodes:

| Opcode | Description |
|--------|-------------|
| `LOAD A, N` | Load immediate N into register A |
| `LOAD A, [addr]` | Load memory[addr] into A |
| `STOR A, [addr]` | Store A into memory[addr] |
| `ADD A, B` | A = A + B |
| `SUB A, B` | A = A - B |
| `MULT A, B` | A = A * B |
| `DIV A, B` | A = A / B (remainder in D) |
| `WRPRN A` | Print A to result tape |
| `HALT` | Stop execution |
| `JMP addr` | Unconditional jump |
| `JZ addr` | Jump if zero flag set |
| `JN addr` | Jump if negative flag set |

### Python API

```python
from backend.src.emulator.card_designer import AECardDesigner

d = AECardDesigner("LOAD A, 7\nLOAD B, 6")
d.append_instruction("MULT A, B")
d.append_instruction("WRPRN A")
d.append_instruction("HALT")

payload = d.to_payload()
# -> {"source": "LOAD A, 7\nLOAD B, 6\nMULT A, B\nWRPRN A\nHALT"}
```

### CLI

```
python3 tools/card_designer.py ae --source "LOAD A, 5\nHALT" --ascii
python3 tools/card_designer.py ae < program.deck --out payload.json
```

### Round-Trip Example

```python
from backend.src.emulator.card_designer import AECardDesigner
from backend.src.emulator.analytical_engine import Engine

source = "LOAD A, 7\nLOAD B, 6\nMULT A, B\nWRPRN A\nHALT"
d = AECardDesigner(source)
payload = d.to_payload()

engine = Engine()
engine.load_program_from_text(payload["source"])
engine.run()
# engine.result_cards contains the WRPRN output
```

---

## 4. Extending with a Fourth Format

To add a new card format (e.g. Colossus 5-channel punched tape):

1. Create a `ColossusCardDesigner(CardDesigner)` class in `card_designer.py`.
2. Implement `to_payload()` returning the format expected by the Colossus
   `lorenz_tape` load handler (currently `{"tape": [[bit5], ...]}`).
3. Implement `render_ascii()` showing the 5-bit rows.
4. Add a `colossus` subcommand to `tools/card_designer.py`.
5. Add tests in `backend/tests/unit/test_card_designer.py`.
6. Update this document with a new section.

---

## References

- Hollerith, H. (1889). An Electric Tabulating System. PhD thesis, Columbia.
- Austrian, G.D. (1982). Herman Hollerith. Columbia UP.
- Jacquard, J.M. (1804). Patent on punched-card loom control, France.
- Babbage, C. / Lovelace, A. (1843). Sketch of the Analytical Engine, Note G.
- Napier, J. (1617). Rabdologia. Edinburgh.
