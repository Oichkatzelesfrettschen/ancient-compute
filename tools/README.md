# tools/

Development and analysis tools for the Ancient Compute project. These are standalone
scripts -- not part of the installed package. Run from the project root unless noted.

## Top-level tools

| File | Purpose |
|------|---------|
| `babbage_emulator.py` | Thin shim: runs the Babbage Analytical Engine from the command line without the full CLI stack. Useful for quick manual tests. |
| `card_compiler.py` | Card Standard compiler: encodes/decodes instruction cards per OPCODES.yaml. Canonical source for the Opcode enum -- keep in sync with docs/hardware/OPCODES.yaml. |
| `todo_report.py` | Generates a markdown summary of open TODO comments across the backend source tree. |
| `validate_bom.py` | Validates hardware_twin/bom/bom_v0.csv against the BOM schema. Run via `make bom-validate`. |
| `validate_opcodes.py` | Asserts OPCODES.yaml and the Opcode enum in card_compiler.py define the same names and count. Run via `make verify` (step 6/6). |

## Subdirectories

### antikythera/

Research tools for the Antikythera Mechanism curriculum module.

- `extract_draconic_gearing.py` -- extracts draconic-cycle gear ratios from the
  mechanism's known tooth counts. Used to generate curriculum content for Volume 1.

### astrolabe/

Research tools for the medieval astrolabe curriculum module.

- `ocr_chaucer_tables.py` -- OCRs and parses numerical tables from Chaucer's
  Treatise on the Astrolabe (c. 1391). Produces structured CSV for content modules.

### experiments/

Exploratory benchmarks and micro-tests. Not part of CI. See experiments/README.md.

- `carriage_benchmark.py` -- measures carriage position update throughput
- `logistic_map_babbage.py` -- runs the logistic map on the Babbage emulator
- `micro_mult_test.py` -- micro-benchmarks for MULT barrel instruction

### quipu/

Research tools for the Incan quipu curriculum module.

- `parse_kfg.py` -- parses KFG (Khipu Field Guide) encoded quipu data files
  into structured records for use in curriculum content.

### simulation/

Physics and emulator analysis scripts. All require `PYTHONPATH=.` from project root.

| Script | Purpose |
|--------|---------|
| `bom_cost_model.py` | Cost sensitivity model from BOM quantities and material costs |
| `extract_params.py` | Extracts physics parameters from emulator state for analysis |
| `monte_carlo_tolerance.py` | Monte Carlo tolerance analysis (7 output dimensions) |
| `operational_envelope.py` | Maps safe operating region across RPM and load space |
| `sensitivity_analysis.py` | One-at-a-time sensitivity analysis (11 output dimensions) |
| `simulate.py` | Runs a full simulation and dumps state at each step |
| `validate_timing_provenance.py` | Checks timing constants against primary sources in SOURCE_MAP.md |
| `verify_babbage_params.py` | Verifies physics parameters match Babbage design values |
| `verify_dimensions.py` | Dimensional analysis checks (35 dimensions) |

### sources/

Tools for fetching and managing primary source materials.

- `fetch_wikipedia_uploads.py` -- downloads Wikipedia upload files referenced in
  docs/sources/SOURCE_MAP.md for local archival.

## Usage Example

```bash
# Validate BOM
PYTHONPATH=. python3 tools/validate_bom.py

# Validate opcode sync
PYTHONPATH=. python3 tools/validate_opcodes.py

# Run dimensional analysis
PYTHONPATH=. python3 tools/simulation/verify_dimensions.py

# Run Monte Carlo tolerance analysis
PYTHONPATH=. python3 tools/simulation/monte_carlo_tolerance.py
```
