# Operational Envelope & Bill of Materials

**Date**: 2026-02-26
**Status**: Complete
**Phase**: 11 of Debt Resolution Roadmap

---

## Overview

The operational envelope tools characterize the Analytical Engine simulation across
its intended operating range (10-120 RPM), identifying failure modes, thermal limits,
and wear progression. The BOM tool estimates subsystem masses from the physics model
geometry and material library.

---

## Operational Envelope Sweep

### Tool

```
PYTHONPATH=. python3 tools/simulation/operational_envelope.py [--max-hours 100]
```

Or via Makefile:

```
make physics-envelope
```

### Output Format

CSV with columns:

| Column | Unit | Description |
|--------|------|-------------|
| rpm | rev/min | Shaft speed |
| max_temp_C | Celsius | Peak temperature at end of run or failure |
| failure_time_s | seconds | Time to first component failure (inf = survived) |
| limiting_component | string | Component that failed first (none = survived) |
| max_wear_mm3 | mm^3 | Maximum bearing wear volume |
| shaft_deflection_mm | mm | Static shaft deflection |
| gear_backlash_mm | mm | Gear backlash |
| lubrication_regime | string | boundary / mixed / hydrodynamic |
| energy_consumed_J | Joules | Total energy consumed |
| steps | count | Simulation timesteps executed |

### Physics Observed

At 30 RPM (nominal operating speed):
- Lubrication regime: boundary (expected for low-speed journal bearings per Hamrock-Dowson)
- Temperature rise: moderate (~0.1-0.4 C/hour depending on cooling)
- Wear: linear accumulation via Archard equation
- No component failure within 100 hours

At 100+ RPM:
- Higher temperature (proportional to friction power)
- Higher wear rate
- Potential failure from bearing clearance or temperature limit

---

## Bill of Materials

### Tool

```
PYTHONPATH=. python3 tools/simulation/bom_cost_model.py
```

### Sample Output

```
Subsystem                 Material              Count    Unit kg   Total kg
---------------------------------------------------------------------------
Main shaft                steel                     1     23.120     23.120
Journal bearings          phosphor_bronze           4      0.456      1.825
Spur gears                brass                     8      0.249      1.996
Frame (cast iron)         cast_iron                 1    300.000    300.000
Digit wheels              brass                 50000      0.030   1496.773
---------------------------------------------------------------------------
TOTAL                                                              1823.713
```

### Notes

- Frame mass estimated as 60% of total machine mass (500 kg default)
- Digit wheel count: 1000 columns x 50 digits per column (Babbage's plan)
- Bearing dimensions derived from shaft diameter + 5mm wall thickness
- Gear dimensions from tooth count x module

### Historical Context

Babbage estimated the full Analytical Engine would weigh several tons. The
Science Museum's Difference Engine No. 2 weighs approximately 2.6 tonnes
(5,000 brass parts). Our BOM estimate of ~1.8 tonnes for the computing
subsystem (excluding the printing apparatus, card readers, and supporting
framework) is within the expected range for the mill and store alone.

---

## Related Files

- `tools/simulation/operational_envelope.py` -- RPM sweep tool
- `tools/simulation/bom_cost_model.py` -- Mass/cost estimator
- `backend/src/emulator/simulation/engine.py` -- SimulationEngine
- `backend/src/emulator/simulation/state.py` -- SimulationConfig
- `backend/src/emulator/materials.py` -- MaterialLibrary
- `docs/babbage_engine/OPCODE_PHYSICS_COUPLING.md` -- Opcode-coupled physics
- `docs/babbage_engine/PHYSICS_MODEL_INVENTORY.md` -- Physics equation catalog
