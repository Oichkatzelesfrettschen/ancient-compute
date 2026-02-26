# Valve Timing Analysis

**Date**: 2026-02-26
**Status**: Phase 3 -- Provisional parameters documented, validation tool created

---

## Overview

The Analytical Engine's operational envelope depends on accurate valve timing
for the steam drive mechanism. Three parameters control steam distribution:
**lap**, **lead**, and **cutoff**. All three are currently provisional.

## Parameter Inventory

| Parameter | Value | Unit | Status | Source |
|-----------|-------|------|--------|--------|
| lap_mm | 3.0 | mm | PROVISIONAL | DE2 Technical Online (lines 8920-8931) |
| lead_mm | 1.0 | mm | PROVISIONAL | DE2 Technical Online (lines 8920-8931) |
| cutoff_pct | 60.0 | % | PROVISIONAL | DE2 Technical Online (lines 8920-8931) |

## Definitions

- **Lap**: Overlap between valve face and port edge at mid-travel. Determines
  the dead-band (crank angle range) before steam admission begins. Prevents
  blow-through and controls timing precision.

- **Lead**: Amount the valve is already open at dead center. Pre-admits steam
  before the piston reverses direction, compensating for port and passage
  volume. Essential for maintaining power at operating speed.

- **Cutoff**: Fraction of the piston stroke at which steam admission ends and
  expansion begins. Lower cutoff = higher expansion ratio = better efficiency
  but lower torque. Period engines operated 25-75% depending on load.

## Stephenson Link Motion

The valve gear is a Stephenson link motion, which:
1. Converts crankshaft rotation to reciprocating valve slide motion
2. Allows variable cutoff via the expansion link position
3. Provides forward/reverse operation

The relationship between crank angle (theta) and valve displacement (x) is:
```
x(theta) = e * sin(theta + alpha) + lap
```
where `e` is the eccentric throw and `alpha` is the angular advance.

Port events occur at:
- **Admission**: x > lap (valve opens, steam enters)
- **Cutoff**: x returns to lap (at cutoff angle, expansion begins)
- **Release**: x < -lap (exhaust port opens)
- **Compression**: x returns to -lap (exhaust closes, compression begins)

## Timing Flow in Codebase

```
TIMING_PROVISIONAL.yaml (source of truth)
    |
sim_schema.yaml (valve_gear_params section)
    |
SimulationConfig (NOT YET CONNECTED -- gap)
    |
SimulationEngine.step() (shaft angle tracked but no valve events)
    |
TimingController (8 mechanical phases, no steam coupling)
```

## Confirmed vs Provisional

12 of 20 timing parameters are confirmed from primary sources:
- Phase widths (45-degree intervals): Bromley 1990
- Carry phase boundaries: Babbage mechanical notation
- Look-ahead positions: Babbage anticipating carriage design
- Barrel timing allocations: Derived from barrel step counts
- Nominal RPM (30): Babbage correspondence

8 remain provisional:
- Valve gear: lap, lead, cutoff
- Steam drive: stroke, pressure, efficiency, power
- Carry timing: degrees_per_digit

## Validation

Run the provenance validator:
```
python3 tools/simulation/validate_timing_provenance.py
```

## Required Primary Sources

To confirm provisional values, the following sources would be needed:
1. Physical measurements from Science Museum DE2 mechanism
2. Babbage's mechanical notation drawings for valve gear linkage
3. Babbage/Clement correspondence on piston dimensions
4. Period steam engine specifications for comparable power ratings

## References

- Science Museum Group, "Difference Engine No. 2 Technical Description"
- Bromley, A.G. (1990), "Difference and Analytical Engines"
- Babbage, C., "On the Mathematical Powers of the Calculating Engine"
- docs/simulation/TIMING_PROVISIONAL.yaml (parameter definitions)
- docs/simulation/CITATIONS.md (citation index)
