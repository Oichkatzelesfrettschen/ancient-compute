# Power Train Specification

**Version**: 0.1
**Date**: 2026-02-26
**Charter ref**: S-5 (Non-electric prime mover)

---

## 1. Prime Mover Options

| Option | Mechanism | Torque | Duration | Complexity | Source |
|--------|-----------|--------|----------|------------|--------|
| Hand crank | Direct drive via reduction gear | ~5 N-m | Unlimited (operator dependent) | Low | ASSUMPTION |
| Spring barrel | Coiled mainspring with fusee | ~3 N-m | 15-30 min per wind | Medium | SOURCE:SWADE-2001 |
| Weight drum | Falling weight on cable/chain | ~10 N-m | Proportional to drop height | Medium | SOURCE:SWADE-2001 |
| Steam | Low-pressure reciprocating engine | ~20 N-m | Unlimited (fuel supply) | High | ASSUMPTION |

### 1.1 Baseline: Hand Crank

The hand crank is the mandatory baseline. All other prime movers are optional
upgrades that must be mechanically compatible with the same input shaft.

| Parameter | Value | Source |
|-----------|-------|--------|
| Crank handle radius | 200 mm | ASSUMPTION: Ergonomic standard |
| Input shaft RPM (target) | 30 | ASSUMPTION: Comfortable hand-crank rate |
| Required input torque | 5 N-m max | ASSUMPTION: Based on estimated friction |
| Gear ratio (crank to main shaft) | 4:1 | ASSUMPTION: Reduces effort |
| Effective main shaft RPM | 7.5 | Derived from 30/4 |

### 1.2 Spring Barrel (Optional)

| Parameter | Value | Source |
|-----------|-------|--------|
| Spring material | High-carbon steel (clock spring) | ASSUMPTION |
| Barrel diameter | 150 mm | ASSUMPTION |
| Run time per wind | 15-30 minutes | ASSUMPTION |
| Fusee equalization | Conical fusee, gut line | SOURCE:SWADE-2001 (clock mechanism precedent) |

## 2. Governor

A centrifugal governor maintains constant main shaft speed regardless
of load variation (e.g., carry propagation vs idle).

| Parameter | Value | Source |
|-----------|-------|--------|
| Type | Watt centrifugal governor | SOURCE:SWADE-2001 |
| Speed setpoint | 30 RPM (main shaft) | ASSUMPTION |
| Regulation band | +/- 5% | ASSUMPTION |
| Feedback mechanism | Throttle valve (steam) or friction brake (crank) | ASSUMPTION |

## 3. Torque Limiter

Protects the mechanism from jamming damage.

| Parameter | Value | Source |
|-----------|-------|--------|
| Type | Shear pin or slip clutch | ASSUMPTION |
| Trip torque | 15 N-m (3x nominal) | ASSUMPTION |
| Reset method | Replace shear pin or re-engage clutch | ASSUMPTION |

## 4. Flywheel

Smooths torque pulses from the prime mover and stores energy for
high-torque carry propagation phases.

| Parameter | Value | Source |
|-----------|-------|--------|
| Material | Cast iron | SOURCE:SWADE-2001 |
| Diameter | 600 mm | ASSUMPTION |
| Mass | ~25 kg | ASSUMPTION |
| Moment of inertia | ~1.1 kg-m2 | Derived: 0.5 * m * r^2 |
| Energy stored at 30 RPM | ~5.4 J | Derived: 0.5 * I * omega^2 |

## 5. References

- [CHARTER.md](../program/CHARTER.md) -- Decision D-2 (no electric components)
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- Source traceability
- [CAM_TIMING_SPEC.md](CAM_TIMING_SPEC.md) -- Timing driven by main shaft
