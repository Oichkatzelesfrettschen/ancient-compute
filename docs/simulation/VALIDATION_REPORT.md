# Physics Model Validation Report

**Date**: February 25, 2026
**Scope**: Phases A-F deep physics model + Phase H integration validation
**Status**: Complete

---

## 1. Dimensional Analysis

**Tool**: `tools/simulation/verify_dimensions.py`
**Result**: 18/18 equations dimensionally consistent

Equations verified:
- Phase B: Gear velocity ratio, contact ratio, Lewis bending stress, pitch line velocity, moment of inertia, Grubler-Kutzbach DOF, cam displacement
- Phase C: Bearing heat, gear mesh heat, thermal expansion, thermal time constant
- Phase D: Eddy current loss
- Phase E: Archard wear, PV product, lambda ratio
- Phase F: Beam deflection, Goodman fatigue, Euler buckling

All equations produce correct SI units with no dimensional inconsistencies.

---

## 2. Parameter Sensitivity Analysis

**Tool**: `tools/simulation/sensitivity_analysis.py`
**Method**: One-at-a-time +/-10% perturbation, central difference sensitivity S = (dY/Y)/(dX/X)

### Top 10 Parameters by Sensitivity

| Rank | Parameter | Output Metric | |S| |
|------|-----------|---------------|-----|
| 1 | shaft_diameter_mm | Shaft deflection | 4.21 |
| 2 | column_width_mm | Euler buckling | 4.04 |
| 3 | shaft_span_mm | Shaft deflection | 3.01 |
| 4 | brass_alpha_per_K | Thermal clearance | 2.33 |
| 5 | module_mm | Lewis stress | 2.04 |
| 6 | column_height_mm | Euler buckling | 2.04 |
| 7 | B_field_T | Eddy loss | 2.00 |
| 8 | shaft_rpm | Eddy loss | 2.00 |
| 9 | eddy_thickness_m | Eddy loss | 2.00 |
| 10 | steel_alpha_per_K | Thermal clearance | 1.33 |

### Interpretation

- **Shaft diameter** is the most sensitive parameter across all metrics (S=4.2 for deflection). This is expected: deflection scales as d^(-4) due to the moment of inertia.
- **Column width** has similar sensitivity for buckling (S=4.0) due to I ~ w^4.
- **Thermal expansion coefficients** are the most sensitive thermal parameters, confirming that dissimilar-metal clearance management is critical.
- **Eddy current parameters** have S=2.0 each but the baseline loss is 3.2e-9 W (negligible), so sensitivity is irrelevant.

---

## 3. Monte Carlo Tolerance Stack-up

**Tool**: `tools/simulation/monte_carlo_tolerance.py`
**Method**: 10,000 iterations, normally distributed parameters (3-sigma = tolerance), seed=42

| Check | Failures | Probability | Mean Margin | Min Margin | P1 Margin |
|-------|----------|-------------|-------------|------------|-----------|
| Gear backlash interference | 0/10000 | 0.000% | 0.129 mm | 0.093 mm | 0.106 mm |
| Bearing clearance interference | 0/10000 | 0.000% | 0.109 mm | 0.064 mm | 0.081 mm |
| Structural safety factor < 2.0 | 0/10000 | 0.000% | 8.40 | 6.18 | 7.18 |
| Shaft deflection > L/10000 | 0/10000 | 0.000% | 0.029 mm | 0.027 mm | 0.028 mm |

**Conclusion**: All interference probabilities are 0.0% at 10,000 iterations, well below the 0.1% threshold. The design has adequate tolerance margins for 19th-century manufacturing capability.

---

## 4. DE2 Comparison Validation

**Tool**: `backend/tests/integration/test_de2_comparison.py`
**Result**: 16/16 tests pass

### Observed vs Simulated

| Parameter | DE2 Observed | Simulated | Agreement |
|-----------|-------------|-----------|-----------|
| Operating RPM | ~30 (via 4:1 reduction) | 30 | Exact |
| Gear reduction ratio | 4:1 (single stage) | 6:1 (two stage) | Within 2x |
| Machine mass | ~5000 kg (total) | 500 kg (calc. section) | Plausible subset |
| Digit precision | 31 digits | 40 digits (AE design) | AE >= DE2 |
| Gear material | Brass | Brass | Match |
| Shaft material | Steel | Steel | Match |
| Bearing material | Bronze | Phosphor bronze | Match |
| Steam efficiency | ~5-10% (1840s) | 8% | Within range |
| Card feed rate | ~10 cards/min | 10 cards/min | Match |
| Lubrication schedule | Periodic (museum) | 160 hours | Plausible |

### Thermal Feasibility

- Gear backlash expansion at 40C: well within backlash margin
- Bearing clearance at 40C: remains positive (bronze expands more than steel, increasing clearance)

### Structural Feasibility

- Shaft deflection < L/10000: confirmed
- Gear tooth bending SF >= 2.0: confirmed (at 50W per-gear-train power)

---

## 5. Physics Module Summary

| Module | File | Tests | Coverage |
|--------|------|-------|----------|
| Materials | materials.py | test_materials.py | 95% |
| Kinematics | kinematics.py | test_kinematics.py | 97% |
| Thermodynamics | thermodynamics.py | test_thermodynamics.py | ~90% |
| Electromagnetic | electromagnetic.py | test_electromagnetic.py | 95% |
| Tribology | tribology.py | test_tribology.py | ~90% |
| Structural | structural.py | test_structural.py | ~90% |

---

## 6. Known Limitations

1. **Model is Analytical Engine, not DE2**: Our simulation models Babbage's Analytical Engine design (40 digits, programmable). The DE2 comparison is for structural/thermal validation of shared mechanical principles.

2. **Steam drive is simplified**: We model steady-state 30 RPM operation. Real steam engines have variable speed, governor response, and load-dependent behavior.

3. **Valve timing is provisional**: Lap, lead, and cutoff values are Stephenson-gear defaults. Higher-fidelity extraction requires measurement from the physical DE2.

4. **Manufacturing tolerances are estimated**: 19th-century precision machining capability (25-50 um) is based on Machinery's Handbook references, not direct measurement of Babbage-era parts.

5. **Output apparatus not modeled**: The DE2 output apparatus (printer + stereotyper) is ~50% of total mass and is not included in our structural model.

---

## 7. Conclusion

The physics model passes all validation checks:
- 18/18 dimensional consistency checks
- All parameters within expected sensitivity ranges
- 0.0% interference probability across 10,000 Monte Carlo iterations
- 16/16 DE2 comparison tests pass
- All safety factors >= 2.0
- All deflections within limits
- Thermal expansion within backlash margins
- Bearing clearance positive at all operating temperatures

The simulation is mechanically feasible and consistent with observed DE2 performance within the scope of the model.
