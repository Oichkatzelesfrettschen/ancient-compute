# UNIFIED SYNTHESIS: PHYSICS FIDELITY & HYPERGRANULAR DEBT RESOLUTION

**Date:** 2026-02-25
**Scope:** Maximal recursive depth synthesis of the Analytical Engine's physics model (hardware, materials, kinematics, electromagnetics) combined with a rigorous, exhaustive resolution of all structural, test, and dependency debt.
**Objective:** Elevate `ancient_compute` to an academically rigorous, publication-ready standard. Harmonize theoretical mechanics with precise code implementations. 

---

## 1. VISION AND PHILOSOPHY

Drawing upon the combined logic of a Vulcan, the resilience of a Klingon, the wisdom of a Jedi, and profound human ingenuity, this roadmap transforms the Babbage Engine emulator from a skeletal logic model into a **hyper-accurate physical simulation**. We will implement complete mechanical, thermal, kinematic, and electromagnetic property tensors, while simultaneously eliminating all technical, structural, and testing debt.

*No workarounds. No hardcoded shortcuts. Fully validated, testable implementations.*

---

## 2. THE PHYSICS & HARDWARE SYNTHESIS (Tier 1)

### Phase A: Deep Materials Science
Expand `sim_schema.yaml` with precise mechanical/thermal property tensors sourced from ASM Handbooks and Shigley's Mechanical Engineering Design.
*   **A.01-A.06:** Source 13+ properties (Young's Modulus, Poisson's ratio, Yield/Ultimate strength, Thermal expansion, Heat capacity, Hardness) for: Brass CZ121, Mild Steel EN10025, Cast Iron GG-25, Phosphor Bronze CuSn8, and Spring Steel EN10132.
*   **A.07-A.12:** Engineer the YAML schema structure and integrate the properties.
*   **A.13-A.16:** Implement `backend/src/emulator/materials.py` (`MaterialLibrary`, `MaterialProperties`) and strictly validate with `backend/tests/unit/test_materials.py`.
*   **A.17:** Document all citations in `docs/simulation/CITATIONS.md`.

### Phase B: Advanced Kinematics & Linkages
Compute angular velocities, contact ratios, Lewis bending stresses, and cam profiles.
*   **B.01-B.05:** Map the kinematic chain (main shaft -> gear trains -> columns) and define equations (contact ratios, Lewis stress, modified trapezoidal cams, Grubler-Kutzbach criterion).
*   **B.06-B.10:** Implement `backend/src/emulator/kinematics.py` (`GearAnalysis`, `CamAnalysis`, `DOFAnalysis`).
*   **B.11-B.13:** Integrate parameters into `sim_schema.yaml` and establish test coverage ensuring DOF == 1 and contact_ratio >= 1.2.

### Phase C: Thermodynamics & Tribology
Quantify heat generation, thermal expansion, wear, and lubrication.
*   **C.01-C.04:** Implement `thermodynamics.py` (Friction heat, clearance vs. temperature, thermal time constants).
*   **C.05-C.08:** Implement `tribology.py` (Archard wear, PV limits, Hamrock-Dowson EHL film thickness, Lambda ratios).
*   **C.09-C.12:** Rigorous testing of heat output (expected 10-50W) and bearing life logic.

### Phase D: Electromagnetic Interference (EMI)
Rigorously prove and calculate EMI, even if negligible.
*   **D.01-D.03:** Research eddy current losses, galvanic series, and magnetic permeability.
*   **D.04-D.06:** Implement `electromagnetic.py` (`EddyCurrentModel`, `GalvanicCorrosionMatrix`, `StaticChargeModel`).
*   **D.07-D.09:** Validate that eddy current losses are < 1mW and generate a 5x5 galvanic risk matrix.

### Phase E: Structural Integrity
*   **E.01-E.05:** Implement `structural.py` (Euler-Bernoulli shaft deflection, Euler buckling for digit columns, Goodman fatigue life).
*   **E.06-E.08:** Ensure Factor of Safety (SF) >= 2.0 for all gears and SF >= 3.0 for columns. Tests must pass seamlessly.

---

## 3. DEBT ERADICATION & HARMONIZATION (Tier 2)

### Phase F: Structural & Organizational Debt
*   **F.01:** Remove duplicate `BabbageNumber` and consolidate `ColumnSnapshot` in `backend/src/emulator/types.py`.
*   **F.02:** Remove duplicate `MechanicalPhase` enums and consolidate Makefile targets.

### Phase G: Emulator & API Debt
*   **G.01:** Implement full `save_trace()` (JSON trace with PC, opcode, registers).
*   **G.02:** Complete the Division barrel micro-program (remove stub) and IR-to-engine loader.
*   **G.03:** Replace mock DB users in `auth.py` and `execution.py` with functional database queries.
*   **G.04:** Implement missing LISP and IDRIS2 service backends.

### Phase H: Cargo Crate & Dependency Debt
*   **H.01:** Audit workspace dependencies (rationalize `tda`, document `ndarray` duplication).
*   **H.02:** Error Handling Standardization: Enforce `thiserror` for libraries and `anyhow` for binaries. Convert error signatures.
*   **H.03:** Remove blanket `#![allow(dead_code)]` suppressions. Replace with item-level justifications or feature gates.

### Phase I: Comprehensive Test Debt (25% -> 90% Coverage)
*   **I.01:** Clear empty test/benchmark scaffolding.
*   **I.02:** Expand tests for `test_analytical_engine.py`, `test_digit_column.py`, and `test_anticipating_carriage.py`.
*   **I.03:** Un-quarantine broken tests (`test_debugger.py`, `test_phase2_languages.py`) by resolving interface mismatches.

---

## 4. EXECUTION STRATEGY

We will proceed iteratively. Each step requires:
1.  **Scope Validation:** Verify target files and current state.
2.  **Implementation:** Apply the algorithmic or architectural change.
3.  **Testing:** Execute `pytest` or `cargo test` dynamically. Treat all warnings as errors.
4.  **Harmonization:** Ensure seamless alignment with existing framework.

### NEXT IMMEDIATE STEP: EXECUTE PHASE A (Deep Materials Science)
1. Modify `sim_schema.yaml` to include complex tensors for Brass, Cast Iron, Steel, Bronze, and Spring Steel.
2. Implement `backend/src/emulator/materials.py`.
3. Test and Validate.
