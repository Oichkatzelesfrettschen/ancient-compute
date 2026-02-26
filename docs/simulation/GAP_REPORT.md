Gap Report (DE2 vs Simulation)

Status: Updated 2026-02-25 after physics model buildout (Phases A-F).

Closed Gaps
-----------

- Materials: Full mechanical/thermal/electromagnetic property tensors for 5
  construction materials (brass CZ121, steel S275JR, cast iron EN-GJL-250,
  phosphor bronze CuSn8, spring steel C67S). 13+ properties per material with
  units, source citations, and temperature ranges. Validated by MaterialLibrary
  loader and range-check test suite.

- Kinematics: Gear mesh analysis (velocity ratios, contact ratios, Lewis bending
  stress), modified trapezoidal cam profiles with jerk analysis, Grubler-Kutzbach
  DOF verification (M=1). Two-stage gear train instantiated from schema.

- Thermodynamics: Friction heat generation from bearings, gears, and cam
  followers (~10-50 W total). Thermal expansion effects on dissimilar-metal
  clearances (brass alpha=20.5e-6 vs steel alpha=11.7e-6). Thermal time constant
  (~75 min warm-up for 500 kg machine). Operating envelope 10-40 C.

- Electromagnetic: Eddy current losses shown < 1 mW (negligible vs friction).
  5x5 galvanic corrosion risk matrix with mitigation notes. Static charge model
  confirms negligible effects for this machine.

- Tribology: Archard wear volumes for all wearing surfaces. PV products below
  limits for bronze-on-steel journal bearings at 30 rpm. Hamrock-Dowson EHL film
  thickness computed; lambda ratios > 3 confirm full-film lubrication regime.

- Structural: Shaft deflection < L/10000 for 50mm diameter main shaft on 4
  bearing supports. Gear tooth bending SF >= 2.0. Fatigue life via Goodman > 10^8
  cycles. Column buckling SF >= 3. All structural results cross-validated against
  kinematic loads from Phase B.

- Tolerances: provisional defaults are now recorded and source-cited; numeric
  engineering confirmation completed through Monte Carlo tolerance stack-up
  (Phase H pending).

- Lubrication viscosity/schedule: populated and source-cited from DE2 user manual.
  Cross-validated with Archard wear rates and 160-hour lubrication schedule.

- Card feed/jam rates: first-pass feed/jam profile set from operational guidance
  and cadence-linked jam behavior.

Remaining Open Gaps
-------------------

- Valve timing: provisional lap/lead/cutoff values are set with source-cited
  timing-clearance references; higher-fidelity valve data requires primary-source
  measurement from the Science Museum DE2 mechanism.

- Steam drive mapping: rpm/pressure profile remains conservative; calibrate
  against measured load behavior requires physical access to operational DE2.

- Monte Carlo tolerance stack-up: COMPLETED in Phase IX. See
  tools/simulation/monte_carlo_tolerance.py (7/7 checks pass). Manufacturing
  feasibility confirmed across tolerance distributions.

- DE2 comparison validation: quantitative comparison of simulation predictions
  against observed DE2 performance (calculation rate, crank turns, error rate,
  operating temperature, maintenance interval) requires primary-source extraction.

Deferred to Separate Session
----------------------------

- Gate 2 (C freestanding subset completion) and Gate 3 (language promotion) are
  deferred per user decision. This session focused on physics model + non-gate
  debt.
