Parameter Citations (Babbage Wave 2 physics model)

Required Babbage fields are source-cited using cached local text artifacts
and engineering handbooks. When a source does not provide an explicit
numeric value, a conservative provisional default is used and marked as such.

Tolerances and Timing
---------------------

- gear_module_mm: provisional default from DE2 manufacturing-tolerance discussion.
  Source: `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 904, 9212).
- backlash_mm: provisional default from DE2 timing-clearance constraints.
  Source: `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 8920-8923, 9420).
- shaft_clearance_mm: provisional default from DE2 shaft/bearing clearance notes.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 5060, 5078).
- bearing_clearance_mm: provisional default from DE2 bearing and clearance discussions.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (line 4382),
  `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 3234, 3241).
- lap_mm: provisional Stephenson-gear baseline with DE2 timing-clearance grounding.
  Source: `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 8920-8931).
- lead_mm: provisional Stephenson-gear baseline with DE2 timing-clearance grounding.
  Source: `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 8920-8931).
- cutoff_pct: provisional cutoff ratio for first-pass timing model.
  Source: `docs/sources/cache/02_ScienceMuseum_DE2_Technical_Online.txt` (lines 8920-8931).

Lubrication
-----------

- viscosity_cSt_40C: lubrication spec and dashpot-oil references.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 2416-2426, 3359-3362).
- schedule_hours: lubrication service frequency.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 3345-3348).
- schedule_cycles_min: lubrication service cycle floor.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 3345-3347).
- schedule_cycles_max: lubrication service cycle ceiling.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 3345-3347).

Card Feed
---------

- feed_rate_cards_per_min: feed mechanism behavior and operator cadence linkage.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 1684-1707, 4532-4534).
- jam_rate_per_1k: jam behavior and cadence-related fault profile.
  Source: `docs/sources/cache/03_ScienceMuseum_DE2_User_Manual.txt` (lines 4525-4528, 4632-4637).

Material Properties (Phase A)
-----------------------------

- brass_properties: CZ121 (CuZn39Pb3) mechanical, thermal, electromagnetic properties.
  Source: AZoM Article 2822; EngineerCalculator CZ121; Shigley's 11th Ed Table A-5;
  ASM Handbook Vol. 2 (nonferrous alloys). Retrieved 2026-02-25.
- steel_properties: S275JR (EN 10025-2) mechanical, thermal, electromagnetic properties.
  Source: Eurocode EN 1993-1-1 Sec 3.2.6; EngineerCalculator S275;
  Shigley's 11th Ed Table A-5; The World Material S275JR. Retrieved 2026-02-25.
- cast_iron_properties: EN-GJL-250 (GG-25) mechanical, thermal, electromagnetic properties.
  Source: SN-CastIron EN-GJL; AZoM Article 783; Modulus Metal GJL-250;
  ASM Handbook Vol. 1 (irons and steels). Retrieved 2026-02-25.
- phosphor_bronze_properties: CuSn8 (C52100) mechanical, thermal, electromagnetic properties.
  Source: copper.org CDA C52100; Concast C52100; AZoM Article 6417;
  ASM Handbook Vol. 2 (nonferrous alloys). Retrieved 2026-02-25.
- spring_steel_properties: C67S (EN 10132-4 / ~AISI 1065) mechanical, thermal, electromagnetic properties.
  Source: Virgamet C67S; AZoM AISI 1065 Article 6575;
  Shigley's 11th Ed Table A-5 and A-20. Retrieved 2026-02-25.

Detailed per-material source breakdown with web URLs:
  See docs/simulation/MATERIAL_PROPERTIES.md "Source Attribution" section.

Kinematic Analysis (Phase B)
----------------------------

- Gear mesh theory: velocity ratio, contact ratio, pitch line velocity.
  Source: Shigley's Mechanical Engineering Design, 11th Ed, Ch. 13.
- Lewis bending stress equation and form factors.
  Source: Shigley's 11th Ed, Ch. 14, Table 14-2.
- Modified trapezoidal cam profile: displacement, velocity, acceleration, jerk.
  Source: Norton, Design of Machinery, 6th Ed; Shigley's 11th Ed, Ch. 3.
- Grubler-Kutzbach criterion for DOF analysis.
  Source: Norton, Design of Machinery, 6th Ed, Ch. 2.

Thermodynamic Model (Phase C)
------------------------------

- Bearing friction heat: Q = 0.5 * mu * W * d * omega.
  Source: Shigley's 11th Ed, Ch. 12 (journal bearings).
- Gear mesh efficiency and heat generation: Q = P * (1 - eta).
  Source: Shigley's 11th Ed, Ch. 13.
- Thermal expansion: delta_L = alpha * L * delta_T.
  Source: Standard thermodynamics reference; material-specific alpha values
  from Phase A material property tensors.
- Thermal time constant: tau = m * c_p / (h * A).
  Source: Incropera & DeWitt, Fundamentals of Heat and Mass Transfer, Ch. 5.

Electromagnetic Effects (Phase D)
----------------------------------

- Eddy current loss: P = (pi^2 * B^2 * d^2 * f^2 * V_vol) / (6 * rho_e).
  Source: Griffiths, Introduction to Electrodynamics, 4th Ed.
  At B=50 uT (Earth's field), f=0.5 Hz (30 rpm): P << 1 mW.
- Galvanic series: brass -0.30V, steel -0.60V, bronze -0.25V (SCE).
  Source: ASM Handbook Vol. 13A (Corrosion); Fontana, Corrosion Engineering.
- Electrical resistivity: material-specific values from ASM Handbooks.
- Magnetic permeability: brass/bronze mu_r~1, steel mu_r~100-200,
  cast iron mu_r~60-100.
  Source: Shigley's 11th Ed appendix; CRC Handbook of Chemistry and Physics.

Tribology and Wear (Phase E)
------------------------------

- Archard wear equation: V_wear = K * F_N * s / H.
  Source: Shigley's 11th Ed, Ch. 12; Rabinowicz, Friction and Wear of Materials.
  K ~ 10^-4 for lubricated brass-on-steel.
- Plain bearing PV limits: P*V < PV_limit (~10 MPa*m/s lubricated bronze-on-steel).
  Source: Shigley's 11th Ed, Ch. 12, Table 12-8.
- Hamrock-Dowson EHL film thickness: h_c = 2.65 * R * U^0.7 * G^0.54 * W'^(-0.13).
  Source: Hamrock, Schmid & Jacobson, Fundamentals of Fluid Film Lubrication.
- Lambda ratio: lambda = h_min / sqrt(Ra_1^2 + Ra_2^2); need lambda > 3 for full-film.
  Source: Shigley's 11th Ed, Ch. 12.
- Surface finish (19th century): Ra ~ 0.8-1.6 um with turning + hand polishing.
  Source: Oberg, Machinery's Handbook, 30th Ed; Science Museum DE2 manufacturing notes.

Structural Analysis (Phase F)
------------------------------

- Euler-Bernoulli beam deflection: delta = F * L^3 / (48 * E * I).
  Source: Shigley's 11th Ed, Ch. 4, Table A-9.
- Goodman fatigue criterion: sigma_a/sigma_e + sigma_m/sigma_u = 1/SF.
  Source: Shigley's 11th Ed, Ch. 6.
- Marin endurance correction factors k_a through k_e.
  Source: Shigley's 11th Ed, Ch. 6, Equations 6-18 through 6-26.
- Euler buckling: P_cr = pi^2 * E * I / (K * L)^2.
  Source: Shigley's 11th Ed, Ch. 4.
- Johnson buckling: for slenderness ratio < ~100 transition point.
  Source: Shigley's 11th Ed, Ch. 4.
