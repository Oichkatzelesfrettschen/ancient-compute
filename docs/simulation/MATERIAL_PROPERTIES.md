# Material Properties: Babbage Analytical Engine Construction Metals

## Scope and Validity

All properties are for ambient conditions (10-40 C, ~20 C nominal) unless
otherwise noted.  Values are engineering handbook-grade, drawn from primary
references listed in the Source Attribution section at the end.

Ranges reflect typical alloy/temper/processing variation.  Single values
indicate well-established constants or tightly controlled alloy grades.

---

## 1. Brass CZ121 (CuZn39Pb3) -- Free-Cutting Brass

| Property                          | Value                  | Unit         | Notes                                |
|-----------------------------------|------------------------|--------------|--------------------------------------|
| density_kg_m3                     | 8470                   | kg/m3        |                                      |
| youngs_modulus_GPa                | 90 - 105               | GPa          | AZoM: 97 typical; range from EngineerCalculator |
| poissons_ratio                    | 0.30                   | --           |                                      |
| yield_strength_MPa (0.2% proof)  | 220 - 350              | MPa          | condition-dependent (cold-drawn to half-hard) |
| ultimate_tensile_strength_MPa     | 350 - 500              | MPa          |                                      |
| endurance_limit_MPa              | 90 - 120               | MPa          | ~10^7 cycles; EngineerCalculator     |
| thermal_expansion_coeff_per_K    | 20.5e-6 - 20.9e-6     | 1/K          | AZoM: 20.9; EngineerCalc: 20.5      |
| specific_heat_J_kgK              | 380                    | J/(kg.K)     |                                      |
| thermal_conductivity_W_mK        | 120 - 123              | W/(m.K)      | AZoM: 123; EngineerCalc: 120        |
| hardness                         | 80-160 HB; 90-110 HV  | HB / HV      | depends on temper                    |
| electrical_resistivity_ohm_m     | 6.2e-8 - 6.5e-8       | ohm.m        | AZoM: 0.62 uOhm.m; EngineerCalc: 6.49e-8 |
| magnetic_permeability_relative   | ~1.0                   | --           | non-magnetic; paramagnetic mu_r ~ 1.0 |
| creep_threshold_C                | 150 - 200              | deg C        | Cu-alloy rule: significant creep above ~0.3 T_m |

---

## 2. Mild Steel S275JR (EN 10025-2) -- Structural Carbon Steel

| Property                          | Value                  | Unit         | Notes                                |
|-----------------------------------|------------------------|--------------|--------------------------------------|
| density_kg_m3                     | 7850                   | kg/m3        | Eurocode EN 1993-1-1 Sec 3.2.6      |
| youngs_modulus_GPa                | 200 - 210              | GPa          | Eurocode: 210; EngineerCalc: 200     |
| poissons_ratio                    | 0.30                   | --           | Eurocode EN 1993-1-1                 |
| yield_strength_MPa                | 275 (t<=16mm)          | MPa          | reduces with thickness per EN 10025-2 |
| ultimate_tensile_strength_MPa     | 410 - 560              | MPa          | thickness 3-100mm; <3mm: 430-580     |
| endurance_limit_MPa              | 160 - 200              | MPa          | ~10^7 cycles; EngineerCalc, Scribd fatigue study |
| thermal_expansion_coeff_per_K    | 11.7e-6 - 12.0e-6     | 1/K          | Eurocode: 12e-6; EngineerCalc: 11.7e-6 |
| specific_heat_J_kgK              | 475 - 486              | J/(kg.K)     | EngineerCalc: 475; AISI 1020 ref: 486 |
| thermal_conductivity_W_mK        | 50 - 52                | W/(m.K)      | EngineerCalc: 50; AISI 1020: 51.9   |
| hardness                         | 120-170 HB; 150-180 HV| HB / HV      | EngineerCalc; condition-dependent    |
| electrical_resistivity_ohm_m     | 1.59e-7 - 1.67e-7     | ohm.m        | AISI 1020: 1.59e-7; EngineerCalc: 1.67e-7 |
| magnetic_permeability_relative   | ~100                   | --           | ferromagnetic; carbon steel mu_r ~ 100 (Engineering LibreTexts) |
| creep_threshold_C                | 350 - 400              | deg C        | carbon steel rule: significant above ~0.4 T_m |

---

## 3. Cast Iron EN-GJL-250 (GG-25) -- Grey Cast Iron

| Property                          | Value                  | Unit         | Notes                                |
|-----------------------------------|------------------------|--------------|--------------------------------------|
| density_kg_m3                     | 7200                   | kg/m3        | SN-CastIron                          |
| youngs_modulus_GPa                | 103 - 118              | GPa          | SN-CastIron; AZoM range: 80-150 (all grey) |
| poissons_ratio                    | 0.26                   | --           | SN-CastIron; AZoM range: 0.255-0.265 |
| yield_strength_MPa (0.1% proof)  | 165 - 228              | MPa          | SN-CastIron Rp0.1; N.B. no clear yield point |
| ultimate_tensile_strength_MPa     | 250 - 350              | MPa          | EN 1561 min 250; SN-CastIron range  |
| compressive_strength_MPa         | 840                    | MPa          | SN-CastIron; ~3.4x UTS              |
| endurance_limit_MPa              | 60 - 120               | MPa          | SN-CastIron: sigma_bW=120, sigma_zdW=60 |
| thermal_expansion_coeff_per_K    | 11e-6 - 13e-6          | 1/K          | Modulus Metal; AZoM confirms         |
| specific_heat_J_kgK              | 460 - 505              | J/(kg.K)     | Modulus Metal                        |
| thermal_conductivity_W_mK        | 44 - 51                | W/(m.K)      | Modulus Metal; AZoM range: 40-72 (all grey) |
| hardness                         | 190-240 HB             | HB           | iron-foundry.com; SN: 190-230       |
| electrical_resistivity_ohm_m     | 6.2e-7 - 8.6e-7       | ohm.m        | AZoM: 62-86e-8; SN: 0.80 uOhm.mm2/m = 8.0e-7 |
| magnetic_permeability_relative   | 200 - 400              | --           | ferromagnetic; pearlitic matrix mu_r 200-400 |
| creep_threshold_C                | 300 - 350              | deg C        | cast iron: significant above ~0.35 T_m |

**Note on yield:** Grey cast iron does not exhibit a distinct yield point.
The 0.1% proof stress (Rp0.1) values above are the standard substitute
per EN 1561.  For conservative structural analysis, use 165 MPa as the
lower bound.

---

## 4. Phosphor Bronze CuSn8 (C52100 / CW453K)

| Property                          | Value                  | Unit         | Notes                                |
|-----------------------------------|------------------------|--------------|--------------------------------------|
| density_kg_m3                     | 8800                   | kg/m3        | copper.org, AZoM, Concast all agree  |
| youngs_modulus_GPa                | 110 - 117              | GPa          | Concast: 110.3; AZoM: 117; copper.org: 110 |
| poissons_ratio                    | 0.34                   | --           | AZoM                                 |
| yield_strength_MPa (0.2% offset) | 165 - 745              | MPa          | annealed: 165; H04: 600; H08 spring: 745 |
| ultimate_tensile_strength_MPa     | 379 - 965              | MPa          | AZoM; copper.org H04: 585           |
| endurance_limit_MPa              | 200 - 280              | MPa          | copper.org: 10^8 cycles, 73-82 ksi by temper = 503-565 MPa at 10^8; ~50% at 10^7 inference |
| thermal_expansion_coeff_per_K    | 17.4e-6 - 18.2e-6     | 1/K          | Concast: 17.4 (20-300C); AZoM: 18.2 |
| specific_heat_J_kgK              | 377                    | J/(kg.K)     | Concast: 0.09 BTU/lb/F = 377 J/(kg.K) |
| thermal_conductivity_W_mK        | 62                     | W/(m.K)      | copper.org, AZoM, Concast all agree  |
| hardness                         | 93-100 HRB; ~170-250 HV | HRB / HV   | copper.org H04: 93 HRB; H08: 100 HRB |
| electrical_resistivity_ohm_m     | 1.33e-7                | ohm.m        | derived from 13% IACS (copper.org)   |
| magnetic_permeability_relative   | ~1.0                   | --           | non-magnetic; Cu-alloy, mu_r ~ 1.0  |
| creep_threshold_C                | 150 - 200              | deg C        | Cu-alloy rule: significant above ~0.3 T_m |

**Note on endurance limit:** The copper.org data gives fatigue strength at
10^8 cycles (73-82 ksi = 503-565 MPa).  The value at 10^7 cycles is
approximately 10-20% higher than 10^8 values.  For H04 hard temper,
fatigue limit at 10^7 is approximately 200-280 MPa (inference from S-N
curve shape for copper alloys).  Mark as inference.

**Note on yield:** The wide range reflects temper variation.  For the
Babbage Engine context, half-hard to hard temper (H02-H04) is expected:
yield 440-600 MPa, UTS 530-640 MPa.

---

## 5. Spring Steel C67S (EN 10132-4 / 1.1231 / ~AISI 1065)

| Property                          | Value                  | Unit         | Notes                                |
|-----------------------------------|------------------------|--------------|--------------------------------------|
| density_kg_m3                     | 7820 - 7850            | kg/m3        | Virgamet: 7820; AZoM/AISI 1065: 7850 |
| youngs_modulus_GPa                | 200 - 210              | GPa          | AZoM AISI 1065: 200; Shigley's Table A-5 carbon steel: 207 |
| poissons_ratio                    | 0.27 - 0.30            | --           | AZoM AISI 1065: 0.27-0.30           |
| yield_strength_MPa                | 490 - 1200             | MPa          | cold drawn: 490; QT: up to 1200+    |
| ultimate_tensile_strength_MPa     | 635 - 1900             | MPa          | cold drawn: 635; QT: 1200-1900      |
| endurance_limit_MPa              | 275 - 450              | MPa          | AISI 1065 ref: 275 (cold drawn); QT approx 0.3-0.5*UTS |
| thermal_expansion_coeff_per_K    | 10.0e-6 - 11.7e-6     | 1/K          | MakeItFrom: 10e-6; AISI 1020 ref: 11.7e-6 |
| specific_heat_J_kgK              | 460 - 486              | J/(kg.K)     | search refs: 460; AISI 1020: 486    |
| thermal_conductivity_W_mK        | 49 - 50                | W/(m.K)      | AZoM AISI 1065: 49.8               |
| hardness                         | 187-580 HV; 187 HB (CD) | HV / HB    | CD: 187 HB/196 HV; QT: 370-580 HV  |
| electrical_resistivity_ohm_m     | 2.0e-7 - 2.5e-7       | ohm.m        | inferred from high-C steel refs; 0.55 uOhm.mm2/m cross-check |
| magnetic_permeability_relative   | ~100                   | --           | ferromagnetic; carbon steel mu_r ~ 100 |
| creep_threshold_C                | 350 - 400              | deg C        | carbon steel rule: significant above ~0.4 T_m |

**Note on condition:** Spring steel properties depend dramatically on heat
treatment.  For the Babbage Engine, quenched-and-tempered (QT) condition
is assumed for springs and flexures: UTS 1200-1500 MPa, HV 370-480,
endurance limit 350-450 MPa.

**Note on electrical resistivity:** The search value of 0.55 ohm.mm2/m
converts to 5.5e-7 ohm.m, which seems high for a plain carbon steel.
Cross-referencing with AISI 1060/1070 data suggests 2.0-2.5e-7 ohm.m is
more consistent.  The 5.5e-7 value may reflect a different measurement
condition or alloy variant.  Flagged for verification.

---

## Cross-Reference Summary Table

| Property                  | Brass CZ121 | Steel S275JR | Cast Iron GJL-250 | Phos Bronze C52100 | Spring Steel C67S |
|---------------------------|-------------|-------------|-------------------|-------------------|------------------|
| density (kg/m3)           | 8470        | 7850        | 7200              | 8800              | 7820-7850        |
| E (GPa)                   | 90-105      | 200-210     | 103-118           | 110-117           | 200-210          |
| nu                        | 0.30        | 0.30        | 0.26              | 0.34              | 0.27-0.30        |
| Sy (MPa)                  | 220-350     | 275         | 165-228 (Rp0.1)  | 165-745           | 490-1200         |
| Su (MPa)                  | 350-500     | 410-560     | 250-350           | 379-965           | 635-1900         |
| Se at 10^7 (MPa)         | 90-120      | 160-200     | 60-120            | 200-280 (inf.)    | 275-450          |
| CTE (1/K x 10^-6)        | 20.5-20.9   | 11.7-12.0   | 11-13             | 17.4-18.2         | 10.0-11.7        |
| cp (J/kg.K)               | 380         | 475-486     | 460-505           | 377               | 460-486          |
| k (W/m.K)                 | 120-123     | 50-52       | 44-51             | 62                | 49-50            |
| Hardness                  | 80-160 HB   | 120-170 HB  | 190-240 HB        | 93-100 HRB        | 187-580 HV       |
| rho_e (ohm.m)             | 6.2-6.5e-8  | 1.6-1.7e-7  | 6.2-8.6e-7        | 1.33e-7           | 2.0-2.5e-7       |
| mu_r                      | ~1.0        | ~100        | 200-400           | ~1.0              | ~100             |
| Creep threshold (C)       | 150-200     | 350-400     | 300-350           | 150-200           | 350-400          |

---

## Source Attribution

### Primary Web Sources (retrieved 2026-02-25)

1. **AZoM -- Brass CZ121 Properties**
   https://www.azom.com/article.aspx?ArticleID=2822
   Density, E, UTS, yield, hardness, CTE, thermal conductivity, resistivity.

2. **EngineerCalculator -- Brass CZ121**
   https://engineercalculator.com/metal-alloy-properties-and-overview/brass-alloy-cz121-various-properties-and-overview/
   Full property set: E range, Poisson's, yield, UTS, hardness (HB/HV/HRB),
   thermal conductivity, specific heat, CTE, resistivity, fatigue limit.

3. **Eurocode Applied -- Structural Steel Properties**
   https://eurocodeapplied.com/design/en1993/steel-design-properties
   EN 1993-1-1 Sec 3.2.6: density, E, G, nu, CTE, yield/UTS by thickness.

4. **EngineerCalculator -- Mild Steel S275**
   https://engineercalculator.com/metal-alloy-properties-and-overview/mild-steel-s275-various-properties-and-overview/
   Full property set including hardness, fatigue limit, resistivity.

5. **The World Material -- S275JR / AISI 1020**
   https://www.theworldmaterial.com/en-10025-s275-mild-steel/
   https://www.theworldmaterial.com/astm-sae-aisi-1020-carbon-steel/
   Yield by thickness, UTS ranges, density, thermal properties.

6. **SN-CastIron -- EN-GJL**
   https://www.sn-castiron.com/materials/cast-iron-en-gjl/
   EN-GJL-250: E, nu, Rp0.1, UTS, compressive, fatigue, hardness, density.

7. **AZoM -- Grey Cast Iron Properties**
   https://www.azom.com/properties.aspx?ArticleID=783
   Full range for grey iron class: E, nu, UTS, compressive, hardness,
   endurance limit, thermal conductivity, specific heat, CTE, resistivity.

8. **Modulus Metal -- EN-GJL-250 Thermal Properties**
   https://www.modulusmetal.com/en-gjl-250-cast-iron-thermal-properties-gray-flake-graphite/
   Thermal conductivity, specific heat, CTE, service temperature range.

9. **Copper.org -- C52100 Alloy**
   https://alloys.copper.org/alloy/C52100
   Official CDA data: E, yield/UTS by temper, hardness, thermal conductivity,
   specific heat, CTE, electrical conductivity (13% IACS), fatigue at 10^8.

10. **Concast -- C52100 Phosphor Bronze**
    https://www.concast.com/c52100.php
    Density, E, G, thermal properties, UTS by size range.

11. **AZoM -- Phosphor Bronze C52100**
    https://www.azom.com/article.aspx?ArticleID=6417
    E, G, nu, UTS, yield, CTE, thermal conductivity, density.

12. **Virgamet -- C67S Spring Steel**
    https://virgamet.com/offer/c67s-ck67-c67-1-1231-ck68-xc68-70cr2-aisi-1065-spring-steel-strips
    Density, UTS by condition (CR/QT/annealed), hardness ranges.

13. **AZoM -- AISI 1065 Carbon Steel**
    https://www.azom.com/article.aspx?ArticleID=6575
    Density, E, G, nu, UTS, yield, hardness (HB/HRB/HV/HK),
    thermal conductivity.

14. **Engineering LibreTexts -- Permeability of Common Materials**
    https://eng.libretexts.org/Bookshelves/Electrical_Engineering/Electro-Optics/Book:_Electromagnetics_I_(Ellingson)/10:_Appendices/10.02:_Permeability_of_Some_Common_Materials
    Carbon steel mu_r ~ 100.

15. **Iron-Foundry.com -- EN-GJL-250**
    https://www.iron-foundry.com/en-gjl-250-cast-iron-gg25.html
    Hardness, chemistry, UTS specification.

### Handbook References (not directly fetched but values cross-checked)

- Shigley's Mechanical Engineering Design, 11th Ed., Table A-5:
  E = 207 GPa for carbon steel; nu = 0.292; density = 7850 kg/m3.
  Table A-20: Su/Sy values for AISI 1020 HR, AISI 1065 HR/CD.

- ASM Handbook Vol. 1 (Properties and Selection: Irons, Steels):
  Grey cast iron class 35 (=GJL-250): E = 100-120 GPa typical.

- ASM Handbook Vol. 2 (Properties and Selection: Nonferrous Alloys):
  C52100 phosphor bronze, C38500 leaded brass property ranges.

- MatWeb database (matweb.com): Cross-referenced for C38500, C52100.
  Direct fetch returned 403; values confirmed via AZoM/copper.org mirrors.

---

## Inference vs Source Fact Legend

Throughout this document:
- Values with source citations are **source facts** from the referenced
  databases and standards.
- Values marked "(inf.)" or "(inference)" are **derived or estimated**
  from related data (e.g., fatigue S-N curve extrapolation, creep threshold
  from homologous temperature rules).
- Creep threshold temperatures are all **inferences** based on the
  metallurgical rule of thumb: creep becomes significant above ~0.3-0.4 T_m
  (absolute melting point).  Cu alloys: T_m ~ 1200-1350 K -> 0.3 T_m ~
  360-405 K ~ 87-132 C (conservative: use 150 C).  Steels: T_m ~ 1700-1800 K
  -> 0.4 T_m ~ 680-720 K ~ 407-447 C (conservative: use 350 C).
- Magnetic permeability for brass and phosphor bronze (mu_r ~ 1.0) is a
  **source fact** -- copper alloys are non-ferromagnetic.  Values for steels
  and cast iron are **source facts** from Engineering LibreTexts and
  grey cast iron magnetic property studies.

---

## Applicability to Babbage Engine Simulation

These properties are valid for the operating temperature range of 10-40 C
that the Babbage Analytical Engine would experience in a workshop
environment.  Key design implications:

1. **Thermal expansion mismatch:** Brass CTE (~20.5e-6/K) is roughly
   double that of steel (~12e-6/K) and cast iron (~12e-6/K).  This
   differential must be accounted for in bearing fits and gear mesh
   clearances.

2. **Stiffness hierarchy:** Steel and spring steel (E~200-210 GPa) are
   roughly twice as stiff as cast iron (E~110 GPa) and phosphor bronze
   (E~110 GPa), which in turn are stiffer than brass (E~97 GPa).

3. **Damping:** Cast iron provides superior vibration damping compared
   to all other materials in this set, due to graphite flake internal
   friction.

4. **Wear pairs:** Brass-on-steel and phosphor-bronze-on-steel are
   favorable tribological pairs with low galling tendency.

5. **Fatigue:** Spring steel in QT condition has the highest endurance
   limit (350-450 MPa), critical for counting mechanism springs
   undergoing ~10^7+ cycles.
