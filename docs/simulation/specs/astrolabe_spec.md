# Emulator Spec: Astrolabe

## 1. Mechanism Summary
- Name: Astrolabe
- Era: Classical to medieval
- Geography/Culture: Mediterranean, Islamic world, Europe
- Primary purpose: Astronomical position and time
- Physical substrate: Rete and plates with angular scales

## 2. Source Basis
- Primary sources (citations + links): Chaucer 1872 edition
- Secondary sources (citations + links): museum catalogs TBD
- Local cache files:
  - docs/sources/cache/Chaucer_Treatise_on_the_Astrolabe_1872.pdf
- Access limitations: instrument-specific tables needed

## 3. Fidelity Tier
- Target tier: Tier 1 -> Tier 2
- Rationale: logic model first, add angular mechanics later
- Known anachronisms: simplified spherical geometry

## 4. Logic Graph
- [Date/time] -> (rotate_rete) -> {rete_state}
- [Altitude] -> (align_alidade) -> {alignment} -> [time/position]

## 5. State Model
- State variables: rete angle, plate angle
- Invariants: angles in degrees
- Initialization: angles = 0

## 6. Operations
- Supported operations: set_latitude, rotate_time, read_altitude
- Operation semantics: angular transforms
- Error handling: invalid latitude -> error

## 7. Inputs and Outputs
- Input formats: date/time or altitude
- Output formats: time/position estimate
- Determinism guarantees: strict

## 8. Timing / Constraints (Tier 2+)
- Optional: angular step per time unit

## 9. Test Vectors
- Known date/time -> reference star position

## 10. Implementation Notes
- Language/runtime: Python
- Module path: backend/src/emulator/astrolabe.py
- API surface (step/run/state/reset): rotate/read

## 11. Validation Checklist
- Sources cited: partial
- Logic graph present: yes
- Tests implemented: no
- Fidelity tier documented: yes
