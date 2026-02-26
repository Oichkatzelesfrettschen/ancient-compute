# Physics Model Inventory

**Date**: 2026-02-26
**Status**: Complete (Phases A-F, validated Phase IX)

## Module Summary

| Module | File | Equations | Key Models |
|--------|------|-----------|------------|
| Materials | materials.py | 5 materials, 16+ properties | MaterialLibrary, thermal expansion, hardness |
| Kinematics | kinematics.py | 8+ | CamFollower, HertzianContact, TorsionalVibration, CamTorqueRipple, ShaftLateralDynamics |
| Thermodynamics | thermodynamics.py | 10+ | FrictionHeatModel, RadiationHeatModel, TransientThermalSolver, ThermalClearanceFeedback |
| Structural | structural.py | 12+ | ShaftAnalysis (Euler/Johnson buckling), StressConcentration, DynamicLoadFactor, ShaftCriticalSpeed, NotchSensitivity |
| Tribology | tribology.py | 10+ | WearModel (Archard), RunningInWear, LubricationModel (Hamrock-Dowson), WearClearanceFeedback, PVAnalysis, TimeToFailure |
| Electromagnetic | electromagnetic.py | 5+ | EddyCurrentModel, GalvanicCorrosionMatrix |

## Simulation Engine

| Component | File | Description |
|-----------|------|-------------|
| SimulationState | simulation/state.py | State vector: time, temperature, clearances, wear, loads, lubrication |
| SimulationConfig | simulation/state.py | Parameters: geometry, materials, limits, tolerances |
| SimulationEngine | simulation/engine.py | 13-phase step cycle, long-duration run, maintenance prediction |
| CouplingFunctions | simulation/coupling.py | Inter-module coupling: thermal-structural, tribology-clearance, load redistribution |
| SimulationBridge | simulation/bridge.py | Adapter: opcode execution -> physics time advancement |

## Verification Tools

| Tool | File | Checks |
|------|------|--------|
| Dimensional Analysis | verify_dimensions.py | 35 dimensional checks across all modules |
| Sensitivity Analysis | sensitivity_analysis.py | 11 output variables vs 6+ input parameters |
| Monte Carlo Tolerance | monte_carlo_tolerance.py | 7 statistical checks with manufacturing variance |
| DE2 Comparison | -- | 24 parameter comparisons against Science Museum measurements |

## Material Library

| Material | Application | Key Properties |
|----------|------------|----------------|
| Steel (1045) | Shafts, gears | E=200 GPa, yield=530 MPa, rho=7850 kg/m3 |
| Cast Iron | Frame, columns | E=100-130 GPa, brittle, good damping |
| Phosphor Bronze | Bearings | E=110 GPa, mu=0.10, excellent wear resistance |
| Brass | Gears, decorative | E=100-125 GPa, good machinability |
| Babbage Bronze | Period-accurate alloy | E=103 GPa, historically sourced composition |

## Cross-Reference: Equations to Sources

| Equation | Module | Source |
|----------|--------|--------|
| Archard wear law | tribology.py | Archard (1953) |
| Hamrock-Dowson film thickness | tribology.py | Hamrock et al. (2004) |
| AGMA K_v dynamic factor | structural.py | Shigley Eq. 14-27 |
| Johnson buckling | structural.py | Shigley Ch. 4 |
| Euler critical load | structural.py | Timoshenko (1956) |
| Peterson K_t | structural.py | Peterson (1974) |
| Hertzian contact stress | kinematics.py | Shigley Ch. 3 |
| Crank-Nicolson thermal | thermodynamics.py | Incropera & DeWitt (2002) |
| Stefan-Boltzmann radiation | thermodynamics.py | Standard physics |
| Eddy current losses | electromagnetic.py | Standard EM theory |

## Known Limitations

1. **Valve timing**: Lap, lead, cutoff values are provisional (not measured from DE2)
2. **Steam drive**: Pressure-RPM mapping is estimated, not from Babbage's notebooks
3. **Material properties**: Babbage-era alloy compositions are approximate
4. **Lubrication**: Oil properties use modern mineral oil (period-accurate would be tallow/sperm oil)
5. **Galvanic corrosion**: Simplified accumulator model, not full electrochemistry
