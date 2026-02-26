"""
Simulation Orchestration Layer for Babbage Analytical Engine

Dynamically couples all physics modules (materials, kinematics, thermodynamics,
electromagnetic, tribology, structural) through a time-stepping framework.

Public API:
    SimulationState    - Complete state vector at a point in time
    SimulationConfig   - Configuration parameters for a simulation run
    SimulationEngine   - Time-stepping orchestrator
    StepResult         - Diagnostic output from a single step
    SimulationResult   - Complete result from a long-duration run
    CouplingFunctions  - Inter-module coupling (thermal-tribo, kinematic-structural)
"""

from backend.src.emulator.simulation.state import (
    SimulationState,
    SimulationConfig,
)
from backend.src.emulator.simulation.engine import (
    SimulationEngine,
    StepResult,
    SimulationResult,
)
from backend.src.emulator.simulation.coupling import CouplingFunctions

__all__ = [
    "SimulationState",
    "SimulationConfig",
    "SimulationEngine",
    "StepResult",
    "SimulationResult",
    "CouplingFunctions",
]
