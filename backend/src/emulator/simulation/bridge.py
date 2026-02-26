"""SimulationBridge -- connects logic-level opcode execution to physics.

When physics_enabled=True, each opcode executed by the AnalyticalEngine
advances the SimulationEngine by the mechanical time cost of that operation.
Temperature, wear, and clearances accumulate realistically. If any component
exceeds its failure limit, the engine raises MechanicalFailureError.

Usage:
    config = SimulationConfig(rpm=30.0)
    phys = SimulationEngine(config)
    bridge = SimulationBridge(phys)
    engine = Engine(physical_engine=bridge)
    # Now engine.execute_instruction() advances physics automatically.
"""

from __future__ import annotations

from dataclasses import dataclass

from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig, SimulationState
from backend.src.emulator.timing import BarrelTimingBridge

# Mapping from TIMING_TABLE opcode names to barrel names used by
# BarrelTimingBridge. Only mechanical operations have barrel equivalents.
_OPCODE_TO_BARREL = {
    "ADD": "ADD",
    "SUB": "SUB",
    "MULT": "MULT",
    "DIV": "DIV",
    "SQRT": "SQRT",
    "LOAD": "LOAD",
    "STOR": "STOR",
    "SHL": "SHL",
    "SHR": "SHR",
}


@dataclass
class PhysicsSnapshot:
    """Snapshot of physics state at an instruction boundary."""

    time_s: float
    temperature_C: float
    shaft_deflection_mm: float
    max_clearance_mm: float
    gear_backlash_mm: float
    lubrication_regime: str
    energy_consumed_J: float


class SimulationBridge:
    """Adapter between AnalyticalEngine and SimulationEngine.

    Implements the interface expected by AnalyticalEngine.physical_engine:
    - config: SimulationConfig (for RPM, lag calculations)
    - state: SimulationState (for deflection-based lag)
    - failed: bool (checked before each instruction)
    - failure_reason: str
    - run(end_time_s): advance physics to given time

    Additionally provides:
    - opcode_advance(opcode): compute and apply time cost for one opcode
    - physics_report(): summary of accumulated physics state
    """

    def __init__(self, engine: SimulationEngine) -> None:
        self._engine = engine
        self._barrel_timing = BarrelTimingBridge()
        self._instruction_count = 0
        self._opcode_counts: dict[str, int] = {}

    @property
    def config(self) -> SimulationConfig:
        return self._engine.config

    @property
    def state(self) -> SimulationState:
        return self._engine.state

    @property
    def failed(self) -> bool:
        return self._engine.failed

    @property
    def failure_reason(self) -> str:
        return self._engine.failure_reason

    def run(self, end_time_s: float) -> None:
        """Advance physics to the given absolute time.

        Called by AnalyticalEngine.execute_instruction() after computing
        the time cost of each opcode.
        """
        # SimulationEngine.run() expects absolute end time
        if end_time_s > self._engine.state.time_s:
            self._engine.run(end_time_s)
        self._instruction_count += 1

    def opcode_advance(self, opcode: str) -> float:
        """Compute time cost [s] for one opcode and advance physics.

        Returns the time cost in seconds. Uses BarrelTimingBridge for
        mechanical operations and falls back to TIMING_TABLE cycles
        converted via RPM for non-mechanical operations.
        """
        self._opcode_counts[opcode] = self._opcode_counts.get(opcode, 0) + 1

        barrel_name = _OPCODE_TO_BARREL.get(opcode)
        if barrel_name:
            degrees = self._barrel_timing.total_degrees(barrel_name)
            rpm = self.config.rpm
            # time = degrees / (360 * rpm/60)
            time_s = degrees / (360.0 * rpm / 60.0)
        else:
            # Non-mechanical: use abstract cycle count at nominal rate
            from backend.src.emulator.analytical_engine import TIMING_TABLE
            cycles = TIMING_TABLE.get(opcode, 0)
            rpm = self.config.rpm
            time_s = cycles / (rpm / 60.0) if rpm > 0 else 0.0

        if time_s > 0:
            self._engine.run(self._engine.state.time_s + time_s)

        return time_s

    def snapshot(self) -> PhysicsSnapshot:
        """Capture current physics state for logging."""
        s = self._engine.state
        max_cl = max(s.bearing_clearances_mm) if s.bearing_clearances_mm else 0.0
        return PhysicsSnapshot(
            time_s=s.time_s,
            temperature_C=s.temperature_C,
            shaft_deflection_mm=s.shaft_deflection_mm,
            max_clearance_mm=max_cl,
            gear_backlash_mm=s.gear_backlash_mm,
            lubrication_regime=s.lubrication_regime,
            energy_consumed_J=s.energy_consumed_J,
        )

    def physics_report(self) -> dict:
        """Generate a summary report of the physics state."""
        s = self._engine.state
        max_cl = max(s.bearing_clearances_mm) if s.bearing_clearances_mm else 0.0
        max_wear = max(s.bearing_wear_volumes_mm3) if s.bearing_wear_volumes_mm3 else 0.0
        return {
            "instructions_executed": self._instruction_count,
            "opcode_counts": dict(self._opcode_counts),
            "simulated_time_s": s.time_s,
            "temperature_C": s.temperature_C,
            "shaft_deflection_mm": s.shaft_deflection_mm,
            "max_bearing_clearance_mm": max_cl,
            "max_bearing_wear_mm3": max_wear,
            "gear_backlash_mm": s.gear_backlash_mm,
            "lubrication_regime": s.lubrication_regime,
            "energy_consumed_J": s.energy_consumed_J,
            "failed": self.failed,
            "failure_reason": self.failure_reason if self.failed else None,
        }

    def reset(self) -> None:
        """Reset physics state and counters."""
        self._engine.reset()
        self._instruction_count = 0
        self._opcode_counts.clear()
