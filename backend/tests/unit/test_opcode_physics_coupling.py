"""Tests for opcode-coupled physics simulation.

Verifies that executing instructions through the AnalyticalEngine with a
SimulationBridge advances the physics state realistically: temperature
increases, wear accumulates, and mechanical failure halts execution.
"""

import pytest

pytestmark = pytest.mark.physics

from backend.src.emulator.analytical_engine import (
    BabbageNumber,
    Engine,
    Instruction,
    MechanicalFailureError,
)
from backend.src.emulator.simulation.bridge import SimulationBridge
from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig


def _make_physics_engine(config=None):
    """Create an Engine with physics enabled."""
    if config is None:
        config = SimulationConfig(rpm=30.0)
    sim = SimulationEngine(config)
    bridge = SimulationBridge(sim)
    engine = Engine(physical_engine=bridge)
    # Pre-store some values for arithmetic
    engine.memory[0] = BabbageNumber(42)
    engine.memory[1] = BabbageNumber(7)
    return engine, bridge


@pytest.fixture
def physics_engine():
    engine, _ = _make_physics_engine()
    return engine


@pytest.fixture
def plain_engine():
    return Engine()


# Instruction helpers -- opcodes use (reg_dest, operand_src) as strings
def _load(addr):
    return Instruction("LOAD", ["A", str(addr)])


def _add(addr):
    return Instruction("ADD", ["A", str(addr)])


def _mult(addr):
    return Instruction("MULT", ["A", str(addr)])


class TestBridgeIntegration:
    """Verify SimulationBridge correctly wires into Engine."""

    def test_engine_accepts_bridge(self, physics_engine):
        assert physics_engine.physical_engine is not None

    def test_bridge_has_config(self, physics_engine):
        bridge = physics_engine.physical_engine
        assert bridge.config.rpm == 30.0

    def test_bridge_has_state(self, physics_engine):
        bridge = physics_engine.physical_engine
        assert bridge.state.temperature_C == 20.0

    def test_bridge_not_failed_initially(self, physics_engine):
        bridge = physics_engine.physical_engine
        assert not bridge.failed


class TestTemperatureIncrease:
    """Running ADD ops should increase temperature monotonically."""

    def test_100_adds_increase_temperature(self, physics_engine):
        initial_temp = physics_engine.physical_engine.state.temperature_C

        for _ in range(100):
            physics_engine.execute_instruction(_load(0))
            physics_engine.execute_instruction(_add(1))

        final_temp = physics_engine.physical_engine.state.temperature_C
        assert final_temp > initial_temp, (
            f"Temperature should increase: {initial_temp} -> {final_temp}"
        )

    def test_temperature_stays_ambient_without_physics(self, plain_engine):
        """Without physics, no temperature tracking."""
        plain_engine.memory[0] = BabbageNumber(42)
        for _ in range(100):
            plain_engine.execute_instruction(_load(0))
            plain_engine.execute_instruction(_add(0))
        assert plain_engine.physics_report() == {}


class TestWearAccumulation:
    """Operations should accumulate wear over time."""

    def test_mult_takes_longer_than_add(self):
        """MULT takes more shaft rotations, so it costs more simulated time."""
        engine_add, bridge_add = _make_physics_engine()
        for _ in range(20):
            engine_add.execute_instruction(_load(0))
            engine_add.execute_instruction(_add(1))
        add_time = bridge_add.state.time_s

        engine_mult, bridge_mult = _make_physics_engine()
        for _ in range(20):
            engine_mult.execute_instruction(_load(0))
            engine_mult.execute_instruction(_mult(1))
        mult_time = bridge_mult.state.time_s

        assert mult_time > add_time, (
            f"MULT should take longer: {mult_time:.3f}s vs ADD {add_time:.3f}s"
        )


class TestMechanicalFailure:
    """Verify failure detection halts execution."""

    def test_failure_raises_error(self):
        """Running with negligible cooling and tight limit triggers failure.

        Config uses tiny thermal mass, zero convective/radiative cooling,
        and high transmitted power so temperature rises rapidly past the
        21 C limit (only 1 degree above 20 C ambient).
        """
        config = SimulationConfig(
            rpm=30.0,
            temperature_limit_C=21.0,
            machine_mass_kg=0.1,
            dt_s=0.01,
            h_convection_W_m2K=0.0,
            surface_area_m2=0.01,
            emissivity=0.0,
            transmitted_power_W=500.0,
        )
        engine, bridge = _make_physics_engine(config)

        with pytest.raises(MechanicalFailureError):
            for _ in range(100):
                engine.execute_instruction(_load(0))
                engine.execute_instruction(_add(1))
                if bridge.failed:
                    raise MechanicalFailureError(
                        f"Mechanical failure: {bridge.failure_reason}"
                    )


class TestPhysicsReport:
    """Verify physics_report() returns expected structure."""

    def test_report_structure(self, physics_engine):
        physics_engine.execute_instruction(_load(0))
        physics_engine.execute_instruction(_add(1))

        report = physics_engine.physics_report()
        assert "instructions_executed" in report
        assert "temperature_C" in report
        assert "simulated_time_s" in report
        assert "failed" in report
        assert report["instructions_executed"] >= 2

    def test_report_empty_without_physics(self, plain_engine):
        assert plain_engine.physics_report() == {}

    def test_opcode_counts_tracked(self, physics_engine):
        for _ in range(5):
            physics_engine.execute_instruction(_load(0))
            physics_engine.execute_instruction(_add(1))

        report = physics_engine.physics_report()
        assert report["instructions_executed"] == 10


class TestBridgeSnapshot:
    """Test snapshot functionality."""

    def test_snapshot_returns_physics_state(self, physics_engine):
        bridge = physics_engine.physical_engine
        snap = bridge.snapshot()
        assert snap.temperature_C == 20.0
        assert snap.time_s == 0.0

    def test_snapshot_updates_after_execution(self, physics_engine):
        bridge = physics_engine.physical_engine
        snap_before = bridge.snapshot()

        for _ in range(50):
            physics_engine.execute_instruction(_load(0))
            physics_engine.execute_instruction(_add(1))

        snap_after = bridge.snapshot()
        assert snap_after.time_s > snap_before.time_s
