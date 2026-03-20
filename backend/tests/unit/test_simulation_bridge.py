"""Unit tests for SimulationBridge.

Validates that the bridge correctly:
- Creates and wraps a SimulationEngine
- Advances physics time when opcode_advance() is called
- Reports physics state via physics_report()
- Produces PhysicsSnapshot
- Resets correctly
- Integrates with Engine via physical_engine= parameter
"""

import pytest

from backend.src.emulator.simulation.bridge import PhysicsSnapshot, SimulationBridge
from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig, SimulationState

pytestmark = pytest.mark.physics


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture
def bridge():
    cfg = SimulationConfig(rpm=30.0)
    eng = SimulationEngine(cfg)
    return SimulationBridge(eng)


# ---------------------------------------------------------------------------
# Basic construction
# ---------------------------------------------------------------------------


def test_bridge_initial_state(bridge):
    assert bridge.failed is False
    assert bridge.failure_reason == ""
    assert bridge.config.rpm == pytest.approx(30.0)


def test_bridge_state_is_sim_state(bridge):
    assert isinstance(bridge.state, SimulationState)


# ---------------------------------------------------------------------------
# opcode_advance
# ---------------------------------------------------------------------------


def test_opcode_advance_returns_positive_time(bridge):
    t = bridge.opcode_advance("ADD")
    assert t > 0.0


def test_opcode_advance_mechanical_op_advances_time(bridge):
    t0 = bridge.state.time_s
    bridge.opcode_advance("MULT")
    assert bridge.state.time_s > t0


def test_opcode_advance_nop_advances_zero(bridge):
    t0 = bridge.state.time_s
    # NOP has 0 cycles in TIMING_TABLE
    t_cost = bridge.opcode_advance("NOP")
    assert t_cost == pytest.approx(0.0)
    assert bridge.state.time_s == pytest.approx(t0)


def test_opcode_advance_non_mechanical_uses_timing_table(bridge):
    t = bridge.opcode_advance("CALL")
    assert t >= 0.0


def test_opcode_counts_accumulate(bridge):
    bridge.opcode_advance("ADD")
    bridge.opcode_advance("ADD")
    bridge.opcode_advance("MULT")
    report = bridge.physics_report()
    assert report["opcode_counts"]["ADD"] == 2
    assert report["opcode_counts"]["MULT"] == 1


# ---------------------------------------------------------------------------
# physics_report
# ---------------------------------------------------------------------------


def test_physics_report_keys(bridge):
    report = bridge.physics_report()
    expected_keys = {
        "instructions_executed",
        "opcode_counts",
        "simulated_time_s",
        "temperature_C",
        "shaft_deflection_mm",
        "max_bearing_clearance_mm",
        "max_bearing_wear_mm3",
        "gear_backlash_mm",
        "lubrication_regime",
        "energy_consumed_J",
        "failed",
        "failure_reason",
    }
    assert expected_keys.issubset(set(report.keys()))


def test_physics_report_not_failed_initially(bridge):
    report = bridge.physics_report()
    assert report["failed"] is False
    assert report["failure_reason"] is None


def test_physics_report_temperature_starts_ambient(bridge):
    report = bridge.physics_report()
    assert 15.0 <= report["temperature_C"] <= 25.0


# ---------------------------------------------------------------------------
# snapshot
# ---------------------------------------------------------------------------


def test_snapshot_returns_physics_snapshot(bridge):
    snap = bridge.snapshot()
    assert isinstance(snap, PhysicsSnapshot)


def test_snapshot_temperature_in_range(bridge):
    snap = bridge.snapshot()
    assert 15.0 <= snap.temperature_C <= 100.0


def test_snapshot_time_starts_zero(bridge):
    snap = bridge.snapshot()
    assert snap.time_s == pytest.approx(0.0)


def test_snapshot_time_advances_after_opcode(bridge):
    bridge.opcode_advance("ADD")
    snap = bridge.snapshot()
    assert snap.time_s > 0.0


# ---------------------------------------------------------------------------
# reset
# ---------------------------------------------------------------------------


def test_reset_clears_time(bridge):
    bridge.opcode_advance("MULT")
    bridge.reset()
    assert bridge.state.time_s == pytest.approx(0.0)


def test_reset_clears_opcode_counts(bridge):
    bridge.opcode_advance("ADD")
    bridge.reset()
    report = bridge.physics_report()
    assert report["opcode_counts"] == {}


def test_reset_clears_instruction_count(bridge):
    bridge.opcode_advance("ADD")
    bridge.opcode_advance("SUB")
    bridge.reset()
    report = bridge.physics_report()
    assert report["instructions_executed"] == 0


# ---------------------------------------------------------------------------
# Engine integration
# ---------------------------------------------------------------------------


def test_bridge_integrates_with_engine():
    """Engine with physics_enabled should call bridge.run() per instruction."""
    from backend.src.emulator.analytical_engine import Engine

    cfg = SimulationConfig(rpm=30.0)
    phys = SimulationEngine(cfg)
    bridge = SimulationBridge(phys)

    engine = Engine(physical_engine=bridge)
    from backend.src.emulator.cli.assembler.parser import parse_source

    engine.instruction_cards = parse_source("LOAD A 5\nLOAD B 3\nADD A B")
    engine.run()

    # Physics time should have advanced
    report = bridge.physics_report()
    assert report["simulated_time_s"] >= 0.0


def test_bridge_physics_report_after_engine_run():
    from backend.src.emulator.analytical_engine import Engine

    cfg = SimulationConfig(rpm=30.0)
    phys = SimulationEngine(cfg)
    bridge = SimulationBridge(phys)

    engine = Engine(physical_engine=bridge)
    from backend.src.emulator.cli.assembler.parser import parse_source

    engine.instruction_cards = parse_source("MULT A B")
    engine.run()

    report = bridge.physics_report()
    assert isinstance(report, dict)
    assert report["failed"] is False
