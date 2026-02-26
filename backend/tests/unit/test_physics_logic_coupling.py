import pytest

pytestmark = pytest.mark.physics
import math

from backend.src.emulator.analytical_engine import Engine, Instruction, MechanicalFailureError
from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig


def test_logic_physics_timing_coupling():
    """Verify that linked physics engine affects instruction timing."""
    # Setup physics engine
    config = SimulationConfig(rpm=30.0) # 0.5 rotations per second
    # With 30 RPM, 8 cycles should take 8 / (30/60) = 16 seconds if lag is 0.

    sim = SimulationEngine(config)
    engine = Engine(physical_engine=sim)

    # Execute an ADD instruction (8 cycles)
    instr = Instruction("ADD", ["A", "5"])
    engine.execute_instruction(instr)

    # Check if time advanced correctly in both engines
    # lag = 1.0 + 0.05 * deflection. Initial deflection is small but not 0 if load exists.
    # In SimulationEngine._initial_state, initial loads are set.
    # But shaft_deflection starts at 0.0.

    assert engine.clock_time > 0
    assert sim.state.time_s == engine.clock_time
    assert sim.state.time_s >= 16.0 # 8 / 0.5

def test_mechanical_failure_halts_logic():
    """Verify that a physical failure (seizure) raises MechanicalFailureError in logic engine."""
    config = SimulationConfig(rpm=30.0)
    sim = SimulationEngine(config)
    engine = Engine(physical_engine=sim)
    engine.instruction_cards = [Instruction("NOP")]

    # Manually trigger failure in physics engine
    sim.state.bearing_clearances_mm = [0.0, 0.05, 0.05, 0.05]
    sim._check_failures()
    assert sim.failed
    assert sim.failure_reason == "bearing_seizure"

    # Try to execute an instruction
    with pytest.raises(MechanicalFailureError) as excinfo:
        engine.step_one_instruction()

    assert "Mechanical failure: bearing_seizure" in str(excinfo.value)

def test_oil_instruction_resets_wear():
    """Verify that the OIL instruction resets wear volumes in the physics state."""
    config = SimulationConfig()
    sim = SimulationEngine(config)
    engine = Engine(physical_engine=sim)

    # Artificially add wear
    sim.state.bearing_wear_volumes_mm3 = [10.0, 10.0, 10.0, 10.0]

    # Execute OIL instruction
    engine._execute_OIL()

    # Verify wear is reset
    assert all(v == 0.0 for v in sim.state.bearing_wear_volumes_mm3)

def test_monte_carlo_variance():
    """Verify that randomize_tolerances creates distinct engine configurations."""
    config1 = SimulationConfig(initial_clearance_mm=0.05)
    config2 = SimulationConfig(initial_clearance_mm=0.05)

    config1.randomize_tolerances(seed=42)
    config2.randomize_tolerances(seed=43)

    assert config1.initial_clearance_mm != config2.initial_clearance_mm
    assert config1.initial_gear_backlash_mm != config2.initial_gear_backlash_mm

def test_energy_consumption():
    """Verify that energy consumption is tracked in the physics state."""
    config = SimulationConfig(rpm=60.0) # 1 rotation per sec
    sim = SimulationEngine(config)

    # Run for 10 seconds
    sim.run(10.0)

    assert sim.state.energy_consumed_J > 0
    # Expected work ~ Torque * 10 rotations * 2pi
    # Torque ~ Q_total / omega
    expected_rotations = 10
    expected_radians = expected_rotations * 2 * math.pi
    # Rough check
    assert sim.state.energy_consumed_J > 0

def test_electromagnetic_integration():
    """Verify that EM effects are integrated into the simulation loop."""
    config = SimulationConfig(rpm=60.0)
    sim = SimulationEngine(config)

    # Run for 10 seconds
    sim.run(10.0)

    # Eddy loss should be non-zero (though small)
    # Galvanic risk should accumulate
    assert sim.state.galvanic_risk_accumulator > 0
    # Heat generation should include Q_eddy
    assert sim.state.total_heat_generation_W > 0
