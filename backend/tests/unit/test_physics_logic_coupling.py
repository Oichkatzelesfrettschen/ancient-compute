import math

import pytest

from backend.src.emulator.analytical_engine import Engine, Instruction, MechanicalFailureError
from backend.src.emulator.simulation.engine import SimulationEngine
from backend.src.emulator.simulation.state import SimulationConfig

pytestmark = pytest.mark.physics


def test_logic_physics_timing_coupling():
    """Verify that linked physics engine affects instruction timing."""
    # Setup physics engine
    config = SimulationConfig(rpm=30.0)  # 0.5 rotations per second
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
    assert sim.state.time_s >= 16.0  # 8 / 0.5


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
    config = SimulationConfig(rpm=60.0)  # 1 rotation per sec
    sim = SimulationEngine(config)

    # Run for 10 seconds
    sim.run(10.0)

    assert sim.state.energy_consumed_J > 0
    # Expected work ~ Torque * 10 rotations * 2pi
    # Torque ~ Q_total / omega
    expected_rotations = 10
    _expected_radians = expected_rotations * 2 * math.pi
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


class TestSimulationEngineRunDirect:
    def test_run_advances_time(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        sim.run(1.0)
        assert sim.state.time_s >= 1.0

    def test_run_accumulates_energy(self) -> None:
        config = SimulationConfig(rpm=60.0)
        sim = SimulationEngine(config)
        sim.run(5.0)
        assert sim.state.energy_consumed_J > 0

    def test_run_galvanic_risk_accumulates(self) -> None:
        config = SimulationConfig(rpm=60.0)
        sim = SimulationEngine(config)
        sim.run(10.0)
        assert sim.state.galvanic_risk_accumulator > 0

    def test_run_state_not_failed_at_low_rpm(self) -> None:
        config = SimulationConfig(rpm=5.0)
        sim = SimulationEngine(config)
        sim.run(0.5)
        assert not sim.failed


class TestMonteCarloVariance:
    def test_different_seeds_produce_distinct_configs(self) -> None:
        c1 = SimulationConfig()
        c2 = SimulationConfig()
        c1.randomize_tolerances(seed=1)
        c2.randomize_tolerances(seed=2)
        assert c1.initial_clearance_mm != c2.initial_clearance_mm

    def test_same_seed_produces_identical_configs(self) -> None:
        c1 = SimulationConfig()
        c2 = SimulationConfig()
        c1.randomize_tolerances(seed=42)
        c2.randomize_tolerances(seed=42)
        assert math.isclose(c1.initial_clearance_mm, c2.initial_clearance_mm)
        assert math.isclose(c1.initial_gear_backlash_mm, c2.initial_gear_backlash_mm)


class TestOILInstruction:
    """OIL instruction resets wear; multiple calls are idempotent."""

    def test_oil_resets_all_bearings(self) -> None:
        config = SimulationConfig()
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)
        sim.state.bearing_wear_volumes_mm3 = [5.0, 5.0, 5.0, 5.0]
        engine._execute_OIL()
        assert all(v == 0.0 for v in sim.state.bearing_wear_volumes_mm3)

    def test_oil_called_twice_remains_zero(self) -> None:
        config = SimulationConfig()
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)
        sim.state.bearing_wear_volumes_mm3 = [3.0, 3.0, 3.0, 3.0]
        engine._execute_OIL()
        engine._execute_OIL()
        assert all(v == 0.0 for v in sim.state.bearing_wear_volumes_mm3)

    def test_oil_does_not_reset_temperature(self) -> None:
        config = SimulationConfig()
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)
        sim.run(5.0)  # warm up
        temp_before = sim.state.temperature_C
        engine._execute_OIL()
        # OIL should not reset temperature
        assert sim.state.temperature_C == pytest.approx(temp_before)


class TestTimingAtDifferentRPM:
    """Higher RPM should execute instructions faster in real-time seconds."""

    def test_higher_rpm_faster_execution(self) -> None:
        # At 60 RPM, each cycle takes 1 second; at 30 RPM each takes 2 seconds.
        # ADD takes 8 cycles, so: 8s at 60 RPM vs 16s at 30 RPM.
        config_fast = SimulationConfig(rpm=60.0)
        config_slow = SimulationConfig(rpm=30.0)

        sim_fast = SimulationEngine(config_fast)
        sim_slow = SimulationEngine(config_slow)

        engine_fast = Engine(physical_engine=sim_fast)
        engine_slow = Engine(physical_engine=sim_slow)

        instr = Instruction("ADD", ["A", "5"])
        engine_fast.execute_instruction(instr)
        engine_slow.execute_instruction(instr)

        # Faster RPM -> less real-world time per cycle
        assert engine_fast.clock_time < engine_slow.clock_time

    def test_clock_time_tracks_sim_time(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)

        instr = Instruction("NOP")
        engine.execute_instruction(instr)
        # NOP has 0 cycles; engine may advance minimally but sim_time tracks
        assert engine.clock_time >= 0.0
        assert math.isclose(sim.state.time_s, engine.clock_time)


class TestSimulationFailurePropagation:
    """Verify that multiple bearings seizing triggers failure."""

    def test_two_bearing_seizures_triggers_failure(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)
        engine.instruction_cards = [Instruction("NOP")]

        # Seize the first two bearings
        sim.state.bearing_clearances_mm[0] = 0.0
        sim.state.bearing_clearances_mm[1] = 0.0
        sim._check_failures()
        assert sim.failed

        with pytest.raises(MechanicalFailureError):
            engine.step_one_instruction()

    def test_failure_reason_is_bearing_seizure(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        engine = Engine(physical_engine=sim)
        engine.instruction_cards = [Instruction("NOP")]

        sim.state.bearing_clearances_mm = [0.0, 0.05, 0.05, 0.05]
        sim._check_failures()

        try:
            engine.step_one_instruction()
        except MechanicalFailureError as exc:
            assert "bearing_seizure" in str(exc)


class TestSimulationConfigDefaults:
    """SimulationConfig default field values and randomize_tolerances."""

    def test_default_rpm_is_30(self) -> None:
        cfg = SimulationConfig()
        assert cfg.rpm == pytest.approx(30.0)

    def test_default_initial_clearance_positive(self) -> None:
        cfg = SimulationConfig()
        assert cfg.initial_clearance_mm > 0.0

    def test_default_ambient_temperature(self) -> None:
        cfg = SimulationConfig()
        assert 15.0 <= cfg.ambient_temperature_C <= 25.0

    def test_randomize_does_not_preserve_defaults(self) -> None:
        cfg = SimulationConfig()
        orig_clearance = cfg.initial_clearance_mm
        cfg.randomize_tolerances(seed=99)
        assert cfg.initial_clearance_mm != orig_clearance

    def test_randomize_backlash_positive(self) -> None:
        cfg = SimulationConfig()
        cfg.randomize_tolerances(seed=7)
        assert cfg.initial_gear_backlash_mm > 0.0

    def test_randomize_clearance_within_bounds(self) -> None:
        for seed in range(10):
            cfg = SimulationConfig()
            cfg.randomize_tolerances(seed=seed)
            assert cfg.initial_clearance_mm > 0.0


class TestMechanicalFailureErrorType:
    """MechanicalFailureError is an Exception subclass with message."""

    def test_is_exception_subclass(self) -> None:
        assert issubclass(MechanicalFailureError, Exception)

    def test_message_preserved(self) -> None:
        err = MechanicalFailureError("bearing_seizure")
        assert "bearing_seizure" in str(err)

    def test_can_be_raised(self) -> None:
        with pytest.raises(MechanicalFailureError):
            raise MechanicalFailureError("test")

    def test_is_exception_instance(self) -> None:
        err = MechanicalFailureError("test")
        assert isinstance(err, Exception)


class TestPhysicsStateFields:
    """SimulationState fields remain in valid ranges during brief run."""

    def test_temperature_positive_after_run(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        sim.run(1.0)
        assert sim.state.temperature_C > 0.0

    def test_shaft_deflection_nonneg_after_run(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        sim.run(1.0)
        assert sim.state.shaft_deflection_mm >= 0.0

    def test_bearing_clearances_length_is_4(self) -> None:
        config = SimulationConfig()
        sim = SimulationEngine(config)
        assert len(sim.state.bearing_clearances_mm) == 4

    def test_bearing_wear_nonneg_after_run(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim = SimulationEngine(config)
        sim.run(1.0)
        assert all(v >= 0.0 for v in sim.state.bearing_wear_volumes_mm3)

    def test_time_s_advances_proportionally(self) -> None:
        config = SimulationConfig(rpm=30.0)
        sim1 = SimulationEngine(config)
        sim2 = SimulationEngine(config)
        sim1.run(1.0)
        sim2.run(2.0)
        assert sim2.state.time_s > sim1.state.time_s


def test_simulation_config_dt_positive() -> None:
    """SimulationConfig default dt_s must be a positive timestep."""
    config = SimulationConfig()
    assert config.dt_s > 0.0
