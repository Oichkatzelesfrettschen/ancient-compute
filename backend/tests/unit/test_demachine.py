"""
Test suite for Difference Engine No. 2 - DEMachine Orchestrator

Tests complete mechanical orchestration, phase execution, and integration.
Validates column operations, carry propagation, and polynomial evaluation.

Requires: backend/src/emulator/machine.py
Test structure: 40+ tests covering initialization, operations, cycles, polynomial eval
"""

import pytest
from backend.src.emulator.machine import DEMachine, DEMachineSnapshot, OperationResult
from backend.src.emulator.timing import MechanicalPhase


class TestDEMachineInitialization:
    """Initialize DEMachine and verify initial state."""

    def test_demachine_init(self):
        """Create new DEMachine with default state."""
        de = DEMachine()
        assert de.cycle_count == 0
        assert de.total_operations == 0
        assert len(de.operation_history) == 0

    def test_demachine_has_all_subsystems(self):
        """DEMachine should have all four core subsystems."""
        de = DEMachine()
        assert de.analytical_engine is not None
        assert de.column_bank is not None
        assert de.carriage is not None
        assert de.timing is not None

    def test_demachine_initial_column_values(self):
        """Initial column values should be zero."""
        de = DEMachine()
        values = de.get_column_values()
        assert len(values) == 8
        assert all(v == 0 for v in values)

    def test_demachine_has_8_columns(self):
        """DEMachine should have exactly 8 columns."""
        de = DEMachine()
        assert len(de.column_bank.columns) == 8

    def test_demachine_initial_phase(self):
        """Initial phase should be IDLE."""
        de = DEMachine()
        assert de.timing.phase == MechanicalPhase.IDLE


class TestColumnValueManagement:
    """Test setting and getting column values."""

    def test_set_single_column_value(self):
        """Set value in single column."""
        de = DEMachine()
        de.column_bank.columns[0].set_value_from_int(123)
        assert de.column_bank.columns[0].get_value_as_int() == 123

    def test_get_all_column_values(self):
        """Get all column values as list."""
        de = DEMachine()
        de.column_bank.columns[0].set_value_from_int(100)
        de.column_bank.columns[1].set_value_from_int(200)
        values = de.get_column_values()
        assert values[0] == 100
        assert values[1] == 200

    def test_set_column_values_list(self):
        """Set multiple column values at once."""
        de = DEMachine()
        values = [10, 20, 30, 40, 50, 60, 70, 80]
        de.set_column_values(values)
        result = de.get_column_values()
        assert result == values

    def test_set_difference_values(self):
        """set_difference_values() is alias for set_column_values()."""
        de = DEMachine()
        values = [1, 2, 3, 4, 5, 6, 7, 8]
        de.set_difference_values(values)
        assert de.get_difference_values() == values

    def test_column_values_partial_set(self):
        """Setting fewer columns than 8 should only update those."""
        de = DEMachine()
        de.set_column_values([100, 200])
        values = de.get_column_values()
        assert values[0] == 100
        assert values[1] == 200
        assert values[2] == 0  # Untouched columns remain 0


class TestFullCycleExecution:
    """Test complete 360-degree rotation cycles."""

    def test_run_full_cycle_increments_count(self):
        """Running full cycle should increment cycle_count."""
        de = DEMachine()
        assert de.cycle_count == 0
        de.run_full_cycle()
        assert de.cycle_count == 1

    def test_run_full_cycle_returns_operations(self):
        """run_full_cycle() should return number of operations."""
        de = DEMachine()
        ops = de.run_full_cycle()
        assert ops > 0  # Should execute multiple operations

    def test_multiple_full_cycles(self):
        """Multiple full cycles should increment counter properly."""
        de = DEMachine()
        de.run_full_cycle()
        de.run_full_cycle()
        de.run_full_cycle()
        assert de.cycle_count == 3

    def test_full_cycle_generates_operations(self):
        """Full cycle should create operation records."""
        de = DEMachine()
        initial_ops = de.total_operations
        de.run_full_cycle()
        assert de.total_operations > initial_ops

    def test_full_cycle_timing_resets(self):
        """After full cycle, timing should be back at IDLE."""
        de = DEMachine()
        de.run_full_cycle()
        assert de.timing.angle == 0

    def test_full_cycle_with_initial_values(self):
        """Full cycle with pre-set column values."""
        de = DEMachine()
        de.set_column_values([1, 2, 3, 4, 5, 6, 7, 8])
        de.run_full_cycle()
        # Values should be processed through cycle
        assert de.cycle_count == 1


class TestOperationHistory:
    """Test operation recording and history tracking."""

    def test_operation_recorded_input(self):
        """INPUT phase operation should be recorded."""
        de = DEMachine()
        de.run_full_cycle()
        operations = [op.operation for op in de.operation_history]
        assert "INPUT" in operations

    def test_operation_recorded_addition(self):
        """ADDITION phase operation should be recorded."""
        de = DEMachine()
        de.run_full_cycle()
        operations = [op.operation for op in de.operation_history]
        assert "ADDITION" in operations

    def test_operation_recorded_carry(self):
        """CARRY phase operation should be recorded."""
        de = DEMachine()
        de.run_full_cycle()
        operations = [op.operation for op in de.operation_history]
        assert "CARRY" in operations

    def test_operation_recorded_output(self):
        """OUTPUT phase operation should be recorded."""
        de = DEMachine()
        de.run_full_cycle()
        operations = [op.operation for op in de.operation_history]
        assert "OUTPUT" in operations

    def test_operation_success_flag(self):
        """All operations should succeed."""
        de = DEMachine()
        de.run_full_cycle()
        for op in de.operation_history:
            assert op.success == True

    def test_get_operation_history_limit(self):
        """get_operation_history(limit) should return last N operations."""
        de = DEMachine()
        de.run_full_cycle()
        de.run_full_cycle()
        history = de.get_operation_history(limit=3)
        assert len(history) <= 3

    def test_operation_has_phase(self):
        """Each operation should record its mechanical phase."""
        de = DEMachine()
        de.run_full_cycle()
        for op in de.operation_history:
            assert op.phase is not None
            assert isinstance(op.phase, MechanicalPhase)


class TestOperationResult:
    """Test operation result data structure."""

    def test_operation_result_creation(self):
        """Create operation result."""
        result = OperationResult(operation="TEST", phase=MechanicalPhase.INPUT, success=True)
        assert result.operation == "TEST"
        assert result.phase == MechanicalPhase.INPUT
        assert result.success == True

    def test_operation_result_with_data(self):
        """Operation result can contain optional data."""
        data = {"values": [1, 2, 3]}
        result = OperationResult(
            operation="TEST",
            phase=MechanicalPhase.INPUT,
            success=True,
            data=data,
        )
        assert result.data == data

    def test_operation_result_with_error(self):
        """Operation result can record errors."""
        result = OperationResult(
            operation="TEST",
            phase=MechanicalPhase.INPUT,
            success=False,
            error="Test error",
        )
        assert result.error == "Test error"


class TestPolynomialEvaluation:
    """Test polynomial evaluation using DE2."""

    def test_evaluate_polynomial_constant(self):
        """Evaluate constant polynomial f(x) = 5."""
        de = DEMachine()
        results = de.evaluate_polynomial([5], (1, 3))
        assert len(results) == 3
        # f(1)=5, f(2)=5, f(3)=5
        assert results[0] == 5
        assert results[1] == 5
        assert results[2] == 5

    def test_evaluate_polynomial_linear(self):
        """Evaluate linear polynomial f(x) = x + 1."""
        de = DEMachine()
        results = de.evaluate_polynomial([1, 1], (1, 3))
        # f(1)=2, f(2)=3, f(3)=4
        assert len(results) == 3
        assert results[0] == 2
        assert results[1] == 3
        assert results[2] == 4

    def test_evaluate_polynomial_quadratic(self):
        """Evaluate quadratic polynomial f(x) = x^2 + 1."""
        de = DEMachine()
        results = de.evaluate_polynomial([1, 0, 1], (1, 3))
        # f(1)=2, f(2)=5, f(3)=10
        assert len(results) == 3
        assert results[0] == 2
        assert results[1] == 5
        assert results[2] == 10

    def test_evaluate_polynomial_single_point(self):
        """Evaluate polynomial at single point."""
        de = DEMachine()
        results = de.evaluate_polynomial([1, 1, 1], (5, 5))
        # f(5) = 1 + 5 + 25 = 31
        assert len(results) == 1
        assert results[0] == 31

    def test_evaluate_polynomial_range(self):
        """Evaluate polynomial over range."""
        de = DEMachine()
        results = de.evaluate_polynomial([0, 1], (0, 5))
        # f(x) = x for x in 0..5
        expected = [0, 1, 2, 3, 4, 5]
        assert len(results) == 6
        assert results == expected

    def test_polynomial_evaluation_increments_cycles(self):
        """Polynomial evaluation should increment cycle count."""
        de = DEMachine()
        initial_cycles = de.cycle_count
        de.evaluate_polynomial([1, 1], (1, 3))
        # One cycle per evaluation point
        assert de.cycle_count == initial_cycles + 3


class TestReset:
    """Test reset functionality."""

    def test_reset_cycle_count(self):
        """reset() should reset cycle_count to 0."""
        de = DEMachine()
        de.run_full_cycle()
        assert de.cycle_count == 1
        de.reset()
        assert de.cycle_count == 0

    def test_reset_total_operations(self):
        """reset() should reset total_operations to 0."""
        de = DEMachine()
        de.run_full_cycle()
        assert de.total_operations > 0
        de.reset()
        assert de.total_operations == 0

    def test_reset_operation_history(self):
        """reset() should clear operation_history."""
        de = DEMachine()
        de.run_full_cycle()
        assert len(de.operation_history) > 0
        de.reset()
        assert len(de.operation_history) == 0

    def test_reset_column_values(self):
        """reset() should reset all columns to 0."""
        de = DEMachine()
        de.set_column_values([1, 2, 3, 4, 5, 6, 7, 8])
        de.reset()
        assert de.get_column_values() == [0] * 8

    def test_reset_timing(self):
        """reset() should reset timing controller."""
        de = DEMachine()
        de.run_full_cycle()
        assert de.timing.rotation_count == 1
        de.reset()
        assert de.timing.rotation_count == 0
        assert de.timing.angle == 0


class TestSnapshots:
    """Test snapshot capture and debugging."""

    def test_get_snapshot_contains_cycle_count(self):
        """Snapshot should contain cycle_count."""
        de = DEMachine()
        de.run_full_cycle()
        snapshot = de.get_snapshot()
        assert snapshot.cycle_count == 1

    def test_get_snapshot_contains_phase(self):
        """Snapshot should contain current phase."""
        de = DEMachine()
        snapshot = de.get_snapshot()
        assert snapshot.current_phase == MechanicalPhase.IDLE

    def test_get_snapshot_contains_angle(self):
        """Snapshot should contain timing angle."""
        de = DEMachine()
        snapshot = de.get_snapshot()
        assert snapshot.timing_angle == 0

    def test_get_snapshot_contains_column_values(self):
        """Snapshot should contain column values."""
        de = DEMachine()
        de.set_column_values([1, 2, 3, 4, 5, 6, 7, 8])
        snapshot = de.get_snapshot()
        assert len(snapshot.column_values) == 8

    def test_get_snapshot_contains_carry_signals(self):
        """Snapshot should contain carry signals."""
        de = DEMachine()
        snapshot = de.get_snapshot()
        assert len(snapshot.carry_signals) == 8

    def test_snapshot_is_independent(self):
        """Snapshot should be independent of machine state."""
        de = DEMachine()
        snapshot1 = de.get_snapshot()
        de.run_full_cycle()
        snapshot2 = de.get_snapshot()
        assert snapshot1.cycle_count != snapshot2.cycle_count


class TestRepr:
    """Test string representation."""

    def test_repr_contains_cycles(self):
        """String representation should contain cycle count."""
        de = DEMachine()
        de.run_full_cycle()
        repr_str = repr(de)
        assert "cycles=1" in repr_str

    def test_repr_contains_phase(self):
        """String representation should contain phase."""
        de = DEMachine()
        repr_str = repr(de)
        assert "phase=" in repr_str

    def test_repr_contains_operations(self):
        """String representation should contain operations count."""
        de = DEMachine()
        repr_str = repr(de)
        assert "ops=" in repr_str


class TestEdgeCasesAndIntegration:
    """Test edge cases and full integration scenarios."""

    def test_multiple_cycles_with_values(self):
        """Run multiple cycles with changing values."""
        de = DEMachine()
        for i in range(3):
            de.set_column_values([i, i * 2, i * 3, 0, 0, 0, 0, 0])
            de.run_full_cycle()
        assert de.cycle_count == 3

    def test_polynomial_eval_sequence(self):
        """Evaluate multiple polynomials sequentially."""
        de = DEMachine()
        # Evaluate x for x in 1..3
        de.evaluate_polynomial([0, 1], (1, 3))
        cycles_after_first = de.cycle_count
        # Then evaluate x^2 for x in 1..2
        de.evaluate_polynomial([0, 0, 1], (1, 2))
        assert de.cycle_count == cycles_after_first + 2

    def test_column_bank_integration(self):
        """DEMachine coordinates with ColumnBank correctly."""
        de = DEMachine()
        de.set_column_values([100, 200, 300, 400, 500, 600, 700, 800])
        values = de.get_column_values()
        assert values[0] == 100
        assert values[7] == 800

    def test_analytical_engine_integration(self):
        """DEMachine coordinates with AnalyticalEngine."""
        from backend.src.emulator.analytical_engine import BabbageNumber

        de = DEMachine()
        de.analytical_engine.registers["A"] = BabbageNumber(42)
        snapshot = de.get_snapshot()
        assert snapshot.ae_accumulator == 42

    def test_carriage_integration(self):
        """DEMachine coordinates with AnticipatingCarriage."""
        de = DEMachine()
        de.run_full_cycle()
        # Carriage should be deactivated after cycle
        assert de.carriage.is_active == False

    def test_timing_integration(self):
        """DEMachine coordinates with TimingController."""
        de = DEMachine()
        de.run_full_cycle()
        # After full cycle, timing should be at 0 degrees
        assert de.timing.angle == 0
        # And rotation count should be 1
        assert de.timing.rotation_count == 1

    def test_demachine_state_isolation(self):
        """Multiple DEMachine instances should be independent."""
        de1 = DEMachine()
        de2 = DEMachine()
        de1.run_full_cycle()
        assert de1.cycle_count == 1
        assert de2.cycle_count == 0

    def test_large_polynomial_range(self):
        """Evaluate polynomial over larger range."""
        de = DEMachine()
        results = de.evaluate_polynomial([1, 1], (1, 10))
        assert len(results) == 10
        # f(x) = 1 + x for x in 1..10
        expected = [2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
        assert results == expected

    def test_zero_coefficient_polynomial(self):
        """Evaluate polynomial with zero coefficients."""
        de = DEMachine()
        results = de.evaluate_polynomial([0, 0, 1], (1, 3))
        # f(x) = x^2
        assert results[0] == 1  # f(1) = 1
        assert results[1] == 4  # f(2) = 4
        assert results[2] == 9  # f(3) = 9

    def test_demachine_snapshot_types(self):
        """DEMachineSnapshot should have correct field types."""
        de = DEMachine()
        snapshot = de.get_snapshot()
        assert isinstance(snapshot.cycle_count, int)
        assert isinstance(snapshot.current_phase, MechanicalPhase)
        assert isinstance(snapshot.timing_angle, int)
        assert isinstance(snapshot.column_values, list)
        assert isinstance(snapshot.carry_signals, list)
        assert isinstance(snapshot.ae_accumulator, int)
        assert isinstance(snapshot.total_operations, int)

    def test_operation_phases_sequence(self):
        """Operations should follow expected phase sequence."""
        de = DEMachine()
        de.run_full_cycle()
        phases = [op.phase for op in de.operation_history]
        # Should have operations from multiple phases
        assert MechanicalPhase.INPUT in phases
        assert MechanicalPhase.ADDITION in phases
        assert MechanicalPhase.CARRY in phases

    def test_continuous_operation(self):
        """DEMachine should handle continuous operation without errors."""
        de = DEMachine()
        for _ in range(5):
            de.set_column_values([i for i in range(8)])
            de.run_full_cycle()
        # Should complete without errors
        assert de.cycle_count == 5
        assert de.total_operations > 0
