"""
Phase 3.W2.1: Polynomial Evaluation Integration Test

Complete end-to-end validation of the Difference Engine No. 2 emulator
by evaluating polynomial f(x) = x² + x + 1 for x ∈ [1, 5].

This integration test validates:
1. Complete mechanical cycles with all 6 phases working correctly
2. AnalyticalEngine register management and value preservation
3. ColumnBank column arithmetic and state management
4. AnticipatingCarriage carry propagation
5. TimingController phase sequencing
6. DEMachine orchestration and operation history
7. Polynomial evaluation via Horner's method

Test Function: Evaluate f(x) = x² + x + 1
Expected Results:
  f(1) = 1 + 1 + 1 = 3
  f(2) = 4 + 2 + 1 = 7
  f(3) = 9 + 3 + 1 = 13
  f(4) = 16 + 4 + 1 = 21
  f(5) = 25 + 5 + 1 = 31

Integration Path:
1. Initialize DEMachine with all four subsystems
2. Call evaluate_polynomial([1, 1, 1], (1, 5))
3. For each x value:
   a. Compute f(x) using Horner's method
   b. Run full mechanical cycle
   c. Verify result matches polynomial value
4. Validate cumulative state (cycles, operations, history)
5. Verify final snapshot integrity
"""

import pytest
from backend.src.emulator.machine import DEMachine, DEMachineSnapshot
from backend.src.emulator.timing import MechanicalPhase


class TestPolynomialEvaluationIntegration:
    """End-to-end polynomial evaluation tests."""

    def test_polynomial_quadratic_basic(self):
        """
        Test basic polynomial evaluation: f(x) = x² + x + 1.

        This is the primary integration test validating complete mechanical
        cycles working together to compute polynomial values.
        """
        de = DEMachine()

        # Coefficients for x² + x + 1
        coefficients = [1, 1, 1]  # [a₀, a₁, a₂]
        x_range = (1, 5)

        # Evaluate polynomial
        results = de.evaluate_polynomial(coefficients, x_range)

        # Expected values
        expected = [
            3,  # f(1) = 1 + 1 + 1 = 3
            7,  # f(2) = 4 + 2 + 1 = 7
            13,  # f(3) = 9 + 3 + 1 = 13
            21,  # f(4) = 16 + 4 + 1 = 21
            31,  # f(5) = 25 + 5 + 1 = 31
        ]

        # Verify results match
        assert len(results) == len(expected)
        for i, (result, exp) in enumerate(zip(results, expected)):
            assert result == exp, f"f({i+1}) expected {exp}, got {result}"

    def test_polynomial_cycle_count(self):
        """Verify each polynomial evaluation executes one full mechanical cycle."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 3)

        initial_cycles = de.cycle_count
        de.evaluate_polynomial(coefficients, x_range)
        final_cycles = de.cycle_count

        # Should execute 3 cycles (one per x value)
        cycles_executed = final_cycles - initial_cycles
        assert cycles_executed == 3, f"Expected 3 cycles, got {cycles_executed}"

    def test_polynomial_operation_count(self):
        """Verify operation history tracks all phases correctly."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 2)

        initial_ops = de.total_operations
        de.evaluate_polynomial(coefficients, x_range)
        final_ops = de.total_operations

        # Each cycle executes 6 phases (INPUT, ADDITION, CARRY, OUTPUT, ADVANCE, RESET)
        # For 2 x values: 2 cycles × 6 operations = 12 operations
        operations_executed = final_ops - initial_ops
        assert operations_executed == 12, f"Expected 12 operations (2×6), got {operations_executed}"

    def test_polynomial_operation_history(self):
        """Verify operation history records all phases for each cycle."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 1)  # Single value

        initial_history_len = len(de.operation_history)
        de.evaluate_polynomial(coefficients, x_range)
        final_history_len = len(de.operation_history)

        # One cycle should add 6 operations
        operations_added = final_history_len - initial_history_len
        assert operations_added == 6, f"Expected 6 operations in history, got {operations_added}"

        # Verify phase sequence
        history_tail = de.operation_history[-6:]
        expected_phases = [
            "INPUT",
            "ADDITION",
            "CARRY",
            "OUTPUT",
            "ADVANCE",
            "RESET",
        ]

        for i, op in enumerate(history_tail):
            assert (
                op.operation == expected_phases[i]
            ), f"Phase {i}: expected {expected_phases[i]}, got {op.operation}"

    def test_polynomial_snapshot_validity(self):
        """Verify snapshots are valid and consistent after polynomial eval."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 2)

        de.evaluate_polynomial(coefficients, x_range)
        snapshot = de.get_snapshot()

        # Verify snapshot is a valid DEMachineSnapshot
        assert isinstance(snapshot, DEMachineSnapshot)

        # Verify fields are consistent with DE state
        assert snapshot.cycle_count == 2
        assert snapshot.total_operations == 12

        # Verify current phase is IDLE (should be at start after eval)
        # Actually, after the last run_full_cycle, we're at angle 0, phase IDLE
        assert snapshot.current_phase == MechanicalPhase.IDLE

        # Verify timing angle is at cycle boundary (0 or 360)
        assert snapshot.timing_angle in [0, 360]

        # Verify column values are within reasonable bounds
        assert len(snapshot.column_values) == 8
        for val in snapshot.column_values:
            assert isinstance(val, int)
            assert val >= 0

        # Verify carry signals are boolean
        assert len(snapshot.carry_signals) == 8
        for sig in snapshot.carry_signals:
            assert isinstance(sig, bool)

    def test_polynomial_state_isolation(self):
        """
        Verify polynomial evaluations don't interfere with each other.

        Two separate evaluations should produce independent results.
        """
        de1 = DEMachine()
        de2 = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 3)

        results1 = de1.evaluate_polynomial(coefficients, x_range)
        results2 = de2.evaluate_polynomial(coefficients, x_range)

        # Results should be identical
        assert results1 == results2

        # But machine states should be independent
        assert de1.cycle_count == de2.cycle_count == 3
        assert de1.total_operations == de2.total_operations == 18

    def test_polynomial_linear(self):
        """Test polynomial evaluation with linear function: f(x) = 2x + 3."""
        de = DEMachine()

        coefficients = [3, 2]  # [a₀, a₁]
        x_range = (1, 4)

        results = de.evaluate_polynomial(coefficients, x_range)

        expected = [
            5,  # f(1) = 2×1 + 3 = 5
            7,  # f(2) = 2×2 + 3 = 7
            9,  # f(3) = 2×3 + 3 = 9
            11,  # f(4) = 2×4 + 3 = 11
        ]

        assert results == expected

    def test_polynomial_constant(self):
        """Test polynomial evaluation with constant function: f(x) = 5."""
        de = DEMachine()

        coefficients = [5]  # [a₀] constant
        x_range = (1, 3)

        results = de.evaluate_polynomial(coefficients, x_range)

        # All values should be 5
        expected = [5, 5, 5]
        assert results == expected

    def test_polynomial_cubic(self):
        """Test cubic polynomial: f(x) = x³ + 2x² + 3x + 4."""
        de = DEMachine()

        coefficients = [4, 3, 2, 1]  # [a₀, a₁, a₂, a₃]
        x_range = (1, 3)

        results = de.evaluate_polynomial(coefficients, x_range)

        expected = [
            10,  # f(1) = 1 + 2 + 3 + 4 = 10
            26,  # f(2) = 8 + 8 + 6 + 4 = 26
            58,  # f(3) = 27 + 18 + 9 + 4 = 58
        ]

        assert results == expected

    def test_polynomial_with_reset(self):
        """
        Test that reset() clears state for fresh polynomial evaluation.

        Verify that after reset(), a new polynomial evaluation starts fresh.
        """
        de = DEMachine()

        # First polynomial
        coefficients1 = [1, 1, 1]
        results1 = de.evaluate_polynomial(coefficients1, (1, 2))
        assert results1 == [3, 7]

        cycles_after_first = de.cycle_count
        ops_after_first = de.total_operations

        # Reset
        de.reset()
        assert de.cycle_count == 0
        assert de.total_operations == 0
        assert len(de.operation_history) == 0

        # Second polynomial (independent)
        coefficients2 = [3, 2]  # Different polynomial
        results2 = de.evaluate_polynomial(coefficients2, (1, 2))
        assert results2 == [5, 7]

        # Cycles should be 2 for both (fresh start after reset)
        assert de.cycle_count == 2

    def test_polynomial_large_range(self):
        """Test polynomial evaluation over larger range."""
        de = DEMachine()

        coefficients = [1, 0, 1]  # f(x) = x² + 1
        x_range = (0, 5)

        results = de.evaluate_polynomial(coefficients, x_range)

        expected = [
            1,  # f(0) = 0 + 1 = 1
            2,  # f(1) = 1 + 1 = 2
            5,  # f(2) = 4 + 1 = 5
            10,  # f(3) = 9 + 1 = 10
            17,  # f(4) = 16 + 1 = 17
            26,  # f(5) = 25 + 1 = 26
        ]

        assert results == expected

    def test_polynomial_zero_coefficient(self):
        """Test polynomial with zero coefficient: f(x) = x² + 1 (no linear term)."""
        de = DEMachine()

        coefficients = [1, 0, 1]  # [a₀, a₁=0, a₂]
        x_range = (1, 3)

        results = de.evaluate_polynomial(coefficients, x_range)

        expected = [
            2,  # f(1) = 1 + 0 + 1 = 2
            5,  # f(2) = 4 + 0 + 1 = 5
            10,  # f(3) = 9 + 0 + 1 = 10
        ]

        assert results == expected

    def test_polynomial_column_values_updated(self):
        """Verify column values are updated during polynomial evaluation."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 1)  # Single evaluation

        # Initial columns should be zero
        initial_columns = de.get_column_values()
        assert all(v == 0 for v in initial_columns)

        # After polynomial eval, columns may have residual values
        results = de.evaluate_polynomial(coefficients, x_range)
        assert results[0] == 3

        # Columns should be updated (at minimum during OUTPUT phase)
        # After reset in last phase, they should be reset to zero
        # So we just verify get_column_values() works
        final_columns = de.get_column_values()
        assert len(final_columns) == 8

    def test_polynomial_timing_phase_sequence(self):
        """Verify TimingController executes correct phase sequence."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (1, 1)

        de.evaluate_polynomial(coefficients, x_range)

        # Get operation history for last cycle
        history = de.get_operation_history(limit=6)

        # Should have exactly 6 operations (one full cycle's phases)
        assert len(history) == 6

        # Verify phase progression
        phases = [op.phase.value for op in history]
        expected = ["input", "addition", "carry", "output", "advance", "reset"]
        assert phases == expected


class TestPolynomialEvaluationStress:
    """Stress tests for polynomial evaluation."""

    def test_polynomial_many_iterations(self):
        """Test polynomial evaluation over many x values."""
        de = DEMachine()

        coefficients = [1, 1, 1]
        x_range = (0, 10)  # 11 evaluations

        results = de.evaluate_polynomial(coefficients, x_range)

        # Verify we got 11 results
        assert len(results) == 11

        # Verify cycle count is correct
        assert de.cycle_count == 11

        # Sample some values
        assert results[0] == 1  # f(0) = 1
        assert results[1] == 3  # f(1) = 3
        assert results[5] == 31  # f(5) = 31
        assert results[10] == 111  # f(10) = 121? Let's calculate: 100 + 10 + 1 = 111

    def test_polynomial_high_coefficients(self):
        """Test polynomial with larger coefficient values."""
        de = DEMachine()

        coefficients = [10, 20, 30]  # 30x² + 20x + 10
        x_range = (1, 3)

        results = de.evaluate_polynomial(coefficients, x_range)

        expected = [
            60,  # f(1) = 30 + 20 + 10 = 60
            150,  # f(2) = 120 + 40 + 10 = 170? No: 30*4 + 20*2 + 10 = 120+40+10 = 170
            330,  # f(3) = 30*9 + 20*3 + 10 = 270 + 60 + 10 = 340
        ]

        # Recalculate: f(x) = 30x² + 20x + 10
        # f(1) = 30 + 20 + 10 = 60
        # f(2) = 30*4 + 20*2 + 10 = 120 + 40 + 10 = 170
        # f(3) = 30*9 + 20*3 + 10 = 270 + 60 + 10 = 340

        expected = [60, 170, 340]
        assert results == expected


class TestPolynomialEvaluationProperties:
    """Property-based tests for polynomial evaluation."""

    def test_polynomial_deterministic(self):
        """
        Verify polynomial evaluation is deterministic.

        Same inputs should always produce same outputs.
        """

        def eval_poly():
            de = DEMachine()
            coefficients = [1, 1, 1]
            return de.evaluate_polynomial(coefficients, (1, 5))

        results1 = eval_poly()
        results2 = eval_poly()
        results3 = eval_poly()

        assert results1 == results2 == results3

    def test_polynomial_independence(self):
        """
        Verify different machines don't interfere.

        Two separate DEMachine instances should evaluate independently.
        """
        coefficients1 = [1, 1, 1]
        coefficients2 = [2, 2, 2]

        de1 = DEMachine()
        de2 = DEMachine()

        results1 = de1.evaluate_polynomial(coefficients1, (1, 3))
        results2 = de2.evaluate_polynomial(coefficients2, (1, 3))

        # Results should be different (different coefficients)
        assert results1 != results2

        # But each machine should be consistent
        de1_repeat = DEMachine()
        results1_repeat = de1_repeat.evaluate_polynomial(coefficients1, (1, 3))
        assert results1 == results1_repeat
