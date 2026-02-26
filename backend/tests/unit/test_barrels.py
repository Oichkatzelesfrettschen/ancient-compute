"""Tests for Analytical Engine Barrel micro-architecture.

Validates micro-op sequencing for all four barrel programs (ADD, SUB, MULT, DIV)
and the BarrelController dispatch mechanism.
"""

import pytest

from backend.src.emulator.barrels import (
    Barrel,
    BarrelController,
    BarrelRow,
    MicroOp,
)
from backend.src.emulator.timing import (
    CarryPropagationModel,
    BarrelTimingBridge,
    build_opcode_timing_sequences,
    MechanicalPhase,
)


# ---------------------------------------------------------------------------
# BarrelController construction
# ---------------------------------------------------------------------------

class TestBarrelControllerInit:
    """Verify that all standard barrels are registered on construction."""

    def test_five_barrels_registered(self):
        ctrl = BarrelController()
        assert set(ctrl.barrels.keys()) == {"ADD", "SUB", "MULT", "DIV", "SQRT"}

    def test_initial_state_idle(self):
        ctrl = BarrelController()
        assert ctrl.active_barrel is None
        assert ctrl.step_index == 0


# ---------------------------------------------------------------------------
# Barrel selection
# ---------------------------------------------------------------------------

class TestBarrelSelection:

    def test_select_known_barrel(self):
        ctrl = BarrelController()
        ctrl.select_barrel("ADD")
        assert ctrl.active_barrel == "ADD"
        assert ctrl.step_index == 0

    def test_select_unknown_barrel_raises(self):
        ctrl = BarrelController()
        with pytest.raises(ValueError, match="Unknown barrel"):
            ctrl.select_barrel("BOGUS")

    def test_select_resets_step_index(self):
        ctrl = BarrelController()
        ctrl.select_barrel("ADD")
        ctrl.step()  # advance one step
        ctrl.select_barrel("SUB")
        assert ctrl.step_index == 0


# ---------------------------------------------------------------------------
# Step sequencing
# ---------------------------------------------------------------------------

class TestStepSequencing:

    def test_step_returns_empty_when_idle(self):
        ctrl = BarrelController()
        assert ctrl.step() == []

    def test_step_advances_index(self):
        ctrl = BarrelController()
        ctrl.select_barrel("ADD")
        ctrl.step()
        assert ctrl.step_index == 1

    def test_barrel_finishes_after_all_steps(self):
        ctrl = BarrelController()
        ctrl.select_barrel("ADD")
        n_steps = len(ctrl.barrels["ADD"].rows)
        for _ in range(n_steps):
            ops = ctrl.step()
            assert len(ops) > 0
        # Next step should indicate completion
        assert ctrl.step() == []
        assert ctrl.active_barrel is None


# ---------------------------------------------------------------------------
# ADD barrel micro-ops
# ---------------------------------------------------------------------------

class TestADDBarrel:

    def setup_method(self):
        self.ctrl = BarrelController()
        self.ctrl.select_barrel("ADD")

    def test_step_count(self):
        assert len(self.ctrl.barrels["ADD"].rows) == 6

    def test_step_0_fetch_and_lift(self):
        ops = self.ctrl.step()
        assert MicroOp.FETCH_VAR_CARD in ops
        assert MicroOp.LIFT_STORE_AXIS in ops

    def test_step_1_connect_store_to_mill(self):
        self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.CONNECT_STORE_TO_MILL in ops
        assert MicroOp.DROP_STORE_AXIS in ops

    def test_step_2_lift_ingress(self):
        for _ in range(2):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.LIFT_INGRESS in ops

    def test_step_3_advance_mill(self):
        for _ in range(3):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.ADVANCE_MILL in ops

    def test_step_4_run_carry(self):
        for _ in range(4):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.RUN_CARRY in ops

    def test_step_5_egress(self):
        for _ in range(5):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.DROP_INGRESS in ops
        assert MicroOp.LIFT_EGRESS in ops


# ---------------------------------------------------------------------------
# SUB barrel micro-ops
# ---------------------------------------------------------------------------

class TestSUBBarrel:

    def setup_method(self):
        self.ctrl = BarrelController()
        self.ctrl.select_barrel("SUB")

    def test_step_count(self):
        assert len(self.ctrl.barrels["SUB"].rows) == 6

    def test_uses_reverse_mill(self):
        """SUB barrel must use REVERSE_MILL instead of ADVANCE_MILL."""
        for _ in range(3):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REVERSE_MILL in ops


# ---------------------------------------------------------------------------
# MULT barrel micro-ops
# ---------------------------------------------------------------------------

class TestMULTBarrel:

    def setup_method(self):
        self.ctrl = BarrelController()
        self.ctrl.select_barrel("MULT")

    def test_step_count(self):
        assert len(self.ctrl.barrels["MULT"].rows) == 4

    def test_step_0_get_multiplier_digit(self):
        ops = self.ctrl.step()
        assert MicroOp.GET_MULTIPLIER_DIGIT in ops

    def test_step_1_advance_and_decrement(self):
        self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.ADVANCE_MILL in ops
        assert MicroOp.DECREMENT_COUNTER in ops

    def test_step_2_repeat_if_counter(self):
        for _ in range(2):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REPEAT_IF_COUNTER in ops

    def test_step_3_store_accumulator(self):
        for _ in range(3):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.STORE_MILL_ACCUMULATOR in ops

    def test_full_sequence_completes(self):
        for _ in range(4):
            ops = self.ctrl.step()
            assert len(ops) > 0
        assert self.ctrl.step() == []


# ---------------------------------------------------------------------------
# DIV barrel micro-ops (restoring division algorithm)
# ---------------------------------------------------------------------------

class TestDIVBarrel:

    def setup_method(self):
        self.ctrl = BarrelController()
        self.ctrl.select_barrel("DIV")

    def test_step_count(self):
        assert len(self.ctrl.barrels["DIV"].rows) == 13

    def test_step_0_fetch_dividend(self):
        ops = self.ctrl.step()
        assert MicroOp.FETCH_VAR_CARD in ops
        assert MicroOp.LIFT_STORE_AXIS in ops

    def test_step_1_load_accumulator(self):
        self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.LOAD_MILL_ACCUMULATOR in ops
        assert MicroOp.CONNECT_STORE_TO_MILL in ops

    def test_step_4_compare(self):
        for _ in range(4):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.COMPARE_MILL_BUFFERS in ops

    def test_step_5_conditional_subtract(self):
        for _ in range(5):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REVERSE_MILL in ops

    def test_step_6_increment_quotient(self):
        for _ in range(6):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.INCREMENT_QUOTIENT_DIGIT in ops

    def test_step_7_repeat_loop(self):
        for _ in range(7):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REPEAT_IF_COUNTER in ops

    def test_step_8_shift_divisor(self):
        for _ in range(8):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.SHIFT_MILL_DIVISOR in ops

    def test_step_12_store_remainder(self):
        for _ in range(12):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.STORE_MILL_ACCUMULATOR in ops

    def test_full_sequence_completes(self):
        for _ in range(13):
            ops = self.ctrl.step()
            assert len(ops) > 0
        assert self.ctrl.step() == []


# ---------------------------------------------------------------------------
# Custom barrel construction
# ---------------------------------------------------------------------------

class TestCustomBarrel:

    def test_empty_barrel_finishes_immediately(self):
        b = Barrel("EMPTY")
        ctrl = BarrelController()
        ctrl.barrels["EMPTY"] = b
        ctrl.select_barrel("EMPTY")
        assert ctrl.step() == []

    def test_single_step_barrel(self):
        b = Barrel("ONE")
        b.add_step([MicroOp.RESET_MILL])
        ctrl = BarrelController()
        ctrl.barrels["ONE"] = b
        ctrl.select_barrel("ONE")
        ops = ctrl.step()
        assert MicroOp.RESET_MILL in ops
        assert ctrl.step() == []

    def test_barrel_row_stores_as_set(self):
        row = BarrelRow({MicroOp.ADVANCE_MILL, MicroOp.RUN_CARRY})
        assert MicroOp.ADVANCE_MILL in row.studs
        assert MicroOp.RUN_CARRY in row.studs


# ---------------------------------------------------------------------------
# Phase II: Regression tests for MULT barrel infinite loop fix
# ---------------------------------------------------------------------------

class TestMULTBarrelRegression:
    """Regression: MULT barrel counter=3 terminates in bounded iterations.

    Historical bug: REPEAT_IF_COUNTER jumped to itself (step 2 -> step 2)
    instead of back to step 1, causing infinite loop. Fixed by rewriting
    MULT from 5 steps to 4 and using step_index-2 accounting.
    """

    def test_mult_barrel_terminates_bounded(self):
        """MULT barrel completes in at most 100 steps (well under limit)."""
        ctrl = BarrelController()
        ctrl.select_barrel("MULT")
        steps = 0
        while ctrl.active_barrel is not None and steps < 100:
            ctrl.step()
            steps += 1
        assert ctrl.active_barrel is None, (
            f"MULT barrel did not terminate after {steps} steps"
        )
        assert steps <= 10, f"MULT barrel took {steps} steps (expected <= 10)"

    def test_mult_barrel_repeat_if_counter_targets_correct_step(self):
        """REPEAT_IF_COUNTER is at step 2, which must jump to step 1 (not itself)."""
        ctrl = BarrelController()
        mult_barrel = ctrl.barrels["MULT"]
        # Step 2 has REPEAT_IF_COUNTER
        assert MicroOp.REPEAT_IF_COUNTER in mult_barrel.rows[2].studs
        # Step 1 has ADVANCE_MILL + DECREMENT_COUNTER (the loop body)
        assert MicroOp.ADVANCE_MILL in mult_barrel.rows[1].studs
        assert MicroOp.DECREMENT_COUNTER in mult_barrel.rows[1].studs


# ---------------------------------------------------------------------------
# SQRT barrel micro-ops
# ---------------------------------------------------------------------------

class TestSQRTBarrel:

    def setup_method(self):
        self.ctrl = BarrelController()
        self.ctrl.select_barrel("SQRT")

    def test_step_count(self):
        assert len(self.ctrl.barrels["SQRT"].rows) == 6

    def test_step_0_init_guess(self):
        ops = self.ctrl.step()
        assert MicroOp.INIT_SQRT_GUESS in ops

    def test_step_1_divide(self):
        self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.DIVIDE_S_BY_X in ops

    def test_step_2_add(self):
        for _ in range(2):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.ADD_X_AND_QUOTIENT in ops

    def test_step_3_halve(self):
        for _ in range(3):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.HALVE_ACCUMULATOR in ops

    def test_step_4_convergence_check(self):
        for _ in range(4):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.CHECK_SQRT_CONVERGENCE in ops

    def test_step_5_store_result(self):
        for _ in range(5):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.STORE_SQRT_RESULT in ops

    def test_full_sequence_completes(self):
        for _ in range(6):
            ops = self.ctrl.step()
            assert len(ops) > 0
        assert self.ctrl.step() == []


# ---------------------------------------------------------------------------
# Phase VII: Carry Propagation Timing
# ---------------------------------------------------------------------------

class TestCarryPropagation:
    def test_carry_fits_within_phase(self):
        """Worst-case 50-digit carry with 2-position look-ahead fits in 45 degrees."""
        degrees = CarryPropagationModel.worst_case_degrees(50)
        assert CarryPropagationModel.fits_within_phase(degrees)

    def test_carry_zero_for_no_digits(self):
        degrees = CarryPropagationModel.carry_propagation_degrees(0)
        assert degrees == 0.0

    def test_anticipating_carriage_reduces_propagation(self):
        """Look-ahead reduces carry time vs no look-ahead."""
        with_la = CarryPropagationModel.carry_propagation_degrees(50, 2)
        without_la = CarryPropagationModel.carry_propagation_degrees(50, 0)
        assert with_la < without_la


# ---------------------------------------------------------------------------
# Phase VII: Barrel-Timing Bridge
# ---------------------------------------------------------------------------

class TestBarrelTimingBridge:
    def test_add_uses_one_rotation(self):
        assert BarrelTimingBridge.rotations_required("ADD") <= 1.0

    def test_mult_spans_full_rotation(self):
        assert BarrelTimingBridge.rotations_required("MULT") >= 1.0

    def test_all_barrels_have_timing(self):
        for name in ["ADD", "SUB", "MULT", "DIV", "SQRT", "LOAD", "STOR"]:
            assert BarrelTimingBridge.total_degrees(name) > 0

    def test_timing_consistent_with_barrel_steps(self):
        ctrl = BarrelController()
        for name in ["ADD", "SUB", "MULT", "DIV", "SQRT"]:
            barrel = ctrl.barrels[name]
            n_steps = len(barrel.rows)
            dps = BarrelTimingBridge.degrees_per_step(name)
            total = dps * n_steps
            expected = BarrelTimingBridge.total_degrees(name)
            assert total == pytest.approx(expected, rel=0.01)


# ---------------------------------------------------------------------------
# Phase VII: Opcode Timing Sequences
# ---------------------------------------------------------------------------

class TestOpcodeTimingSequences:
    def test_add_uses_addition_and_carry(self):
        seqs = build_opcode_timing_sequences()
        add = seqs["ADD"]
        phases_used = list(add.operations.keys())
        assert MechanicalPhase.ADDITION in phases_used
        assert MechanicalPhase.CARRY in phases_used

    def test_mult_spans_multiple_rotations(self):
        seqs = build_opcode_timing_sequences()
        assert seqs["MULT"].duration > 360

    def test_all_opcodes_have_sequences(self):
        seqs = build_opcode_timing_sequences()
        for name in ["ADD", "SUB", "MULT", "DIV", "SQRT", "LOAD", "STOR"]:
            assert name in seqs
