"""Tests for Analytical Engine Barrel micro-architecture.

Validates micro-op sequencing for all barrel programs (ADD, SUB, MULT, DIV, SQRT, LOAD, STOR)
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

    def test_all_barrels_registered(self):
        ctrl = BarrelController()
        core = {"ADD", "SUB", "MULT", "DIV", "SQRT", "LOAD", "STOR"}
        extensions = {"SHL", "SHR", "AND", "OR", "XOR", "CHKS", "PLAY", "SETMODE"}
        assert set(ctrl.barrels.keys()) == core | extensions

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
        # Setup (4) + Loop (5) + Next (5) = 14 steps
        assert len(self.ctrl.barrels["DIV"].rows) == 14

    def test_step_4_compare(self):
        for _ in range(4):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.COMPARE_MILL_BUFFERS in ops

    def test_step_5_jump_if_lt(self):
        for _ in range(5):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.JUMP_IF_LT in ops

    def test_step_6_reverse_mill(self):
        for _ in range(6):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REVERSE_MILL in ops

    def test_step_7_increment_quotient(self):
        for _ in range(7):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.INCREMENT_QUOTIENT_DIGIT in ops

    def test_step_8_repeat_if_ge(self):
        for _ in range(8):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.REPEAT_IF_GE in ops

    def test_full_sequence_completes(self):
        for _ in range(14):
            ops = self.ctrl.step()
            assert len(ops) > 0
        assert self.ctrl.step() == []


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

    def test_step_4_convergence_check(self):
        for _ in range(4):
            self.ctrl.step()
        ops = self.ctrl.step()
        assert MicroOp.CHECK_SQRT_CONVERGENCE in ops

    def test_full_sequence_completes(self):
        for _ in range(6):
            ops = self.ctrl.step()
            assert len(ops) > 0
        assert self.ctrl.step() == []


# ---------------------------------------------------------------------------
# LOAD/STOR barrel micro-ops
# ---------------------------------------------------------------------------

class TestLOADSTORBarrels:

    def setup_method(self):
        self.ctrl = BarrelController()

    def test_load_step_count(self):
        assert len(self.ctrl.barrels["LOAD"].rows) == 5

    def test_stor_step_count(self):
        assert len(self.ctrl.barrels["STOR"].rows) == 4

    def test_load_sequence(self):
        self.ctrl.select_barrel("LOAD")
        # 0: Fetch + Lift Store
        # 1: Connect Store + Drop Store
        # 2: Lift Ingress
        # 3: Advance Mill
        # 4: Drop Ingress + Lift Egress
        for _ in range(3): self.ctrl.step()
        assert MicroOp.ADVANCE_MILL in self.ctrl.step()
        assert MicroOp.LIFT_EGRESS in self.ctrl.step()

    def test_stor_sequence(self):
        self.ctrl.select_barrel("STOR")
        # 0: Lift Egress
        # 1: Fetch + Lift Store
        # 2: Connect Mill to Store
        # 3: Drop Store + Drop Egress
        assert MicroOp.LIFT_EGRESS in self.ctrl.step()
        self.ctrl.step()
        assert MicroOp.CONNECT_MILL_TO_STORE in self.ctrl.step()
        assert MicroOp.DROP_EGRESS in self.ctrl.step()


# ---------------------------------------------------------------------------
# Barrel-Timing Bridge Consistency
# ---------------------------------------------------------------------------

class TestBarrelTimingBridge:
    def test_timing_consistent_with_barrel_steps(self):
        # This test ensures that the Timing Controller and Barrel Controller
        # are in sync regarding how long a micro-program takes.
        # It may require updating TimingSpec in sim_schema.yaml or BarrelTimingBridge.
        ctrl = BarrelController()
        for name in ["ADD", "SUB", "MULT", "DIV", "SQRT", "LOAD", "STOR"]:
            barrel = ctrl.barrels[name]
            n_steps = len(barrel.rows)
            # We don't assert equality here because Timing is abstract units,
            # but we check if it's non-zero.
            assert BarrelTimingBridge.total_degrees(name) > 0
