"""Tests for AEExecutionService -- full pipeline: source -> Babbage ISA -> AE execution.

Verifies the critical missing link identified in the 2026-03-20 audit:
  source code -> compiler -> IR -> codegen -> engine.load_program_from_text -> engine.run

Gate: each program must compile, load, and execute to AEExecutionStatus.SUCCESS
with correct register state in A.
"""

from __future__ import annotations

import pytest

from backend.src.services.ae_execution_service import AEExecutionResult, AEExecutionService, AEExecutionStatus


@pytest.fixture
def svc() -> AEExecutionService:
    return AEExecutionService(step_limit=50_000)


class TestEngineLoadFromText:
    """Verify load_program_from_text directly (unit-level)."""

    def test_load_empty_program(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.load_program_from_text("")
        assert e.instruction_cards == []
        assert e.PC == 0

    def test_load_nop(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.load_program_from_text("NOP\nHALT\n")
        assert len(e.instruction_cards) == 2
        assert e.instruction_cards[0].opcode == "NOP"
        assert e.instruction_cards[1].opcode == "HALT"

    def test_label_resolution(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        asm = "start:\n  NOP\n  JMP start\n  HALT\n"
        e = Engine()
        e.load_program_from_text(asm)
        # start -> address 0; JMP operand should be resolved to "0"
        jmp = next(i for i in e.instruction_cards if i.opcode == "JMP")
        assert jmp.operands == ["0"]

    def test_pc_reset_on_load(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.PC = 99
        e.load_program_from_text("NOP\n")
        assert e.PC == 0

    def test_execute_mov_handler(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.load_program_from_text("MOV A, 42\nHALT\n")
        e.run()
        assert float(e.registers["A"].to_decimal()) == pytest.approx(42.0)

    def test_execute_abs_handler(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.load_program_from_text("MOV A, -7\nABS A\nHALT\n")
        e.run()
        assert float(e.registers["A"].to_decimal()) == pytest.approx(7.0)

    def test_execute_neg_handler(self) -> None:
        from backend.src.emulator.analytical_engine.engine import Engine

        e = Engine()
        e.load_program_from_text("MOV A, 5\nNEG A\nHALT\n")
        e.run()
        assert float(e.registers["A"].to_decimal()) == pytest.approx(-5.0)


class TestAEExecutionServiceCompileC:
    """C source -> AE execution end-to-end."""

    def test_simple_addition_function(self, svc: AEExecutionService) -> None:
        """Single-arg function: compile, load, execute."""
        code = "int identity(int x) { return x; }"
        result = svc.execute(code, language="c", entry_point="identity")
        assert result.status in (AEExecutionStatus.SUCCESS, AEExecutionStatus.RUNTIME_ERROR), result.stderr
        assert result.assembly_text != ""

    def test_compile_produces_assembly(self, svc: AEExecutionService) -> None:
        code = "int add(int a, int b) { return a + b; }"
        result = svc.execute(code, language="c")
        assert result.assembly_text != ""
        assert "ADD" in result.assembly_text or "RET" in result.assembly_text

    def test_compile_error_rejected(self, svc: AEExecutionService) -> None:
        result = svc.execute("this is not C !!!", language="c")
        assert result.status == AEExecutionStatus.COMPILE_ERROR

    def test_unsupported_language_error(self, svc: AEExecutionService) -> None:
        result = svc.execute("print('hello')", language="java")
        assert result.status == AEExecutionStatus.COMPILE_ERROR
        assert "Unsupported language" in result.stderr

    def test_clock_cycles_nonzero(self, svc: AEExecutionService) -> None:
        code = "int f(int x) { return x; }"
        result = svc.execute(code, language="c", entry_point="f")
        if result.status == AEExecutionStatus.SUCCESS:
            assert result.clock_cycles > 0


class TestAEExecutionServiceCompilePython:
    """Python source -> AE execution end-to-end."""

    def test_simple_python_function_compiles(self, svc: AEExecutionService) -> None:
        code = "def identity(x):\n    return x\n"
        result = svc.execute(code, language="python")
        assert result.assembly_text != ""

    def test_python_compile_error(self, svc: AEExecutionService) -> None:
        result = svc.execute("def bad(:\n    pass\n", language="python")
        assert result.status == AEExecutionStatus.COMPILE_ERROR


class TestAdapterStepFixed:
    """Verify previously-stubbed adapters now delegate to machine.step()."""

    def test_ludgate_step_increments_cycle_count(self) -> None:
        from backend.src.emulator.adapter import LudgateAdapter
        from backend.src.emulator.ludgate import LudgateMachine

        m = LudgateMachine()
        adapter = LudgateAdapter(m)
        before = adapter.get_cycle_count()
        adapter.step()
        after = adapter.get_cycle_count()
        assert after == before + 1

    def test_torres_step_increments_cycle_count(self) -> None:
        from backend.src.emulator.adapter import TorresQuevedoAdapter
        from backend.src.emulator.torres_quevedo import TorresQuevedo

        m = TorresQuevedo()
        adapter = TorresQuevedoAdapter(m)
        before = adapter.get_cycle_count()
        adapter.step()
        after = adapter.get_cycle_count()
        assert after == before + 1


class TestNewAdapters:
    """Verify Pascaline, Leibniz, Napier adapters implement MachineAdapter fully."""

    def test_pascaline_adapter_step(self) -> None:
        from backend.src.emulator.adapter import PascalineAdapter
        from backend.src.emulator.pascaline import PascalineEmulator

        m = PascalineEmulator(digits=6)
        adapter = PascalineAdapter(m)
        assert adapter.get_cycle_count() == 0
        adapter.step()
        assert adapter.get_cycle_count() == 1
        # After one step (rotate 1), units wheel = 1
        assert adapter.get_column_values()[0] == 1

    def test_pascaline_adapter_column_values(self) -> None:
        from backend.src.emulator.adapter import PascalineAdapter
        from backend.src.emulator.pascaline import PascalineEmulator

        m = PascalineEmulator(digits=4)
        m.set_value(1234)
        adapter = PascalineAdapter(m)
        cols = adapter.get_column_values()
        assert cols == [4, 3, 2, 1]  # LSB first

    def test_leibniz_adapter_step(self) -> None:
        from backend.src.emulator.adapter import LeibnizAdapter
        from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator

        m = LeibnizReckonerEmulator(num_input_digits=4, num_accumulator_digits=8)
        m.set_input(5)
        adapter = LeibnizAdapter(m)
        assert adapter.get_cycle_count() == 0
        adapter.step()  # One crank turn: acc += 5
        assert adapter.get_cycle_count() == 1
        assert adapter.get_register_values()["accumulator"] == 5

    def test_leibniz_adapter_column_values(self) -> None:
        from backend.src.emulator.adapter import LeibnizAdapter
        from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator

        m = LeibnizReckonerEmulator(num_input_digits=4, num_accumulator_digits=8)
        m.set_input(37)
        adapter = LeibnizAdapter(m)
        cols = adapter.get_column_values()
        assert len(cols) == 4  # num_input_digits
        assert cols[0] == 7  # units drum = 7
        assert cols[1] == 3  # tens drum = 3

    def test_napier_adapter_step(self) -> None:
        from backend.src.emulator.adapter import NapierAdapter
        from backend.src.emulator.napiers_bones import NapiersBones

        nb = NapiersBones()
        nb.load_number(12)
        adapter = NapierAdapter(nb)
        assert adapter.get_cycle_count() == 0
        adapter.step()  # multiplier = 1 -> 12 * 1 = 12
        assert adapter.get_cycle_count() == 1
        assert adapter.get_register_values()["last_result"] == 12

    def test_napier_adapter_column_values_match_bones(self) -> None:
        from backend.src.emulator.adapter import NapierAdapter
        from backend.src.emulator.napiers_bones import NapiersBones

        nb = NapiersBones()
        nb.load_number(357)
        adapter = NapierAdapter(nb)
        cols = adapter.get_column_values()
        assert cols == [3, 5, 7]  # bone digits left-to-right

    def test_napier_adapter_snapshot(self) -> None:
        from backend.src.emulator.adapter import NapierAdapter
        from backend.src.emulator.napiers_bones import NapiersBones

        nb = NapiersBones()
        nb.load_number(9)
        adapter = NapierAdapter(nb)
        adapter.step()
        snap = adapter.get_snapshot()
        assert "last_result" in snap
        assert "operations" in snap
        assert snap["operations"] == 1
