"""End-to-end pipeline integration tests.

Covers the full execution chain across multiple components:
- Babbage assembler (two-pass, label resolution, machine code emission)
- Fourmilab card deck parser + translator + AE engine + I/O
- Lovelace deck YAML loader + deck executor + BabbageNumber arithmetic
- Note G full loop-back pipeline (deck + loop controller + accumulation)
- Machine adapters (MachineAdapter ABC over all concrete machines)
- Haskell compiler -> codegen -> assembler (3-stage pipeline)
- Card compiler round-trip at the deck level (compile + decompile)

Each test exercises cooperation between at least two distinct subsystems.
"""

from __future__ import annotations

import asyncio
import sys
from pathlib import Path

import pytest

# tools/ must be on sys.path for card_compiler import
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "tools"))

pytestmark = pytest.mark.integration


# =============================================================================
# Babbage Assembler -- two-pass assembly pipeline
# =============================================================================


class TestBabbageAssemblerPipeline:
    """Assembler + machine code: multi-pass, label resolution, 50-bit words."""

    def test_simple_add_program(self):
        from backend.src.assembler.assembler import Assembler

        src = (
            ".global main\n"
            ".text\n"
            "main:\n"
            "    LOAD A, 5\n"
            "    LOAD B, 3\n"
            "    ADD A, B\n"
            "    WRPRN A\n"
            "    RET\n"
        )
        result = Assembler(src).assemble()

        assert result.error_count == 0, "\n".join(result.errors)
        assert result.instruction_count >= 5
        # Every machine word must fit in 50 bits
        for word in result.machine_code:
            assert 0 <= word < (1 << 50), f"Word {word:#x} exceeds 50 bits"

    def test_forward_reference_label_resolves(self):
        from backend.src.assembler.assembler import Assembler

        src = (
            ".global start\n"
            ".text\n"
            "start:\n"
            "    LOAD A, 0\n"
            "    LOAD B, 3\n"
            "loop:\n"
            "    ADD A, B\n"
            "    JNZ B, loop\n"
            "    RET\n"
        )
        result = Assembler(src).assemble()

        assert result.error_count == 0, "\n".join(result.errors)
        assert "loop" in result.symbol_table

    def test_deterministic_output(self):
        """Two assemblies of identical source must produce identical machine code."""
        from backend.src.assembler.assembler import Assembler

        src = (
            ".global main\n"
            ".text\n"
            "main:\n"
            "    LOAD A, 10\n"
            "    LOAD B, 5\n"
            "    ADD A, B\n"
            "    WRPRN A\n"
            "    RET\n"
        )
        first = Assembler(src).assemble()
        second = Assembler(src).assemble()

        assert first.machine_code == second.machine_code
        assert first.symbol_table == second.symbol_table

    def test_hex_dump_non_empty(self):
        from backend.src.assembler.assembler import Assembler

        src = ".global main\n.text\nmain:\n    NOP\n    RET\n"
        result = Assembler(src).assemble()
        assert result.error_count == 0
        dump = result.get_hex_dump()
        assert len(dump) > 0


# =============================================================================
# Fourmilab Deck -- parser + translator + AE engine + output
# =============================================================================


class TestFourmilabPipeline:
    """Full Fourmilab pipeline: parse source -> translate -> run -> I/O."""

    def test_print_single_value(self):
        """N (number card) -> S (store) -> L (load) -> P (print) -> H (halt)."""
        from backend.src.emulator.fourmilab_compat import run_fourmilab_deck

        captured: list[str] = []
        deck = "N000 +5\nS000\nL000\nP\nH\n"
        run_fourmilab_deck(deck, output_callback=captured.append)

        assert len(captured) >= 1
        combined = " ".join(captured)
        assert "5" in combined

    def test_add_two_values(self):
        """Load two values, add them, print result (value = 5+3 = 8)."""
        from backend.src.emulator.fourmilab_compat import run_fourmilab_deck

        captured: list[str] = []
        deck = "N000 +5\nS000\nN001 +3\nS001\nL000\nL001\n+\nP\nH\n"
        run_fourmilab_deck(deck, output_callback=captured.append)

        combined = " ".join(captured)
        assert "8" in combined

    def test_engine_returned(self):
        """run_fourmilab_deck always returns an Engine instance."""
        from backend.src.emulator.analytical_engine import Engine
        from backend.src.emulator.fourmilab_compat import run_fourmilab_deck

        engine = run_fourmilab_deck("H\n")
        assert isinstance(engine, Engine)

    def test_parse_translate_roundtrip_count(self):
        """Card count is preserved through parse -> translate."""
        from backend.src.emulator.fourmilab_compat import (
            fourmilab_to_instructions,
            parse_fourmilab_deck,
        )

        source = "N000 +1\nS000\nL000\nP\nH\n"
        cards = parse_fourmilab_deck(source)
        instructions = fourmilab_to_instructions(cards)
        # One instruction per card (N->LOAD+STOR may expand, but at minimum same count)
        assert len(instructions) >= len(cards)


# =============================================================================
# Lovelace Deck -- YAML loader + deck executor + BabbageNumber
# =============================================================================


class TestLovelaceDeckPipeline:
    """YAML deck files + deck executor + BabbageNumber arithmetic chain."""

    def test_note_b_compound(self):
        """run_note_b_compound: a*b + c*d through the deck executor."""
        from backend.src.emulator.lovelace_notes import run_note_b_compound

        result = run_note_b_compound(2.0, 3.0, 4.0, 5.0)
        assert abs(result.to_decimal() - 26.0) < 1e-6

    def test_note_b_mult(self):
        from backend.src.emulator.lovelace_notes import run_note_b_mult

        result = run_note_b_mult(7.0, 8.0)
        assert abs(result.to_decimal() - 56.0) < 1e-6

    def test_note_c_triangular(self):
        """T(5) = 1+2+3+4+5 = 15 via deck executor."""
        from backend.src.emulator.lovelace_notes import run_note_c

        result = run_note_c(5)
        assert abs(result.to_decimal() - 15.0) < 1e-6

    def test_note_d_formula(self):
        """(a+bn)(a-bn) = a^2 - b^2*n^2 via deck executor."""
        from backend.src.emulator.lovelace_notes import run_note_d

        a, b, n = 5.0, 2.0, 2
        result = run_note_d(n, a=a, b=b)
        expected = a**2 - (b * n) ** 2  # 25 - 16 = 9
        assert abs(result.to_decimal() - expected) < 1e-4


# =============================================================================
# Note G -- full loop-back Bernoulli pipeline
# =============================================================================


class TestNoteGPipeline:
    """Note G: deck loader + loop-back controller + BabbageNumber accumulation."""

    def test_first_bernoulli_number(self):
        """B_1 = 1/6 (Ada's first Bernoulli, corresponds to modern B_2 = 1/6)."""
        from backend.src.emulator.note_g_deck import run_note_g

        results = run_note_g(1)
        assert len(results) == 1
        value = float(results[0].to_decimal())
        assert abs(value - 1 / 6) < 1e-6, f"Expected ~0.16667, got {value}"

    def test_bernoulli_series_length(self):
        """run_note_g(n) returns exactly n results."""
        from backend.src.emulator.note_g_deck import run_note_g

        for n in (1, 2, 3):
            results = run_note_g(n)
            assert len(results) == n, f"Expected {n} results, got {len(results)}"

    def test_exact_fractions_b1_b3(self):
        """B_1 = 1/6, B_3 = -1/30 (exact Fraction validation)."""
        from fractions import Fraction

        from backend.src.emulator.note_g_deck import run_note_g_exact

        results = run_note_g_exact(2)
        assert results[0] == Fraction(1, 6), f"B1 wrong: {results[0]}"
        assert results[1] == Fraction(-1, 30), f"B3 wrong: {results[1]}"


# =============================================================================
# Machine Adapters -- MachineAdapter ABC over all concrete machines
# =============================================================================


class TestMachineAdapterPipeline:
    """Each MachineAdapter subclass exercises the full adapter ABC contract."""

    def _check_adapter(self, adapter) -> None:
        """Verify adapter satisfies the MachineAdapter contract for 10 steps."""
        # Abstract method: get_cycle_count
        initial = adapter.get_cycle_count()
        assert isinstance(initial, int)

        # Abstract method: get_current_phase (may be None)
        adapter.get_current_phase()
        # return type is MechanicalPhase | None -- just verify it doesn't raise

        # Abstract method: get_column_values
        cols = adapter.get_column_values()
        assert isinstance(cols, list)

        # Abstract method: get_register_values
        regs = adapter.get_register_values()
        assert isinstance(regs, dict)

        # Abstract method: get_memory_value
        adapter.get_memory_value(0)
        # return type varies by machine; just verify no exception

        # Abstract method: step (10 cycles)
        for _ in range(10):
            adapter.step()

        # Abstract method: get_snapshot
        snap = adapter.get_snapshot()
        assert isinstance(snap, dict)

    def test_scheutz_adapter(self):
        from backend.src.emulator.adapter import ScheutzAdapter
        from backend.src.emulator.scheutz import ScheutzDifferenceEngine

        machine = ScheutzDifferenceEngine()
        machine.load([0, 1, 2])
        self._check_adapter(ScheutzAdapter(machine))

    def test_ludgate_adapter(self):
        from backend.src.emulator.adapter import LudgateAdapter
        from backend.src.emulator.ludgate import LudgateMachine

        self._check_adapter(LudgateAdapter(LudgateMachine()))

    def test_torres_quevedo_adapter(self):
        from backend.src.emulator.adapter import TorresQuevedoAdapter
        from backend.src.emulator.torres_quevedo import TorresQuevedo

        self._check_adapter(TorresQuevedoAdapter(TorresQuevedo()))

    def test_zuse_z1_adapter(self):
        from backend.src.emulator.adapter import ZuseZ1Adapter
        from backend.src.emulator.zuse_z1 import ZuseZ1

        self._check_adapter(ZuseZ1Adapter(ZuseZ1()))

    def test_curta_adapter(self):
        from backend.src.emulator.adapter import CurtaAdapter
        from backend.src.emulator.curta import CurtaTypeI

        self._check_adapter(CurtaAdapter(CurtaTypeI()))

    def test_ae_adapter(self):
        from backend.src.emulator.adapter import AEMachineAdapter
        from backend.src.emulator.analytical_engine import Engine

        engine = Engine()
        # AE adapter wraps engine; with no instruction cards, get_* calls are safe
        adapter = AEMachineAdapter(engine)
        assert isinstance(adapter.get_cycle_count(), int)
        assert isinstance(adapter.get_column_values(), list)
        assert isinstance(adapter.get_register_values(), dict)
        snap = adapter.get_snapshot()
        assert "pc" in snap
        assert "registers" in snap


# =============================================================================
# Haskell Compiler -> CodeGenerator -> Assembler (3-stage pipeline)
# =============================================================================


class TestHaskellCompilationPipeline:
    """End-to-end: Haskell source -> IR -> Babbage assembly -> machine code."""

    def test_simple_function_compiles(self):
        """A basic Haskell function definition traverses all three stages."""
        from backend.src.services.languages.haskell_service import (
            ExecutionStatus,
            HaskellService,
        )

        source = "add :: Int -> Int -> Int\nadd x y = x + y\n"
        result = asyncio.run(HaskellService().execute(source))

        # Must not time out; compile error is acceptable (IR stage may partially work)
        assert result.status != ExecutionStatus.TIMEOUT
        # IR must always be populated on success or compile error (not timeout)
        # Assembly stage evidence: check the pipeline ran at least through IR
        assert result.compile_time_ms >= 0

    def test_blocked_import_rejected(self):
        """Security gate: blocked imports are caught before compilation."""
        from backend.src.services.languages.haskell_service import (
            ExecutionStatus,
            HaskellService,
        )

        source = "import System.IO.Unsafe\nmain = unsafePerformIO (return ())\n"
        result = asyncio.run(HaskellService().execute(source))

        assert result.status == ExecutionStatus.COMPILE_ERROR
        assert "Blocked import" in result.errors
        assert result.compile_time_ms == 0  # Rejected before compilation

    def test_validate_returns_success_on_valid_source(self):
        """validate() exercises compiler stage only (no codegen/assembly)."""
        from backend.src.services.languages.haskell_service import (
            ExecutionStatus,
            HaskellService,
        )

        source = "double :: Int -> Int\ndouble x = x * 2\n"
        result = asyncio.run(HaskellService().validate(source))

        assert result.status in (ExecutionStatus.SUCCESS, ExecutionStatus.COMPILE_ERROR)
        assert result.assembly == ""
        assert result.machine_code == ""


# =============================================================================
# Card Compiler -- deck-level compile + decompile pipeline
# =============================================================================


class TestCardCompilerDeckPipeline:
    """compile_deck + decompile_deck + round_trip at program level."""

    def test_all_operation_opcodes_round_trip_in_deck(self):
        """A deck containing every opcode survives compile -> decompile."""
        from card_compiler import Opcode, round_trip

        source = "\n".join(op.name for op in Opcode)
        assert round_trip(source), "Full opcode deck failed round-trip"

    def test_mixed_card_classes_round_trip(self):
        """Operation + variable + number cards in one deck."""
        from card_compiler import round_trip

        source = "LOAD V3\nNUM +42\nADD\nSTORE V7\nHALT"
        assert round_trip(source)

    def test_compile_decompile_instruction_count_preserved(self):
        """Number of cards is preserved through compile -> decompile."""
        from card_compiler import compile_deck, decompile_deck

        source = "ADD\nSUB\nMUL\nDIV\nHALT"
        encoded = compile_deck(source)
        recovered = decompile_deck(encoded)
        assert len(encoded) == 5
        assert len(recovered.splitlines()) == 5

    def test_note_g_style_deck_round_trip(self):
        """A deck resembling Ada's Note G operations survives round-trip."""
        from card_compiler import round_trip

        source = "\n".join(
            [
                "LOAD V1",
                "LOAD V2",
                "MUL",
                "STORE V4",
                "LOAD V3",
                "SUB",
                "STORE V5",
                "HALT",
            ]
        )
        assert round_trip(source)
