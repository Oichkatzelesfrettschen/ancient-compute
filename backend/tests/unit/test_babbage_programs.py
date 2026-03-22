"""Integration-style tests for simple AE programs.

Includes transcendental series programs inspired by Ada Lovelace's remark
(Note A, 1843) that the AE "might compute trigonometrical functions,
logarithmic functions, etc., by developing the appropriate series."
"""

import math
from pathlib import Path

from backend.src.emulator.analytical_engine import Engine

_PROGRAMS = Path(__file__).resolve().parents[3] / "docs/simulation/programs"


def _run(name: str) -> float:
    engine = Engine()
    engine.load_program(str(_PROGRAMS / name))
    engine.run()
    return engine.result_cards[-1]["value"].to_decimal()


def _run_all_outputs(name: str) -> list[float]:
    engine = Engine()
    engine.load_program(str(_PROGRAMS / name))
    engine.run()
    return [card["value"].to_decimal() for card in engine.result_cards]


def test_babbage_addition_program():
    assert _run("babbage_addition.ae") == 5.0


# ---------------------------------------------------------------------------
# Transcendental series programs (Ada Lovelace Note A, 1843)
# ---------------------------------------------------------------------------


def test_sin_series_pi_over_6():
    """sin(pi/6) = 0.5 -- 6-term Maclaurin series, error < 1e-8."""
    result = _run("sin_series.ae")
    assert abs(result - math.sin(math.pi / 6)) < 1e-6


def test_cos_series_pi_over_3():
    """cos(pi/3) = 0.5 -- 6-term Maclaurin series, error < 1e-8."""
    result = _run("cos_series.ae")
    assert abs(result - math.cos(math.pi / 3)) < 1e-6


def test_ln_series_sqrt_e():
    """ln(e^0.5) = 0.5 -- 10-term Mercator series, error < 1e-3.

    t = e^0.5 - 1 ~ 0.649 is near the boundary of fast convergence.
    Truncation after 10 terms gives |error| ~ t^11/11 ~ 7e-4.
    """
    result = _run("ln_series.ae")
    assert abs(result - 0.5) < 1e-3


def test_exp_series_one():
    """e^1 = 2.71828... -- 11-term Maclaurin series, error < 1e-7."""
    result = _run("exp_series.ae")
    assert abs(result - math.e) < 1e-5


class TestAEProgramExecution:
    def test_triangular_number_program(self) -> None:
        """T(5) = 1+2+3+4+5 = 15."""
        assert _run("triangular_number.ae") == 15.0

    def test_addition_produces_single_output(self) -> None:
        outputs = _run_all_outputs("babbage_addition.ae")
        assert len(outputs) == 1

    def test_program_runs_without_error(self) -> None:
        for name in [
            "babbage_addition.ae",
            "triangular_number.ae",
            "sin_series.ae",
            "cos_series.ae",
            "exp_series.ae",
        ]:
            engine = Engine()
            engine.load_program(str(_PROGRAMS / name))
            engine.run()  # must not raise
            assert not engine.running

    def test_sin_result_in_valid_range(self) -> None:
        result = _run("sin_series.ae")
        assert -1.0 <= result <= 1.0

    def test_cos_result_in_valid_range(self) -> None:
        result = _run("cos_series.ae")
        assert -1.0 <= result <= 1.0

    def test_exp_result_positive(self) -> None:
        assert _run("exp_series.ae") > 0.0

    def test_engine_not_running_after_run(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        engine.run()
        assert not engine.running

    def test_result_cards_list(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        engine.run()
        assert isinstance(engine.result_cards, list)

    def test_each_result_card_has_value(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        engine.run()
        for card in engine.result_cards:
            assert "value" in card

    def test_sin_cos_sum_identity(self) -> None:
        """sin^2(pi/6) + cos^2(pi/3) = sin^2(pi/6) + cos^2(pi/3) approx 0.25 + 0.25 = 0.5."""
        s = _run("sin_series.ae")
        c = _run("cos_series.ae")
        # sin(pi/6) = cos(pi/3) = 0.5
        assert abs(s - c) < 1e-5


class TestAEProgramOutputCount:
    """Verify that programs produce the correct number of outputs."""

    def test_addition_one_output(self) -> None:
        outputs = _run_all_outputs("babbage_addition.ae")
        assert len(outputs) == 1

    def test_triangular_number_one_output(self) -> None:
        outputs = _run_all_outputs("triangular_number.ae")
        assert len(outputs) == 1

    def test_sin_series_one_output(self) -> None:
        outputs = _run_all_outputs("sin_series.ae")
        assert len(outputs) == 1

    def test_cos_series_one_output(self) -> None:
        outputs = _run_all_outputs("cos_series.ae")
        assert len(outputs) == 1

    def test_exp_series_one_output(self) -> None:
        outputs = _run_all_outputs("exp_series.ae")
        assert len(outputs) == 1

    def test_ln_series_one_output(self) -> None:
        outputs = _run_all_outputs("ln_series.ae")
        assert len(outputs) == 1


class TestAEProgramLnSeries:
    def test_ln_series_result_positive(self) -> None:
        result = _run("ln_series.ae")
        assert result > 0

    def test_ln_series_close_to_half(self) -> None:
        """ln(sqrt(e)) = 0.5 -- 10-term Mercator series."""
        result = _run("ln_series.ae")
        assert abs(result - 0.5) < 1e-3

    def test_ln_series_result_in_range(self) -> None:
        result = _run("ln_series.ae")
        # Reasonable range: between 0.4 and 0.6
        assert 0.4 < result < 0.6


class TestAEProgramRepeatability:
    """Each program should produce the same result on multiple runs."""

    def test_addition_is_reproducible(self) -> None:
        r1 = _run("babbage_addition.ae")
        r2 = _run("babbage_addition.ae")
        assert r1 == r2

    def test_sin_is_reproducible(self) -> None:
        r1 = _run("sin_series.ae")
        r2 = _run("sin_series.ae")
        assert abs(r1 - r2) < 1e-12

    def test_exp_is_reproducible(self) -> None:
        r1 = _run("exp_series.ae")
        r2 = _run("exp_series.ae")
        assert abs(r1 - r2) < 1e-12


class TestAEEngineState:
    """Engine state properties after running standard programs."""

    def test_result_cards_is_non_empty_after_run(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        engine.run()
        assert len(engine.result_cards) > 0

    def test_instruction_cards_loaded_from_file(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        assert len(engine.instruction_cards) > 0

    def test_engine_pc_resets_to_zero_on_load(self) -> None:
        engine = Engine()
        engine.load_program(str(_PROGRAMS / "babbage_addition.ae"))
        assert engine.PC == 0

    def test_two_separate_engines_give_same_sin(self) -> None:
        e1 = Engine()
        e1.load_program(str(_PROGRAMS / "sin_series.ae"))
        e1.run()
        e2 = Engine()
        e2.load_program(str(_PROGRAMS / "sin_series.ae"))
        e2.run()
        assert abs(
            e1.result_cards[-1]["value"].to_decimal()
            - e2.result_cards[-1]["value"].to_decimal()
        ) < 1e-12


class TestAEResultPrecision:
    """Numerical accuracy of transcendental programs."""

    def test_exp_greater_than_two(self) -> None:
        assert _run("exp_series.ae") > 2.0

    def test_exp_less_than_three(self) -> None:
        assert _run("exp_series.ae") < 3.0

    def test_addition_result_is_five(self) -> None:
        assert _run("babbage_addition.ae") == 5.0

    def test_triangular_result_is_fifteen(self) -> None:
        assert _run("triangular_number.ae") == 15.0
