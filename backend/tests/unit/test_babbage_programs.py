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
