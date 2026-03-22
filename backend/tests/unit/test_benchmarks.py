"""pytest-benchmark suite for key emulator hot paths.

Measures wall-clock performance of the most compute-intensive operations:
  BabbageNumber   -- multiply (core arithmetic in all physics modules)
  ThomasArithmometer -- multiply and divide (pinwheel mechanical loops)
  JacquardLoom    -- step() across a large card deck (tight inner loop)
  AE Engine       -- load_program_from_text + step_one_instruction (full pipeline)
  LispCompiler    -- compile a non-trivial AST (IR generation)

Run in isolation with:
  pytest tests/unit/test_benchmarks.py --benchmark-only -v

WHY these targets:
  - BabbageNumber.mul is called in every physics simulation step (tribology, thermal).
  - ThomasArithmometer multiply/divide are O(n_digits * n_crank_turns).
  - JacquardLoom.step drives the main emulator API loop for woven-output programs.
  - AE step_one_instruction exercises the full barrel + mill + carry pipeline.
  - LispCompiler.compile exercises the IR builder critical path.
"""

from __future__ import annotations

from typing import Any

import pytest

from backend.src.compilers.lisp_ast import Number, SExpression, Symbol
from backend.src.compilers.lisp_compiler import LispCompiler
from backend.src.emulator.analytical_engine import Engine
from backend.src.emulator.jacquard import JacquardLoom
from backend.src.emulator.thomas_arithmometer import ThomasArithmometer
from backend.src.emulator.types import BabbageNumber

# ---------------------------------------------------------------------------
# BabbageNumber -- core 50-digit fixed-point arithmetic
# ---------------------------------------------------------------------------


def test_bench_babbage_multiply(benchmark: Any) -> None:
    """Benchmark BabbageNumber multiply: 999 * 999."""
    a = BabbageNumber(999)
    b = BabbageNumber(999)
    result = benchmark(lambda: a * b)
    assert result.to_decimal() == pytest.approx(999 * 999, rel=1e-9)


def test_bench_babbage_add_chain(benchmark: Any) -> None:
    """Benchmark 100 successive BabbageNumber additions."""
    nums = [BabbageNumber(i) for i in range(1, 101)]

    def _run() -> BabbageNumber:
        acc = BabbageNumber(0)
        for n in nums:
            acc = acc + n
        return acc

    result = benchmark(_run)
    assert result.to_decimal() == pytest.approx(sum(range(1, 101)), rel=1e-9)


def test_bench_babbage_divide(benchmark: Any) -> None:
    """Benchmark BabbageNumber divide: 1000000 / 7."""
    a = BabbageNumber(1_000_000)
    b = BabbageNumber(7)
    result = benchmark(lambda: a / b)
    assert result.to_decimal() == pytest.approx(1_000_000 / 7, rel=1e-6)


# ---------------------------------------------------------------------------
# ThomasArithmometer -- pinwheel machine multiply / divide
# ---------------------------------------------------------------------------


def test_bench_thomas_multiply(benchmark: Any) -> None:
    """Benchmark ThomasArithmometer.multiply(9999, 9999)."""
    t = ThomasArithmometer()
    result = benchmark(lambda: t.multiply(9999, 9999))
    assert result == 9999 * 9999


def test_bench_thomas_divide(benchmark: Any) -> None:
    """Benchmark ThomasArithmometer.divide(99_990_001, 9999)."""
    t = ThomasArithmometer()
    result = benchmark(lambda: t.divide(99_990_001, 9999))
    q, r = result
    assert q == 10_000
    assert r == 1


# ---------------------------------------------------------------------------
# JacquardLoom -- step() through a 100-card deck
# ---------------------------------------------------------------------------


@pytest.fixture()
def jacquard_with_deck() -> JacquardLoom:
    """Return a JacquardLoom pre-loaded with 100 alternating cards."""
    loom = JacquardLoom(num_hooks=8)
    deck = [[(i + j) % 2 for j in range(8)] for i in range(100)]
    loom.load_deck(deck)
    return loom


def test_bench_jacquard_step_100(
    benchmark: Any,
    jacquard_with_deck: JacquardLoom,
) -> None:
    """Benchmark stepping through a 100-card Jacquard deck."""

    def _run() -> None:
        jacquard_with_deck.state.card_index = 0
        for _ in range(100):
            jacquard_with_deck.step()

    benchmark(_run)
    # All 100 cards were consumed; index sits at 100
    assert jacquard_with_deck.card_index == 100


# ---------------------------------------------------------------------------
# AE Engine -- load + step_one_instruction
# ---------------------------------------------------------------------------

_AE_PROGRAM = """\
LOAD A, 7
LOAD B, 3
ADD A, B
STOR [0], A
HALT
"""


def test_bench_ae_load_and_run(benchmark: Any) -> None:
    """Benchmark loading a 5-instruction AE program and stepping to HALT."""

    def _run() -> Engine:
        eng = Engine()
        eng.load_program_from_text(_AE_PROGRAM)
        while eng.running:
            eng.step_one_instruction()
        return eng

    eng = benchmark(_run)
    assert not eng.running


def test_bench_ae_step_only(benchmark: Any) -> None:
    """Benchmark 100 AE step_one_instruction calls (program already loaded)."""
    _loop_program = "\n".join(["LOAD A, 1"] + ["ADD A, 1"] * 98 + ["HALT"])
    eng = Engine()
    eng.load_program_from_text(_loop_program)

    def _run() -> None:
        eng.load_program_from_text(_loop_program)
        while eng.running:
            eng.step_one_instruction()

    benchmark(_run)
    assert not eng.running


# ---------------------------------------------------------------------------
# LispCompiler -- compile a non-trivial Lisp AST to Babbage IR
# ---------------------------------------------------------------------------


def test_bench_lisp_compile(benchmark: Any) -> None:
    """Benchmark LispCompiler.compile on a 10-deep nested addition AST."""
    # Build AST: (defun deep_add () (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10))))))))))
    # The compiler requires a defun wrapper to establish the IR builder context.
    inner: SExpression | Number = Number(10)
    for i in range(9, 0, -1):
        inner = SExpression([Symbol("+"), Number(i), inner])
    ast = SExpression([Symbol("defun"), Symbol("deep_add"), SExpression([]), inner])

    program = benchmark(lambda: LispCompiler().compile(ast))
    assert program is not None
    assert len(program.functions) >= 1
