"""Tests for the Torres y Quevedo electromechanical calculator emulator."""

import math
import pytest

from backend.src.emulator.torres_quevedo import (
    FloatingPointNumber,
    TorresQuevedo,
)


def _close(a: float, b: float, tol: float = 1e-5) -> bool:
    if abs(b) < 1e-12:
        return abs(a) < tol
    return abs(a - b) / abs(b) < tol


class TestFloatingPointNumber:
    def test_zero(self):
        fp = FloatingPointNumber()
        assert fp.to_float() == 0.0

    def test_from_float_positive(self):
        fp = FloatingPointNumber.from_float(3.14)
        assert _close(fp.to_float(), 3.14)

    def test_from_float_negative(self):
        fp = FloatingPointNumber.from_float(-2.5)
        assert fp.negative is True
        assert _close(abs(fp.to_float()), 2.5)

    def test_from_float_one(self):
        fp = FloatingPointNumber.from_float(1.0)
        assert _close(fp.to_float(), 1.0)

    def test_from_float_large(self):
        fp = FloatingPointNumber.from_float(12345678.0)
        assert _close(fp.to_float(), 12345678.0, tol=1e-3)

    def test_from_float_small(self):
        fp = FloatingPointNumber.from_float(0.001)
        assert _close(fp.to_float(), 0.001)

    def test_normalization_range(self):
        fp = FloatingPointNumber.from_float(5.0)
        # Mantissa should be in [0.1, 1.0)
        assert 0.0 <= fp.mantissa < 1.0

    def test_repr(self):
        fp = FloatingPointNumber.from_float(1.0)
        assert "TQ(" in repr(fp)

    def test_equality(self):
        fp1 = FloatingPointNumber.from_float(3.14)
        fp2 = FloatingPointNumber.from_float(3.14)
        assert fp1 == fp2

    def test_invalid_mantissa_raises(self):
        with pytest.raises(ValueError):
            FloatingPointNumber(mantissa=1.5)


class TestTorresRegisterOps:
    def test_load_and_read(self):
        t = TorresQuevedo()
        fp = FloatingPointNumber.from_float(2.5)
        t.load_register(0, fp)
        result = t.read_register(0)
        assert _close(result.to_float(), 2.5)

    def test_register_out_of_range(self):
        t = TorresQuevedo()
        with pytest.raises(IndexError):
            t.load_register(8, FloatingPointNumber.from_float(1.0))

    def test_reset_clears_registers(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(99.0))
        t.reset()
        assert t.read_register(0).to_float() == 0.0


class TestTorresArithmetic:
    def setup_method(self):
        self.t = TorresQuevedo()
        self.t.load_register(0, FloatingPointNumber.from_float(6.0))
        self.t.load_register(1, FloatingPointNumber.from_float(4.0))

    def test_add(self):
        result = self.t.add(0, 1, dest=2)
        assert _close(result.to_float(), 10.0)

    def test_subtract(self):
        result = self.t.subtract(0, 1, dest=2)
        assert _close(result.to_float(), 2.0)

    def test_multiply(self):
        result = self.t.multiply(0, 1, dest=2)
        assert _close(result.to_float(), 24.0)

    def test_divide(self):
        result = self.t.divide(0, 1, dest=2)
        assert _close(result.to_float(), 1.5)

    def test_divide_by_zero(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber())  # zero
        with pytest.raises(ZeroDivisionError):
            t.divide(0, 1, dest=2)

    def test_chain_operations(self):
        # (6 + 4) * 2 = 20
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(6.0))
        t.load_register(1, FloatingPointNumber.from_float(4.0))
        t.load_register(2, FloatingPointNumber.from_float(2.0))
        t.add(0, 1, dest=3)      # R3 = 10
        result = t.multiply(3, 2, dest=4)  # R4 = 20
        assert _close(result.to_float(), 20.0)


class TestTorresTypewriter:
    def test_typewriter_print(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(42.0))
        output = t.typewriter_print(0)
        assert "TQ(" in output
        assert len(t.state.typewriter_output) == 1

    def test_state_dict(self):
        t = TorresQuevedo()
        d = t.state_dict()
        assert "registers" in d
        assert "cycle_count" in d

    def test_cycle_count_increments(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(1.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        initial = t.state.cycle_count
        t.add(0, 1, dest=2)
        assert t.state.cycle_count == initial + 1

    def test_run_program(self):
        t = TorresQuevedo()
        t.load_register(0, FloatingPointNumber.from_float(3.0))
        t.load_register(1, FloatingPointNumber.from_float(2.0))
        results = t.run([
            ("add", 0, 1, 2),
            ("mul", 0, 1, 3),
        ])
        assert _close(results[0].to_float(), 5.0)
        assert _close(results[1].to_float(), 6.0)
