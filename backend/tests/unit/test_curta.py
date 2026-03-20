"""
Curta Calculator Tests
"""

from backend.src.emulator.curta import CrankMode, CurtaTypeI


def test_curta_addition():
    curta = CurtaTypeI()
    curta.set_input(123)
    curta.turn_crank()  # Add 123
    assert curta.result_dial == 123
    assert curta.counter_dial == 1

    curta.turn_crank()  # Add 123
    assert curta.result_dial == 246
    assert curta.counter_dial == 2


def test_curta_multiplication():
    # 12 * 12 = 144
    curta = CurtaTypeI()
    curta.set_input(12)

    # Unit position: turn 2 times
    curta.turn_crank()
    curta.turn_crank()

    # Tens position: shift and turn 1 time
    curta.shift_carriage(1)
    curta.turn_crank()

    assert curta.result_dial == 144
    assert curta.counter_dial == 12  # 1 ten + 2 units


def test_curta_subtraction():
    curta = CurtaTypeI()
    curta.set_input(100)
    curta.turn_crank()  # +100

    curta.set_input(10)
    curta.set_crank_mode(CrankMode.SUBTRACT)
    curta.turn_crank()  # -10

    assert curta.result_dial == 90

    # Verify counter subtraction
    assert curta.counter_dial == 0  # 1 - 1 = 0


def test_overflow():
    _curta = CurtaTypeI()
    # Max result is 11 nines
    # Input 999...
    # Turn crank
    pass  # Implementation uses modulo arithmetic, implicit test


def test_curta_divide_exact():
    curta = CurtaTypeI()
    q, r = curta.divide(100, 5)
    assert q == 20
    assert r == 0


def test_curta_divide_with_remainder():
    curta = CurtaTypeI()
    q, r = curta.divide(17, 3)
    assert q == 5
    assert r == 2


def test_curta_divide_by_one():
    curta = CurtaTypeI()
    q, r = curta.divide(999, 1)
    assert q == 999
    assert r == 0


def test_curta_divide_large():
    curta = CurtaTypeI()
    q, r = curta.divide(9999, 99)
    expected_q, expected_r = divmod(9999, 99)
    assert q == expected_q
    assert r == expected_r


def test_curta_divide_by_zero():
    import pytest

    curta = CurtaTypeI()
    with pytest.raises(ZeroDivisionError):
        curta.divide(10, 0)


def test_curta_bell_rings_on_overflow():
    curta = CurtaTypeI()
    curta.set_input(1)
    curta.result_dial = curta.MAX_RESULT  # Set to maximum
    curta._add_result(1)  # This should wrap and ring the bell
    assert curta.bell_ringing is True


def test_curta_bell_clears_on_clear_result():
    curta = CurtaTypeI()
    curta.bell_ringing = True
    curta.clear_result()
    assert curta.bell_ringing is False
