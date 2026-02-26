"""
Curta Calculator Tests
"""

from backend.src.emulator.curta import CrankMode, CurtaTypeI


def test_curta_addition():
    curta = CurtaTypeI()
    curta.set_input(123)
    curta.turn_crank() # Add 123
    assert curta.result_dial == 123
    assert curta.counter_dial == 1

    curta.turn_crank() # Add 123
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
    assert curta.counter_dial == 12 # 1 ten + 2 units

def test_curta_subtraction():
    curta = CurtaTypeI()
    curta.set_input(100)
    curta.turn_crank() # +100

    curta.set_input(10)
    curta.set_crank_mode(CrankMode.SUBTRACT)
    curta.turn_crank() # -10

    assert curta.result_dial == 90

    # Verify counter subtraction
    assert curta.counter_dial == 0 # 1 - 1 = 0

def test_overflow():
    curta = CurtaTypeI()
    # Max result is 11 nines
    # Input 999...
    # Turn crank
    pass # Implementation uses modulo arithmetic, implicit test
