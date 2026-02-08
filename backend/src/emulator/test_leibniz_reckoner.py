"""
Leibniz Reckoner Emulator Tests
"""

import pytest
from backend.src.emulator.leibniz_reckoner import LeibnizReckonerEmulator, CountingWheel

def test_accumulator_value_computation():
    reckoner = LeibnizReckonerEmulator(num_accumulator_digits=4)
    reckoner.accumulator_wheels[0].value = 3
    reckoner.accumulator_wheels[1].value = 2
    reckoner.accumulator_wheels[2].value = 1
    assert reckoner.get_accumulator_value() == 123

def test_leibniz_reset():
    reckoner = LeibnizReckonerEmulator(num_input_digits=3, num_accumulator_digits=6)
    reckoner.set_input(123)
    reckoner.crank_turn()
    assert reckoner.get_accumulator_value() == 123
    reckoner.reset()
    assert reckoner.get_accumulator_value() == 0
    assert reckoner.carriage_position == 0

def test_leibniz_set_input():
    reckoner = LeibnizReckonerEmulator(num_input_digits=3)
    reckoner.set_input(456)
    assert [d.value for d in reckoner.input_drums] == [6, 5, 4] # Stored in reverse order

def test_leibniz_add_single_digit():
    reckoner = LeibnizReckonerEmulator(num_input_digits=1, num_accumulator_digits=2)
    reckoner.set_input(1)
    reckoner.crank_turn()
    assert reckoner.get_accumulator_value() == 1
    reckoner.set_input(2)
    reckoner.crank_turn()
    assert reckoner.get_accumulator_value() == 3

def test_leibniz_add_with_carry():
    reckoner = LeibnizReckonerEmulator(num_input_digits=1, num_accumulator_digits=2)
    reckoner.set_input(9)
    reckoner.crank_turn() # 9
    reckoner.set_input(1)
    reckoner.crank_turn() # 9 + 1 = 10
    assert reckoner.get_accumulator_value() == 10

def test_leibniz_shift_carriage():
    reckoner = LeibnizReckonerEmulator(num_input_digits=1, num_accumulator_digits=2)
    reckoner.set_input(1)
    reckoner.shift_carriage(1) # Shift input to tens position
    reckoner.crank_turn() # Should add 10
    assert reckoner.get_accumulator_value() == 10

def test_leibniz_multiplication_simple_case():
    reckoner = LeibnizReckonerEmulator(num_input_digits=2, num_accumulator_digits=4)
    result = reckoner.multiply(19, 3) # 19 * 3 = 57
    assert result == 57

def test_leibniz_multiplication_with_shift():
    reckoner = LeibnizReckonerEmulator(num_input_digits=3, num_accumulator_digits=6)
    result = reckoner.multiply(123, 4) # 123 * 4 = 492
    assert result == 492

def test_leibniz_multiplication_large_numbers():
    reckoner = LeibnizReckonerEmulator(num_input_digits=5, num_accumulator_digits=10)
    result = reckoner.multiply(9999, 99) # 9999 * 99 = 989901
    assert result == 989901