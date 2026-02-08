"""
Tests for the Babbage Analytical Engine Emulator.
"""

import pytest
from backend.src.emulator.analytical_engine import Engine, BabbageNumber, Instruction
from backend.src.emulator.barrels import MicroOp

@pytest.fixture
def ae_engine():
    """Fixture for a fresh Analytical Engine instance."""
    return Engine()

def test_babbage_number_arithmetic():
    num1 = BabbageNumber(10.5)
    num2 = BabbageNumber(2.5)

    assert (num1 + num2).to_decimal() == 13.0
    assert (num1 - num2).to_decimal() == 8.0
    assert (num1 * num2).to_decimal() == 26.25 # 10.5 * 2.5
    assert (num1 / num2).to_decimal() == 4.2   # 10.5 / 2.5

def test_babbage_number_overflow():
    large_num = BabbageNumber(10**50 - 1)
    overflow_num = BabbageNumber(10**50)
    
    assert large_num._check_overflow() == False
    assert overflow_num._check_overflow() == True # Should set overflow flag

def test_engine_init(ae_engine):
    assert ae_engine.PC == 0
    assert ae_engine.registers['A'].to_decimal() == 0.0
    assert ae_engine.flags['ZERO'] == False

def test_engine_load_and_store(ae_engine):
    ae_engine.registers['A'] = BabbageNumber(123.45)
    ae_engine._execute_STOR('A', '[0]')
    assert ae_engine.memory[0].to_decimal() == 123.45

    ae_engine.registers['B'] = BabbageNumber(0)
    ae_engine._execute_LOAD('B', '[0]')
    assert ae_engine.registers['B'].to_decimal() == 123.45

def test_engine_add_micro(ae_engine):
    # Setup initial state
    ae_engine.registers['A'] = BabbageNumber(100)
    ae_engine.memory[0] = BabbageNumber(20) # Store operand in memory
    
    # Execute ADD (A = A + memory[0])
    ae_engine._execute_ADD_micro('A', '[0]')
    
    # Assert result
    assert ae_engine.registers['A'].to_decimal() == 120.0
    assert ae_engine.flags['ZERO'] == False
    assert ae_engine.flags['OVERFLOW'] == False

def test_engine_sub_micro(ae_engine):
    # Setup initial state
    ae_engine.registers['A'] = BabbageNumber(100)
    ae_engine.memory[0] = BabbageNumber(20) # Store operand in memory

    # Execute SUB (A = A - memory[0])
    ae_engine._execute_SUB_micro('A', '[0]')

    # Assert result
    assert ae_engine.registers['A'].to_decimal() == 80.0
    assert ae_engine.flags['ZERO'] == False
    assert ae_engine.flags['OVERFLOW'] == False

def test_engine_mult_micro_simplified(ae_engine):
    # Setup initial state
    ae_engine.registers['A'] = BabbageNumber(10)
    ae_engine.memory[0] = BabbageNumber(5) # Store operand in memory
    
    # Execute MULT (A = A * memory[0])
    ae_engine._execute_MULT_micro('A', '[0]') # Now testing the micro-programmed MULT
    
    # Check if 'A' contains the result, and 'D' might contain lower parts if multi-register.
    assert ae_engine.mill_product_accumulator.to_decimal() == 50.0
    assert ae_engine.registers['A'].to_decimal() == 50.0
    assert ae_engine.flags['ZERO'] == False

def test_engine_div_micro_simplified(ae_engine):
    # Setup initial state
    ae_engine.registers['A'] = BabbageNumber(100) # Dividend
    ae_engine.memory[0] = BabbageNumber(20)  # Divisor

    # Execute DIV (A = A / memory[0])
    ae_engine._execute_DIV_micro('A', '[0]')

    # Assert result
    assert ae_engine.mill_quotient_buffer.to_decimal() == 5.0
    assert ae_engine.registers['A'].to_decimal() == 5.0
    assert ae_engine.flags['ZERO'] == False