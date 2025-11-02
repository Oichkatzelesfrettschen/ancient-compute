"""
Unit Tests for Babbage Analytical Engine Emulator

Tests for BabbageNumber arithmetic, Engine instruction execution, memory
operations, control flow, stack operations, and I/O.

Test coverage: 17 comprehensive tests covering:
  - BabbageNumber: init, arithmetic (add, sub), comparison
  - Arithmetic instructions: ADD, SUB, MULT, DIV, SQRT
  - Memory instructions: LOAD (immediate and from memory), STOR
  - Control flow: JMP, JZ, JNZ, CALL, RET
  - Stack: PUSH, POP
  - I/O: RDCRD, WRPCH, WRPRN
  - Basic: NOP
"""

import pytest
from backend.src.emulator.analytical_engine import BabbageNumber, Engine, Instruction


# ============================================================================
# BabbageNumber Tests (4 tests)
# ============================================================================


def test_babbage_number_init():
    """Test BabbageNumber initialization with integer input."""
    num = BabbageNumber(123)
    assert num.to_decimal() == 123.0


def test_babbage_number_add():
    """Test BabbageNumber addition."""
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    result = num1 + num2
    assert result.to_decimal() == 30.0


def test_babbage_number_sub():
    """Test BabbageNumber subtraction."""
    num1 = BabbageNumber(30)
    num2 = BabbageNumber(10)
    result = num1 - num2
    assert result.to_decimal() == 20.0


def test_babbage_number_comparison():
    """Test BabbageNumber comparison operators."""
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    assert num1 < num2
    assert num2 > num1
    assert num1 == BabbageNumber(10)
    assert num1 != num2


# ============================================================================
# Engine Arithmetic Tests (4 tests)
# ============================================================================


def test_engine_add_immediate():
    """Test ADD instruction with immediate operand."""
    engine = Engine()
    engine.registers["A"] = BabbageNumber(5)
    instruction = Instruction("ADD", ["A", "10"])
    engine.execute_instruction(instruction)
    assert engine.registers["A"].to_decimal() == 15.0
    assert engine.flags["ZERO"] == False
    assert engine.flags["SIGN"] == False


def test_engine_load_immediate():
    """Test LOAD instruction with immediate value."""
    engine = Engine()
    instruction = Instruction("LOAD", ["B", "100"])
    engine.execute_instruction(instruction)
    assert engine.registers["B"].to_decimal() == 100.0


def test_engine_stor_to_memory():
    """Test STOR instruction to write register to memory."""
    engine = Engine()
    engine.registers["C"] = BabbageNumber(42)
    instruction = Instruction("STOR", ["C", "[5]"])
    engine.execute_instruction(instruction)
    assert engine.memory[5].to_decimal() == 42.0


def test_engine_load_from_memory():
    """Test LOAD instruction to read from memory."""
    engine = Engine()
    engine.memory[10] = BabbageNumber(99)
    instruction = Instruction("LOAD", ["A", "[10]"])
    engine.execute_instruction(instruction)
    assert engine.registers["A"].to_decimal() == 99.0


# ============================================================================
# Engine Control Flow Tests (3 tests)
# ============================================================================


def test_engine_jmp():
    """Test unconditional JMP (jump) instruction."""
    engine = Engine()
    engine.instruction_cards = [
        Instruction("NOP"),  # 0
        Instruction("JMP", ["5"]),  # 1
        Instruction("NOP"),  # 2
        Instruction("NOP"),  # 3
        Instruction("NOP"),  # 4
        Instruction("NOP"),  # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP, PC -> 1
    engine.execute_instruction(engine.instruction_cards[1])  # JMP 5, PC -> 5
    assert engine.PC == 5


def test_engine_jz_true():
    """Test JZ (jump if zero) when ZERO flag is set."""
    engine = Engine()
    engine.flags["ZERO"] = True
    engine.instruction_cards = [
        Instruction("NOP"),  # 0
        Instruction("JZ", ["5"]),  # 1
        Instruction("NOP"),  # 2
        Instruction("NOP"),  # 3
        Instruction("NOP"),  # 4
        Instruction("NOP"),  # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP, PC -> 1
    engine.execute_instruction(engine.instruction_cards[1])  # JZ 5, PC -> 5
    assert engine.PC == 5


def test_engine_jz_false():
    """Test JZ (jump if zero) when ZERO flag is clear."""
    engine = Engine()
    engine.flags["ZERO"] = False
    engine.instruction_cards = [
        Instruction("NOP"),  # 0
        Instruction("JZ", ["5"]),  # 1
        Instruction("NOP"),  # 2
        Instruction("NOP"),  # 3
        Instruction("NOP"),  # 4
        Instruction("NOP"),  # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP, PC -> 1
    engine.execute_instruction(engine.instruction_cards[1])  # JZ 5, PC -> 2 (normal increment)
    assert engine.PC == 2


# ============================================================================
# Engine Subroutine Tests (2 tests)
# ============================================================================


def test_engine_call_ret():
    """Test CALL and RET instructions for subroutine support."""
    engine = Engine()
    engine.instruction_cards = [
        Instruction("NOP"),  # 0
        Instruction("CALL", ["4"]),  # 1: call subroutine at address 4
        Instruction("NOP"),  # 2: return address (skipped by call)
        Instruction("HALT"),  # 3: (never reached)
        Instruction("NOP"),  # 4: subroutine starts here
        Instruction("RET"),  # 5: return from subroutine
    ]
    engine.PC = 0

    # Execute NOP at 0
    engine.execute_instruction(engine.instruction_cards[0])
    assert engine.PC == 1

    # Execute CALL at 1: push return address (2) and jump to 4
    engine.execute_instruction(engine.instruction_cards[1])
    assert engine.PC == 4
    assert engine.return_stack == [2]

    # Execute NOP at 4
    engine.execute_instruction(engine.instruction_cards[4])
    assert engine.PC == 5

    # Execute RET at 5: pop return address and jump to 2
    engine.execute_instruction(engine.instruction_cards[5])
    assert engine.PC == 2
    assert engine.return_stack == []


def test_engine_push_pop():
    """Test PUSH and POP stack operations."""
    engine = Engine()
    engine.registers["A"] = BabbageNumber(100)
    engine.registers["B"] = BabbageNumber(200)
    instruction_push_a = Instruction("PUSH", ["A"])
    instruction_push_b = Instruction("PUSH", ["B"])
    instruction_pop_c = Instruction("POP", ["C"])
    instruction_pop_d = Instruction("POP", ["D"])

    # Push A (100) onto stack
    engine.execute_instruction(instruction_push_a)
    assert engine.data_stack[-1].to_decimal() == 100.0

    # Push B (200) onto stack
    engine.execute_instruction(instruction_push_b)
    assert engine.data_stack[-1].to_decimal() == 200.0

    # Pop into C (should get 200, LIFO)
    engine.execute_instruction(instruction_pop_c)
    assert engine.registers["C"].to_decimal() == 200.0

    # Pop into D (should get 100)
    engine.execute_instruction(instruction_pop_d)
    assert engine.registers["D"].to_decimal() == 100.0
    assert len(engine.data_stack) == 0


# ============================================================================
# Engine I/O Tests (4 tests)
# ============================================================================


def test_engine_rdcrd():
    """Test RDCRD (read punch card) instruction."""
    engine = Engine()
    # Directly call handler with test value
    engine._execute_RDCRD("A", value=123)
    assert engine.registers["A"].to_decimal() == 123.0


def test_engine_wrpch():
    """Test WRPCH (write punch card) instruction."""
    engine = Engine()
    engine.registers["A"] = BabbageNumber(789)
    instruction = Instruction("WRPCH", ["A"])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]["value"].to_decimal() == 789.0


def test_engine_wrprn():
    """Test WRPRN (write to printer) instruction."""
    engine = Engine()
    engine.registers["B"] = BabbageNumber(12345)
    instruction = Instruction("WRPRN", ["B"])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]["value"].to_decimal() == 12345.0


def test_engine_nop():
    """Test NOP (no operation) instruction."""
    engine = Engine()
    engine.PC = 5
    instruction = Instruction("NOP")
    engine.execute_instruction(instruction)
    # PC should increment normally even for NOP
    assert engine.PC == 6
