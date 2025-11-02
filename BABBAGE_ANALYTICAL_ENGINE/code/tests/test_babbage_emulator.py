import pytest
from ancient_compute.BABBAGE_ANALYTICAL_ENGINE.babbage_emulator import (
    BabbageNumber,
    Engine,
    Instruction,
)


# Test BabbageNumber class
def test_babbage_number_init():
    num = BabbageNumber(123)
    assert num.to_decimal() == 123.0


def test_babbage_number_add():
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    result = num1 + num2
    assert result.to_decimal() == 30.0


def test_babbage_number_sub():
    num1 = BabbageNumber(30)
    num2 = BabbageNumber(10)
    result = num1 - num2
    assert result.to_decimal() == 20.0


def test_babbage_number_comparison():
    num1 = BabbageNumber(10)
    num2 = BabbageNumber(20)
    assert num1 < num2
    assert num2 > num1
    assert not (num2 < num1)
    assert not (num1 > num2)
    assert num1 == BabbageNumber(10)
    assert num1 != num2


# Test Engine class - basic instruction execution
def test_engine_add_immediate():
    engine = Engine()
    engine.registers["A"] = BabbageNumber(5)
    instruction = Instruction("ADD", ["A", "10"])
    engine.execute_instruction(instruction)
    assert engine.registers["A"].to_decimal() == 15.0
    assert engine.flags["ZERO"] == False
    assert engine.flags["SIGN"] == False


def test_engine_load_immediate():
    engine = Engine()
    instruction = Instruction("LOAD", ["B", "100"])
    engine.execute_instruction(instruction)
    assert engine.registers["B"].to_decimal() == 100.0


def test_engine_stor_to_memory():
    engine = Engine()
    engine.registers["C"] = BabbageNumber(42)
    instruction = Instruction("STOR", ["C", "[5]"])
    engine.execute_instruction(instruction)
    assert engine.memory[5].to_decimal() == 42.0


def test_engine_load_from_memory():
    engine = Engine()
    engine.memory[10] = BabbageNumber(99)
    instruction = Instruction("LOAD", ["A", "[10]"])
    engine.execute_instruction(instruction)
    assert engine.registers["A"].to_decimal() == 99.0


def test_engine_jmp():
    engine = Engine()
    engine.instruction_cards = [
        Instruction("NOP"),
        Instruction("JMP", ["5"]),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP
    engine.execute_instruction(engine.instruction_cards[1])  # JMP 5
    assert engine.PC == 5


def test_engine_jz_true():
    engine = Engine()
    engine.flags["ZERO"] = True
    engine.instruction_cards = [
        Instruction("NOP"),
        Instruction("JZ", ["5"]),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP
    engine.execute_instruction(engine.instruction_cards[1])  # JZ 5
    assert engine.PC == 5


def test_engine_jz_false():
    engine = Engine()
    engine.flags["ZERO"] = False
    engine.instruction_cards = [
        Instruction("NOP"),
        Instruction("JZ", ["5"]),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
        Instruction("NOP"),
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP
    engine.execute_instruction(engine.instruction_cards[1])  # JZ 5
    assert engine.PC == 2  # PC increments normally


def test_engine_call_ret():
    engine = Engine()
    engine.instruction_cards = [
        Instruction("NOP"),  # 0
        Instruction("CALL", ["4"]),  # 1
        Instruction("NOP"),  # 2 (should be skipped)
        Instruction("HALT"),  # 3 (should be skipped)
        Instruction("NOP"),  # 4 (target of CALL)
        Instruction("RET"),  # 5
    ]
    engine.PC = 0
    engine.execute_instruction(engine.instruction_cards[0])  # NOP
    assert engine.PC == 1
    engine.execute_instruction(engine.instruction_cards[1])  # CALL 4
    assert engine.PC == 4  # PC is 4 (target of CALL)
    assert engine.return_stack == [2]  # Return address should be 2
    engine.execute_instruction(engine.instruction_cards[4])  # NOP (at PC=4)
    assert engine.PC == 5
    engine.execute_instruction(engine.instruction_cards[5])  # RET
    assert engine.PC == 2  # Should return to PC=2
    assert engine.return_stack == []


def test_engine_push_pop():
    engine = Engine()
    engine.registers["A"] = BabbageNumber(100)
    engine.registers["B"] = BabbageNumber(200)
    instruction_push_a = Instruction("PUSH", ["A"])
    instruction_push_b = Instruction("PUSH", ["B"])
    instruction_pop_c = Instruction("POP", ["C"])
    instruction_pop_d = Instruction("POP", ["D"])

    engine.execute_instruction(instruction_push_a)
    assert engine.data_stack[-1].to_decimal() == 100.0
    engine.execute_instruction(instruction_push_b)
    assert engine.data_stack[-1].to_decimal() == 200.0

    engine.execute_instruction(instruction_pop_c)
    assert engine.registers["C"].to_decimal() == 200.0
    engine.execute_instruction(instruction_pop_d)
    assert engine.registers["D"].to_decimal() == 100.0
    assert len(engine.data_stack) == 0


def test_engine_rdcrd():
    engine = Engine()
    instruction = Instruction("RDCRD", ["A"])
    # Pass the expected value to _execute_RDCRD for testing
    engine._execute_RDCRD("A", value=123)  # Directly call the handler with the test value
    assert engine.registers["A"].to_decimal() == 123.0


def test_engine_wrpch():
    engine = Engine()
    engine.registers["A"] = BabbageNumber(789)
    instruction = Instruction("WRPCH", ["A"])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]["value"].to_decimal() == 789.0


def test_engine_wrprn():
    engine = Engine()
    engine.registers["B"] = BabbageNumber(12345)
    instruction = Instruction("WRPRN", ["B"])
    engine.execute_instruction(instruction)
    assert engine.result_cards[-1]["value"].to_decimal() == 12345.0


def test_engine_nop():
    engine = Engine()
    engine.PC = 5
    instruction = Instruction("NOP")
    engine.execute_instruction(instruction)
    assert engine.PC == 6  # PC should increment
