"""Gate 1 ABI/runtime conformance tests for Babbage call behavior."""

from __future__ import annotations

import pytest

from backend.src.emulator.analytical_engine import Engine, Instruction


def _run_program(instructions: list[Instruction]) -> Engine:
    engine = Engine()
    engine.instruction_cards = instructions
    engine.run()
    return engine


def _assert_gate1_contract(engine: Engine, expected_return: float) -> None:
    # Gate 1 baseline:
    # - return value in A
    # - C and D are callee-saved
    # - control/data stacks are balanced after call return
    assert engine.registers["A"].to_decimal() == expected_return
    assert engine.registers["C"].to_decimal() == 99.0
    assert engine.registers["D"].to_decimal() == 77.0
    assert engine.return_stack == []
    assert engine.data_stack == []


@pytest.mark.integration
def test_gate1_abi_positive_callee_preserves_c_and_d() -> None:
    instructions = [
        Instruction("LOAD", ["A", "7"]),    # 0 caller setup
        Instruction("LOAD", ["C", "99"]),   # 1 caller setup
        Instruction("LOAD", ["D", "77"]),   # 2 caller setup
        Instruction("CALL", ["6"]),         # 3 call callee
        Instruction("JMP", ["14"]),         # 4 skip callee body post-return
        Instruction("NOP"),                 # 5 padding
        Instruction("PUSH", ["C"]),         # 6 callee prologue
        Instruction("PUSH", ["D"]),         # 7 callee prologue
        Instruction("LOAD", ["C", "123"]),  # 8 clobber C internally
        Instruction("LOAD", ["D", "456"]),  # 9 clobber D internally
        Instruction("ADD", ["A", "1"]),     # 10 produce return value in A
        Instruction("POP", ["D"]),          # 11 restore D
        Instruction("POP", ["C"]),          # 12 restore C
        Instruction("RET"),                 # 13 return
        Instruction("NOP"),                 # 14 end
    ]

    engine = _run_program(instructions)
    _assert_gate1_contract(engine, expected_return=8.0)


@pytest.mark.integration
def test_gate1_abi_detects_callee_saved_violation() -> None:
    instructions = [
        Instruction("LOAD", ["A", "7"]),
        Instruction("LOAD", ["C", "99"]),
        Instruction("LOAD", ["D", "77"]),
        Instruction("CALL", ["6"]),
        Instruction("JMP", ["10"]),
        Instruction("NOP"),
        Instruction("LOAD", ["C", "123"]),  # no save/restore for C: ABI violation
        Instruction("ADD", ["A", "1"]),
        Instruction("RET"),
        Instruction("NOP"),
        Instruction("NOP"),
    ]

    engine = _run_program(instructions)
    with pytest.raises(AssertionError):
        _assert_gate1_contract(engine, expected_return=8.0)


@pytest.mark.integration
def test_gate1_abi_detects_return_stack_underflow() -> None:
    engine = Engine()
    with pytest.raises(IndexError):
        engine.execute_instruction(Instruction("RET"))
