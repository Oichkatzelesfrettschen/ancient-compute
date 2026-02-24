"""Unit tests for indirect-call IR lowering."""

from __future__ import annotations

from backend.src.codegen.regalloc import AllocationMap
from backend.src.codegen.selector import InstructionSelector
from backend.src.ir_types import IndirectCall, VariableValue


def test_indirect_call_with_register_pointer_emits_call_and_result_move() -> None:
    allocation = AllocationMap(allocations={"fn_ptr": "C", "ret_val": "B"})
    selector = InstructionSelector(allocation)

    instr = IndirectCall(function_pointer=VariableValue("fn_ptr"), target="ret_val")
    asm = selector.select_instruction(instr)

    assert [item.mnemonic for item in asm] == ["CALL", "MOV"]
    assert asm[0].operands[0].value == "C"
    assert asm[1].operands[0].value == "B"
    assert asm[1].operands[1].value == "A"


def test_indirect_call_with_spilled_pointer_loads_before_call() -> None:
    allocation = AllocationMap(allocations={"fn_ptr": "mem_600", "ret_val": "A"})
    selector = InstructionSelector(allocation)

    instr = IndirectCall(function_pointer=VariableValue("fn_ptr"), target="ret_val")
    asm = selector.select_instruction(instr)

    assert [item.mnemonic for item in asm] == ["LOAD", "CALL"]
    assert asm[0].operands[0].value == "C"
    assert asm[0].operands[1].value == "600"
    assert asm[1].operands[0].value == "C"
