"""Unit tests for indirect-call IR lowering."""

from __future__ import annotations

from backend.src.codegen.regalloc import AllocationMap
from backend.src.codegen.selector import InstructionSelector
from backend.src.ir_types import (
    Assignment,
    BinaryOp,
    Call,
    IndirectCall,
    Load,
    VariableValue,
)


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


class TestAllocationMap:
    def test_get_allocation_register(self) -> None:
        am = AllocationMap(allocations={"x": "A"})
        assert am.get_allocation("x") == "A"

    def test_get_allocation_memory(self) -> None:
        am = AllocationMap(allocations={"y": "mem_200"})
        assert am.get_allocation("y") == "mem_200"

    def test_get_allocation_unknown_returns_question_mark(self) -> None:
        am = AllocationMap(allocations={})
        assert am.get_allocation("unknown") == "?"

    def test_is_spilled_false_for_register(self) -> None:
        am = AllocationMap(allocations={"x": "A"})
        assert am.is_spilled("x") is False

    def test_is_spilled_false_when_not_in_spilled_dict(self) -> None:
        # allocation says mem_200, but spilled dict is empty
        am = AllocationMap(allocations={"y": "mem_200"})
        assert am.is_spilled("y") is False

    def test_is_spilled_true_when_in_spilled_dict(self) -> None:
        am = AllocationMap(allocations={"z": "mem_300"}, spilled={"z": 300})
        assert am.is_spilled("z") is True

    def test_print_allocation_returns_string(self) -> None:
        am = AllocationMap(allocations={"a": "A", "b": "B"})
        out = am.print_allocation()
        assert isinstance(out, str)
        assert "a" in out
        assert "b" in out

    def test_spill_count_default_zero(self) -> None:
        am = AllocationMap()
        assert am.spill_count == 0

    def test_register_pressure_default_zero(self) -> None:
        am = AllocationMap()
        assert am.register_pressure == 0.0


class TestCallDispatch:
    def test_call_emits_call_and_mov(self) -> None:
        sel = InstructionSelector(AllocationMap(allocations={"fn": "A", "r": "B"}))
        c = Call(function_name="fn", arguments=[], target="r")
        asm = sel.select_instruction(c)
        assert [i.mnemonic for i in asm] == ["CALL", "MOV"]

    def test_call_result_mov_target_is_allocation(self) -> None:
        sel = InstructionSelector(AllocationMap(allocations={"fn": "A", "r": "C"}))
        asm = sel.select_instruction(Call(function_name="fn", arguments=[], target="r"))
        mov = asm[1]
        assert mov.operands[0].value == "C"


class TestAssignmentDispatch:
    def test_assignment_emits_mov(self) -> None:
        sel = InstructionSelector(AllocationMap(allocations={"src": "A", "dst": "B"}))
        asm = sel.select_instruction(Assignment(target="dst", source=VariableValue("src")))
        assert [i.mnemonic for i in asm] == ["MOV"]

    def test_assignment_mov_operands(self) -> None:
        sel = InstructionSelector(AllocationMap(allocations={"src": "A", "dst": "B"}))
        asm = sel.select_instruction(Assignment(target="dst", source=VariableValue("src")))
        assert asm[0].operands[0].value == "B"  # dst
        assert asm[0].operands[1].value == "A"  # src


class TestBinaryOpDispatch:
    def test_add_emits_add_and_mov(self) -> None:
        alloc = AllocationMap(allocations={"x": "A", "y": "B", "z": "C"})
        sel = InstructionSelector(alloc)
        bop = BinaryOp(
            op="add",
            target="z",
            operand1=VariableValue("x"),
            operand2=VariableValue("y"),
        )
        asm = sel.select_instruction(bop)
        assert [i.mnemonic for i in asm] == ["ADD", "MOV"]


class TestLoadDispatch:
    def test_load_emits_load(self) -> None:
        sel = InstructionSelector(AllocationMap(allocations={"dst": "A"}))
        asm = sel.select_instruction(Load(target="dst", address=VariableValue("addr")))
        assert asm[0].mnemonic == "LOAD"
