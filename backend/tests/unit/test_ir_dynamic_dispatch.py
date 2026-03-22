"""Unit tests for indirect-call IR lowering, code emitter, and liveness analysis."""

from __future__ import annotations

from backend.src.codegen.emitter import AssemblyOutput, CodeEmitter
from backend.src.codegen.liveness import LiveInterval, LivenessAnalyzer
from backend.src.codegen.regalloc import AllocationMap
from backend.src.codegen.selector import AsmInstruction, AsmOperand, InstructionSelector
from backend.src.ir_types import (
    Assignment,
    BasicBlock,
    BinaryOp,
    Call,
    Function,
    IndirectCall,
    Load,
    ReturnTerminator,
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


# ---------------------------------------------------------------------------
# CodeEmitter
# ---------------------------------------------------------------------------


class TestCodeEmitter:
    def _instr(self, mnemonic: str, *operand_vals: str) -> AsmInstruction:
        return AsmInstruction(
            mnemonic=mnemonic,
            operands=[AsmOperand(operand_type="reg", value=v) for v in operand_vals],
        )

    def test_add_instruction_increments_address(self) -> None:
        emitter = CodeEmitter()
        assert emitter.current_address == 0
        emitter.add_instruction(self._instr("LOAD", "A", "5"))
        assert emitter.current_address == 1

    def test_add_label_maps_to_current_address(self) -> None:
        emitter = CodeEmitter()
        emitter.add_instruction(self._instr("NOP"))
        emitter.add_label("here")
        assert emitter.labels["here"] == 1

    def test_emit_returns_assembly_output(self) -> None:
        emitter = CodeEmitter()
        emitter.add_label("main")
        emitter.add_instruction(self._instr("HALT"))
        out = emitter.emit()
        assert isinstance(out, AssemblyOutput)

    def test_emit_instruction_count(self) -> None:
        emitter = CodeEmitter()
        for _ in range(4):
            emitter.add_instruction(self._instr("NOP"))
        out = emitter.emit()
        assert out.instruction_count == 4

    def test_emit_includes_global_main(self) -> None:
        emitter = CodeEmitter()
        emitter.add_instruction(self._instr("RET"))
        out = emitter.emit()
        assert ".global main" in out.assembly_text

    def test_emit_with_spill_count(self) -> None:
        emitter = CodeEmitter()
        emitter.add_instruction(self._instr("NOP"))
        out = emitter.emit(spill_count=3)
        assert out.spill_count == 3

    def test_label_in_label_map(self) -> None:
        emitter = CodeEmitter()
        emitter.add_label("start")
        emitter.add_instruction(self._instr("NOP"))
        out = emitter.emit()
        assert "start" in out.label_map

    def test_multiple_instructions_in_output(self) -> None:
        emitter = CodeEmitter()
        for op in ["LOAD", "ADD", "RET"]:
            emitter.add_instruction(self._instr(op, "A"))
        out = emitter.emit()
        assert "LOAD" in out.assembly_text
        assert "ADD" in out.assembly_text
        assert "RET" in out.assembly_text


# ---------------------------------------------------------------------------
# LiveInterval
# ---------------------------------------------------------------------------


class TestLiveInterval:
    def test_overlaps_with_self(self) -> None:
        iv = LiveInterval(name="x", start=0, end=5)
        assert iv.overlaps_with(iv) is True

    def test_non_overlapping_intervals(self) -> None:
        a = LiveInterval(name="a", start=0, end=2)
        b = LiveInterval(name="b", start=3, end=5)
        assert a.overlaps_with(b) is False

    def test_adjacent_intervals_do_not_overlap(self) -> None:
        a = LiveInterval(name="a", start=0, end=2)
        b = LiveInterval(name="b", start=3, end=5)
        assert b.overlaps_with(a) is False

    def test_overlapping_intervals(self) -> None:
        a = LiveInterval(name="a", start=0, end=4)
        b = LiveInterval(name="b", start=2, end=6)
        assert a.overlaps_with(b) is True

    def test_contained_interval_overlaps(self) -> None:
        outer = LiveInterval(name="outer", start=0, end=10)
        inner = LiveInterval(name="inner", start=3, end=7)
        assert outer.overlaps_with(inner) is True

    def test_repr_contains_name_and_range(self) -> None:
        iv = LiveInterval(name="foo", start=1, end=9)
        r = repr(iv)
        assert "foo" in r
        assert "1" in r
        assert "9" in r

    def test_definitions_is_set(self) -> None:
        iv = LiveInterval(name="x", start=0, end=5)
        assert isinstance(iv.definitions, set)

    def test_uses_is_set(self) -> None:
        iv = LiveInterval(name="x", start=0, end=5)
        assert isinstance(iv.uses, set)


# ---------------------------------------------------------------------------
# LivenessAnalyzer -- basic function analysis
# ---------------------------------------------------------------------------


def _make_simple_function() -> Function:
    """Build Function: x = 1 + 2; return x."""
    fn = Function(name="simple", parameters=[])
    bb = BasicBlock(label="entry")
    bb.add_instruction(Assignment(target="x", source=VariableValue("1")))
    bb.set_terminator(ReturnTerminator(value=VariableValue("x")))
    fn.add_block(bb)
    return fn


class TestLivenessAnalyzer:
    def test_analyze_returns_dict(self) -> None:
        fn = _make_simple_function()
        analyzer = LivenessAnalyzer(fn)
        result = analyzer.analyze()
        assert isinstance(result, dict)

    def test_defined_variable_has_interval(self) -> None:
        fn = _make_simple_function()
        analyzer = LivenessAnalyzer(fn)
        intervals = analyzer.analyze()
        assert "x" in intervals

    def test_interval_start_le_end(self) -> None:
        fn = _make_simple_function()
        analyzer = LivenessAnalyzer(fn)
        intervals = analyzer.analyze()
        for iv in intervals.values():
            assert iv.start <= iv.end

    def test_parameter_tracked(self) -> None:
        fn = Function(name="f", parameters=["a"])
        bb = BasicBlock(label="entry")
        bb.set_terminator(ReturnTerminator(value=VariableValue("a")))
        fn.add_block(bb)
        analyzer = LivenessAnalyzer(fn)
        intervals = analyzer.analyze()
        assert "a" in intervals


class TestAssemblyOutputExtra:
    """AssemblyOutput dataclass field tests."""

    def _make_ao(
        self,
        text: str = "",
        labels: dict | None = None,
        count: int = 0,
    ) -> AssemblyOutput:
        return AssemblyOutput(
            assembly_text=text, label_map=labels or {}, instruction_count=count
        )

    def test_assembly_text_stored(self) -> None:
        ao = self._make_ao(text="MOV A, B\nHALT")
        assert "MOV A, B" in ao.assembly_text

    def test_instruction_count_stored(self) -> None:
        ao = self._make_ao(count=7)
        assert ao.instruction_count == 7

    def test_label_map_stored(self) -> None:
        ao = self._make_ao(labels={"main": 0, "loop": 3})
        assert ao.label_map["main"] == 0
        assert ao.label_map["loop"] == 3

    def test_spill_count_default_zero(self) -> None:
        ao = self._make_ao()
        assert ao.spill_count == 0

    def test_comment_default_empty(self) -> None:
        ao = self._make_ao()
        assert ao.comment == ""

    def test_custom_comment(self) -> None:
        ao = AssemblyOutput(
            assembly_text="NOP", label_map={}, instruction_count=1, comment="test"
        )
        assert ao.comment == "test"


class TestCodeEmitterExtra:
    """CodeEmitter add/emit pipeline tests."""

    def _instr(self, mnemonic: str, *vals: str) -> AsmInstruction:
        return AsmInstruction(
            mnemonic=mnemonic,
            operands=[AsmOperand(operand_type="reg", value=v) for v in vals],
        )

    def test_add_two_instructions_count_two(self) -> None:
        ce = CodeEmitter()
        ce.add_instruction(self._instr("NOP"))
        ce.add_instruction(self._instr("NOP"))
        ao = ce.emit()
        assert ao.instruction_count == 2

    def test_emit_includes_instruction_text(self) -> None:
        ce = CodeEmitter()
        ce.add_instruction(self._instr("HALT"))
        ao = ce.emit()
        assert "HALT" in ao.assembly_text

    def test_emit_label_appears_in_label_map(self) -> None:
        ce = CodeEmitter()
        ce.add_label("entry")
        ce.add_instruction(self._instr("NOP"))
        ao = ce.emit()
        assert "entry" in ao.label_map

    def test_spill_count_propagated(self) -> None:
        ce = CodeEmitter()
        ce.add_instruction(self._instr("NOP"))
        ao = ce.emit(spill_count=5)
        assert ao.spill_count == 5

    def test_global_main_in_output(self) -> None:
        ce = CodeEmitter()
        ce.add_instruction(self._instr("RET"))
        ao = ce.emit()
        assert ".global main" in ao.assembly_text

    def test_multiple_labels_all_in_map(self) -> None:
        ce = CodeEmitter()
        ce.add_label("a")
        ce.add_instruction(self._instr("NOP"))
        ce.add_label("b")
        ce.add_instruction(self._instr("NOP"))
        ao = ce.emit()
        assert "a" in ao.label_map
        assert "b" in ao.label_map


class TestLivenessAnalyzerExtra:
    """Additional LivenessAnalyzer tests."""

    def test_analyze_returns_dict(self) -> None:
        fn = Function(name="f", parameters=["x"])
        bb = BasicBlock(label="entry")
        bb.set_terminator(ReturnTerminator(value=VariableValue("x")))
        fn.add_block(bb)
        intervals = LivenessAnalyzer(fn).analyze()
        assert isinstance(intervals, dict)

    def test_parameter_gets_interval(self) -> None:
        fn = Function(name="f", parameters=["a", "b"])
        bb = BasicBlock(label="entry")
        bb.set_terminator(ReturnTerminator(value=VariableValue("a")))
        fn.add_block(bb)
        intervals = LivenessAnalyzer(fn).analyze()
        assert "a" in intervals

    def test_interval_values_are_live_intervals(self) -> None:
        fn = Function(name="f", parameters=["x"])
        bb = BasicBlock(label="entry")
        bb.set_terminator(ReturnTerminator(value=VariableValue("x")))
        fn.add_block(bb)
        intervals = LivenessAnalyzer(fn).analyze()
        for iv in intervals.values():
            assert isinstance(iv, LiveInterval)
