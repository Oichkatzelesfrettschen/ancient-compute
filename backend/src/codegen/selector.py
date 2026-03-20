"""
Instruction Selection for Babbage

Maps IR instructions to Babbage ISA mnemonics and operands.

Key responsibility:
- IR BinaryOp("add", ...) → Babbage ADD mnemonic
- Handle register allocation (map IR values to physical registers)
- Generate intermediate moves if needed (e.g., for non-commutative ops)
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any

from backend.src.codegen.regalloc import AllocationMap
from backend.src.ir_types import (
    Assignment,
    BinaryOp,
    BranchTerminator,
    Call,
    Constant,
    IndirectCall,
    Instruction,
    JumpTerminator,
    Load,
    MemoryValue,
    Operand,
    RegisterValue,
    Return,
    ReturnTerminator,
    Store,
    VariableValue,
)


@dataclass
class AsmOperand:
    """Operand in assembly (register, immediate, address, label)"""

    operand_type: str  # "reg", "immed", "addr", "label"
    value: str  # "A", "B", "C", "D", "42", "100", "loop_start", etc.


@dataclass
class AsmInstruction:
    """Assembly instruction"""

    mnemonic: str
    operands: list[AsmOperand]
    comment: str = ""

    def to_asm_string(self) -> str:
        """Convert to assembly syntax"""
        if not self.operands:
            return self.mnemonic

        ops_str = ", ".join(op.value for op in self.operands)

        if self.comment:
            return f"{self.mnemonic:10} {ops_str:20} # {self.comment}"
        else:
            return f"{self.mnemonic:10} {ops_str}"


class InstructionSelector:
    """
    Selects Babbage ISA instructions based on IR and register allocation.

    Algorithm:
    1. For each IR instruction, determine corresponding Babbage mnemonics
    2. Use allocation map to translate IR values to physical registers/memory
    3. Generate intermediate moves if needed
    4. Handle special cases (division, function calls, etc.)
    """

    def __init__(self, allocation: AllocationMap):
        self.allocation = allocation
        self.asm_instructions: list[AsmInstruction] = []

    def select_instruction(self, instr: Instruction) -> list[AsmInstruction]:
        """Select Babbage instructions for single IR instruction"""
        result: list[AsmInstruction] = []

        if isinstance(instr, Assignment):
            result = self._select_assignment(instr)
        elif isinstance(instr, BinaryOp):
            result = self._select_binary_op(instr)
        elif isinstance(instr, Load):
            result = self._select_load(instr)
        elif isinstance(instr, Store):
            result = self._select_store(instr)
        elif isinstance(instr, Call):
            result = self._select_call(instr)
        elif isinstance(instr, IndirectCall):
            result = self._select_indirect_call(instr)
        elif isinstance(instr, Return):
            result = self._select_return(instr)

        return result

    def _get_operand(self, value: Operand) -> AsmOperand:
        """Convert IR value to assembly operand"""
        if isinstance(value, VariableValue):
            reg_or_mem = self.allocation.get_allocation(value.name)
            if reg_or_mem.startswith("mem_"):
                # Spilled to memory
                return AsmOperand("addr", reg_or_mem[4:])
            else:
                # Register
                return AsmOperand("reg", reg_or_mem)

        elif isinstance(value, RegisterValue):
            # Physical register
            return AsmOperand("reg", value.register.value.upper())

        elif isinstance(value, Constant):
            # Immediate constant
            return AsmOperand("immed", str(int(value.value)))

        elif isinstance(value, MemoryValue):
            # Direct memory address
            return AsmOperand("addr", str(value.address))

        else:
            raise ValueError(f"Unknown operand type: {type(value)}")

    def _select_assignment(self, instr: Assignment) -> list[AsmInstruction]:
        """Select for: target = source"""
        target_reg = self.allocation.get_allocation(instr.target)
        source_op = self._get_operand(instr.source)

        # MOV target, source
        return [
            AsmInstruction(
                "MOV",
                [AsmOperand("reg", target_reg), source_op],
                comment=f"{instr.target} = {instr.source}",
            )
        ]

    def _select_binary_op(self, instr: BinaryOp) -> list[AsmInstruction]:
        """Select for: target = op operand1, operand2"""
        result: list[AsmInstruction] = []

        target_reg = self.allocation.get_allocation(instr.target)
        op1 = self._get_operand(instr.operand1)

        mnemonic_map = {
            "add": "ADD",
            "sub": "SUB",
            "mul": "MULT",
            "div": "DIV",
            "sqrt": "SQRT",
            "abs": "ABS",
            "neg": "NEG",
            "min": "MIN",
            "max": "MAX",
            # Comparison ops: emit SUB so result sign encodes relationship.
            # For branch contexts these are bypassed by _emit_condition_branch
            # in c_compiler.py; this fallback handles standalone boolean values.
            "eq": "SUB",
            "ne": "SUB",
            "lt": "SUB",
            "le": "SUB",
            "gt": "SUB",
            "ge": "SUB",
            # Symbol aliases emitted by Python compiler (arithmetic + comparisons).
            "+": "ADD",
            "-": "SUB",
            "*": "MULT",
            "/": "DIV",
            "//": "DIV",
            "%": "SUB",  # Modulo: closest Babbage fallback is SUB
            "==": "SUB",
            "!=": "SUB",
            "<": "SUB",
            "<=": "SUB",
            ">": "SUB",
            ">=": "SUB",
        }

        mnemonic = mnemonic_map.get(instr.op, instr.op.upper())

        if instr.op in ["sqrt", "abs", "neg"]:
            # Unary operations: only one operand
            # Ensure source in A (Babbage convention)
            if op1.operand_type == "reg" and op1.value != "A":
                result.append(
                    AsmInstruction(
                        "MOV", [AsmOperand("reg", "A"), op1], comment="move to A for unary op"
                    )
                )

            result.append(
                AsmInstruction(
                    mnemonic,
                    [AsmOperand("reg", "A")],
                    comment=f"{instr.target} = {instr.op}({instr.operand1})",
                )
            )

            # Move result back to target if needed
            if target_reg != "A":
                result.append(
                    AsmInstruction(
                        "MOV",
                        [AsmOperand("reg", target_reg), AsmOperand("reg", "A")],
                        comment="move result to target",
                    )
                )

        else:
            # Binary operations: need two operands
            assert instr.operand2 is not None
            op2 = self._get_operand(instr.operand2)

            # Babbage convention: operations are A ← A op B
            # So we need op1 in A, op2 in B

            if op1.operand_type == "reg" and op1.value != "A":
                result.append(
                    AsmInstruction("MOV", [AsmOperand("reg", "A"), op1], comment="move op1 to A")
                )

            if op2.operand_type == "reg" and op2.value != "B":
                result.append(
                    AsmInstruction("MOV", [AsmOperand("reg", "B"), op2], comment="move op2 to B")
                )

            result.append(
                AsmInstruction(
                    mnemonic,
                    [AsmOperand("reg", "A"), AsmOperand("reg", "B")],
                    comment=f"{instr.target} = {instr.operand1} {instr.op} {instr.operand2}",
                )
            )

            # Move result back to target if needed
            if target_reg != "A":
                result.append(
                    AsmInstruction(
                        "MOV",
                        [AsmOperand("reg", target_reg), AsmOperand("reg", "A")],
                        comment="move result to target",
                    )
                )

        return result

    def _select_load(self, instr: Load) -> list[AsmInstruction]:
        """Select for: target = load address"""
        target_reg = self.allocation.get_allocation(instr.target)
        addr_op = self._get_operand(instr.address)

        # LOAD target, address
        return [
            AsmInstruction(
                "LOAD",
                [AsmOperand("reg", target_reg), addr_op],
                comment=f"load {instr.target} from {instr.address}",
            )
        ]

    def _select_store(self, instr: Store) -> list[AsmInstruction]:
        """Select for: store value, address"""
        value_op = self._get_operand(instr.value)
        addr_op = self._get_operand(instr.address)

        # STOR value, address
        return [
            AsmInstruction(
                "STOR", [value_op, addr_op], comment=f"store {instr.value} to {instr.address}"
            )
        ]

    def _select_call(self, instr: Call) -> list[AsmInstruction]:
        """Select for: target = call func(args).

        Babbage ABI calling convention:
          - First 4 arguments passed in registers A, B, C, D (left-to-right).
          - Arguments 5+ are PUSHed right-to-left before the CALL instruction.
          - Callee returns result in register A.
          - Caller pops extra stack arguments after CALL (if any).

        WHY: The AE Mill has 4 ingress axes (A/B/C/D).  Using registers for the
        first 4 args directly maps to the Mill's physical ingress paths.  Extra
        args use the data stack (Babbage's variable-card back-transfer path).
        """
        result: list[AsmInstruction] = []

        _ARG_REGS = ["A", "B", "C", "D"]

        # Arguments 5+ are pushed right-to-left (stack-based overflow).
        overflow_args = instr.arguments[len(_ARG_REGS) :]
        for arg in reversed(overflow_args):
            arg_op = self._get_operand(arg)
            result.append(AsmInstruction("PUSH", [arg_op], comment="push overflow arg"))

        # First 4 args: move into A/B/C/D.
        for reg_name, arg in zip(_ARG_REGS, instr.arguments, strict=False):
            arg_op = self._get_operand(arg)
            if not (arg_op.operand_type == "reg" and arg_op.value == reg_name):
                result.append(
                    AsmInstruction(
                        "MOV",
                        [AsmOperand("reg", reg_name), arg_op],
                        comment=f"arg -> {reg_name}",
                    )
                )

        # CALL function_label
        result.append(
            AsmInstruction(
                "CALL",
                [AsmOperand("label", instr.function_name)],
                comment=f"call {instr.function_name}",
            )
        )

        # Pop stack-overflow args (caller cleanup).
        for _ in overflow_args:
            result.append(
                AsmInstruction("POP", [AsmOperand("reg", "D")], comment="discard overflow arg")
            )

        # Return value in A (Babbage convention).
        if instr.target:
            target_reg = self.allocation.get_allocation(instr.target)
            if target_reg != "A":
                result.append(
                    AsmInstruction(
                        "MOV",
                        [AsmOperand("reg", target_reg), AsmOperand("reg", "A")],
                        comment="move return value to target",
                    )
                )

        return result

    def _select_indirect_call(self, instr: IndirectCall) -> list[AsmInstruction]:
        """Select for: target = call function_pointer(args)"""
        result: list[AsmInstruction] = []
        ptr_op = self._get_operand(instr.function_pointer)

        # If pointer is spilled to memory, load into C and call via register.
        if ptr_op.operand_type == "addr":
            result.append(
                AsmInstruction(
                    "LOAD",
                    [AsmOperand("reg", "C"), ptr_op],
                    comment="load function pointer for indirect call",
                )
            )
            call_operand = AsmOperand("reg", "C")
        elif ptr_op.operand_type == "reg" or ptr_op.operand_type == "immed":
            call_operand = ptr_op
        else:
            call_operand = AsmOperand("reg", "C")
            result.append(
                AsmInstruction(
                    "MOV",
                    [AsmOperand("reg", "C"), ptr_op],
                    comment="normalize function pointer for indirect call",
                )
            )

        result.append(
            AsmInstruction(
                "CALL",
                [call_operand],
                comment="indirect call",
            )
        )

        # Return value is in A by convention.
        if instr.target:
            target_reg = self.allocation.get_allocation(instr.target)
            if target_reg != "A":
                result.append(
                    AsmInstruction(
                        "MOV",
                        [AsmOperand("reg", target_reg), AsmOperand("reg", "A")],
                        comment="move indirect call result to target",
                    )
                )

        return result

    def _select_return(self, instr: Return) -> list[AsmInstruction]:
        """Select for: return value"""
        result: list[AsmInstruction] = []

        if instr.value:
            value_op = self._get_operand(instr.value)
            if value_op.operand_type == "immed":
                # Constants must be loaded into A first
                result.append(
                    AsmInstruction(
                        "LOAD",
                        [AsmOperand("reg", "A"), value_op],
                        comment="load return value into A",
                    )
                )
            elif value_op.operand_type == "reg" and value_op.value != "A":
                result.append(
                    AsmInstruction(
                        "MOV", [AsmOperand("reg", "A"), value_op], comment="move return value to A"
                    )
                )

        result.append(AsmInstruction("RET", [], comment="return"))

        return result

    def select_terminator(self, term, label_map: dict[str, Any]) -> list[AsmInstruction]:  # type: ignore[no-untyped-def]
        """Select for block terminator"""
        result: list[AsmInstruction] = []

        if isinstance(term, JumpTerminator):
            result.append(
                AsmInstruction(
                    "JMP", [AsmOperand("label", term.label)], comment=f"jump to {term.label}"
                )
            )

        elif isinstance(term, BranchTerminator):
            # Compare operands first
            op1 = self._get_operand(term.operand1)

            # CMP op1, op2 (if binary condition)
            if term.operand2:
                op2 = self._get_operand(term.operand2)
                result.append(
                    AsmInstruction(
                        "CMP", [op1, op2], comment=f"compare {term.operand1} vs {term.operand2}"
                    )
                )

            # Conditional jump based on condition (symbol aliases for Python compiler).
            condition_map = {
                "eq": "JZ",
                "==": "JZ",
                "ne": "JNZ",
                "!=": "JNZ",
                "lt": "JLT",
                "<": "JLT",
                "gt": "JGT",
                ">": "JGT",
                "le": "JLE",
                "<=": "JLE",
                "ge": "JGE",
                ">=": "JGE",
                "zero": "JZ",
                "nonzero": "JNZ",
            }

            jump_mnemonic = condition_map.get(term.condition, "JMP")

            result.append(
                AsmInstruction(
                    jump_mnemonic,
                    [AsmOperand("label", term.true_label)],
                    comment=f"jump to {term.true_label} if {term.condition}",
                )
            )

            result.append(
                AsmInstruction(
                    "JMP",
                    [AsmOperand("label", term.false_label)],
                    comment=f"jump to {term.false_label}",
                )
            )

        elif isinstance(term, ReturnTerminator):
            # emit_return() now adds a Return instruction to block.instructions,
            # which handles the actual RET emission.  ReturnTerminator is kept
            # as a control-flow marker only; nothing to emit here.
            pass

        return result


def example_instruction_selection() -> None:
    """Example: Instruction selection"""
    from ir_types import Constant, IRBuilder, VariableValue

    # Build simple IR
    builder = IRBuilder("example", [])
    block = builder.new_block("entry")
    builder.emit_assignment("a", Constant(10.0))
    builder.emit_binary_op("add", "b", VariableValue("a"), Constant(5.0))
    builder.emit_return(VariableValue("b"))
    _func = builder.finalize()

    # Simulate allocation (a→A, b→B)
    from regalloc import AllocationMap

    allocation = AllocationMap(
        allocations={"a": "A", "b": "B"},
        spilled={},
        spill_count=0,
    )

    # Select instructions
    selector = InstructionSelector(allocation)
    for instr in block.instructions:
        selected = selector.select_instruction(instr)
        for asm_instr in selected:
            print(asm_instr.to_asm_string())

    # Select terminator
    selected_term = selector.select_terminator(block.terminator, {})
    for asm_instr in selected_term:
        print(asm_instr.to_asm_string())

    print("\nInstruction selection test PASSED")


if __name__ == "__main__":
    example_instruction_selection()
