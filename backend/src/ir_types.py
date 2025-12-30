"""
Babbage Intermediate Representation (IR) Data Structures

Defines the IR types used by all language compilers targeting Babbage.
See BABBAGE_IR_SPECIFICATION.md for detailed semantics.
"""

from __future__ import annotations
from dataclasses import dataclass, field
from typing import List, Dict, Optional, Union, Set
from enum import Enum


class IRType(str, Enum):
    """IR Data Types (all ultimately 50-digit decimal at runtime)"""
    I64 = "i64"              # 64-bit integer (compile-time only)
    F64 = "f64"              # 64-bit float (compile-time only)
    DEC50 = "dec50"          # 50-digit decimal (Babbage native)
    PTR = "ptr"              # Pointer (memory address 0-2047)
    VOID = "void"            # No value (function return, discards)


class Register(str, Enum):
    """Babbage Physical Registers"""
    A = "a"  # Accumulator / Primary arithmetic
    B = "b"  # Secondary arithmetic, loop counter
    C = "c"  # Counter/Address
    D = "d"  # Destination, temporary


@dataclass
class Value:
    """Represents an IR value (constant, register, memory, variable, etc.)"""
    pass


@dataclass
class Constant(Value):
    """Decimal constant value"""
    value: float
    ir_type: IRType = IRType.DEC50

    def __repr__(self) -> str:
        return f"{self.value}"


@dataclass
class RegisterValue(Value):
    """Physical register (A, B, C, D)"""
    register: Register

    def __repr__(self) -> str:
        return self.register.value.upper()


@dataclass
class MemoryValue(Value):
    """Direct memory access at fixed address"""
    address: int
    ir_type: IRType = IRType.DEC50

    def __repr__(self) -> str:
        return f"memory[{self.address}]"


@dataclass
class VariableValue(Value):
    """Local variable (resolved to register or stack during allocation)"""
    name: str
    ir_type: IRType = IRType.DEC50

    def __repr__(self) -> str:
        return self.name


@dataclass
class UndefValue(Value):
    """Undefined value (optimization barrier)"""
    ir_type: IRType = IRType.DEC50

    def __repr__(self) -> str:
        return "undef"


# Type alias for operands
Operand = Union[Constant, RegisterValue, MemoryValue, VariableValue, UndefValue]


@dataclass
class Instruction:
    """Base class for IR instructions"""
    pass


@dataclass
class Assignment(Instruction):
    """target = source"""
    target: str
    source: Operand


@dataclass
class BinaryOp(Instruction):
    """target = op operand1, operand2"""
    op: str  # "add", "sub", "mul", "div", "sqrt", "abs", "neg", "min", "max"
    target: str
    operand1: Operand
    operand2: Optional[Operand] = None  # None for unary ops (sqrt, abs, neg)


@dataclass
class Load(Instruction):
    """target = load address"""
    target: str
    address: Operand


@dataclass
class Store(Instruction):
    """store value, address"""
    value: Operand
    address: Operand


@dataclass
class LoadEffectiveAddress(Instruction):
    """target = lea variable"""
    target: str
    variable: str


@dataclass
class Jump(Instruction):
    """jump label"""
    label: str


@dataclass
class ConditionalBranch(Instruction):
    """branch_condition operand1, operand2, true_label, false_label"""
    condition: str  # "eq", "ne", "lt", "gt", "le", "ge", "zero", "nonzero"
    operand1: Operand
    operand2: Optional[Operand] = None  # None for zero/nonzero conditions
    true_label: Optional[str] = None
    false_label: Optional[str] = None


@dataclass
class Call(Instruction):
    """target = call function_name, arguments"""
    function_name: str
    arguments: List[Operand] = field(default_factory=list)
    target: Optional[str] = None  # None if return value discarded


@dataclass
class Return(Instruction):
    """return value"""
    value: Optional[Operand] = None


@dataclass
class Terminator:
    """Base class for block terminators"""
    pass


@dataclass
class ReturnTerminator(Terminator):
    """return value"""
    value: Optional[Operand] = None


@dataclass
class JumpTerminator(Terminator):
    """jump label"""
    label: str


@dataclass
class BranchTerminator(Terminator):
    """branch condition true_label false_label"""
    condition: str  # "eq", "ne", "lt", "gt", "le", "ge", "zero", "nonzero"
    operand1: Operand
    operand2: Optional[Operand] = None
    true_label: str = ""
    false_label: str = ""


@dataclass
class CallTerminator(Terminator):
    """tail call (return from called function)"""
    function_name: str
    arguments: List[Operand] = field(default_factory=list)


@dataclass
class BasicBlock:
    """Basic block: sequence of instructions ending with terminator"""
    label: str
    instructions: List[Instruction] = field(default_factory=list)
    terminator: Optional[Terminator] = None
    
    def add_instruction(self, instr: Instruction) -> None:
        """Add instruction to block"""
        self.instructions.append(instr)
    
    def set_terminator(self, term: Terminator) -> None:
        """Set block terminator"""
        self.terminator = term


@dataclass
class Function:
    """IR Function: sequence of basic blocks with parameters"""
    name: str
    parameters: List[str] = field(default_factory=list)
    basic_blocks: List[BasicBlock] = field(default_factory=list)
    local_variables: Dict[str, IRType] = field(default_factory=dict)
    return_type: IRType = IRType.DEC50
    
    def add_block(self, block: BasicBlock) -> None:
        """Add basic block to function"""
        self.basic_blocks.append(block)
    
    def get_block(self, label: str) -> Optional[BasicBlock]:
        """Get basic block by label"""
        for block in self.basic_blocks:
            if block.label == label:
                return block
        return None


@dataclass
class GlobalVariable:
    """Global variable declaration"""
    name: str
    initial_value: Optional[float] = None
    size: Optional[int] = None  # For arrays
    ir_type: IRType = IRType.DEC50


@dataclass
class GlobalFunction:
    """Function declaration (for forward references)"""
    name: str
    parameter_types: List[IRType] = field(default_factory=list)
    return_type: IRType = IRType.DEC50


@dataclass
class Program:
    """Complete IR program"""
    global_variables: Dict[str, GlobalVariable] = field(default_factory=dict)
    global_functions: Dict[str, GlobalFunction] = field(default_factory=dict)
    functions: Dict[str, Function] = field(default_factory=dict)
    
    def add_global_variable(self, var: GlobalVariable) -> None:
        """Add global variable"""
        self.global_variables[var.name] = var
    
    def add_global_function(self, func: GlobalFunction) -> None:
        """Add function declaration"""
        self.global_functions[func.name] = func
    
    def add_function(self, func: Function) -> None:
        """Add function definition"""
        self.functions[func.name] = func
    
    def get_function(self, name: str) -> Optional[Function]:
        """Get function by name"""
        return self.functions.get(name)


# IR Builder API for constructing IR from compilers
class IRBuilder:
    """Helper for constructing IR programs"""
    
    def __init__(self, function_name: str, parameters: List[str] = None):
        if parameters is None:
            parameters = []
        self.function = Function(function_name, parameters)
        self.current_block: Optional[BasicBlock] = None
    
    def new_block(self, label: str) -> BasicBlock:
        """Create and add new basic block"""
        block = BasicBlock(label)
        self.function.add_block(block)
        self.current_block = block
        return block
    
    def emit_assignment(self, target: str, source: Operand) -> None:
        """Emit: target = source"""
        self.current_block.add_instruction(Assignment(target, source))
    
    def emit_binary_op(self, op: str, target: str, op1: Operand, op2: Operand) -> None:
        """Emit: target = op op1, op2"""
        self.current_block.add_instruction(BinaryOp(op, target, op1, op2))
    
    def emit_load(self, target: str, address: Operand) -> None:
        """Emit: target = load address"""
        self.current_block.add_instruction(Load(target, address))
    
    def emit_store(self, value: Operand, address: Operand) -> None:
        """Emit: store value, address"""
        self.current_block.add_instruction(Store(value, address))
    
    def emit_call(self, function_name: str, arguments: List[Operand], 
                  target: Optional[str] = None) -> None:
        """Emit: target = call function_name, arguments"""
        self.current_block.add_instruction(Call(function_name, arguments, target))
    
    def emit_jump(self, label: str) -> None:
        """Emit: jump label (terminator)"""
        self.current_block.set_terminator(JumpTerminator(label))
    
    def emit_branch(self, condition: str, op1: Operand, op2: Optional[Operand],
                    true_label: str, false_label: str) -> None:
        """Emit: branch condition op1, op2, true_label, false_label (terminator)"""
        self.current_block.set_terminator(
            BranchTerminator(condition, op1, op2, true_label, false_label)
        )
    
    def emit_return(self, value: Optional[Operand] = None) -> None:
        """Emit: return value (terminator)"""
        self.current_block.set_terminator(ReturnTerminator(value))
    
    def finalize(self) -> Function:
        """Get completed function"""
        return self.function
