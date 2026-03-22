# Ancient Compute - LISP Compiler Tests

from __future__ import annotations

from typing import Any

from ..ir_types import (
    Assignment,
    BinaryOp,
    BranchTerminator,
    Call,
    Constant,
    Function,
    Program,
    ReturnTerminator,
    VariableValue,
)
from .lisp_ast import Number, SExpression, String, Symbol
from .lisp_compiler import LispCompiler
from .lisp_parser import parser

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _parse(code: str) -> Any:
    return parser.parse(code)


def _compile(code: str) -> Program:
    return LispCompiler().compile(_parse(code))


def _func(code: str, name: str) -> Function:
    return _compile(code).functions[name]


def _all_instrs(func: Function) -> list:
    return [i for b in func.basic_blocks for i in b.instructions]


# ---------------------------------------------------------------------------
# Original tests (kept verbatim)
# ---------------------------------------------------------------------------


def test_compile_defun() -> None:
    code = """(defun my-func (a b)
                 (+ a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-func" in program.functions

    func = program.functions["my-func"]
    assert isinstance(func, Function)
    assert func.parameters == ["a", "b"]

    # The function should return the result of the addition (VariableValue)
    entry_block = func.basic_blocks[0]
    assert isinstance(entry_block.terminator, ReturnTerminator)
    # The return value should be a VariableValue (the result of the addition), not a Constant(0)
    assert isinstance(entry_block.terminator.value, VariableValue)


def test_compile_arithmetic() -> None:
    code = """(defun my-add (a b)
                 (+ a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-add" in program.functions

    func = program.functions["my-add"]
    assert isinstance(func, Function)

    # Check that the function body contains a binary operation with 'add'
    entry_block = func.basic_blocks[0]
    assert any(
        isinstance(instr, BinaryOp) and instr.op == "add" for instr in entry_block.instructions
    )


def test_compile_nested_arithmetic() -> None:
    code = """(defun my-nested-add (a b c)
                 (+ a (+ b c)))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-nested-add" in program.functions

    func = program.functions["my-nested-add"]
    assert isinstance(func, Function)

    # Check that the function body contains two binary operations
    entry_block = func.basic_blocks[0]
    assert len([instr for instr in entry_block.instructions if isinstance(instr, BinaryOp)]) == 2


def test_compile_variables() -> None:
    code = """(defun my-vars (a b)
                 a)"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-vars" in program.functions

    func = program.functions["my-vars"]
    assert isinstance(func, Function)
    assert "a" in func.local_variables
    assert "b" in func.local_variables


def test_compile_if() -> None:
    code = """(defun my-if (a b)
                 (if (> a b) a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-if" in program.functions

    func = program.functions["my-if"]
    assert isinstance(func, Function)

    # Check that the function has the correct number of basic blocks
    assert len(func.basic_blocks) == 4  # entry, then, else, merge

    # Check that the entry block has a conditional branch
    entry_block = func.basic_blocks[0]
    assert isinstance(entry_block.terminator, BranchTerminator)


def test_compile_function_call() -> None:
    code = """(defun my-call (a b)
                 (my-add a b))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "my-call" in program.functions

    func = program.functions["my-call"]
    assert isinstance(func, Function)

    # Check that the function body contains a call instruction
    entry_block = func.basic_blocks[0]
    assert any(
        isinstance(instr, Call) and instr.function_name == "my-add"
        for instr in entry_block.instructions
    )


def test_compile_let() -> None:
    code = """(defun my-let ()
                 (let ((x 10) (y 20))
                    (+ x y)))"""
    ast = parser.parse(code)
    compiler = LispCompiler()
    program = compiler.compile(ast)

    func = program.functions["my-let"]

    # Should have local variables x and y
    assert "x" in func.local_variables
    assert "y" in func.local_variables

    # Should have assignments
    entry_block = func.basic_blocks[0]
    assignments = [instr for instr in entry_block.instructions if isinstance(instr, Assignment)]
    assert len(assignments) >= 2

    # Should end with add
    assert any(
        isinstance(instr, BinaryOp) and instr.op == "add" for instr in entry_block.instructions
    )


# ---------------------------------------------------------------------------
# TestLispCompilerBasics
# ---------------------------------------------------------------------------


class TestLispCompilerBasics:
    def test_compile_returns_program(self) -> None:
        assert isinstance(_compile("(defun f () 42)"), Program)

    def test_defun_name_in_functions(self) -> None:
        assert "f" in _compile("(defun f () 42)").functions

    def test_function_is_function_instance(self) -> None:
        assert isinstance(_func("(defun f () 42)", "f"), Function)

    def test_defun_no_params(self) -> None:
        func = _func("(defun f () 42)", "f")
        assert func.parameters == []

    def test_defun_params_preserved(self) -> None:
        func = _func("(defun add (x y) (+ x y))", "add")
        assert func.parameters == ["x", "y"]

    def test_multiple_functions(self) -> None:
        program = LispCompiler().compile(
            SExpression(
                [
                    Symbol("defun"),
                    Symbol("f"),
                    SExpression([]),
                    Number(1),
                ]
            )
        )
        assert "f" in program.functions

    def test_function_has_entry_block(self) -> None:
        func = _func("(defun f () 42)", "f")
        assert len(func.basic_blocks) >= 1

    def test_function_has_return_terminator(self) -> None:
        func = _func("(defun f () 42)", "f")
        assert isinstance(func.basic_blocks[0].terminator, ReturnTerminator)


# ---------------------------------------------------------------------------
# TestLispArithmetic
# ---------------------------------------------------------------------------


class TestLispArithmetic:
    def test_subtraction_emits_sub_op(self) -> None:
        func = _func("(defun f (a b) (- a b))", "f")
        instrs = _all_instrs(func)
        assert any(isinstance(i, BinaryOp) and i.op == "sub" for i in instrs)

    def test_multiplication_emits_mul_op(self) -> None:
        func = _func("(defun f (a b) (* a b))", "f")
        instrs = _all_instrs(func)
        assert any(isinstance(i, BinaryOp) and i.op == "mul" for i in instrs)

    def test_division_emits_div_op(self) -> None:
        func = _func("(defun f (a b) (/ a b))", "f")
        instrs = _all_instrs(func)
        assert any(isinstance(i, BinaryOp) and i.op == "div" for i in instrs)

    def test_addition_result_is_variable_value(self) -> None:
        func = _func("(defun f (a b) (+ a b))", "f")
        entry = func.basic_blocks[0]
        assert isinstance(entry.terminator, ReturnTerminator)
        assert isinstance(entry.terminator.value, VariableValue)

    def test_three_arg_addition_reduces(self) -> None:
        # (+ a b c) -> (+ (+ a b) c): 2 binary ops
        func = _func("(defun f (a b c) (+ a b c))", "f")
        entry = func.basic_blocks[0]
        binary_ops = [i for i in entry.instructions if isinstance(i, BinaryOp)]
        assert len(binary_ops) == 2

    def test_arithmetic_with_literal(self) -> None:
        func = _func("(defun f (a) (+ a 1))", "f")
        instrs = _all_instrs(func)
        assert any(isinstance(i, BinaryOp) and i.op == "add" for i in instrs)

    def test_arithmetic_operands_include_constant(self) -> None:
        func = _func("(defun f (a) (+ a 5))", "f")
        instrs = _all_instrs(func)
        binary_ops = [i for i in instrs if isinstance(i, BinaryOp)]
        assert len(binary_ops) == 1
        assert isinstance(binary_ops[0].operand2, Constant)
        assert binary_ops[0].operand2.value == 5.0


# ---------------------------------------------------------------------------
# TestLispIfExpr
# ---------------------------------------------------------------------------


class TestLispIfExpr:
    def test_if_has_four_blocks(self) -> None:
        func = _func("(defun f (x) (if x 1 0))", "f")
        assert len(func.basic_blocks) == 4

    def test_if_entry_has_branch_terminator(self) -> None:
        func = _func("(defun f (x) (if x 1 0))", "f")
        assert isinstance(func.basic_blocks[0].terminator, BranchTerminator)

    def test_if_returns_variable_value(self) -> None:
        func = _func("(defun f (x) (if x 1 0))", "f")
        merge_block = func.basic_blocks[3]
        assert isinstance(merge_block.terminator, ReturnTerminator)
        assert isinstance(merge_block.terminator.value, VariableValue)

    def test_if_without_else_compiles(self) -> None:
        # (if cond then) without else clause
        func = _func("(defun f (x) (if x 42))", "f")
        assert len(func.basic_blocks) == 4

    def test_if_condition_is_comparison(self) -> None:
        func = _func("(defun f (a b) (if (> a b) a b))", "f")
        entry = func.basic_blocks[0]
        assert isinstance(entry.terminator, BranchTerminator)
        assert entry.terminator.condition == "ne"


# ---------------------------------------------------------------------------
# TestLispCondExpr
# ---------------------------------------------------------------------------


class TestLispCondExpr:
    def test_cond_single_clause_with_t(self) -> None:
        func = _func("(defun f (x) (cond (t 42)))", "f")
        instrs = _all_instrs(func)
        # (t 42) is the unconditional else: just compiles 42
        from ..ir_types import Return

        ret = next((i for i in instrs if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 42.0

    def test_cond_two_clauses_emits_branch(self) -> None:
        func = _func("(defun f (x) (cond (x 1) (t 0)))", "f")
        terminators = [b.terminator for b in func.basic_blocks]
        assert any(isinstance(t, BranchTerminator) for t in terminators)

    def test_cond_otherwise_is_unconditional(self) -> None:
        func = _func("(defun f (x) (cond (otherwise 99)))", "f")
        instrs = _all_instrs(func)
        from ..ir_types import Return

        ret = next((i for i in instrs if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 99.0

    def test_cond_multiple_clauses_creates_blocks(self) -> None:
        func = _func("(defun f (x y) (cond (x 1) (y 2) (t 0)))", "f")
        assert len(func.basic_blocks) > 1


# ---------------------------------------------------------------------------
# TestLispFunctionCall
# ---------------------------------------------------------------------------


class TestLispFunctionCall:
    def test_function_call_emits_call_instr(self) -> None:
        func = _func("(defun f (a) (g a))", "f")
        entry = func.basic_blocks[0]
        assert any(isinstance(i, Call) for i in entry.instructions)

    def test_function_call_name_correct(self) -> None:
        func = _func("(defun f (a) (my-helper a))", "f")
        entry = func.basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert call.function_name == "my-helper"

    def test_function_call_args_passed(self) -> None:
        func = _func("(defun f (a b) (g a b))", "f")
        entry = func.basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert len(call.arguments) == 2

    def test_function_call_no_args(self) -> None:
        func = _func("(defun f () (g))", "f")
        entry = func.basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert call.arguments == []

    def test_function_call_returns_variable_value(self) -> None:
        func = _func("(defun f (a) (g a))", "f")
        entry = func.basic_blocks[0]
        assert isinstance(entry.terminator, ReturnTerminator)
        assert isinstance(entry.terminator.value, VariableValue)


# ---------------------------------------------------------------------------
# TestLispLetExpr
# ---------------------------------------------------------------------------


class TestLispLetExpr:
    def test_let_single_binding(self) -> None:
        func = _func("(defun f () (let ((x 5)) x))", "f")
        assert "x" in func.local_variables

    def test_let_assignment_emitted(self) -> None:
        func = _func("(defun f () (let ((x 10)) x))", "f")
        entry = func.basic_blocks[0]
        assert any(isinstance(i, Assignment) for i in entry.instructions)

    def test_let_binding_value_is_constant(self) -> None:
        func = _func("(defun f () (let ((x 7)) x))", "f")
        entry = func.basic_blocks[0]
        assignments = [i for i in entry.instructions if isinstance(i, Assignment)]
        assert len(assignments) >= 1
        assert any(isinstance(a.source, Constant) and a.source.value == 7.0 for a in assignments)


# ---------------------------------------------------------------------------
# TestLispStringAndAtom
# ---------------------------------------------------------------------------


class TestLispStringAndAtom:
    def test_string_literal_compiles_to_constant(self) -> None:
        # Compile a defun that returns a string literal
        ast = SExpression(
            [
                Symbol("defun"),
                Symbol("f"),
                SExpression([]),
                String("hello"),
            ]
        )
        program = LispCompiler().compile(ast)
        func = program.functions["f"]
        entry = func.basic_blocks[0]
        assert isinstance(entry.terminator, ReturnTerminator)
        assert isinstance(entry.terminator.value, Constant)
        assert entry.terminator.value.value >= 0.0

    def test_number_literal_compiles_to_constant(self) -> None:
        func = _func("(defun f () 42)", "f")
        entry = func.basic_blocks[0]
        assert isinstance(entry.terminator, ReturnTerminator)
        assert isinstance(entry.terminator.value, Constant)
        assert entry.terminator.value.value == 42.0

    def test_compile_top_level_atom_does_not_crash(self) -> None:
        # compile(atom) calls _compile_atom which is a no-op
        compiler = LispCompiler()
        program = compiler.compile(Number(5))
        assert isinstance(program, Program)

    def test_compile_symbol_atom_does_not_crash(self) -> None:
        compiler = LispCompiler()
        program = compiler.compile(Symbol("x"))
        assert isinstance(program, Program)
