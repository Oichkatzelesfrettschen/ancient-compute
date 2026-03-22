"""
System F Compiler Tests
"""

from __future__ import annotations

from backend.src.compilers.systemf_compiler import SystemFCompiler
from backend.src.ir_types import (
    Assignment,
    BranchTerminator,
    Call,
    Constant,
    Program,
    Return,
    VariableValue,
)

# ---------------------------------------------------------------------------
# Original tests (kept verbatim)
# ---------------------------------------------------------------------------


def test_compile_identity() -> None:
    # id = /\a => \x : a => x
    source = r"/\a => \x : a => x"
    compiler = SystemFCompiler()
    program = compiler.compile(source)

    assert "main" in program.functions
    assert len(program.functions) >= 2  # main + lambda_1

    main_func = program.functions["main"]
    entry = main_func.basic_blocks[0]
    # Lambda is lifted; main returns a Constant (hash of lifted fn name)
    assert isinstance(entry.terminator.value, Constant)  # type: ignore[union-attr]
    # Lambda function name appears in the program functions dict
    lambda_names = [name for name in program.functions if "lambda" in name]
    assert len(lambda_names) >= 1


def test_compile_type_application() -> None:
    source = r"(/\a => \x : a => x) [Int] 42"
    compiler = SystemFCompiler()
    program = compiler.compile(source)

    main_func = program.functions["main"]
    entry = main_func.basic_blocks[0]
    assert any(isinstance(instr, Call) for instr in entry.instructions)


def test_compile_let() -> None:
    # Note: 'x' is identifier, but my lexer says single lowercase is TYPE_VAR
    # let val : Int = 10 in val
    source = r"let val : Int = 10 in val"
    compiler = SystemFCompiler()
    program = compiler.compile(source)

    main_func = program.functions["main"]
    assert "val" in main_func.local_variables


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _compile(source: str) -> Program:
    return SystemFCompiler().compile(source)


def _all_instrs(program: Program) -> list:
    func = program.functions["main"]
    return [i for b in func.basic_blocks for i in b.instructions]


# ---------------------------------------------------------------------------
# TestSystemFCompilerBasics
# ---------------------------------------------------------------------------


class TestSystemFCompilerBasics:
    def test_compile_returns_program(self) -> None:
        assert isinstance(_compile("42"), Program)

    def test_compile_has_main_function(self) -> None:
        assert "main" in _compile("42").functions

    def test_literal_int_compiles(self) -> None:
        program = _compile("42")
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 42.0

    def test_literal_zero_compiles(self) -> None:
        program = _compile("0")
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 0.0

    def test_literal_float_compiles(self) -> None:
        program = _compile("3")
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 3.0

    def test_main_entry_block_label(self) -> None:
        program = _compile("1")
        main = program.functions["main"]
        assert main.basic_blocks[0].label == "entry"

    def test_main_has_at_least_one_block(self) -> None:
        program = _compile("5")
        assert len(program.functions["main"].basic_blocks) >= 1

    def test_compile_does_not_raise_on_simple_expr(self) -> None:
        _compile("100")  # must not raise


# ---------------------------------------------------------------------------
# TestSystemFLambda
# ---------------------------------------------------------------------------


class TestSystemFLambda:
    def test_lambda_creates_extra_function(self) -> None:
        source = r"\x : Int => x"
        program = _compile(source)
        assert len(program.functions) >= 2

    def test_lambda_function_has_param(self) -> None:
        source = r"\x : Int => x"
        program = _compile(source)
        lambda_funcs = [f for name, f in program.functions.items() if "lambda" in name]
        assert len(lambda_funcs) >= 1
        assert "x" in lambda_funcs[0].parameters

    def test_lambda_function_has_entry_block(self) -> None:
        source = r"\x : Int => x"
        program = _compile(source)
        lambda_funcs = [f for name, f in program.functions.items() if "lambda" in name]
        assert len(lambda_funcs[0].basic_blocks) >= 1

    def test_lambda_body_return_is_var(self) -> None:
        # \x : Int => x; lambda body returns x
        source = r"\x : Int => x"
        program = _compile(source)
        lambda_funcs = [f for name, f in program.functions.items() if "lambda" in name]
        entry = lambda_funcs[0].basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, VariableValue)
        assert ret.value.name == "x"

    def test_lambda_main_returns_constant(self) -> None:
        # Main gets a Constant reference back from lambda lift
        source = r"\x : Int => x"
        program = _compile(source)
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)

    def test_nested_lambda_creates_two_functions(self) -> None:
        source = r"\x : Int => \y : Int => x"
        program = _compile(source)
        lambda_funcs = [name for name in program.functions if "lambda" in name]
        assert len(lambda_funcs) >= 2


# ---------------------------------------------------------------------------
# TestSystemFTypeAbstraction
# ---------------------------------------------------------------------------


class TestSystemFTypeAbstraction:
    def test_type_abstraction_erased_returns_body(self) -> None:
        # /\a => 42 compiles body (42) as if no type abstraction
        source = r"/\a => 42"
        program = _compile(source)
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 42.0

    def test_type_abstraction_over_lambda_lifts_function(self) -> None:
        # /\a => \x : a => x: lambda should still be lifted
        source = r"/\a => \x : a => x"
        program = _compile(source)
        assert len(program.functions) >= 2

    def test_double_type_abstraction_erased(self) -> None:
        source = r"/\a => /\b => 7"
        program = _compile(source)
        main = program.functions["main"]
        entry = main.basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 7.0


# ---------------------------------------------------------------------------
# TestSystemFApplication
# ---------------------------------------------------------------------------


class TestSystemFApplication:
    def test_application_emits_call_in_main(self) -> None:
        source = r"(\x : Int => x) 42"
        program = _compile(source)
        all_instrs = _all_instrs(program)
        assert any(isinstance(i, Call) for i in all_instrs)

    def test_type_application_erased_compiles(self) -> None:
        # Type application erased at runtime
        source = r"(/\a => \x : a => x) [Int]"
        program = _compile(source)
        assert "main" in program.functions

    def test_full_identity_application_has_call(self) -> None:
        # Apply identity to 42
        source = r"(/\a => \x : a => x) [Int] 42"
        program = _compile(source)
        all_instrs = _all_instrs(program)
        assert any(isinstance(i, Call) for i in all_instrs)


# ---------------------------------------------------------------------------
# TestSystemFIfExpr
# ---------------------------------------------------------------------------


class TestSystemFIfExpr:
    def test_if_compiles(self) -> None:
        program = _compile("if 1 then 2 else 3")
        assert "main" in program.functions

    def test_if_creates_multiple_blocks(self) -> None:
        program = _compile("if 1 then 2 else 3")
        main = program.functions["main"]
        assert len(main.basic_blocks) > 1

    def test_if_has_branch_terminator(self) -> None:
        program = _compile("if 1 then 2 else 3")
        main = program.functions["main"]
        terminators = [b.terminator for b in main.basic_blocks]
        assert any(isinstance(t, BranchTerminator) for t in terminators)

    def test_if_result_in_local_variables(self) -> None:
        program = _compile("if 1 then 2 else 3")
        main = program.functions["main"]
        result_vars = [k for k in main.local_variables if "if_res" in k]
        assert len(result_vars) >= 1

    def test_if_then_else_emit_assignments(self) -> None:
        program = _compile("if 1 then 2 else 3")
        main = program.functions["main"]
        all_instrs = [i for b in main.basic_blocks for i in b.instructions]
        assert any(isinstance(i, Assignment) for i in all_instrs)

    def test_if_with_lambda_condition_compiles(self) -> None:
        source = r"if 0 then \x : Int => x else \y : Int => y"
        program = _compile(source)
        assert "main" in program.functions


# ---------------------------------------------------------------------------
# TestSystemFLetExpr
# ---------------------------------------------------------------------------


class TestSystemFLetExpr:
    def test_let_assigns_variable(self) -> None:
        source = r"let val : Int = 10 in val"
        program = _compile(source)
        assert "val" in program.functions["main"].local_variables

    def test_let_emits_assignment_instruction(self) -> None:
        source = r"let val : Int = 10 in val"
        program = _compile(source)
        entry = program.functions["main"].basic_blocks[0]
        assert any(isinstance(i, Assignment) for i in entry.instructions)

    def test_let_body_returns_variable(self) -> None:
        # let x : Int = 5 in x; return should reference x
        source = r"let x : Int = 5 in x"
        program = _compile(source)
        entry = program.functions["main"].basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, VariableValue)
        assert ret.value.name == "x"

    def test_let_rhs_is_constant(self) -> None:
        source = r"let n : Int = 99 in n"
        program = _compile(source)
        # n is in local_variables with DEC50 type
        assert "n" in program.functions["main"].local_variables

    def test_let_with_lambda_rhs(self) -> None:
        source = r"let f : Int = \x : Int => x in f"
        program = _compile(source)
        assert "f" in program.functions["main"].local_variables


# ---------------------------------------------------------------------------
# TestSystemFComplexPrograms
# ---------------------------------------------------------------------------


class TestSystemFComplexPrograms:
    def test_identity_function(self) -> None:
        source = r"/\a => \x : a => x"
        program = _compile(source)
        assert "main" in program.functions
        assert len(program.functions) >= 2

    def test_type_application_identity(self) -> None:
        source = r"(/\a => \x : a => x) [Int] 42"
        program = _compile(source)
        entry = program.functions["main"].basic_blocks[0]
        assert any(isinstance(i, Call) for i in entry.instructions)

    def test_deeply_nested_type_abstraction(self) -> None:
        source = r"/\a => /\b => \x : a => x"
        program = _compile(source)
        lambda_funcs = [name for name in program.functions if "lambda" in name]
        assert len(lambda_funcs) >= 1

    def test_curried_lambda_application(self) -> None:
        source = r"(\x : Int => \y : Int => x) 1"
        program = _compile(source)
        all_instrs = [i for b in program.functions["main"].basic_blocks for i in b.instructions]
        assert any(isinstance(i, Call) for i in all_instrs)

    def test_if_with_let_body(self) -> None:
        source = r"if 1 then let x : Int = 2 in x else 3"
        program = _compile(source)
        assert "main" in program.functions

    def test_lambda_with_literal_body(self) -> None:
        source = r"\x : Int => 42"
        program = _compile(source)
        lambda_funcs = [f for name, f in program.functions.items() if "lambda" in name]
        assert len(lambda_funcs) >= 1
        entry = lambda_funcs[0].basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 42.0
