# Ancient Compute - IDRIS Compiler Tests

from __future__ import annotations

import pytest

from ..ir_types import (
    Assignment,
    BinaryOp,
    BranchTerminator,
    Call,
    Constant,
    Function,
    Program,
    Return,
    VariableValue,
)
from .idris_ast import (
    ASTNode,
    Case,
    FunctionApplication,
    FunctionDeclaration,
    Identifier,
    IfExpr,
    Lambda,
    Let,
    Literal,
    Module,
    ProofExpr,
    TypeDeclaration,
    Var,
)
from .idris_compiler import IDRIS2Compiler, IdrisCompiler
from .idris_lexer import lexer as ply_lexer
from .idris_parser import parser

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _compile_module_body(decls: list) -> Program:
    """Compile a list of declarations wrapped in a Module."""
    compiler = IdrisCompiler()
    return compiler.compile(Module("Test", decls))


def _compile_func(name: str, args: list[str], body: ASTNode) -> Program:
    """Compile a single FunctionDeclaration."""
    return _compile_module_body([FunctionDeclaration(name, args, body)])


def _all_instructions(program: Program, func_name: str) -> list:
    func = program.functions[func_name]
    return [i for b in func.basic_blocks for i in b.instructions]


# ---------------------------------------------------------------------------
# Original golden test (kept verbatim)
# ---------------------------------------------------------------------------


def test_compile_function_declaration() -> None:
    # Remove type sig to avoid parser ambiguity without indentation
    code = """module Main
                 main = putStrLn "Hello, World!" """
    ast = parser.parse(code, lexer=ply_lexer)
    compiler = IdrisCompiler()
    program = compiler.compile(ast)

    assert isinstance(program, Program)
    assert "main" in program.functions

    func = program.functions["main"]
    assert isinstance(func, Function)
    assert func.name == "main"

    # Check that the function body contains a call instruction
    entry_block = func.basic_blocks[0]
    assert any(
        isinstance(instr, Call) and instr.function_name == "putStrLn"
        for instr in entry_block.instructions
    )


# ---------------------------------------------------------------------------
# TestIdrisCompilerBasics
# ---------------------------------------------------------------------------


class TestIdrisCompilerBasics:
    def test_compile_module_returns_program(self) -> None:
        module = Module("M", [FunctionDeclaration("f", [], Literal(1))])
        program = IdrisCompiler().compile(module)
        assert isinstance(program, Program)

    def test_compile_list_returns_program(self) -> None:
        program = IdrisCompiler().compile([FunctionDeclaration("g", [], Literal(2))])
        assert isinstance(program, Program)

    def test_compile_module_function_added(self) -> None:
        program = _compile_func("myFunc", [], Literal(0))
        assert "myFunc" in program.functions

    def test_compile_list_function_added(self) -> None:
        compiler = IdrisCompiler()
        program = compiler.compile([FunctionDeclaration("listFunc", [], Literal(5))])
        assert "listFunc" in program.functions

    def test_compile_empty_list(self) -> None:
        program = IdrisCompiler().compile([])
        assert isinstance(program, Program)
        assert len(program.functions) == 0

    def test_type_declaration_erased(self) -> None:
        # TypeDeclaration should not produce a function entry
        decls = [
            TypeDeclaration("f", Identifier("Int")),
            FunctionDeclaration("f", [], Literal(42)),
        ]
        program = IdrisCompiler().compile(decls)
        assert len(program.functions) == 1
        assert "f" in program.functions

    def test_function_with_args_has_parameters(self) -> None:
        program = _compile_func("add", ["x", "y"], Identifier("x"))
        func = program.functions["add"]
        assert "x" in func.parameters
        assert "y" in func.parameters

    def test_function_is_function_instance(self) -> None:
        program = _compile_func("fn", [], Literal(7))
        assert isinstance(program.functions["fn"], Function)

    def test_function_has_entry_block(self) -> None:
        program = _compile_func("fn", [], Literal(7))
        assert len(program.functions["fn"].basic_blocks) >= 1

    def test_multiple_functions_in_module(self) -> None:
        decls = [
            FunctionDeclaration("f", [], Literal(1)),
            FunctionDeclaration("g", [], Literal(2)),
        ]
        program = IdrisCompiler().compile(decls)
        assert "f" in program.functions
        assert "g" in program.functions

    def test_args_added_to_local_variables(self) -> None:
        program = _compile_func("h", ["a", "b"], Identifier("a"))
        func = program.functions["h"]
        assert "a" in func.local_variables
        assert "b" in func.local_variables


# ---------------------------------------------------------------------------
# TestIdrisLiteralAndIdentifier
# ---------------------------------------------------------------------------


class TestIdrisLiteralAndIdentifier:
    def _get_return_val(self, program: Program, name: str) -> object:
        entry = program.functions[name].basic_blocks[0]
        ret = next((i for i in entry.instructions if isinstance(i, Return)), None)
        assert ret is not None, "No Return instruction in entry block"
        return ret.value

    def test_literal_int_compiles_to_constant(self) -> None:
        program = _compile_func("f", [], Literal(42))
        val = self._get_return_val(program, "f")
        assert isinstance(val, Constant)
        assert val.value == 42.0

    def test_literal_float_compiles_to_constant(self) -> None:
        program = _compile_func("f", [], Literal(3.14))
        val = self._get_return_val(program, "f")
        assert isinstance(val, Constant)
        assert abs(val.value - 3.14) < 1e-9

    def test_literal_string_compiles_to_nonnegative_constant(self) -> None:
        program = _compile_func("f", [], Literal("hello"))
        val = self._get_return_val(program, "f")
        assert isinstance(val, Constant)
        assert val.value >= 0.0

    def test_identifier_returns_variable_value(self) -> None:
        program = _compile_func("f", ["x"], Identifier("x"))
        val = self._get_return_val(program, "f")
        assert isinstance(val, VariableValue)
        assert val.name == "x"

    def test_var_node_returns_variable_value(self) -> None:
        program = _compile_func("f", [], Var("alpha"))
        val = self._get_return_val(program, "f")
        assert isinstance(val, VariableValue)
        assert val.name == "alpha"

    def test_literal_zero(self) -> None:
        program = _compile_func("f", [], Literal(0))
        val = self._get_return_val(program, "f")
        assert isinstance(val, Constant)
        assert val.value == 0.0


# ---------------------------------------------------------------------------
# TestIdrisFunctionApplication
# ---------------------------------------------------------------------------


class TestIdrisFunctionApplication:
    def test_application_emits_call_instruction(self) -> None:
        app = FunctionApplication(Identifier("putStrLn"), [Literal("hello")])
        program = _compile_func("main", [], app)
        entry = program.functions["main"].basic_blocks[0]
        assert any(isinstance(i, Call) for i in entry.instructions)

    def test_application_call_function_name(self) -> None:
        app = FunctionApplication(Identifier("myFunc"), [Literal(1)])
        program = _compile_func("f", [], app)
        entry = program.functions["f"].basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert call.function_name == "myFunc"

    def test_application_dynamic_call_for_non_identifier_func(self) -> None:
        # func is a Literal (not Identifier) -> "dynamic_call"
        app = FunctionApplication(Literal(42), [Literal(1)])
        program = _compile_func("f", [], app)
        entry = program.functions["f"].basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert call.function_name == "dynamic_call"

    def test_application_with_multiple_args(self) -> None:
        app = FunctionApplication(Identifier("add"), [Identifier("x"), Identifier("y")])
        program = _compile_func("f", ["x", "y"], app)
        entry = program.functions["f"].basic_blocks[0]
        call = next(i for i in entry.instructions if isinstance(i, Call))
        assert len(call.arguments) == 2

    def test_application_returns_variable_value(self) -> None:
        app = FunctionApplication(Identifier("foo"), [Literal(0)])
        program = _compile_func("f", [], app)
        entry = program.functions["f"].basic_blocks[0]
        ret = next(i for i in entry.instructions if isinstance(i, Return))
        assert isinstance(ret.value, VariableValue)


# ---------------------------------------------------------------------------
# TestIdrisLetExpr
# ---------------------------------------------------------------------------


class TestIdrisLetExpr:
    def test_let_binds_variable_in_local_vars(self) -> None:
        let_expr = Let(
            bindings=[FunctionDeclaration("n", [], Literal(5))],
            body=Identifier("n"),
        )
        program = _compile_func("f", [], let_expr)
        assert "n" in program.functions["f"].local_variables

    def test_let_emits_assignment(self) -> None:
        let_expr = Let(
            bindings=[FunctionDeclaration("n", [], Literal(5))],
            body=Identifier("n"),
        )
        program = _compile_func("f", [], let_expr)
        entry = program.functions["f"].basic_blocks[0]
        assert any(isinstance(i, Assignment) for i in entry.instructions)

    def test_let_body_is_function_application(self) -> None:
        # body is FunctionApplication -> Call in instructions
        let_expr = Let(
            bindings=[FunctionDeclaration("fn", [], Literal(1))],
            body=FunctionApplication(Identifier("fn"), []),
        )
        program = _compile_func("f", [], let_expr)
        entry = program.functions["f"].basic_blocks[0]
        assert any(isinstance(i, Call) for i in entry.instructions)

    def test_let_multiple_bindings(self) -> None:
        let_expr = Let(
            bindings=[
                FunctionDeclaration("a", [], Literal(1)),
                FunctionDeclaration("b", [], Literal(2)),
            ],
            body=Identifier("a"),
        )
        program = _compile_func("f", [], let_expr)
        func = program.functions["f"]
        assert "a" in func.local_variables
        assert "b" in func.local_variables


# ---------------------------------------------------------------------------
# TestIdrisCaseExpr
# ---------------------------------------------------------------------------


class TestIdrisCaseExpr:
    def _case_program(self) -> Program:
        case_expr = Case(
            expr=Identifier("n"),
            alternatives=[
                (Literal(1), Literal(100)),  # type: ignore[list-item]
                (Identifier("_"), Literal(0)),  # type: ignore[list-item]
            ],
        )
        return _compile_func("f", ["n"], case_expr)

    def test_case_compiles_without_error(self) -> None:
        self._case_program()

    def test_case_creates_multiple_blocks(self) -> None:
        program = self._case_program()
        assert len(program.functions["f"].basic_blocks) > 1

    def test_case_result_variable_in_local_vars(self) -> None:
        program = self._case_program()
        # At least the scrutinee arg "n" plus the result tmp variable
        assert len(program.functions["f"].local_variables) >= 1

    def test_case_literal_pattern_emits_eq_binary_op(self) -> None:
        program = self._case_program()
        all_instrs = _all_instructions(program, "f")
        assert any(isinstance(i, BinaryOp) and i.op == "eq" for i in all_instrs)

    def test_case_wildcard_binds_scrutinee_name(self) -> None:
        program = self._case_program()
        # Identifier "_" pattern binds scrutinee to "_" in local_variables
        assert "_" in program.functions["f"].local_variables

    def test_case_no_alternatives_still_compiles(self) -> None:
        case_expr = Case(expr=Identifier("x"), alternatives=[])
        program = _compile_func("f", ["x"], case_expr)
        assert "f" in program.functions


# ---------------------------------------------------------------------------
# TestIdrisIfExpr
# ---------------------------------------------------------------------------


class TestIdrisIfExpr:
    def _if_program(self) -> Program:
        if_expr = IfExpr(
            condition=Identifier("x"),
            then_expr=Literal(1),
            else_expr=Literal(0),
        )
        return _compile_func("f", ["x"], if_expr)

    def test_if_compiles_without_error(self) -> None:
        self._if_program()

    def test_if_creates_multiple_blocks(self) -> None:
        program = self._if_program()
        assert len(program.functions["f"].basic_blocks) > 1

    def test_if_has_branch_terminator(self) -> None:
        program = self._if_program()
        func = program.functions["f"]
        terminators = [b.terminator for b in func.basic_blocks]
        assert any(isinstance(t, BranchTerminator) for t in terminators)

    def test_if_then_and_else_blocks_emit_assignments(self) -> None:
        program = self._if_program()
        all_instrs = _all_instructions(program, "f")
        assert any(isinstance(i, Assignment) for i in all_instrs)

    def test_if_nested_compiles(self) -> None:
        inner = IfExpr(
            condition=Identifier("y"),
            then_expr=Literal(10),
            else_expr=Literal(20),
        )
        outer = IfExpr(
            condition=Identifier("x"),
            then_expr=inner,
            else_expr=Literal(0),
        )
        program = _compile_func("f", ["x", "y"], outer)
        assert "f" in program.functions
        assert len(program.functions["f"].basic_blocks) > 2


# ---------------------------------------------------------------------------
# TestIdrisLambdaExpr
# ---------------------------------------------------------------------------


class TestIdrisLambdaExpr:
    def _lambda_program(self) -> Program:
        lambda_expr = Lambda("y", None, Identifier("y"))
        return _compile_func("g", [], lambda_expr)

    def test_lambda_creates_additional_function(self) -> None:
        program = self._lambda_program()
        assert len(program.functions) >= 2

    def test_lambda_function_has_correct_param(self) -> None:
        program = self._lambda_program()
        lambda_funcs = [f for name, f in program.functions.items() if "__lambda_" in name]
        assert len(lambda_funcs) >= 1
        assert "y" in lambda_funcs[0].parameters

    def test_lambda_function_has_basic_blocks(self) -> None:
        program = self._lambda_program()
        lambda_funcs = [f for name, f in program.functions.items() if "__lambda_" in name]
        assert len(lambda_funcs) >= 1
        assert len(lambda_funcs[0].basic_blocks) >= 1

    def test_lambda_outer_func_returns_variable_value(self) -> None:
        # Outer function "g" returns a VariableValue referencing the lambda fn name
        program = self._lambda_program()
        entry = program.functions["g"].basic_blocks[0]
        ret = next(i for i in entry.instructions if isinstance(i, Return))
        assert isinstance(ret.value, VariableValue)

    def test_lambda_body_identity(self) -> None:
        # Lambda body: Identifier("y") -> lambda function returns VariableValue("y")
        program = self._lambda_program()
        lambda_funcs = [f for name, f in program.functions.items() if "__lambda_" in name]
        assert len(lambda_funcs) >= 1
        entry = lambda_funcs[0].basic_blocks[0]
        ret = next(i for i in entry.instructions if isinstance(i, Return))
        assert isinstance(ret.value, VariableValue)
        assert ret.value.name == "y"


# ---------------------------------------------------------------------------
# TestIdrisProofExpr
# ---------------------------------------------------------------------------


class TestIdrisProofExpr:
    def test_proof_expr_compiles_to_zero_constant(self) -> None:
        proof = ProofExpr("refl", [])
        program = _compile_func("h", [], proof)
        entry = program.functions["h"].basic_blocks[0]
        ret = next(i for i in entry.instructions if isinstance(i, Return))
        assert isinstance(ret.value, Constant)
        assert ret.value.value == 0

    def test_proof_expr_no_extra_function(self) -> None:
        proof = ProofExpr("refl", [])
        program = _compile_func("h", [], proof)
        assert len(program.functions) == 1

    def test_proof_expr_with_args_compiles(self) -> None:
        proof = ProofExpr("cong", [Identifier("f"), Identifier("x")])
        program = _compile_func("h", ["f", "x"], proof)
        assert "h" in program.functions

    def test_proof_expr_various_names_compile(self) -> None:
        for name in ("refl", "sym", "trans", "cong"):
            proof = ProofExpr(name, [])
            program = _compile_func("h", [], proof)
            entry = program.functions["h"].basic_blocks[0]
            ret = next(i for i in entry.instructions if isinstance(i, Return))
            assert isinstance(ret.value, Constant)
            assert ret.value.value == 0


# ---------------------------------------------------------------------------
# TestIDRIS2Compiler
# ---------------------------------------------------------------------------


class TestIDRIS2Compiler:
    def test_accepts_source_string(self) -> None:
        code = 'module Main\nmain = putStrLn "Hello"'
        program = IDRIS2Compiler().compile(code)
        assert isinstance(program, Program)

    def test_source_function_in_program(self) -> None:
        code = 'module Main\nmain = putStrLn "Hello"'
        program = IDRIS2Compiler().compile(code)
        assert "main" in program.functions

    def test_source_function_has_call(self) -> None:
        code = 'module Main\nmain = putStrLn "Hello"'
        program = IDRIS2Compiler().compile(code)
        entry = program.functions["main"].basic_blocks[0]
        assert any(isinstance(i, Call) for i in entry.instructions)

    def test_accepts_ast_module(self) -> None:
        module = Module("M", [FunctionDeclaration("f", [], Literal(1))])
        program = IDRIS2Compiler().compile(module)
        assert isinstance(program, Program)
        assert "f" in program.functions

    def test_accepts_list_ast(self) -> None:
        decls = [FunctionDeclaration("g", [], Literal(99))]
        program = IDRIS2Compiler().compile(decls)
        assert "g" in program.functions

    def test_idris2_is_subclass_of_idris(self) -> None:
        assert issubclass(IDRIS2Compiler, IdrisCompiler)

    def test_invalid_source_raises(self) -> None:
        with pytest.raises(ValueError):
            IDRIS2Compiler().compile("@@@invalid source###")
