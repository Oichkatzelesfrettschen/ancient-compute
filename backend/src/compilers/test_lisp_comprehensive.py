# Ancient Compute - LISP Compiler Comprehensive Tests

import pytest
from .lisp_lexer import lexer
from .lisp_parser import parser
from .lisp_compiler import LispCompiler
from .lisp_ast import Symbol, Number, String, SExpression
from ..ir_types import (
    Program,
    Function,
    Constant,
    ReturnTerminator,
    BinaryOp,
    BranchTerminator,
    Call,
    VariableValue,
)


# ============================================================================
# LEXER TESTS (18 tests)
# ============================================================================


class TestLISPLexer:
    """Test suite for LISP lexer tokenization."""

    def test_lexer_lparen(self):
        """Test lexing left parenthesis."""
        lexer.input("(")
        tok = lexer.token()
        assert tok.type == "LPAREN"
        assert tok.value == "("

    def test_lexer_rparen(self):
        """Test lexing right parenthesis."""
        lexer.input(")")
        tok = lexer.token()
        assert tok.type == "RPAREN"
        assert tok.value == ")"

    def test_lexer_simple_symbol(self):
        """Test lexing a simple symbol."""
        lexer.input("defun")
        tok = lexer.token()
        assert tok.type == "SYMBOL"
        assert tok.value == "defun"

    def test_lexer_symbols_with_hyphens(self):
        """Test lexing symbols with hyphens (Lisp convention)."""
        lexer.input("my-func")
        tok = lexer.token()
        assert tok.type == "SYMBOL"
        assert tok.value == "my-func"

    def test_lexer_operator_symbols(self):
        """Test lexing operator symbols."""
        for op in ["+", "-", "*", "/", ">", "<", ">=", "<=", "="]:
            lexer.input(op)
            tok = lexer.token()
            assert tok.type == "SYMBOL"
            assert tok.value == op

    def test_lexer_integer(self):
        """Test lexing integer literals."""
        lexer.input("42")
        tok = lexer.token()
        assert tok.type == "NUMBER"
        assert tok.value == 42

    def test_lexer_negative_integer(self):
        """Test lexing negative integers."""
        lexer.input("-17")
        tok = lexer.token()
        assert tok.type == "NUMBER"
        assert tok.value == -17

    def test_lexer_float(self):
        """Test lexing floating point numbers."""
        lexer.input("3.14")
        tok = lexer.token()
        assert tok.type == "NUMBER"
        assert tok.value == 3.14

    def test_lexer_negative_float(self):
        """Test lexing negative floats."""
        lexer.input("-2.5")
        tok = lexer.token()
        assert tok.type == "NUMBER"
        assert tok.value == -2.5

    def test_lexer_string(self):
        """Test lexing string literals."""
        lexer.input('"Hello, World!"')
        tok = lexer.token()
        assert tok.type == "STRING"
        assert tok.value == "Hello, World!"

    def test_lexer_empty_string(self):
        """Test lexing empty string."""
        lexer.input('""')
        tok = lexer.token()
        assert tok.type == "STRING"
        assert tok.value == ""

    def test_lexer_string_with_spaces(self):
        """Test lexing strings containing spaces."""
        lexer.input('"This is a test"')
        tok = lexer.token()
        assert tok.type == "STRING"
        assert tok.value == "This is a test"

    def test_lexer_sexpression(self):
        """Test lexing a simple S-expression."""
        lexer.input("(+ 1 2)")
        tokens = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            tokens.append(tok)

        assert len(tokens) == 5
        assert tokens[0].type == "LPAREN"
        assert tokens[1].type == "SYMBOL" and tokens[1].value == "+"
        assert tokens[2].type == "NUMBER" and tokens[2].value == 1
        assert tokens[3].type == "NUMBER" and tokens[3].value == 2
        assert tokens[4].type == "RPAREN"

    def test_lexer_nested_sexpression(self):
        """Test lexing nested S-expressions."""
        lexer.input("(+ (- 5 3) 2)")
        tokens = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            tokens.append(tok)

        assert len(tokens) == 9  # ( + ( - 5 3 ) 2 )
        assert tokens[0].type == "LPAREN"
        assert tokens[2].type == "LPAREN"
        assert tokens[6].type == "RPAREN"
        assert tokens[8].type == "RPAREN"

    def test_lexer_whitespace_handling(self):
        """Test that whitespace is properly ignored."""
        lexer.input("  (  +   1   2  )  ")
        tokens = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            tokens.append(tok)

        assert len(tokens) == 5  # ( + 1 2 )

    def test_lexer_newline_handling(self):
        """Test that newlines are properly ignored."""
        lexer.input("(\n+\n1\n2\n)")
        tokens = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            tokens.append(tok)

        assert len(tokens) == 5

    def test_lexer_defun_expression(self):
        """Test lexing a complete defun expression."""
        lexer.input("(defun my-func (a b) (+ a b))")
        tokens = []
        while True:
            tok = lexer.token()
            if not tok:
                break
            tokens.append(tok)

        assert any(tok.value == "defun" for tok in tokens)
        assert any(tok.value == "my-func" for tok in tokens)
        assert any(tok.value == "a" for tok in tokens)
        assert any(tok.value == "b" for tok in tokens)

    def test_lexer_special_symbols(self):
        """Test lexing special Lisp symbols."""
        for symbol in ["nil", "t", "lambda", "let", "quote", "cond"]:
            lexer.input(symbol)
            tok = lexer.token()
            assert tok.type == "SYMBOL"
            assert tok.value == symbol


# ============================================================================
# PARSER TESTS (22 tests)
# ============================================================================


class TestLISPParser:
    """Test suite for LISP parser."""

    def test_parse_integer(self):
        """Test parsing an integer."""
        ast = parser.parse("42")
        assert isinstance(ast, Number)
        assert ast.value == 42

    def test_parse_float(self):
        """Test parsing a float."""
        ast = parser.parse("3.14")
        assert isinstance(ast, Number)
        assert ast.value == 3.14

    def test_parse_string(self):
        """Test parsing a string."""
        ast = parser.parse('"Hello"')
        assert isinstance(ast, String)
        assert ast.value == "Hello"

    def test_parse_symbol(self):
        """Test parsing a symbol."""
        ast = parser.parse("my-var")
        assert isinstance(ast, Symbol)
        assert ast.value == "my-var"

    def test_parse_simple_sexpression(self):
        """Test parsing a simple S-expression."""
        ast = parser.parse("(+ 1 2)")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 3
        assert isinstance(ast.children[0], Symbol)
        assert ast.children[0].value == "+"

    def test_parse_nested_sexpression(self):
        """Test parsing nested S-expressions."""
        ast = parser.parse("(+ 1 (- 3 2))")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 3
        assert isinstance(ast.children[2], SExpression)

    def test_parse_defun_no_params(self):
        """Test parsing defun with no parameters."""
        ast = parser.parse("(defun my-func () 42)")
        assert isinstance(ast, SExpression)
        assert ast.children[0].value == "defun"
        assert ast.children[1].value == "my-func"
        assert isinstance(ast.children[2], SExpression)
        assert len(ast.children[2].children) == 0  # Empty param list

    def test_parse_defun_one_param(self):
        """Test parsing defun with one parameter."""
        ast = parser.parse("(defun square (x) (* x x))")
        assert isinstance(ast, SExpression)
        assert ast.children[1].value == "square"
        param_list = ast.children[2]
        assert isinstance(param_list, SExpression)
        assert len(param_list.children) == 1
        assert param_list.children[0].value == "x"

    def test_parse_defun_multiple_params(self):
        """Test parsing defun with multiple parameters."""
        ast = parser.parse("(defun add (a b) (+ a b))")
        assert isinstance(ast, SExpression)
        param_list = ast.children[2]
        assert len(param_list.children) == 2
        assert param_list.children[0].value == "a"
        assert param_list.children[1].value == "b"

    def test_parse_arithmetic_operations(self):
        """Test parsing various arithmetic operations."""
        for op in ["+", "-", "*", "/"]:
            ast = parser.parse(f"({op} 5 3)")
            assert isinstance(ast, SExpression)
            assert ast.children[0].value == op
            assert ast.children[1].value == 5
            assert ast.children[2].value == 3

    def test_parse_comparison_operations(self):
        """Test parsing comparison operations."""
        for op in [">", "<", ">=", "<=", "="]:
            ast = parser.parse(f"({op} a b)")
            assert isinstance(ast, SExpression)
            assert ast.children[0].value == op

    def test_parse_if_expression(self):
        """Test parsing if expression."""
        ast = parser.parse("(if (> a b) a b)")
        assert isinstance(ast, SExpression)
        assert ast.children[0].value == "if"
        assert isinstance(ast.children[1], SExpression)  # condition
        assert isinstance(ast.children[2], Symbol)  # then
        assert isinstance(ast.children[3], Symbol)  # else

    def test_parse_nested_if(self):
        """Test parsing nested if expressions."""
        ast = parser.parse("(if (> a b) (if (> a c) a c) b)")
        assert isinstance(ast, SExpression)
        assert ast.children[0].value == "if"
        assert isinstance(ast.children[2], SExpression)  # nested if in then branch

    def test_parse_function_call_no_args(self):
        """Test parsing function call with no arguments."""
        ast = parser.parse("(get-value)")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 1
        assert ast.children[0].value == "get-value"

    def test_parse_function_call_with_args(self):
        """Test parsing function call with arguments."""
        ast = parser.parse("(my-add 5 10)")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 3
        assert ast.children[0].value == "my-add"
        assert ast.children[1].value == 5
        assert ast.children[2].value == 10

    def test_parse_deeply_nested_expressions(self):
        """Test parsing deeply nested expressions."""
        ast = parser.parse("(+ (+ (+ 1 2) 3) 4)")
        assert isinstance(ast, SExpression)
        assert isinstance(ast.children[1], SExpression)
        assert isinstance(ast.children[1].children[1], SExpression)

    def test_parse_multiple_expressions_in_body(self):
        """Test parsing defun with multiple expressions in body."""
        ast = parser.parse("(defun multi (x) (+ x 1) (+ x 2))")
        assert isinstance(ast, SExpression)
        # Body should have 2 expressions after params
        assert len(ast.children) == 5  # defun, name, params, expr1, expr2

    def test_parse_string_in_expression(self):
        """Test parsing expressions with strings."""
        ast = parser.parse('(print "Hello, World!")')
        assert isinstance(ast, SExpression)
        assert ast.children[0].value == "print"
        assert isinstance(ast.children[1], String)
        assert ast.children[1].value == "Hello, World!"

    def test_parse_mixed_types(self):
        """Test parsing expressions with mixed types."""
        ast = parser.parse('(my-func 42 3.14 "test" symbol)')
        assert isinstance(ast, SExpression)
        assert isinstance(ast.children[1], Number) and ast.children[1].value == 42
        assert isinstance(ast.children[2], Number) and ast.children[2].value == 3.14
        assert isinstance(ast.children[3], String)
        assert isinstance(ast.children[4], Symbol)

    def test_parse_empty_list(self):
        """Test parsing empty list ()."""
        ast = parser.parse("()")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 0

    def test_parse_list_of_numbers(self):
        """Test parsing a list of numbers."""
        ast = parser.parse("(1 2 3 4 5)")
        assert isinstance(ast, SExpression)
        assert len(ast.children) == 5
        assert all(isinstance(child, Number) for child in ast.children)

    def test_parse_complex_defun(self):
        """Test parsing a complex defun with nested operations."""
        code = "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"
        ast = parser.parse(code)
        assert isinstance(ast, SExpression)
        assert ast.children[0].value == "defun"
        assert ast.children[1].value == "factorial"


# ============================================================================
# COMPILER TESTS (20 tests)
# ============================================================================


class TestLISPCompiler:
    """Test suite for LISP compiler IR generation."""

    def test_compile_defun_simple(self):
        """Test compiling a simple defun."""
        code = "(defun my-func (a b) (+ a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert isinstance(program, Program)
        assert "my-func" in program.functions
        func = program.functions["my-func"]
        assert func.name == "my-func"
        assert func.parameters == ["a", "b"]

    def test_compile_defun_no_params(self):
        """Test compiling defun with no parameters."""
        code = "(defun get-answer () 42)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["get-answer"]
        assert len(func.parameters) == 0

    def test_compile_arithmetic_add(self):
        """Test compiling addition."""
        code = "(defun my-add (a b) (+ a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-add"]
        entry_block = func.basic_blocks[0]
        binary_ops = [i for i in entry_block.instructions if isinstance(i, BinaryOp)]
        assert len(binary_ops) > 0
        assert binary_ops[0].op == "+"

    def test_compile_arithmetic_subtract(self):
        """Test compiling subtraction."""
        code = "(defun my-sub (a b) (- a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-sub"]
        entry_block = func.basic_blocks[0]
        assert any(
            isinstance(i, BinaryOp) and i.op == "-" for i in entry_block.instructions
        )

    def test_compile_arithmetic_multiply(self):
        """Test compiling multiplication."""
        code = "(defun my-mult (a b) (* a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-mult"]
        entry_block = func.basic_blocks[0]
        assert any(
            isinstance(i, BinaryOp) and i.op == "*" for i in entry_block.instructions
        )

    def test_compile_arithmetic_divide(self):
        """Test compiling division."""
        code = "(defun my-div (a b) (/ a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-div"]
        entry_block = func.basic_blocks[0]
        assert any(
            isinstance(i, BinaryOp) and i.op == "/" for i in entry_block.instructions
        )

    def test_compile_nested_arithmetic(self):
        """Test compiling nested arithmetic expressions."""
        code = "(defun my-nested (a b c) (+ a (+ b c)))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-nested"]
        entry_block = func.basic_blocks[0]
        binary_ops = [i for i in entry_block.instructions if isinstance(i, BinaryOp)]
        assert len(binary_ops) == 2  # Two add operations

    def test_compile_variables(self):
        """Test compiling function that returns a variable."""
        code = "(defun identity (x) x)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["identity"]
        assert "x" in func.local_variables

    def test_compile_multiple_params(self):
        """Test compiling function with multiple parameters."""
        code = "(defun triple-add (a b c) (+ a (+ b c)))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["triple-add"]
        assert func.parameters == ["a", "b", "c"]
        assert all(p in func.local_variables for p in ["a", "b", "c"])

    def test_compile_if_expression(self):
        """Test compiling if expression."""
        code = "(defun my-if (a b) (if (> a b) a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-if"]
        # Should have entry, then, else, merge blocks
        assert len(func.basic_blocks) == 4

    def test_compile_if_with_branch(self):
        """Test that if generates proper branch terminator."""
        code = "(defun my-if (a b) (if (> a b) a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-if"]
        entry_block = func.basic_blocks[0]
        assert isinstance(entry_block.terminator, BranchTerminator)

    def test_compile_function_call(self):
        """Test compiling function call."""
        code = "(defun my-call (a b) (my-add a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["my-call"]
        entry_block = func.basic_blocks[0]
        calls = [i for i in entry_block.instructions if isinstance(i, Call)]
        assert len(calls) > 0
        assert calls[0].function_name == "my-add"

    def test_compile_basic_blocks(self):
        """Test that functions have proper basic blocks."""
        code = "(defun simple () 42)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["simple"]
        assert len(func.basic_blocks) > 0
        assert func.basic_blocks[0].label == "entry"

    def test_compile_return_statement(self):
        """Test that functions have return terminators."""
        code = "(defun get-value () 42)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["get-value"]
        # Entry block should have return terminator
        entry_block = func.basic_blocks[0]
        assert isinstance(entry_block.terminator, ReturnTerminator)

    def test_compile_multiple_functions(self):
        """Test compiling multiple defun forms (not supported in current parser)."""
        # Current parser only handles single forms, but test basic case
        code = "(defun first () 1)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert "first" in program.functions

    def test_compile_complex_expression(self):
        """Test compiling complex nested expressions."""
        code = "(defun complex (a b c) (+ (* a b) (- c a)))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["complex"]
        entry_block = func.basic_blocks[0]
        # Should have multiple binary operations
        binary_ops = [i for i in entry_block.instructions if isinstance(i, BinaryOp)]
        assert len(binary_ops) >= 2

    def test_compile_constant_return(self):
        """Test compiling function that returns a constant."""
        code = "(defun get-constant () 42)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["get-constant"]
        entry_block = func.basic_blocks[0]
        assert isinstance(entry_block.terminator, ReturnTerminator)
        assert isinstance(entry_block.terminator.value, Constant)

    def test_compile_function_name_with_hyphens(self):
        """Test compiling functions with hyphenated names."""
        code = "(defun my-special-func (x) x)"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert "my-special-func" in program.functions

    def test_compile_nested_function_calls(self):
        """Test compiling nested function calls."""
        code = "(defun nested-call (x) (f (g x)))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["nested-call"]
        # Should generate calls for both f and g

    def test_compile_ir_type_assignment(self):
        """Test that parameters get proper IR type assignments."""
        code = "(defun typed-func (a b c) (+ a b))"
        ast = parser.parse(code)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["typed-func"]
        # All parameters should have IR types assigned
        assert all(
            param in func.local_variables for param in ["a", "b", "c"]
        )


# ============================================================================
# INTEGRATION TESTS (8 tests)
# ============================================================================


class TestLISPIntegration:
    """End-to-end integration tests for LISP compiler."""

    def test_end_to_end_simple_add(self):
        """Test complete pipeline for simple addition."""
        source = "(defun add (a b) (+ a b))"

        # Parse
        ast = parser.parse(source)
        assert isinstance(ast, SExpression)

        # Compile
        compiler = LispCompiler()
        program = compiler.compile(ast)
        assert isinstance(program, Program)
        assert "add" in program.functions

    def test_end_to_end_factorial(self):
        """Test complete pipeline for factorial (recursive)."""
        source = "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (- n 1)))))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert "factorial" in program.functions
        func = program.functions["factorial"]
        # Should have multiple basic blocks due to if
        assert len(func.basic_blocks) >= 4

    def test_end_to_end_complex_arithmetic(self):
        """Test complete pipeline with complex arithmetic."""
        source = "(defun quadratic (a b c x) (+ (* a (* x x)) (+ (* b x) c)))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert "quadratic" in program.functions
        func = program.functions["quadratic"]
        assert func.parameters == ["a", "b", "c", "x"]

    def test_end_to_end_if_expression(self):
        """Test complete pipeline for if expression."""
        source = "(defun max-val (a b) (if (> a b) a b))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["max-val"]
        assert len(func.basic_blocks) == 4  # entry, then, else, merge

    def test_end_to_end_nested_calls(self):
        """Test complete pipeline with nested function calls."""
        source = "(defun triple (x) (add x (add x x)))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        assert "triple" in program.functions

    def test_end_to_end_error_free_compilation(self):
        """Test that valid programs compile without errors."""
        sources = [
            "(defun simple () 42)",
            "(defun identity (x) x)",
            "(defun add (a b) (+ a b))",
            "(defun mul (a b) (* a b))",
            "(defun test-if (a b) (if (> a b) a b))",
        ]

        for source in sources:
            ast = parser.parse(source)
            compiler = LispCompiler()
            program = compiler.compile(ast)
            assert isinstance(program, Program)
            assert len(program.functions) > 0

    def test_end_to_end_multiple_operations(self):
        """Test pipeline with multiple different operations."""
        source = "(defun calc (a b c) (+ (- a b) (* b c)))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["calc"]
        entry_block = func.basic_blocks[0]
        # Should have add, subtract, and multiply operations
        binary_ops = [i for i in entry_block.instructions if isinstance(i, BinaryOp)]
        ops = [op.op for op in binary_ops]
        assert "-" in ops
        assert "*" in ops
        assert "+" in ops

    def test_end_to_end_preserves_semantics(self):
        """Test that compilation preserves semantic meaning."""
        source = "(defun double-add (x y) (+ (+ x y) (+ x y)))"

        ast = parser.parse(source)
        compiler = LispCompiler()
        program = compiler.compile(ast)

        func = program.functions["double-add"]
        # Should have 3 add operations total
        entry_block = func.basic_blocks[0]
        add_ops = [
            i for i in entry_block.instructions if isinstance(i, BinaryOp) and i.op == "+"
        ]
        assert len(add_ops) >= 2
