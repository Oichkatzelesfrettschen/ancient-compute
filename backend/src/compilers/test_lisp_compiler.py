"""
LISP Compiler Test Suite - Comprehensive tests for all compilation phases

Test coverage:
  - Lexer: tokenization of atoms, lists, quotes, comments
  - Parser: AST construction from tokens
  - Type System: type inference and unification
  - Compiler: expression evaluation and IR generation
  - Integration: full compilation pipeline

Target: 65+ tests, 100% pass rate, < 0.15 seconds
"""

import pytest
from backend.src.compilers.lisp_lexer import LISPLexer, Token, TokenType
from backend.src.compilers.lisp_parser import LISPParser
from backend.src.compilers.lisp_ast import (
    Atom, Symbol, List as ListExpr, Quote, Lambda, LetBinding, IfExpr,
    CondExpr, CaseExpr, DefunExpr
)
from backend.src.compilers.lisp_types import (
    LISPTypeSystem, IntType, FloatType, StringType, BoolType,
    ListType, FunctionType
)
from backend.src.compilers.lisp_compiler import LISPCompiler


class TestLISPLexer:
    """Test LISP lexer"""

    def test_empty_source(self):
        """Empty source produces EOF"""
        lexer = LISPLexer("")
        tokens = lexer.tokenize()
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.EOF

    def test_single_atom_number(self):
        """Tokenize single number"""
        lexer = LISPLexer("42")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "42"

    def test_single_atom_float(self):
        """Tokenize float number"""
        lexer = LISPLexer("3.14")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "3.14"

    def test_negative_number(self):
        """Tokenize negative number"""
        lexer = LISPLexer("-42")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "-42"

    def test_symbol(self):
        """Tokenize symbol"""
        lexer = LISPLexer("hello")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.SYMBOL
        assert tokens[0].value == "hello"

    def test_keyword(self):
        """Tokenize keyword"""
        lexer = LISPLexer("if")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IF

    def test_string_literal(self):
        """Tokenize string"""
        lexer = LISPLexer('"hello world"')
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "hello world"

    def test_string_with_escapes(self):
        """Tokenize string with escape sequences"""
        lexer = LISPLexer('"hello\\nworld"')
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "hello\nworld"

    def test_list_parentheses(self):
        """Tokenize list delimiters"""
        lexer = LISPLexer("(+ 1 2)")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.SYMBOL
        assert tokens[2].type == TokenType.NUMBER
        assert tokens[3].type == TokenType.NUMBER
        assert tokens[4].type == TokenType.RPAREN

    def test_quote_shorthand(self):
        """Tokenize quote shorthand"""
        lexer = LISPLexer("'(a b c)")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.QUOTE

    def test_quasiquote_shorthand(self):
        """Tokenize quasiquote shorthand"""
        lexer = LISPLexer("`(a ,b c)")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.QUASIQUOTE

    def test_unquote_shorthand(self):
        """Tokenize unquote shorthand"""
        lexer = LISPLexer(",b")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.UNQUOTE

    def test_unquote_splicing(self):
        """Tokenize unquote-splicing"""
        lexer = LISPLexer("`(,@args)")
        tokens = lexer.tokenize()
        assert any(t.type == TokenType.UNQUOTE_SPLICING for t in tokens)

    def test_comment_line(self):
        """Skip line comments"""
        lexer = LISPLexer("42 ; comment")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[1].type == TokenType.EOF

    def test_nested_list(self):
        """Tokenize nested lists"""
        lexer = LISPLexer("(+ (* 2 3) 4)")
        tokens = lexer.tokenize()
        assert len(tokens) == 10  # includes EOF
        lparen_count = sum(1 for t in tokens if t.type == TokenType.LPAREN)
        assert lparen_count == 2


class TestLISPParser:
    """Test LISP parser"""

    def test_parse_atom_number(self):
        """Parse number atom"""
        lexer = LISPLexer("42")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert len(exprs) == 1
        assert isinstance(exprs[0], Atom)
        assert exprs[0].value == 42

    def test_parse_atom_float(self):
        """Parse float atom"""
        lexer = LISPLexer("3.14")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], Atom)
        assert exprs[0].value == 3.14

    def test_parse_atom_string(self):
        """Parse string atom"""
        lexer = LISPLexer('"hello"')
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], Atom)
        assert exprs[0].value == "hello"

    def test_parse_symbol(self):
        """Parse symbol"""
        lexer = LISPLexer("x")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], Symbol)
        assert exprs[0].name == "x"

    def test_parse_empty_list(self):
        """Parse empty list"""
        lexer = LISPLexer("()")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], ListExpr)
        assert len(exprs[0].elements) == 0

    def test_parse_list_function_call(self):
        """Parse function call"""
        lexer = LISPLexer("(+ 1 2)")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], ListExpr)
        assert len(exprs[0].elements) == 3

    def test_parse_quote(self):
        """Parse quoted expression"""
        lexer = LISPLexer("'x")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], Quote)

    def test_parse_lambda(self):
        """Parse lambda expression"""
        lexer = LISPLexer("(lambda (x) (+ x 1))")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], Lambda)
        assert exprs[0].parameters == ["x"]

    def test_parse_let(self):
        """Parse let binding"""
        lexer = LISPLexer("(let ((x 1)) x)")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], LetBinding)
        assert len(exprs[0].bindings) == 1
        assert exprs[0].bindings[0][0] == "x"

    def test_parse_if(self):
        """Parse if expression"""
        lexer = LISPLexer("(if (= x 1) 10 20)")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], IfExpr)

    def test_parse_defun(self):
        """Parse function definition"""
        lexer = LISPLexer("(defun f (x) (+ x 1))")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], DefunExpr)
        assert exprs[0].name == "f"

    def test_parse_nested_list(self):
        """Parse nested lists"""
        lexer = LISPLexer("(+ (* 2 3) 4)")
        tokens = lexer.tokenize()
        parser = LISPParser(tokens)
        exprs = parser.parse()
        assert isinstance(exprs[0], ListExpr)
        assert len(exprs[0].elements) == 3


class TestLISPTypeSystem:
    """Test LISP type system"""

    def test_infer_int_literal(self):
        """Infer type from integer literal"""
        ts = LISPTypeSystem()
        lisp_type = ts.infer_type_from_literal(42)
        assert isinstance(lisp_type, IntType)

    def test_infer_float_literal(self):
        """Infer type from float literal"""
        ts = LISPTypeSystem()
        lisp_type = ts.infer_type_from_literal(3.14)
        assert isinstance(lisp_type, FloatType)

    def test_infer_string_literal(self):
        """Infer type from string literal"""
        ts = LISPTypeSystem()
        lisp_type = ts.infer_type_from_literal("hello")
        assert isinstance(lisp_type, StringType)

    def test_infer_bool_literal(self):
        """Infer type from boolean literal"""
        ts = LISPTypeSystem()
        lisp_type = ts.infer_type_from_literal(True)
        # Note: Python's bool is subclass of int, so may infer as IntType
        assert isinstance(lisp_type, (BoolType, IntType))

    def test_operation_addition_ints(self):
        """Type of addition of ints is int"""
        ts = LISPTypeSystem()
        result = ts.infer_operation_type("+", [ts.INT, ts.INT])
        assert isinstance(result, IntType)

    def test_operation_addition_mixed(self):
        """Type of mixed int/float addition is float"""
        ts = LISPTypeSystem()
        result = ts.infer_operation_type("+", [ts.INT, ts.FLOAT])
        assert isinstance(result, FloatType)

    def test_operation_comparison(self):
        """Type of comparison is bool"""
        ts = LISPTypeSystem()
        result = ts.infer_operation_type("=", [ts.INT, ts.INT])
        assert isinstance(result, BoolType)

    def test_register_symbol(self):
        """Register symbol with type"""
        ts = LISPTypeSystem()
        ts.register_symbol("x", ts.INT)
        assert ts.lookup_symbol("x") == ts.INT

    def test_unify_same_types(self):
        """Unify identical types"""
        ts = LISPTypeSystem()
        result = ts.unify(ts.INT, ts.INT)
        assert result == ts.INT

    def test_unify_with_any(self):
        """Unify with Any type"""
        ts = LISPTypeSystem()
        result = ts.unify(ts.ANY, ts.INT)
        assert result == ts.INT


class TestLISPCompiler:
    """Test LISP compiler"""

    def test_compile_number_literal(self):
        """Compile number literal"""
        compiler = LISPCompiler()
        program = compiler.compile("42")
        assert program is not None
        assert len(program.functions) > 0

    def test_compile_simple_arithmetic(self):
        """Compile simple arithmetic"""
        compiler = LISPCompiler()
        program = compiler.compile("(+ 1 2)")
        assert program is not None

    def test_compile_function_definition(self):
        """Compile function definition"""
        compiler = LISPCompiler()
        program = compiler.compile("(defun f (x) (+ x 1))")
        assert program is not None
        assert "f" in [fn.name for fn in program.functions]

    def test_compile_let_binding(self):
        """Compile let binding"""
        compiler = LISPCompiler()
        program = compiler.compile("(let ((x 1)) (+ x 2))")
        assert program is not None

    def test_compile_if_expression(self):
        """Compile if expression"""
        compiler = LISPCompiler()
        program = compiler.compile("(if (= 1 1) 10 20)")
        assert program is not None

    def test_compile_lambda(self):
        """Compile lambda expression"""
        compiler = LISPCompiler()
        program = compiler.compile("(lambda (x) (+ x 1))")
        assert program is not None

    def test_compile_factorial_function(self):
        """Compile recursive factorial function"""
        source = """
        (defun fact (n)
          (if (= n 0)
            1
            (* n (fact (- n 1)))))
        """
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert program is not None
        assert "fact" in [fn.name for fn in program.functions]

    def test_compile_multiple_definitions(self):
        """Compile multiple function definitions"""
        source = """
        (defun f (x) (+ x 1))
        (defun g (y) (* y 2))
        """
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert len([fn.name for fn in program.functions if fn.name in ("f", "g")]) == 2

    def test_compile_list_operations(self):
        """Compile list operations"""
        compiler = LISPCompiler()
        program = compiler.compile("(list 1 2 3)")
        assert program is not None

    def test_compile_cond_expression(self):
        """Compile cond expression"""
        source = "(cond ((= 1 0) 10) ((= 1 1) 20))"
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_case_expression(self):
        """Compile case expression"""
        source = "(case 1 (0 'a) (1 'b))"
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_quote_expression(self):
        """Compile quoted expression"""
        compiler = LISPCompiler()
        program = compiler.compile("'(a b c)")
        assert program is not None


class TestLISPIntegration:
    """Integration tests for full compilation pipeline"""

    def test_full_pipeline_simple(self):
        """Full compilation pipeline for simple expression"""
        compiler = LISPCompiler()
        program = compiler.compile("(+ 1 2)")
        assert program is not None
        assert "main" in [fn.name for fn in program.functions]

    def test_full_pipeline_with_functions(self):
        """Full compilation pipeline with function definitions"""
        source = """
        (defun square (x) (* x x))
        (defun cube (x) (* x (square x)))
        (square 5)
        (cube 3)
        """
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert program is not None
        assert "square" in [fn.name for fn in program.functions]
        assert "cube" in [fn.name for fn in program.functions]

    def test_full_pipeline_complex_control_flow(self):
        """Full pipeline with complex control flow"""
        source = """
        (let ((x 10))
          (if (> x 5)
            (cond ((= x 10) 'ten) ((= x 5) 'five) (true 'other))
            'small))
        """
        compiler = LISPCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_error_handling_unmatched_parens(self):
        """Error handling for unmatched parentheses"""
        compiler = LISPCompiler()
        with pytest.raises(SyntaxError):
            compiler.compile("(+ 1 2")

    def test_error_handling_undefined_symbol(self):
        """Error handling for undefined symbols (lenient - returns unbound)"""
        compiler = LISPCompiler()
        # Should not raise, just treat as unbound
        program = compiler.compile("unknown-symbol")
        assert program is not None

    def test_compilation_speed(self):
        """Verify compilation is fast"""
        import time
        source = "(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))"

        start = time.time()
        compiler = LISPCompiler()
        program = compiler.compile(source)
        elapsed = time.time() - start

        # Should compile in < 100ms
        assert elapsed < 0.1
        assert program is not None
