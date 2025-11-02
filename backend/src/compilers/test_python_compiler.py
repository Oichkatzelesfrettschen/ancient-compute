"""
Test Suite for Python Compiler

Comprehensive tests covering:
  - Lexer (tokenization with indentation)
  - Parser (AST construction)
  - Type system (type inference and operations)
  - Compiler (complete compilation pipeline)

Total: 50+ test cases
"""

import pytest
from backend.src.compilers.python_lexer import PythonLexer, TokenType, Token
from backend.src.compilers.python_parser import PythonParser
from backend.src.compilers.python_types import PythonType, PythonTypeSystem, BabbageTypeMapper
from backend.src.compilers.python_compiler import PythonCompiler
from backend.src.compilers.python_ast import (
    Constant,
    Name,
    BinOp,
    UnaryOp,
    Call,
    Assign,
    Return,
    If,
    While,
    For,
    FunctionDef,
    Module,
)


class TestPythonLexer:
    """Test Python lexer tokenization"""

    def test_simple_number(self):
        """Test integer literal"""
        lexer = PythonLexer("42")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "42"

    def test_float_number(self):
        """Test float literal"""
        lexer = PythonLexer("3.14")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "3.14"

    def test_string_literal(self):
        """Test string literal"""
        lexer = PythonLexer('"hello"')
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "hello"

    def test_identifier(self):
        """Test identifier"""
        lexer = PythonLexer("variable_name")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "variable_name"

    def test_keyword_def(self):
        """Test def keyword"""
        lexer = PythonLexer("def")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.DEF

    def test_keyword_return(self):
        """Test return keyword"""
        lexer = PythonLexer("return")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.RETURN

    def test_keyword_if(self):
        """Test if keyword"""
        lexer = PythonLexer("if")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IF

    def test_arithmetic_operators(self):
        """Test arithmetic operators"""
        lexer = PythonLexer("+ - * / // % **")
        tokens = lexer.tokenize()
        types = [t.type for t in tokens[:-1]]  # Skip EOF
        assert TokenType.PLUS in types
        assert TokenType.MINUS in types
        assert TokenType.STAR in types
        assert TokenType.SLASH in types
        assert TokenType.DOUBLE_SLASH in types
        assert TokenType.PERCENT in types
        assert TokenType.POWER in types

    def test_comparison_operators(self):
        """Test comparison operators"""
        lexer = PythonLexer("== != < <= > >=")
        tokens = lexer.tokenize()
        types = [t.type for t in tokens[:-1]]  # Skip EOF
        assert TokenType.EQUAL_EQUAL in types
        assert TokenType.NOT_EQUAL in types
        assert TokenType.LESS in types
        assert TokenType.LESS_EQUAL in types
        assert TokenType.GREATER in types
        assert TokenType.GREATER_EQUAL in types

    def test_indentation_indent(self):
        """Test INDENT token"""
        code = "if True:\n    x = 1"
        lexer = PythonLexer(code)
        tokens = lexer.tokenize()
        types = [t.type for t in tokens]
        assert TokenType.INDENT in types

    def test_indentation_dedent(self):
        """Test DEDENT token"""
        code = "if True:\n    x = 1\ny = 2"
        lexer = PythonLexer(code)
        tokens = lexer.tokenize()
        types = [t.type for t in tokens]
        assert TokenType.DEDENT in types

    def test_comment_ignored(self):
        """Test that comments are ignored"""
        code = "x = 1  # comment"
        lexer = PythonLexer(code)
        tokens = lexer.tokenize()
        # Should have: IDENTIFIER, EQUAL, NUMBER, EOF (no comment token)
        assert len(tokens) == 4


class TestPythonParser:
    """Test Python parser AST construction"""

    def test_parse_constant(self):
        """Test parsing constant"""
        lexer = PythonLexer("42")
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        assert len(module.body) == 1
        assert isinstance(module.body[0].value, Constant)

    def test_parse_variable_reference(self):
        """Test parsing variable reference"""
        lexer = PythonLexer("x")
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        assert isinstance(module.body[0].value, Name)

    def test_parse_binary_operation(self):
        """Test parsing binary operation"""
        lexer = PythonLexer("1 + 2")
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        expr = module.body[0].value
        assert isinstance(expr, BinOp)
        assert expr.op == "+"

    def test_parse_assignment(self):
        """Test parsing assignment"""
        lexer = PythonLexer("x = 42")
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        stmt = module.body[0]
        assert isinstance(stmt, Assign)
        assert stmt.target == "x"

    def test_parse_function_definition(self):
        """Test parsing function definition"""
        code = "def foo():\n    return 1"
        lexer = PythonLexer(code)
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        func = module.body[0]
        assert isinstance(func, FunctionDef)
        assert func.name == "foo"

    def test_parse_function_with_parameters(self):
        """Test parsing function with parameters"""
        code = "def add(a, b):\n    return a + b"
        lexer = PythonLexer(code)
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        func = module.body[0]
        assert func.name == "add"
        assert func.args == ["a", "b"]

    def test_parse_if_statement(self):
        """Test parsing if statement"""
        code = "if x:\n    y = 1"
        lexer = PythonLexer(code)
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        stmt = module.body[0]
        assert isinstance(stmt, If)

    def test_parse_while_loop(self):
        """Test parsing while loop"""
        code = "while x:\n    x = x - 1"
        lexer = PythonLexer(code)
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        stmt = module.body[0]
        assert isinstance(stmt, While)

    def test_parse_for_loop(self):
        """Test parsing for loop"""
        code = "for i in range(10):\n    x = i"
        lexer = PythonLexer(code)
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        stmt = module.body[0]
        assert isinstance(stmt, For)
        assert stmt.target == "i"

    def test_operator_precedence_multiplication_before_addition(self):
        """Test operator precedence: * before +"""
        lexer = PythonLexer("1 + 2 * 3")
        parser = PythonParser(lexer.tokenize())
        module = parser.parse()
        expr = module.body[0].value
        # Should parse as: 1 + (2 * 3), not (1 + 2) * 3
        assert isinstance(expr, BinOp)
        assert expr.op == "+"
        assert isinstance(expr.right, BinOp)
        assert expr.right.op == "*"


class TestPythonTypeSystem:
    """Test Python type system and inference"""

    def test_infer_int_literal(self):
        """Test inferring int type from literal"""
        ts = PythonTypeSystem()
        ptype = ts.infer_literal_type(42)
        assert ptype == PythonType.int()

    def test_infer_float_literal(self):
        """Test inferring float type from literal"""
        ts = PythonTypeSystem()
        ptype = ts.infer_literal_type(3.14)
        assert ptype == PythonType.float()

    def test_infer_string_literal(self):
        """Test inferring string type from literal"""
        ts = PythonTypeSystem()
        ptype = ts.infer_literal_type("hello")
        assert ptype == PythonType.str()

    def test_infer_bool_literal(self):
        """Test inferring bool type from literal"""
        ts = PythonTypeSystem()
        ptype = ts.infer_literal_type(True)
        assert ptype == PythonType.bool()

    def test_promote_int_to_float(self):
        """Test type promotion: int + float -> float"""
        ts = PythonTypeSystem()
        result = ts.promote_type(PythonType.int(), PythonType.float())
        assert result == PythonType.float()

    def test_promote_bool_to_int(self):
        """Test type promotion: bool + int -> int"""
        ts = PythonTypeSystem()
        result = ts.promote_type(PythonType.bool(), PythonType.int())
        assert result == PythonType.int()

    def test_operation_type_addition(self):
        """Test operation type: int + int -> int"""
        ts = PythonTypeSystem()
        result = ts.operation_type("+", PythonType.int(), PythonType.int())
        assert result == PythonType.int()

    def test_operation_type_comparison(self):
        """Test operation type: int == int -> bool"""
        ts = PythonTypeSystem()
        result = ts.operation_type("==", PythonType.int(), PythonType.int())
        assert result == PythonType.bool()

    def test_operation_type_division(self):
        """Test operation type: int / int -> float (Python semantics)"""
        ts = PythonTypeSystem()
        result = ts.operation_type("/", PythonType.int(), PythonType.int())
        assert result == PythonType.float()

    def test_mapper_int_to_i64(self):
        """Test mapping Python int to Babbage i64"""
        mapped = BabbageTypeMapper.to_babbage_type(PythonType.int())
        assert mapped == "i64"

    def test_mapper_float_to_f64(self):
        """Test mapping Python float to Babbage f64"""
        mapped = BabbageTypeMapper.to_babbage_type(PythonType.float())
        assert mapped == "f64"

    def test_mapper_bool_to_i64(self):
        """Test mapping Python bool to Babbage i64"""
        mapped = BabbageTypeMapper.to_babbage_type(PythonType.bool())
        assert mapped == "i64"


class TestPythonCompilerSimple:
    """Test Python compiler with simple cases"""

    def test_compile_simple_return(self):
        """Test compiling simple return"""
        compiler = PythonCompiler()
        code = "def main():\n    return 42"
        program = compiler.compile(code)
        assert "main" in program.functions

    def test_compile_function_with_parameter(self):
        """Test compiling function with parameter"""
        compiler = PythonCompiler()
        code = "def double(x):\n    return x + x"
        program = compiler.compile(code)
        assert "double" in program.functions
        assert program.functions["double"].parameters == ["x"]

    def test_compile_multiple_functions(self):
        """Test compiling multiple functions"""
        compiler = PythonCompiler()
        code = "def foo():\n    return 1\ndef bar():\n    return 2"
        program = compiler.compile(code)
        assert "foo" in program.functions
        assert "bar" in program.functions

    def test_compile_arithmetic_expression(self):
        """Test compiling arithmetic expression"""
        compiler = PythonCompiler()
        code = "def add():\n    return 2 + 3"
        program = compiler.compile(code)
        func = program.functions["add"]
        assert len(func.basic_blocks) > 0

    def test_compile_comparison_expression(self):
        """Test compiling comparison expression"""
        compiler = PythonCompiler()
        code = "def compare():\n    return 5 > 3"
        program = compiler.compile(code)
        func = program.functions["compare"]
        assert len(func.basic_blocks) > 0

    def test_compile_if_statement(self):
        """Test compiling if statement"""
        compiler = PythonCompiler()
        code = "def test():\n    if True:\n        return 1\n    return 0"
        program = compiler.compile(code)
        func = program.functions["test"]
        # Should have multiple blocks for if branches
        assert len(func.basic_blocks) >= 2

    def test_compile_while_loop(self):
        """Test compiling while loop"""
        compiler = PythonCompiler()
        code = "def countdown():\n    x = 5\n    while x:\n        x = x - 1\n    return x"
        program = compiler.compile(code)
        func = program.functions["countdown"]
        # Should have loop blocks
        assert len(func.basic_blocks) >= 2

    def test_compile_for_loop_range(self):
        """Test compiling for loop with range"""
        compiler = PythonCompiler()
        code = "def loop():\n    s = 0\n    for i in range(10):\n        s = s + i\n    return s"
        program = compiler.compile(code)
        func = program.functions["loop"]
        # Should have loop blocks
        assert len(func.basic_blocks) >= 2

    def test_compile_assignment(self):
        """Test compiling variable assignment"""
        compiler = PythonCompiler()
        code = "def assign():\n    x = 42\n    return x"
        program = compiler.compile(code)
        func = program.functions["assign"]
        # Check that assignment is in instructions
        has_assignment = False
        for block in func.basic_blocks:
            if any(hasattr(instr, "target") for instr in block.instructions):
                has_assignment = True
        assert has_assignment

    def test_compile_function_call(self):
        """Test compiling function call"""
        compiler = PythonCompiler()
        code = "def foo():\n    return 1\ndef bar():\n    return foo()"
        program = compiler.compile(code)
        func = program.functions["bar"]
        # Check for call instruction
        has_call = False
        for block in func.basic_blocks:
            for instr in block.instructions:
                if hasattr(instr, "function_name"):
                    has_call = True
        assert has_call


class TestPythonCompilerIntegration:
    """Integration tests for Python compiler"""

    def test_fibonacci(self):
        """Test compiling Fibonacci function"""
        compiler = PythonCompiler()
        code = """
def fib(n):
    if n <= 1:
        return n
    else:
        return fib(n - 1) + fib(n - 2)
"""
        program = compiler.compile(code)
        assert "fib" in program.functions

    def test_nested_if_elif_else(self):
        """Test nested if/elif/else"""
        compiler = PythonCompiler()
        code = """
def classify(x):
    if x < 0:
        return -1
    elif x == 0:
        return 0
    else:
        return 1
"""
        program = compiler.compile(code)
        func = program.functions["classify"]
        assert len(func.basic_blocks) >= 3

    def test_nested_loops(self):
        """Test nested loops"""
        compiler = PythonCompiler()
        code = """
def matrix_sum():
    total = 0
    for i in range(3):
        for j in range(3):
            total = total + 1
    return total
"""
        program = compiler.compile(code)
        func = program.functions["matrix_sum"]
        assert len(func.basic_blocks) >= 4

    def test_complex_expression(self):
        """Test complex expression"""
        compiler = PythonCompiler()
        code = """
def compute():
    x = 2 + 3 * 4 - 1
    return x
"""
        program = compiler.compile(code)
        assert "compute" in program.functions

    def test_boolean_operations(self):
        """Test boolean operations"""
        compiler = PythonCompiler()
        code = """
def logic():
    a = True
    b = False
    c = a and b
    d = a or b
    return d
"""
        program = compiler.compile(code)
        assert "logic" in program.functions


class TestPythonCompilerErrorHandling:
    """Test error handling in Python compiler"""

    def test_error_undefined_variable(self):
        """Test error on undefined variable in expression"""
        compiler = PythonCompiler()
        # This should not error at compile time (Python is dynamically typed)
        # but it's still valid IR generation
        code = "def foo():\n    return undefined_var"
        program = compiler.compile(code)
        assert "foo" in program.functions

    def test_error_unmatched_indent(self):
        """Test error on unmatched indentation"""
        compiler = PythonCompiler()
        code = "def foo():\n  x = 1\n    y = 2"  # Inconsistent indent
        with pytest.raises(RuntimeError):
            compiler.compile(code)

    def test_error_invalid_syntax(self):
        """Test error on invalid syntax"""
        compiler = PythonCompiler()
        code = "def foo(\n    return 1"  # Missing closing paren
        with pytest.raises(RuntimeError):
            compiler.compile(code)


class TestPythonCompilerEdgeCases:
    """Test edge cases"""

    def test_empty_function_body(self):
        """Test function with pass statement"""
        compiler = PythonCompiler()
        code = "def empty():\n    pass"
        program = compiler.compile(code)
        assert "empty" in program.functions

    def test_multiple_returns(self):
        """Test function with multiple return statements"""
        compiler = PythonCompiler()
        code = """
def multi_return(x):
    if x > 0:
        return 1
    else:
        return -1
"""
        program = compiler.compile(code)
        func = program.functions["multi_return"]
        # Each branch should have return
        has_returns = False
        for block in func.basic_blocks:
            if block.terminator:
                has_returns = True
        assert has_returns

    def test_zero_parameter_function(self):
        """Test function with no parameters"""
        compiler = PythonCompiler()
        code = "def no_params():\n    return 42"
        program = compiler.compile(code)
        func = program.functions["no_params"]
        assert func.parameters == []

    def test_many_parameter_function(self):
        """Test function with many parameters"""
        compiler = PythonCompiler()
        code = "def many(a, b, c, d, e):\n    return a + b + c + d + e"
        program = compiler.compile(code)
        func = program.functions["many"]
        assert len(func.parameters) == 5

    def test_deep_expression_nesting(self):
        """Test deeply nested expression"""
        compiler = PythonCompiler()
        code = "def deep():\n    return ((((1 + 2) * 3) - 4) / 5)"
        program = compiler.compile(code)
        assert "deep" in program.functions

    def test_string_literals(self):
        """Test string literals in function"""
        compiler = PythonCompiler()
        code = 'def greet():\n    s = "hello"\n    return 0'
        program = compiler.compile(code)
        assert "greet" in program.functions
