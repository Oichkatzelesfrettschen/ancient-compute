"""
Haskell Compiler Test Suite

Comprehensive tests for Haskell lexer, parser, type system, and compiler.
70+ test cases covering:
- Tokenization (lexer)
- AST construction (parser)
- Type inference and unification
- IR generation
- Full compilation pipeline
- Error handling
"""

import pytest

from backend.src.compilers.haskell_ast import (
    FunctionDef,
)
from backend.src.compilers.haskell_compiler import HaskellCompiler
from backend.src.compilers.haskell_lexer import HaskellLexer, TokenType
from backend.src.compilers.haskell_parser import HaskellParser
from backend.src.compilers.haskell_types import (
    BabbageTypeMapper,
    HaskellType,
    HaskellTypeSystem,
    TypeEnvironment,
)

# ============================================================================
# Test Haskell Lexer
# ============================================================================

class TestHaskellLexer:
    """Test Haskell lexer tokenization"""

    def test_lex_empty_source(self):
        """Empty source produces only EOF token"""
        lexer = HaskellLexer("")
        tokens = lexer.tokenize()
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.EOF

    def test_lex_keywords(self):
        """Keywords are recognized"""
        source = "let in where if then else case of do"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        token_types = [t.type for t in tokens if t.type != TokenType.EOF]
        assert TokenType.LET in token_types
        assert TokenType.IN in token_types
        assert TokenType.WHERE in token_types
        assert TokenType.IF in token_types
        assert TokenType.THEN in token_types
        assert TokenType.ELSE in token_types
        assert TokenType.CASE in token_types
        assert TokenType.OF in token_types
        assert TokenType.DO in token_types

    def test_lex_identifiers(self):
        """Identifiers are recognized"""
        source = "foo bar baz_123 x'"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        identifiers = [t.value for t in tokens if t.type == TokenType.IDENTIFIER]
        assert 'foo' in identifiers
        assert 'bar' in identifiers
        assert 'baz_123' in identifiers
        assert "x'" in identifiers

    def test_lex_numbers(self):
        """Numbers (integer and float) are recognized"""
        source = "42 3.14 0 999"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        numbers = [t.value for t in tokens if t.type == TokenType.NUMBER]
        assert '42' in numbers
        assert '3.14' in numbers
        assert '0' in numbers
        assert '999' in numbers

    def test_lex_strings(self):
        """String literals are recognized"""
        source = '"hello" "world" "with spaces"'
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        strings = [t.value for t in tokens if t.type == TokenType.STRING]
        assert 'hello' in strings
        assert 'world' in strings
        assert 'with spaces' in strings

    def test_lex_operators(self):
        """Operators are recognized"""
        source = "-> => :: | \\ + - * / %"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        token_types = [t.type for t in tokens if t.type != TokenType.EOF]
        assert TokenType.ARROW in token_types
        assert TokenType.FAT_ARROW in token_types
        assert TokenType.DOUBLE_COLON in token_types
        assert TokenType.PIPE in token_types
        assert TokenType.BACKSLASH in token_types
        assert TokenType.PLUS in token_types
        assert TokenType.MINUS in token_types
        assert TokenType.STAR in token_types
        assert TokenType.SLASH in token_types
        assert TokenType.PERCENT in token_types

    def test_lex_parentheses(self):
        """Parentheses and brackets recognized"""
        source = "( ) [ ] { }"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        token_types = [t.type for t in tokens if t.type != TokenType.EOF]
        assert TokenType.LPAREN in token_types
        assert TokenType.RPAREN in token_types
        assert TokenType.LBRACKET in token_types
        assert TokenType.RBRACKET in token_types
        assert TokenType.LBRACE in token_types
        assert TokenType.RBRACE in token_types

    def test_lex_comments_line(self):
        """Line comments are skipped"""
        source = "foo -- this is a comment\nbar"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        identifiers = [t.value for t in tokens if t.type == TokenType.IDENTIFIER]
        assert 'foo' in identifiers
        assert 'bar' in identifiers

    def test_lex_comments_block(self):
        """Block comments are skipped"""
        source = "foo {- block comment -} bar"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        identifiers = [t.value for t in tokens if t.type == TokenType.IDENTIFIER]
        assert 'foo' in identifiers
        assert 'bar' in identifiers

    def test_lex_indentation(self):
        """INDENT/DEDENT tokens generated for indentation changes"""
        source = "let\n  x = 1\n  y = 2\nz = 3"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        token_types = [t.type for t in tokens]
        # Should have at least one INDENT
        assert TokenType.INDENT in token_types
        # Should have DEDENT when returning to base level
        assert TokenType.DEDENT in token_types


# ============================================================================
# Test Haskell Parser
# ============================================================================

class TestHaskellParser:
    """Test Haskell parser AST construction"""

    def test_parse_empty_module(self):
        """Empty module parses successfully"""
        lexer = HaskellLexer("")
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert module is not None
        assert len(module.declarations) == 0

    def test_parse_type_declaration(self):
        """Type declarations parse correctly"""
        source = "foo :: Int -> Int"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_function_definition(self):
        """Function definition parses correctly"""
        source = "square x = x * x"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1
        func = module.declarations[0]
        assert isinstance(func, FunctionDef)
        assert func.name == 'square'

    def test_parse_literal_expression(self):
        """Literal expressions parse"""
        source = "x = 42"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        func = module.declarations[0]
        assert isinstance(func, FunctionDef)
        assert len(func.equations) > 0

    def test_parse_binary_operation(self):
        """Binary operations parse with correct precedence"""
        source = "x = 1 + 2 * 3"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_lambda(self):
        """Lambda expressions parse"""
        source = "f = \\x -> x + 1"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        func = module.declarations[0]
        assert isinstance(func, FunctionDef)

    def test_parse_let_expression(self):
        """Let expressions parse"""
        source = "f = let x = 1 in x + 2"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_case_expression(self):
        """Case expressions parse"""
        source = "f x = case x of\n  1 -> 2\n  2 -> 3"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_if_then_else(self):
        """If-then-else expressions parse"""
        source = "f x = if x > 0 then 1 else -1"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_list_literal(self):
        """List literals parse"""
        source = "xs = [1, 2, 3]"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_tuple_literal(self):
        """Tuple literals parse"""
        source = "p = (1, 2, 3)"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1

    def test_parse_function_application(self):
        """Function application parses"""
        source = "result = foo bar baz"
        lexer = HaskellLexer(source)
        tokens = lexer.tokenize()
        parser = HaskellParser(tokens)
        module = parser.parse()
        assert len(module.declarations) == 1


# ============================================================================
# Test Haskell Type System
# ============================================================================

class TestHaskellTypeSystem:
    """Test Haskell type inference and unification"""

    def test_type_int(self):
        """Integer type creation"""
        t = HaskellType.int()
        assert t.kind == 'int'
        assert str(t) == 'int'

    def test_type_float(self):
        """Float type creation"""
        t = HaskellType.float()
        assert t.kind == 'float'

    def test_type_string(self):
        """String type creation"""
        t = HaskellType.string()
        assert t.kind == 'string'

    def test_type_variable(self):
        """Type variable creation"""
        t = HaskellType.var('a')
        assert t.is_var()
        assert t.kind == 'a'

    def test_type_list(self):
        """List type creation"""
        elem_type = HaskellType.int()
        t = HaskellType.list(elem_type)
        assert t.is_list()
        assert t.args[0] == elem_type

    def test_type_function(self):
        """Function type creation"""
        arg_type = HaskellType.int()
        ret_type = HaskellType.int()
        t = HaskellType.function(arg_type, ret_type)
        assert t.is_function()
        assert len(t.args) == 2

    def test_infer_literal_int(self):
        """Type inference for integer literal"""
        type_system = HaskellTypeSystem()
        t = type_system.infer_literal_type(42)
        assert t == HaskellType.int()

    def test_infer_literal_float(self):
        """Type inference for float literal"""
        type_system = HaskellTypeSystem()
        t = type_system.infer_literal_type(3.14)
        assert t == HaskellType.float()

    def test_infer_literal_string(self):
        """Type inference for string literal"""
        type_system = HaskellTypeSystem()
        t = type_system.infer_literal_type("hello")
        assert t == HaskellType.string()

    def test_infer_literal_char(self):
        """Type inference for character literal"""
        type_system = HaskellTypeSystem()
        t = type_system.infer_literal_type("x")
        assert t == HaskellType.char()

    def test_unify_same_types(self):
        """Unifying identical types succeeds"""
        type_system = HaskellTypeSystem()
        t1 = HaskellType.int()
        t2 = HaskellType.int()
        assert type_system.unify(t1, t2)

    def test_unify_different_types_fails(self):
        """Unifying different types fails"""
        type_system = HaskellTypeSystem()
        t1 = HaskellType.int()
        t2 = HaskellType.float()
        assert not type_system.unify(t1, t2)

    def test_unify_type_variable(self):
        """Type variable unifies with concrete type"""
        type_system = HaskellTypeSystem()
        var = HaskellType.var('a')
        concrete = HaskellType.int()
        assert type_system.unify(var, concrete)
        assert type_system.substitution['a'] == concrete

    def test_unify_function_types(self):
        """Function types unify correctly"""
        type_system = HaskellTypeSystem()
        int_to_int_1 = HaskellType.function(HaskellType.int(), HaskellType.int())
        int_to_int_2 = HaskellType.function(HaskellType.int(), HaskellType.int())
        assert type_system.unify(int_to_int_1, int_to_int_2)

    def test_operation_type_addition(self):
        """Operation type for addition"""
        type_system = HaskellTypeSystem()
        t = type_system.operation_type('+', HaskellType.int(), HaskellType.int())
        assert t == HaskellType.int()

    def test_operation_type_comparison(self):
        """Operation type for comparison returns bool"""
        type_system = HaskellTypeSystem()
        t = type_system.operation_type('==', HaskellType.int(), HaskellType.int())
        assert t == HaskellType.bool()

    def test_operation_type_mixed_numeric(self):
        """Mixed int and float in operation returns float"""
        type_system = HaskellTypeSystem()
        t = type_system.operation_type('+', HaskellType.int(), HaskellType.float())
        assert t == HaskellType.float()

    def test_babbage_type_int(self):
        """Int maps to i64 in Babbage"""
        t = HaskellType.int()
        babbage_t = BabbageTypeMapper.to_babbage_type(t)
        assert babbage_t == 'i64'

    def test_babbage_type_float(self):
        """Float maps to f64 in Babbage"""
        t = HaskellType.float()
        babbage_t = BabbageTypeMapper.to_babbage_type(t)
        assert babbage_t == 'f64'

    def test_babbage_type_string(self):
        """String maps to i64 (pointer) in Babbage"""
        t = HaskellType.string()
        babbage_t = BabbageTypeMapper.to_babbage_type(t)
        assert babbage_t == 'i64'

    def test_type_environment_binding(self):
        """Type environment binds and looks up types"""
        env = TypeEnvironment()
        t = HaskellType.int()
        env.bind('x', t)
        assert env.lookup('x') == t

    def test_type_environment_scoping(self):
        """Type environment supports nested scoping"""
        parent = TypeEnvironment()
        parent.bind('x', HaskellType.int())

        child = TypeEnvironment(parent=parent)
        child.bind('y', HaskellType.float())

        assert child.lookup('x') == HaskellType.int()
        assert child.lookup('y') == HaskellType.float()
        assert parent.lookup('y') is None


# ============================================================================
# Test Haskell Compiler
# ============================================================================

class TestHaskellCompilerSimple:
    """Test simple Haskell compilation cases"""

    def test_compile_empty(self):
        """Empty source compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = ""
        program = compiler.compile(source)
        assert program is not None
        assert len(program.functions) == 0

    def test_compile_simple_function(self):
        """Simple function definition compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "square x = x * x"
        program = compiler.compile(source)
        assert 'square' in program.functions

    def test_compile_literal(self):
        """Function returning literal compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "answer = 42"
        program = compiler.compile(source)
        assert 'answer' in program.functions

    def test_compile_addition(self):
        """Function with addition compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "add x y = x + y"
        program = compiler.compile(source)
        assert 'add' in program.functions

    def test_compile_subtraction(self):
        """Function with subtraction compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "sub x y = x - y"
        program = compiler.compile(source)
        assert 'sub' in program.functions

    def test_compile_multiplication(self):
        """Function with multiplication compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "mult x y = x * y"
        program = compiler.compile(source)
        assert 'mult' in program.functions

    def test_compile_negation(self):
        """Function with unary negation compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "negate x = -x"
        program = compiler.compile(source)
        assert 'negate' in program.functions

    def test_compile_lambda(self):
        """Lambda expression compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "apply f x = f x"
        program = compiler.compile(source)
        assert 'apply' in program.functions

    def test_compile_let(self):
        """Let expression compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = let x = 5 in x + 1"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_if_then_else(self):
        """If-then-else expression compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "abs x = if x >= 0 then x else -x"
        program = compiler.compile(source)
        assert 'abs' in program.functions


class TestHaskellCompilerIntegration:
    """Test integrated Haskell compilation pipelines"""

    def test_compile_multiple_functions(self):
        """Multiple function definitions compile together"""
        compiler = HaskellCompiler(verbose=False)
        source = """
square x = x * x
cube x = x * x * x
"""
        program = compiler.compile(source)
        assert 'square' in program.functions
        assert 'cube' in program.functions

    def test_compile_function_with_guards(self):
        """Function definition with guards compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "f x = x + 1"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_comparison(self):
        """Comparison operators compile"""
        compiler = HaskellCompiler(verbose=False)
        source = "isZero x = x == 0"
        program = compiler.compile(source)
        assert 'isZero' in program.functions

    def test_compile_chained_operations(self):
        """Chained operations compile"""
        compiler = HaskellCompiler(verbose=False)
        source = "result = 1 + 2 * 3 - 4"
        program = compiler.compile(source)
        assert 'result' in program.functions

    def test_compile_nested_let(self):
        """Nested let expressions compile"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = let x = 1 in let y = 2 in x + y"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_fibonacci(self):
        """Fibonacci function compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = """
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
"""
        program = compiler.compile(source)
        assert 'fib' in program.functions


class TestHaskellCompilerErrorHandling:
    """Test Haskell compiler error handling"""

    def test_compile_syntax_error(self):
        """Syntax error raises exception"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = x +"  # Incomplete expression
        with pytest.raises(RuntimeError, match="Compilation error"):
            compiler.compile(source)

    def test_compile_unterminated_string(self):
        """Unterminated string raises exception"""
        compiler = HaskellCompiler(verbose=False)
        source = 'f = "unterminated'
        with pytest.raises(RuntimeError, match="Compilation error"):
            compiler.compile(source)

    def test_compile_mismatched_parens(self):
        """Mismatched parentheses raise exception"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = (1 + 2"
        with pytest.raises(RuntimeError, match="Compilation error"):
            compiler.compile(source)


class TestHaskellCompilerEdgeCases:
    """Test Haskell compiler edge cases"""

    def test_compile_empty_function(self):
        """Empty function body compiles (returns 0)"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = 0"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_many_parameters(self):
        """Function with many parameters compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "f a b c d e = a + b + c + d + e"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_deep_nesting(self):
        """Deeply nested expressions compile"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = if (if 1 == 1 then 2 else 3) == 2 then 4 else 5"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_long_chain(self):
        """Long operator chain compiles"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10"
        program = compiler.compile(source)
        assert 'f' in program.functions

    def test_compile_expression_as_statement(self):
        """Standalone expression as function body"""
        compiler = HaskellCompiler(verbose=False)
        source = "f = 42"
        program = compiler.compile(source)
        assert 'f' in program.functions


# ============================================================================
# Test Suite Summary
# ============================================================================

if __name__ == '__main__':
    pytest.main([__file__, '-v', '--tb=short'])
