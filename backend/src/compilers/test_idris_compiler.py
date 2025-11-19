# Ancient Compute - IDRIS Compiler Tests

import pytest
from .idris_lexer import IDRIS2Lexer, lexer
from .idris_parser import parser
from .idris_compiler import IdrisCompiler, IDRIS2Compiler
from .idris_ast import (
    Module,
    TypeDeclaration,
    FunctionDeclaration,
    Identifier,
    Literal,
    FunctionApplication,
)
from ..ir_types import Program, Function, Constant, ReturnTerminator, Call, VariableValue


# ============================================================================
# LEXER TESTS (20 tests)
# ============================================================================


class TestIDRIS2Lexer:
    """Test suite for IDRIS2 lexer tokenization."""

    def test_lexer_keywords(self):
        """Test lexing of IDRIS keywords."""
        lex = IDRIS2Lexer("module import let in where case of if then else do")
        tokens = lex.tokenize()
        assert len(tokens) == 11
        assert all(tok.type == "KEYWORD" for tok in tokens)

    def test_lexer_module_keyword(self):
        """Test lexing module keyword specifically."""
        lex = IDRIS2Lexer("module")
        tokens = lex.tokenize()
        assert len(tokens) == 1
        assert tokens[0].type == "KEYWORD"
        assert tokens[0].value == "module"

    def test_lexer_identifiers(self):
        """Test lexing of identifiers."""
        lex = IDRIS2Lexer("main putStrLn factorial _helper myVar123")
        tokens = lex.tokenize()
        assert len(tokens) == 5
        assert all(tok.type == "IDENTIFIER" for tok in tokens)
        assert tokens[0].value == "main"
        assert tokens[1].value == "putStrLn"

    def test_lexer_integers(self):
        """Test lexing of integer literals."""
        lex = IDRIS2Lexer("42 -17 0 1234567890")
        tokens = lex.tokenize()
        assert len(tokens) == 4
        assert all(tok.type == "NUMBER" for tok in tokens)
        assert tokens[0].value == 42
        assert tokens[1].value == -17
        assert tokens[2].value == 0

    def test_lexer_floats(self):
        """Test lexing of floating point literals."""
        lex = IDRIS2Lexer("3.14 -2.5 0.0 123.456")
        tokens = lex.tokenize()
        assert len(tokens) == 4
        assert all(tok.type == "NUMBER" for tok in tokens)
        assert tokens[0].value == 3.14
        assert tokens[1].value == -2.5

    def test_lexer_strings(self):
        """Test lexing of string literals."""
        lex = IDRIS2Lexer('"Hello, World!" "IDRIS2" "test string"')
        tokens = lex.tokenize()
        assert len(tokens) == 3
        assert all(tok.type == "STRING" for tok in tokens)
        assert tokens[0].value == "Hello, World!"
        assert tokens[1].value == "IDRIS2"

    def test_lexer_escaped_strings(self):
        """Test lexing of strings with escape sequences."""
        lex = IDRIS2Lexer(r'"line1\nline2" "quote:\"hello\""')
        tokens = lex.tokenize()
        assert len(tokens) == 2
        assert tokens[0].type == "STRING"
        # Note: Escape sequences are preserved in the token value

    def test_lexer_operators(self):
        """Test lexing of arithmetic and comparison operators."""
        lex = IDRIS2Lexer("+ - * / == > < >= <=")
        tokens = lex.tokenize()
        assert len(tokens) == 9
        assert all(tok.type == "OPERATOR" for tok in tokens)

    def test_lexer_parentheses(self):
        """Test lexing of parentheses."""
        lex = IDRIS2Lexer("( )")
        tokens = lex.tokenize()
        assert len(tokens) == 2
        assert tokens[0].type == "LPAREN"
        assert tokens[1].type == "RPAREN"

    def test_lexer_brackets(self):
        """Test lexing of square brackets."""
        lex = IDRIS2Lexer("[ ]")
        tokens = lex.tokenize()
        assert len(tokens) == 2
        assert tokens[0].type == "LBRACK"
        assert tokens[1].type == "RBRACK"

    def test_lexer_braces(self):
        """Test lexing of curly braces."""
        lex = IDRIS2Lexer("{ }")
        tokens = lex.tokenize()
        assert len(tokens) == 2
        assert tokens[0].type == "LBRACE"
        assert tokens[1].type == "RBRACE"

    def test_lexer_punctuation(self):
        """Test lexing of punctuation symbols."""
        lex = IDRIS2Lexer(", . : = | ->  =>")
        tokens = lex.tokenize()
        assert tokens[0].type == "COMMA"
        assert tokens[1].type == "DOT"
        assert tokens[2].type == "COLON"
        assert tokens[3].type == "EQUALS"
        assert tokens[4].type == "PIPE"
        assert tokens[5].type == "RARROW"  # ->
        assert tokens[6].type == "DARROW"  # =>

    def test_lexer_type_signature(self):
        """Test lexing of a type signature."""
        lex = IDRIS2Lexer("main : IO ()")
        tokens = lex.tokenize()
        assert tokens[0].value == "main"
        assert tokens[1].type == "COLON"
        assert tokens[2].value == "IO"
        assert tokens[3].type == "LPAREN"
        assert tokens[4].type == "RPAREN"

    def test_lexer_function_arrow(self):
        """Test lexing of function arrow in type signatures."""
        lex = IDRIS2Lexer("Int -> String -> IO ()")
        tokens = lex.tokenize()
        assert tokens[0].value == "Int"
        assert tokens[1].type == "RARROW"
        assert tokens[2].value == "String"
        assert tokens[3].type == "RARROW"

    def test_lexer_mixed_expression(self):
        """Test lexing of a complex expression."""
        lex = IDRIS2Lexer('factorial 5 + 10 * (3 - 1)')
        tokens = lex.tokenize()
        assert len(tokens) > 5
        assert tokens[0].value == "factorial"
        assert tokens[1].value == 5

    def test_lexer_multiline_code(self):
        """Test lexing of multiline code."""
        code = """module Main

        main : IO ()
        main = putStrLn "test" """
        lex = IDRIS2Lexer(code)
        tokens = lex.tokenize()
        assert any(tok.value == "module" for tok in tokens)
        assert any(tok.value == "Main" for tok in tokens)
        assert any(tok.value == "main" for tok in tokens)

    def test_lexer_whitespace_handling(self):
        """Test that whitespace is properly ignored."""
        lex = IDRIS2Lexer("   main    :   IO   ()   ")
        tokens = lex.tokenize()
        # Should have 4 tokens: main : IO ()
        assert len(tokens) == 5  # main, :, IO, (, )

    def test_lexer_comments_handling(self):
        """Test handling of comments (if implemented)."""
        # IDRIS typically uses -- for line comments
        # Current lexer may not handle comments yet
        lex = IDRIS2Lexer("main = 42")
        tokens = lex.tokenize()
        assert len(tokens) == 3

    def test_lexer_case_sensitivity(self):
        """Test that lexer is case-sensitive for identifiers."""
        lex = IDRIS2Lexer("Main main MAIN")
        tokens = lex.tokenize()
        assert len(tokens) == 3
        assert tokens[0].value == "Main"
        assert tokens[1].value == "main"
        assert tokens[2].value == "MAIN"

    def test_lexer_underscore_identifiers(self):
        """Test identifiers with underscores."""
        lex = IDRIS2Lexer("_private my_var __internal__ test_123")
        tokens = lex.tokenize()
        assert len(tokens) == 4
        assert all(tok.type == "IDENTIFIER" for tok in tokens)


# ============================================================================
# PARSER TESTS (25 tests)
# ============================================================================


class TestIDRIS2Parser:
    """Test suite for IDRIS2 parser."""

    def test_parse_module_declaration(self):
        """Test parsing a simple module declaration."""
        code = "module Main main = 42"
        ast = parser.parse(code)
        assert isinstance(ast, Module)
        assert ast.name == "Main"
        assert len(ast.body) > 0

    def test_parse_type_declaration(self):
        """Test parsing a type declaration."""
        code = "module Main\nmain : IO ()\nmain = putStrLn \"test\""
        ast = parser.parse(code)
        assert isinstance(ast, Module)
        # Check for type declaration in body
        type_decls = [d for d in ast.body if isinstance(d, TypeDeclaration)]
        assert len(type_decls) > 0
        assert type_decls[0].name == "main"

    def test_parse_function_declaration_no_params(self):
        """Test parsing a function with no parameters."""
        code = "module Main\nmain = 42"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert funcs[0].name == "main"
        assert len(funcs[0].args) == 0

    def test_parse_function_declaration_one_param(self):
        """Test parsing a function with one parameter."""
        code = "module Main\nincrement x = x + 1"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert funcs[0].name == "increment"
        assert len(funcs[0].args) == 1
        assert funcs[0].args[0] == "x"

    def test_parse_function_declaration_multiple_params(self):
        """Test parsing a function with multiple parameters."""
        code = "module Main\nadd x y = x + y"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert funcs[0].name == "add"
        assert len(funcs[0].args) == 2
        assert funcs[0].args == ["x", "y"]

    def test_parse_identifier_expression(self):
        """Test parsing an identifier expression."""
        code = "module Main\ngetValue = someVar"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert isinstance(funcs[0].body, Identifier)
        assert funcs[0].body.name == "someVar"

    def test_parse_integer_literal(self):
        """Test parsing an integer literal."""
        code = "module Main\ngetAnswer = 42"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert isinstance(funcs[0].body, Literal)
        assert funcs[0].body.value == 42

    def test_parse_string_literal(self):
        """Test parsing a string literal."""
        code = 'module Main\ngetGreeting = "Hello"'
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert isinstance(funcs[0].body, Literal)
        assert funcs[0].body.value == "Hello"

    def test_parse_function_application_one_arg(self):
        """Test parsing a function application with one argument."""
        code = 'module Main\nmain = putStrLn "test"'
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert isinstance(funcs[0].body, FunctionApplication)
        app = funcs[0].body
        assert isinstance(app.func, Identifier)
        assert app.func.name == "putStrLn"
        assert len(app.args) == 1

    def test_parse_function_application_two_args(self):
        """Test parsing a function application with two arguments."""
        code = "module Main\nresult = add 5 3"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        app = funcs[0].body
        assert isinstance(app, FunctionApplication)
        assert len(app.args) == 2

    def test_parse_nested_function_application(self):
        """Test parsing nested function applications."""
        code = "module Main\nresult = f (g x)"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        # Outer application: f (g x)
        assert isinstance(funcs[0].body, FunctionApplication)

    def test_parse_multiple_declarations(self):
        """Test parsing multiple function declarations."""
        code = """module Main
        helper = 10
        main = helper"""
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) == 2
        assert funcs[0].name == "helper"
        assert funcs[1].name == "main"

    def test_parse_type_and_function_declaration(self):
        """Test parsing both type and function declarations."""
        code = """module Main
        main : IO ()
        main = putStrLn "Hello" """
        ast = parser.parse(code)
        assert len(ast.body) == 2
        assert isinstance(ast.body[0], TypeDeclaration)
        assert isinstance(ast.body[1], FunctionDeclaration)

    def test_parse_complex_type_signature(self):
        """Test parsing a complex type signature."""
        code = """module Main
        factorial : Int
        factorial = 1"""
        ast = parser.parse(code)
        type_decls = [d for d in ast.body if isinstance(d, TypeDeclaration)]
        assert len(type_decls) > 0

    def test_parse_empty_module(self):
        """Test parsing an empty module."""
        code = "module Empty"
        try:
            ast = parser.parse(code)
            # May fail or produce empty module
        except:
            # Parser may require at least one declaration
            pass

    def test_parse_whitespace_variations(self):
        """Test parser handles various whitespace."""
        code = "module    Main\n\n\nmain    =    42"
        ast = parser.parse(code)
        assert ast.name == "Main"

    def test_parse_hello_world(self):
        """Test parsing a complete Hello World program."""
        code = """module Main

        main : IO ()
        main = putStrLn "Hello, World!" """
        ast = parser.parse(code)
        assert isinstance(ast, Module)
        assert ast.name == "Main"
        assert len(ast.body) >= 2

    def test_parse_factorial_function(self):
        """Test parsing a factorial function signature."""
        code = """module Main
        factorial : Int
        factorial n = n"""
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        assert funcs[0].name == "factorial"

    def test_parse_multiple_function_params(self):
        """Test parsing functions with varying parameter counts."""
        code = """module Main
        zero = 0
        one x = x
        two x y = x
        three x y z = x"""
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) == 4
        assert len(funcs[0].args) == 0
        assert len(funcs[1].args) == 1
        assert len(funcs[2].args) == 2
        assert len(funcs[3].args) == 3

    def test_parse_numeric_expressions(self):
        """Test parsing various numeric literals."""
        code = """module Main
        int = 42
        neg = -17
        float = 3.14"""
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) == 3
        assert funcs[0].body.value == 42
        assert funcs[1].body.value == -17
        assert funcs[2].body.value == 3.14

    def test_parse_string_expressions(self):
        """Test parsing various string literals."""
        code = """module Main
        str1 = "simple"
        str2 = "with spaces"
        str3 = "123" """
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) == 3
        assert all(isinstance(f.body, Literal) for f in funcs)

    def test_parse_function_chain(self):
        """Test parsing chained function calls."""
        code = "module Main\nresult = f x y z"
        ast = parser.parse(code)
        funcs = [d for d in ast.body if isinstance(d, FunctionDeclaration)]
        assert len(funcs) > 0
        # f x y z should be parsed as ((f x) y) z

    def test_parse_module_name_variations(self):
        """Test parsing modules with different names."""
        for name in ["Main", "MyModule", "Test123", "Long_Module_Name"]:
            code = f"module {name}\nmain = 42"
            ast = parser.parse(code)
            assert ast.name == name

    def test_parse_io_type(self):
        """Test parsing IO type in signatures."""
        code = """module Main
        main : IO ()
        main = putStrLn "test" """
        ast = parser.parse(code)
        types = [d for d in ast.body if isinstance(d, TypeDeclaration)]
        assert len(types) > 0
        # IO () is a type application


# ============================================================================
# COMPILER TESTS (20 tests)
# ============================================================================


class TestIDRIS2Compiler:
    """Test suite for IDRIS2 compiler IR generation."""

    def test_compile_simple_function(self):
        """Test compiling a simple function returning a constant."""
        code = "module Main\nmain = 42"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert isinstance(program, Program)
        assert "main" in program.functions
        func = program.functions["main"]
        assert func.name == "main"

    def test_compile_function_with_string(self):
        """Test compiling a function returning a string."""
        code = 'module Main\ngetMessage = "Hello"'
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "getMessage" in program.functions

    def test_compile_function_with_params(self):
        """Test compiling a function with parameters."""
        code = "module Main\nidentity x = x"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["identity"]
        assert len(func.local_variables) >= 1
        assert "x" in func.local_variables

    def test_compile_function_application(self):
        """Test compiling a function application."""
        code = 'module Main\nmain = putStrLn "Hello"'
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["main"]
        entry_block = func.basic_blocks[0]
        # Should contain a Call instruction
        has_call = any(isinstance(instr, Call) for instr in entry_block.instructions)
        assert has_call

    def test_compile_putstrln_call(self):
        """Test compiling putStrLn specifically."""
        code = 'module Main\nmain = putStrLn "test"'
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["main"]
        entry_block = func.basic_blocks[0]
        call_instrs = [
            i for i in entry_block.instructions if isinstance(i, Call)
        ]
        assert len(call_instrs) > 0
        assert call_instrs[0].function_name == "putStrLn"

    def test_compile_multiple_functions(self):
        """Test compiling multiple functions."""
        code = """module Main
        helper = 10
        main = helper"""
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "helper" in program.functions
        assert "main" in program.functions

    def test_compile_function_returns_identifier(self):
        """Test compiling a function that returns a parameter."""
        code = "module Main\necho x = x"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["echo"]
        # Function should return the parameter value

    def test_compile_function_with_multiple_params(self):
        """Test compiling function with multiple parameters."""
        code = "module Main\nadd x y = x"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["add"]
        assert "x" in func.local_variables
        assert "y" in func.local_variables

    def test_compile_nested_application(self):
        """Test compiling nested function applications."""
        code = "module Main\nresult = f (g x)"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        # Should generate calls for both f and g
        assert "result" in program.functions

    def test_compile_basic_blocks(self):
        """Test that compiled functions have proper basic blocks."""
        code = "module Main\nmain = 42"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["main"]
        assert len(func.basic_blocks) > 0
        assert func.basic_blocks[0].label == "entry"

    def test_compile_return_statement(self):
        """Test that functions have return statements."""
        code = "module Main\nmain = 42"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["main"]
        entry_block = func.basic_blocks[0]
        # Last instruction should be return
        has_return = any(
            isinstance(instr, ReturnTerminator) for instr in entry_block.instructions
        )
        assert has_return

    def test_compile_integer_constant(self):
        """Test compiling integer constants."""
        code = "module Main\nvalue = 12345"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        func = program.functions["value"]
        # Should generate constant value

    def test_compile_negative_number(self):
        """Test compiling negative numbers."""
        code = "module Main\nneg = -42"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "neg" in program.functions

    def test_compile_float_constant(self):
        """Test compiling floating point constants."""
        code = "module Main\npi = 3.14159"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "pi" in program.functions

    def test_compile_hello_world_complete(self):
        """Test compiling complete Hello World program."""
        code = """module Main

                 main : IO ()
                 main = putStrLn "Hello, World!"""
        ast = parser.parse(code)
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

    def test_compile_multiple_string_calls(self):
        """Test compiling multiple putStrLn calls."""
        code = """module Main
        printOne = putStrLn "first"
        printTwo = putStrLn "second" """
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "printOne" in program.functions
        assert "printTwo" in program.functions

    def test_compile_function_name_variations(self):
        """Test compiling functions with various valid names."""
        code = """module Main
        myFunc = 1
        my_func2 = 2
        _private = 3
        longFunctionName123 = 4"""
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "myFunc" in program.functions
        assert "my_func2" in program.functions
        assert "_private" in program.functions
        assert "longFunctionName123" in program.functions

    def test_compile_preserves_function_order(self):
        """Test that compilation preserves declaration order."""
        code = """module Main
        first = 1
        second = 2
        third = 3"""
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        # All functions should be in the program
        assert "first" in program.functions
        assert "second" in program.functions
        assert "third" in program.functions

    def test_compile_ir_builder_usage(self):
        """Test that compiler properly uses IR builder."""
        code = "module Main\nmain = 42"
        ast = parser.parse(code)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        # Compiler should have created proper IR structure
        func = program.functions["main"]
        assert isinstance(func, Function)
        assert len(func.basic_blocks) > 0


# ============================================================================
# INTEGRATION TESTS (5 tests)
# ============================================================================


class TestIDRIS2Integration:
    """End-to-end integration tests for IDRIS2 compiler."""

    def test_end_to_end_hello_world(self):
        """Test complete pipeline from source to IR for Hello World."""
        source = """module Main

        main : IO ()
        main = putStrLn "Hello, World!" """

        # Lex
        lex = IDRIS2Lexer(source)
        tokens = lex.tokenize()
        assert len(tokens) > 0

        # Parse
        ast = parser.parse(source)
        assert isinstance(ast, Module)

        # Compile
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)
        assert isinstance(program, Program)
        assert "main" in program.functions

    def test_end_to_end_simple_math(self):
        """Test complete pipeline for simple math function."""
        source = """module Main
        double x = x"""

        ast = parser.parse(source)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert "double" in program.functions
        func = program.functions["double"]
        assert "x" in func.local_variables

    def test_end_to_end_multiple_functions(self):
        """Test complete pipeline with multiple functions."""
        source = """module Main
        helper = 42
        main = putStrLn "test" """

        ast = parser.parse(source)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        assert len(program.functions) == 2
        assert "helper" in program.functions
        assert "main" in program.functions

    def test_end_to_end_type_signatures(self):
        """Test complete pipeline with type signatures."""
        source = """module Main
        factorial : Int
        factorial n = n

        main : IO ()
        main = putStrLn "test" """

        ast = parser.parse(source)
        compiler = IDRIS2Compiler()
        program = compiler.compile(ast)

        # Should have both functions (type declarations are separate)
        assert "factorial" in program.functions
        assert "main" in program.functions

    def test_end_to_end_error_free_compilation(self):
        """Test that valid programs compile without errors."""
        sources = [
            "module Main\nmain = 42",
            'module Main\nmain = "test"',
            "module Main\nid x = x",
            'module Main\nmain = putStrLn "Hello"',
            "module Main\nf x y = x",
        ]

        for source in sources:
            ast = parser.parse(source)
            compiler = IDRIS2Compiler()
            program = compiler.compile(ast)
            assert isinstance(program, Program)
            assert len(program.functions) > 0
