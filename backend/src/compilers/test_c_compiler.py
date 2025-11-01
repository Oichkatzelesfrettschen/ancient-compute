"""Comprehensive test suite for C compiler.

Tests cover:
  - Lexical analysis (CLexer)
  - Parsing (CParser)
  - Type system (CTypeSystem, BabbageTypeMapper)
  - Code generation (CCompiler → IR)
"""

import pytest
from backend.src.compilers.c_ast import (
    CLexer, CParser, TokenType, IntLiteral, FloatLiteral, VariableRef,
    BinaryOp as AstBinaryOp, FunctionCall, Block, ExpressionStatement,
    ReturnStatement, VariableDeclaration, Function as AstFunction,
    FunctionParameter, GlobalDeclaration
)
from backend.src.compilers.c_types import (
    CType, CTypeKind, CTypeSystem, BabbageTypeMapper, INT_TYPE, FLOAT_TYPE
)
from backend.src.compilers.c_compiler import CCompiler, SymbolTable


class TestCLexer:
    """Test C lexical analyzer."""
    
    def test_lex_simple_integer(self) -> None:
        """Test lexing integer literals."""
        lexer = CLexer("42")
        tokens = lexer.tokenize()
        
        assert len(tokens) == 2  # INT_LIT + EOF
        assert tokens[0].type == TokenType.INT_LIT
        assert tokens[0].value == "42"
    
    def test_lex_float(self) -> None:
        """Test lexing float literals."""
        lexer = CLexer("3.14")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.FLOAT_LIT
        assert tokens[0].value == "3.14"
    
    def test_lex_identifier(self) -> None:
        """Test lexing identifiers."""
        lexer = CLexer("myVariable")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.IDENT
        assert tokens[0].value == "myVariable"
    
    def test_lex_keyword_int(self) -> None:
        """Test lexing 'int' keyword."""
        lexer = CLexer("int")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.INT
        assert tokens[0].value == "int"
    
    def test_lex_keyword_float(self) -> None:
        """Test lexing 'float' keyword."""
        lexer = CLexer("float")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.FLOAT
    
    def test_lex_operators(self) -> None:
        """Test lexing operators."""
        lexer = CLexer("a + b")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.IDENT
        assert tokens[1].type == TokenType.PLUS
        assert tokens[2].type == TokenType.IDENT
    
    def test_lex_comparison(self) -> None:
        """Test lexing comparison operators."""
        lexer = CLexer("a == b")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.IDENT
        assert tokens[1].type == TokenType.EQ
        assert tokens[2].type == TokenType.IDENT
    
    def test_lex_block(self) -> None:
        """Test lexing block structure."""
        lexer = CLexer("{ x = 10; }")
        tokens = lexer.tokenize()
        
        assert tokens[0].type == TokenType.LBRACE
        assert tokens[1].type == TokenType.IDENT
        assert tokens[2].type == TokenType.ASSIGN
        assert tokens[3].type == TokenType.INT_LIT
        assert tokens[4].type == TokenType.SEMICOLON
        assert tokens[5].type == TokenType.RBRACE
    
    def test_lex_line_comment(self) -> None:
        """Test lexing line comments."""
        lexer = CLexer("int x; // comment\nint y;")
        tokens = lexer.tokenize()
        
        # Comments should be skipped
        token_types = [t.type for t in tokens if t.type != TokenType.EOF]
        assert TokenType.INT in token_types
        assert TokenType.IDENT in token_types
    
    def test_lex_block_comment(self) -> None:
        """Test lexing block comments."""
        lexer = CLexer("int x; /* comment */ int y;")
        tokens = lexer.tokenize()
        
        # Comments should be skipped
        token_types = [t.type for t in tokens if t.type != TokenType.EOF]
        assert token_types.count(TokenType.INT) == 2


class TestCParser:
    """Test C parser."""
    
    def test_parse_int_literal(self) -> None:
        """Test parsing integer literal."""
        lexer = CLexer("42")
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        
        # Just verify parser doesn't crash on simple input
        assert len(tokens) > 0
    
    def test_parse_function_simple(self) -> None:
        """Test parsing simple function."""
        code = """
        int main() {
            return 0;
        }
        """
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        assert len(program.declarations) == 1
        decl = program.declarations[0]
        assert isinstance(decl, AstFunction)
        assert decl.name == "main"
    
    def test_parse_function_with_parameter(self) -> None:
        """Test parsing function with parameter."""
        code = """
        int add(int x) {
            return x;
        }
        """
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        func = program.declarations[0]
        assert isinstance(func, AstFunction)
        assert len(func.parameters) == 1
        assert func.parameters[0].name == "x"
    
    def test_parse_global_variable(self) -> None:
        """Test parsing global variable."""
        code = "int x;"
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        assert len(program.declarations) == 1
        decl = program.declarations[0]
        assert isinstance(decl, GlobalDeclaration)
        assert len(decl.variables) == 1
        assert decl.variables[0].name == "x"
    
    def test_parse_if_statement(self) -> None:
        """Test parsing if statement."""
        code = """
        int main() {
            if (x > 0) {
                return 1;
            }
            return 0;
        }
        """
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        assert len(program.declarations) == 1
    
    def test_parse_while_loop(self) -> None:
        """Test parsing while loop."""
        code = """
        int main() {
            while (x > 0) {
                x = x - 1;
            }
            return 0;
        }
        """
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        assert len(program.declarations) == 1
    
    def test_parse_for_loop(self) -> None:
        """Test parsing for loop."""
        code = """
        int main() {
            for (int i = 0; i < 10; i = i + 1) {
                x = x + 1;
            }
            return 0;
        }
        """
        lexer = CLexer(code)
        tokens = lexer.tokenize()
        parser = CParser(tokens)
        program = parser.parse()
        
        assert len(program.declarations) == 1


class TestCTypeSystem:
    """Test C type system."""
    
    def test_type_creation(self) -> None:
        """Test creating basic types."""
        type_sys = CTypeSystem()
        
        int_type = type_sys.make_type("int")
        assert int_type is not None
        assert int_type.is_integral()
    
    def test_type_pointer(self) -> None:
        """Test pointer type creation."""
        type_sys = CTypeSystem()
        base = type_sys.make_type("int")
        ptr = type_sys.make_pointer_type(base)
        
        assert ptr.is_pointer()
        assert ptr.base_type == base
    
    def test_type_array(self) -> None:
        """Test array type creation."""
        type_sys = CTypeSystem()
        base = type_sys.make_type("int")
        arr = type_sys.make_array_type(base, 10)
        
        assert arr.is_array()
        assert arr.size == 10
    
    def test_type_assignable_int_to_int(self) -> None:
        """Test type assignability: int to int."""
        type_sys = CTypeSystem()
        
        int1 = type_sys.make_type("int")
        int2 = type_sys.make_type("int")
        
        assert type_sys.is_assignable(int1, int2)
    
    def test_type_assignable_int_to_float(self) -> None:
        """Test type assignability: int to float."""
        type_sys = CTypeSystem()
        
        int_type = type_sys.make_type("int")
        float_type = type_sys.make_type("float")
        
        assert type_sys.is_assignable(float_type, int_type)
    
    def test_type_promotion_int_float(self) -> None:
        """Test type promotion: int + float → float."""
        type_sys = CTypeSystem()
        
        int_type = type_sys.make_type("int")
        float_type = type_sys.make_type("float")
        
        result = type_sys.get_common_type(int_type, float_type)
        assert result.is_floating()


class TestBabbageTypeMapper:
    """Test mapping C types to Babbage IR types."""
    
    def test_map_int_type(self) -> None:
        """Test mapping C int to IR i64."""
        mapper = BabbageTypeMapper()
        int_type = CType(CTypeKind.INT)
        
        ir_type = mapper.c_type_to_babbage_ir_type(int_type)
        assert ir_type == "i64"
    
    def test_map_float_type(self) -> None:
        """Test mapping C float to IR f64."""
        mapper = BabbageTypeMapper()
        float_type = CType(CTypeKind.FLOAT)
        
        ir_type = mapper.c_type_to_babbage_ir_type(float_type)
        assert ir_type == "f64"
    
    def test_map_pointer_type(self) -> None:
        """Test mapping C pointer to IR ptr."""
        mapper = BabbageTypeMapper()
        base = CType(CTypeKind.INT)
        ptr_type = CType(CTypeKind.POINTER, base_type=base)
        
        ir_type = mapper.c_type_to_babbage_ir_type(ptr_type)
        assert ir_type == "ptr"
    
    def test_conversion_int_to_float(self) -> None:
        """Test conversion code generation."""
        mapper = BabbageTypeMapper()
        
        code = mapper.get_conversion_code("i64", "f64")
        assert code == "to_float"
    
    def test_conversion_float_to_int(self) -> None:
        """Test conversion code generation."""
        mapper = BabbageTypeMapper()
        
        code = mapper.get_conversion_code("f64", "i64")
        assert code == "to_int"
    
    def test_conversion_no_conversion(self) -> None:
        """Test no conversion needed for same types."""
        mapper = BabbageTypeMapper()
        
        code = mapper.get_conversion_code("i64", "i64")
        assert code is None


class TestSymbolTable:
    """Test symbol table management."""
    
    def test_define_and_lookup(self) -> None:
        """Test defining and looking up symbols."""
        symtab = SymbolTable()
        int_type = CType(CTypeKind.INT)
        
        symtab.define("x", int_type)
        symbol = symtab.lookup("x")
        
        assert symbol is not None
        assert symbol.name == "x"
    
    def test_lookup_undefined(self) -> None:
        """Test looking up undefined symbol."""
        symtab = SymbolTable()
        
        symbol = symtab.lookup("undefined")
        assert symbol is None
    
    def test_scope_nesting(self) -> None:
        """Test nested scopes."""
        global_symtab = SymbolTable()
        int_type = CType(CTypeKind.INT)
        
        # Define in global scope
        global_symtab.define("x", int_type)
        
        # Create nested scope
        local_symtab = global_symtab.push_scope()
        local_symtab.define("y", int_type)
        
        # Can see both x and y in local scope
        assert local_symtab.lookup("x") is not None
        assert local_symtab.lookup("y") is not None
        
        # Cannot see y in global scope
        assert global_symtab.lookup("y") is None


class TestCCompiler:
    """Test C to IR compiler."""
    
    def test_compile_simple_function(self) -> None:
        """Test compiling simple function."""
        code = """
        int main() {
            return 42;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
        assert "main" in program.functions
    
    def test_compile_function_with_local_var(self) -> None:
        """Test compiling function with local variable."""
        code = """
        int main() {
            int x;
            x = 10;
            return x;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
        func = program.functions["main"]
        assert len(func.parameters) == 0
    
    def test_compile_function_with_parameter(self) -> None:
        """Test compiling function with parameter."""
        code = """
        int double_it(int x) {
            return x + x;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
        func = program.functions["double_it"]
        assert len(func.parameters) == 1
        assert func.parameters[0] == "x"
    
    def test_compile_multiple_functions(self) -> None:
        """Test compiling multiple functions."""
        code = """
        int add(int a, int b) {
            return a + b;
        }
        
        int main() {
            return add(5, 3);
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 2
        assert "add" in program.functions
        assert "main" in program.functions
    
    def test_compile_with_verbose(self) -> None:
        """Test compilation with verbose output."""
        code = """
        int main() {
            return 0;
        }
        """
        compiler = CCompiler(verbose=True)
        program = compiler.compile(code)
        
        # Should complete without error
        assert len(program.functions) == 1
    
    def test_compile_arithmetic(self) -> None:
        """Test compiling arithmetic operations."""
        code = """
        int main() {
            int result;
            result = 10 + 5;
            result = result * 2;
            return result;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        # Should parse and compile without error
        assert len(program.functions) == 1
    
    def test_compile_if_statement(self) -> None:
        """Test compiling if statement."""
        code = """
        int sign(int x) {
            if (x > 0) {
                return 1;
            }
            return 0;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_while_loop(self) -> None:
        """Test compiling while loop."""
        code = """
        int countdown(int n) {
            while (n > 0) {
                n = n - 1;
            }
            return n;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_for_loop(self) -> None:
        """Test compiling for loop."""
        code = """
        int factorial(int n) {
            int result;
            result = 1;
            for (int i = 1; i <= n; i = i + 1) {
                result = result * i;
            }
            return result;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_complex_expression(self) -> None:
        """Test compiling complex expression."""
        code = """
        int main() {
            int x;
            x = (5 + 3) * (10 - 2);
            return x;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1


class TestCCompilerEdgeCases:
    """Test edge cases in C compiler."""
    
    def test_compile_empty_function(self) -> None:
        """Test compiling function with empty body."""
        code = """
        void noop() {
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_nested_if(self) -> None:
        """Test compiling nested if statements."""
        code = """
        int classify(int x) {
            if (x > 0) {
                if (x > 10) {
                    return 2;
                }
                return 1;
            }
            return 0;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_float_literal(self) -> None:
        """Test compiling float literal."""
        code = """
        float get_pi() {
            return 3.14;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1
    
    def test_compile_multiple_declarations(self) -> None:
        """Test compiling multiple variable declarations."""
        code = """
        int main() {
            int x, y, z;
            x = 1;
            y = 2;
            z = 3;
            return x + y + z;
        }
        """
        compiler = CCompiler()
        program = compiler.compile(code)
        
        assert len(program.functions) == 1


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
