"""
IDRIS2 Compiler Test Suite - Comprehensive tests for all compilation phases

Test coverage:
  - Lexer: tokenization of types, functions, dependent syntax
  - Parser: AST construction from tokens, type parsing
  - Type System: type inference, dependent type checking, unification
  - Compiler: expression evaluation, IR generation, function compilation
  - Integration: full compilation pipeline, error handling

Target: 75+ tests, 100% pass rate, < 0.20 seconds
"""

import pytest
from backend.src.compilers.idris_lexer import IDRIS2Lexer, Token, TokenType
from backend.src.compilers.idris_parser import IDRIS2Parser
from backend.src.compilers.idris_ast import (
    Literal, Var, Lambda, Application, LetExpr, CaseExpr, IfExpr, DataConstructor,
    BaseType, TypeVariable, FunctionType, DependentType, TypeFamily, RefinementType,
    VarPattern, LiteralPattern, ConstructorPattern, WildcardPattern,
    TypeDeclaration, FunctionDef, DataDef, TypeDef, Module
)
from backend.src.compilers.idris_types import IDRISTypeSystem
from backend.src.compilers.idris_compiler import IDRIS2Compiler


class TestIDRIS2Lexer:
    """Test IDRIS2 lexer"""

    def test_empty_source(self) -> None:
        """Empty source produces EOF"""
        lexer = IDRIS2Lexer("")
        tokens = lexer.tokenize()
        assert len(tokens) == 1
        assert tokens[0].type == TokenType.EOF

    def test_single_number(self) -> None:
        """Tokenize single number"""
        lexer = IDRIS2Lexer("42")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "42"

    def test_type_name(self) -> None:
        """Tokenize type name (uppercase)"""
        lexer = IDRIS2Lexer("Nat")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.TYPE_NAME
        assert tokens[0].value == "Nat"

    def test_identifier(self) -> None:
        """Tokenize identifier (lowercase)"""
        lexer = IDRIS2Lexer("length")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "length"

    def test_data_keyword(self) -> None:
        """Tokenize data keyword"""
        lexer = IDRIS2Lexer("data")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.DATA

    def test_forall_keyword(self) -> None:
        """Tokenize forall keyword"""
        lexer = IDRIS2Lexer("forall")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.FORALL

    def test_arrow_operator(self) -> None:
        """Tokenize arrow operator"""
        lexer = IDRIS2Lexer("->")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.ARROW
        assert tokens[0].value == "->"

    def test_fat_arrow_operator(self) -> None:
        """Tokenize fat arrow operator"""
        lexer = IDRIS2Lexer("=>")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.FAT_ARROW
        assert tokens[0].value == "=>"

    def test_string_literal(self) -> None:
        """Tokenize string literal"""
        lexer = IDRIS2Lexer('"hello"')
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING
        assert tokens[0].value == "hello"

    def test_char_literal(self) -> None:
        """Tokenize character literal"""
        lexer = IDRIS2Lexer("'a'")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.CHAR
        assert tokens[0].value == "a"

    def test_line_comment(self) -> None:
        """Skip line comments"""
        lexer = IDRIS2Lexer("42 -- comment")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[1].type == TokenType.EOF

    def test_block_comment(self) -> None:
        """Skip block comments"""
        lexer = IDRIS2Lexer("42 {- comment -} 43")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "42"
        assert tokens[1].type == TokenType.NUMBER
        assert tokens[1].value == "43"

    def test_nested_block_comment(self) -> None:
        """Handle nested block comments"""
        lexer = IDRIS2Lexer("42 {- outer {- inner -} outer -} 43")
        tokens = lexer.tokenize()
        assert len([t for t in tokens if t.type == TokenType.NUMBER]) == 2

    def test_function_type_syntax(self) -> None:
        """Tokenize function type"""
        lexer = IDRIS2Lexer("Nat -> Int")
        tokens = lexer.tokenize()
        assert len(tokens) == 4  # Nat, ->, Int, EOF
        assert tokens[1].type == TokenType.ARROW

    def test_dependent_type_parentheses(self) -> None:
        """Tokenize dependent type syntax"""
        lexer = IDRIS2Lexer("(n : Nat)")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.LPAREN
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[2].type == TokenType.COLON
        assert tokens[3].type == TokenType.TYPE_NAME


class TestIDRIS2Parser:
    """Test IDRIS2 parser"""

    def test_parse_literal_number(self) -> None:
        """Parse number literal"""
        lexer = IDRIS2Lexer("42")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert module.name == "main"

    def test_parse_variable(self) -> None:
        """Parse variable reference"""
        lexer = IDRIS2Lexer("x")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert module.name == "main"

    def test_parse_function_type(self) -> None:
        """Parse function type"""
        lexer = IDRIS2Lexer("f : Nat -> Int")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert len(module.declarations) > 0

    def test_parse_dependent_type(self) -> None:
        """Parse dependent function type"""
        lexer = IDRIS2Lexer("(n : Nat) -> Vect n Int")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        type_expr = parser._parse_type()
        assert isinstance(type_expr, DependentType)

    def test_parse_type_family(self) -> None:
        """Parse type family"""
        lexer = IDRIS2Lexer("Vect 5 Int")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        type_expr = parser._parse_primary_type()
        assert isinstance(type_expr, TypeFamily)

    def test_parse_lambda(self) -> None:
        """Parse lambda expression"""
        lexer = IDRIS2Lexer("\\x => x")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        expr = parser._parse_primary_expr()
        assert isinstance(expr, Lambda)

    def test_parse_if_expression(self) -> None:
        """Parse if expression"""
        lexer = IDRIS2Lexer("if x then 1 else 0")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        expr = parser._parse_primary_expr()
        assert isinstance(expr, IfExpr)

    def test_parse_let_expression(self) -> None:
        """Parse let expression"""
        lexer = IDRIS2Lexer("let x = 5 in x")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        expr = parser._parse_primary_expr()
        assert isinstance(expr, LetExpr)

    def test_parse_case_expression(self) -> None:
        """Parse case expression"""
        lexer = IDRIS2Lexer("case n of Zero => 0 | Succ m => 1")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        expr = parser._parse_primary_expr()
        assert isinstance(expr, CaseExpr)

    def test_parse_data_declaration(self) -> None:
        """Parse data type declaration"""
        lexer = IDRIS2Lexer("data Nat : Type where Zero : Nat | Succ : Nat -> Nat")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert any(isinstance(d, DataDef) for d in module.declarations)

    def test_parse_type_definition(self) -> None:
        """Parse type synonym"""
        lexer = IDRIS2Lexer("type MyInt = Int")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert any(isinstance(d, TypeDef) for d in module.declarations)

    def test_parse_function_definition(self) -> None:
        """Parse function definition"""
        lexer = IDRIS2Lexer("f : Nat -> Nat = \\x => x")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        module = parser.parse()
        assert any(isinstance(d, FunctionDef) for d in module.declarations)

    def test_parse_var_pattern(self) -> None:
        """Parse variable pattern"""
        lexer = IDRIS2Lexer("x")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        pattern = parser._parse_pattern()
        assert isinstance(pattern, VarPattern)

    def test_parse_wildcard_pattern(self) -> None:
        """Parse wildcard pattern"""
        lexer = IDRIS2Lexer("_")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        pattern = parser._parse_pattern()
        assert isinstance(pattern, WildcardPattern)

    def test_parse_constructor_pattern(self) -> None:
        """Parse constructor pattern"""
        lexer = IDRIS2Lexer("Succ n")
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        pattern = parser._parse_pattern()
        assert isinstance(pattern, ConstructorPattern)


class TestIDRIS2TypeSystem:
    """Test IDRIS2 type system"""

    def test_infer_nat_literal(self) -> None:
        """Infer Nat type from literal"""
        ts = IDRISTypeSystem()
        type_ = ts.infer_type(Literal(5))
        assert isinstance(type_, BaseType)
        assert type_.name == "Nat"

    def test_infer_string_literal(self) -> None:
        """Infer String type from literal"""
        ts = IDRISTypeSystem()
        type_ = ts.infer_type(Literal("hello"))
        assert isinstance(type_, BaseType)
        assert type_.name == "String"

    def test_infer_variable_type(self) -> None:
        """Infer variable type from symbol table"""
        ts = IDRISTypeSystem()
        ts.register_symbol("x", BaseType("Int"))
        type_ = ts.infer_type(Var("x"))
        assert isinstance(type_, BaseType)
        assert type_.name == "Int"

    def test_unify_same_types(self) -> None:
        """Unify identical types"""
        ts = IDRISTypeSystem()
        unified = ts.unify(BaseType("Nat"), BaseType("Nat"))
        assert isinstance(unified, BaseType)
        assert unified.name == "Nat"

    def test_unify_with_type_variable(self) -> None:
        """Unify with type variable"""
        ts = IDRISTypeSystem()
        unified = ts.unify(TypeVariable("a"), BaseType("Int"))
        assert isinstance(unified, BaseType)
        assert unified.name == "Int"

    def test_types_equal_base_types(self) -> None:
        """Check equality of base types"""
        ts = IDRISTypeSystem()
        assert ts._types_equal(BaseType("Nat"), BaseType("Nat"))
        assert not ts._types_equal(BaseType("Nat"), BaseType("Int"))

    def test_types_equal_function_types(self) -> None:
        """Check equality of function types"""
        ts = IDRISTypeSystem()
        t1 = FunctionType(BaseType("Nat"), BaseType("Int"))
        t2 = FunctionType(BaseType("Nat"), BaseType("Int"))
        assert ts._types_equal(t1, t2)

    def test_types_equal_type_family(self) -> None:
        """Check equality of type families"""
        ts = IDRISTypeSystem()
        t1 = TypeFamily("Vect", [BaseType("Nat"), BaseType("Int")])
        t2 = TypeFamily("Vect", [BaseType("Nat"), BaseType("Int")])
        assert ts._types_equal(t1, t2)

    def test_dependent_type_parameter_count(self) -> None:
        """Create dependent type with parameters"""
        ts = IDRISTypeSystem()
        dep_type = DependentType("n", BaseType("Nat"), BaseType("Type"))
        assert dep_type.param_name == "n"
        assert isinstance(dep_type.param_type, BaseType)

    def test_register_data_type(self) -> None:
        """Register data type definition"""
        ts = IDRISTypeSystem()
        ts.register_data_type(
            "Nat",
            [],
            [("Zero", BaseType("Nat")), ("Succ", FunctionType(BaseType("Nat"), BaseType("Nat")))]
        )
        nat_sym = ts.lookup_symbol("Nat")
        assert nat_sym is not None

    def test_register_function(self) -> None:
        """Register function with type"""
        ts = IDRISTypeSystem()
        func_type = FunctionType(BaseType("Nat"), BaseType("Int"))
        ts.register_function("double", func_type)
        func_sym = ts.lookup_symbol("double")
        assert func_sym is not None

    def test_to_babbage_type_nat(self) -> None:
        """Map Nat to Babbage type"""
        ts = IDRISTypeSystem()
        babbage_type = ts.to_babbage_type(BaseType("Nat"))
        assert babbage_type == "i64"

    def test_to_babbage_type_string(self) -> None:
        """Map String to Babbage type"""
        ts = IDRISTypeSystem()
        babbage_type = ts.to_babbage_type(BaseType("String"))
        assert babbage_type == "ptr"

    def test_to_babbage_type_function(self) -> None:
        """Map function type to Babbage type"""
        ts = IDRISTypeSystem()
        func_type = FunctionType(BaseType("Nat"), BaseType("Int"))
        babbage_type = ts.to_babbage_type(func_type)
        assert babbage_type == "ptr"


class TestIDRIS2Compiler:
    """Test IDRIS2 compiler"""

    def test_compile_empty_source(self) -> None:
        """Compile empty source"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("")
        assert program is not None
        assert len(program.functions) > 0

    def test_compile_type_declaration(self) -> None:
        """Compile type declaration"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("f : Nat -> Int")
        assert program is not None

    def test_compile_data_declaration(self) -> None:
        """Compile data declaration"""
        compiler = IDRIS2Compiler()
        source = "data Bool : Type where True : Bool | False : Bool"
        program = compiler.compile(source)
        assert program is not None

    def test_compile_function_definition(self) -> None:
        """Compile function definition"""
        compiler = IDRIS2Compiler()
        source = "id : Nat -> Nat = \\x => x"
        program = compiler.compile(source)
        assert program is not None

    def test_compile_lambda_expression(self) -> None:
        """Compile lambda expression"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("\\x => x")
        assert program is not None

    def test_compile_if_expression(self) -> None:
        """Compile if expression"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("if True then 1 else 0")
        assert program is not None

    def test_compile_let_expression(self) -> None:
        """Compile let expression"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("let x = 5 in x")
        assert program is not None

    def test_compile_case_expression(self) -> None:
        """Compile case expression"""
        compiler = IDRIS2Compiler()
        source = "case Zero of Zero => 0 | Succ n => 1"
        program = compiler.compile(source)
        assert program is not None

    def test_compile_type_family(self) -> None:
        """Compile type family usage"""
        compiler = IDRIS2Compiler()
        source = """
        data Vect : Nat -> Type -> Type where
          Nil : Vect Zero a
          Cons : Vect n a -> a -> Vect (Succ n) a
        """
        program = compiler.compile(source)
        assert program is not None

    def test_compile_dependent_function(self) -> None:
        """Compile function with dependent type"""
        compiler = IDRIS2Compiler()
        source = "vecReplicate : (n : Nat) -> a -> Vect n a = \\n => \\x => x"
        program = compiler.compile(source)
        assert program is not None


class TestIDRIS2Integration:
    """Integration tests for full compilation pipeline"""

    def test_full_pipeline_simple(self) -> None:
        """Full pipeline for simple expression"""
        compiler = IDRIS2Compiler()
        program = compiler.compile("42")
        assert program is not None
        assert "main" in [fn.name for fn in program.functions]

    def test_full_pipeline_with_functions(self) -> None:
        """Full pipeline with functions"""
        source = """
        double : Nat -> Nat = \\x => x
        quadruple : Nat -> Nat = \\x => x
        """
        compiler = IDRIS2Compiler()
        program = compiler.compile(source)
        assert program is not None

    def test_full_pipeline_with_data_types(self) -> None:
        """Full pipeline with data types"""
        source = """
        data Color : Type where
          Red : Color
          Green : Color
          Blue : Color
        """
        compiler = IDRIS2Compiler()
        program = compiler.compile(source)
        assert program is not None

    def test_error_handling_syntax_error(self) -> None:
        """Handle syntax errors gracefully"""
        compiler = IDRIS2Compiler()
        with pytest.raises(SyntaxError):
            compiler.compile("(incomplete")

    def test_error_handling_undefined_symbol(self) -> None:
        """Handle undefined symbols"""
        compiler = IDRIS2Compiler()
        # Should compile but symbol reference will be unresolved
        program = compiler.compile("unknown_function")
        assert program is not None

    def test_compilation_speed(self) -> None:
        """Verify compilation is fast"""
        import time

        source = """
        data Nat : Type where
          Zero : Nat
          Succ : Nat -> Nat

        fact : Nat -> Nat = \\n => n

        double : Nat -> Nat = \\x => x
        """

        start = time.time()
        compiler = IDRIS2Compiler()
        program = compiler.compile(source)
        elapsed = time.time() - start

        # Should compile in < 200ms
        assert elapsed < 0.2
        assert program is not None

    def test_verbose_compilation(self) -> None:
        """Test verbose output"""
        compiler = IDRIS2Compiler(verbose=True)
        program = compiler.compile("f : Nat -> Nat")
        assert program is not None

    def test_type_family_instantiation(self) -> None:
        """Test type family instantiation"""
        source = """
        data Vect : Nat -> Type -> Type where
          Nil : Vect Zero a
          Cons : Vect n a

        myVec : Vect 5 Int = Nil
        """
        compiler = IDRIS2Compiler()
        program = compiler.compile(source)
        assert program is not None

    def test_pattern_matching_exhaustiveness(self) -> None:
        """Test pattern matching compilation"""
        source = """
        match_nat : Nat -> Int =
          case n of
            Zero => 0
            Succ m => 1
        """
        compiler = IDRIS2Compiler()
        program = compiler.compile(source)
        assert program is not None
