"""
Java Compiler Test Suite

Comprehensive tests for all compilation phases:
  - Lexical analysis (tokenization)
  - Syntax analysis (parsing to AST)
  - Semantic analysis (type checking)
  - Code generation (IR)
"""

import pytest
from backend.src.compilers.java_lexer import JavaLexer, TokenType
from backend.src.compilers.java_parser import JavaParser
from backend.src.compilers.java_ast import (
    PrimitiveType, ReferenceType, ArrayType, Variable, Literal,
    MethodCall, BinaryOp, MethodDecl, ClassDecl, CompilationUnit
)
from backend.src.compilers.java_compiler import JavaCompiler
from backend.src.compilers.java_types import JavaTypeSystem


# ==============================================================================
# LEXER TESTS
# ==============================================================================

class TestJavaLexer:
    """Test Java lexer tokenization"""

    def test_lex_simple_literal(self) -> None:
        lexer = JavaLexer("42")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "42"

    def test_lex_float_literal(self) -> None:
        lexer = JavaLexer("3.14")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NUMBER
        assert tokens[0].value == "3.14"

    def test_lex_string_literal(self) -> None:
        lexer = JavaLexer('"hello"')
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING_LIT
        assert tokens[0].value == "hello"

    def test_lex_char_literal(self) -> None:
        lexer = JavaLexer("'a'")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.CHAR_LIT
        assert tokens[0].value == "a"

    def test_lex_boolean_true(self) -> None:
        lexer = JavaLexer("true")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.BOOLEAN

    def test_lex_boolean_false(self) -> None:
        lexer = JavaLexer("false")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.BOOLEAN

    def test_lex_null(self) -> None:
        lexer = JavaLexer("null")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NULL

    def test_lex_keyword_class(self) -> None:
        lexer = JavaLexer("class")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.CLASS

    def test_lex_keyword_public(self) -> None:
        lexer = JavaLexer("public")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.PUBLIC

    def test_lex_keyword_static(self) -> None:
        lexer = JavaLexer("static")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STATIC

    def test_lex_keyword_void(self) -> None:
        lexer = JavaLexer("void")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.VOID

    def test_lex_keyword_int(self) -> None:
        lexer = JavaLexer("int")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.INT

    def test_lex_keyword_if(self) -> None:
        lexer = JavaLexer("if")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IF

    def test_lex_keyword_else(self) -> None:
        lexer = JavaLexer("else")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.ELSE

    def test_lex_identifier(self) -> None:
        lexer = JavaLexer("myVariable")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.IDENTIFIER
        assert tokens[0].value == "myVariable"

    def test_lex_type_name(self) -> None:
        lexer = JavaLexer("String")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.STRING

    def test_lex_plus_operator(self) -> None:
        lexer = JavaLexer("+")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.PLUS

    def test_lex_equal_equal(self) -> None:
        lexer = JavaLexer("==")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.EQUAL_EQUAL

    def test_lex_not_equal(self) -> None:
        lexer = JavaLexer("!=")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.NOT_EQUAL

    def test_lex_less_equal(self) -> None:
        lexer = JavaLexer("<=")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.LESS_EQUAL

    def test_lex_and_operator(self) -> None:
        lexer = JavaLexer("&&")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.AND

    def test_lex_or_operator(self) -> None:
        lexer = JavaLexer("||")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.OR

    def test_lex_increment(self) -> None:
        lexer = JavaLexer("++")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.INCREMENT

    def test_lex_decrement(self) -> None:
        lexer = JavaLexer("--")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.DECREMENT

    def test_lex_left_paren(self) -> None:
        lexer = JavaLexer("(")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.LPAREN

    def test_lex_right_paren(self) -> None:
        lexer = JavaLexer(")")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.RPAREN

    def test_lex_left_brace(self) -> None:
        lexer = JavaLexer("{")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.LBRACE

    def test_lex_right_brace(self) -> None:
        lexer = JavaLexer("}")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.RBRACE

    def test_lex_semicolon(self) -> None:
        lexer = JavaLexer(";")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.SEMICOLON

    def test_lex_dot(self) -> None:
        lexer = JavaLexer(".")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.DOT

    def test_lex_line_comment(self) -> None:
        lexer = JavaLexer("// comment\nint x;")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.INT
        assert tokens[0].value == "int"

    def test_lex_block_comment(self) -> None:
        lexer = JavaLexer("/* comment */ int x;")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.INT
        assert tokens[0].value == "int"

    def test_lex_complex_expression(self) -> None:
        lexer = JavaLexer("int x = 5 + 3;")
        tokens = lexer.tokenize()
        assert tokens[0].type == TokenType.INT
        assert tokens[1].type == TokenType.IDENTIFIER
        assert tokens[2].type == TokenType.EQUAL
        assert tokens[3].type == TokenType.NUMBER
        assert tokens[4].type == TokenType.PLUS
        assert tokens[5].type == TokenType.NUMBER
        assert tokens[6].type == TokenType.SEMICOLON


# ==============================================================================
# PARSER TESTS
# ==============================================================================

class TestJavaParser:
    """Test Java parser AST construction"""

    def test_parse_simple_class(self) -> None:
        source = "class HelloWorld {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1
        assert isinstance(cu.type_decls[0], ClassDecl)
        assert cu.type_decls[0].name == "HelloWorld"

    def test_parse_class_with_method(self) -> None:
        source = "class Test { public void method() {} }"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1
        cls = cu.type_decls[0]
        assert isinstance(cls, ClassDecl)
        assert len(cls.members) > 0

    def test_parse_literal_integer(self) -> None:
        source = "class T { int x = 42; }"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert cu.type_decls[0].name == "T"

    def test_parse_literal_string(self) -> None:
        source = 'class T { String s = "hello"; }'
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert cu.type_decls[0].name == "T"

    def test_parse_if_statement(self) -> None:
        source = """
        class T {
            public void test() {
                if (x > 0) {
                    y = 1;
                }
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_while_loop(self) -> None:
        source = """
        class T {
            public void test() {
                while (x < 10) {
                    x++;
                }
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_for_loop(self) -> None:
        source = """
        class T {
            public void test() {
                for (int i = 0; i < 10; i++) {
                    System.out.println(i);
                }
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_method_call(self) -> None:
        source = """
        class T {
            public void test() {
                System.out.println("hello");
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_package_declaration(self) -> None:
        source = "package com.example; class T {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert cu.package_decl is not None
        assert cu.package_decl.name == "com.example"

    def test_parse_import_statement(self) -> None:
        source = "import java.util.*; class T {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.import_decls) == 1

    def test_parse_field_declaration(self) -> None:
        source = "class T { private int count; }"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_public_modifier(self) -> None:
        source = "public class T {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        cls = cu.type_decls[0]
        assert "public" in cls.modifiers

    def test_parse_static_modifier(self) -> None:
        source = "class T { public static void main() {} }"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_final_modifier(self) -> None:
        source = "class T { final int x = 5; }"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_generic_type(self) -> None:
        source = "class T<X> {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_extends_clause(self) -> None:
        source = "class T extends Base {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        cls = cu.type_decls[0]
        assert cls.superclass is not None

    def test_parse_implements_clause(self) -> None:
        source = "class T implements Comparable {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        cls = cu.type_decls[0]
        assert len(cls.interfaces) > 0

    def test_parse_return_statement(self) -> None:
        source = """
        class T {
            public int getValue() {
                return 42;
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

    def test_parse_new_expression(self) -> None:
        source = """
        class T {
            public void test() {
                Object o = new Object();
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1


# ==============================================================================
# TYPE SYSTEM TESTS
# ==============================================================================

class TestJavaTypeSystem:
    """Test Java type system"""

    def test_infer_integer_literal(self) -> None:
        ts = JavaTypeSystem()
        lit = Literal(42)
        type_ = ts.infer_type(lit)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "int"

    def test_infer_float_literal(self) -> None:
        ts = JavaTypeSystem()
        lit = Literal(3.14)
        type_ = ts.infer_type(lit)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "double"

    def test_infer_string_literal(self) -> None:
        ts = JavaTypeSystem()
        lit = Literal("hello")
        type_ = ts.infer_type(lit)
        assert isinstance(type_, ReferenceType)
        assert type_.name == "String"

    def test_infer_boolean_literal(self) -> None:
        ts = JavaTypeSystem()
        lit = Literal(True)
        type_ = ts.infer_type(lit)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "boolean"

    def test_infer_null_literal(self) -> None:
        ts = JavaTypeSystem()
        lit = Literal(None)
        type_ = ts.infer_type(lit)
        assert isinstance(type_, ReferenceType)
        assert type_.name == "null"

    def test_infer_variable(self) -> None:
        ts = JavaTypeSystem()
        ts.register_symbol("x", PrimitiveType("int"))
        var = Variable("x")
        type_ = ts.infer_type(var)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "int"

    def test_infer_this_reference(self) -> None:
        ts = JavaTypeSystem()
        var = Variable("this")
        type_ = ts.infer_type(var)
        assert isinstance(type_, ReferenceType)

    def test_infer_super_reference(self) -> None:
        ts = JavaTypeSystem()
        var = Variable("super")
        type_ = ts.infer_type(var)
        assert isinstance(type_, ReferenceType)

    def test_infer_addition_int_int(self) -> None:
        ts = JavaTypeSystem()
        left = Literal(1)
        right = Literal(2)
        op = BinaryOp("+", left, right)
        type_ = ts.infer_type(op)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "int"

    def test_infer_comparison_less(self) -> None:
        ts = JavaTypeSystem()
        left = Literal(1)
        right = Literal(2)
        op = BinaryOp("<", left, right)
        type_ = ts.infer_type(op)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "boolean"

    def test_infer_logical_and(self) -> None:
        ts = JavaTypeSystem()
        left = Literal(True)
        right = Literal(False)
        op = BinaryOp("&&", left, right)
        type_ = ts.infer_type(op)
        assert isinstance(type_, PrimitiveType)
        assert type_.name == "boolean"

    def test_type_assignability_same_type(self) -> None:
        ts = JavaTypeSystem()
        type1 = PrimitiveType("int")
        type2 = PrimitiveType("int")
        assert ts.is_assignable_to(type1, type2)

    def test_type_assignability_int_to_long(self) -> None:
        ts = JavaTypeSystem()
        from_type = PrimitiveType("int")
        to_type = PrimitiveType("long")
        assert ts.is_assignable_to(from_type, to_type)

    def test_type_assignability_long_to_int_fails(self) -> None:
        ts = JavaTypeSystem()
        from_type = PrimitiveType("long")
        to_type = PrimitiveType("int")
        assert not ts.is_assignable_to(from_type, to_type)

    def test_type_assignability_null_to_reference(self) -> None:
        ts = JavaTypeSystem()
        from_type = ReferenceType("null")
        to_type = ReferenceType("String")
        assert ts.is_assignable_to(from_type, to_type)

    def test_type_assignability_to_object(self) -> None:
        ts = JavaTypeSystem()
        from_type = ReferenceType("String")
        to_type = ReferenceType("Object")
        assert ts.is_assignable_to(from_type, to_type)

    def test_to_babbage_type_int(self) -> None:
        ts = JavaTypeSystem()
        type_ = PrimitiveType("int")
        babbage_type = ts.to_babbage_type(type_)
        assert babbage_type == "i64"

    def test_to_babbage_type_double(self) -> None:
        ts = JavaTypeSystem()
        type_ = PrimitiveType("double")
        babbage_type = ts.to_babbage_type(type_)
        assert babbage_type == "f64"

    def test_to_babbage_type_string(self) -> None:
        ts = JavaTypeSystem()
        type_ = ReferenceType("String")
        babbage_type = ts.to_babbage_type(type_)
        assert babbage_type == "ptr"

    def test_to_babbage_type_array(self) -> None:
        ts = JavaTypeSystem()
        type_ = ArrayType(PrimitiveType("int"))
        babbage_type = ts.to_babbage_type(type_)
        assert babbage_type == "ptr"

    def test_numeric_type_promotion(self) -> None:
        ts = JavaTypeSystem()
        int_type = PrimitiveType("int")
        double_type = PrimitiveType("double")
        promoted = ts._promote_numeric_type(int_type, double_type)
        assert promoted.name == "double"


# ==============================================================================
# COMPILER TESTS
# ==============================================================================

class TestJavaCompiler:
    """Test Java compiler IR generation"""

    def test_compile_empty_class(self) -> None:
        source = "class T {}"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None
        assert len(program.functions) > 0

    def test_compile_class_with_field(self) -> None:
        source = "class T { int x; }"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_class_with_method(self) -> None:
        source = "class T { public void test() {} }"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_method_with_statement(self) -> None:
        source = """
        class T {
            public void test() {
                int x = 5;
            }
        }
        """
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_method_with_return(self) -> None:
        source = """
        class T {
            public int getValue() {
                return 42;
            }
        }
        """
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_multiple_classes(self) -> None:
        source = "class A {} class B {}"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_with_verbose_output(self) -> None:
        source = "class T {}"
        compiler = JavaCompiler(verbose=True)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_inheritance(self) -> None:
        source = "class T extends Base {}"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_compile_interface_implementation(self) -> None:
        source = "class T implements Runnable {}"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        assert program is not None

    def test_ir_code_generation(self) -> None:
        source = "class T {}"
        compiler = JavaCompiler(verbose=False)
        program = compiler.compile(source)
        ir_code = compiler._generate_ir_code(program)
        assert "; Java to Babbage IR" in ir_code
        assert "@function" in ir_code


# ==============================================================================
# INTEGRATION TESTS
# ==============================================================================

class TestJavaIntegration:
    """Integration tests for complete Java compilation"""

    def test_compile_and_validate_simple_class(self) -> None:
        source = "class HelloWorld {}"
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        assert len(tokens) > 0

        parser = JavaParser(tokens)
        cu = parser.parse()
        assert cu.type_decls[0].name == "HelloWorld"

        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_and_validate_method(self) -> None:
        source = """
        class Test {
            public int calculate(int x, int y) {
                return x + y;
            }
        }
        """
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()

        parser = JavaParser(tokens)
        cu = parser.parse()
        assert len(cu.type_decls) == 1

        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_and_validate_control_flow(self) -> None:
        source = """
        class Flow {
            public void test() {
                if (x > 0) {
                    y = 1;
                } else {
                    y = 0;
                }
            }
        }
        """
        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_and_validate_loops(self) -> None:
        source = """
        class Loops {
            public void test() {
                for (int i = 0; i < 10; i++) {
                    System.out.println(i);
                }
            }
        }
        """
        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_and_validate_arrays(self) -> None:
        source = """
        class Arrays {
            public void test() {
                int[] arr = new int[10];
                arr[0] = 42;
            }
        }
        """
        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_compile_and_validate_generics(self) -> None:
        source = """
        class Generic<T> {
            private T value;
        }
        """
        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None

    def test_full_pipeline_from_source_to_ir(self) -> None:
        source = """
        public class Main {
            public static void main() {
                System.out.println("Hello, World!");
            }
        }
        """
        # Phase 1: Lex
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()
        assert len(tokens) > 0

        # Phase 2: Parse
        parser = JavaParser(tokens)
        cu = parser.parse()
        assert cu.type_decls[0].name == "Main"

        # Phase 3: Type check
        ts = JavaTypeSystem()
        ts.register_symbol("Main", cu.type_decls[0])

        # Phase 4: Compile to IR
        compiler = JavaCompiler()
        program = compiler.compile(source)
        assert program is not None
        ir_code = compiler._generate_ir_code(program)
        assert len(ir_code) > 0
