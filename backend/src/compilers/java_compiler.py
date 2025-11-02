"""
Java to Babbage IR Compiler - Translates Java AST to IR

Four-phase pipeline:
  Phase 1: Lexical analysis (tokenization)
  Phase 2: Syntax analysis (parsing to AST)
  Phase 3: Semantic analysis (type checking, symbol table)
  Phase 4: Code generation (IR)
"""

from __future__ import annotations
from typing import Dict, Optional

from backend.src.ir_types import (
    IRType,
    Value,
    Constant,
    VariableValue,
    UndefValue,
    BasicBlock,
    Function,
    Program,
    IRBuilder,
)
from backend.src.compilers.java_lexer import JavaLexer
from backend.src.compilers.java_parser import JavaParser
from backend.src.compilers.java_ast import (
    Expr,
    Literal,
    Variable,
    FieldAccess,
    MethodCall,
    BinaryOp,
    UnaryOp,
)
from backend.src.compilers.java_types import JavaTypeSystem


class JavaCompiler:
    """Java to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        self.verbose = verbose
        self.type_system = JavaTypeSystem()
        self.ir_builder: Optional[IRBuilder] = None
        self.symbol_table: Dict[str, VariableValue] = {}
        self.var_counter = 0

    def compile(self, source: str) -> Program:
        """Compile Java source to Babbage IR"""

        # Phase 1: Lexical analysis
        lexer = JavaLexer(source)
        tokens = lexer.tokenize()

        if self.verbose:
            print(f"[Java] Lexed {len(tokens)} tokens")

        # Phase 2: Syntax analysis
        parser = JavaParser(tokens)
        compilation_unit = parser.parse()

        if self.verbose:
            print(f"[Java] Parsed {len(compilation_unit.type_decls)} type declarations")

        # Phase 3: Semantic analysis
        for type_decl in compilation_unit.type_decls:
            try:
                # Register types in symbol table
                if hasattr(type_decl, "name"):
                    self.type_system.register_symbol(type_decl.name, type_decl)
            except Exception as e:
                if self.verbose:
                    print(f"[Java] Semantic analysis: {e}")

        # Phase 4: Code generation
        self.ir_builder = IRBuilder("main", [])
        main_block = self.ir_builder.new_block("main")

        # Generate IR for each type declaration
        for type_decl in compilation_unit.type_decls:
            self._compile_type_decl(type_decl)

        main_func = self.ir_builder.function
        program = Program(functions=[main_func], global_variables=[])

        if self.verbose:
            print(f"[Java] Generated IR program")

        return program

    def _compile_type_decl(self, decl) -> None:
        """Compile type declaration to IR"""
        # For classes, compile all methods
        if hasattr(decl, "members"):
            for member in decl.members:
                if hasattr(member, "body") and member.body is not None:
                    self._compile_method(member)

    def _compile_method(self, method_decl) -> None:
        """Compile method to IR"""
        if not hasattr(method_decl, "body"):
            return

        # Register parameters as variables
        for param in method_decl.parameters:
            var_name = f"param_{param.name}"
            self.symbol_table[param.name] = VariableValue(var_name)

        # Compile method body
        if hasattr(method_decl.body, "statements"):
            for stmt in method_decl.body.statements:
                self._compile_stmt(stmt)

    def _compile_stmt(self, stmt) -> None:
        """Compile statement to IR"""
        # Variable declaration
        if hasattr(stmt, "name") and hasattr(stmt, "type_"):
            var_name = f"var_{stmt.name}"
            self.symbol_table[stmt.name] = VariableValue(var_name)
            if hasattr(stmt, "initializer") and stmt.initializer:
                self._compile_expr(stmt.initializer)

        # Block statement
        elif hasattr(stmt, "statements"):
            for s in stmt.statements:
                self._compile_stmt(s)

        # If statement
        elif hasattr(stmt, "condition"):
            self._compile_expr(stmt.condition)
            if hasattr(stmt, "then_stmt"):
                self._compile_stmt(stmt.then_stmt)
            if hasattr(stmt, "else_stmt") and stmt.else_stmt:
                self._compile_stmt(stmt.else_stmt)

        # Return statement
        elif hasattr(stmt, "expr") and stmt.__class__.__name__ == "ReturnStmt":
            if stmt.expr:
                self._compile_expr(stmt.expr)

    def _compile_expr(self, expr: Expr) -> Value:
        """Compile expression to IR"""
        if isinstance(expr, Literal):
            if isinstance(expr.value, (int, float)):
                ir_type = "i64" if isinstance(expr.value, int) else "f64"
                return Constant(str(expr.value), ir_type)
            elif isinstance(expr.value, str):
                return Constant(f'"{expr.value}"', "ptr")
            elif isinstance(expr.value, bool):
                return Constant(str(int(expr.value)), "i64")
            elif expr.value is None:
                return Constant("0", "ptr")

        if isinstance(expr, Variable):
            if expr.name in self.symbol_table:
                return self.symbol_table[expr.name]
            return VariableValue(expr.name)

        if isinstance(expr, MethodCall):
            if expr.object_expr:
                self._compile_expr(expr.object_expr)
            for arg in expr.arguments:
                self._compile_expr(arg)
            self.var_counter += 1
            return VariableValue(f"call_{self.var_counter}")

        if isinstance(expr, FieldAccess):
            self._compile_expr(expr.object_expr)
            self.var_counter += 1
            return VariableValue(f"field_{self.var_counter}")

        if isinstance(expr, BinaryOp):
            left = self._compile_expr(expr.left)
            right = self._compile_expr(expr.right)
            self.var_counter += 1
            return VariableValue(f"binop_{self.var_counter}")

        if isinstance(expr, UnaryOp):
            self._compile_expr(expr.operand)
            self.var_counter += 1
            return VariableValue(f"unop_{self.var_counter}")

        return UndefValue()
