"""
System F to Babbage IR Compiler - Translates System F to language-agnostic IR
"""

from __future__ import annotations
from typing import Dict, Optional

from backend.src.ir_types import (
    IRType, Value, Constant, VariableValue, UndefValue,
    BasicBlock, Function, Program, IRBuilder
)
from backend.src.compilers.systemf_lexer import SystemFLexer
from backend.src.compilers.systemf_parser import SystemFParser
from backend.src.compilers.systemf_ast import (
    Type, Expr, Var, Literal, Lambda, TypeAbstraction, Application, TypeApplication,
    IfExpr, LetExpr, FixExpr, Annotation
)
from backend.src.compilers.systemf_types import SystemFTypeSystem


class SystemFCompiler:
    """System F to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        self.verbose = verbose
        self.type_system = SystemFTypeSystem()
        self.ir_builder: Optional[IRBuilder] = None
        self.var_counter = 0

    def compile(self, source: str) -> Program:
        """Compile System F source to Babbage IR"""

        # Phase 1: Lex
        lexer = SystemFLexer(source)
        tokens = lexer.tokenize()

        if self.verbose:
            print(f"[SystemF] Lexed {len(tokens)} tokens")

        # Phase 2: Parse
        parser = SystemFParser(tokens)
        exprs = parser.parse()

        if self.verbose:
            print(f"[SystemF] Parsed {len(exprs)} expression(s)")

        # Phase 3: Type checking
        for expr in exprs:
            try:
                self.type_system.infer_type(expr)
            except Exception as e:
                if self.verbose:
                    print(f"[SystemF] Type check: {e}")

        # Phase 4: IR Generation
        self.ir_builder = IRBuilder("main", [])
        main_block = self.ir_builder.new_block("main")

        for expr in exprs:
            self._compile_expr(expr)

        main_func = self.ir_builder.function

        program = Program(functions=[main_func], global_variables=[])

        if self.verbose:
            print(f"[SystemF] Generated IR program")

        return program

    def _compile_expr(self, expr: Expr) -> Value:
        """Compile expression to IR"""
        if isinstance(expr, Literal):
            if isinstance(expr.value, (int, float)):
                ir_type = "i64" if isinstance(expr.value, int) else "f64"
                return Constant(str(expr.value), ir_type)
            elif isinstance(expr.value, str):
                return Constant(f'"{expr.value}"', "ptr")

        if isinstance(expr, Var):
            return VariableValue(expr.name)

        if isinstance(expr, Lambda):
            # Simplified: lambda returns function pointer
            return VariableValue(f"lambda_{self.var_counter}")

        if isinstance(expr, TypeAbstraction):
            # Type abstraction erased at runtime
            return self._compile_expr_dummy()

        if isinstance(expr, Application):
            func_val = self._compile_expr(expr.func)
            arg_val = self._compile_expr(expr.arg)
            return VariableValue(f"app_{self.var_counter}")

        if isinstance(expr, TypeApplication):
            # Type application erased at runtime
            return self._compile_expr(expr.expr)

        if isinstance(expr, IfExpr):
            return self._compile_expr(expr.then_expr)

        if isinstance(expr, LetExpr):
            return self._compile_expr(expr.body_expr)

        if isinstance(expr, FixExpr):
            return self._compile_expr(expr.func)

        if isinstance(expr, Annotation):
            return self._compile_expr(expr.expr)

        return UndefValue()

    def _compile_expr_dummy(self) -> Value:
        """Generate dummy value for type-erased expressions"""
        self.var_counter += 1
        return VariableValue(f"tmp_{self.var_counter}")
