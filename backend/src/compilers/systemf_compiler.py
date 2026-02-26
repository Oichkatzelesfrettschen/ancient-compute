"""
System F to Babbage IR Compiler - Translates System F to language-agnostic IR
"""

from __future__ import annotations

from backend.src.compilers.systemf_ast import (
    Application,
    Expr,
    IfExpr,
    Lambda,
    LetExpr,
    Literal,
    TypeAbstraction,
    TypeApplication,
    Var,
)
from backend.src.compilers.systemf_lexer import SystemFLexer
from backend.src.compilers.systemf_parser import SystemFParser
from backend.src.compilers.systemf_types import SystemFTypeSystem
from backend.src.ir_types import (
    BranchTerminator,
    Constant,
    IRBuilder,
    IRType,
    Program,
    UndefValue,
    Value,
    VariableValue,
)


class SystemFCompiler:
    """System F to Babbage IR compiler"""

    def __init__(self, verbose: bool = False) -> None:
        self.verbose = verbose
        self.type_system = SystemFTypeSystem()
        self.ir_builder: IRBuilder | None = None
        self.var_counter = 0
        self.program = Program()
        self.symbol_table: dict[str, VariableValue] = {}

    def compile(self, source: str) -> Program:
        """Compile System F source to Babbage IR"""

        # Phase 1: Lex
        lexer = SystemFLexer(source)
        tokens = lexer.tokenize()

        # Phase 2: Parse
        parser = SystemFParser(tokens)
        exprs = parser.parse()

        # Phase 4: IR Generation
        # For simplicity, we treat the sequence of expressions as a main function body
        self.ir_builder = IRBuilder("main", [])
        self.ir_builder.new_block("entry")

        last_val = Constant(0)
        for expr in exprs:
            last_val = self._compile_expr(expr)

        if not self.ir_builder.current_block.terminator:
            self.ir_builder.emit_return(last_val)

        self.program.add_function(self.ir_builder.finalize())

        return self.program

    def _compile_expr(self, expr: Expr) -> Value:
        """Compile expression to IR"""
        if isinstance(expr, Literal):
            return Constant(expr.value)

        if isinstance(expr, Var):
            return self.symbol_table.get(expr.name, VariableValue(expr.name))

        if isinstance(expr, Lambda):
            # Lift lambda to global function
            func_name = self._new_tmp("lambda")
            params = [expr.param_name]

            # Save current builder
            old_builder = self.ir_builder
            old_symbols = self.symbol_table.copy()

            self.ir_builder = IRBuilder(func_name, params)
            self.ir_builder.function.local_variables = {expr.param_name: IRType.DEC50}
            self.ir_builder.new_block("entry")
            self.symbol_table = {expr.param_name: VariableValue(expr.param_name)}

            body_val = self._compile_expr(expr.body)
            self.ir_builder.emit_return(body_val)
            self.program.add_function(self.ir_builder.finalize())

            # Restore builder
            self.ir_builder = old_builder
            self.symbol_table = old_symbols

            # Return function reference (simplified)
            return Constant(func_name, IRType.PTR)

        if isinstance(expr, TypeAbstraction):
            # Type abstraction erased at runtime
            return self._compile_expr(expr.body)

        if isinstance(expr, Application):
            func_val = self._compile_expr(expr.func)
            arg_val = self._compile_expr(expr.arg)
            target = self._new_tmp("app")

            # Simplified: call assuming func_val is a function pointer/name
            # If func_val is a Constant containing the name:
            if isinstance(func_val, Constant):
                self.ir_builder.emit_call(str(func_val.value), [arg_val], target)
            else:
                # Dynamic call not fully supported in IRBuilder yet, assume static for now
                self.ir_builder.emit_call("dynamic_call", [func_val, arg_val], target)

            return VariableValue(target)

        if isinstance(expr, TypeApplication):
            # Type application erased at runtime
            return self._compile_expr(expr.expr)

        if isinstance(expr, IfExpr):
            cond_val = self._compile_expr(expr.condition)

            result_var = self._new_tmp("if_res")
            self.ir_builder.function.local_variables[result_var] = IRType.DEC50

            then_label = self._new_tmp("then")
            else_label = self._new_tmp("else")
            merge_label = self._new_tmp("merge")

            # Save current block to emit branch from it
            cond_block = self.ir_builder.current_block

            # Build then block
            then_block = self.ir_builder.new_block(then_label)
            then_val = self._compile_expr(expr.then_expr)
            self.ir_builder.emit_assignment(result_var, then_val)
            self.ir_builder.emit_jump(merge_label)

            # Build else block
            else_block = self.ir_builder.new_block(else_label)
            else_val = self._compile_expr(expr.else_expr)
            self.ir_builder.emit_assignment(result_var, else_val)
            self.ir_builder.emit_jump(merge_label)

            # Emit branch on condition block
            cond_block.terminator = BranchTerminator(
                "ne", cond_val, Constant(0), then_label, else_label
            )

            # Merge block
            self.ir_builder.new_block(merge_label)
            return VariableValue(result_var)

        if isinstance(expr, LetExpr):
            val = self._compile_expr(expr.value_expr)
            self.ir_builder.function.local_variables[expr.var_name] = IRType.DEC50
            self.ir_builder.emit_assignment(expr.var_name, val)
            self.symbol_table[expr.var_name] = VariableValue(expr.var_name)
            return self._compile_expr(expr.body_expr)

        return UndefValue()

    def _new_tmp(self, prefix: str = "tmp") -> str:
        self.var_counter += 1
        return f"{prefix}_{self.var_counter}"
