# Ancient Compute - IDRIS Compiler

from .idris_ast import (
    Module, Import, TypeDeclaration, FunctionDeclaration, Identifier, Literal, FunctionApplication, Let, Case
)
from ..ir_types import IRBuilder, Program, Function, BasicBlock, Constant, ReturnTerminator, IRType, VariableValue

class IdrisCompiler:
    def __init__(self):
        self.program = Program()
        self.builder: IRBuilder = None
        self.tmp_counter = 0

    def compile(self, ast) -> Program:
        if isinstance(ast, Module):
            self._compile_module(ast)
        elif isinstance(ast, list):
            for decl in ast:
                self._compile_decl(decl)
        return self.program

    def _compile_module(self, module: Module):
        for decl in module.body:
            self._compile_decl(decl)

    def _compile_decl(self, decl):
        if isinstance(decl, FunctionDeclaration):
            self._compile_function_declaration(decl)
        elif isinstance(decl, TypeDeclaration):
            # Erase type declarations at runtime
            pass

    def _compile_function_declaration(self, decl: FunctionDeclaration):
        self.builder = IRBuilder(decl.name, decl.args)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in decl.args}
        self.builder.new_block('entry')

        return_val = self._compile_expression(decl.body)

        if not self.builder.current_block.terminator:
            self.builder.emit_return(return_val)
        self.program.add_function(self.builder.finalize())

    def _compile_expression(self, expr):
        if isinstance(expr, Identifier):
            return VariableValue(expr.name)
        elif isinstance(expr, Literal):
            return Constant(expr.value)
        elif isinstance(expr, FunctionApplication):
            return self._compile_function_application(expr)
        elif isinstance(expr, Let):
            return self._compile_let(expr)
        else:
            # STUB: Placeholder for other expression types
            return Constant(0)

    def _compile_function_application(self, app: FunctionApplication):
        func_name = app.func.name if isinstance(app.func, Identifier) else "dynamic_call"
        args = [self._compile_expression(arg) for arg in app.args]

        target = self._new_tmp()
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)

    def _compile_let(self, let_expr: Let):
        for binding in let_expr.bindings:
            if isinstance(binding, FunctionDeclaration):
                # Simplified: handle as variable assignment if no args
                val = self._compile_expression(binding.body)
                self.builder.function.local_variables[binding.name] = IRType.DEC50
                self.builder.emit_assignment(binding.name, val)
        
        return self._compile_expression(let_expr.body)

    def _new_tmp(self):
        self.tmp_counter += 1
        return f"t{self.tmp_counter}"


class IDRIS2Compiler(IdrisCompiler):
    """Compatibility alias exposing expected class name in tests."""
    pass