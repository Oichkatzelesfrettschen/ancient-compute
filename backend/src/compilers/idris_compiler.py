# Ancient Compute - IDRIS Compiler

from .idris_ast import (
    Module, Import, TypeDeclaration, FunctionDeclaration, Identifier, Literal, FunctionApplication, Let, Case
)
from ..ir_types import IRBuilder, Program, Function, BasicBlock, Constant, ReturnTerminator, IRType, VariableValue

class IdrisCompiler:
    def __init__(self):
        self.program = Program()
        self.builder: IRBuilder = None

    def compile(self, ast) -> Program:
        if isinstance(ast, Module):
            self._compile_module(ast)
        return self.program

    def _compile_module(self, module: Module):
        for decl in module.body:
            if isinstance(decl, FunctionDeclaration):
                self._compile_function_declaration(decl)

    def _compile_function_declaration(self, decl: FunctionDeclaration):
        self.builder = IRBuilder(decl.name, decl.args)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in decl.args}
        self.builder.new_block('entry')

        return_val = self._compile_expression(decl.body)

        self.builder.emit_return(return_val)
        self.program.add_function(self.builder.finalize())

    def _compile_expression(self, expr):
        if isinstance(expr, Identifier):
            return VariableValue(expr.name)
        elif isinstance(expr, Literal):
            return Constant(expr.value)
        elif isinstance(expr, FunctionApplication):
            return self._compile_function_application(expr)
        else:
            # STUB: Placeholder for other expression types
            return Constant(0)

    def _compile_function_application(self, app: FunctionApplication):
        func_name = app.func.name
        args = [self._compile_expression(arg) for arg in app.args]

        target = self.builder.function.name + "_tmp"
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)


class IDRIS2Compiler(IdrisCompiler):
    """Compatibility alias exposing expected class name in tests."""
    pass
