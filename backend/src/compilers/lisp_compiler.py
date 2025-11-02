# Ancient Compute - LISP Compiler

from .lisp_ast import ASTNode, SExpression, Symbol, Number, String
from ..ir_types import (
    IRBuilder,
    Program,
    Function,
    BasicBlock,
    Constant,
    ReturnTerminator,
    IRType,
    VariableValue,
    BranchTerminator,
)


class LispCompiler:
    def __init__(self):
        self.program = Program()
        self.builder: IRBuilder = None

    def compile(self, ast: ASTNode) -> Program:
        if isinstance(ast, SExpression):
            self._compile_sexpression(ast)
        else:
            self._compile_atom(ast)
        return self.program

    def _compile_sexpression(self, sexp: SExpression):
        if not sexp.children:
            return None

        first = sexp.children[0]
        if isinstance(first, Symbol):
            if first.value == "defun":
                return self._compile_defun(sexp)
            elif first.value == "if":
                return self._compile_if(sexp)
            elif first.value in ["+", "-", "*", "/"]:
                return self._compile_arithmetic(sexp)
            else:
                return self._compile_function_call(sexp)

    def _compile_defun(self, sexp: SExpression):
        func_name = sexp.children[1].value
        params = [p.value for p in sexp.children[2].children]
        body = sexp.children[3:]

        self.builder = IRBuilder(func_name, params)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in params}
        self.builder.new_block("entry")

        for expr in body:
            self._compile_expression(expr)

        # STUB: For now, we just return 0. This should be the value of the last expression.
        self.builder.emit_return(Constant(0))

        self.program.add_function(self.builder.finalize())

    def _compile_arithmetic(self, sexp: SExpression):
        op = sexp.children[0].value
        args = sexp.children[1:]

        # For now, we only handle binary operations
        if len(args) != 2:
            return

        op1 = self._compile_expression(args[0])
        op2 = self._compile_expression(args[1])

        target = self.builder.function.name + "_tmp"
        self.builder.emit_binary_op(op, target, op1, op2)

        return VariableValue(target)

    def _compile_expression(self, expr):
        if isinstance(expr, Number):
            return Constant(expr.value)
        elif isinstance(expr, Symbol):
            return VariableValue(expr.value)
        elif isinstance(expr, SExpression):
            self._compile_sexpression(expr)

    def _compile_if(self, sexp: SExpression):
        condition = sexp.children[1]
        then_branch = sexp.children[2]
        else_branch = sexp.children[3]

        entry_block = self.builder.current_block

        cond_op = condition.children[0].value
        op1 = self._compile_expression(condition.children[1])
        op2 = self._compile_expression(condition.children[2])

        then_block = self.builder.new_block("then")
        else_block = self.builder.new_block("else")
        merge_block = self.builder.new_block("merge")

        entry_block.set_terminator(
            BranchTerminator(cond_op, op1, op2, then_block.label, else_block.label)
        )

        self.builder.current_block = then_block
        then_val = self._compile_expression(then_branch)
        self.builder.emit_jump(merge_block.label)

        self.builder.current_block = else_block
        else_val = self._compile_expression(else_branch)
        self.builder.emit_jump(merge_block.label)

        self.builder.current_block = merge_block
        # For now, we don't handle the PHI node, so we just return the then_val
        # self.builder.emit_assignment("if_result", then_val)

    def _compile_function_call(self, sexp: SExpression):
        func_name = sexp.children[0].value
        args = [self._compile_expression(arg) for arg in sexp.children[1:]]

        target = self.builder.function.name + "_tmp"
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)

    def _compile_atom(self, atom):
        # STUB: For now, we only compile top-level atoms in the REPL which is not yet implemented
        pass
