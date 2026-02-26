# Ancient Compute - LISP Compiler

from ..ir_types import (
    BranchTerminator,
    Constant,
    IRBuilder,
    IRType,
    Program,
    VariableValue,
)
from .lisp_ast import ASTNode, Number, SExpression, String, Symbol


class LispCompiler:
    def __init__(self):
        self.program = Program()
        self.builder: IRBuilder = None
        self.tmp_counter = 0

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
            if first.value == 'defun':
                return self._compile_defun(sexp)
            elif first.value == 'if':
                return self._compile_if(sexp)
            elif first.value == 'let':
                return self._compile_let(sexp)
            elif first.value in ['+', '-', '*', '/']:
                return self._compile_arithmetic(sexp)
            else:
                return self._compile_function_call(sexp)
        else:
             # Handle ((lambda ...) args)
             return self._compile_function_call(sexp)

    def _compile_defun(self, sexp: SExpression):
        func_name = sexp.children[1].value
        params = [p.value for p in sexp.children[2].children]
        body = sexp.children[3:]

        self.builder = IRBuilder(func_name, params)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in params}
        self.builder.new_block('entry')

        last_result = Constant(0)
        for expr in body:
            last_result = self._compile_expression(expr)

        self.builder.emit_return(last_result)
        self.program.add_function(self.builder.finalize())

    def _compile_arithmetic(self, sexp: SExpression):
        op_map = {
            '+': 'add',
            '-': 'sub',
            '*': 'mul',
            '/': 'div'
        }
        op_sym = sexp.children[0].value
        op = op_map.get(op_sym)
        args = sexp.children[1:]

        # IR currently supports binary ops. Lisp supports n-ary.
        # Reduce: (+ a b c) -> (+ (+ a b) c)

        if not args:
            return Constant(0)

        current_val = self._compile_expression(args[0])

        for arg in args[1:]:
            next_val = self._compile_expression(arg)
            target = self._new_tmp()
            self.builder.emit_binary_op(op, target, current_val, next_val)
            current_val = VariableValue(target)

        return current_val

    def _compile_expression(self, expr):
        if isinstance(expr, Number):
            return Constant(expr.value)
        elif isinstance(expr, Symbol):
            return VariableValue(expr.value)
        elif isinstance(expr, SExpression):
            return self._compile_sexpression(expr)
        elif isinstance(expr, String):
            # Babbage engine operates on 50-digit decimals; encode strings
            # as integer hash values (the engine has no native string type).
            # Truncate to 50-digit capacity (10^50 - 1).
            str_hash = abs(hash(expr.value)) % (10**50)
            return Constant(str_hash)

    def _compile_if(self, sexp: SExpression):
        # (if cond then else)
        condition = sexp.children[1]
        then_branch = sexp.children[2]
        else_branch = sexp.children[3] if len(sexp.children) > 3 else Number(0)

        entry_block = self.builder.current_block

        # Compile condition
        # IR BranchTerminator expects: condition, op1, op2, true_lbl, false_lbl
        # We need to reduce the Lisp condition to a comparison if it isn't one

        cond_val = self._compile_expression(condition)

        then_block = self.builder.new_block('then')
        else_block = self.builder.new_block('else')
        merge_block = self.builder.new_block('merge')

        result_var = self._new_tmp()

        # Emit branch. checking if cond_val != 0 (Lisp truthiness)
        # Using "ne" (not equal) 0
        entry_block.set_terminator(
            BranchTerminator("ne", cond_val, Constant(0), then_block.label, else_block.label)
        )

        # Then Block
        self.builder.current_block = then_block
        then_val = self._compile_expression(then_branch)
        if then_val:
            self.builder.emit_assignment(result_var, then_val)
        self.builder.emit_jump(merge_block.label)

        # Else Block
        self.builder.current_block = else_block
        else_val = self._compile_expression(else_branch)
        if else_val:
            self.builder.emit_assignment(result_var, else_val)
        self.builder.emit_jump(merge_block.label)

        # Merge
        self.builder.current_block = merge_block
        return VariableValue(result_var)

    def _compile_let(self, sexp: SExpression):
        # (let ((var val) ...) body...)
        bindings = sexp.children[1].children
        body = sexp.children[2:]

        # For this basic implementation, we just register variables in the function scope
        # Shadowing is not handled (flat scope)

        for binding in bindings:
            # binding is SExpression(Symbol(var), Expr(val))
            var_name = binding.children[0].value
            val_expr = binding.children[1]

            val_compiled = self._compile_expression(val_expr)
            self.builder.function.local_variables[var_name] = IRType.DEC50
            self.builder.emit_assignment(var_name, val_compiled)

        last_result = Constant(0)
        for expr in body:
            last_result = self._compile_expression(expr)

        return last_result

    def _compile_function_call(self, sexp: SExpression):
        func_name = sexp.children[0].value
        args = [self._compile_expression(arg) for arg in sexp.children[1:]]

        target = self._new_tmp()
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)

    def _compile_atom(self, atom):
        # Top level atom? probably just return it if we are evaluating
        pass

    def _new_tmp(self):
        self.tmp_counter += 1
        return f"tmp_{self.tmp_counter}"
