# Ancient Compute - LISP Compiler

from typing import Any

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
    def __init__(self) -> None:
        self.program = Program()
        self.builder: IRBuilder | None = None
        self.tmp_counter = 0

    def compile(self, ast: ASTNode) -> Program:
        if isinstance(ast, SExpression):
            self._compile_sexpression(ast)
        else:
            self._compile_atom(ast)
        return self.program

    def _compile_sexpression(self, sexp: SExpression) -> Any:
        if not sexp.children:
            return None

        first = sexp.children[0]
        if isinstance(first, Symbol):
            if first.value == "defun":
                return self._compile_defun(sexp)
            elif first.value == "if":
                return self._compile_if(sexp)
            elif first.value == "cond":
                return self._compile_cond(sexp)
            elif first.value == "let":
                return self._compile_let(sexp)
            elif first.value in ["+", "-", "*", "/"]:
                return self._compile_arithmetic(sexp)
            else:
                return self._compile_function_call(sexp)
        else:
            # Handle ((lambda ...) args)
            return self._compile_function_call(sexp)

    def _compile_defun(self, sexp: SExpression) -> None:
        assert isinstance(sexp.children[1], Symbol)
        func_name = sexp.children[1].value
        assert isinstance(sexp.children[2], SExpression)
        params = [p.value for p in sexp.children[2].children]  # type: ignore[attr-defined]
        body = sexp.children[3:]

        self.builder = IRBuilder(func_name, params)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in params}
        self.builder.new_block("entry")

        last_result = Constant(0)
        for expr in body:
            last_result = self._compile_expression(expr)

        self.builder.emit_return(last_result)
        self.program.add_function(self.builder.finalize())

    def _compile_arithmetic(self, sexp: SExpression) -> Any:
        assert self.builder is not None
        op_map = {"+": "add", "-": "sub", "*": "mul", "/": "div"}
        assert isinstance(sexp.children[0], Symbol)
        op_sym = sexp.children[0].value
        op = op_map.get(op_sym, op_sym)
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

    def _compile_expression(self, expr: object) -> Any:
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

    def _compile_if(self, sexp: SExpression) -> Any:
        # (if cond then else)
        condition = sexp.children[1]
        then_branch = sexp.children[2]
        else_branch = sexp.children[3] if len(sexp.children) > 3 else Number(0)

        assert self.builder is not None
        assert self.builder.current_block is not None
        entry_block = self.builder.current_block

        # Compile condition
        # IR BranchTerminator expects: condition, op1, op2, true_lbl, false_lbl
        # We need to reduce the Lisp condition to a comparison if it isn't one

        cond_val = self._compile_expression(condition)

        then_block = self.builder.new_block("then")
        else_block = self.builder.new_block("else")
        merge_block = self.builder.new_block("merge")

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

    def _compile_cond(self, sexp: SExpression) -> Any:
        """Compile (cond (test1 e1) (test2 e2) ... (t else)) to nested if-else."""
        clauses = sexp.children[1:]
        return self._cond_clauses(list(clauses))

    def _cond_clauses(self, clauses: list[ASTNode]) -> Any:
        """Recursively lower cond clauses to if-else IR."""
        if not clauses:
            return Constant(0)

        clause = clauses[0]
        assert isinstance(clause, SExpression)
        test = clause.children[0]
        expr_node = clause.children[1]

        # t / otherwise / else / true is the unconditional else branch
        if isinstance(test, Symbol) and test.value in ("t", "otherwise", "else", "true"):
            return self._compile_expression(expr_node)

        assert self.builder is not None
        assert self.builder.current_block is not None
        entry_block = self.builder.current_block

        cond_val = self._compile_expression(test)

        then_block = self.builder.new_block("cond_then")
        else_block = self.builder.new_block("cond_else")
        merge_block = self.builder.new_block("cond_merge")

        result_var = self._new_tmp()

        entry_block.set_terminator(
            BranchTerminator("ne", cond_val, Constant(0), then_block.label, else_block.label)
        )

        # Then branch
        self.builder.current_block = then_block
        then_val = self._compile_expression(expr_node)
        if then_val:
            self.builder.emit_assignment(result_var, then_val)
        self.builder.emit_jump(merge_block.label)

        # Else branch: remaining cond clauses
        self.builder.current_block = else_block
        else_val = self._cond_clauses(clauses[1:])
        if else_val:
            self.builder.emit_assignment(result_var, else_val)
        self.builder.emit_jump(merge_block.label)

        self.builder.current_block = merge_block
        return VariableValue(result_var)

    def _compile_let(self, sexp: SExpression) -> Any:
        # (let ((var val) ...) body...)
        assert isinstance(sexp.children[1], SExpression)
        bindings = sexp.children[1].children
        body = sexp.children[2:]

        # For this basic implementation, we just register variables in the function scope
        # Shadowing is not handled (flat scope)

        for binding in bindings:
            # binding is SExpression(Symbol(var), Expr(val))
            assert isinstance(binding, SExpression)
            assert isinstance(binding.children[0], Symbol)
            var_name = binding.children[0].value
            val_expr = binding.children[1]

            val_compiled = self._compile_expression(val_expr)
            assert self.builder is not None
            self.builder.function.local_variables[var_name] = IRType.DEC50
            self.builder.emit_assignment(var_name, val_compiled)

        last_result = Constant(0)
        for expr in body:
            last_result = self._compile_expression(expr)

        return last_result

    def _compile_function_call(self, sexp: SExpression) -> Any:
        assert self.builder is not None
        assert isinstance(sexp.children[0], (Symbol, String))
        func_name = sexp.children[0].value
        args = [self._compile_expression(arg) for arg in sexp.children[1:]]

        target = self._new_tmp()
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)

    def _compile_atom(self, atom: object) -> None:
        # Top level atom? probably just return it if we are evaluating
        pass

    def _new_tmp(self) -> str:
        self.tmp_counter += 1
        return f"tmp_{self.tmp_counter}"
