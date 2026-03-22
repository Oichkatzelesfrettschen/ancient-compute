# Ancient Compute - IDRIS Compiler

from typing import Any

from ..ir_types import (
    BinaryOp,
    BranchTerminator,
    Constant,
    IRBuilder,
    IRType,
    Program,
    VariableValue,
)
from .idris_ast import (
    Case,
    FunctionApplication,
    FunctionDeclaration,
    Identifier,
    IfExpr,
    Lambda,
    Let,
    Literal,
    Module,
    ProofExpr,
    TypeDeclaration,
    Var,
)


class IdrisCompiler:
    def __init__(self) -> None:
        self.program = Program()
        self.builder: IRBuilder | None = None
        self.tmp_counter = 0

    def compile(self, ast: object) -> Program:
        if isinstance(ast, Module):
            self._compile_module(ast)
        elif isinstance(ast, list):
            for decl in ast:
                self._compile_decl(decl)
        return self.program

    def _compile_module(self, module: Module) -> None:
        for decl in module.body:
            self._compile_decl(decl)

    def _compile_decl(self, decl: object) -> None:
        if isinstance(decl, FunctionDeclaration):
            self._compile_function_declaration(decl)
        elif isinstance(decl, TypeDeclaration):
            # Erase type declarations at runtime
            pass

    def _compile_function_declaration(self, decl: FunctionDeclaration) -> None:
        self.builder = IRBuilder(decl.name, decl.args)
        self.builder.function.local_variables = {p: IRType.DEC50 for p in decl.args}
        self.builder.new_block("entry")

        return_val = self._compile_expression(decl.body)

        assert self.builder.current_block is not None
        if not self.builder.current_block.terminator:
            self.builder.emit_return(return_val)
        self.program.add_function(self.builder.finalize())

    def _compile_expression(self, expr: object) -> Any:
        if isinstance(expr, Identifier):
            return VariableValue(expr.name)
        elif isinstance(expr, Var):
            # Var is a type-system variable reference; treat as a named value
            return VariableValue(expr.name)
        elif isinstance(expr, Literal):
            v = expr.value
            if isinstance(v, str):
                return Constant(float(abs(hash(v)) % (10**40)))
            return Constant(float(v))
        elif isinstance(expr, FunctionApplication):
            return self._compile_function_application(expr)
        elif isinstance(expr, Let):
            return self._compile_let(expr)
        elif isinstance(expr, Case):
            return self._compile_case(expr)
        elif isinstance(expr, IfExpr):
            return self._compile_if_expr(expr)
        elif isinstance(expr, Lambda):
            return self._compile_lambda(expr)
        elif isinstance(expr, ProofExpr):
            # Proof terms (refl, cong, etc.) have no runtime value in the
            # Babbage ISA -- return a zero constant as a placeholder.
            return Constant(0)
        else:
            raise NotImplementedError(f"Idris expression type not supported: {type(expr).__name__}")

    def _compile_function_application(self, app: FunctionApplication) -> Any:
        assert self.builder is not None
        func_name = app.func.name if isinstance(app.func, Identifier) else "dynamic_call"
        args = [self._compile_expression(arg) for arg in app.args]

        target = self._new_tmp()
        self.builder.emit_call(func_name, args, target)

        return VariableValue(target)

    def _compile_let(self, let_expr: Let) -> Any:
        assert self.builder is not None
        for binding in let_expr.bindings:
            if isinstance(binding, FunctionDeclaration):
                # Simplified: handle as variable assignment if no args
                val = self._compile_expression(binding.body)
                self.builder.function.local_variables[binding.name] = IRType.DEC50
                self.builder.emit_assignment(binding.name, val)

        return self._compile_expression(let_expr.body)

    def _compile_case(self, case_expr: Case) -> Any:
        """Compile case expression as cascading equality branches.

        Each alternative is assumed to be a (pattern, body) pair.
        Patterns that are Literal nodes trigger equality checks; Identifier
        patterns bind the scrutinee to a variable (wildcard/default).
        """
        assert self.builder is not None
        scrutinee = self._compile_expression(case_expr.expr)
        result_var = self._new_tmp()
        self.builder.function.local_variables[result_var] = IRType.DEC50
        merge_label = f"case_merge_{self.tmp_counter}"

        for i, alt in enumerate(case_expr.alternatives):
            # Alternatives may be (pattern, body) tuples or structured nodes.
            # Support both tuple form and attribute form.
            if isinstance(alt, tuple) and len(alt) == 2:
                pat, body = alt
            elif hasattr(alt, "pattern") and hasattr(alt, "body"):
                pat, body = alt.pattern, alt.body  # type: ignore[union-attr]
            else:
                continue

            branch_label = f"case_alt_{self.tmp_counter}_{i}"
            next_label = f"case_next_{self.tmp_counter}_{i}"

            if isinstance(pat, Literal):
                cond_tmp = self._new_tmp()
                assert self.builder.current_block is not None
                self.builder.current_block.instructions.append(
                    BinaryOp(
                        op="eq",
                        target=cond_tmp,
                        operand1=scrutinee,
                        operand2=Constant(
                            float(pat.value)
                            if isinstance(pat.value, (int, float))
                            else float(abs(hash(pat.value)) % (10**40))
                        ),
                    )
                )
                assert self.builder.current_block is not None
                self.builder.current_block.terminator = BranchTerminator(
                    condition="nonzero",
                    operand1=VariableValue(cond_tmp),
                    operand2=None,
                    true_label=branch_label,
                    false_label=next_label,
                )
            elif isinstance(pat, Identifier):
                # Default/wildcard: bind scrutinee to name, always taken
                self.builder.function.local_variables[pat.name] = IRType.DEC50
                self.builder.emit_assignment(pat.name, scrutinee)
                self.builder.emit_jump(branch_label)
                # Create a dummy next block (unreachable)
                self.builder.new_block(next_label)
                # Fall through handled below
            else:
                self.builder.emit_jump(branch_label)
                self.builder.new_block(next_label)

            _alt_block = self.builder.new_block(branch_label)
            body_val = self._compile_expression(body)
            self.builder.emit_assignment(result_var, body_val)
            self.builder.emit_jump(merge_label)

            # Continue checking next alternative
            self.builder.current_block = (
                self.builder.new_block(next_label)
                if not any(b.label == next_label for b in self.builder.function.basic_blocks)
                else next(b for b in self.builder.function.basic_blocks if b.label == next_label)
            )

        # Fallback: return 0 if no alternative matched
        self.builder.emit_assignment(result_var, Constant(0))
        self.builder.emit_jump(merge_label)

        self.builder.new_block(merge_label)
        return VariableValue(result_var)

    def _compile_if_expr(self, expr: IfExpr) -> Any:
        """Compile if-then-else: branch on condition, merge at end."""
        then_label = self._new_tmp() + "_then"
        else_label = self._new_tmp() + "_else"
        merge_label = self._new_tmp() + "_merge"
        result_var = self._new_tmp() + "_if_result"

        cond = self._compile_expression(expr.condition)
        assert self.builder is not None
        self.builder.emit_branch("nonzero", cond, None, then_label, else_label)

        self.builder.new_block(then_label)
        then_val = self._compile_expression(expr.then_expr)
        self.builder.emit_assignment(result_var, then_val)
        self.builder.emit_jump(merge_label)

        self.builder.new_block(else_label)
        else_val = self._compile_expression(expr.else_expr)
        self.builder.emit_assignment(result_var, else_val)
        self.builder.emit_jump(merge_label)

        self.builder.new_block(merge_label)
        return VariableValue(result_var)

    def _compile_lambda(self, expr: Lambda) -> Any:
        """Compile lambda as a local anonymous function, returning its name."""
        assert self.builder is not None
        fn_name = f"__lambda_{self._new_tmp()}"
        saved_builder = self.builder

        self.builder = IRBuilder(fn_name, [expr.param])
        self.builder.function.local_variables = {expr.param: IRType.DEC50}
        self.builder.new_block("entry")
        body_val = self._compile_expression(expr.body)
        self.builder.emit_return(body_val)
        fn = self.builder.finalize()
        self.program.add_function(fn)

        self.builder = saved_builder
        return VariableValue(fn_name)

    def _new_tmp(self) -> str:
        self.tmp_counter += 1
        return f"t{self.tmp_counter}"


class IDRIS2Compiler(IdrisCompiler):
    """Extended Idris compiler that accepts source strings (lex+parse+compile)."""

    def compile(self, ast: object) -> Program:
        if isinstance(ast, str):
            from .idris_parser import IdrisParser

            parser = IdrisParser(ast)
            parsed = parser.parse()
            return super().compile(parsed)
        return super().compile(ast)
