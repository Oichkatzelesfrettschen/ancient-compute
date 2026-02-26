# Ancient Compute - IDRIS Compiler

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
    Let,
    Literal,
    Module,
    TypeDeclaration,
)


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
        elif isinstance(expr, Case):
            return self._compile_case(expr)
        else:
            raise NotImplementedError(
                f"Idris expression type not supported: {type(expr).__name__}"
            )

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

    def _compile_case(self, case_expr: Case):
        """Compile case expression as cascading equality branches.

        Each alternative is assumed to be a (pattern, body) pair.
        Patterns that are Literal nodes trigger equality checks; Identifier
        patterns bind the scrutinee to a variable (wildcard/default).
        """
        scrutinee = self._compile_expression(case_expr.expr)
        result_var = self._new_tmp()
        self.builder.function.local_variables[result_var] = IRType.DEC50
        merge_label = f"case_merge_{self.tmp_counter}"

        for i, alt in enumerate(case_expr.alternatives):
            # Alternatives may be (pattern, body) tuples or structured nodes.
            # Support both tuple form and attribute form.
            if isinstance(alt, tuple) and len(alt) == 2:
                pat, body = alt
            elif hasattr(alt, 'pattern') and hasattr(alt, 'body'):
                pat, body = alt.pattern, alt.body
            else:
                continue

            branch_label = f"case_alt_{self.tmp_counter}_{i}"
            next_label = f"case_next_{self.tmp_counter}_{i}"

            if isinstance(pat, Literal):
                cond_tmp = self._new_tmp()
                self.builder.current_block.instructions.append(
                    BinaryOp(op='eq', target=cond_tmp,
                             operand1=scrutinee, operand2=Constant(pat.value))
                )
                self.builder.current_block.terminator = BranchTerminator(
                    condition='nonzero',
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

            alt_block = self.builder.new_block(branch_label)
            body_val = self._compile_expression(body)
            self.builder.emit_assignment(result_var, body_val)
            self.builder.emit_jump(merge_label)

            # Continue checking next alternative
            self.builder.current_block = self.builder.new_block(next_label) \
                if not any(b.label == next_label for b in self.builder.function.blocks) \
                else next(b for b in self.builder.function.blocks if b.label == next_label)

        # Fallback: return 0 if no alternative matched
        self.builder.emit_assignment(result_var, Constant(0))
        self.builder.emit_jump(merge_label)

        self.builder.new_block(merge_label)
        return VariableValue(result_var)

    def _new_tmp(self):
        self.tmp_counter += 1
        return f"t{self.tmp_counter}"


class IDRIS2Compiler(IdrisCompiler):
    """Extended Idris compiler that accepts source strings (lex+parse+compile)."""

    def compile(self, source_or_ast) -> Program:
        if isinstance(source_or_ast, str):
            from .idris_parser import IdrisParser
            parser = IdrisParser(source_or_ast)
            ast = parser.parse()
            return super().compile(ast)
        return super().compile(source_or_ast)
