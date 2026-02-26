# Ancient Compute - LISP Language Service

import time

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus

from backend.src.compilers.lisp_parser import parser
from backend.src.compilers.lisp_compiler import LispCompiler


class LISPService(BaseExecutor):
    """LISP language service executor -- compiles LISP to Babbage IR."""

    def __init__(self):
        super().__init__("lisp", "ancient-compute-lisp", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for LISP"""
        return f"sbcl --script {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Compile LISP code to Babbage IR and return compilation results."""
        start = time.monotonic()
        try:
            ast = parser.parse(code)
            if ast is None:
                raise ValueError("LISP parse error: no AST produced")
            compiler = LispCompiler()
            program = compiler.compile(ast)

            elapsed = time.monotonic() - start
            func_names = list(program.functions.keys()) if hasattr(program, 'functions') else []
            return ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout=f"Compiled {len(func_names)} function(s): {', '.join(func_names) or '(top-level)'}",
                stderr="",
                execution_time=elapsed,
            )
        except Exception as e:
            elapsed = time.monotonic() - start
            return ExecutionResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stdout="",
                stderr=str(e),
                execution_time=elapsed,
            )
