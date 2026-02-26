# Ancient Compute - IDRIS Language Service

import time

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class IDRISService(BaseExecutor):
    """IDRIS language service executor -- compiles Idris source to Babbage IR."""

    def __init__(self):
        super().__init__("idris", "ancient-compute-idris", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for IDRIS"""
        return f"idris2 -x {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Compile Idris code to Babbage IR and return compilation results."""
        start = time.monotonic()
        try:
            from backend.src.compilers.idris_compiler import IdrisCompiler
            from backend.src.compilers.idris_parser import IdrisParser

            parser = IdrisParser(code)
            ast = parser.parse()
            compiler = IdrisCompiler()
            program = compiler.compile(ast)

            elapsed = time.monotonic() - start
            func_names = list(program.functions.keys()) if hasattr(program, 'functions') else []
            return ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout=f"Compiled {len(func_names)} function(s): {', '.join(func_names) or 'main'}",
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
