# Ancient Compute - System F Language Service

import time

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class SystemFService(BaseExecutor):
    """System F language service executor -- compiles System F to Babbage IR."""

    def __init__(self):
        super().__init__("systemf", "ancient-compute-systemf", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for System F"""
        return f"systemf-interpreter {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Compile System F code to Babbage IR and return compilation results."""
        start = time.monotonic()
        try:
            from backend.src.compilers.systemf_compiler import SystemFCompiler

            compiler = SystemFCompiler()
            program = compiler.compile(code)

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
