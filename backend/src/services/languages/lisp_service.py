# Ancient Compute - LISP Language Service

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus

from backend.src.compilers.lisp_parser import parser
from backend.src.compilers.lisp_compiler import LispCompiler

class LISPService(BaseExecutor):
    """LISP language service executor"""

    def __init__(self):
        super().__init__("lisp", "ancient-compute-lisp", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for LISP"""
        return f"sbcl --script {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute LISP code"""
        try:
            ast = parser.parse(code)
            if ast is None:
                raise ValueError("LISP parse error")
            compiler = LispCompiler()
            program = compiler.compile(ast)
            # STUB: For now, we just return a success message if the compilation is successful.
            # In the future, we will need to either interpret the IR or generate assembly code and run it.
            return ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout="Compilation successful.",
                stderr="",
                execution_time=0,
            )
        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stdout="",
                stderr=str(e),
                execution_time=0,
            )
