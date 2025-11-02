# Ancient Compute - Java Language Service

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class JavaService(BaseExecutor):
    """Java language service executor"""

    def __init__(self):
        super().__init__("java", "ancient-compute-java", timeout=15)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for Java"""
        return f"javac {code_path} && java Main"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute Java code"""
        return ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Java service is not yet implemented.",
            stderr="",
            execution_time=0,
        )
