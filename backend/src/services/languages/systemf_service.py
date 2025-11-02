# Ancient Compute - System F Language Service

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class SystemFService(BaseExecutor):
    """System F language service executor"""

    def __init__(self):
        super().__init__("systemf", "ancient-compute-systemf", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for System F"""
        return f"systemf-interpreter {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute System F code"""
        return ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="System F service is not yet implemented.",
            stderr="",
            execution_time=0,
        )
