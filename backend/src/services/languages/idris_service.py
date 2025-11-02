# Ancient Compute - IDRIS Language Service

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class IDRISService(BaseExecutor):
    """IDRIS language service executor"""

    def __init__(self):
        super().__init__("idris", "ancient-compute-idris", timeout=10)

    def _get_command(self, code_path: str) -> str:
        """Get execution command for IDRIS"""
        # STUB: This is a placeholder command.
        return f"idris2 -x {code_path}"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute IDRIS code"""
        # STUB: This is a placeholder implementation.
        return ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="IDRIS service is not yet implemented.",
            stderr="",
            execution_time=0,
        )
