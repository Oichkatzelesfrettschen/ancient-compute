# Ancient Compute - C Language Execution Service
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class CExecutor(BaseExecutor):
    """C language executor with GCC compilation"""

    def __init__(self):
        super().__init__(
            language="c",
            docker_image="ancient-compute/c:latest",
            timeout=10
        )

    def _get_command(self, code_path: str) -> str:
        """GCC compile and execute command"""
        return "gcc -Wall -Werror -O2 -o /tmp/program main.c && /tmp/program < input.txt"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute C code with security checks"""
        # Security: Check for forbidden patterns
        forbidden_patterns = [
            "#include <sys/",
            "system(",
            "exec",
            "fork(",
            "socket(",
            "__asm__",
            "inline asm"
        ]

        for pattern in forbidden_patterns:
            if pattern in code:
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Security violation: '{pattern}' is not allowed",
                    execution_time=0
                )

        return await super().execute(code, input_data)
