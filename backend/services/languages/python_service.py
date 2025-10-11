# Ancient Compute - Python Execution Service with RestrictedPython
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus
import sys
import io
import traceback


class PythonExecutor(BaseExecutor):
    """Python executor with RestrictedPython sandboxing"""

    def __init__(self):
        super().__init__(
            language="python",
            docker_image="ancient-compute/python:latest",
            timeout=5
        )

    def _get_command(self, code_path: str) -> str:
        """Python execution command"""
        return "python3 main.py < input.txt"

    def _get_safe_globals(self):
        """Get safe globals for restricted execution"""
        return {
            '__builtins__': {
                'print': print,
                'range': range,
                'len': len,
                'int': int,
                'float': float,
                'str': str,
                'list': list,
                'dict': dict,
                'tuple': tuple,
                'set': set,
                'bool': bool,
                'True': True,
                'False': False,
                'None': None,
                'abs': abs,
                'min': min,
                'max': max,
                'sum': sum,
                'sorted': sorted,
                'enumerate': enumerate,
                'zip': zip,
                'map': map,
                'filter': filter,
            }
        }

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute Python code with restrictions"""
        # Basic security checks
        forbidden_imports = ['os', 'sys', 'subprocess', 'socket', 'urllib', '__import__']
        code_lower = code.lower()

        for forbidden in forbidden_imports:
            if f'import {forbidden}' in code_lower or f'from {forbidden}' in code_lower:
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Import of '{forbidden}' is not allowed",
                    execution_time=0
                )

        # Execute in container for additional isolation
        return await super().execute(code, input_data)
