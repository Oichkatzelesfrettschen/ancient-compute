"""
Ancient Compute - Python Execution Service with Proper RestrictedPython Integration
This is the CORRECTED implementation that actually uses RestrictedPython
"""
from typing import Optional, List, Dict, Any
import asyncio
import time
from io import StringIO
import sys
import traceback

# RestrictedPython imports
try:
    from RestrictedPython import compile_restricted, safe_globals
    from RestrictedPython.Guards import guarded_iter_unpack_sequence
    from RestrictedPython.PrintCollector import PrintCollector
    RESTRICTED_PYTHON_AVAILABLE = True
except ImportError:
    RESTRICTED_PYTHON_AVAILABLE = False
    print("Warning: RestrictedPython not available. Install with: pip install RestrictedPython")

from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus
from ..docker_manager import get_docker_manager, ExecutionBackend


class RestrictedPythonRunner:
    """
    Handles execution of Python code using RestrictedPython
    Provides a sandboxed environment without Docker
    """

    def __init__(self, timeout: int = 5):
        self.timeout = timeout
        self.output_buffer = StringIO()
        self.input_lines: List[str] = []
        self.input_index = 0

    def _create_restricted_builtins(self) -> Dict[str, Any]:
        """Create a safe set of builtins for restricted execution"""
        safe_builtins = {
            # Safe built-in functions
            'abs': abs,
            'all': all,
            'any': any,
            'ascii': ascii,
            'bin': bin,
            'bool': bool,
            'bytearray': bytearray,
            'bytes': bytes,
            'chr': chr,
            'complex': complex,
            'dict': dict,
            'divmod': divmod,
            'enumerate': enumerate,
            'filter': filter,
            'float': float,
            'format': format,
            'frozenset': frozenset,
            'hex': hex,
            'int': int,
            'isinstance': isinstance,
            'issubclass': issubclass,
            'iter': iter,
            'len': len,
            'list': list,
            'map': map,
            'max': max,
            'min': min,
            'next': next,
            'oct': oct,
            'ord': ord,
            'pow': pow,
            'range': range,
            'repr': repr,
            'reversed': reversed,
            'round': round,
            'set': set,
            'slice': slice,
            'sorted': sorted,
            'str': str,
            'sum': sum,
            'tuple': tuple,
            'type': type,
            'zip': zip,

            # Input function
            'input': self._safe_input,

            # Constants
            'True': True,
            'False': False,
            'None': None,

            # Restricted imports (very limited)
            '__import__': self._restricted_import,
        }

        # Add safe exceptions
        safe_exceptions = [
            'ArithmeticError', 'AssertionError', 'AttributeError',
            'BaseException', 'BlockingIOError', 'BrokenPipeError',
            'BufferError', 'BytesWarning', 'ChildProcessError',
            'ConnectionAbortedError', 'ConnectionError',
            'ConnectionRefusedError', 'ConnectionResetError',
            'DeprecationWarning', 'EOFError', 'Exception',
            'FileExistsError', 'FileNotFoundError', 'FloatingPointError',
            'FutureWarning', 'GeneratorExit', 'ImportError',
            'ImportWarning', 'IndentationError', 'IndexError',
            'InterruptedError', 'IsADirectoryError', 'KeyError',
            'KeyboardInterrupt', 'LookupError', 'MemoryError',
            'ModuleNotFoundError', 'NameError', 'NotADirectoryError',
            'NotImplementedError', 'OSError', 'OverflowError',
            'PendingDeprecationWarning', 'PermissionError',
            'ProcessLookupError', 'RecursionError', 'ReferenceError',
            'ResourceWarning', 'RuntimeError', 'RuntimeWarning',
            'StopAsyncIteration', 'StopIteration', 'SyntaxError',
            'SyntaxWarning', 'SystemError', 'SystemExit', 'TabError',
            'TimeoutError', 'TypeError', 'UnboundLocalError',
            'UnicodeDecodeError', 'UnicodeEncodeError', 'UnicodeError',
            'UnicodeTranslateError', 'UnicodeWarning', 'UserWarning',
            'ValueError', 'Warning', 'ZeroDivisionError'
        ]

        for exc_name in safe_exceptions:
            if hasattr(__builtins__, exc_name):
                safe_builtins[exc_name] = getattr(__builtins__, exc_name)

        return safe_builtins

    def _safe_input(self, prompt: str = "") -> str:
        """Safe input function that reads from pre-provided input"""
        if prompt:
            self.output_buffer.write(str(prompt))

        if self.input_index < len(self.input_lines):
            line = self.input_lines[self.input_index]
            self.input_index += 1
            return line
        return ""  # Return empty string if no more input

    def _restricted_import(self, name, *args, **kwargs):
        """Restricted import that only allows specific safe modules"""
        allowed_modules = {
            'math': __import__('math'),
            'random': __import__('random'),
            'itertools': __import__('itertools'),
            'functools': __import__('functools'),
            'collections': __import__('collections'),
            'string': __import__('string'),
            're': __import__('re'),
            'datetime': __import__('datetime'),
            'decimal': __import__('decimal'),
            'fractions': __import__('fractions'),
            'time': __import__('time'),
        }

        if name in allowed_modules:
            return allowed_modules[name]

        raise ImportError(f"Import of module '{name}' is not allowed")

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute Python code in a restricted environment"""
        start_time = time.time()

        if not RESTRICTED_PYTHON_AVAILABLE:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr="RestrictedPython not available. Install with: pip install RestrictedPython",
                execution_time=0
            )

        # Reset state
        self.output_buffer = StringIO()
        self.input_lines = input_data.splitlines()
        self.input_index = 0
        try:
            try:
                compiled = compile_restricted(code, "<user_code>", "exec")
            except SyntaxError as exc:
                message = f"{exc.msg} (line {exc.lineno})" if exc.lineno else exc.msg
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr=message,
                    compile_output=message,
                    execution_time=time.time() - start_time,
                )

            restricted_globals = {
                "__builtins__": self._create_restricted_builtins(),
                "__name__": "__main__",
                "__doc__": None,
                "__package__": None,
                "__loader__": None,
                "__spec__": None,
                "__file__": "<user_code>",
                "__cached__": None,
                "_iter_unpack_sequence_": guarded_iter_unpack_sequence,
                "_getiter_": iter,
                "_print_": PrintCollector,
                "_getitem_": lambda obj, item: obj[item],
                "_getattr_": getattr,
                "_write_": lambda obj: obj,
                "input": self._safe_input,
            }
            locals_dict: Dict[str, Any] = {}

            # Execute with timeout
            try:
                # Run in executor to enable timeout
                await asyncio.wait_for(
                    asyncio.get_event_loop().run_in_executor(
                        None,
                        self._execute_code,
                        compiled,
                        restricted_globals,
                        locals_dict,
                    ),
                    timeout=self.timeout
                )

                collector_output = ""
                if callable(locals_dict.get("_print")):
                    collector_output = locals_dict["_print"]()

                combined_output = self.output_buffer.getvalue() + collector_output
                if len(combined_output) > 10000:
                    combined_output = combined_output[:10000] + "\n[Output truncated at 10KB]"

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS,
                    stdout=combined_output,
                    stderr="",
                    execution_time=time.time() - start_time,
                    exit_code=0
                )

            except asyncio.TimeoutError:
                collector_output = ""
                if callable(locals_dict.get("_print")):
                    collector_output = locals_dict["_print"]()
                combined_output = self.output_buffer.getvalue() + collector_output
                if len(combined_output) > 10000:
                    combined_output = combined_output[:10000] + "\n[Output truncated at 10KB]"

                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout=combined_output,
                    stderr=f"Execution timeout ({self.timeout} seconds exceeded)",
                    execution_time=self.timeout,
                    exit_code=-1
                )

        except Exception as e:
            # Capture the traceback for better error reporting
            tb = traceback.format_exc()
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout=self.output_buffer.getvalue(),
                stderr=f"Runtime error: {str(e)}\n\nTraceback:\n{tb}",
                execution_time=time.time() - start_time,
                exit_code=1
            )

    def _execute_code(self, compiled_code, globals_dict, locals_dict):
        """Execute the compiled code (called in executor)"""
        exec(compiled_code, globals_dict, locals_dict)


class PythonExecutor(BaseExecutor):
    """
    Python executor with multiple execution backends:
    1. RestrictedPython for simple, safe code (preferred for performance)
    2. Docker for complex code or when additional isolation is needed
    3. Graceful fallback when Docker is not available
    """

    def __init__(self):
        super().__init__(
            language="python",
            docker_image="ancient-compute/python:latest",
            timeout=5
        )
        self.docker_manager = get_docker_manager()
        self.restricted_runner = RestrictedPythonRunner(timeout=self.timeout)

    def _analyze_code_complexity(self, code: str) -> bool:
        """
        Analyze if code is simple enough for RestrictedPython
        Returns True if code should use Docker for safety
        """
        # Check for potentially dangerous patterns
        dangerous_patterns = [
            # File operations
            'open(', 'file(', 'with open',
            # System operations
            '__import__', 'eval(', 'exec(', 'compile(',
            'globals(', 'locals(', 'vars(',
            # Network operations
            'urllib', 'requests', 'socket',
            # Process operations
            'subprocess', 'os.', 'sys.',
            # Memory/resource intensive
            'while True', 'for i in range(10000000',
        ]

        code_lower = code.lower()
        for pattern in dangerous_patterns:
            if pattern.lower() in code_lower:
                return True  # Needs Docker

        # Check code size (large code might be resource intensive)
        if len(code) > 10000:
            return True  # Needs Docker

        return False  # Can use RestrictedPython

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """
        Execute Python code using the best available backend
        Priority: RestrictedPython (if safe) > Docker > Error
        """
        # Ensure ASCII only (as per requirements)
        try:
            code = code.encode('ascii', errors='replace').decode('ascii')
            input_data = input_data.encode('ascii', errors='replace').decode('ascii')
        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=f"ASCII encoding error: {str(e)}",
                execution_time=0
            )

        # Determine best execution backend
        backend = self.docker_manager.get_backend_for_language("python")
        if not self.docker_available and backend == ExecutionBackend.DOCKER:
            if self.docker_manager.backends[ExecutionBackend.RESTRICTED_PYTHON].available:
                backend = ExecutionBackend.RESTRICTED_PYTHON
            elif self.docker_manager.backends[ExecutionBackend.SUBPROCESS].available:
                backend = ExecutionBackend.SUBPROCESS
            else:
                backend = ExecutionBackend.UNAVAILABLE
        needs_docker = self._analyze_code_complexity(code)

        # Try RestrictedPython first if code is simple
        if not needs_docker and backend in [ExecutionBackend.RESTRICTED_PYTHON, ExecutionBackend.SUBPROCESS]:
            result = await self.restricted_runner.execute(code, input_data)

            # If RestrictedPython succeeded or definitively failed, return result
            if result.status != ExecutionStatus.SECURITY_VIOLATION:
                return result

        # Fall back to Docker if available and needed
        if backend == ExecutionBackend.DOCKER:
            # Ensure Docker is actually available
            if self.docker_manager.docker_available:
                return await self._execute_in_docker(code, input_data)
            else:
                # Docker should be available but isn't - check why
                docker_info = self.docker_manager.backends[ExecutionBackend.DOCKER]
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=f"Docker execution failed: {docker_info.reason}",
                    execution_time=0
                )

        # No suitable backend available
        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stdout="",
            stderr="No suitable execution environment available for this code. "
                   "Complex code requires Docker. Please ensure Docker Desktop is running.",
            execution_time=0
        )

    async def _execute_in_docker(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute code in Docker container (inherited from BaseExecutor)"""
        # Check for obvious security violations before Docker execution
        forbidden_imports = ['socket', 'urllib', 'subprocess', 'ctypes']
        code_lower = code.lower()

        for forbidden in forbidden_imports:
            if f'import {forbidden}' in code_lower or f'from {forbidden}' in code_lower:
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Import of '{forbidden}' is not allowed for security reasons",
                    execution_time=0
                )

        # Use parent class Docker execution
        return await super().execute(code, input_data)


# Example usage and testing
if __name__ == "__main__":
    async def test_executor():
        executor = PythonExecutor()

        # Test 1: Simple safe code (should use RestrictedPython)
        print("Test 1: Simple safe code")
        code1 = """
print("Hello, World!")
x = 5 + 3
print(f"5 + 3 = {x}")
"""
        result1 = await executor.execute(code1)
        print(f"Status: {result1.status.value}")
        print(f"Output: {result1.stdout}")
        print()

        # Test 2: Code with input (should use RestrictedPython)
        print("Test 2: Code with input")
        code2 = """
name = input("Enter your name: ")
print(f"Hello, {name}!")
"""
        result2 = await executor.execute(code2, "Alice")
        print(f"Status: {result2.status.value}")
        print(f"Output: {result2.stdout}")
        print()

        # Test 3: Dangerous code (should be blocked)
        print("Test 3: Dangerous code")
        code3 = """
import os
print(os.listdir())
"""
        result3 = await executor.execute(code3)
        print(f"Status: {result3.status.value}")
        print(f"Error: {result3.stderr}")
        print()

        # Test 4: Math module (allowed import)
        print("Test 4: Math module")
        code4 = """
import math
print(f"Pi = {math.pi}")
print(f"sqrt(16) = {math.sqrt(16)}")
"""
        result4 = await executor.execute(code4)
        print(f"Status: {result4.status.value}")
        print(f"Output: {result4.stdout}")

    # Run tests
    import asyncio
    asyncio.run(test_executor())
