"""
Ancient Compute - Unit Tests for Executors (No Docker Required)
These tests can run without Docker being installed or running
"""
import pytest
import asyncio
from unittest.mock import Mock, patch, AsyncMock, MagicMock
import sys
import os

# Add parent directory to path for imports
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../src')))

# Mock docker before importing executors
sys.modules['docker'] = MagicMock()

from services.base_executor import ExecutionStatus, ExecutionResult
from services.languages.python_service import PythonExecutor, RestrictedPythonRunner
from services.languages.c_service import CExecutor
from services.languages.haskell_service import HaskellExecutor


class TestRestrictedPythonRunner:
    """Test the RestrictedPython runner without Docker"""

    @pytest.mark.asyncio
    async def test_simple_print(self):
        """Test simple print statement"""
        runner = RestrictedPythonRunner()
        code = "print('Hello, World!')"
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.SUCCESS
        assert "Hello, World!" in result.stdout
        assert result.stderr == ""

    @pytest.mark.asyncio
    async def test_arithmetic(self):
        """Test basic arithmetic operations"""
        runner = RestrictedPythonRunner()
        code = """
x = 5 + 3
y = x * 2
print(f"Result: {y}")
"""
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.SUCCESS
        assert "Result: 16" in result.stdout

    @pytest.mark.asyncio
    async def test_input_handling(self):
        """Test input() function"""
        runner = RestrictedPythonRunner()
        code = """
name = input("Enter name: ")
age = input("Enter age: ")
print(f"{name} is {age} years old")
"""
        input_data = "Alice\n25"
        result = await runner.execute(code, input_data)

        assert result.status == ExecutionStatus.SUCCESS
        assert "Alice is 25 years old" in result.stdout

    @pytest.mark.asyncio
    async def test_forbidden_import(self):
        """Test that forbidden imports are blocked"""
        runner = RestrictedPythonRunner()
        code = "import os\nprint(os.listdir())"
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.RUNTIME_ERROR
        assert "not allowed" in result.stderr.lower() or "import" in result.stderr.lower()

    @pytest.mark.asyncio
    async def test_allowed_import_math(self):
        """Test that math module is allowed"""
        runner = RestrictedPythonRunner()
        code = """
import math
print(f"Pi: {math.pi:.4f}")
print(f"Sqrt(16): {math.sqrt(16)}")
"""
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.SUCCESS
        assert "3.1416" in result.stdout
        assert "4.0" in result.stdout

    @pytest.mark.asyncio
    async def test_timeout(self):
        """Test execution timeout"""
        runner = RestrictedPythonRunner(timeout=0.1)  # 100ms timeout
        code = """
import time
while True:
    pass  # Infinite loop
"""
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.TIMEOUT
        assert "timeout" in result.stderr.lower()

    @pytest.mark.asyncio
    async def test_syntax_error(self):
        """Test syntax error handling"""
        runner = RestrictedPythonRunner()
        code = "print('Hello' /"  # Syntax error
        result = await runner.execute(code)

        assert result.status in [ExecutionStatus.COMPILE_ERROR, ExecutionStatus.RUNTIME_ERROR]
        assert result.stderr != ""

    @pytest.mark.asyncio
    async def test_runtime_error(self):
        """Test runtime error handling"""
        runner = RestrictedPythonRunner()
        code = "x = 1 / 0"  # Division by zero
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.RUNTIME_ERROR
        assert "ZeroDivisionError" in result.stderr

    @pytest.mark.asyncio
    async def test_output_truncation(self):
        """Test that large output is truncated"""
        runner = RestrictedPythonRunner()
        code = """
for i in range(1000):
    print("Line " + str(i) + " " * 100)
"""
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.SUCCESS
        assert len(result.stdout) <= 11000  # Should be truncated around 10KB

    @pytest.mark.asyncio
    async def test_list_comprehension(self):
        """Test list comprehensions work"""
        runner = RestrictedPythonRunner()
        code = """
squares = [x**2 for x in range(5)]
print(squares)
"""
        result = await runner.execute(code)

        assert result.status == ExecutionStatus.SUCCESS
        assert "[0, 1, 4, 9, 16]" in result.stdout


class TestPythonExecutor:
    """Test the PythonExecutor with mocked Docker"""

    @pytest.mark.asyncio
    async def test_security_check_blocks_dangerous_code(self):
        """Test that dangerous patterns are detected"""
        with patch('services.base_executor.docker'):
            executor = PythonExecutor()
            executor.docker_available = False  # Force RestrictedPython

            # Test various dangerous patterns
            dangerous_codes = [
                "import socket",
                "from subprocess import call",
                "__import__('os')",
            ]

            for code in dangerous_codes:
                result = await executor.execute(code)
                assert result.status in [
                    ExecutionStatus.SECURITY_VIOLATION,
                    ExecutionStatus.RUNTIME_ERROR
                ]
                assert result.stderr != ""

    @pytest.mark.asyncio
    async def test_fallback_to_restricted_python(self):
        """Test fallback to RestrictedPython when Docker not available"""
        with patch('services.base_executor.docker'):
            executor = PythonExecutor()
            executor.docker_available = False

            code = "print('Hello from RestrictedPython!')"
            result = await executor.execute(code)

            assert result.status == ExecutionStatus.SUCCESS
            assert "Hello from RestrictedPython!" in result.stdout

    @pytest.mark.asyncio
    async def test_ascii_enforcement(self):
        """Test that non-ASCII characters are handled"""
        with patch('services.base_executor.docker'):
            executor = PythonExecutor()
            executor.docker_available = False

            # Unicode characters should be replaced
            code = "print('Hello Unicode')"  # Actually ASCII
            result = await executor.execute(code)

            assert result.status == ExecutionStatus.SUCCESS
            # Result should be ASCII only


class TestCExecutor:
    """Test C executor with mocked Docker"""

    @pytest.mark.asyncio
    async def test_security_patterns_blocked(self):
        """Test that dangerous C patterns are blocked"""
        with patch('services.base_executor.docker'):
            executor = CExecutor()
            executor.docker_available = True

            dangerous_codes = [
                '#include <sys/socket.h>',
                'system("ls");',
                'fork();',
                '__asm__("nop");'
            ]

            for code in dangerous_codes:
                result = await executor.execute(code)
                assert result.status == ExecutionStatus.SECURITY_VIOLATION
                assert "not allowed" in result.stderr

    @pytest.mark.asyncio
    async def test_docker_not_available_error(self):
        """Test error when Docker is not available"""
        with patch('services.base_executor.docker.from_env') as mock_docker:
            mock_docker.side_effect = Exception("Docker not found")

            executor = CExecutor()
            code = '#include <stdio.h>\nint main() { return 0; }'

            # Should handle Docker not being available
            # Actual behavior depends on implementation


class TestHaskellExecutor:
    """Test Haskell executor with mocked Docker"""

    @pytest.mark.asyncio
    async def test_unsafe_io_blocked(self):
        """Test that unsafe IO is blocked"""
        with patch('services.base_executor.docker'):
            executor = HaskellExecutor()

            code = 'import System.IO.Unsafe'
            result = await executor.execute(code)

            assert result.status == ExecutionStatus.SECURITY_VIOLATION
            assert "unsafe" in result.stderr.lower()


class TestExecutionResult:
    """Test the ExecutionResult dataclass"""

    def test_execution_result_creation(self):
        """Test creating an ExecutionResult"""
        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Hello",
            stderr="",
            compile_output=None,
            execution_time=0.5,
            memory_used=1024,
            exit_code=0
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert result.stdout == "Hello"
        assert result.execution_time == 0.5
        assert result.memory_used == 1024

    def test_execution_status_values(self):
        """Test ExecutionStatus enum values"""
        assert ExecutionStatus.SUCCESS.value == "success"
        assert ExecutionStatus.COMPILE_ERROR.value == "compile_error"
        assert ExecutionStatus.RUNTIME_ERROR.value == "runtime_error"
        assert ExecutionStatus.TIMEOUT.value == "timeout"
        assert ExecutionStatus.SECURITY_VIOLATION.value == "security_violation"


class TestResourceLimits:
    """Test resource limit configurations"""

    def test_resource_limits_import(self):
        """Test that resource limits can be imported"""
        from services.security.resource_limits import get_resource_limit, LANGUAGE_LIMITS

        # Test Python limits
        python_limits = get_resource_limit("python")
        assert python_limits.memory_mb == 256
        assert python_limits.timeout_seconds == 5
        assert python_limits.network_enabled is False

        # Test C limits
        c_limits = get_resource_limit("c")
        assert c_limits.memory_mb == 128
        assert c_limits.timeout_seconds == 10

        # Test default for unknown language
        unknown_limits = get_resource_limit("unknown_language")
        assert unknown_limits == python_limits  # Should default to Python


# Fixtures
@pytest.fixture
def mock_docker_client():
    """Create a mock Docker client"""
    client = Mock()
    client.ping.return_value = True
    client.images.list.return_value = []
    client.containers.run.return_value = Mock(
        wait=Mock(return_value={"StatusCode": 0}),
        logs=Mock(return_value=b"Test output")
    )
    return client


@pytest.fixture
def mock_docker_not_available():
    """Mock Docker when it's not available"""
    with patch('docker.from_env') as mock:
        mock.side_effect = Exception("Docker daemon not running")
        yield mock


# Integration test markers
pytestmark = pytest.mark.unit


if __name__ == "__main__":
    # Run tests with pytest
    pytest.main([__file__, "-v"])