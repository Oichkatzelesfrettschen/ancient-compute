# Ancient Compute - Language Executor Tests

import pytest

from ..base_executor import ExecutionStatus

# Note: These tests require Docker to be running and images to be built
# Run with: pytest backend/services/tests/test_executors.py -v


@pytest.mark.asyncio
@pytest.mark.skipif(
    not pytest.config.getoption("--run-docker", default=False),  # type: ignore[attr-defined]
    reason="Docker tests require --run-docker flag",
)
async def test_c_hello_world() -> None:
    """Test C hello world execution"""
    from ..docker_executor import CExecutor

    executor = CExecutor()
    code = """
#include <stdio.h>
int main() {
    printf("Hello, Ancient Compute!");
    return 0;
}
    """
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SUCCESS
    assert "Hello" in result.stdout


@pytest.mark.asyncio
async def test_c_security_violation() -> None:
    """Test C security violation detection"""
    from ..docker_executor import CExecutor

    executor = CExecutor()
    code = """
#include <stdio.h>
#include <sys/socket.h>
int main() {
    return 0;
}
    """
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SECURITY_VIOLATION
    assert "socket" in result.stderr.lower() or "sys/" in result.stderr


@pytest.mark.asyncio
async def test_python_security_violation() -> None:
    """Test Python security violation detection"""
    from ..docker_executor import PythonExecutor

    executor = PythonExecutor()
    code = """
import os  # Should be blocked
print("Should not execute")
    """
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SECURITY_VIOLATION
    assert "os" in result.stderr.lower()


def test_get_executor() -> None:
    """Test executor factory function"""
    from ..languages import get_executor

    c_executor = get_executor("c")
    assert c_executor is not None
    assert c_executor.language == "c"

    python_executor = get_executor("python")
    assert python_executor is not None
    assert python_executor.language == "python"

    invalid_executor = get_executor("invalid")
    assert invalid_executor is None


def test_resource_limits() -> None:
    """Test resource limit configuration"""
    from ..security.resource_limits import get_resource_limit

    c_limits = get_resource_limit("c")
    assert c_limits.memory_mb == 128
    assert c_limits.timeout_seconds == 10
    assert c_limits.network_enabled is False

    python_limits = get_resource_limit("python")
    assert python_limits.memory_mb == 256
    assert python_limits.timeout_seconds == 5


def test_source_filenames() -> None:
    """Test source filename generation"""
    from ..docker_executor import CExecutor, HaskellExecutor, PythonExecutor

    assert CExecutor()._get_source_filename() == "main.c"
    assert PythonExecutor()._get_source_filename() == "main.py"
    assert HaskellExecutor()._get_source_filename() == "Main.hs"
