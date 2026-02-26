"""Unit tests for language executor factory and service wiring.

These tests validate the active service API used by `backend/src/api/code_execution.py`
and replace the stale Jan 2026 placeholder module.
"""

from __future__ import annotations

import pytest

from backend.src.services.base_executor import ExecutionStatus as BaseExecutionStatus
from backend.src.services.languages import (
    CService,
    HaskellService,
    IDRISService,
    JavaService,
    LISPService,
    PythonService,
    SystemFService,
    get_executor,
)


def test_get_executor_returns_expected_service_types() -> None:
    """Factory returns the currently wired concrete service classes."""
    assert isinstance(get_executor("c"), CService)
    assert isinstance(get_executor("python"), PythonService)
    assert isinstance(get_executor("haskell"), HaskellService)
    assert isinstance(get_executor("lisp"), LISPService)
    assert isinstance(get_executor("idris"), IDRISService)
    assert isinstance(get_executor("idris2"), IDRISService)
    assert isinstance(get_executor("systemf"), SystemFService)
    assert isinstance(get_executor("system-f"), SystemFService)
    assert isinstance(get_executor("java"), JavaService)


def test_get_executor_is_case_insensitive_and_rejects_unknown() -> None:
    """Factory accepts mixed-case input and rejects unsupported languages."""
    assert isinstance(get_executor("PyThOn"), PythonService)
    assert get_executor("cobol") is None
    assert get_executor("brainfuck") is None


@pytest.mark.asyncio
async def test_java_stub_returns_placeholder_success() -> None:
    """Java (only remaining stub) should return a placeholder message."""
    executor = get_executor("java")
    result = await executor.execute("main = 0")
    assert result.status == BaseExecutionStatus.SUCCESS
    assert "not yet implemented" in result.stdout.lower()


@pytest.mark.asyncio
async def test_idris_service_compiles_source() -> None:
    """IDRIS service should invoke compiler and return compilation result."""
    executor = get_executor("idris2")
    result = await executor.execute("main = 0")
    # May succeed or fail depending on parser, but should not be a placeholder
    assert result.status in (BaseExecutionStatus.SUCCESS, BaseExecutionStatus.COMPILE_ERROR)
    assert "not yet implemented" not in result.stdout.lower()


@pytest.mark.asyncio
async def test_systemf_service_compiles_source() -> None:
    """SystemF service should invoke compiler and return compilation result."""
    executor = get_executor("systemf")
    result = await executor.execute("42")
    assert result.status in (BaseExecutionStatus.SUCCESS, BaseExecutionStatus.COMPILE_ERROR)
    assert "not yet implemented" not in result.stdout.lower()


@pytest.mark.asyncio
async def test_lisp_service_reports_compile_error_for_invalid_source() -> None:
    """LISP service should surface parser/compiler failures as compile errors."""
    executor = get_executor("lisp")
    # Missing closing parenthesis.
    result = await executor.execute("(defun broken (x) (+ x 1)")

    assert result.status == BaseExecutionStatus.COMPILE_ERROR
    assert result.stderr
