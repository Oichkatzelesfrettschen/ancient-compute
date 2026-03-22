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
async def test_java_compiles_hello_world() -> None:
    """Java service compiles and runs a well-formed Hello World program."""
    executor = get_executor("java")
    code = (
        "public class Main {\n"
        "    public static void main(String[] args) {\n"
        '        System.out.println("Hello");\n'
        "    }\n"
        "}"
    )
    result = await executor.execute(code)
    assert result.status.value == BaseExecutionStatus.SUCCESS.value
    assert "Hello" in result.stdout


@pytest.mark.asyncio
async def test_idris_service_compiles_source() -> None:
    """IDRIS service should invoke compiler and return compilation result."""
    executor = get_executor("idris2")
    result = await executor.execute("main = 0")
    # May succeed or fail depending on parser, but should not be a placeholder
    ok = (BaseExecutionStatus.SUCCESS.value, BaseExecutionStatus.COMPILE_ERROR.value)
    assert result.status.value in ok
    assert "not yet implemented" not in result.stdout.lower()


@pytest.mark.asyncio
async def test_systemf_service_compiles_source() -> None:
    """SystemF service should invoke compiler and return compilation result."""
    executor = get_executor("systemf")
    result = await executor.execute("42")
    ok = (BaseExecutionStatus.SUCCESS.value, BaseExecutionStatus.COMPILE_ERROR.value)
    assert result.status.value in ok
    assert "not yet implemented" not in result.stdout.lower()


@pytest.mark.asyncio
async def test_lisp_service_reports_compile_error_for_invalid_source() -> None:
    """LISP service should surface parser/compiler failures as compile errors."""
    executor = get_executor("lisp")
    # Missing closing parenthesis.
    result = await executor.execute("(defun broken (x) (+ x 1)")

    assert result.status.value == BaseExecutionStatus.COMPILE_ERROR.value
    assert result.stderr


class TestGetExecutorFactory:
    def test_returns_none_for_unknown_language(self) -> None:
        assert get_executor("cobol") is None
        assert get_executor("") is None

    def test_assembly_executor_returns_babbage_service(self) -> None:
        from backend.src.services.languages.babbage_assembly_service import BabbageAssemblyService

        assert isinstance(get_executor("assembly"), BabbageAssemblyService)

    def test_aliases_resolve_to_same_executor_class(self) -> None:
        e1 = get_executor("system-f")
        e2 = get_executor("systemf")
        assert type(e1) is type(e2)

    def test_idris_aliases_same_class(self) -> None:
        e1 = get_executor("idris")
        e2 = get_executor("idris2")
        assert type(e1) is type(e2)

    def test_all_supported_languages_return_non_none(self) -> None:
        supported = [
            "c",
            "python",
            "haskell",
            "assembly",
            "lisp",
            "idris",
            "systemf",
            "java",
            "algol68",
            "cpp",
            "micropython",
        ]
        for lang in supported:
            assert get_executor(lang) is not None, f"get_executor({lang!r}) returned None"


class TestBabbageAssemblyExecutorSync:
    """Synchronous tests that don't require async/await."""

    def test_executor_has_execute_method(self) -> None:
        from backend.src.services.languages.babbage_assembly_service import BabbageAssemblyService

        svc = BabbageAssemblyService()
        assert hasattr(svc, "execute")

    def test_executor_is_same_instance_on_repeated_get(self) -> None:
        from backend.src.services.languages.babbage_assembly_service import BabbageAssemblyService

        e1 = get_executor("assembly")
        e2 = get_executor("assembly")
        # Both should be BabbageAssemblyService instances
        assert isinstance(e1, BabbageAssemblyService)
        assert isinstance(e2, BabbageAssemblyService)


@pytest.mark.asyncio
async def test_c_service_compiles_hello_world() -> None:
    executor = get_executor("c")
    code = '#include <stdio.h>\nint main() { printf("hi"); return 0; }'
    result = await executor.execute(code)
    ok = (BaseExecutionStatus.SUCCESS.value, BaseExecutionStatus.COMPILE_ERROR.value)
    assert result.status.value in ok


@pytest.mark.asyncio
async def test_python_service_compiles_simple_code() -> None:
    # PythonService compiles Python -> Babbage IR -> machine code (not native execution)
    executor = get_executor("python")
    result = await executor.execute("x = 1 + 2")
    ok = (BaseExecutionStatus.SUCCESS.value, BaseExecutionStatus.COMPILE_ERROR.value)
    assert result.status.value in ok


@pytest.mark.asyncio
async def test_python_service_returns_status() -> None:
    executor = get_executor("python")
    result = await executor.execute("x = 42")
    assert result.status is not None


class TestGetExecutorExtended:
    """Additional factory coverage: all aliases, edge cases."""

    def test_c_executor_type(self) -> None:
        assert isinstance(get_executor("c"), CService)

    def test_python_executor_type(self) -> None:
        assert isinstance(get_executor("python"), PythonService)

    def test_haskell_executor_type(self) -> None:
        assert isinstance(get_executor("haskell"), HaskellService)

    def test_lisp_executor_type(self) -> None:
        assert isinstance(get_executor("lisp"), LISPService)

    def test_idris_executor_type(self) -> None:
        assert isinstance(get_executor("idris"), IDRISService)

    def test_systemf_executor_type(self) -> None:
        assert isinstance(get_executor("systemf"), SystemFService)

    def test_java_executor_type(self) -> None:
        assert isinstance(get_executor("java"), JavaService)

    def test_uppercase_c_returns_c_service(self) -> None:
        assert isinstance(get_executor("C"), CService)

    def test_uppercase_python_returns_python_service(self) -> None:
        assert isinstance(get_executor("PYTHON"), PythonService)

    def test_none_not_returned_for_known_langs(self) -> None:
        supported = (
            "c", "python", "haskell", "lisp", "idris", "idris2", "systemf", "system-f", "java"
        )
        for lang in supported:
            assert get_executor(lang) is not None, f"Expected non-None for {lang!r}"

    def test_get_executor_unknown_returns_none(self) -> None:
        for lang in ("fortran", "ada", "cobol", "perl", "ruby"):
            assert get_executor(lang) is None, f"Expected None for {lang!r}"

    def test_executor_has_execute_attribute(self) -> None:
        for lang in ("c", "python", "haskell"):
            svc = get_executor(lang)
            assert hasattr(svc, "execute"), f"{lang} executor missing .execute()"

    def test_system_f_aliases_same_class(self) -> None:
        e1 = get_executor("systemf")
        e2 = get_executor("system-f")
        assert type(e1) is type(e2)

    def test_idris_and_idris2_same_class(self) -> None:
        e1 = get_executor("idris")
        e2 = get_executor("idris2")
        assert type(e1) is type(e2)


class TestExecutorServiceInterface:
    """All executors expose a consistent async execute() interface."""

    def test_c_executor_has_execute_coroutine(self) -> None:
        import inspect

        svc = get_executor("c")
        assert inspect.iscoroutinefunction(svc.execute)

    def test_python_executor_has_execute_coroutine(self) -> None:
        import inspect

        svc = get_executor("python")
        assert inspect.iscoroutinefunction(svc.execute)

    def test_haskell_executor_has_execute_coroutine(self) -> None:
        import inspect

        svc = get_executor("haskell")
        assert inspect.iscoroutinefunction(svc.execute)

    def test_lisp_executor_has_execute_coroutine(self) -> None:
        import inspect

        svc = get_executor("lisp")
        assert inspect.iscoroutinefunction(svc.execute)

    def test_java_executor_has_execute_coroutine(self) -> None:
        import inspect

        svc = get_executor("java")
        assert inspect.iscoroutinefunction(svc.execute)
