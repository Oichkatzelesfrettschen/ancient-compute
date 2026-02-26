"""
Ancient Compute - Execution Orchestrator

Coordinates code execution across all supported language services with
unified interface for test case validation and result handling.
"""

import asyncio
import json
from typing import Any

from .base_executor import ExecutionResult, ExecutionStatus
from .docker_executor import ExecutorRegistry
from .docker_manager import DockerManager

# Language service mappings to Docker images and configurations
LANGUAGE_CONFIG = {
    "python": {
        "image": "ancient-compute/python:latest",
        "extensions": ".py",
        "executor_class": "PythonExecutor",
    },
    "c": {
        "image": "ancient-compute/c:latest",
        "extensions": ".c",
        "executor_class": "CExecutor",
    },
    "haskell": {
        "image": "ancient-compute/haskell:latest",
        "extensions": ".hs",
        "executor_class": "HaskellExecutor",
    },
    "idris": {
        "image": "ancient-compute/idris:latest",
        "extensions": ".idr",
        "executor_class": "IdrisExecutor",
    },
    "lisp": {
        "image": "ancient-compute/lisp:latest",
        "extensions": ".lisp",
        "executor_class": "LispExecutor",
    },
    "java": {
        "image": "ancient-compute/java:latest",
        "extensions": ".java",
        "executor_class": "JavaExecutor",
    },
    "assembly": {
        "image": "ancient-compute/assembly:latest",
        "extensions": ".asm",
        "executor_class": "AssemblyExecutor",
    },
    "systemf": {
        "image": "ancient-compute/systemf:latest",
        "extensions": ".sf",
        "executor_class": "SystemFExecutor",
    },
}


class ExecutionOrchestrator:
    """
    Orchestrates code execution across all supported languages.

    Maps language names to Docker-based execution services and provides unified
    interface for code execution, test case validation, and result handling.
    """

    def __init__(self):
        """Initialize execution orchestrator with Docker manager."""
        self.docker_manager = DockerManager()
        self.language_config = LANGUAGE_CONFIG

    async def execute_code(
        self,
        code: str,
        language: str,
        exercise: Any | None = None,
        timeout: int = 10,
    ) -> ExecutionResult:
        """
        Execute code in specified language with resource constraints.

        Args:
            code: Source code to execute
            language: Programming language (python, c, haskell, etc.)
            exercise: Optional Exercise model for constraints
            timeout: Execution timeout in seconds

        Returns:
            ExecutionResult with status, output, and metrics
        """
        language = language.lower()

        # Check if language is supported
        if not ExecutorRegistry.is_supported(language):
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=f"Language '{language}' not supported. "
                f"Supported: {', '.join(ExecutorRegistry.list_supported_languages())}",
            )

        # Get execution constraints from exercise
        execution_timeout = timeout
        if exercise:
            execution_timeout = exercise.time_limit_seconds

        try:
            # Get executor for language with appropriate timeout
            executor = ExecutorRegistry.get_executor(
                language,
                timeout=execution_timeout,
            )

            if executor is None:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=f"Failed to create executor for language '{language}'",
                )

            # Execute code with timeout
            result = await asyncio.wait_for(
                executor.execute(code),
                timeout=execution_timeout + 5,  # Give buffer beyond service timeout
            )
            return result

        except TimeoutError:
            return ExecutionResult(
                status=ExecutionStatus.TIMEOUT,
                stdout="",
                stderr=f"Execution timeout ({execution_timeout}s exceeded)",
            )
        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=f"Execution error: {str(e)}",
            )


    async def validate_test_cases(
        self,
        execution_result: ExecutionResult,
        test_cases: list[dict[str, Any]],
        exercise_language: str = "python",
    ) -> tuple[list[dict[str, Any]], int, int]:
        """
        Validate execution result against test cases.

        Compares actual output with expected output for each test case.

        Args:
            execution_result: Result from code execution
            test_cases: List of test case dicts with 'input' and 'expected'
            exercise_language: Language for output formatting

        Returns:
            Tuple of (test_results, passed_count, total_count)
        """
        if not test_cases:
            # No test cases = automatic pass
            return [], 0, 0

        test_results = []
        passed_tests = 0
        total_tests = len(test_cases)

        # If execution failed, all tests fail
        if execution_result.status != ExecutionStatus.SUCCESS:
            for i, test_case in enumerate(test_cases):
                test_results.append(
                    {
                        "test_case_index": i,
                        "input": test_case.get("input", ""),
                        "expected_output": test_case.get("expected", ""),
                        "actual_output": "",
                        "passed": False,
                        "error": f"Execution failed: {execution_result.status.value}",
                    }
                )
            return test_results, 0, total_tests

        # Parse output based on language
        output_lines = self._parse_output(
            execution_result.stdout,
            exercise_language,
        )

        # Compare each test case
        for i, test_case in enumerate(test_cases):
            expected = str(test_case.get("expected", "")).strip()
            actual = output_lines[i].strip() if i < len(output_lines) else ""

            passed = self._compare_outputs(actual, expected, exercise_language)
            if passed:
                passed_tests += 1

            test_results.append(
                {
                    "test_case_index": i,
                    "input": test_case.get("input", ""),
                    "expected_output": expected,
                    "actual_output": actual,
                    "passed": passed,
                }
            )

        return test_results, passed_tests, total_tests

    def _parse_output(self, output: str, language: str) -> list[str]:
        """
        Parse execution output into lines.

        Language-specific parsing for different output formats.

        Args:
            output: Raw stdout from execution
            language: Programming language for parsing hints

        Returns:
            List of output lines
        """
        if not output:
            return []

        # Language-specific parsing (most print one result per line)
        if language == "haskell":
            # Haskell might need special parsing for lists/tuples
            return [line.strip() for line in output.strip().split("\n") if line.strip()]
        elif language == "lisp":
            # Lisp output might be S-expressions
            return [line.strip() for line in output.strip().split("\n") if line.strip()]
        else:
            # Default: split by newlines
            return [line.strip() for line in output.strip().split("\n") if line.strip()]

    def _compare_outputs(
        self,
        actual: str,
        expected: str,
        language: str,
    ) -> bool:
        """
        Compare actual and expected outputs with language-aware normalization.

        Args:
            actual: Actual output from execution
            expected: Expected output from test case
            language: Programming language for comparison logic

        Returns:
            True if outputs match (within tolerance)
        """
        actual = actual.strip()
        expected = expected.strip()

        # Exact match
        if actual == expected:
            return True

        # Numeric comparison (useful for floating point tolerance)
        if language in ["python", "c", "haskell"]:
            try:
                actual_num = float(actual)
                expected_num = float(expected)
                # Allow small floating point error
                return abs(actual_num - expected_num) < 1e-6
            except (ValueError, TypeError):
                pass

        # Whitespace-insensitive comparison
        actual_normalized = " ".join(actual.split())
        expected_normalized = " ".join(expected.split())
        if actual_normalized == expected_normalized:
            return True

        # JSON comparison (for complex data structures)
        try:
            actual_json = json.loads(actual)
            expected_json = json.loads(expected)
            return actual_json == expected_json
        except (json.JSONDecodeError, TypeError):
            pass

        return False

    def get_supported_languages(self) -> list[str]:
        """Get list of supported programming languages."""
        return ExecutorRegistry.list_supported_languages()

    def get_language_info(self, language: str) -> dict[str, Any] | None:
        """Get information about a specific language service."""
        language = language.lower()
        if not ExecutorRegistry.is_supported(language):
            return None

        executor = ExecutorRegistry.get_executor(language)
        if executor is None:
            return None

        return {
            "language": language,
            "executor_class": executor.__class__.__name__,
            "timeout": executor.timeout,
            "docker_image": executor.docker_image,
        }
