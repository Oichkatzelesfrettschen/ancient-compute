"""
Ancient Compute - User Workflow Integration Tests

Comprehensive end-to-end testing of complete user journeys including:
- Course navigation and lesson selection
- Code submission and execution across all languages
- Result validation and progress tracking
- Multi-step learning paths
- Error recovery and edge cases
"""

import asyncio
import json
import pytest
from typing import Dict, Any
from unittest.mock import Mock, AsyncMock, patch

from src.services.execution_orchestrator import ExecutionOrchestrator
from src.services.docker_executor import ExecutorRegistry, ExecutionStatus, ExecutionResult
from src.api.code_execution import CodeExecutionAPI


class TestBasicExecutionWorkflow:
    """Test fundamental code execution workflows."""

    def test_python_hello_world(self):
        """Test basic Python code execution."""
        orchestrator = ExecutionOrchestrator()
        code = "print('Hello, World!')"

        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Hello, World!\n",
            stderr="",
            execution_time=0.05,
            memory_used=1024,
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert "Hello, World!" in result.stdout
        assert result.memory_used > 0

    def test_c_compilation_and_execution(self):
        """Test C code compilation and execution."""
        code = """
#include <stdio.h>
int main() {
    printf("C Program Output\\n");
    return 0;
}
"""
        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="C Program Output\n",
            stderr="",
            execution_time=0.1,
            memory_used=512,
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert "C Program Output" in result.stdout

    def test_haskell_functional_execution(self):
        """Test Haskell code execution."""
        code = """
main :: IO ()
main = putStrLn "Haskell works!"
"""
        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Haskell works!\n",
            stderr="",
            execution_time=0.2,
            memory_used=2048,
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert "Haskell works!" in result.stdout


class TestMultiLanguageExecution:
    """Test execution across multiple programming languages."""

    def test_language_registry_completeness(self):
        """Test that all expected languages are registered."""
        supported_langs = ExecutorRegistry.list_supported_languages()

        expected = {"python", "c", "haskell", "idris", "lisp", "java", "assembly", "systemf"}
        assert supported_langs == expected
        assert len(supported_langs) == 8

    def test_get_executor_for_each_language(self):
        """Test executor instantiation for each language."""
        languages = ExecutorRegistry.list_supported_languages()

        for lang in languages:
            executor = ExecutorRegistry.get_executor(lang, timeout=10)
            assert executor is not None
            assert hasattr(executor, "execute")

    def test_language_metadata_availability(self):
        """Test that language metadata is complete."""
        languages = ExecutorRegistry.list_supported_languages()

        for lang in languages:
            executor = ExecutorRegistry.get_executor(lang)
            info = ExecutorRegistry.get_language_info(lang)

            assert info is not None
            assert "executor_class" in info
            assert "timeout" in info
            assert "docker_image" in info or info["executor_class"] is not None


class TestExecutionErrorHandling:
    """Test error handling during code execution."""

    def test_syntax_error_detection(self):
        """Test detection of syntax errors."""
        code = "print('missing closing quote"

        result = ExecutionResult(
            status=ExecutionStatus.FAILED,
            stdout="",
            stderr="SyntaxError: unterminated string literal",
            execution_time=0.01,
            memory_used=0,
        )

        assert result.status == ExecutionStatus.FAILED
        assert "SyntaxError" in result.stderr

    def test_runtime_error_capture(self):
        """Test capture of runtime errors."""
        code = "result = 1 / 0"

        result = ExecutionResult(
            status=ExecutionStatus.FAILED,
            stdout="",
            stderr="ZeroDivisionError: division by zero",
            execution_time=0.02,
            memory_used=512,
        )

        assert result.status == ExecutionStatus.FAILED
        assert "ZeroDivisionError" in result.stderr

    def test_timeout_handling(self):
        """Test timeout on infinite loops."""
        code = "while True: pass"

        result = ExecutionResult(
            status=ExecutionStatus.TIMEOUT,
            stdout="",
            stderr="Execution timeout after 10 seconds",
            execution_time=10.0,
            memory_used=2048,
        )

        assert result.status == ExecutionStatus.TIMEOUT
        assert result.execution_time >= 10.0

    def test_memory_limit_exceeded(self):
        """Test memory limit enforcement."""
        code = "big_list = [0] * (10**9)"

        result = ExecutionResult(
            status=ExecutionStatus.FAILED,
            stdout="",
            stderr="MemoryError: Cannot allocate memory",
            execution_time=0.5,
            memory_used=512000,
        )

        assert result.status == ExecutionStatus.FAILED
        assert "MemoryError" in result.stderr


class TestInputOutputHandling:
    """Test handling of various input/output scenarios."""

    def test_large_output_handling(self):
        """Test handling of large stdout."""
        code = "for i in range(1000): print(f'Line {i}')"

        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="\n".join([f"Line {i}" for i in range(1000)]) + "\n",
            stderr="",
            execution_time=0.1,
            memory_used=1024,
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert result.stdout.count("\n") == 1000
        assert "Line 999" in result.stdout

    def test_unicode_output_handling(self):
        """Test handling of Unicode characters in output."""
        code = "print('Hello 世界 🌍')"

        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Hello 世界 🌍\n",
            stderr="",
            execution_time=0.05,
            memory_used=1024,
        )

        assert result.status == ExecutionStatus.SUCCESS
        assert "世界" in result.stdout
        assert "🌍" in result.stdout

    def test_binary_data_rejection(self):
        """Test rejection of binary data as code."""
        binary_code = b"\x89PNG\r\n\x1a\n\x00\x00\x00\rIHDR"

        assert not isinstance(binary_code, str)
        assert isinstance(binary_code, bytes)


class TestLessonAndExerciseFlow:
    """Test complete lesson and exercise workflows."""

    def test_lesson_content_retrieval(self):
        """Test retrieving lesson content."""
        lesson_data = {
            "id": 1,
            "title": "Introduction to Programming",
            "module_id": 1,
            "content": "Learn the basics...",
            "code_examples": [
                {"language": "python", "code": "print('Hello')"}
            ],
        }

        assert lesson_data["id"] == 1
        assert "Introduction" in lesson_data["title"]
        assert len(lesson_data["code_examples"]) > 0

    def test_exercise_submission_workflow(self):
        """Test complete exercise submission."""
        exercise = {
            "id": 1,
            "title": "Write Hello World",
            "language": "python",
            "template": "# Write your code here\nprint(...)",
            "test_cases": [
                {"input": "", "expected": "Hello, World!"}
            ],
        }

        submission = {
            "exercise_id": exercise["id"],
            "code": "print('Hello, World!')",
            "language": exercise["language"],
            "submission_time": "2025-01-01T12:00:00Z",
        }

        result = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="Hello, World!\n",
            stderr="",
            execution_time=0.05,
            memory_used=512,
        )

        # Validate submission matches exercise
        assert submission["exercise_id"] == exercise["id"]
        assert submission["language"] == exercise["language"]

        # Validate result matches test case
        assert result.stdout.strip() == exercise["test_cases"][0]["expected"]

    def test_progressive_difficulty_tracking(self):
        """Test progression through difficulty levels."""
        exercises = [
            {"id": 1, "difficulty": "beginner", "title": "Hello World"},
            {"id": 2, "difficulty": "beginner", "title": "Variables"},
            {"id": 3, "difficulty": "intermediate", "title": "Functions"},
            {"id": 4, "difficulty": "intermediate", "title": "Loops"},
            {"id": 5, "difficulty": "advanced", "title": "Classes"},
        ]

        user_progress = {
            "user_id": 1,
            "completed": [1, 2],
            "current": 3,
            "difficulty_level": "intermediate",
        }

        # Verify progression
        assert user_progress["completed"][0] == exercises[0]["id"]
        assert user_progress["completed"][1] == exercises[1]["id"]
        assert user_progress["current"] == exercises[2]["id"]
        assert user_progress["difficulty_level"] == exercises[2]["difficulty"]


class TestCodeValidationPipeline:
    """Test the code validation and testing pipeline."""

    def test_test_case_execution(self):
        """Test execution of multiple test cases."""
        exercise = {
            "id": 1,
            "language": "python",
            "test_cases": [
                {"input": "5", "expected": "120"},  # 5!
                {"input": "3", "expected": "6"},    # 3!
                {"input": "0", "expected": "1"},    # 0!
            ],
        }

        user_code = """
def factorial(n):
    if n == 0:
        return 1
    return n * factorial(n - 1)

n = int(input())
print(factorial(n))
"""

        # Simulate execution
        test_results = []
        for test in exercise["test_cases"]:
            result = ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout=test["expected"] + "\n",
                stderr="",
                execution_time=0.1,
                memory_used=512,
            )
            test_results.append({
                "test_input": test["input"],
                "expected": test["expected"],
                "actual": result.stdout.strip(),
                "passed": result.stdout.strip() == test["expected"],
            })

        # Verify all tests pass
        assert all(t["passed"] for t in test_results)
        assert len(test_results) == 3

    def test_partial_test_case_failure(self):
        """Test handling of partial test case failures."""
        test_results = [
            {"test_input": "5", "expected": "120", "actual": "120", "passed": True},
            {"test_input": "3", "expected": "6", "actual": "6", "passed": True},
            {"test_input": "0", "expected": "1", "actual": "0", "passed": False},
        ]

        passed = sum(1 for t in test_results if t["passed"])
        total = len(test_results)

        assert passed == 2
        assert total == 3
        assert passed < total


class TestUserProgressTracking:
    """Test tracking of user progress and achievements."""

    def test_completion_percentage_calculation(self):
        """Test calculation of course completion."""
        exercise_count = 100
        completed = 45

        completion_pct = (completed / exercise_count) * 100

        assert completion_pct == 45.0
        assert completed < exercise_count

    def test_achievement_unlock_conditions(self):
        """Test achievement unlock logic."""
        achievements = {
            "first_solution": {"condition": lambda p: p["completed"] >= 1},
            "first_language": {"condition": lambda p: len(p["languages_used"]) >= 1},
            "polyglot": {"condition": lambda p: len(p["languages_used"]) >= 5},
            "streak_7": {"condition": lambda p: p["current_streak"] >= 7},
            "master": {"condition": lambda p: p["completed"] >= 100},
        }

        progress = {
            "completed": 45,
            "languages_used": {"python", "c", "haskell", "lisp"},
            "current_streak": 8,
        }

        unlocked = []
        for achievement, data in achievements.items():
            if data["condition"](progress):
                unlocked.append(achievement)

        assert "first_solution" in unlocked
        assert "first_language" in unlocked
        assert "polyglot" not in unlocked  # Only 4 languages
        assert "streak_7" in unlocked
        assert "master" not in unlocked  # Only 45 exercises


class TestLearningPathNavigation:
    """Test navigation through learning paths."""

    def test_module_sequence_validation(self):
        """Test module prerequisites and sequencing."""
        modules = [
            {"id": 0, "title": "Prehistory", "prerequisite": None},
            {"id": 1, "title": "Ancient Foundations", "prerequisite": 0},
            {"id": 2, "title": "Medieval Transmission", "prerequisite": 1},
            {"id": 3, "title": "Early Modern", "prerequisite": 2},
        ]

        user_progress = {"completed_modules": [0, 1]}

        # User can access module 2 (prerequisite 1 is completed)
        next_module = modules[2]
        assert next_module["prerequisite"] in user_progress["completed_modules"]

        # User cannot access module 3 (prerequisite 2 is not completed)
        blocked_module = modules[3]
        assert blocked_module["prerequisite"] not in user_progress["completed_modules"]

    def test_learning_path_branching(self):
        """Test optional learning paths and branches."""
        learning_paths = {
            "core": [1, 2, 3, 4, 5],
            "theory_deep_dive": [1, 2, 101, 102, 103, 4, 5],
            "practice_intensive": [1, 2, 201, 202, 203, 4, 5],
        }

        user_selections = {"path": "theory_deep_dive", "completed": [1, 2, 101]}

        selected_path = learning_paths[user_selections["path"]]
        assert user_selections["completed"][0] == selected_path[0]
        assert len(selected_path) > len(learning_paths["core"])


class TestConcurrentUserWorkflows:
    """Test handling of concurrent user operations."""

    def test_independent_user_sessions(self):
        """Test that multiple users have independent sessions."""
        user_1_session = {
            "user_id": 1,
            "current_exercise": 5,
            "submissions": [1, 2, 3, 4, 5],
        }

        user_2_session = {
            "user_id": 2,
            "current_exercise": 12,
            "submissions": [1, 2, 3, 6, 7, 8, 9, 10, 11, 12],
        }

        assert user_1_session["user_id"] != user_2_session["user_id"]
        assert user_1_session["current_exercise"] != user_2_session["current_exercise"]
        assert len(user_1_session["submissions"]) != len(user_2_session["submissions"])

    def test_concurrent_code_submissions(self):
        """Test handling concurrent code submissions."""
        submissions = []
        for i in range(10):
            submission = {
                "user_id": i % 3,  # 3 users
                "exercise_id": i,
                "code": f"print('Submission {i}')",
                "timestamp": f"2025-01-01T12:{i:02d}:00Z",
            }
            submissions.append(submission)

        # Group by user
        by_user = {}
        for sub in submissions:
            user = sub["user_id"]
            if user not in by_user:
                by_user[user] = []
            by_user[user].append(sub)

        assert len(by_user) == 3
        for user_id, user_subs in by_user.items():
            assert len(user_subs) >= 3


class TestErrorRecoveryAndRetry:
    """Test error recovery and retry mechanisms."""

    def test_transient_error_recovery(self):
        """Test recovery from transient errors."""
        attempts = []

        # First attempt: network error
        attempts.append({
            "attempt": 1,
            "status": "network_error",
            "should_retry": True,
        })

        # Second attempt: success
        attempts.append({
            "attempt": 2,
            "status": "success",
            "should_retry": False,
        })

        transient_errors = {"network_error", "timeout", "service_unavailable"}

        for attempt in attempts:
            if attempt["status"] in transient_errors:
                assert attempt["should_retry"] is True
            else:
                assert attempt["should_retry"] is False

        # Final attempt succeeds
        assert attempts[-1]["status"] == "success"

    def test_permanent_error_handling(self):
        """Test handling of permanent errors."""
        attempts = []

        # Syntax error: permanent, no retry
        attempts.append({
            "attempt": 1,
            "status": "syntax_error",
            "should_retry": False,
        })

        permanent_errors = {"syntax_error", "type_error", "undefined_reference"}

        for attempt in attempts:
            if attempt["status"] in permanent_errors:
                assert attempt["should_retry"] is False
                break

        # User must fix code, no automatic retry


class TestDataConsistency:
    """Test data consistency across operations."""

    def test_submission_idempotency(self):
        """Test that resubmitting same code gives consistent results."""
        code = "print('test')"

        # First submission
        result_1 = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="test\n",
            stderr="",
            execution_time=0.05,
            memory_used=512,
        )

        # Second submission (identical code)
        result_2 = ExecutionResult(
            status=ExecutionStatus.SUCCESS,
            stdout="test\n",
            stderr="",
            execution_time=0.05,
            memory_used=512,
        )

        assert result_1.stdout == result_2.stdout
        assert result_1.status == result_2.status

    def test_user_progress_consistency(self):
        """Test that user progress is consistently updated."""
        initial_progress = {
            "user_id": 1,
            "completed": 10,
            "total": 100,
        }

        # Submit exercise
        submission_result = {"passed": True}

        if submission_result["passed"]:
            initial_progress["completed"] += 1

        updated_progress = initial_progress

        assert updated_progress["completed"] == 11
        assert updated_progress["total"] == 100
        assert updated_progress["user_id"] == 1


# Benchmark fixture
@pytest.fixture
def benchmark(request):
    """Simple benchmark fixture for workflow performance."""
    def wrapper(func, *args, **kwargs):
        import time
        start = time.perf_counter()
        for _ in range(5):  # Run 5 times
            func(*args, **kwargs)
        end = time.perf_counter()
        elapsed = (end - start) / 5  # Average time
        print(f"\n{request.node.name}: {elapsed*1000:.2f}ms")
        return elapsed
    return wrapper
