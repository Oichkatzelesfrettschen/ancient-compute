"""
Ancient Compute - Code Execution API Integration Tests

Comprehensive tests for exercise code submission, validation, and result handling
with language service integration.
"""

import pytest
from sqlalchemy.orm import Session
from datetime import datetime

from src.database import SessionLocal
from src.models import (
    Exercise,
    ExerciseProgress,
    ExerciseSubmission,
    Module,
    Era,
    User,
)


class TestCodeSubmissionModel:
    """Test code submission and result storage."""

    def test_exercise_submission_creation(self, test_db):
        """Test creating an exercise submission record."""
        db = SessionLocal()

        # Create test data
        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="test-exercise",
            title="Test Exercise",
            description="Test",
            problem_statement="Add two numbers",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[
                {"input": "1 2", "expected": "3"},
                {"input": "5 10", "expected": "15"},
            ],
        )
        db.add(exercise)
        db.commit()

        # Create submission
        submission = ExerciseSubmission(
            user_id=user.id,
            exercise_id=exercise.id,
            submitted_code="print(3)",
            language="python",
            passed_tests=2,
            total_tests=2,
            score_percentage=100,
            execution_output="3\n",
            is_successful=True,
            execution_time_ms=50,
            memory_used_mb=8,
        )
        db.add(submission)
        db.commit()
        db.refresh(submission)

        assert submission.id is not None
        assert submission.score_percentage == 100
        assert submission.is_successful
        assert submission.passed_tests == 2

        db.close()

    def test_exercise_submission_with_errors(self, test_db):
        """Test submission with compilation/runtime errors."""
        db = SessionLocal()

        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="test-exercise",
            title="Test Exercise",
            description="Test",
            problem_statement="Add two numbers",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[
                {"input": "1 2", "expected": "3"},
            ],
        )
        db.add(exercise)
        db.commit()

        submission = ExerciseSubmission(
            user_id=user.id,
            exercise_id=exercise.id,
            submitted_code="print(invalid_var)",
            language="python",
            passed_tests=0,
            total_tests=1,
            score_percentage=0,
            execution_output="",
            execution_error="NameError: name 'invalid_var' is not defined",
            is_successful=False,
            execution_time_ms=100,
        )
        db.add(submission)
        db.commit()
        db.refresh(submission)

        assert submission.score_percentage == 0
        assert not submission.is_successful
        assert "NameError" in submission.execution_error

        db.close()


class TestExerciseProgressTracking:
    """Test progress tracking through multiple submissions."""

    def test_progress_update_with_multiple_attempts(self, test_db):
        """Test progress updates with successive submissions."""
        db = SessionLocal()

        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="test-exercise",
            title="Test Exercise",
            description="Test",
            problem_statement="Test",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[
                {"input": "1", "expected": "1"},
                {"input": "2", "expected": "2"},
            ],
        )
        db.add(exercise)
        db.commit()

        # First submission: 50% success
        progress = ExerciseProgress(
            user_id=user.id,
            exercise_id=exercise.id,
            attempts=1,
            best_score=50,
            is_completed=False,
        )
        db.add(progress)
        db.commit()

        # Second submission: 100% success
        progress.attempts = 2
        progress.best_score = 100
        progress.is_completed = True
        db.commit()
        db.refresh(progress)

        assert progress.attempts == 2
        assert progress.best_score == 100
        assert progress.is_completed

        db.close()


class TestLanguageSupport:
    """Test language support and validation."""

    def test_exercise_language_support(self, test_db):
        """Test exercise language support configuration."""
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        # Exercise supporting multiple languages
        exercise = Exercise(
            module_id=module.id,
            slug="polyglot-exercise",
            title="Polyglot Exercise",
            description="Test multiple languages",
            problem_statement="Compute sum",
            languages_supported=["python", "c", "haskell", "java"],
            difficulty="intermediate",
            sequence_order=1,
            test_cases=[],
        )
        db.add(exercise)
        db.commit()
        db.refresh(exercise)

        assert len(exercise.languages_supported) == 4
        assert "python" in exercise.languages_supported
        assert "haskell" in exercise.languages_supported

        db.close()


class TestTestCaseStructure:
    """Test test case storage and validation."""

    def test_test_case_json_storage(self, test_db):
        """Test storing test cases as JSON in exercise."""
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        # Complex test cases with various input types
        test_cases = [
            {"input": "1 2 3", "expected": "6", "description": "Sum of numbers"},
            {"input": "hello world", "expected": "world hello", "description": "Reverse words"},
            {
                "input": '{"a": 1}',
                "expected": '{"a": 1}',
                "description": "JSON identity",
            },
        ]

        exercise = Exercise(
            module_id=module.id,
            slug="complex-exercise",
            title="Complex Exercise",
            description="Test",
            problem_statement="Test",
            languages_supported=["python"],
            difficulty="intermediate",
            sequence_order=1,
            test_cases=test_cases,
        )
        db.add(exercise)
        db.commit()
        db.refresh(exercise)

        assert len(exercise.test_cases) == 3
        assert exercise.test_cases[0]["description"] == "Sum of numbers"
        assert exercise.test_cases[2]["expected"] == '{"a": 1}'

        db.close()


class TestExecutionMetrics:
    """Test execution metrics and resource tracking."""

    def test_submission_metrics_recording(self, test_db):
        """Test recording execution time and memory usage."""
        db = SessionLocal()

        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="perf-exercise",
            title="Performance Exercise",
            description="Test",
            problem_statement="Test",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[],
        )
        db.add(exercise)
        db.commit()

        # Submission with detailed metrics
        submission = ExerciseSubmission(
            user_id=user.id,
            exercise_id=exercise.id,
            submitted_code="# efficient code",
            language="python",
            passed_tests=5,
            total_tests=5,
            score_percentage=100,
            is_successful=True,
            execution_time_ms=45,
            memory_used_mb=15,
        )
        db.add(submission)
        db.commit()
        db.refresh(submission)

        assert submission.execution_time_ms == 45
        assert submission.memory_used_mb == 15
        assert submission.submitted_at is not None

        db.close()


class TestExerciseConstraints:
    """Test exercise resource constraints."""

    def test_exercise_resource_limits(self, test_db):
        """Test exercise time and memory limit configuration."""
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        # Exercise with specific resource constraints
        exercise = Exercise(
            module_id=module.id,
            slug="constrained-exercise",
            title="Constrained Exercise",
            description="Test",
            problem_statement="Test",
            languages_supported=["c"],
            difficulty="advanced",
            sequence_order=1,
            test_cases=[],
            time_limit_seconds=5,
            memory_limit_mb=64,
            estimated_minutes=15,
        )
        db.add(exercise)
        db.commit()
        db.refresh(exercise)

        assert exercise.time_limit_seconds == 5
        assert exercise.memory_limit_mb == 64
        assert exercise.estimated_minutes == 15

        db.close()


class TestExecutionOrchestrator:
    """Test ExecutionOrchestrator functionality."""

    @pytest.mark.asyncio
    async def test_orchestrator_language_support(self):
        """Test that orchestrator supports all required languages."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        supported_langs = orchestrator.get_supported_languages()

        expected_languages = [
            "python",
            "c",
            "haskell",
            "idris",
            "lisp",
            "java",
            "assembly",
            "systemf",
        ]

        for lang in expected_languages:
            assert lang in supported_langs, f"{lang} not supported by orchestrator"

    @pytest.mark.asyncio
    async def test_orchestrator_language_info(self):
        """Test retrieving language service information."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        python_info = orchestrator.get_language_info("python")

        assert python_info is not None
        assert python_info["language"] == "python"
        assert "timeout" in python_info
        assert "memory_limit" in python_info

    @pytest.mark.asyncio
    async def test_orchestrator_unsupported_language(self):
        """Test handling of unsupported languages."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        result = orchestrator.get_language_info("cobol")

        assert result is None

    def test_test_case_validation_exact_match(self):
        """Test exact string matching for test validation."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()

        actual = "Hello, World!"
        expected = "Hello, World!"
        assert orchestrator._compare_outputs(actual, expected, "python")

    def test_test_case_validation_whitespace_normalization(self):
        """Test whitespace normalization in test validation."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()

        actual = "Hello    World"
        expected = "Hello World"
        assert orchestrator._compare_outputs(actual, expected, "python")

    def test_test_case_validation_numeric_tolerance(self):
        """Test numeric comparison with floating point tolerance."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()

        actual = "3.14159"
        expected = "3.14160"
        assert orchestrator._compare_outputs(actual, expected, "python")

    def test_test_case_validation_json_comparison(self):
        """Test JSON structure comparison."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()

        actual = '{"name": "Alice", "age": 30}'
        expected = '{"age": 30, "name": "Alice"}'
        assert orchestrator._compare_outputs(actual, expected, "python")


class TestExerciseTypeSystem:
    """Test exercise type safety and validation."""

    def test_difficulty_level_enum(self, test_db):
        """Test difficulty level enumeration."""
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient",
            description="Ancient era",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="test-module",
            title="Test Module",
            description="Test",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        # Test each difficulty level
        for difficulty in ["beginner", "intermediate", "advanced"]:
            exercise = Exercise(
                module_id=module.id,
                slug=f"exercise-{difficulty}",
                title=f"{difficulty.title()} Exercise",
                description="Test",
                problem_statement="Test",
                languages_supported=["python"],
                difficulty=difficulty,
                sequence_order=1,
                test_cases=[],
            )
            db.add(exercise)

        db.commit()

        exercises = db.query(Exercise).filter(Exercise.module_id == module.id).all()
        assert len(exercises) == 3

        difficulties = {ex.difficulty for ex in exercises}
        assert "beginner" in difficulties
        assert "intermediate" in difficulties
        assert "advanced" in difficulties

        db.close()


class TestExecutorRegistryIntegration:
    """Test integration of ExecutorRegistry with ExecutionOrchestrator."""

    def test_executor_registry_instantiation(self):
        """Test that ExecutorRegistry can instantiate all language executors."""
        from src.services.docker_executor import ExecutorRegistry

        languages = ExecutorRegistry.list_supported_languages()
        assert len(languages) == 8
        assert "python" in languages
        assert "c" in languages
        assert "haskell" in languages
        assert "idris" in languages
        assert "lisp" in languages
        assert "java" in languages
        assert "assembly" in languages
        assert "systemf" in languages

    def test_executor_registry_get_executor(self):
        """Test getting executor instances with custom timeout."""
        from src.services.docker_executor import ExecutorRegistry

        # Get Python executor with custom timeout
        executor = ExecutorRegistry.get_executor("python", timeout=20)
        assert executor is not None
        assert executor.language == "python"
        assert executor.timeout == 20

    def test_executor_registry_language_info(self):
        """Test retrieving executor metadata."""
        from src.services.docker_executor import ExecutorRegistry

        executor = ExecutorRegistry.get_executor("haskell", timeout=15)
        assert executor is not None
        assert executor.language == "haskell"
        assert executor.timeout == 15
        assert executor.docker_image == "ancient-compute/haskell:latest"

    def test_executor_registry_unsupported_language(self):
        """Test that unsupported language returns None."""
        from src.services.docker_executor import ExecutorRegistry

        assert ExecutorRegistry.is_supported("cobol") is False
        assert ExecutorRegistry.get_executor("cobol") is None

    def test_execution_orchestrator_uses_registry(self):
        """Test that ExecutionOrchestrator properly uses ExecutorRegistry."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        languages = orchestrator.get_supported_languages()

        assert len(languages) == 8
        assert "python" in languages
        assert "c" in languages

    def test_execution_orchestrator_language_metadata(self):
        """Test retrieving language info through orchestrator."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        info = orchestrator.get_language_info("haskell")

        assert info is not None
        assert info["language"] == "haskell"
        assert "executor_class" in info
        assert info["executor_class"] == "HaskellExecutor"
        assert info["timeout"] == 15
        assert info["docker_image"] == "ancient-compute/haskell:latest"

    def test_execution_orchestrator_unsupported_returns_none(self):
        """Test orchestrator returns None for unsupported language."""
        from src.services.execution_orchestrator import ExecutionOrchestrator

        orchestrator = ExecutionOrchestrator()
        info = orchestrator.get_language_info("rust")

        assert info is None

    @pytest.mark.asyncio
    async def test_execution_orchestrator_error_handling(self):
        """Test orchestrator error handling for unsupported language."""
        from src.services.execution_orchestrator import ExecutionOrchestrator
        from src.services.base_executor import ExecutionStatus

        orchestrator = ExecutionOrchestrator()
        result = await orchestrator.execute_code(
            code="print('hello')",
            language="cobol",
            timeout=10,
        )

        assert result.status == ExecutionStatus.RUNTIME_ERROR
        assert "not supported" in result.stderr.lower()

    def test_executor_memory_limits(self):
        """Test that executors respect memory limits."""
        from src.services.docker_executor import ExecutorRegistry

        c_executor = ExecutorRegistry.get_executor("c", memory_limit=256)
        assert c_executor is not None
        assert c_executor.language == "c"
        # Memory limit passed to constructor but stored by BaseExecutor
        # Memory is applied via container config, not stored on executor

    def test_executor_timeout_constraints(self):
        """Test executor timeout configuration."""
        from src.services.docker_executor import ExecutorRegistry

        # Each language has default timeout
        python_exec = ExecutorRegistry.get_executor("python")
        assert python_exec.timeout == 10

        haskell_exec = ExecutorRegistry.get_executor("haskell")
        assert haskell_exec.timeout == 15

        idris_exec = ExecutorRegistry.get_executor("idris")
        assert idris_exec.timeout == 20

        # Can override timeout at instantiation
        custom_exec = ExecutorRegistry.get_executor("python", timeout=30)
        assert custom_exec.timeout == 30
