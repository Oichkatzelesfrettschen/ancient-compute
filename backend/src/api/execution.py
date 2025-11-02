"""
Ancient Compute - Code Execution API Endpoints

Endpoints for submitting and validating code against exercises, with integration
to language services for polyglot code execution in Docker containers.
"""

from fastapi import APIRouter, Depends, HTTPException, status
from pydantic import BaseModel
from sqlalchemy.orm import Session
from typing import Optional, List
from datetime import datetime

from ..database import get_db
from ..models import (
    Exercise,
    ExerciseSubmission,
    ExerciseProgress,
    User,
    Module,
)
from ..services.execution_orchestrator import ExecutionOrchestrator

router = APIRouter(prefix="/exercises", tags=["execution"])


# ============================================================================
# REQUEST/RESPONSE MODELS
# ============================================================================


class CodeSubmissionRequest(BaseModel):
    """Request model for code submission."""

    code: str
    language: str


class TestCaseResult(BaseModel):
    """Result of a single test case."""

    test_case_index: int
    input: str
    expected_output: str
    actual_output: str
    passed: bool


class CodeExecutionResult(BaseModel):
    """Result of code execution."""

    status: str  # success, compile_error, runtime_error, timeout, memory_exceeded
    stdout: str
    stderr: str
    execution_time_ms: float
    memory_used_mb: Optional[int]
    exit_code: int
    test_results: Optional[List[TestCaseResult]] = None
    passed_tests: Optional[int] = None
    total_tests: Optional[int] = None
    score_percentage: Optional[int] = None


class ExerciseSubmissionResponse(BaseModel):
    """Response model for exercise submission."""

    submission_id: int
    exercise_id: int
    user_id: int
    language: str
    execution_result: CodeExecutionResult
    passed_tests: int
    total_tests: int
    score_percentage: int
    is_successful: bool
    submitted_at: datetime


# ============================================================================
# CODE EXECUTION ENDPOINTS
# ============================================================================


@router.post(
    "/{exercise_id}/submit",
    response_model=ExerciseSubmissionResponse,
    summary="Submit code for an exercise",
)
async def submit_exercise_code(
    exercise_id: int,
    request: CodeSubmissionRequest,
    db: Session = Depends(get_db),
) -> ExerciseSubmissionResponse:
    """
    Submit code for evaluation against exercise test cases.

    Executes the provided code in a sandboxed Docker container and validates
    it against the exercise's test cases. Stores submission and updates progress.

    Args:
        exercise_id: Exercise database ID
        request: Code submission with language
        db: Database session

    Returns:
        Submission result with test case validation
    """
    # Validate exercise exists
    exercise = db.query(Exercise).filter(Exercise.id == exercise_id).first()
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")

    # Validate language is supported
    if request.language not in exercise.languages_supported:
        raise HTTPException(
            status_code=400,
            detail=f"Language {request.language} not supported for this exercise. "
            f"Supported: {', '.join(exercise.languages_supported)}",
        )

    # Execute code
    orchestrator = ExecutionOrchestrator()
    execution_result = await orchestrator.execute_code(
        code=request.code,
        language=request.language,
        exercise=exercise,
    )

    # Validate against test cases
    test_results, passed_tests, total_tests = await orchestrator.validate_test_cases(
        execution_result=execution_result,
        test_cases=exercise.test_cases,
        exercise_language=request.language,
    )

    # Calculate score
    score_percentage = (passed_tests // total_tests * 100) if total_tests > 0 else 0
    is_successful = passed_tests == total_tests

    # Create submission record
    submission = ExerciseSubmission(
        user_id=1,  # TODO: Get from auth context
        exercise_id=exercise_id,
        submitted_code=request.code[:5000],  # Limit stored code
        language=request.language,
        passed_tests=passed_tests,
        total_tests=total_tests,
        score_percentage=score_percentage,
        execution_output=execution_result.stdout[:2000],  # Limit output
        execution_error=execution_result.stderr[:2000],  # Limit errors
        is_successful=is_successful,
        execution_time_ms=int(execution_result.execution_time * 1000),
        memory_used_mb=execution_result.memory_used,
    )
    db.add(submission)

    # Update or create progress
    progress = db.query(ExerciseProgress).filter(
        ExerciseProgress.user_id == 1,
        ExerciseProgress.exercise_id == exercise_id,
    ).first()

    if progress:
        progress.attempts += 1
        progress.best_score = max(progress.best_score or 0, score_percentage)
        progress.is_completed = is_successful
        progress.is_submitted = True
    else:
        progress = ExerciseProgress(
            user_id=1,
            exercise_id=exercise_id,
            attempts=1,
            best_score=score_percentage,
            is_completed=is_successful,
            is_submitted=True,
        )
        db.add(progress)

    db.commit()
    db.refresh(submission)

    return ExerciseSubmissionResponse(
        submission_id=submission.id,
        exercise_id=exercise_id,
        user_id=submission.user_id,
        language=request.language,
        execution_result=CodeExecutionResult(
            status=execution_result.status.value,
            stdout=execution_result.stdout[:5000],
            stderr=execution_result.stderr[:5000],
            execution_time_ms=execution_result.execution_time * 1000,
            memory_used_mb=execution_result.memory_used,
            exit_code=execution_result.exit_code,
            test_results=test_results,
            passed_tests=passed_tests,
            total_tests=total_tests,
            score_percentage=score_percentage,
        ),
        passed_tests=passed_tests,
        total_tests=total_tests,
        score_percentage=score_percentage,
        is_successful=is_successful,
        submitted_at=submission.submitted_at,
    )


@router.post(
    "/{exercise_id}/validate",
    response_model=CodeExecutionResult,
    summary="Validate code without storing submission",
)
async def validate_exercise_code(
    exercise_id: int,
    request: CodeSubmissionRequest,
    db: Session = Depends(get_db),
) -> CodeExecutionResult:
    """
    Validate code against exercise test cases without storing submission.

    Useful for immediate feedback during development without recording attempts.

    Args:
        exercise_id: Exercise database ID
        request: Code submission with language
        db: Database session

    Returns:
        Execution result with test validation
    """
    # Validate exercise exists
    exercise = db.query(Exercise).filter(Exercise.id == exercise_id).first()
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")

    # Validate language is supported
    if request.language not in exercise.languages_supported:
        raise HTTPException(
            status_code=400,
            detail=f"Language {request.language} not supported for this exercise. "
            f"Supported: {', '.join(exercise.languages_supported)}",
        )

    # Execute code
    orchestrator = ExecutionOrchestrator()
    execution_result = await orchestrator.execute_code(
        code=request.code,
        language=request.language,
        exercise=exercise,
    )

    # Validate against test cases
    test_results, passed_tests, total_tests = await orchestrator.validate_test_cases(
        execution_result=execution_result,
        test_cases=exercise.test_cases,
        exercise_language=request.language,
    )

    # Calculate score
    score_percentage = (passed_tests // total_tests * 100) if total_tests > 0 else 0

    return CodeExecutionResult(
        status=execution_result.status.value,
        stdout=execution_result.stdout[:5000],
        stderr=execution_result.stderr[:5000],
        execution_time_ms=execution_result.execution_time * 1000,
        memory_used_mb=execution_result.memory_used,
        exit_code=execution_result.exit_code,
        test_results=test_results,
        passed_tests=passed_tests,
        total_tests=total_tests,
        score_percentage=score_percentage,
    )


@router.get(
    "/{exercise_id}/submissions",
    summary="Get user's submission history for an exercise",
)
async def get_exercise_submissions(
    exercise_id: int,
    db: Session = Depends(get_db),
):
    """
    Get user's submission history for an exercise.

    Returns list of previous submissions with execution results and scores.

    Args:
        exercise_id: Exercise database ID
        db: Database session

    Returns:
        List of submissions with metadata
    """
    exercise = db.query(Exercise).filter(Exercise.id == exercise_id).first()
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")

    submissions = (
        db.query(ExerciseSubmission)
        .filter(
            ExerciseSubmission.exercise_id == exercise_id,
            ExerciseSubmission.user_id == 1,  # TODO: Get from auth context
        )
        .order_by(ExerciseSubmission.submitted_at.desc())
        .limit(20)
        .all()
    )

    return {
        "exercise_id": exercise_id,
        "submission_count": len(submissions),
        "submissions": [
            {
                "id": sub.id,
                "language": sub.language,
                "submitted_at": sub.submitted_at,
                "score_percentage": sub.score_percentage,
                "is_successful": sub.is_successful,
                "passed_tests": sub.passed_tests,
                "total_tests": sub.total_tests,
                "execution_time_ms": sub.execution_time_ms,
            }
            for sub in submissions
        ],
    }


@router.get(
    "/{exercise_id}/progress",
    summary="Get user's progress on an exercise",
)
async def get_exercise_progress(
    exercise_id: int,
    db: Session = Depends(get_db),
):
    """
    Get user's progress tracking for an exercise.

    Shows attempt count, best score, and completion status.

    Args:
        exercise_id: Exercise database ID
        db: Database session

    Returns:
        Progress data with attempt history
    """
    exercise = db.query(Exercise).filter(Exercise.id == exercise_id).first()
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")

    progress = (
        db.query(ExerciseProgress)
        .filter(
            ExerciseProgress.exercise_id == exercise_id,
            ExerciseProgress.user_id == 1,  # TODO: Get from auth context
        )
        .first()
    )

    if not progress:
        return {
            "exercise_id": exercise_id,
            "attempts": 0,
            "best_score": None,
            "is_completed": False,
            "is_submitted": False,
        }

    return {
        "exercise_id": exercise_id,
        "attempts": progress.attempts,
        "best_score": progress.best_score,
        "is_completed": progress.is_completed,
        "is_submitted": progress.is_submitted,
        "started_at": progress.started_at,
        "completed_at": progress.completed_at,
    }
