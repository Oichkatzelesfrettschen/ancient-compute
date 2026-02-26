# Ancient Compute - Code Execution API
from __future__ import annotations

import inspect
from typing import Any

from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session

from ..auth import UserResponse, get_current_user
from ..database import get_db
from ..models import CodeSubmission
from ..services.languages import (
    get_executor,
    language_status_summary,
    list_language_capabilities,
    normalize_language_id,
)

router = APIRouter(prefix="/execute", tags=["code-execution"])


class ExecutionRequest(BaseModel):
    """Request model for code execution"""

    language: str = Field(..., description="Language id or alias (for example: c, python, idris2)")
    code: str = Field(..., max_length=50000, description="Source code to execute")
    input_data: str = Field(default="", max_length=10000, description="Input data for program")
    lesson_id: int | None = Field(default=None, description="Associated lesson ID")


class ExecutionResponse(BaseModel):
    """Response model for code execution"""

    status: str
    stdout: str
    stderr: str
    compile_output: str | None = None
    execution_time: float
    memory_used: int = 0


def _to_execution_response(result: Any) -> ExecutionResponse:
    """Normalize mixed executor result shapes into the API response schema."""
    status = getattr(result, "status", "runtime_error")
    status_value = status.value if hasattr(status, "value") else str(status)

    stdout = getattr(result, "stdout", getattr(result, "output", ""))
    stderr = getattr(result, "stderr", getattr(result, "errors", ""))

    compile_output = getattr(result, "compile_output", None)
    if compile_output is None:
        compile_output = getattr(result, "assembly_text", None)

    execution_time = getattr(result, "execution_time", None)
    if execution_time is None:
        execution_time = getattr(result, "compilation_time", None)
    if execution_time is None:
        compile_time_ms = getattr(result, "compile_time_ms", 0)
        execution_time = float(compile_time_ms) / 1000.0

    memory_used = int(getattr(result, "memory_used", 0) or 0)

    return ExecutionResponse(
        status=status_value,
        stdout=stdout,
        stderr=stderr,
        compile_output=compile_output,
        execution_time=float(execution_time or 0.0),
        memory_used=memory_used,
    )


async def _execute_with_compatible_signature(
    executor: Any,
    code: str,
    input_data: str,
) -> Any:
    """Call executor.execute across heterogeneous service signatures."""
    execute_fn = executor.execute
    params = inspect.signature(execute_fn).parameters

    if "input_data" in params:
        return await execute_fn(code, input_data)

    # Some legacy services expose execute(source, timeout_seconds=...)
    # and should not receive input_data as a positional second argument.
    return await execute_fn(code)


@router.post("/run", response_model=ExecutionResponse)
async def execute_code(
    request: ExecutionRequest,
    db: Session = Depends(get_db),
    current_user: UserResponse = Depends(get_current_user),
):
    """
    Execute code in a sandboxed environment.

    Supported language IDs and aliases are exposed by GET /api/v1/execute/languages.
    Implementation readiness differs by service and is reported in metadata.

    Returns execution results with stdout, stderr, and timing information.
    """
    canonical_language = normalize_language_id(request.language)
    if canonical_language is None:
        raise HTTPException(
            status_code=400,
            detail=(
                f"Language '{request.language}' is not currently supported. "
                "See GET /api/v1/execute/languages for valid ids and aliases."
            ),
        )

    # Get executor for requested language
    executor = get_executor(canonical_language)

    if not executor:
        raise HTTPException(
            status_code=400,
            detail=f"Language '{request.language}' is not currently supported",
        )

    # Execute code in language-specific execution path
    result = await _execute_with_compatible_signature(
        executor=executor,
        code=request.code,
        input_data=request.input_data,
    )
    response = _to_execution_response(result)

    # Save submission to database if lesson_id provided
    if request.lesson_id:
        current_user_id = current_user.id
        submission = CodeSubmission(
            user_id=current_user_id,
            lesson_id=request.lesson_id,
            submitted_code=request.code,
            language=canonical_language,
            execution_output=response.stdout,
            execution_error=response.stderr,
            is_successful=response.status == "success",
        )
        db.add(submission)
        db.commit()

    return response


@router.get("/languages")
async def get_supported_languages():
    """
    Get list of supported programming languages.

    Returns language metadata including version information.
    """
    return {
        "languages": list_language_capabilities(),
        "summary": language_status_summary(),
    }


@router.get("/health")
async def execution_service_health():
    """Health check for code execution service"""
    summary = language_status_summary()
    return {
        "status": "healthy",
        "service": "code-execution",
        "languages_available": summary["non_stub"],
        "languages_total": summary["total"],
        "languages_by_status": {
            "implemented": summary["implemented"],
            "partial": summary["partial"],
            "stub": summary["stub"],
        },
    }
