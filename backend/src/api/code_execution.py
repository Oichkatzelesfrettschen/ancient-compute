# Ancient Compute - Code Execution API
from typing import Literal, Optional

from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field
from sqlalchemy.orm import Session

from ..database import get_db
from ..models import CodeSubmission
from ..services.languages import get_executor

router = APIRouter(prefix="/execute", tags=["code-execution"])


class ExecutionRequest(BaseModel):
    """Request model for code execution"""

    language: Literal["c", "python", "haskell", "idris", "lisp", "assembly", "java", "systemf"]
    code: str = Field(..., max_length=50000, description="Source code to execute")
    input_data: str = Field(default="", max_length=10000, description="Input data for program")
    lesson_id: Optional[int] = Field(default=None, description="Associated lesson ID")


class ExecutionResponse(BaseModel):
    """Response model for code execution"""

    status: str
    stdout: str
    stderr: str
    compile_output: Optional[str] = None
    execution_time: float
    memory_used: int = 0


@router.post("/run", response_model=ExecutionResponse)
async def execute_code(request: ExecutionRequest, db: Session = Depends(get_db)):
    """
    Execute code in a sandboxed environment.

    Supports 8 programming languages with security restrictions:
    - C (GCC)
    - Python (RestrictedPython)
    - Haskell (GHC)
    - IDRIS2
    - LISP (SBCL)
    - Assembly (NASM x86-64)
    - Java (OpenJDK)
    - System F

    Returns execution results with stdout, stderr, and timing information.
    """
    # Get executor for requested language
    executor = get_executor(request.language)

    if not executor:
        raise HTTPException(
            status_code=400, detail=f"Language '{request.language}' is not currently supported"
        )

    # Execute code in sandboxed container
    result = await executor.execute(request.code, request.input_data)

    # Save submission to database if lesson_id provided
    if request.lesson_id:
        # Placeholder for current user
        current_user_id = 1
        submission = CodeSubmission(
            user_id=current_user_id,
            lesson_id=request.lesson_id,
            submitted_code=request.code,
            language=request.language,
            execution_output=result.stdout,
            execution_error=result.stderr,
            is_successful=(result.status.value == "success"),
        )
        db.add(submission)
        db.commit()

    return ExecutionResponse(
        status=result.status.value,
        stdout=result.stdout,
        stderr=result.stderr,
        compile_output=result.compile_output,
        execution_time=result.execution_time,
        memory_used=result.memory_used,
    )


@router.get("/languages")
async def get_supported_languages():
    """
    Get list of supported programming languages.

    Returns language metadata including version information.
    """
    return {
        "languages": [
            {
                "id": "c",
                "name": "C",
                "version": "GCC 12.2",
                "description": "C language with GCC compiler",
                "timeout": 10,
                "memory_limit_mb": 128,
            },
            {
                "id": "python",
                "name": "Python",
                "version": "3.11 (Restricted)",
                "description": "Python with RestrictedPython sandboxing",
                "timeout": 5,
                "memory_limit_mb": 256,
            },
            {
                "id": "haskell",
                "name": "Haskell",
                "version": "GHC 9.2",
                "description": "Haskell with Glasgow Haskell Compiler",
                "timeout": 15,
                "memory_limit_mb": 512,
            },
            {
                "id": "idris",
                "name": "IDRIS2",
                "version": "0.6.0",
                "description": "IDRIS2 with dependent types",
                "timeout": 20,
                "memory_limit_mb": 512,
            },
            {
                "id": "lisp",
                "name": "Common LISP",
                "version": "SBCL 2.3",
                "description": "Common LISP with SBCL",
                "timeout": 10,
                "memory_limit_mb": 256,
            },
            {
                "id": "assembly",
                "name": "Assembly",
                "version": "NASM x86-64",
                "description": "x86-64 Assembly with NASM",
                "timeout": 5,
                "memory_limit_mb": 64,
            },
            {
                "id": "java",
                "name": "Java",
                "version": "OpenJDK 17",
                "description": "Java with OpenJDK",
                "timeout": 15,
                "memory_limit_mb": 256,
            },
            {
                "id": "systemf",
                "name": "System F",
                "version": "Academic",
                "description": "System F polymorphic lambda calculus",
                "timeout": 10,
                "memory_limit_mb": 128,
            },
        ]
    }


@router.get("/health")
async def execution_service_health():
    """Health check for code execution service"""
    return {
        "status": "healthy",
        "service": "code-execution",
        "languages_available": 3,  # C, Python, Haskell currently implemented
    }
