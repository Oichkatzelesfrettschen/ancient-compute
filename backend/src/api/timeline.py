# Ancient Compute - Timeline Content Delivery Endpoints
"""
FastAPI endpoints for serving the 12,500-year educational timeline.

Provides full content delivery for:
- All 8 historical eras with metadata
- 50+ educational modules
- 300+ lessons with content and code examples
- 150+ coding exercises with test cases
- User progress tracking
"""

from fastapi import APIRouter, Depends, HTTPException, status
from sqlalchemy.orm import Session

from ..database import get_db
from ..models import Era, Module, Lesson, Exercise, User

router = APIRouter(prefix="/timeline", tags=["timeline"])


# ============================================================================
# ERA ENDPOINTS
# ============================================================================


@router.get("/eras", summary="List all historical eras")
async def list_eras(db: Session = Depends(get_db)):
    """Get all 8 historical eras spanning 12,500 years of computational history.

    Returns eras in chronological order with metadata for UI visualization.
    """
    eras = db.query(Era).order_by(Era.order).all()
    if not eras:
        return {"eras": [], "count": 0}

    return {
        "eras": [
            {
                "id": era.id,
                "label": era.label,
                "fullName": era.full_name,
                "description": era.description,
                "historicalContext": era.historical_context,
                "startYear": era.start_year,
                "endYear": era.end_year,
                "color": era.color,
                "icon": era.icon,
                "order": era.order,
                "moduleCount": len(era.modules),
            }
            for era in eras
        ],
        "count": len(eras),
    }


@router.get("/eras/{era_id}", summary="Get era details with modules")
async def get_era_detail(era_id: int, db: Session = Depends(get_db)):
    """Get detailed information about a specific era including all its modules.

    Args:
        era_id: Era database ID

    Returns:
        Era metadata plus all associated modules
    """
    era = db.query(Era).filter(Era.id == era_id).first()
    if not era:
        raise HTTPException(status_code=404, detail="Era not found")

    return {
        "id": era.id,
        "label": era.label,
        "fullName": era.full_name,
        "description": era.description,
        "historicalContext": era.historical_context,
        "startYear": era.start_year,
        "endYear": era.end_year,
        "color": era.color,
        "icon": era.icon,
        "order": era.order,
        "modules": [
            {
                "id": module.id,
                "title": module.title,
                "description": module.description,
                "order": module.sequence_order,
                "estimatedTime": module.estimated_hours,
                "difficulty": module.difficulty_level,
                "lessonCount": len(module.lessons),
                "exerciseCount": len(module.exercises),
            }
            for module in era.modules
        ],
    }


# ============================================================================
# MODULE ENDPOINTS
# ============================================================================


@router.get("/modules", summary="List all modules")
async def list_modules(db: Session = Depends(get_db)):
    """Get all educational modules across all eras."""
    modules = db.query(Module).order_by(Module.sequence_order).all()

    return {
        "modules": [
            {
                "id": module.id,
                "eraId": module.era_id,
                "title": module.title,
                "description": module.description,
                "order": module.sequence_order,
                "estimatedTime": module.estimated_hours,
                "difficulty": module.difficulty_level,
                "lessonCount": len(module.lessons),
                "exerciseCount": len(module.exercises),
            }
            for module in modules
        ],
        "count": len(modules),
    }


@router.get("/modules/{module_id}", summary="Get module details with lessons and exercises")
async def get_module_detail(module_id: int, db: Session = Depends(get_db)):
    """Get detailed information about a specific module.

    Args:
        module_id: Module database ID

    Returns:
        Module metadata plus all lessons and exercises
    """
    module = db.query(Module).filter(Module.id == module_id).first()
    if not module:
        raise HTTPException(status_code=404, detail="Module not found")

    return {
        "id": module.id,
        "eraId": module.era_id,
        "title": module.title,
        "description": module.description,
        "order": module.sequence_order,
        "estimatedTime": module.estimated_hours,
        "difficulty": module.difficulty_level,
        "lessons": [
            {
                "id": lesson.id,
                "title": lesson.title,
                "description": lesson.description,
                "type": lesson.lesson_type,
                "order": lesson.sequence_order,
                "estimatedMinutes": lesson.estimated_minutes,
                "optional": lesson.is_optional,
            }
            for lesson in module.lessons
        ],
        "exercises": [
            {
                "id": exercise.id,
                "title": exercise.title,
                "description": exercise.description,
                "difficulty": exercise.difficulty,
                "languages": exercise.languages_supported,
                "order": exercise.sequence_order,
                "estimatedMinutes": exercise.estimated_minutes,
                "optional": exercise.is_optional,
            }
            for exercise in module.exercises
        ],
    }


# ============================================================================
# LESSON ENDPOINTS
# ============================================================================


@router.get("/lessons/{lesson_id}", summary="Get lesson details with content")
async def get_lesson_detail(lesson_id: int, db: Session = Depends(get_db)):
    """Get detailed lesson content including markdown and code examples.

    Args:
        lesson_id: Lesson database ID

    Returns:
        Complete lesson content with all metadata
    """
    lesson = db.query(Lesson).filter(Lesson.id == lesson_id).first()
    if not lesson:
        raise HTTPException(status_code=404, detail="Lesson not found")

    return {
        "id": lesson.id,
        "moduleId": lesson.module_id,
        "title": lesson.title,
        "description": lesson.description,
        "type": lesson.lesson_type,
        "order": lesson.sequence_order,
        "estimatedMinutes": lesson.estimated_minutes,
        "optional": lesson.is_optional,
        "content": lesson.content_markdown,
        "contentJson": lesson.content_json,
        "language": lesson.language,
        "starterCode": lesson.starter_code,
        "solutionCode": lesson.solution_code,
        "testCases": lesson.test_cases,
        "createdAt": lesson.created_at.isoformat() if lesson.created_at else None,
        "updatedAt": lesson.updated_at.isoformat() if lesson.updated_at else None,
    }


# ============================================================================
# EXERCISE ENDPOINTS
# ============================================================================


@router.get("/exercises/{exercise_id}", summary="Get exercise details with test cases")
async def get_exercise_detail(exercise_id: int, db: Session = Depends(get_db)):
    """Get detailed exercise content including problem statement and test cases.

    Args:
        exercise_id: Exercise database ID

    Returns:
        Complete exercise content with all metadata and test cases
    """
    exercise = db.query(Exercise).filter(Exercise.id == exercise_id).first()
    if not exercise:
        raise HTTPException(status_code=404, detail="Exercise not found")

    return {
        "id": exercise.id,
        "moduleId": exercise.module_id,
        "title": exercise.title,
        "description": exercise.description,
        "problemStatement": exercise.problem_statement,
        "difficulty": exercise.difficulty,
        "languages": exercise.languages_supported,
        "order": exercise.sequence_order,
        "estimatedMinutes": exercise.estimated_minutes,
        "optional": exercise.is_optional,
        "starterCode": exercise.starter_code,
        "solutionCode": exercise.solution_code,
        "testCases": exercise.test_cases,
        "hints": exercise.hints,
        "explanation": exercise.explanation,
        "timeLimitSeconds": exercise.time_limit_seconds,
        "memoryLimitMb": exercise.memory_limit_mb,
        "createdAt": exercise.created_at.isoformat() if exercise.created_at else None,
        "updatedAt": exercise.updated_at.isoformat() if exercise.updated_at else None,
    }


# ============================================================================
# FULL TIMELINE ENDPOINT
# ============================================================================


@router.get("/full", summary="Get complete timeline with all content")
async def get_full_timeline(db: Session = Depends(get_db)):
    """Get the entire 12,500-year timeline with all eras, modules, lessons, and exercises.

    Warning: Large response - typically 1-5 MB. Consider fetching by era for performance.

    Returns:
        Complete hierarchical timeline data suitable for frontend state initialization
    """
    eras = db.query(Era).order_by(Era.order).all()

    timeline_data = {
        "eras": [
            {
                "id": era.id,
                "label": era.label,
                "fullName": era.full_name,
                "description": era.description,
                "historicalContext": era.historical_context,
                "startYear": era.start_year,
                "endYear": era.end_year,
                "color": era.color,
                "icon": era.icon,
                "order": era.order,
                "modules": [
                    {
                        "id": module.id,
                        "eraId": era.id,
                        "title": module.title,
                        "description": module.description,
                        "order": module.sequence_order,
                        "estimatedTime": module.estimated_hours,
                        "difficulty": module.difficulty_level,
                        "lessons": [
                            {
                                "id": lesson.id,
                                "moduleId": module.id,
                                "eraId": era.id,
                                "title": lesson.title,
                                "description": lesson.description,
                                "type": lesson.lesson_type,
                                "order": lesson.sequence_order,
                                "estimatedMinutes": lesson.estimated_minutes,
                                "optional": lesson.is_optional,
                                "content": lesson.content_markdown,
                            }
                            for lesson in module.lessons
                        ],
                        "exercises": [
                            {
                                "id": exercise.id,
                                "moduleId": module.id,
                                "eraId": era.id,
                                "title": exercise.title,
                                "description": exercise.description,
                                "difficulty": exercise.difficulty,
                                "languages": exercise.languages_supported,
                                "order": exercise.sequence_order,
                                "estimatedMinutes": exercise.estimated_minutes,
                                "optional": exercise.is_optional,
                                "problemStatement": exercise.problem_statement,
                            }
                            for exercise in module.exercises
                        ],
                    }
                    for module in era.modules
                ],
            }
            for era in eras
        ]
    }

    return {
        "timeline": timeline_data,
        "metadata": {
            "totalEras": len(eras),
            "totalModules": sum(len(era.modules) for era in eras),
            "totalLessons": sum(
                len(lesson) for era in eras for module in era.modules for lesson in [module.lessons]
            ),
            "totalExercises": sum(
                len(exercise)
                for era in eras
                for module in era.modules
                for exercise in [module.exercises]
            ),
        },
    }


# ============================================================================
# TIMELINE METADATA ENDPOINT
# ============================================================================


@router.get("/metadata", summary="Get timeline metadata and statistics")
async def get_timeline_metadata(db: Session = Depends(get_db)):
    """Get summary statistics about the entire timeline.

    Useful for UI initialization and progress tracking.
    """
    eras = db.query(Era).all()
    modules = db.query(Module).all()
    lessons = db.query(Lesson).all()
    exercises = db.query(Exercise).all()

    return {
        "statistics": {
            "totalEras": len(eras),
            "totalModules": len(modules),
            "totalLessons": len(lessons),
            "totalExercises": len(exercises),
            "yearSpan": {
                "start": min((e.start_year for e in eras), default=0),
                "end": max((e.end_year for e in eras), default=2025),
            },
        },
        "erasSummary": [
            {
                "id": era.id,
                "label": era.label,
                "moduleCount": len(era.modules),
            }
            for era in sorted(eras, key=lambda e: e.order)
        ],
    }
