# Ancient Compute - Exercise Model
"""
Exercise model for hands-on coding challenges within modules.

Exercises are distinct from lessons - they are interactive coding problems
that require compilation and testing, complementing the theoretical content
of lessons.
"""

import enum

from sqlalchemy import JSON, Boolean, Column, DateTime, Enum, ForeignKey, Integer, String, Text
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func

from ..database import Base


class DifficultyLevel(str, enum.Enum):
    """Difficulty levels for exercises."""

    BEGINNER = "beginner"
    INTERMEDIATE = "intermediate"
    ADVANCED = "advanced"


class Exercise(Base):
    """Hands-on coding exercise within a module.

    Provides scaffolded practice for programming concepts introduced
    in lessons. Can support multiple programming languages.
    """

    __tablename__ = "exercises"

    id = Column(Integer, primary_key=True, index=True)
    module_id = Column(
        Integer, ForeignKey("modules.id", ondelete="CASCADE"), nullable=False, index=True
    )
    slug = Column(String(100), index=True, nullable=False)
    title = Column(String(255), nullable=False)

    # Content
    description = Column(Text, nullable=False)
    problem_statement = Column(Text, nullable=False)  # Full problem description
    sequence_order = Column(Integer, nullable=False)

    # Programming languages supported for this exercise
    languages_supported = Column(JSON, nullable=False, default=list)  # ['python', 'haskell', ...]

    # Difficulty
    difficulty = Column(Enum(DifficultyLevel), nullable=False, default=DifficultyLevel.BEGINNER)

    # Exercise content
    starter_code = Column(JSON, nullable=True)  # {language: code_string}
    solution_code = Column(JSON, nullable=True)  # {language: code_string}
    test_cases = Column(JSON, nullable=False, default=list)  # List of test case objects

    # Hints and explanations
    hints = Column(JSON, nullable=True, default=list)  # List of hint strings
    explanation = Column(Text, nullable=True)  # Detailed explanation

    # Constraints
    time_limit_seconds = Column(Integer, default=60)  # Execution time limit
    memory_limit_mb = Column(Integer, default=256)  # Memory limit

    # Metadata
    estimated_minutes = Column(Integer, default=30)
    is_optional = Column(Boolean, default=False, nullable=False)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), onupdate=func.now())

    # Relationships
    module = relationship("Module", back_populates="exercises")
    submissions = relationship(
        "ExerciseSubmission", back_populates="exercise", cascade="all, delete-orphan"
    )
    progress = relationship(
        "ExerciseProgress", back_populates="exercise", cascade="all, delete-orphan"
    )

    def __repr__(self):
        return f"<Exercise {self.slug}>"


class ExerciseProgress(Base):
    """Tracks a user's progress on a specific exercise.

    Records attempts, best score, completion, and time spent.
    """

    __tablename__ = "exercise_progress"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(
        Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True
    )
    exercise_id = Column(
        Integer, ForeignKey("exercises.id", ondelete="CASCADE"), nullable=False, index=True
    )

    started_at = Column(DateTime(timezone=True), server_default=func.now())
    completed_at = Column(DateTime(timezone=True), nullable=True)
    last_attempted_at = Column(DateTime(timezone=True), onupdate=func.now())

    # Progress tracking
    attempts = Column(Integer, default=0, nullable=False)
    best_score = Column(Integer, nullable=True)  # Percentage (0-100)
    is_completed = Column(Boolean, default=False, nullable=False)
    is_submitted = Column(Boolean, default=False, nullable=False)

    # Relationships
    user = relationship("User", back_populates="exercise_progress")
    exercise = relationship("Exercise", back_populates="progress")

    def __repr__(self):
        return f"<ExerciseProgress user={self.user_id} exercise={self.exercise_id}>"


class ExerciseSubmission(Base):
    """Records a code submission for an exercise.

    Stores the submitted code, language, and execution results.
    """

    __tablename__ = "exercise_submissions"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(
        Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True
    )
    exercise_id = Column(
        Integer, ForeignKey("exercises.id", ondelete="CASCADE"), nullable=False, index=True
    )

    # Submission details
    submitted_code = Column(Text, nullable=False)
    language = Column(String(50), nullable=False)  # python, haskell, c, etc.

    # Execution results
    passed_tests = Column(Integer, default=0, nullable=False)
    total_tests = Column(Integer, nullable=False)
    score_percentage = Column(Integer, nullable=True)  # Calculated as (passed / total) * 100

    # Detailed output
    execution_output = Column(Text, nullable=True)
    execution_error = Column(Text, nullable=True)
    is_successful = Column(Boolean, default=False, nullable=False)

    # Metadata
    execution_time_ms = Column(Integer, nullable=True)
    memory_used_mb = Column(Integer, nullable=True)
    submitted_at = Column(DateTime(timezone=True), server_default=func.now())

    # Relationships
    user = relationship("User", back_populates="exercise_submissions")
    exercise = relationship("Exercise", back_populates="submissions")

    def __repr__(self):
        return f"<ExerciseSubmission user={self.user_id} exercise={self.exercise_id}>"
