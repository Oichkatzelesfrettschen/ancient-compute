# Ancient Compute - Lesson Model
import enum

from sqlalchemy import JSON, Boolean, Column, DateTime, Enum, ForeignKey, Integer, String, Text
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func

from ..database import Base


class LessonType(str, enum.Enum):
    """Types of lessons in the curriculum."""

    READING = "reading"  # Text-based content
    INTERACTIVE = "interactive"  # D3.js visualization, timeline interaction
    CODING = "coding"  # Code execution in supported languages
    QUIZ = "quiz"  # Knowledge check
    SYNTHESIS = "synthesis"  # Cross-module integration


class SupportedLanguage(str, enum.Enum):
    """Programming languages supported for code execution."""

    C = "c"
    PYTHON = "python"
    HASKELL = "haskell"
    IDRIS2 = "idris2"
    LISP = "lisp"
    ASSEMBLY = "assembly"
    JAVA = "java"
    SYSTEMF = "systemf"


class Lesson(Base):
    """Individual lesson within a module.

    Can be reading material, interactive visualization, or coding exercise.
    Each lesson teaches a specific concept within the module's scope.
    """

    __tablename__ = "lessons"

    id = Column(Integer, primary_key=True, index=True)
    module_id = Column(
        Integer, ForeignKey("modules.id", ondelete="CASCADE"), nullable=False, index=True
    )
    slug = Column(String(100), index=True, nullable=False)
    title = Column(String(255), nullable=False)

    lesson_type = Column(Enum(LessonType), nullable=False, index=True)
    sequence_order = Column(Integer, nullable=False)

    # Content
    description = Column(Text, nullable=False)
    content_markdown = Column(Text, nullable=True)  # Main lesson content
    content_json = Column(JSON, nullable=True)  # Structured data for interactive/coding lessons

    # For coding lessons
    language = Column(Enum(SupportedLanguage), nullable=True)
    starter_code = Column(Text, nullable=True)
    solution_code = Column(Text, nullable=True)
    test_cases = Column(JSON, nullable=True)

    # Metadata
    estimated_minutes = Column(Integer, default=15)
    is_optional = Column(Boolean, default=False, nullable=False)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), onupdate=func.now())

    # Relationships
    module = relationship("Module", back_populates="lessons")
    progress = relationship("LessonProgress", back_populates="lesson", cascade="all, delete-orphan")
    submissions = relationship(
        "CodeSubmission", back_populates="lesson", cascade="all, delete-orphan"
    )

    def __repr__(self):
        return f"<Lesson {self.slug}>"


class LessonProgress(Base):
    """Tracks a user's progress through a specific lesson.

    Records completion, time spent, and attempts for coding lessons.
    """

    __tablename__ = "lesson_progress"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(
        Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True
    )
    lesson_id = Column(
        Integer, ForeignKey("lessons.id", ondelete="CASCADE"), nullable=False, index=True
    )

    started_at = Column(DateTime(timezone=True), server_default=func.now())
    completed_at = Column(DateTime(timezone=True), nullable=True)
    last_accessed_at = Column(DateTime(timezone=True), onupdate=func.now())

    attempts = Column(Integer, default=0, nullable=False)
    is_completed = Column(Boolean, default=False, nullable=False)

    # Relationships
    user = relationship("User", back_populates="lesson_progress")
    lesson = relationship("Lesson", back_populates="progress")

    def __repr__(self):
        return f"<LessonProgress user={self.user_id} lesson={self.lesson_id}>"


class CodeSubmission(Base):
    """Records code submissions for coding lessons.

    Stores user's code, execution results, and feedback.
    """

    __tablename__ = "code_submissions"

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(
        Integer, ForeignKey("users.id", ondelete="CASCADE"), nullable=False, index=True
    )
    lesson_id = Column(
        Integer, ForeignKey("lessons.id", ondelete="CASCADE"), nullable=False, index=True
    )

    submitted_code = Column(Text, nullable=False)
    language = Column(Enum(SupportedLanguage), nullable=False)

    # Execution results
    passed_tests = Column(Integer, default=0, nullable=False)
    total_tests = Column(Integer, nullable=False)
    execution_output = Column(Text, nullable=True)
    execution_error = Column(Text, nullable=True)
    is_successful = Column(Boolean, default=False, nullable=False)

    submitted_at = Column(DateTime(timezone=True), server_default=func.now())

    # Relationships
    user = relationship("User", back_populates="code_submissions")
    lesson = relationship("Lesson", back_populates="submissions")

    def __repr__(self):
        return f"<CodeSubmission user={self.user_id} lesson={self.lesson_id}>"
