# Ancient Compute - User Model
from sqlalchemy import Boolean, Column, DateTime, Integer, String
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func

from ..database import Base


class User(Base):
    """User model for learners in the Ancient Compute platform.

    Tracks user identity, authentication, and learning progress.
    """

    __tablename__ = "users"

    id = Column(Integer, primary_key=True, index=True)
    email = Column(String(255), unique=True, index=True, nullable=False)
    username = Column(String(100), unique=True, index=True, nullable=False)
    hashed_password = Column(String(255), nullable=False)
    full_name = Column(String(255), nullable=True)

    is_active = Column(Boolean, default=True, nullable=False)
    is_superuser = Column(Boolean, default=False, nullable=False)

    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), onupdate=func.now())
    last_login_at = Column(DateTime(timezone=True), nullable=True)

    # Relationships
    module_progress = relationship(
        "ModuleProgress", back_populates="user", cascade="all, delete-orphan"
    )
    lesson_progress = relationship(
        "LessonProgress", back_populates="user", cascade="all, delete-orphan"
    )
    code_submissions = relationship(
        "CodeSubmission", back_populates="user", cascade="all, delete-orphan"
    )
    exercise_progress = relationship(
        "ExerciseProgress", back_populates="user", cascade="all, delete-orphan"
    )
    exercise_submissions = relationship(
        "ExerciseSubmission", back_populates="user", cascade="all, delete-orphan"
    )

    def __repr__(self):
        return f"<User {self.username}>"
