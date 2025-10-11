# Ancient Compute - Module Model
from sqlalchemy import Column, Integer, String, Text, DateTime, Boolean, ForeignKey, Enum
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func
import enum
from ..database import Base


class ModuleEra(str, enum.Enum):
    """Historical eras for organizing modules chronologically."""
    PREHISTORY = 'prehistory'  # 20000 BC - 3000 BC
    ANCIENT = 'ancient'  # 3000 BC - 500 AD
    MEDIEVAL = 'medieval'  # 500 AD - 1500 AD
    EARLY_MODERN = 'early_modern'  # 1500 AD - 1800 AD
    MODERN = 'modern'  # 1800 AD - 1950 AD
    CONTEMPORARY = 'contemporary'  # 1950 AD - present


class Module(Base):
    """Educational module covering a specific period or concept.

    Each module represents a major section of the 12,500-year curriculum.
    Example: "Babylonian Computation", "Greek Logic", "Lambda Calculus"
    """
    __tablename__ = 'modules'

    id = Column(Integer, primary_key=True, index=True)
    slug = Column(String(100), unique=True, index=True, nullable=False)
    title = Column(String(255), nullable=False)
    description = Column(Text, nullable=False)
    era = Column(Enum(ModuleEra), nullable=False, index=True)

    # Chronological ordering
    start_year = Column(Integer, nullable=False)  # Can be negative for BC
    end_year = Column(Integer, nullable=False)
    sequence_order = Column(Integer, nullable=False, index=True)

    # Module metadata
    estimated_hours = Column(Integer, default=4)
    difficulty_level = Column(Integer, default=1)  # 1-5 scale
    prerequisites = Column(Text, nullable=True)  # JSON array of prerequisite module slugs

    is_published = Column(Boolean, default=False, nullable=False)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), onupdate=func.now())

    # Relationships
    lessons = relationship('Lesson', back_populates='module', cascade='all, delete-orphan', order_by='Lesson.sequence_order')
    progress = relationship('ModuleProgress', back_populates='module', cascade='all, delete-orphan')

    def __repr__(self):
        return f'<Module {self.slug}>'


class ModuleProgress(Base):
    """Tracks a user's progress through a specific module.

    Records completion status, time spent, and overall progress.
    """
    __tablename__ = 'module_progress'

    id = Column(Integer, primary_key=True, index=True)
    user_id = Column(Integer, ForeignKey('users.id', ondelete='CASCADE'), nullable=False, index=True)
    module_id = Column(Integer, ForeignKey('modules.id', ondelete='CASCADE'), nullable=False, index=True)

    started_at = Column(DateTime(timezone=True), server_default=func.now())
    completed_at = Column(DateTime(timezone=True), nullable=True)
    last_accessed_at = Column(DateTime(timezone=True), onupdate=func.now())

    lessons_completed = Column(Integer, default=0, nullable=False)
    total_lessons = Column(Integer, nullable=False)
    progress_percentage = Column(Integer, default=0, nullable=False)

    # Relationships
    user = relationship('User', back_populates='module_progress')
    module = relationship('Module', back_populates='progress')

    def __repr__(self):
        return f'<ModuleProgress user={self.user_id} module={self.module_id}>'
