# Ancient Compute - Era Model
"""
Historical era model for organizing the 12,500-year timeline.

Each era represents a major period in computational history:
0. Prehistory (20,000 BC - 3,000 BC): Ishango bone, tally marks
1. Ancient (3,000 BC - 500 AD): Babylonian algorithms, Greek logic
2. Medieval (500 - 1,500 AD): Islamic mathematics, scholasticism
3. Early Modern (1,500 - 1,800 AD): Leibniz, binary system, Lovelace
4. Foundations Crisis (1,850 - 1,940 AD): Frege, Russell, Gödel, Church, Turing
5. Electronic Age (1,940 - 1,980 AD): ENIAC, von Neumann, LISP, ALGOL
6. Type Theory (1,970 - 2,000 AD): System F, Hindley-Milner, dependent types
7. Paradigm Synthesis (1,980 - 2,025 AD): Modern type systems, functional programming
"""

from sqlalchemy import Boolean, Column, DateTime, Integer, String, Text
from sqlalchemy.orm import relationship
from sqlalchemy.sql import func

from ..database import Base


class Era(Base):
    """Historical era containing modules about a period in computational history.

    Represents a major timespan from the 12,500-year timeline.
    Examples: Ancient Period, Medieval Era, Electronic Age, etc.
    """

    __tablename__ = "eras"

    id = Column(Integer, primary_key=True, index=True)
    label = Column(String(50), unique=True, index=True, nullable=False)
    full_name = Column(String(255), nullable=False)
    description = Column(Text, nullable=False)

    # Historical context
    historical_context = Column(Text, nullable=False)  # Detailed historical background
    start_year = Column(Integer, nullable=False)  # Can be negative for BC
    end_year = Column(Integer, nullable=False)

    # Visual presentation
    color = Column(String(7), nullable=False)  # Hex color for timeline visualization
    order = Column(Integer, nullable=False, unique=True, index=True)  # Chronological order

    # Icons/emoji support for UI
    icon = Column(String(10), nullable=True)  # Optional emoji or symbol

    # Metadata
    is_published = Column(Boolean, default=False, nullable=False)
    created_at = Column(DateTime(timezone=True), server_default=func.now())
    updated_at = Column(DateTime(timezone=True), onupdate=func.now())

    # Relationships
    modules = relationship(
        "Module",
        back_populates="era",
        cascade="all, delete-orphan",
        order_by="Module.sequence_order",
    )

    def __repr__(self):
        return f"<Era {self.label} ({self.start_year} - {self.end_year})>"
