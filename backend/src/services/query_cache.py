"""
Ancient Compute - Database Query Optimization

Implements caching and eager loading for frequently accessed database queries
to reduce query count and improve response times.
"""

import time
from typing import Optional, List, Dict, Any
from functools import wraps
from dataclasses import dataclass

from typing import TYPE_CHECKING
try:
    # Only needed at runtime when DB paths are used
    from sqlalchemy.orm import Session  # type: ignore
except Exception:  # pragma: no cover - environment without SQLAlchemy
    Session = Any  # type: ignore

# Avoid importing SQLAlchemy models at module import time to keep this module
# usable in environments where SQLAlchemy isn't available (e.g., limited CI).
if TYPE_CHECKING:  # for type checkers only
    from src.models import (
        Exercise,
        Module,
        Era,
        ExerciseProgress,
        ExerciseSubmission,
    )


@dataclass
class QueryCacheEntry:
    """Single query cache entry with timestamp."""

    data: Any
    created_at: float
    ttl_seconds: int = 300  # Default 5 minute TTL for queries

    def is_expired(self) -> bool:
        """Check if cache entry has expired."""
        return time.time() - self.created_at > self.ttl_seconds


class QueryCache:
    """
    Database query result cache for frequently accessed entities.

    Caches Exercise, Module, and Era queries with TTL-based expiration.
    """

    def __init__(self, max_entries: int = 500, default_ttl: int = 300):
        """
        Initialize query cache.

        Args:
            max_entries: Maximum number of cached query results
            default_ttl: Default time-to-live in seconds
        """
        self._cache: Dict[str, QueryCacheEntry] = {}
        self.max_entries = max_entries
        self.default_ttl = default_ttl
        self.hits = 0
        self.misses = 0

    def _generate_key(self, query_type: str, **kwargs) -> str:
        """Generate cache key from query type and parameters."""
        params = "|".join(f"{k}={v}" for k, v in sorted(kwargs.items()))
        return f"{query_type}:{params}"

    def get_exercise_with_relations(
        self, db: Session, exercise_id: int
    ) -> Optional["Exercise"]:
        """
        Get exercise with eagerly loaded relations.

        Caches exercise data including module and era relations.
        """
        key = self._generate_key("exercise", id=exercise_id)

        if key in self._cache:
            entry = self._cache[key]
            if not entry.is_expired():
                self.hits += 1
                return entry.data
            else:
                del self._cache[key]

        self.misses += 1

        # Query with eager loading to avoid N+1 problem
        from sqlalchemy.orm import joinedload  # local import to avoid hard dependency
        from src.models import Exercise, Module

        exercise = (
            db.query(Exercise)
            .options(
                joinedload(Exercise.module).joinedload(Module.era),
            )
            .filter(Exercise.id == exercise_id)
            .first()
        )

        if exercise:
            self._store(key, exercise)

        return exercise

    def get_module_with_exercises(
        self, db: Session, module_id: int
    ) -> Optional["Module"]:
        """
        Get module with all exercises eagerly loaded.

        Reduces database round-trips for module content retrieval.
        """
        key = self._generate_key("module", id=module_id)

        if key in self._cache:
            entry = self._cache[key]
            if not entry.is_expired():
                self.hits += 1
                return entry.data
            else:
                del self._cache[key]

        self.misses += 1

        # Eager load exercises to prevent N+1
        from sqlalchemy.orm import joinedload
        from src.models import Module

        module = (
            db.query(Module)
            .options(
                joinedload(Module.era),
                joinedload(Module.exercises),
            )
            .filter(Module.id == module_id)
            .first()
        )

        if module:
            self._store(key, module)

        return module

    def get_exercise_progress_with_submissions(
        self, db: Session, user_id: int, exercise_id: int
    ) -> Optional["ExerciseProgress"]:
        """
        Get exercise progress with latest submissions eagerly loaded.

        Avoids separate queries for submission history.
        """
        key = self._generate_key("progress", user=user_id, exercise=exercise_id)

        if key in self._cache:
            entry = self._cache[key]
            if not entry.is_expired():
                self.hits += 1
                return entry.data
            else:
                del self._cache[key]

        self.misses += 1

        # Eager load submissions to avoid N+1
        from sqlalchemy.orm import joinedload
        from src.models import ExerciseProgress

        progress = (
            db.query(ExerciseProgress)
            .options(
                joinedload(ExerciseProgress.submissions),
            )
            .filter(
                ExerciseProgress.user_id == user_id,
                ExerciseProgress.exercise_id == exercise_id,
            )
            .first()
        )

        if progress:
            self._store(key, progress)

        return progress

    def list_exercises_by_module(
        self, db: Session, module_id: int
    ) -> List["Exercise"]:
        """
        List all exercises for a module with eager loading.

        Caches full exercise list with minimal database queries.
        """
        key = self._generate_key("module_exercises", module=module_id)

        if key in self._cache:
            entry = self._cache[key]
            if not entry.is_expired():
                self.hits += 1
                return entry.data
            else:
                del self._cache[key]

        self.misses += 1

        from src.models import Exercise

        exercises = (
            db.query(Exercise)
            .filter(Exercise.module_id == module_id)
            .order_by(Exercise.sequence_order)
            .all()
        )

        if exercises:
            self._store(key, exercises)

        return exercises

    def _store(self, key: str, data: Any) -> None:
        """Store data in cache."""
        if len(self._cache) >= self.max_entries:
            # Simple LRU: remove oldest entry
            oldest_key = min(
                self._cache.keys(),
                key=lambda k: self._cache[k].created_at,
            )
            del self._cache[oldest_key]

        self._cache[key] = QueryCacheEntry(
            data=data,
            created_at=time.time(),
            ttl_seconds=self.default_ttl,
        )

    def invalidate(self, pattern: str = "*") -> int:
        """
        Invalidate cache entries matching pattern.

        Args:
            pattern: Simple pattern matching (e.g., "exercise:*" or "*")

        Returns:
            Number of entries invalidated
        """
        if pattern == "*":
            count = len(self._cache)
            self._cache.clear()
            return count

        # Simple prefix matching for patterns like "exercise:*"
        prefix = pattern.rstrip("*")
        keys_to_delete = [k for k in self._cache.keys() if k.startswith(prefix)]
        for key in keys_to_delete:
            del self._cache[key]

        return len(keys_to_delete)

    def clear(self) -> None:
        """Clear all cached results."""
        self._cache.clear()
        self.hits = 0
        self.misses = 0

    def cleanup_expired(self) -> int:
        """
        Remove expired entries from cache.

        Returns:
            Number of entries removed
        """
        initial_size = len(self._cache)
        expired_keys = [
            key for key, entry in self._cache.items() if entry.is_expired()
        ]
        for key in expired_keys:
            del self._cache[key]
        return initial_size - len(self._cache)

    def get_stats(self) -> Dict[str, any]:
        """Get cache statistics."""
        total_requests = self.hits + self.misses
        hit_rate = (
            self.hits / total_requests if total_requests > 0 else 0
        )

        return {
            "entries": len(self._cache),
            "max_entries": self.max_entries,
            "hits": self.hits,
            "misses": self.misses,
            "hit_rate": hit_rate,
            "total_requests": total_requests,
        }


# Global query cache instance
_query_cache = QueryCache()


def get_query_cache() -> QueryCache:
    """Get global query cache instance."""
    return _query_cache
