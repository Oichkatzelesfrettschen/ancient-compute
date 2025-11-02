"""
Ancient Compute - Execution Result Caching

Implements result caching for code execution to avoid recomputing identical
code submissions. Uses content-addressable hashing for cache key generation.
"""

import hashlib
import time
from typing import Optional, Dict, Tuple
from dataclasses import dataclass

from .base_executor import ExecutionResult, ExecutionStatus


@dataclass
class CacheEntry:
    """Single cache entry with timestamp and TTL."""

    result: ExecutionResult
    created_at: float
    ttl_seconds: int = 3600  # Default 1 hour TTL

    def is_expired(self) -> bool:
        """Check if cache entry has expired."""
        return time.time() - self.created_at > self.ttl_seconds

    def age_seconds(self) -> float:
        """Get age of cache entry in seconds."""
        return time.time() - self.created_at


class ExecutionCache:
    """
    In-memory cache for code execution results.

    Cache key is sha256(language + code + input_data) to ensure deterministic
    matching of identical executions.
    """

    def __init__(self, max_entries: int = 1000, default_ttl: int = 3600):
        """
        Initialize execution cache.

        Args:
            max_entries: Maximum number of cached results
            default_ttl: Default time-to-live in seconds
        """
        self._cache: Dict[str, CacheEntry] = {}
        self.max_entries = max_entries
        self.default_ttl = default_ttl
        self.hits = 0
        self.misses = 0

    @staticmethod
    def _generate_key(language: str, code: str, input_data: str = "") -> str:
        """
        Generate cache key from language, code, and input.

        Uses SHA256 for content-addressable caching.
        """
        content = f"{language}|{code}|{input_data}".encode("utf-8")
        return hashlib.sha256(content).hexdigest()

    def get(self, language: str, code: str, input_data: str = "") -> Optional[ExecutionResult]:
        """
        Retrieve cached execution result if available.

        Args:
            language: Programming language
            code: Source code
            input_data: Optional stdin input

        Returns:
            Cached ExecutionResult or None if not found/expired
        """
        key = self._generate_key(language, code, input_data)

        if key not in self._cache:
            self.misses += 1
            return None

        entry = self._cache[key]

        if entry.is_expired():
            del self._cache[key]
            self.misses += 1
            return None

        self.hits += 1
        return entry.result

    def set(self, language: str, code: str, result: ExecutionResult, input_data: str = "") -> None:
        """
        Store execution result in cache.

        Args:
            language: Programming language
            code: Source code
            result: ExecutionResult to cache
            input_data: Optional stdin input
        """
        # Don't cache failure results (compile errors, runtime errors, timeouts)
        # to avoid masking transient issues
        if result.status != ExecutionStatus.SUCCESS:
            return

        key = self._generate_key(language, code, input_data)

        # Simple LRU-like eviction: remove oldest entry if at capacity
        if len(self._cache) >= self.max_entries:
            oldest_key = min(
                self._cache.keys(),
                key=lambda k: self._cache[k].created_at,
            )
            del self._cache[oldest_key]

        self._cache[key] = CacheEntry(
            result=result,
            created_at=time.time(),
            ttl_seconds=self.default_ttl,
        )

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
        expired_keys = [key for key, entry in self._cache.items() if entry.is_expired()]
        for key in expired_keys:
            del self._cache[key]
        return initial_size - len(self._cache)

    def get_stats(self) -> Dict[str, any]:
        """Get cache statistics."""
        total_requests = self.hits + self.misses
        hit_rate = self.hits / total_requests if total_requests > 0 else 0

        return {
            "entries": len(self._cache),
            "max_entries": self.max_entries,
            "hits": self.hits,
            "misses": self.misses,
            "hit_rate": hit_rate,
            "total_requests": total_requests,
        }


# Global cache instance
_execution_cache = ExecutionCache()


def get_execution_cache() -> ExecutionCache:
    """Get global execution cache instance."""
    return _execution_cache
