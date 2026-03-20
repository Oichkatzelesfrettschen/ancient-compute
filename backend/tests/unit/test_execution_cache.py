"""Unit tests for ExecutionCache -- in-memory TTL cache for code execution results.

All tests are purely in-memory; no Docker, DB, or network required.
"""

from __future__ import annotations

import time

from backend.src.services.base_executor import ExecutionResult, ExecutionStatus
from backend.src.services.execution_cache import CacheEntry, ExecutionCache

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

_OK = ExecutionStatus.SUCCESS
_ERR = ExecutionStatus.COMPILE_ERROR


def _result(status: ExecutionStatus = _OK, stdout: str = "42") -> ExecutionResult:
    return ExecutionResult(status=status, stdout=stdout, stderr="")


# ---------------------------------------------------------------------------
# CacheEntry
# ---------------------------------------------------------------------------


class TestCacheEntry:
    def test_fresh_entry_is_not_expired(self) -> None:
        entry = CacheEntry(result=_result(), created_at=time.time(), ttl_seconds=60)
        assert entry.is_expired() is False

    def test_old_entry_is_expired(self) -> None:
        entry = CacheEntry(result=_result(), created_at=time.time() - 3700, ttl_seconds=3600)
        assert entry.is_expired() is True

    def test_age_seconds_is_nonnegative(self) -> None:
        entry = CacheEntry(result=_result(), created_at=time.time(), ttl_seconds=60)
        assert entry.age_seconds() >= 0


# ---------------------------------------------------------------------------
# ExecutionCache -- key generation
# ---------------------------------------------------------------------------


class TestExecutionCacheKey:
    def test_same_inputs_produce_same_key(self) -> None:
        k1 = ExecutionCache._generate_key("python", "print(1)")
        k2 = ExecutionCache._generate_key("python", "print(1)")
        assert k1 == k2

    def test_different_language_different_key(self) -> None:
        k1 = ExecutionCache._generate_key("python", "x")
        k2 = ExecutionCache._generate_key("c", "x")
        assert k1 != k2

    def test_different_code_different_key(self) -> None:
        k1 = ExecutionCache._generate_key("python", "print(1)")
        k2 = ExecutionCache._generate_key("python", "print(2)")
        assert k1 != k2

    def test_different_input_data_different_key(self) -> None:
        k1 = ExecutionCache._generate_key("python", "x", "input_a")
        k2 = ExecutionCache._generate_key("python", "x", "input_b")
        assert k1 != k2

    def test_key_is_hex_string(self) -> None:
        key = ExecutionCache._generate_key("c", "int main(){}")
        assert all(c in "0123456789abcdef" for c in key)
        assert len(key) == 64  # SHA256 hex digest


# ---------------------------------------------------------------------------
# ExecutionCache -- basic get/set
# ---------------------------------------------------------------------------


class TestExecutionCacheGetSet:
    def test_miss_on_empty_cache(self) -> None:
        cache = ExecutionCache()
        assert cache.get("python", "print(1)") is None

    def test_hit_after_successful_set(self) -> None:
        cache = ExecutionCache()
        r = _result(stdout="hello")
        cache.set("python", "print('hello')", r)
        result = cache.get("python", "print('hello')")
        assert result is not None
        assert result.stdout == "hello"

    def test_failure_results_not_cached(self) -> None:
        cache = ExecutionCache()
        cache.set("python", "bad code", _result(status=_ERR))
        assert cache.get("python", "bad code") is None

    def test_set_with_input_data(self) -> None:
        cache = ExecutionCache()
        r = _result(stdout="5")
        cache.set("c", "main(){}", r, input_data="5")
        assert cache.get("c", "main(){}", input_data="5") is not None
        assert cache.get("c", "main(){}", input_data="6") is None


# ---------------------------------------------------------------------------
# TTL / expiry
# ---------------------------------------------------------------------------


class TestExecutionCacheTTL:
    def test_expired_entry_returns_none(self) -> None:
        cache = ExecutionCache(default_ttl=1)
        cache.set("python", "x", _result())
        # Manually rewind the created_at timestamp.
        key = ExecutionCache._generate_key("python", "x")
        cache._cache[key] = CacheEntry(result=_result(), created_at=time.time() - 5, ttl_seconds=1)
        assert cache.get("python", "x") is None

    def test_expired_entry_increments_miss(self) -> None:
        cache = ExecutionCache(default_ttl=1)
        cache.set("python", "x", _result())
        key = ExecutionCache._generate_key("python", "x")
        cache._cache[key] = CacheEntry(result=_result(), created_at=time.time() - 5, ttl_seconds=1)
        cache.get("python", "x")
        assert cache.misses >= 1


# ---------------------------------------------------------------------------
# Hit/miss counters
# ---------------------------------------------------------------------------


class TestExecutionCacheStats:
    def test_miss_increments_miss_counter(self) -> None:
        cache = ExecutionCache()
        cache.get("python", "x")
        assert cache.misses == 1
        assert cache.hits == 0

    def test_hit_increments_hit_counter(self) -> None:
        cache = ExecutionCache()
        cache.set("python", "x", _result())
        cache.get("python", "x")
        assert cache.hits == 1
        assert cache.misses == 0

    def test_get_stats_hit_rate_zero_on_no_requests(self) -> None:
        cache = ExecutionCache()
        stats = cache.get_stats()
        assert stats["hit_rate"] == 0.0
        assert stats["total_requests"] == 0

    def test_get_stats_hit_rate_after_hits_and_misses(self) -> None:
        cache = ExecutionCache()
        cache.set("python", "x", _result())
        cache.get("python", "x")  # hit
        cache.get("python", "y")  # miss
        stats = cache.get_stats()
        assert stats["hit_rate"] == pytest.approx(0.5)
        assert stats["total_requests"] == 2

    def test_clear_resets_counters(self) -> None:
        cache = ExecutionCache()
        cache.set("python", "x", _result())
        cache.get("python", "x")
        cache.get("python", "y")
        cache.clear()
        assert cache.hits == 0
        assert cache.misses == 0
        assert len(cache._cache) == 0


# ---------------------------------------------------------------------------
# Eviction
# ---------------------------------------------------------------------------


class TestExecutionCacheEviction:
    def test_oldest_entry_evicted_at_capacity(self) -> None:
        cache = ExecutionCache(max_entries=3)
        for i in range(3):
            cache.set("python", f"code_{i}", _result(stdout=str(i)))
        # All 3 are present.
        assert len(cache._cache) == 3
        # The oldest entry is code_0 (smallest created_at).  Setting a 4th should evict it.
        cache.set("python", "code_3", _result(stdout="3"))
        assert len(cache._cache) == 3
        assert cache.get("python", "code_3") is not None

    def test_cleanup_expired_removes_stale_entries(self) -> None:
        cache = ExecutionCache()
        cache.set("python", "fresh", _result())
        cache.set("python", "stale", _result())
        key_stale = ExecutionCache._generate_key("python", "stale")
        cache._cache[key_stale] = CacheEntry(
            result=_result(), created_at=time.time() - 7200, ttl_seconds=3600
        )
        removed = cache.cleanup_expired()
        assert removed == 1
        assert cache.get("python", "fresh") is not None


# pytest import needed for approx
import pytest  # noqa: E402 -- must follow class definitions
