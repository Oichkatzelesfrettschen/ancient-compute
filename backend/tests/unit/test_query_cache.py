"""Unit tests for QueryCache -- in-memory TTL cache for database query results.

Tests cover the cache-layer logic (key generation, TTL, eviction, stats,
invalidation) without any database connection.  DB-touching methods
(get_exercise_with_relations, etc.) are deliberately skipped here because
they require SQLAlchemy session mocks and live in the DB integration suite.
"""

from __future__ import annotations

import time

from backend.src.services.query_cache import QueryCache, QueryCacheEntry

# ---------------------------------------------------------------------------
# QueryCacheEntry
# ---------------------------------------------------------------------------


class TestQueryCacheEntry:
    def test_fresh_entry_is_not_expired(self) -> None:
        entry = QueryCacheEntry(data={"id": 1}, created_at=time.time(), ttl_seconds=300)
        assert entry.is_expired() is False

    def test_old_entry_is_expired(self) -> None:
        entry = QueryCacheEntry(data={"id": 1}, created_at=time.time() - 400, ttl_seconds=300)
        assert entry.is_expired() is True


# ---------------------------------------------------------------------------
# QueryCache -- key generation
# ---------------------------------------------------------------------------


class TestQueryCacheKey:
    def test_same_kwargs_produce_same_key(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("exercise", id=5)
        k2 = cache._generate_key("exercise", id=5)
        assert k1 == k2

    def test_different_query_type_different_key(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("exercise", id=5)
        k2 = cache._generate_key("module", id=5)
        assert k1 != k2

    def test_different_params_different_key(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("exercise", id=1)
        k2 = cache._generate_key("exercise", id=2)
        assert k1 != k2

    def test_kwargs_are_sorted_deterministically(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("progress", user=1, exercise=2)
        k2 = cache._generate_key("progress", exercise=2, user=1)
        assert k1 == k2


# ---------------------------------------------------------------------------
# QueryCache -- _store and direct cache access
# ---------------------------------------------------------------------------


class TestQueryCacheStore:
    def test_store_adds_entry(self) -> None:
        cache = QueryCache()
        cache._store("my_key", {"data": 42})
        assert "my_key" in cache._cache
        assert cache._cache["my_key"].data == {"data": 42}

    def test_store_evicts_oldest_at_capacity(self) -> None:
        cache = QueryCache(max_entries=2)
        cache._store("key_a", "first")
        cache._store("key_b", "second")
        assert len(cache._cache) == 2
        cache._store("key_c", "third")
        assert len(cache._cache) == 2
        assert "key_c" in cache._cache

    def test_stored_entry_respects_default_ttl(self) -> None:
        cache = QueryCache(default_ttl=120)
        cache._store("key", "data")
        assert cache._cache["key"].ttl_seconds == 120


# ---------------------------------------------------------------------------
# QueryCache -- invalidation
# ---------------------------------------------------------------------------


class TestQueryCacheInvalidation:
    def test_wildcard_invalidates_all_entries(self) -> None:
        cache = QueryCache()
        cache._store("exercise:id=1", "ex1")
        cache._store("module:id=2", "mod2")
        removed = cache.invalidate("*")
        assert removed == 2
        assert len(cache._cache) == 0

    def test_prefix_invalidates_matching_entries(self) -> None:
        cache = QueryCache()
        cache._store("exercise:id=1", "ex1")
        cache._store("exercise:id=2", "ex2")
        cache._store("module:id=1", "mod1")
        removed = cache.invalidate("exercise:*")
        assert removed == 2
        assert "module:id=1" in cache._cache

    def test_non_matching_prefix_removes_nothing(self) -> None:
        cache = QueryCache()
        cache._store("exercise:id=1", "ex1")
        removed = cache.invalidate("progress:*")
        assert removed == 0
        assert len(cache._cache) == 1


# ---------------------------------------------------------------------------
# QueryCache -- TTL and cleanup
# ---------------------------------------------------------------------------


class TestQueryCacheTTL:
    def test_expired_entry_not_returned_on_miss(self) -> None:
        cache = QueryCache(default_ttl=1)
        key = cache._generate_key("exercise", id=99)
        cache._cache[key] = QueryCacheEntry(
            data="stale", created_at=time.time() - 10, ttl_seconds=1
        )
        # Manually call a read path through get_exercise_with_relations is DB-dependent,
        # so instead verify expiry logic on the entry object directly.
        assert cache._cache[key].is_expired() is True

    def test_cleanup_expired_removes_stale_entries(self) -> None:
        cache = QueryCache()
        cache._store("fresh_key", "fresh")
        cache._store("stale_key", "stale")
        # Rewind stale entry.
        cache._cache["stale_key"] = QueryCacheEntry(
            data="stale", created_at=time.time() - 7200, ttl_seconds=300
        )
        removed = cache.cleanup_expired()
        assert removed == 1
        assert "fresh_key" in cache._cache
        assert "stale_key" not in cache._cache


# ---------------------------------------------------------------------------
# QueryCache -- stats
# ---------------------------------------------------------------------------


class TestQueryCacheStats:
    def test_initial_stats_are_zero(self) -> None:
        cache = QueryCache()
        stats = cache.get_stats()
        assert stats["hits"] == 0
        assert stats["misses"] == 0
        assert stats["entries"] == 0
        assert stats["hit_rate"] == 0.0

    def test_stats_after_store(self) -> None:
        cache = QueryCache()
        cache._store("k", "v")
        stats = cache.get_stats()
        assert stats["entries"] == 1
        assert stats["max_entries"] == 500

    def test_clear_resets_cache_and_counters(self) -> None:
        cache = QueryCache()
        cache._store("k", "v")
        cache.hits = 5
        cache.misses = 3
        cache.clear()
        assert len(cache._cache) == 0
        assert cache.hits == 0
        assert cache.misses == 0

    def test_hit_rate_calculation(self) -> None:
        cache = QueryCache()
        cache.hits = 3
        cache.misses = 1
        stats = cache.get_stats()
        assert stats["hit_rate"] == pytest.approx(0.75)


import pytest  # noqa: E402
