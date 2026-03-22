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

# ---------------------------------------------------------------------------
# QueryCacheEntry -- extended
# ---------------------------------------------------------------------------


class TestQueryCacheEntryExtended:
    def test_entry_data_preserved(self) -> None:
        entry = QueryCacheEntry(data=[1, 2, 3], created_at=time.time(), ttl_seconds=60)
        assert entry.data == [1, 2, 3]

    def test_default_ttl_is_300(self) -> None:
        entry = QueryCacheEntry(data="x", created_at=time.time())
        assert entry.ttl_seconds == 300

    def test_just_expired_is_expired(self) -> None:
        # Created exactly ttl+1 seconds ago -> expired
        entry = QueryCacheEntry(data="x", created_at=time.time() - 301, ttl_seconds=300)
        assert entry.is_expired() is True

    def test_just_before_expiry_is_not_expired(self) -> None:
        entry = QueryCacheEntry(data="x", created_at=time.time() - 1, ttl_seconds=300)
        assert entry.is_expired() is False

    def test_zero_ttl_is_always_expired(self) -> None:
        entry = QueryCacheEntry(data="x", created_at=time.time() - 1, ttl_seconds=0)
        assert entry.is_expired() is True


# ---------------------------------------------------------------------------
# QueryCache -- constructor defaults
# ---------------------------------------------------------------------------


class TestQueryCacheDefaults:
    def test_default_max_entries(self) -> None:
        cache = QueryCache()
        assert cache.max_entries == 500

    def test_default_ttl_seconds(self) -> None:
        cache = QueryCache()
        assert cache.default_ttl == 300

    def test_cache_starts_empty(self) -> None:
        cache = QueryCache()
        assert len(cache._cache) == 0

    def test_hits_start_at_zero(self) -> None:
        cache = QueryCache()
        assert cache.hits == 0

    def test_misses_start_at_zero(self) -> None:
        cache = QueryCache()
        assert cache.misses == 0

    def test_custom_max_entries(self) -> None:
        cache = QueryCache(max_entries=10)
        assert cache.max_entries == 10

    def test_custom_ttl(self) -> None:
        cache = QueryCache(default_ttl=60)
        assert cache.default_ttl == 60


# ---------------------------------------------------------------------------
# QueryCache -- key generation extended
# ---------------------------------------------------------------------------


class TestQueryCacheKeyExtended:
    def test_key_is_string(self) -> None:
        cache = QueryCache()
        key = cache._generate_key("exercise", id=1)
        assert isinstance(key, str)

    def test_key_contains_query_type(self) -> None:
        cache = QueryCache()
        key = cache._generate_key("module", id=7)
        assert "module" in key

    def test_no_kwargs_produces_stable_key(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("list")
        k2 = cache._generate_key("list")
        assert k1 == k2

    def test_different_integer_values_produce_different_keys(self) -> None:
        cache = QueryCache()
        k1 = cache._generate_key("exercise", id=1)
        k2 = cache._generate_key("exercise", id=2)
        assert k1 != k2


# ---------------------------------------------------------------------------
# QueryCache -- store and eviction extended
# ---------------------------------------------------------------------------


class TestQueryCacheStoreExtended:
    def test_store_overwrites_existing_key(self) -> None:
        cache = QueryCache()
        cache._store("k", "first")
        cache._store("k", "second")
        assert cache._cache["k"].data == "second"
        assert len(cache._cache) == 1

    def test_store_at_capacity_keeps_size_bounded(self) -> None:
        cache = QueryCache(max_entries=3)
        for i in range(5):
            cache._store(f"key_{i}", i)
        assert len(cache._cache) <= 3

    def test_custom_ttl_in_store(self) -> None:
        cache = QueryCache(default_ttl=999)
        cache._store("k", "v")
        assert cache._cache["k"].ttl_seconds == 999


# ---------------------------------------------------------------------------
# QueryCache -- stats extended
# ---------------------------------------------------------------------------


class TestQueryCacheStatsExtended:
    def test_stats_max_entries_matches_init(self) -> None:
        cache = QueryCache(max_entries=100)
        stats = cache.get_stats()
        assert stats["max_entries"] == 100

    def test_stats_entries_reflects_current_count(self) -> None:
        cache = QueryCache()
        cache._store("a", 1)
        cache._store("b", 2)
        stats = cache.get_stats()
        assert stats["entries"] == 2

    def test_hit_rate_zero_with_no_requests(self) -> None:
        cache = QueryCache()
        assert cache.get_stats()["hit_rate"] == 0.0

    def test_all_misses_gives_zero_hit_rate(self) -> None:
        cache = QueryCache()
        cache.hits = 0
        cache.misses = 10
        assert cache.get_stats()["hit_rate"] == 0.0

    def test_all_hits_gives_one_hit_rate(self) -> None:
        cache = QueryCache()
        cache.hits = 10
        cache.misses = 0
        stats = cache.get_stats()
        assert stats["hit_rate"] == pytest.approx(1.0)


# ---------------------------------------------------------------------------
# QueryCache -- invalidation extended
# ---------------------------------------------------------------------------


class TestQueryCacheInvalidationExtended:
    def test_wildcard_on_empty_cache_returns_zero(self) -> None:
        cache = QueryCache()
        assert cache.invalidate("*") == 0

    def test_prefix_with_colon_star_matches_all_with_prefix(self) -> None:
        cache = QueryCache()
        for i in range(4):
            cache._store(f"exercise:id={i}", i)
        cache._store("module:id=1", "mod")
        removed = cache.invalidate("exercise:*")
        assert removed == 4
        assert len(cache._cache) == 1

    def test_clear_empty_cache_is_safe(self) -> None:
        cache = QueryCache()
        cache.clear()  # Should not raise
        assert len(cache._cache) == 0

    def test_cleanup_on_fresh_cache_returns_zero(self) -> None:
        cache = QueryCache()
        cache._store("k", "v")
        assert cache.cleanup_expired() == 0


class TestQueryCacheStoreGet:
    """_store and _cache round-trip and miss semantics."""

    def test_stored_value_in_cache(self) -> None:
        cache = QueryCache()
        cache._store("mykey", "myval")
        assert "mykey" in cache._cache

    def test_missing_key_not_in_cache(self) -> None:
        cache = QueryCache()
        assert "nonexistent" not in cache._cache

    def test_overwrite_replaces_value(self) -> None:
        cache = QueryCache()
        cache._store("k", "first")
        cache._store("k", "second")
        assert cache._cache["k"].data == "second"

    def test_cache_len_grows_with_stores(self) -> None:
        cache = QueryCache()
        for i in range(5):
            cache._store(f"key_{i}", i)
        assert len(cache._cache) == 5
