"""Unit tests for RateLimiter token-bucket logic and RateLimitMiddleware routing.

No real HTTP calls or Redis required -- all tests use the in-memory RateLimiter
directly, or mock Request/Response for middleware path tests.
"""

from __future__ import annotations

import time
from unittest.mock import MagicMock

import pytest

from backend.src.rate_limiting import RateLimiter, RateLimitMiddleware

# ---------------------------------------------------------------------------
# RateLimiter -- core token bucket
# ---------------------------------------------------------------------------


class TestRateLimiterAllowed:
    def test_first_request_always_allowed(self) -> None:
        rl = RateLimiter()
        allowed, retry = rl.is_allowed("1.2.3.4", "/api/v1/execute", 10, 60)
        assert allowed is True
        assert retry is None

    def test_requests_within_limit_all_allowed(self) -> None:
        rl = RateLimiter()
        for _ in range(5):
            allowed, retry = rl.is_allowed("1.2.3.4", "/test", 10, 60)
            assert allowed is True, "expected all 5 requests within limit to be allowed"
            assert retry is None

    def test_different_identifiers_tracked_independently(self) -> None:
        rl = RateLimiter()
        # Drain one identifier to 0 tokens.
        for _ in range(3):
            rl.is_allowed("ip-A", "/test", 3, 60)
        allowed_a, _ = rl.is_allowed("ip-A", "/test", 3, 60)
        assert allowed_a is False
        # ip-B should still be fresh.
        allowed_b, retry_b = rl.is_allowed("ip-B", "/test", 3, 60)
        assert allowed_b is True
        assert retry_b is None

    def test_different_routes_tracked_independently(self) -> None:
        rl = RateLimiter()
        for _ in range(3):
            rl.is_allowed("1.2.3.4", "/route-A", 3, 60)
        allowed_a, _ = rl.is_allowed("1.2.3.4", "/route-A", 3, 60)
        assert allowed_a is False
        # Route B should still have tokens.
        allowed_b, _ = rl.is_allowed("1.2.3.4", "/route-B", 3, 60)
        assert allowed_b is True


class TestRateLimiterDenied:
    def test_exceeding_limit_returns_false(self) -> None:
        rl = RateLimiter()
        # First request initialises bucket at max_requests - 1 = 2 tokens.
        # Next 2 consume those.  4th should be denied.
        for _ in range(3):
            rl.is_allowed("1.2.3.4", "/api", 3, 60)
        allowed, retry = rl.is_allowed("1.2.3.4", "/api", 3, 60)
        assert allowed is False
        assert retry is not None
        assert retry >= 0

    def test_retry_after_is_positive_integer(self) -> None:
        rl = RateLimiter()
        for _ in range(5):
            rl.is_allowed("x", "/y", 5, 60)
        allowed, retry = rl.is_allowed("x", "/y", 5, 60)
        assert allowed is False
        assert isinstance(retry, int)
        assert retry >= 0


class TestRateLimiterRefill:
    def test_tokens_refill_after_time_passes(self) -> None:
        rl = RateLimiter()
        # Drain 3-token bucket.
        for _ in range(3):
            rl.is_allowed("ip", "/r", 3, 60)
        # Verify it is now empty.
        allowed, _ = rl.is_allowed("ip", "/r", 3, 60)
        assert allowed is False

        # Fast-forward time by 20 seconds (1/3 of window -> 1 token refilled).
        key = rl._get_key("ip", "/r")
        tokens, _ = rl.buckets[key]
        # Manually rewind last_update by 20 s so refill calculation kicks in.
        rl.buckets[key] = (tokens, time.time() - 20)
        allowed2, _ = rl.is_allowed("ip", "/r", 3, 60)
        assert allowed2 is True


class TestRateLimiterCleanup:
    def test_cleanup_removes_stale_entries(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("stale-ip", "/route", 10, 60)
        key = rl._get_key("stale-ip", "/route")
        assert key in rl.buckets

        # Set last_update to 2 hours ago (outside 1h cleanup window).
        rl.buckets[key] = (10.0, time.time() - 7300)
        # Force cleanup by rewinding last_cleanup.
        rl.last_cleanup = time.time() - 3601
        rl._cleanup_old_entries()
        assert key not in rl.buckets

    def test_cleanup_preserves_recent_entries(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("fresh-ip", "/route", 10, 60)
        key = rl._get_key("fresh-ip", "/route")
        assert key in rl.buckets

        rl.last_cleanup = time.time() - 3601
        rl._cleanup_old_entries()
        # Recent entry (last_update = now) must survive.
        assert key in rl.buckets


class TestRateLimiterKeyFormat:
    def test_key_includes_identifier_and_route(self) -> None:
        rl = RateLimiter()
        key = rl._get_key("10.0.0.1", "/api/v1/execute")
        assert "10.0.0.1" in key
        assert "/api/v1/execute" in key

    def test_different_inputs_produce_different_keys(self) -> None:
        rl = RateLimiter()
        k1 = rl._get_key("1.1.1.1", "/a")
        k2 = rl._get_key("2.2.2.2", "/a")
        k3 = rl._get_key("1.1.1.1", "/b")
        assert k1 != k2
        assert k1 != k3
        assert k2 != k3


# ---------------------------------------------------------------------------
# RateLimitMiddleware -- route limit selection and identifier extraction
# ---------------------------------------------------------------------------


class TestRateLimitMiddlewareRouteLimits:
    def setup_method(self) -> None:
        self.mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        self.mw.limiter = RateLimiter()
        self.mw.route_limits = {
            "/api/v1/execute": (10, 60),
            "/api/v1/compile": (20, 60),
            "/api/v1/auth/login": (5, 300),
            "/api/v1/auth/register": (3, 3600),
            "default": (100, 60),
        }

    def test_execute_route_returns_10_per_60(self) -> None:
        max_req, window = self.mw._get_rate_limit("/api/v1/execute")
        assert (max_req, window) == (10, 60)

    def test_compile_route_returns_20_per_60(self) -> None:
        max_req, window = self.mw._get_rate_limit("/api/v1/compile")
        assert (max_req, window) == (20, 60)

    def test_auth_login_returns_5_per_300(self) -> None:
        max_req, window = self.mw._get_rate_limit("/api/v1/auth/login")
        assert (max_req, window) == (5, 300)

    def test_auth_register_returns_3_per_3600(self) -> None:
        max_req, window = self.mw._get_rate_limit("/api/v1/auth/register")
        assert (max_req, window) == (3, 3600)

    def test_unknown_path_returns_default(self) -> None:
        max_req, window = self.mw._get_rate_limit("/some/random/path")
        assert (max_req, window) == (100, 60)

    def test_subpath_of_execute_matches_execute_limit(self) -> None:
        # /api/v1/execute/something contains the pattern
        max_req, _ = self.mw._get_rate_limit("/api/v1/execute/something")
        assert max_req == 10


class TestRateLimitMiddlewareClientIdentifier:
    def setup_method(self) -> None:
        self.mw = RateLimitMiddleware.__new__(RateLimitMiddleware)

    def _make_request(self, forwarded: str | None, client_host: str | None) -> MagicMock:
        req = MagicMock()
        req.headers.get = MagicMock(return_value=forwarded)
        if client_host is not None:
            req.client = MagicMock()
            req.client.host = client_host
        else:
            req.client = None
        return req

    def test_uses_x_forwarded_for_when_present(self) -> None:
        req = self._make_request("203.0.113.5, 10.0.0.1", "10.0.0.1")
        identifier = self.mw._get_client_identifier(req)
        assert identifier == "203.0.113.5"

    def test_uses_direct_client_ip_when_no_forward_header(self) -> None:
        req = self._make_request(None, "192.168.1.1")
        identifier = self.mw._get_client_identifier(req)
        assert identifier == "192.168.1.1"

    def test_falls_back_to_unknown_when_no_client(self) -> None:
        req = self._make_request(None, None)
        identifier = self.mw._get_client_identifier(req)
        assert identifier == "unknown"

    def test_strips_whitespace_from_forwarded_header(self) -> None:
        req = self._make_request("  198.51.100.1  , 10.0.0.1", "10.0.0.1")
        identifier = self.mw._get_client_identifier(req)
        assert identifier == "198.51.100.1"


class TestRateLimiterBucketState:
    def test_bucket_created_on_first_request(self) -> None:
        rl = RateLimiter()
        key = rl._get_key("10.0.0.1", "/api")
        assert key not in rl.buckets
        rl.is_allowed("10.0.0.1", "/api", 10, 60)
        assert key in rl.buckets

    def test_bucket_has_tokens_and_timestamp(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("1.2.3.4", "/test", 5, 60)
        key = rl._get_key("1.2.3.4", "/test")
        tokens, last_update = rl.buckets[key]
        assert isinstance(tokens, (int, float))
        assert isinstance(last_update, float)

    def test_tokens_decrease_after_each_request(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("1.2.3.4", "/test", 10, 60)
        key = rl._get_key("1.2.3.4", "/test")
        tokens_1, _ = rl.buckets[key]
        rl.is_allowed("1.2.3.4", "/test", 10, 60)
        tokens_2, _ = rl.buckets[key]
        assert tokens_2 < tokens_1

    def test_buckets_dict_initially_empty(self) -> None:
        rl = RateLimiter()
        assert len(rl.buckets) == 0

    def test_last_cleanup_is_float(self) -> None:
        rl = RateLimiter()
        assert isinstance(rl.last_cleanup, float)

    def test_multiple_identifiers_create_separate_buckets(self) -> None:
        rl = RateLimiter()
        for i in range(5):
            rl.is_allowed(f"ip-{i}", "/test", 10, 60)
        assert len(rl.buckets) == 5

    def test_same_ip_different_routes_separate_buckets(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("1.2.3.4", "/route-A", 10, 60)
        rl.is_allowed("1.2.3.4", "/route-B", 10, 60)
        assert len(rl.buckets) == 2


class TestRateLimitMiddlewareExtended:
    def setup_method(self) -> None:
        self.mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        self.mw.limiter = RateLimiter()
        self.mw.route_limits = {
            "/api/v1/execute": (10, 60),
            "/api/v1/compile": (20, 60),
            "/api/v1/auth/login": (5, 300),
            "default": (100, 60),
        }

    def test_get_rate_limit_returns_two_element_tuple(self) -> None:
        limit = self.mw._get_rate_limit("/api/v1/execute")
        assert len(limit) == 2

    def test_all_configured_routes_have_positive_limits(self) -> None:
        for path in ("/api/v1/execute", "/api/v1/compile", "/api/v1/auth/login"):
            max_req, window = self.mw._get_rate_limit(path)
            assert max_req > 0
            assert window > 0

    def test_default_max_requests_is_highest(self) -> None:
        max_exec, _ = self.mw._get_rate_limit("/api/v1/execute")
        max_default, _ = self.mw._get_rate_limit("/unknown/path")
        assert max_default > max_exec


class TestRateLimiterTokenBucketSemantics:
    """Token bucket fill/drain model verification."""

    def test_initial_bucket_has_full_capacity(self) -> None:
        rl = RateLimiter()
        # After first request, tokens should be max - 1
        rl.is_allowed("a", "/x", 5, 60)
        key = rl._get_key("a", "/x")
        tokens, _ = rl.buckets[key]
        assert tokens == pytest.approx(4, abs=1)

    def test_deny_after_exactly_limit_requests(self) -> None:
        rl = RateLimiter()
        for _ in range(5):
            a, _ = rl.is_allowed("z", "/p", 5, 60)
        # 6th should be denied
        allowed, retry = rl.is_allowed("z", "/p", 5, 60)
        assert allowed is False
        assert retry is not None

    def test_limit_of_one_denies_on_second(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("x", "/q", 1, 60)
        allowed, _ = rl.is_allowed("x", "/q", 1, 60)
        assert allowed is False

    def test_two_routes_same_ip_total_independent(self) -> None:
        rl = RateLimiter()
        for _ in range(3):
            rl.is_allowed("1.1.1.1", "/a", 3, 60)
        # /a exhausted; /b still has capacity
        a_result, _ = rl.is_allowed("1.1.1.1", "/a", 3, 60)
        b_result, _ = rl.is_allowed("1.1.1.1", "/b", 3, 60)
        assert a_result is False
        assert b_result is True

    def test_key_format_is_deterministic(self) -> None:
        rl = RateLimiter()
        k1 = rl._get_key("1.2.3.4", "/api")
        k2 = rl._get_key("1.2.3.4", "/api")
        assert k1 == k2

    def test_large_limit_allows_many_requests(self) -> None:
        rl = RateLimiter()
        for i in range(100):
            a, _ = rl.is_allowed("bulk", "/fast", 100, 60)
            assert a is True, f"Request {i + 1} was denied unexpectedly"

    def test_auth_window_longer_than_execute_window(self) -> None:
        mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        mw.limiter = RateLimiter()
        mw.route_limits = {
            "/api/v1/execute": (10, 60),
            "/api/v1/auth/login": (5, 300),
            "default": (100, 60),
        }
        _, exec_window = mw._get_rate_limit("/api/v1/execute")
        _, auth_window = mw._get_rate_limit("/api/v1/auth/login")
        assert auth_window > exec_window

    def test_client_identifier_returns_non_empty_string(self) -> None:
        mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        req = MagicMock()
        req.headers.get = MagicMock(return_value=None)
        req.client = None
        identifier = mw._get_client_identifier(req)
        assert isinstance(identifier, str)
        assert len(identifier) > 0

    def test_forwarded_header_first_ip_selected(self) -> None:
        mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        req = MagicMock()
        req.headers.get = MagicMock(return_value="1.1.1.1, 2.2.2.2, 3.3.3.3")
        req.client = MagicMock()
        req.client.host = "10.0.0.1"
        identifier = mw._get_client_identifier(req)
        assert identifier == "1.1.1.1"

    def test_unknown_path_uses_default_limit(self) -> None:
        mw = RateLimitMiddleware.__new__(RateLimitMiddleware)
        mw.limiter = RateLimiter()
        mw.route_limits = {"default": (100, 60)}
        max_req, window = mw._get_rate_limit("/completely/unknown/path")
        assert max_req == 100
        assert window == 60


class TestRateLimiterRetryHeader:
    """Retry-After header value on rate limit hit."""

    def test_retry_after_positive_on_rejection(self) -> None:
        rl = RateLimiter()
        for _ in range(3):
            rl.is_allowed("ip-test", "/api/v1/execute", 3, 60)
        allowed, retry = rl.is_allowed("ip-test", "/api/v1/execute", 3, 60)
        assert allowed is False
        assert retry is not None
        assert retry > 0

    def test_retry_after_none_when_allowed(self) -> None:
        rl = RateLimiter()
        _, retry = rl.is_allowed("new-ip", "/test", 10, 60)
        assert retry is None

    def test_exact_limit_allowed_then_blocked(self) -> None:
        rl = RateLimiter()
        # max_requests=2: first 2 allowed, third blocked
        rl.is_allowed("user", "/api", 2, 60)
        rl.is_allowed("user", "/api", 2, 60)
        allowed, _ = rl.is_allowed("user", "/api", 2, 60)
        assert allowed is False

    def test_different_windows_track_separately(self) -> None:
        rl = RateLimiter()
        for _ in range(5):
            rl.is_allowed("ip", "/fast", 5, 1)  # 1 second window
        # Same ip, different route/window
        allowed, _ = rl.is_allowed("ip", "/slow", 5, 3600)
        assert allowed is True


class TestRateLimiterBucketStateExtended:
    """Internal bucket state tests."""

    def test_buckets_dict_populated_after_request(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("test-ip", "/route", 10, 60)
        assert len(rl.buckets) >= 1

    def test_buckets_empty_on_fresh_limiter(self) -> None:
        rl = RateLimiter()
        assert len(rl.buckets) == 0

    def test_two_ips_two_bucket_entries(self) -> None:
        rl = RateLimiter()
        rl.is_allowed("ip-alpha", "/route", 10, 60)
        rl.is_allowed("ip-beta", "/route", 10, 60)
        assert len(rl.buckets) == 2

    def test_repeated_requests_do_not_duplicate_buckets(self) -> None:
        rl = RateLimiter()
        for _ in range(10):
            rl.is_allowed("single-ip", "/route", 100, 60)
        assert len(rl.buckets) == 1

    def test_is_allowed_returns_tuple(self) -> None:
        rl = RateLimiter()
        result = rl.is_allowed("ip", "/route", 10, 60)
        assert isinstance(result, tuple)
        assert len(result) == 2
