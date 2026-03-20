"""Unit tests for RateLimiter token-bucket logic and RateLimitMiddleware routing.

No real HTTP calls or Redis required -- all tests use the in-memory RateLimiter
directly, or mock Request/Response for middleware path tests.
"""

from __future__ import annotations

import time
from unittest.mock import MagicMock

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
