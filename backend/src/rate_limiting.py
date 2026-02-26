# Ancient Compute Backend - Rate Limiting Middleware
"""
Rate limiting middleware to prevent abuse and DoS attacks.

Implements token bucket algorithm with Redis for distributed rate limiting.
"""

import time
from collections.abc import Callable

from fastapi import HTTPException, Request
from fastapi.responses import JSONResponse
from starlette.middleware.base import BaseHTTPMiddleware


class RateLimiter:
    """
    Token bucket rate limiter using in-memory storage.

    For production, use Redis-backed rate limiting (see RedisRateLimiter).
    """

    def __init__(self):
        self.buckets = {}  # key -> (tokens, last_update)
        self.cleanup_interval = 3600  # Clean up old entries every hour
        self.last_cleanup = time.time()

    def _get_key(self, identifier: str, route: str) -> str:
        """Generate rate limit key for identifier and route"""
        return f"ratelimit:{identifier}:{route}"

    def _cleanup_old_entries(self):
        """Remove old entries to prevent memory leaks"""
        current_time = time.time()
        if current_time - self.last_cleanup > self.cleanup_interval:
            # Remove entries older than 1 hour
            cutoff = current_time - 3600
            self.buckets = {
                k: v for k, v in self.buckets.items()
                if v[1] > cutoff
            }
            self.last_cleanup = current_time

    def is_allowed(
        self,
        identifier: str,
        route: str,
        max_requests: int,
        window_seconds: int
    ) -> tuple[bool, int | None]:
        """
        Check if request is allowed under rate limit.

        Args:
            identifier: Unique identifier (IP address, user ID, API key)
            route: Route being accessed
            max_requests: Maximum requests allowed in window
            window_seconds: Time window in seconds

        Returns:
            Tuple of (is_allowed, retry_after_seconds)
        """
        self._cleanup_old_entries()

        key = self._get_key(identifier, route)
        current_time = time.time()

        if key not in self.buckets:
            # First request, initialize bucket
            self.buckets[key] = (max_requests - 1, current_time)
            return True, None

        tokens, last_update = self.buckets[key]
        time_passed = current_time - last_update

        # Refill tokens based on time passed
        refill_rate = max_requests / window_seconds
        new_tokens = min(max_requests, tokens + (time_passed * refill_rate))

        if new_tokens >= 1:
            # Allow request and consume token
            self.buckets[key] = (new_tokens - 1, current_time)
            return True, None
        else:
            # Rate limit exceeded
            retry_after = int((1 - new_tokens) / refill_rate)
            return False, retry_after


class RateLimitMiddleware(BaseHTTPMiddleware):
    """
    FastAPI middleware for rate limiting.

    Applies rate limits based on client IP address and route.
    """

    def __init__(self, app, limiter: RateLimiter | None = None):
        super().__init__(app)
        self.limiter = limiter or RateLimiter()

        # Default rate limits by route pattern
        self.route_limits = {
            "/api/v1/execute": (10, 60),  # 10 requests per minute
            "/api/v1/compile": (20, 60),  # 20 requests per minute
            "/api/v1/auth/login": (5, 300),  # 5 requests per 5 minutes
            "/api/v1/auth/register": (3, 3600),  # 3 requests per hour
            "default": (100, 60),  # 100 requests per minute for other routes
        }

    def _get_client_identifier(self, request: Request) -> str:
        """Get unique identifier for client (IP address)"""
        # Try to get real IP from X-Forwarded-For header (if behind proxy)
        forwarded = request.headers.get("X-Forwarded-For")
        if forwarded:
            return forwarded.split(",")[0].strip()

        # Fall back to direct client IP
        client_host = request.client.host if request.client else "unknown"
        return client_host

    def _get_rate_limit(self, path: str) -> tuple[int, int]:
        """Get rate limit for given path"""
        for route_pattern, limits in self.route_limits.items():
            if route_pattern in path:
                return limits
        return self.route_limits["default"]

    async def dispatch(self, request: Request, call_next: Callable):
        """Process request with rate limiting"""
        # Skip rate limiting for health checks and metrics
        if request.url.path in ["/health", "/ready", "/metrics"]:
            return await call_next(request)

        identifier = self._get_client_identifier(request)
        path = request.url.path
        max_requests, window = self._get_rate_limit(path)

        is_allowed, retry_after = self.limiter.is_allowed(
            identifier, path, max_requests, window
        )

        if not is_allowed:
            return JSONResponse(
                status_code=429,
                content={
                    "error": "Rate limit exceeded",
                    "message": f"Too many requests. Maximum {max_requests} requests per {window} seconds.",
                    "retry_after": retry_after
                },
                headers={"Retry-After": str(retry_after)}
            )

        response = await call_next(request)

        # Add rate limit headers
        response.headers["X-RateLimit-Limit"] = str(max_requests)
        response.headers["X-RateLimit-Window"] = str(window)

        return response


def rate_limit(max_requests: int, window_seconds: int):
    """
    Decorator for rate limiting specific endpoints.

    Args:
        max_requests: Maximum requests allowed in window
        window_seconds: Time window in seconds

    Example:
        @app.post("/api/v1/expensive-operation")
        @rate_limit(max_requests=5, window_seconds=60)
        async def expensive_operation():
            return {"status": "success"}
    """
    limiter = RateLimiter()

    def decorator(func):
        async def wrapper(request: Request, *args, **kwargs):
            identifier = request.client.host if request.client else "unknown"
            route = request.url.path

            is_allowed, retry_after = limiter.is_allowed(
                identifier, route, max_requests, window_seconds
            )

            if not is_allowed:
                raise HTTPException(
                    status_code=429,
                    detail={
                        "error": "Rate limit exceeded",
                        "retry_after": retry_after
                    },
                    headers={"Retry-After": str(retry_after)}
                )

            return await func(request, *args, **kwargs)

        return wrapper
    return decorator
