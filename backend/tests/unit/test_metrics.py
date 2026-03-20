"""Unit tests for metrics.py -- utility functions and ExecutionContext.

Prometheus counter/histogram global state makes counter-increment assertions
fragile across test runs, so tests focus on:
  - Utility functions do not raise (smoke tests)
  - ExecutionContext status tracking (mark_success/error/timeout)
  - ExecutionContext async protocol (aenter/aexit)
  - MetricsMiddleware path skip for /metrics endpoint
  - metrics_response() returns correct MIME type
"""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock

import pytest

from backend.src.metrics import (
    ExecutionContext,
    metrics_response,
    record_cache_access,
    record_code_compilation,
    record_code_execution,
)

# ---------------------------------------------------------------------------
# Utility function smoke tests
# ---------------------------------------------------------------------------


class TestRecordCodeExecution:
    def test_success_status_does_not_raise(self) -> None:
        record_code_execution("python", 0.5, "success")

    def test_error_status_does_not_raise(self) -> None:
        record_code_execution("c", 1.2, "error")

    def test_timeout_status_does_not_raise(self) -> None:
        record_code_execution("haskell", 30.0, "timeout")

    def test_various_languages_do_not_raise(self) -> None:
        for lang in ["python", "c", "haskell", "lisp", "idris", "assembly", "systemf"]:
            record_code_execution(lang, 0.1, "success")


class TestRecordCodeCompilation:
    def test_does_not_raise(self) -> None:
        record_code_compilation("c", 0.05)

    def test_zero_duration_does_not_raise(self) -> None:
        record_code_compilation("haskell", 0.0)


class TestRecordCacheAccess:
    def test_hit_does_not_raise(self) -> None:
        record_cache_access("execution", hit=True)

    def test_miss_does_not_raise(self) -> None:
        record_cache_access("execution", hit=False)

    def test_ir_cache_type_does_not_raise(self) -> None:
        record_cache_access("ir", hit=True)


# ---------------------------------------------------------------------------
# ExecutionContext -- status tracking
# ---------------------------------------------------------------------------


class TestExecutionContextStatus:
    def test_default_status_is_error(self) -> None:
        ctx = ExecutionContext("python")
        assert ctx.status == "error"

    def test_mark_success_sets_status(self) -> None:
        ctx = ExecutionContext("python")
        ctx.mark_success()
        assert ctx.status == "success"

    def test_mark_error_sets_status(self) -> None:
        ctx = ExecutionContext("python")
        ctx.mark_success()
        ctx.mark_error()
        assert ctx.status == "error"

    def test_mark_timeout_sets_status(self) -> None:
        ctx = ExecutionContext("python")
        ctx.mark_timeout()
        assert ctx.status == "timeout"


class TestExecutionContextAsync:
    @pytest.mark.asyncio
    async def test_aenter_returns_context(self) -> None:
        ctx = ExecutionContext("python")
        result = await ctx.__aenter__()
        assert result is ctx
        await ctx.__aexit__(None, None, None)

    @pytest.mark.asyncio
    async def test_aexit_without_exception_preserves_status(self) -> None:
        ctx = ExecutionContext("python")
        await ctx.__aenter__()
        ctx.mark_success()
        await ctx.__aexit__(None, None, None)
        assert ctx.status == "success"

    @pytest.mark.asyncio
    async def test_aexit_with_exception_forces_error_status(self) -> None:
        ctx = ExecutionContext("python")
        await ctx.__aenter__()
        ctx.mark_success()
        await ctx.__aexit__(RuntimeError, RuntimeError("oops"), None)
        assert ctx.status == "error"

    @pytest.mark.asyncio
    async def test_context_manager_protocol_via_async_with(self) -> None:
        async with ExecutionContext("c") as ctx:
            ctx.mark_success()
        assert ctx.status == "success"

    @pytest.mark.asyncio
    async def test_start_time_set_on_aenter(self) -> None:
        ctx = ExecutionContext("python")
        assert ctx.start_time is None
        await ctx.__aenter__()
        assert ctx.start_time is not None
        await ctx.__aexit__(None, None, None)


# ---------------------------------------------------------------------------
# MetricsMiddleware -- /metrics path skip
# ---------------------------------------------------------------------------


class TestMetricsMiddleware:
    @pytest.mark.asyncio
    async def test_metrics_path_is_passed_through_unchanged(self) -> None:
        from backend.src.metrics import MetricsMiddleware

        mock_response = MagicMock()
        call_next = AsyncMock(return_value=mock_response)

        request = MagicMock()
        request.url.path = "/metrics"

        middleware = MetricsMiddleware.__new__(MetricsMiddleware)
        result = await middleware.dispatch(request, call_next)

        call_next.assert_called_once_with(request)
        assert result is mock_response


# ---------------------------------------------------------------------------
# metrics_response -- MIME type
# ---------------------------------------------------------------------------


class TestMetricsResponse:
    def test_returns_prometheus_content_type(self) -> None:
        from prometheus_client import CONTENT_TYPE_LATEST

        response = metrics_response()
        assert response.media_type == CONTENT_TYPE_LATEST

    def test_response_body_is_bytes(self) -> None:
        response = metrics_response()
        assert isinstance(response.body, bytes)

    def test_response_body_contains_metric_names(self) -> None:
        response = metrics_response()
        body = response.body.decode()
        assert "http_requests_total" in body
        assert "code_execution_total" in body
