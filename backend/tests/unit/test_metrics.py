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


# ---------------------------------------------------------------------------
# ExecutionContext language field and independence
# ---------------------------------------------------------------------------


class TestExecutionContextLanguage:
    """ExecutionContext stores language; multiple instances are independent."""

    def test_language_stored(self) -> None:
        ctx = ExecutionContext("rust")
        assert ctx.language == "rust"

    def test_two_contexts_independent_status(self) -> None:
        ctx1 = ExecutionContext("python")
        ctx2 = ExecutionContext("c")
        ctx1.mark_success()
        ctx2.mark_timeout()
        assert ctx1.status == "success"
        assert ctx2.status == "timeout"

    def test_mark_success_then_error_gives_error(self) -> None:
        ctx = ExecutionContext("haskell")
        ctx.mark_success()
        ctx.mark_error()
        assert ctx.status == "error"

    def test_mark_error_then_success_gives_success(self) -> None:
        ctx = ExecutionContext("lisp")
        ctx.mark_error()
        ctx.mark_success()
        assert ctx.status == "success"

    def test_start_time_is_none_before_aenter(self) -> None:
        ctx = ExecutionContext("python")
        assert ctx.start_time is None


class TestRecordCacheAccessTypes:
    """record_cache_access handles various cache type strings."""

    def test_compilation_cache_hit(self) -> None:
        record_cache_access("compilation", hit=True)

    def test_compilation_cache_miss(self) -> None:
        record_cache_access("compilation", hit=False)

    def test_unknown_cache_type_does_not_raise(self) -> None:
        record_cache_access("custom_type", hit=True)


class TestRecordCodeExecutionEdgeCases:
    """Edge cases for record_code_execution helper."""

    def test_zero_duration_does_not_raise(self) -> None:
        record_code_execution("python", 0.0, "success")

    def test_large_duration_does_not_raise(self) -> None:
        record_code_execution("c", 9999.9, "timeout")

    def test_compile_error_status_does_not_raise(self) -> None:
        record_code_execution("haskell", 0.1, "compile_error")

    def test_runtime_error_status_does_not_raise(self) -> None:
        record_code_execution("lisp", 0.5, "runtime_error")

    def test_record_compilation_large_duration(self) -> None:
        record_code_compilation("assembly", 100.0)


class TestExecutionContextStatusTransitions:
    """ExecutionContext status transition tests."""

    def test_default_status_is_error(self) -> None:
        ctx = ExecutionContext("python")
        assert ctx.status == "error"

    def test_mark_success_sets_status(self) -> None:
        ctx = ExecutionContext("c")
        ctx.mark_success()
        assert ctx.status == "success"

    def test_mark_error_keeps_status_error(self) -> None:
        ctx = ExecutionContext("c")
        ctx.mark_error()
        assert ctx.status == "error"

    def test_mark_timeout_sets_status(self) -> None:
        ctx = ExecutionContext("python")
        ctx.mark_timeout()
        assert ctx.status == "timeout"

    def test_success_then_error_is_error(self) -> None:
        ctx = ExecutionContext("haskell")
        ctx.mark_success()
        ctx.mark_error()
        assert ctx.status == "error"

    def test_language_attribute_preserved(self) -> None:
        ctx = ExecutionContext("lisp")
        assert ctx.language == "lisp"


class TestMetricsRecordFunctions:
    """Additional record_* function smoke tests for all languages."""

    def test_record_execution_all_languages(self) -> None:
        for lang in ["python", "c", "haskell", "lisp", "java", "assembly"]:
            record_code_execution(lang, 0.1, "success")

    def test_record_compilation_all_languages(self) -> None:
        for lang in ["python", "c", "haskell"]:
            record_code_compilation(lang, 0.05)

    def test_record_cache_access_hit_and_miss(self) -> None:
        record_cache_access("execution", hit=True)
        record_cache_access("execution", hit=False)

    def test_record_execution_with_timeout_status(self) -> None:
        record_code_execution("python", 30.0, "timeout")

    def test_metrics_response_content_type(self) -> None:
        resp = metrics_response()
        assert "text" in resp.media_type or "plain" in resp.media_type

    def test_metrics_response_status_200(self) -> None:
        resp = metrics_response()
        assert resp.status_code == 200


class TestExecutionContextStatusTransitionsExtra:
    """Additional ExecutionContext status state machine invariants."""

    def test_initial_status_is_a_string(self) -> None:
        from backend.src.metrics import ExecutionContext
        ctx = ExecutionContext("python")
        assert isinstance(ctx.status, str)

    def test_mark_success_sets_status_success(self) -> None:
        from backend.src.metrics import ExecutionContext
        ctx = ExecutionContext("c")
        ctx.mark_success()
        assert ctx.status == "success"

    def test_mark_error_sets_status_error(self) -> None:
        from backend.src.metrics import ExecutionContext
        ctx = ExecutionContext("python")
        ctx.mark_error()
        assert ctx.status == "error"

    def test_language_attribute_stored(self) -> None:
        from backend.src.metrics import ExecutionContext
        ctx = ExecutionContext("haskell")
        assert ctx.language == "haskell"
