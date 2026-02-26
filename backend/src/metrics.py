# Ancient Compute Backend - Prometheus Metrics
"""
Prometheus-style metrics collection for monitoring and observability.

Implements:
- Request counters
- Duration histograms
- Active users gauge
- Language execution metrics
"""

import time

from fastapi import Request
from fastapi.responses import Response
from prometheus_client import CONTENT_TYPE_LATEST, Counter, Gauge, Histogram, Info, generate_latest
from starlette.middleware.base import BaseHTTPMiddleware

# Application info
app_info = Info('ancient_compute_app', 'Ancient Compute application information')
app_info.info({
    'version': '0.1.0',
    'environment': 'development',
    'name': 'Ancient Compute'
})

# Request metrics
http_requests_total = Counter(
    'http_requests_total',
    'Total HTTP requests',
    ['method', 'endpoint', 'status']
)

http_request_duration_seconds = Histogram(
    'http_request_duration_seconds',
    'HTTP request duration in seconds',
    ['method', 'endpoint'],
    buckets=[0.01, 0.05, 0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
)

# Code execution metrics
code_execution_total = Counter(
    'code_execution_total',
    'Total code executions',
    ['language', 'status']
)

code_execution_duration_seconds = Histogram(
    'code_execution_duration_seconds',
    'Code execution duration in seconds',
    ['language'],
    buckets=[0.1, 0.5, 1.0, 5.0, 10.0, 30.0, 60.0]
)

code_compilation_duration_seconds = Histogram(
    'code_compilation_duration_seconds',
    'Code compilation duration in seconds',
    ['language'],
    buckets=[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.0]
)

# Active connections
active_requests = Gauge(
    'active_requests',
    'Number of active HTTP requests'
)

active_executions = Gauge(
    'active_executions',
    'Number of active code executions',
    ['language']
)

# System metrics
database_connections = Gauge(
    'database_connections_active',
    'Number of active database connections'
)

cache_hits = Counter(
    'cache_hits_total',
    'Total cache hits',
    ['cache_type']
)

cache_misses = Counter(
    'cache_misses_total',
    'Total cache misses',
    ['cache_type']
)

# Error metrics
errors_total = Counter(
    'errors_total',
    'Total errors',
    ['error_type', 'endpoint']
)


class MetricsMiddleware(BaseHTTPMiddleware):
    """
    FastAPI middleware for collecting HTTP request metrics.
    """

    async def dispatch(self, request: Request, call_next):
        """Process request and collect metrics"""
        # Skip metrics collection for /metrics endpoint itself
        if request.url.path == "/metrics":
            return await call_next(request)

        method = request.method
        endpoint = request.url.path

        # Increment active requests
        active_requests.inc()

        # Time the request
        start_time = time.time()

        try:
            response = await call_next(request)
            status = response.status_code

            # Record metrics
            http_requests_total.labels(
                method=method,
                endpoint=endpoint,
                status=status
            ).inc()

            duration = time.time() - start_time
            http_request_duration_seconds.labels(
                method=method,
                endpoint=endpoint
            ).observe(duration)

            return response

        except Exception as e:
            # Record error
            errors_total.labels(
                error_type=type(e).__name__,
                endpoint=endpoint
            ).inc()
            raise

        finally:
            # Decrement active requests
            active_requests.dec()


def metrics_response() -> Response:
    """
    Generate Prometheus metrics response.

    Returns:
        Response with Prometheus-formatted metrics
    """
    return Response(
        content=generate_latest(),
        media_type=CONTENT_TYPE_LATEST
    )


# Utility functions for recording metrics

def record_code_execution(language: str, duration: float, status: str):
    """
    Record code execution metrics.

    Args:
        language: Programming language
        duration: Execution duration in seconds
        status: Execution status ('success', 'error', 'timeout')
    """
    code_execution_total.labels(language=language, status=status).inc()
    code_execution_duration_seconds.labels(language=language).observe(duration)


def record_code_compilation(language: str, duration: float):
    """
    Record code compilation metrics.

    Args:
        language: Programming language
        duration: Compilation duration in seconds
    """
    code_compilation_duration_seconds.labels(language=language).observe(duration)


def record_cache_access(cache_type: str, hit: bool):
    """
    Record cache access metrics.

    Args:
        cache_type: Type of cache ('ir', 'compilation', 'execution')
        hit: Whether the access was a cache hit
    """
    if hit:
        cache_hits.labels(cache_type=cache_type).inc()
    else:
        cache_misses.labels(cache_type=cache_type).inc()


class ExecutionContext:
    """
    Context manager for tracking code execution metrics.

    Example:
        async with ExecutionContext('python'):
            result = await execute_code(code)
            if result.status == 'success':
                ctx.mark_success()
            else:
                ctx.mark_error()
    """

    def __init__(self, language: str):
        self.language = language
        self.start_time = None
        self.status = 'error'  # Default to error, mark success explicitly

    async def __aenter__(self):
        self.start_time = time.time()
        active_executions.labels(language=self.language).inc()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        duration = time.time() - self.start_time
        active_executions.labels(language=self.language).dec()

        if exc_type is not None:
            self.status = 'error'

        record_code_execution(self.language, duration, self.status)

    def mark_success(self):
        """Mark execution as successful"""
        self.status = 'success'

    def mark_error(self):
        """Mark execution as error"""
        self.status = 'error'

    def mark_timeout(self):
        """Mark execution as timeout"""
        self.status = 'timeout'
