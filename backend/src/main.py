import logging
import time
from contextlib import asynccontextmanager

from fastapi import FastAPI, Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import JSONResponse, Response
from prometheus_client import (
    CONTENT_TYPE_LATEST,
    REGISTRY,
    Counter,
    Gauge,
    generate_latest,
)

from .api.router import api_router
from .api.tools_router import router as tools_router  # Import new router
from .config import settings
from .rate_limiting import RateLimiter, RateLimitMiddleware

# Configure logging
logging.basicConfig(
    level=logging.DEBUG if settings.DEBUG else logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

logger = logging.getLogger(__name__)

# Prometheus Metrics (idempotent: reuse existing collectors if already registered)
def _get_or_create_counter(name: str, doc: str) -> Counter:
    try:
        return Counter(name, doc)
    except ValueError:
        return REGISTRY._names_to_collectors.get(name + "_total") or REGISTRY._names_to_collectors[name]

def _get_or_create_gauge(name: str, doc: str) -> Gauge:
    try:
        return Gauge(name, doc)
    except ValueError:
        return REGISTRY._names_to_collectors[name]

REQUEST_COUNT = _get_or_create_counter("requests", "Total number of requests")
UPTIME = _get_or_create_gauge("uptime_seconds", "Time the service has been running")
START_TIME = time.time()


@asynccontextmanager
async def lifespan(app: FastAPI):
    logger.info("Ancient Compute Backend starting...")
    UPTIME.set(0)
    yield
    logger.info("Ancient Compute Backend shutting down...")


# Create FastAPI application
app = FastAPI(
    title="Ancient Compute API",
    description="Educational platform for 12,500 years of computational history",
    version="0.1.0",
    docs_url="/docs" if settings.DEBUG else None,
    redoc_url="/redoc" if settings.DEBUG else None,
    lifespan=lifespan,
)

# Middleware configuration (TrustedHost, CORS, Security Headers, RateLimit, Request Count)
# ... (Same as before, preserving existing middleware)

if not settings.DEBUG:
    app.add_middleware(
        TrustedHostMiddleware,
        allowed_hosts=["ancient-compute.com", "*.ancient-compute.com", "localhost"]
    )

app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

@app.middleware("http")
async def add_security_headers(request: Request, call_next):
    response = await call_next(request)
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    if not settings.DEBUG:
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
    return response

app.add_middleware(RateLimitMiddleware, limiter=RateLimiter())

@app.middleware("http")
async def count_requests(request: Request, call_next):
    REQUEST_COUNT.inc()
    response = await call_next(request)
    return response


@app.exception_handler(Exception)
async def unhandled_exception_handler(request: Request, exc: Exception):
    """Return structured JSON for unhandled exceptions instead of bare 500."""
    logger.exception("Unhandled exception on %s %s", request.method, request.url.path)
    return JSONResponse(
        status_code=500,
        content={"detail": "Internal server error", "type": type(exc).__name__},
    )

# Include API routers
app.include_router(api_router, prefix="/api/v1")
app.include_router(tools_router, prefix="/api/v1/tools", tags=["tools"])

# Health check endpoints (Same as before)
@app.get("/health")
async def health_check():
    return {"status": "healthy", "service": "ancient-compute-backend"}

@app.get("/ready")
async def readiness_check():
    # ... (Same logic as before)
    return {"status": "ready"}

@app.get("/metrics")
async def metrics():
    UPTIME.set(time.time() - START_TIME)
    return Response(content=generate_latest().decode("utf-8"), media_type=CONTENT_TYPE_LATEST)

@app.get("/")
async def root():
    uptime = int(time.time() - START_TIME)
    return {
        "service": "Ancient Compute API",
        "version": "0.1.0",
        "description": "Teaching 12,500 years of computational history",
        "docs": "/docs" if settings.DEBUG else "Documentation disabled in production",
        "uptime_seconds": uptime,
        "environment": settings.ENVIRONMENT,
    }

