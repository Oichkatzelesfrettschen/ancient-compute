import logging
import time

from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import Response, JSONResponse
from prometheus_client import Counter, Gauge, generate_latest, CONTENT_TYPE_LATEST
import redis

from .api.router import api_router
from .config import settings
from .rate_limiting import RateLimitMiddleware, RateLimiter
# Note: Avoid importing database/SQLAlchemy at module import time to keep
# app importable in limited environments. Lazily import within handlers.

# Configure logging
logging.basicConfig(
    level=logging.DEBUG if settings.DEBUG else logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

logger = logging.getLogger(__name__)

# Prometheus Metrics (from master)
REQUEST_COUNT = Counter("requests_total", "Total number of requests")
UPTIME = Gauge("uptime_seconds", "Time the service has been running")
START_TIME = time.time()

# Create FastAPI application
app = FastAPI(
    title="Ancient Compute API",
    description="Educational platform for 12,500 years of computational history",
    version="0.1.0",
    docs_url="/docs" if settings.DEBUG else None,
    redoc_url="/redoc" if settings.DEBUG else None,
)

# Add trusted host middleware (production only) (from copilot)
if not settings.DEBUG:
    app.add_middleware(
        TrustedHostMiddleware,
        allowed_hosts=["ancient-compute.com", "*.ancient-compute.com", "localhost"]
    )

# Configure CORS (from both)
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Add security headers middleware (from copilot)
@app.middleware("http")
async def add_security_headers(request: Request, call_next):
    """Add security headers to all responses"""
    response = await call_next(request)
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    if not settings.DEBUG:
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
    return response

# Add rate limiting middleware (from copilot)
app.add_middleware(RateLimitMiddleware, limiter=RateLimiter())

# Middleware to count requests (from master)
@app.middleware("http")
async def count_requests(request: Request, call_next):
    """Increment Prometheus request counter"""
    REQUEST_COUNT.inc()
    response = await call_next(request)
    return response

# Include API router
app.include_router(api_router, prefix="/api/v1")


# Health check endpoints
@app.get("/health")
async def health_check():
    """Basic health check endpoint"""
    return {"status": "healthy", "service": "ancient-compute-backend"}


@app.get("/ready")
async def readiness_check():
    """Readiness check - verifies dependencies are available"""
    # (Implementation from master)
    db_ok = False
    redis_ok = False
    db_error = None
    redis_error = None

    try:
        # Local imports to avoid hard dependency at module import time
        from sqlalchemy import text  # type: ignore
        from .database import SessionLocal  # type: ignore

        db = SessionLocal()
        db.execute(text("SELECT 1"))
        db.close()
        db_ok = True
    except Exception as e:
        db_error = str(e)
        logger.error(f"Database readiness check failed: {e}")

    try:
        r = redis.from_url(settings.REDIS_URL)
        r.ping()
        redis_ok = True
    except Exception as e:
        redis_error = str(e)
        logger.error(f"Redis readiness check failed: {e}")

    if db_ok and redis_ok:
        return {"status": "ready", "service": "ancient-compute-backend"}
    else:
        details = {}
        if not db_ok:
            details["database"] = f"failed: {db_error}"
        if not redis_ok:
            details["redis"] = f"failed: {redis_error}"
            
        raise HTTPException(status_code=503, detail={"status": "Service Unavailable", "checks": details})


@app.get("/metrics")
async def metrics():
    """Prometheus metrics endpoint"""
    # (Implementation from master, fixed to use Response)
    UPTIME.set(time.time() - START_TIME)
    return Response(content=generate_latest().decode("utf-8"), media_type=CONTENT_TYPE_LATEST)


@app.get("/")
async def root():
    """Root endpoint"""
    # (From copilot, updated to use START_TIME)
    uptime = int(time.time() - START_TIME)
    return {
        "service": "Ancient Compute API",
        "version": "0.1.0",
        "description": "Teaching 12,500 years of computational history",
        "docs": "/docs" if settings.DEBUG else "Documentation disabled in production",
        "uptime_seconds": uptime,
        "environment": settings.ENVIRONMENT,
    }


# Startup event (from copilot)
@app.on_event("startup")
async def startup_event():
    logger.info("Ancient Compute Backend starting...")
    logger.info(f"Environment: {settings.ENVIRONMENT}")
    logger.info(f"Debug mode: {settings.DEBUG}")
    UPTIME.set(0) # Initialize uptime gauge


# Shutdown event (from copilot)
@app.on_event("shutdown")
async def shutdown_event():
    logger.info("Ancient Compute Backend shutting down...")
