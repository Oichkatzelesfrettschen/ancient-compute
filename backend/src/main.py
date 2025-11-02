# Ancient Compute Backend - FastAPI Application

import logging
import time
from fastapi import FastAPI, Request, HTTPException
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from prometheus_client import Counter, Gauge, generate_latest, CONTENT_TYPE_LATEST
import redis

from .api.router import api_router
from .config import settings
# Note: Avoid importing database/SQLAlchemy at module import time to keep
# app importable in limited environments. Lazily import within handlers.

# Configure logging
logging.basicConfig(
    level=logging.DEBUG if settings.DEBUG else logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

logger = logging.getLogger(__name__)

# Prometheus Metrics
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

# Middleware to count requests
@app.middleware("http")
async def count_requests(request: Request, call_next):
    REQUEST_COUNT.inc()
    response = await call_next(request)
    return response

# Configure CORS
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.ALLOWED_ORIGINS,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

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
    db_ok = False
    redis_ok = False
    try:
        # Local imports to avoid hard dependency at module import time
        from sqlalchemy import text  # type: ignore
        from .database import SessionLocal  # type: ignore

        db = SessionLocal()
        db.execute(text("SELECT 1"))
        db.close()
        db_ok = True
    except Exception as e:
        logger.error(f"Database readiness check failed: {e}")

    try:
        r = redis.from_url(settings.REDIS_URL)
        r.ping()
        redis_ok = True
    except Exception as e:
        logger.error(f"Redis readiness check failed: {e}")

    if db_ok and redis_ok:
        return {"status": "ready", "service": "ancient-compute-backend"}
    else:
        raise HTTPException(status_code=503, detail="Service Unavailable")


@app.get("/metrics")
async def metrics():
    """Prometheus metrics endpoint"""
    UPTIME.set(time.time() - START_TIME)
    return JSONResponse(content=generate_latest().decode("utf-8"), media_type=CONTENT_TYPE_LATEST)


@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "service": "Ancient Compute API",
        "version": "0.1.0",
        "description": "Teaching 12,500 years of computational history",
        "docs": "/docs" if settings.DEBUG else "Documentation disabled in production",
    }


# Startup event
@app.on_event("startup")
async def startup_event():
    logger.info("Ancient Compute Backend starting...")
    logger.info(f"Environment: {settings.ENVIRONMENT}")
    logger.info(f"Debug mode: {settings.DEBUG}")
    UPTIME.set(0)


# Shutdown event
@app.on_event("shutdown")
async def shutdown_event():
    logger.info("Ancient Compute Backend shutting down...")
