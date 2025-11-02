# Ancient Compute Backend - FastAPI Application

import logging
import time

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.middleware.trustedhost import TrustedHostMiddleware
from fastapi.responses import JSONResponse

from .api.router import api_router
from .config import settings
from .rate_limiting import RateLimitMiddleware, RateLimiter
from .metrics import MetricsMiddleware, metrics_response

# Configure logging
logging.basicConfig(
    level=logging.DEBUG if settings.DEBUG else logging.INFO,
    format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
)

logger = logging.getLogger(__name__)

# Track application startup time
APP_START_TIME = time.time()

# Create FastAPI application
app = FastAPI(
    title="Ancient Compute API",
    description="Educational platform for 12,500 years of computational history",
    version="0.1.0",
    docs_url="/docs" if settings.DEBUG else None,
    redoc_url="/redoc" if settings.DEBUG else None,
)

# Add security headers middleware
@app.middleware("http")
async def add_security_headers(request, call_next):
    """Add security headers to all responses"""
    response = await call_next(request)
    response.headers["X-Content-Type-Options"] = "nosniff"
    response.headers["X-Frame-Options"] = "DENY"
    response.headers["X-XSS-Protection"] = "1; mode=block"
    if not settings.DEBUG:
        response.headers["Strict-Transport-Security"] = "max-age=31536000; includeSubDomains"
    return response

# Add trusted host middleware (production only)
if not settings.DEBUG:
    app.add_middleware(
        TrustedHostMiddleware,
        allowed_hosts=["ancient-compute.com", "*.ancient-compute.com", "localhost"]
    )

# Add metrics middleware
app.add_middleware(MetricsMiddleware)

# Add rate limiting middleware
app.add_middleware(RateLimitMiddleware, limiter=RateLimiter())

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
    """
    Readiness check - verifies dependencies are available.
    
    Returns 200 if service is ready to handle requests,
    503 if service is starting up or dependencies are unavailable.
    """
    try:
        # TODO: Check database connectivity
        # from .database import engine
        # engine.connect()
        
        # TODO: Check Redis connectivity
        # from .services.cache import redis_client
        # redis_client.ping()
        
        return {
            "status": "ready",
            "service": "ancient-compute-backend",
            "checks": {
                "database": "ok",  # TODO: Implement actual check
                "redis": "ok",  # TODO: Implement actual check
            }
        }
    except Exception as e:
        logger.error(f"Readiness check failed: {e}")
        return JSONResponse(
            status_code=503,
            content={
                "status": "not_ready",
                "service": "ancient-compute-backend",
                "error": str(e)
            }
        )


@app.get("/metrics")
async def metrics():
    """Prometheus-style metrics endpoint for monitoring"""
    return metrics_response()


@app.get("/")
async def root():
    """Root endpoint"""
    uptime = int(time.time() - APP_START_TIME)
    return {
        "service": "Ancient Compute API",
        "version": "0.1.0",
        "description": "Teaching 12,500 years of computational history",
        "docs": "/docs" if settings.DEBUG else "Documentation disabled in production",
        "uptime_seconds": uptime,
        "environment": settings.ENVIRONMENT,
    }


# Startup event
@app.on_event("startup")
async def startup_event():
    logger.info("Ancient Compute Backend starting...")
    logger.info(f"Environment: {settings.ENVIRONMENT}")
    logger.info(f"Debug mode: {settings.DEBUG}")


# Shutdown event
@app.on_event("shutdown")
async def shutdown_event():
    logger.info("Ancient Compute Backend shutting down...")
