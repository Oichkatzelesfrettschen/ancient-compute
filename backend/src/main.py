# Ancient Compute Backend - FastAPI Application

from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
import logging

from .config import settings
from .api.router import api_router

# Configure logging
logging.basicConfig(
    level=logging.DEBUG if settings.DEBUG else logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

logger = logging.getLogger(__name__)

# Create FastAPI application
app = FastAPI(
    title="Ancient Compute API",
    description="Educational platform for 12,500 years of computational history",
    version="0.1.0",
    docs_url="/docs" if settings.DEBUG else None,
    redoc_url="/redoc" if settings.DEBUG else None,
)

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
    # TODO: Add database and redis connection checks
    return {"status": "ready", "service": "ancient-compute-backend"}

@app.get("/metrics")
async def metrics():
    """Basic metrics endpoint for monitoring"""
    # TODO: Add Prometheus-style metrics
    return {
        "service": "ancient-compute-backend",
        "uptime_seconds": 0,  # TODO: Implement actual uptime tracking
        "requests_total": 0,  # TODO: Implement request counting
        "active_users": 0,  # TODO: Query from database
        "modules_count": 0,  # TODO: Query from database
        "lessons_count": 0,  # TODO: Query from database
    }

@app.get("/")
async def root():
    """Root endpoint"""
    return {
        "service": "Ancient Compute API",
        "version": "0.1.0",
        "description": "Teaching 12,500 years of computational history",
        "docs": "/docs" if settings.DEBUG else "Documentation disabled in production"
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
