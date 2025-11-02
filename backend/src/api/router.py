# Ancient Compute Backend - Main API Router

from fastapi import APIRouter

from .code_execution import router as execution_router
from .emulator import router as emulator_router
from .timeline import router as timeline_router

# Create main API router
api_router = APIRouter()

# Include code execution routes
api_router.include_router(execution_router)

# Include emulator routes
api_router.include_router(emulator_router)

# Include timeline content delivery routes
api_router.include_router(timeline_router)


@api_router.get("/status")
async def get_status():
    """Get API status"""
    return {"status": "operational", "version": "0.1.0", "service": "ancient-compute-api"}


@api_router.get("/modules")
async def list_modules():
    """List all educational modules"""
    # Placeholder - will be implemented with actual database queries
    return {
        "modules": [
            {
                "id": "module-0",
                "title": "Prehistory of Counting (20,000 BC - 3000 BC)",
                "description": "Ishango bone, clay tokens, one-to-one correspondence",
            },
            {
                "id": "module-1",
                "title": "Ancient Foundations (3000 BC - 500 AD)",
                "description": "Babylonian algorithms, Greek logic, Panini's grammar",
            },
        ]
    }


@api_router.get("/timeline")
async def get_timeline():
    """Get historical timeline events"""
    # Placeholder - will be implemented with actual database queries
    return {
        "timeline": [
            {
                "year": -20000,
                "title": "Ishango Bone",
                "description": "Earliest evidence of mathematical thinking",
            },
            {
                "year": -3000,
                "title": "Babylonian Cuneiform",
                "description": "Development of sexagesimal number system",
            },
        ]
    }
