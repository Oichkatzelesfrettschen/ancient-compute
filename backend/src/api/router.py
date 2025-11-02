# Ancient Compute Backend - Main API Router

from fastapi import APIRouter

from .code_execution import router as code_execution_router
from .emulator import router as emulator_router
from .timeline import router as timeline_router
from .execution import router as exercise_execution_router
import os
import json
from pathlib import Path
from fastapi.responses import PlainTextResponse

# Create main API router
api_router = APIRouter()

# Include code execution routes
api_router.include_router(code_execution_router)

# Include emulator routes
api_router.include_router(emulator_router)

# Include timeline content delivery routes
api_router.include_router(timeline_router)

# Include exercise execution routes (code submission and validation)
api_router.include_router(exercise_execution_router)


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


@api_router.get("/infra/minix/metrics")
async def get_minix_metrics(arch: str = "i386"):
    """Serve latest MINIX boot metrics if present.

    Looks under METRICS_DIR (default: metrics/minix) for `<arch>/boot_time.json`.
    """
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch
    boot_json = base / "boot_time.json"
    if boot_json.exists():
        try:
            data = json.loads(boot_json.read_text())
            return {"available": True, "arch": arch, "metrics": data}
        except Exception:
            return {"available": False, "arch": arch, "error": "invalid_json"}
    return {"available": False, "arch": arch}


@api_router.get("/infra/minix/runs")
async def list_minix_runs(arch: str = "i386"):
    """List available MINIX run IDs with basic stats."""
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch / "runs"
    if not base.exists():
        return {"arch": arch, "runs": []}
    runs = []
    for run_dir in sorted(base.iterdir()):
        bt = run_dir / "boot_time.json"
        if bt.exists():
            try:
                data = json.loads(bt.read_text())
                runs.append(
                    {
                        "runId": run_dir.name,
                        "timestamp": data.get("timestamp"),
                        "bootDurationMs": data.get("boot_duration_ms"),
                    }
                )
            except Exception:
                continue
    return {"arch": arch, "runs": runs}


@api_router.get("/infra/minix/run/{run_id}")
async def get_minix_run(run_id: str, arch: str = "i386"):
    """Get specific run metrics and file names."""
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch / "runs" / run_id
    bt = base / "boot_time.json"
    if not bt.exists():
        return {"found": False, "arch": arch, "runId": run_id}
    try:
        data = json.loads(bt.read_text())
    except Exception:
        data = None
    resp = {
        "found": True,
        "arch": arch,
        "runId": run_id,
        "metrics": data,
        "files": {
            "resource": (base / "resource_timeseries.csv").exists(),
            "bootLog": (base / "boot.log").exists(),
            "debugLog": (base / "qemu-debug.log").exists(),
        },
    }
    return resp


@api_router.get("/infra/minix/summary")
async def get_minix_summary(arch: str = "i386"):
    """Return summary.json if present."""
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch
    summary = base / "summary.json"
    if summary.exists():
        try:
            data = json.loads(summary.read_text())
            return {"available": True, "arch": arch, "summary": data}
        except Exception:
            return {"available": False, "arch": arch, "error": "invalid_json"}
    return {"available": False, "arch": arch}


@api_router.get("/infra/minix/resource")
async def get_minix_resource(arch: str = "i386"):
    """Return latest resource_timeseries.csv as text, if present."""
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch
    csv = base / "resource_timeseries.csv"
    if csv.exists():
        return PlainTextResponse(content=csv.read_text(), media_type="text/plain")
    return PlainTextResponse(status_code=404, content="")


@api_router.get("/infra/minix/run/{run_id}/resource")
async def get_minix_run_resource(run_id: str, arch: str = "i386"):
    """Return run-specific resource_timeseries.csv as text, if present."""
    base = Path(os.getenv("METRICS_DIR", "metrics/minix")) / arch / "runs" / run_id
    csv = base / "resource_timeseries.csv"
    if csv.exists():
        return PlainTextResponse(content=csv.read_text(), media_type="text/plain")
    return PlainTextResponse(status_code=404, content="")


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
