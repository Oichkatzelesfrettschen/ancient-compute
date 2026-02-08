from typing import Dict, Any
from fastapi import APIRouter, HTTPException, Body
from backend.src.models.api_schemas import EmulatorState, DebuggerCommand, PerformanceReport
from backend.src.services.engine_tools_service import EngineTools

router = APIRouter()

# Singleton instance for simple stateful interaction in dev mode
# In production, this would be per-user/session managed via dependency injection
tools_instance = EngineTools()

@router.post("/debug/step", response_model=Dict[str, Any])
async def debug_step():
    """Execute one instruction step."""
    return tools_instance.step()

@router.post("/debug/command")
async def debug_command(cmd: DebuggerCommand):
    """Execute debugger command."""
    if cmd.action == "reset":
        tools_instance.engine.__init__()
        return {"status": "reset"}
    elif cmd.action == "continue":
        # Simplified continue
        for _ in range(100): # Safety limit
            res = tools_instance.step()
            if res["triggered_breakpoints"]:
                return {"status": "paused", "reason": "breakpoint"}
        return {"status": "paused", "reason": "limit_reached"}
    
    return {"status": "unknown_command"}

@router.get("/performance", response_model=PerformanceReport)
async def get_performance():
    """Get performance analysis report."""
    return tools_instance.get_performance_report()
