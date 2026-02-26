from typing import Dict, Any
from fastapi import APIRouter, Depends, HTTPException
from backend.src.models.api_schemas import EmulatorState, DebuggerCommand, PerformanceReport
from backend.src.services.engine_tools_service import EngineTools

router = APIRouter()

_tools = EngineTools()


def get_tools() -> EngineTools:
    """FastAPI dependency returning the shared EngineTools instance."""
    return _tools


@router.post("/debug/step", response_model=Dict[str, Any])
async def debug_step(tools: EngineTools = Depends(get_tools)):
    """Execute one instruction step."""
    return tools.step()


@router.post("/debug/command")
async def debug_command(
    cmd: DebuggerCommand,
    tools: EngineTools = Depends(get_tools),
):
    """Execute debugger command."""
    if cmd.action == "reset":
        tools.engine.__init__()
        return {"status": "reset"}
    elif cmd.action == "continue":
        for _ in range(100):
            res = tools.step()
            if res["triggered_breakpoints"]:
                return {"status": "paused", "reason": "breakpoint"}
        return {"status": "paused", "reason": "limit_reached"}

    return {"status": "unknown_command"}


@router.get("/performance", response_model=PerformanceReport)
async def get_performance(tools: EngineTools = Depends(get_tools)):
    """Get performance analysis report."""
    return tools.get_performance_report()
