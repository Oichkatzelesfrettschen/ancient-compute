"""
Difference Engine No. 2 Emulator API Endpoints

Provides REST API for the emulator frontend:
- Polynomial execution with results streaming
- Debugging with breakpoints and variables
- State inspection and management
"""

from fastapi import APIRouter, HTTPException, Depends
from pydantic import BaseModel
from typing import List, Tuple, Dict, Any, Optional

from backend.src.emulator.machine import DEMachine
from backend.src.emulator.debugger import Debugger, BreakpointType
from backend.src.emulator.timing import MechanicalPhase

# Router
router = APIRouter(prefix="/emulator", tags=["emulator"])


class EmulatorState:
    """Holds the emulator and debugger instances for DI.

    In production this would be per-session or per-user; for now a single
    shared instance is sufficient and avoids bare module-level globals.
    """

    def __init__(self) -> None:
        self.emulator: Optional[DEMachine] = None
        self.debugger: Optional[Debugger] = None


_state = EmulatorState()


def get_emulator_state() -> EmulatorState:
    """FastAPI dependency returning the shared EmulatorState."""
    return _state


# ============================================================================
# Request/Response Models
# ============================================================================


class ExecuteRequest(BaseModel):
    """Request to execute a polynomial evaluation"""

    coefficients: List[int]
    x_range: Tuple[int, int]
    execution_speed: float = 1.0


class ExecutionResultItem(BaseModel):
    """Single result from polynomial evaluation"""

    x: int
    result: int
    cycle: int
    phase: str


class ExecuteResponse(BaseModel):
    """Response from polynomial execution"""

    success: bool
    results: Optional[List[ExecutionResultItem]] = None
    totalCycles: Optional[int] = None
    executionTime: Optional[float] = None
    error: Optional[str] = None


class BreakpointRequest(BaseModel):
    """Request to set a breakpoint"""

    type: str  # CYCLE, PHASE, VALUE_CHANGE, CONDITION
    cycle_target: Optional[int] = None
    phase_target: Optional[str] = None
    variable_name: Optional[str] = None


class BreakpointResponse(BaseModel):
    """Response from breakpoint operation"""

    success: bool
    breakpointId: Optional[int] = None
    error: Optional[str] = None


class VariableRequest(BaseModel):
    """Request to define or update a variable"""

    name: str
    value: int


class StateResponse(BaseModel):
    """Current machine state"""

    cycle: int
    phase: str
    angle: int
    columns: List[int]
    carrySignals: List[bool]
    accumulator: int
    totalOperations: int


# ============================================================================
# Emulator Management Endpoints
# ============================================================================


@router.post("/initialize")
async def initialize_emulator(
    state: EmulatorState = Depends(get_emulator_state),
):
    """Initialize a new emulator instance"""
    try:
        state.emulator = DEMachine()
        state.debugger = Debugger(state.emulator)
        return {
            "success": True,
            "message": "Emulator initialized successfully",
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.post("/reset")
async def reset_emulator(
    state: EmulatorState = Depends(get_emulator_state),
):
    """Reset emulator to initial state"""
    try:
        state.emulator = DEMachine()
        state.debugger = Debugger(state.emulator)
        return {"success": True}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


@router.get("/state")
async def get_state(
    state: EmulatorState = Depends(get_emulator_state),
):
    """Get current emulator state"""
    if state.emulator is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    emu = state.emulator
    return {
        "success": True,
        "state": {
            "cycle": emu.cycle_count,
            "phase": emu.timing.phase.value,
            "angle": emu.timing.angle,
            "columns": emu.column_bank.get_all_values(),
            "carrySignals": emu.carriage.carry_signals.copy(),
            "accumulator": int(emu.analytical_engine.registers.get("A", 0)),
            "totalOperations": emu.total_operations,
        },
    }


# ============================================================================
# Polynomial Execution Endpoints
# ============================================================================


@router.post("/execute")
async def execute_polynomial(
    request: ExecuteRequest,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Execute polynomial evaluation. Returns results as they are computed."""
    # Initialize if needed
    if state.emulator is None:
        state.emulator = DEMachine()

    if request.x_range[0] < 0 or request.x_range[1] < 0:
        raise HTTPException(status_code=422, detail="X range values must be non-negative")
    if request.x_range[0] > request.x_range[1]:
        raise HTTPException(
            status_code=422,
            detail="X start must be less than or equal to X end",
        )

    try:
        results = []
        temp_emu = DEMachine()
        for x in range(request.x_range[0], request.x_range[1] + 1):
            temp_results = temp_emu.evaluate_polynomial(
                request.coefficients, (x, x)
            )
            result = temp_results[0] if temp_results else 0
            results.append(
                ExecutionResultItem(
                    x=x,
                    result=result,
                    cycle=temp_emu.cycle_count,
                    phase=temp_emu.timing.phase.value,
                )
            )

        return ExecuteResponse(
            success=True,
            results=results,
            totalCycles=temp_emu.cycle_count,
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Polynomial evaluation failed: {e}")


@router.get("/results")
async def get_results():
    """Get previous execution results"""
    # Note: In a real application, this would be stored in session
    return {
        "success": True,
        "results": []
    }


# ============================================================================
# Debugging Endpoints
# ============================================================================


def _require_initialized(state: EmulatorState) -> tuple:
    """Raise 400 if emulator/debugger not initialized. Returns (emu, dbg)."""
    if state.emulator is None or state.debugger is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")
    return state.emulator, state.debugger


def _machine_state_dict(emu: DEMachine) -> dict:
    """Build the standard machine state response dict."""
    return {
        "cycle": emu.cycle_count,
        "phase": emu.timing.phase.value,
        "angle": emu.timing.angle,
        "columns": emu.column_bank.get_all_values(),
        "carrySignals": emu.carriage.carry_signals.copy(),
        "accumulator": int(emu.analytical_engine.registers.get("A", 0)),
        "totalOperations": emu.total_operations,
    }


@router.post("/debug/step")
async def debug_step(
    state: EmulatorState = Depends(get_emulator_state),
):
    """Step through one mechanical cycle with debugger"""
    emu, dbg = _require_initialized(state)
    triggered = dbg.step_cycle()
    return {
        "success": True,
        "state": _machine_state_dict(emu),
        "breakpointsHit": triggered if triggered else [],
    }


@router.post("/debug/continue")
async def debug_continue(
    max_cycles: Optional[int] = 1000,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Continue execution until breakpoint or max cycles"""
    emu, dbg = _require_initialized(state)
    result = dbg.continue_execution(max_cycles=max_cycles or 1000)
    return {
        "success": True,
        "cyclesRun": result.get("cycles_run", 0),
        "state": _machine_state_dict(emu),
        "breakpointHit": result.get("breakpoint_hit"),
    }


@router.post("/debug/breakpoint")
async def set_breakpoint(
    request: BreakpointRequest,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Set a new breakpoint"""
    _, dbg = _require_initialized(state)

    breakpoint_type = BreakpointType[request.type]
    kwargs: Dict[str, Any] = {}

    if request.type == "CYCLE" and request.cycle_target is not None:
        kwargs["cycle_target"] = request.cycle_target
    elif request.type == "PHASE" and request.phase_target is not None:
        kwargs["phase_target"] = MechanicalPhase[request.phase_target]
    elif request.type == "VALUE_CHANGE" and request.variable_name is not None:
        kwargs["variable_name"] = request.variable_name

    bp_id = dbg.breakpoint_manager.set_breakpoint(breakpoint_type, **kwargs)
    return {"success": True, "breakpointId": bp_id}


@router.post("/debug/breakpoint/{breakpoint_id}/enable")
async def enable_breakpoint(
    breakpoint_id: int,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Enable a breakpoint"""
    _, dbg = _require_initialized(state)
    dbg.enable_breakpoint(breakpoint_id)
    return {"success": True}


@router.post("/debug/breakpoint/{breakpoint_id}/disable")
async def disable_breakpoint(
    breakpoint_id: int,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Disable a breakpoint"""
    _, dbg = _require_initialized(state)
    dbg.disable_breakpoint(breakpoint_id)
    return {"success": True}


@router.delete("/debug/breakpoint/{breakpoint_id}")
async def remove_breakpoint(
    breakpoint_id: int,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Remove a breakpoint"""
    _, dbg = _require_initialized(state)
    dbg.remove_breakpoint(breakpoint_id)
    return {"success": True}


@router.post("/debug/variable")
async def define_variable(
    request: VariableRequest,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Define a debugger variable"""
    _, dbg = _require_initialized(state)
    dbg.define_variable(request.name, request.value)
    return {"success": True}


@router.put("/debug/variable/{name}")
async def set_variable(
    name: str,
    request: VariableRequest,
    state: EmulatorState = Depends(get_emulator_state),
):
    """Update a debugger variable value"""
    _, dbg = _require_initialized(state)
    dbg.set_variable(name, request.value)
    return {"success": True}
