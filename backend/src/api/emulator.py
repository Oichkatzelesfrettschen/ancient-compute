"""
Difference Engine No. 2 Emulator API Endpoints

Provides REST API for the emulator frontend:
- Polynomial execution with results streaming
- Debugging with breakpoints and variables
- State inspection and management
"""

from typing import Any

from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel

from backend.src.emulator.debugger import BreakpointType, Debugger
from backend.src.emulator.machine import DEMachine
from backend.src.emulator.timing import MechanicalPhase

# Router
router = APIRouter(prefix="/emulator", tags=["emulator"])


class EmulatorState:
    """Holds the emulator and debugger instances for DI.

    In production this would be per-session or per-user; for now a single
    shared instance is sufficient and avoids bare module-level globals.
    """

    def __init__(self) -> None:
        self.emulator: DEMachine | None = None
        self.debugger: Debugger | None = None


_state = EmulatorState()


def get_emulator_state() -> EmulatorState:
    """FastAPI dependency returning the shared EmulatorState."""
    return _state


# ============================================================================
# Request/Response Models
# ============================================================================


class ExecuteRequest(BaseModel):
    """Request to execute a polynomial evaluation"""

    coefficients: list[int]
    x_range: tuple[int, int]
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
    results: list[ExecutionResultItem] | None = None
    totalCycles: int | None = None
    executionTime: float | None = None
    error: str | None = None


class BreakpointRequest(BaseModel):
    """Request to set a breakpoint"""

    type: str  # CYCLE, PHASE, VALUE_CHANGE, CONDITION
    cycle_target: int | None = None
    phase_target: str | None = None
    variable_name: str | None = None


class BreakpointResponse(BaseModel):
    """Response from breakpoint operation"""

    success: bool
    breakpointId: int | None = None
    error: str | None = None


class VariableRequest(BaseModel):
    """Request to define or update a variable"""

    name: str
    value: int


class StateResponse(BaseModel):
    """Current machine state"""

    cycle: int
    phase: str
    angle: int
    columns: list[int]
    carrySignals: list[bool]
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
        return ExecuteResponse(success=False, error="X range values must be non-negative")
    if request.x_range[0] > request.x_range[1]:
        return ExecuteResponse(
            success=False, error="X start must be less than or equal to X end"
        )

    try:
        results = []
        emu = state.emulator
        for x in range(request.x_range[0], request.x_range[1] + 1):
            temp_results = emu.evaluate_polynomial(
                request.coefficients, (x, x)
            )
            result = temp_results[0] if temp_results else 0
            results.append(
                ExecutionResultItem(
                    x=x,
                    result=result,
                    cycle=emu.cycle_count,
                    phase=emu.timing.phase.value,
                )
            )

        return ExecuteResponse(
            success=True,
            results=results,
            totalCycles=emu.cycle_count,
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
    max_cycles: int | None = 1000,
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

    try:
        breakpoint_type = BreakpointType[request.type]
    except KeyError:
        return BreakpointResponse(
            success=False, error=f"Invalid breakpoint type: {request.type}"
        )
    kwargs: dict[str, Any] = {}

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
