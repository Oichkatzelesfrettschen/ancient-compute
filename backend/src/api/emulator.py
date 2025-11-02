"""
Difference Engine No. 2 Emulator API Endpoints

Provides REST API for the emulator frontend:
- Polynomial execution with results streaming
- Debugging with breakpoints and variables
- State inspection and management
"""

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import List, Tuple, Dict, Any, Optional
import sys
import os

# Add backend to path for imports
sys.path.insert(0, os.path.join(os.path.dirname(__file__), "..", "..", ".."))

from backend.src.emulator.machine import DEMachine
from backend.src.emulator.debugger import Debugger, BreakpointType
from backend.src.emulator.timing import MechanicalPhase

# Router
router = APIRouter(prefix="/api", tags=["emulator"])

# Global emulator state (in production, this should be session-based or in a database)
emulator_instance: Optional[DEMachine] = None
debugger_instance: Optional[Debugger] = None


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
async def initialize_emulator():
    """Initialize a new emulator instance"""
    global emulator_instance, debugger_instance

    try:
        emulator_instance = DEMachine()
        debugger_instance = Debugger(emulator_instance)
        return {"success": True, "message": "Emulator initialized successfully"}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/reset")
async def reset_emulator():
    """Reset emulator to initial state"""
    global emulator_instance, debugger_instance

    try:
        if emulator_instance is None:
            emulator_instance = DEMachine()
        else:
            emulator_instance = DEMachine()

        if debugger_instance is None:
            debugger_instance = Debugger(emulator_instance)
        else:
            debugger_instance.reset()

        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.get("/state")
async def get_state():
    """Get current emulator state"""
    global emulator_instance

    if emulator_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        return {
            "success": True,
            "state": {
                "cycle": emulator_instance.cycle_count,
                "phase": emulator_instance.timing.phase.value,
                "angle": emulator_instance.timing.angle,
                "columns": emulator_instance.column_bank.get_all_values(),
                "carrySignals": emulator_instance.carriage.carry_signals.copy(),
                "accumulator": int(emulator_instance.analytical_engine.registers.get("A", 0)),
                "totalOperations": emulator_instance.total_operations,
            },
        }
    except Exception as e:
        return {"success": False, "error": str(e)}


# ============================================================================
# Polynomial Execution Endpoints
# ============================================================================


@router.post("/execute")
async def execute_polynomial(request: ExecuteRequest):
    """
    Execute polynomial evaluation

    Returns results as they are computed
    """
    global emulator_instance

    try:
        # Initialize if needed
        if emulator_instance is None:
            emulator_instance = DEMachine()

        # Validate input
        if request.x_range[0] < 0 or request.x_range[1] < 0:
            raise ValueError("X range values must be non-negative")
        if request.x_range[0] > request.x_range[1]:
            raise ValueError("X start must be less than or equal to X end")

        # Execute polynomial evaluation
        try:
            results_data = emulator_instance.evaluate_polynomial(
                request.coefficients, request.x_range
            )
        except Exception as e:
            raise ValueError(f"Polynomial evaluation failed: {str(e)}")

        # Build results array
        results = []
        emulator_instance_temp = DEMachine()  # Fresh instance for evaluation
        for x in range(request.x_range[0], request.x_range[1] + 1):
            # Evaluate single polynomial value
            temp_results = emulator_instance_temp.evaluate_polynomial(request.coefficients, (x, x))
            result = temp_results[0] if temp_results else 0

            results.append(
                ExecutionResultItem(
                    x=x,
                    result=result,
                    cycle=emulator_instance_temp.cycle_count,
                    phase=emulator_instance_temp.timing.phase.value,
                )
            )

        return ExecuteResponse(
            success=True, results=results, totalCycles=emulator_instance_temp.cycle_count
        )

    except ValueError as e:
        return ExecuteResponse(success=False, error=str(e))
    except Exception as e:
        return ExecuteResponse(success=False, error=f"Internal error: {str(e)}")


@router.get("/results")
async def get_results():
    """Get previous execution results"""
    # Note: In a real application, this would be stored in session
    return {"success": True, "results": []}


# ============================================================================
# Debugging Endpoints
# ============================================================================


@router.post("/debug/step")
async def debug_step():
    """Step through one mechanical cycle with debugger"""
    global emulator_instance, debugger_instance

    if emulator_instance is None or debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        triggered_breakpoints = debugger_instance.step_cycle()

        state = {
            "cycle": emulator_instance.cycle_count,
            "phase": emulator_instance.timing.phase.value,
            "angle": emulator_instance.timing.angle,
            "columns": emulator_instance.column_bank.get_all_values(),
            "carrySignals": emulator_instance.carriage.carry_signals.copy(),
            "accumulator": int(emulator_instance.analytical_engine.registers.get("A", 0)),
            "totalOperations": emulator_instance.total_operations,
        }

        return {
            "success": True,
            "state": state,
            "breakpointsHit": triggered_breakpoints if triggered_breakpoints else [],
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/debug/continue")
async def debug_continue(max_cycles: Optional[int] = None):
    """Continue execution until breakpoint or max cycles"""
    global emulator_instance, debugger_instance

    if emulator_instance is None or debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        result = debugger_instance.continue_execution(max_cycles=max_cycles)

        state = {
            "cycle": emulator_instance.cycle_count,
            "phase": emulator_instance.timing.phase.value,
            "angle": emulator_instance.timing.angle,
            "columns": emulator_instance.column_bank.get_all_values(),
            "carrySignals": emulator_instance.carriage.carry_signals.copy(),
            "accumulator": int(emulator_instance.analytical_engine.registers.get("A", 0)),
            "totalOperations": emulator_instance.total_operations,
        }

        return {
            "success": True,
            "cyclesRun": result.get("cycles_run", 0),
            "state": state,
            "breakpointHit": result.get("breakpoint_hit"),
        }

    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/debug/breakpoint")
async def set_breakpoint(request: BreakpointRequest):
    """Set a new breakpoint"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        breakpoint_type = BreakpointType[request.type]
        kwargs = {}

        if request.type == "CYCLE" and request.cycle_target is not None:
            kwargs["cycle_target"] = request.cycle_target
        elif request.type == "PHASE" and request.phase_target is not None:
            kwargs["phase_target"] = MechanicalPhase[request.phase_target]
        elif request.type == "VALUE_CHANGE" and request.variable_name is not None:
            kwargs["variable_name"] = request.variable_name

        bp_id = debugger_instance.breakpoint_manager.set_breakpoint(breakpoint_type, **kwargs)

        return {"success": True, "breakpointId": bp_id}

    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/debug/breakpoint/{breakpoint_id}/enable")
async def enable_breakpoint(breakpoint_id: int):
    """Enable a breakpoint"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        debugger_instance.enable_breakpoint(breakpoint_id)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/debug/breakpoint/{breakpoint_id}/disable")
async def disable_breakpoint(breakpoint_id: int):
    """Disable a breakpoint"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        debugger_instance.disable_breakpoint(breakpoint_id)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.delete("/debug/breakpoint/{breakpoint_id}")
async def remove_breakpoint(breakpoint_id: int):
    """Remove a breakpoint"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        debugger_instance.remove_breakpoint(breakpoint_id)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.post("/debug/variable")
async def define_variable(request: VariableRequest):
    """Define a debugger variable"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        debugger_instance.define_variable(request.name, request.value)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}


@router.put("/debug/variable/{name}")
async def set_variable(name: str, request: VariableRequest):
    """Update a debugger variable value"""
    global debugger_instance

    if debugger_instance is None:
        raise HTTPException(status_code=400, detail="Emulator not initialized")

    try:
        debugger_instance.set_variable(name, request.value)
        return {"success": True}
    except Exception as e:
        return {"success": False, "error": str(e)}
