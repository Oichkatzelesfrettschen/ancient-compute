from typing import Any, Union

from pydantic import BaseModel, Field


class ExecutionRequest(BaseModel):
    language: str
    code: str
    input_data: str | None = None

class ExecutionResult(BaseModel):
    stdout: str
    stderr: str
    exit_code: int
    duration_ms: float

class CompilationRequest(BaseModel):
    language: str
    source_code: str
    optimize: bool = False

class CompilationResult(BaseModel):
    success: bool
    ir_code: str | None = None
    assembly_code: str | None = None
    errors: list[str] = []

class EmulatorState(BaseModel):
    pc: int
    registers: dict[str, Union[int, float, str]]
    memory_preview: dict[int, Union[int, float, str]]
    flags: dict[str, bool]
    cycle_count: int

class AnalyticalEngineSnapshot(BaseModel):
    pc: int
    registers: dict[str, Union[int, float]] # Assuming values are decimals
    flags: dict[str, bool]
    clock_time: int
    barrel: dict[str, Union[str, int, None]] # active, step
    mill_operand_buffer: float
    mill_result_buffer: float
    active_store_address: int | None = None

class DebuggerCommand(BaseModel):
    action: str = Field(..., description="step, continue, pause, reset")
    breakpoints: list[int] | None = None

class PerformanceReport(BaseModel):
    total_cycles: int
    instruction_frequency: dict[str, int]
    hot_spots: dict[str, Any]
    suggestions: list[str]
