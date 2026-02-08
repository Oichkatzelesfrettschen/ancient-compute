from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any, Union

class ExecutionRequest(BaseModel):
    language: str
    code: str
    input_data: Optional[str] = None

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
    ir_code: Optional[str] = None
    assembly_code: Optional[str] = None
    errors: List[str] = []

class EmulatorState(BaseModel):
    pc: int
    registers: Dict[str, Union[int, float, str]]
    memory_preview: Dict[int, Union[int, float, str]]
    flags: Dict[str, bool]
    cycle_count: int

class AnalyticalEngineSnapshot(BaseModel):
    pc: int
    registers: Dict[str, Union[int, float]] # Assuming values are decimals
    flags: Dict[str, bool]
    clock_time: int
    barrel: Dict[str, Union[str, int, None]] # active, step
    mill_operand_buffer: float
    mill_result_buffer: float
    active_store_address: Optional[int] = None

class DebuggerCommand(BaseModel):
    action: str = Field(..., description="step, continue, pause, reset")
    breakpoints: Optional[List[int]] = None

class PerformanceReport(BaseModel):
    total_cycles: int
    instruction_frequency: Dict[str, int]
    hot_spots: Dict[str, Any]
    suggestions: List[str]
