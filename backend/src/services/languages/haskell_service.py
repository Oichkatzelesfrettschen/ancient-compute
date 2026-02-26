"""
Haskell Language Service

Provides FastAPI endpoints for Haskell code compilation and execution.
Integrates with the Haskell compiler pipeline.

Endpoints:
  POST /execute     - Compile and execute Haskell code
  POST /validate    - Validate Haskell code without execution
  GET /capabilities - Service metadata
"""

import asyncio
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from enum import Enum

from backend.src.compilers.haskell_compiler import HaskellCompiler


class ExecutionStatus(Enum):
    """Execution result status"""
    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"


@dataclass
class CompilationResult:
    """Haskell compilation result"""
    status: ExecutionStatus
    output: str
    errors: str
    ir: str
    assembly: str
    machine_code: str
    compile_time_ms: float
    codegen_time_ms: float
    assembly_time_ms: float


class HaskellService:
    """Haskell language compilation service"""

    def __init__(self, executor: ThreadPoolExecutor | None = None, verbose: bool = False) -> None:
        """Initialize service"""
        self.executor = executor or ThreadPoolExecutor(max_workers=4)
        self.verbose = verbose

    async def execute(self, source: str, timeout_seconds: float = 10.0) -> CompilationResult:
        """
        Compile and execute Haskell code

        Args:
            source: Haskell source code
            timeout_seconds: Execution timeout

        Returns:
            CompilationResult with status and outputs
        """
        try:
            result = await asyncio.wait_for(
                self._compile_and_assemble(source),
                timeout=timeout_seconds
            )
            return result
        except TimeoutError:
            return CompilationResult(
                status=ExecutionStatus.TIMEOUT,
                output="",
                errors="Compilation timeout",
                ir="",
                assembly="",
                machine_code="",
                compile_time_ms=0,
                codegen_time_ms=0,
                assembly_time_ms=0
            )

    async def _compile_and_assemble(self, source: str) -> CompilationResult:
        """Compile Haskell source through full pipeline"""
        loop = asyncio.get_event_loop()

        # Phase 1: Haskell compilation (in thread pool)
        compile_start = time.time()
        try:
            compiler = HaskellCompiler(verbose=self.verbose)
            program = await loop.run_in_executor(
                self.executor,
                lambda: compiler.compile(source)
            )
            compile_time = (time.time() - compile_start) * 1000

            ir_output = self._ir_to_string(program)

        except Exception as e:
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                output="",
                errors=str(e),
                ir="",
                assembly="",
                machine_code="",
                compile_time_ms=(time.time() - compile_start) * 1000,
                codegen_time_ms=0,
                assembly_time_ms=0
            )

        # Phase 2: Code generation (in thread pool)
        codegen_start = time.time()
        try:
            codegen = CodeGenerator()
            asm_code = await loop.run_in_executor(
                self.executor,
                lambda: codegen.generate(program)
            )
            codegen_time = (time.time() - codegen_start) * 1000

        except Exception as e:
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                output="",
                errors=f"Code generation error: {str(e)}",
                ir=ir_output,
                assembly="",
                machine_code="",
                compile_time_ms=compile_time,
                codegen_time_ms=(time.time() - codegen_start) * 1000,
                assembly_time_ms=0
            )

        # Phase 3: Assembly (in thread pool)
        assembly_start = time.time()
        try:
            assembler = Assembler()
            machine_code = await loop.run_in_executor(
                self.executor,
                lambda: assembler.assemble(asm_code)
            )
            assembly_time = (time.time() - assembly_start) * 1000

        except Exception as e:
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                output="",
                errors=f"Assembly error: {str(e)}",
                ir=ir_output,
                assembly=asm_code,
                machine_code="",
                compile_time_ms=compile_time,
                codegen_time_ms=codegen_time,
                assembly_time_ms=(time.time() - assembly_start) * 1000
            )

        machine_code_hex = self._format_hex_dump(machine_code)

        return CompilationResult(
            status=ExecutionStatus.SUCCESS,
            output="Compilation successful",
            errors="",
            ir=ir_output,
            assembly=asm_code,
            machine_code=machine_code_hex,
            compile_time_ms=compile_time,
            codegen_time_ms=codegen_time,
            assembly_time_ms=assembly_time
        )

    async def validate(self, source: str) -> CompilationResult:
        """Validate Haskell code without full assembly"""
        loop = asyncio.get_event_loop()
        compile_start = time.time()

        try:
            compiler = HaskellCompiler(verbose=self.verbose)
            program = await loop.run_in_executor(
                self.executor,
                lambda: compiler.compile(source)
            )
            compile_time = (time.time() - compile_start) * 1000

            return CompilationResult(
                status=ExecutionStatus.SUCCESS,
                output="Validation successful",
                errors="",
                ir=self._ir_to_string(program),
                assembly="",
                machine_code="",
                compile_time_ms=compile_time,
                codegen_time_ms=0,
                assembly_time_ms=0
            )

        except Exception as e:
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                output="",
                errors=str(e),
                ir="",
                assembly="",
                machine_code="",
                compile_time_ms=(time.time() - compile_start) * 1000,
                codegen_time_ms=0,
                assembly_time_ms=0
            )

    async def get_capabilities(self) -> dict:
        """Get service capabilities"""
        return {
            "language": "Haskell",
            "version": "1.0.0",
            "features": [
                "function_definitions",
                "pattern_matching",
                "lambda_expressions",
                "let_bindings",
                "case_expressions",
                "polymorphic_types",
                "type_inference",
                "guards",
            ],
            "maxCodeSize": 1_000_000,
            "timeoutSeconds": 10.0,
            "supportsValidation": True,
            "supportsDebug": False,
        }

    def _ir_to_string(self, program) -> str:
        """Format IR for display"""
        lines = []
        lines.append("=== Haskell Intermediate Representation ===\n")

        for func_name, func in program.functions.items():
            lines.append(f"Function: {func_name}")
            lines.append(f"  Parameters: {func.parameters}")

            for block_name, block in func.basic_blocks.items():
                lines.append(f"  Block: {block_name}")

                for instr in block.instructions:
                    lines.append(f"    {instr}")

                if block.terminator:
                    lines.append(f"    {block.terminator}")

            lines.append("")

        return "\n".join(lines)

    def _format_hex_dump(self, data: str) -> str:
        """Format machine code as hex dump"""
        lines = []
        lines.append("=== Machine Code (Babbage ISA) ===\n")

        # Parse hex string into bytes and format
        if isinstance(data, str):
            hex_str = data.replace("0x", "").replace(" ", "")
            for i in range(0, len(hex_str), 32):
                chunk = hex_str[i:i+32]
                offset = i // 2
                formatted = " ".join(chunk[j:j+2] for j in range(0, len(chunk), 2))
                lines.append(f"  {offset:04x}: {formatted}")
        else:
            lines.append("  (binary format)")

        return "\n".join(lines)


# =============================================================================
# Service Factory
# =============================================================================

_service_instance: HaskellService | None = None


def get_haskell_service(verbose: bool = False) -> HaskellService:
    """Get or create Haskell service instance"""
    global _service_instance
    if _service_instance is None:
        _service_instance = HaskellService(verbose=verbose)
    return _service_instance
