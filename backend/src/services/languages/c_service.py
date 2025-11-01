"""C Language Service - compiles C code to Babbage IR and machine code.

This service integrates the C compiler with the FastAPI framework,
providing REST endpoints for C code compilation and execution.

Features:
  - Full C code parsing and semantic analysis
  - Type checking and type system validation
  - Compilation to Babbage IR
  - Code generation and assembly
  - Fast execution (no Docker containerization needed)
  - Comprehensive error reporting
"""

from __future__ import annotations
from typing import Dict, List, Optional, Any
from dataclasses import dataclass
from enum import Enum
import asyncio
from concurrent.futures import ThreadPoolExecutor
import sys

from backend.src.compilers.c_compiler import CCompiler
from backend.src.codegen.codegen import CodeGenerator
from backend.src.assembler.assembler import Assembler


class ExecutionStatus(str, Enum):
    """Execution status enumeration."""
    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"


@dataclass
class CompilationResult:
    """Result of C code compilation."""
    status: ExecutionStatus
    stdout: str = ""  # Assembly text or hex dump
    stderr: str = ""  # Error messages
    ir_text: str = ""  # Intermediate representation (debug)
    assembly_text: str = ""  # Assembly code
    machine_code: str = ""  # Hex-encoded machine code
    compilation_time: float = 0.0  # Seconds


class CService:
    """C Language Service for compiling and executing C code targeting Babbage ISA."""

    def __init__(self, timeout_seconds: int = 30, verbose: bool = False) -> None:
        """Initialize C service.

        Args:
            timeout_seconds: Maximum execution time
            verbose: Enable verbose compilation output
        """
        self.timeout_seconds = timeout_seconds
        self.verbose = verbose
        self.c_compiler = CCompiler(verbose=verbose)
        self.code_generator = CodeGenerator()
        self.executor = ThreadPoolExecutor(max_workers=4)

    async def execute(self, code: str, input_data: str = "") -> CompilationResult:
        """Execute C code by compiling to IR, generating assembly, and assembling.

        Args:
            code: C source code
            input_data: Input data for the program (not used in pure compilation)

        Returns:
            CompilationResult with status, output, errors, and timing
        """
        loop = asyncio.get_event_loop()

        try:
            # Run compilation in thread pool to avoid blocking
            result = await asyncio.wait_for(
                loop.run_in_executor(self.executor, self._compile_and_assemble, code),
                timeout=self.timeout_seconds
            )
            return result

        except asyncio.TimeoutError:
            return CompilationResult(
                status=ExecutionStatus.TIMEOUT,
                stderr=f"Compilation timed out after {self.timeout_seconds} seconds"
            )
        except Exception as e:
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stderr=str(e)
            )

    def _compile_and_assemble(self, code: str) -> CompilationResult:
        """Internal method: compile C code to machine code.

        Args:
            code: C source code

        Returns:
            CompilationResult with compiled output
        """
        import time
        start_time = time.time()

        try:
            # Phase 1: C → IR compilation
            if self.verbose:
                print("[C SERVICE] Phase 1: C → IR compilation...")

            ir_program = self.c_compiler.compile(code)
            ir_text = self._ir_to_string(ir_program)

            if self.verbose:
                print(f"[C SERVICE]   IR functions: {len(ir_program.functions)}")

            # Phase 2: IR → Assembly code generation
            if self.verbose:
                print("[C SERVICE] Phase 2: IR → Assembly code generation...")

            assembly_outputs = []
            for func_name, ir_func in ir_program.functions.items():
                codegen_result = self.code_generator.generate_function(ir_func)
                assembly_text = codegen_result.get_assembly_text()
                assembly_outputs.append(assembly_text)

            complete_assembly = "\n".join(assembly_outputs)

            if self.verbose:
                print("[C SERVICE]   Assembly generated")

            # Phase 3: Assembly → Machine code
            if self.verbose:
                print("[C SERVICE] Phase 3: Assembly → Machine code...")

            assembler = Assembler(complete_assembly)
            assembler_result = assembler.assemble()

            if assembler_result.error_count > 0:
                error_text = "\n".join(assembler_result.errors)
                return CompilationResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stderr=error_text,
                    assembly_text=complete_assembly,
                    compilation_time=time.time() - start_time
                )

            # Format machine code as hex dump
            machine_code_hex = self._format_hex_dump(assembler_result.machine_code)

            if self.verbose:
                print("[C SERVICE] Compilation COMPLETE")

            return CompilationResult(
                status=ExecutionStatus.SUCCESS,
                stdout=machine_code_hex,
                ir_text=ir_text,
                assembly_text=complete_assembly,
                machine_code=machine_code_hex,
                compilation_time=time.time() - start_time
            )

        except Exception as e:
            import traceback
            error_msg = f"{str(e)}\n\n{traceback.format_exc()}"
            return CompilationResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stderr=error_msg,
                compilation_time=time.time() - start_time
            )

    def _ir_to_string(self, ir_program: Any) -> str:
        """Convert IR program to readable string format.

        Args:
            ir_program: IR Program object

        Returns:
            Formatted IR representation
        """
        lines = []
        lines.append("=== BABBAGE IR PROGRAM ===\n")

        # Global variables
        if ir_program.global_variables:
            lines.append("GLOBAL VARIABLES:")
            for name, var in ir_program.global_variables.items():
                lines.append(f"  {name}: {var.ir_type}")
            lines.append("")

        # Functions
        if ir_program.functions:
            lines.append("FUNCTIONS:")
            for func_name, func in ir_program.functions.items():
                lines.append(f"  {func_name}({', '.join(func.parameters)}):")
                for block in func.basic_blocks:
                    lines.append(f"    {block.label}:")
                    for instr in block.instructions:
                        lines.append(f"      {instr}")
                    if block.terminator:
                        lines.append(f"      {block.terminator}")
                lines.append("")

        return "\n".join(lines)

    def _format_hex_dump(self, machine_code: List[int]) -> str:
        """Format machine code as hex dump.

        Args:
            machine_code: List of 50-bit instruction words

        Returns:
            Formatted hex dump string
        """
        lines = ["=== MACHINE CODE (HEX) ===", ""]
        lines.append("Address  | Instruction (Hex)")
        lines.append("-" * 40)

        for addr, word in enumerate(machine_code):
            # Format as 12 hex digits (48 bits visible, fits in typical display)
            hex_str = f"{word:012x}"
            lines.append(f"{addr:08x}  | {hex_str}")

        return "\n".join(lines)

    async def validate(self, code: str) -> Dict[str, Any]:
        """Validate C code without full compilation.

        Args:
            code: C source code

        Returns:
            Dictionary with validation results
        """
        loop = asyncio.get_event_loop()

        try:
            result = await asyncio.wait_for(
                loop.run_in_executor(self.executor, self._validate_code, code),
                timeout=self.timeout_seconds
            )
            return result

        except asyncio.TimeoutError:
            return {
                "valid": False,
                "error": f"Validation timed out after {self.timeout_seconds} seconds"
            }
        except Exception as e:
            return {
                "valid": False,
                "error": str(e)
            }

    def _validate_code(self, code: str) -> Dict[str, Any]:
        """Internal method: validate C code.

        Args:
            code: C source code

        Returns:
            Dictionary with validation results
        """
        try:
            # Try to compile - if it succeeds, code is valid
            ir_program = self.c_compiler.compile(code)
            return {
                "valid": True,
                "functions": list(ir_program.functions.keys()),
                "error": None
            }
        except Exception as e:
            return {
                "valid": False,
                "error": str(e)
            }

    async def get_capabilities(self) -> Dict[str, Any]:
        """Get service capabilities and metadata.

        Returns:
            Dictionary with service information
        """
        return {
            "language": "C",
            "target_isa": "Babbage",
            "features": [
                "Basic data types (int, float, void)",
                "Functions with parameters",
                "Local and global variables",
                "Arithmetic and logical operations",
                "Control flow (if/else, while, for)",
                "Function calls",
                "Array access (basic)",
            ],
            "limitations": [
                "No structures or unions",
                "No pointers (except function pointers)",
                "No macros or preprocessing",
                "Limited standard library",
                "No dynamic memory allocation",
            ],
            "execution_model": "Compile to IR → Assembly → Machine Code",
            "timeout_seconds": self.timeout_seconds,
            "response_format": "JSON with compilation output and errors"
        }
