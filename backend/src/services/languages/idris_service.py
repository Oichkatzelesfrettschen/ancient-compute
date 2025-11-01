"""
IDRIS2 Language Service - Async FastAPI wrapper for IDRIS2 compilation and execution

Provides:
  - Compilation of IDRIS2 source to Babbage IR
  - Type checking and dependent type validation
  - Code generation with proof type erasure
  - Error reporting with location information
"""

import asyncio
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from typing import Optional

from backend.src.compilers.idris_compiler import IDRIS2Compiler


@dataclass
class CompilationResult:
    """Result of compilation"""
    status: str  # "success" or "error"
    code: Optional[str] = None
    error: Optional[str] = None
    warnings: list = None
    timing: dict = None

    def __post_init__(self) -> None:
        if self.warnings is None:
            self.warnings = []
        if self.timing is None:
            self.timing = {}


class IDRISService:
    """IDRIS2 language service"""

    def __init__(self) -> None:
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.compiler = IDRIS2Compiler(verbose=False)

    async def execute(self, source: str, timeout: float = 30.0) -> CompilationResult:
        """
        Compile IDRIS2 source code

        Args:
            source: IDRIS2 source code
            timeout: Execution timeout in seconds

        Returns:
            CompilationResult with IR code or error
        """
        loop = asyncio.get_event_loop()

        try:
            start_time = time.time()

            # Run compilation in thread pool
            ir_program = await asyncio.wait_for(
                loop.run_in_executor(
                    self.executor,
                    lambda: self.compiler.compile(source)
                ),
                timeout=timeout
            )

            compilation_time = time.time() - start_time

            # Generate IR code
            ir_code = self._generate_ir_code(ir_program)

            return CompilationResult(
                status="success",
                code=ir_code,
                timing={
                    "compilation_ms": compilation_time * 1000,
                    "total_ms": (time.time() - start_time) * 1000
                }
            )

        except asyncio.TimeoutError:
            return CompilationResult(
                status="error",
                error=f"Compilation timeout after {timeout} seconds",
                timing={"timeout_ms": timeout * 1000}
            )
        except SyntaxError as e:
            return CompilationResult(
                status="error",
                error=f"Syntax error: {e}",
                warnings=[]
            )
        except Exception as e:
            return CompilationResult(
                status="error",
                error=f"Compilation error: {e}",
                warnings=[]
            )

    async def validate(self, source: str, timeout: float = 10.0) -> CompilationResult:
        """
        Validate IDRIS2 source code without generating code

        Args:
            source: IDRIS2 source code
            timeout: Validation timeout in seconds

        Returns:
            CompilationResult with validation errors or success
        """
        loop = asyncio.get_event_loop()

        try:
            start_time = time.time()

            # Run validation in thread pool
            await asyncio.wait_for(
                loop.run_in_executor(
                    self.executor,
                    lambda: self._validate_source(source)
                ),
                timeout=timeout
            )

            validation_time = time.time() - start_time

            return CompilationResult(
                status="success",
                code="(validation passed)",
                timing={
                    "validation_ms": validation_time * 1000,
                    "total_ms": (time.time() - start_time) * 1000
                }
            )

        except asyncio.TimeoutError:
            return CompilationResult(
                status="error",
                error=f"Validation timeout after {timeout} seconds"
            )
        except SyntaxError as e:
            return CompilationResult(
                status="error",
                error=f"Syntax error: {e}"
            )
        except Exception as e:
            return CompilationResult(
                status="error",
                error=f"Validation error: {e}"
            )

    async def get_capabilities(self) -> dict:
        """
        Get service capabilities

        Returns:
            Dictionary with service metadata
        """
        return {
            "language": "idris2",
            "version": "0.6.0",
            "features": [
                "dependent_types",
                "type_families",
                "data_declarations",
                "pattern_matching",
                "type_checking",
                "implicit_parameters"
            ],
            "ir_target": "Babbage",
            "status": "operational"
        }

    def _validate_source(self, source: str) -> None:
        """Validate source without full compilation"""
        # Just parse to check syntax
        from backend.src.compilers.idris_lexer import IDRIS2Lexer
        from backend.src.compilers.idris_parser import IDRIS2Parser

        lexer = IDRIS2Lexer(source)
        tokens = lexer.tokenize()
        parser = IDRIS2Parser(tokens)
        parser.parse()

    def _generate_ir_code(self, ir_program) -> str:
        """Generate IR code string from IR program"""
        lines = []

        lines.append("; IDRIS2 to Babbage IR")
        lines.append(f"; Generated {len(ir_program.functions)} function(s)")
        lines.append("")

        # Generate each function
        for func in ir_program.functions:
            lines.append(f"@function {func.name}({', '.join(func.parameters)})")

            # Generate blocks
            if func.blocks:
                for block in func.blocks:
                    lines.append(f"  @block {block.name}:")
                    # Generate instructions
                    if hasattr(block, 'instructions') and block.instructions:
                        for instr in block.instructions:
                            lines.append(f"    {instr}")
            else:
                lines.append("  ; (empty function)")

            lines.append("")

        # Generate global variables
        if ir_program.global_variables:
            lines.append("; Global variables")
            for var in ir_program.global_variables:
                lines.append(f"; {var}")

        return "\n".join(lines)
