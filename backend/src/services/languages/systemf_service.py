"""
System F Language Service - FastAPI wrapper for System F compilation
"""

import asyncio
import time
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from typing import Optional

from backend.src.compilers.systemf_compiler import SystemFCompiler


@dataclass
class CompilationResult:
    """Result of compilation"""
    status: str
    code: Optional[str] = None
    error: Optional[str] = None
    warnings: list = None
    timing: dict = None

    def __post_init__(self) -> None:
        if self.warnings is None:
            self.warnings = []
        if self.timing is None:
            self.timing = {}


class SystemFService:
    """System F language service"""

    def __init__(self) -> None:
        self.executor = ThreadPoolExecutor(max_workers=4)
        self.compiler = SystemFCompiler(verbose=False)

    async def execute(self, source: str, timeout: float = 30.0) -> CompilationResult:
        """Compile System F source code"""
        loop = asyncio.get_event_loop()

        try:
            start_time = time.time()

            ir_program = await asyncio.wait_for(
                loop.run_in_executor(
                    self.executor,
                    lambda: self.compiler.compile(source)
                ),
                timeout=timeout
            )

            compilation_time = time.time() - start_time
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
                error=f"Compilation timeout after {timeout} seconds"
            )
        except Exception as e:
            return CompilationResult(
                status="error",
                error=f"Compilation error: {e}"
            )

    async def validate(self, source: str, timeout: float = 10.0) -> CompilationResult:
        """Validate System F source code"""
        loop = asyncio.get_event_loop()

        try:
            start_time = time.time()

            await asyncio.wait_for(
                loop.run_in_executor(
                    self.executor,
                    lambda: self._validate_source(source)
                ),
                timeout=timeout
            )

            return CompilationResult(
                status="success",
                code="(validation passed)",
                timing={
                    "validation_ms": (time.time() - start_time) * 1000
                }
            )

        except Exception as e:
            return CompilationResult(
                status="error",
                error=f"Validation error: {e}"
            )

    async def get_capabilities(self) -> dict:
        """Get service capabilities"""
        return {
            "language": "systemf",
            "version": "1.0.0",
            "features": [
                "polymorphism",
                "type_abstraction",
                "type_inference",
                "universal_types"
            ],
            "ir_target": "Babbage",
            "status": "operational"
        }

    def _validate_source(self, source: str) -> None:
        """Validate source syntax"""
        from backend.src.compilers.systemf_lexer import SystemFLexer
        from backend.src.compilers.systemf_parser import SystemFParser

        lexer = SystemFLexer(source)
        tokens = lexer.tokenize()
        parser = SystemFParser(tokens)
        parser.parse()

    def _generate_ir_code(self, ir_program) -> str:
        """Generate IR code string"""
        lines = ["; System F to Babbage IR"]

        for func in ir_program.functions:
            lines.append(f"@function {func.name}()")
            if func.blocks:
                for block in func.blocks:
                    lines.append(f"  @block {block.name}:")
            else:
                lines.append("  ; (empty function)")

        return "\n".join(lines)
