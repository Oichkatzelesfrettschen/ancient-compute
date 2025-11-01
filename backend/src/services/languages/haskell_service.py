# Ancient Compute - Haskell Language Service (GHC + Stack)
"""
Haskell Language Service for ancient_compute.

Executes Haskell code in a sandboxed Docker container with:
- GHC compiler with warning flags
- Stack build system
- QuickCheck property testing support
- Memory and CPU limits via cgroups
"""

from __future__ import annotations

import asyncio
import re
import tempfile
import time
from typing import Any

from ..base_executor import (
    BaseExecutor,
    ContainerConfig,
    ExecutionResult,
    ExecutionStatus,
)


class HaskellService(BaseExecutor):
    """Haskell language executor using GHC with type-safe execution"""

    def __init__(self, timeout: int = 15) -> None:
        """Initialize Haskell service with GHC image"""
        super().__init__(
            language="haskell",
            docker_image="ancient-compute/haskell:latest",
            timeout=timeout,
        )
        self.compile_flags = [
            "-Wall",  # All warnings
            "-Werror",  # Warnings as errors
            "-O2",  # Optimize
            "-fforce-recomp",  # Force recompilation
        ]

    def _get_command(self, code_path: str) -> str:
        """Get command to compile and run Haskell code"""
        compile_cmd = " ".join([
            "ghc",
            *self.compile_flags,
            "/workspace/Main.hs",
            "-o /tmp/program",
            "2>&1",
        ])
        run_cmd = "/tmp/program < /workspace/input.txt 2>&1"
        return f"({compile_cmd} && {run_cmd}) || echo '[COMPILATION_FAILED]'"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute Haskell code with proper error classification"""
        # Security: Check for unsafe functions
        unsafe_patterns = [
            "System.IO.Unsafe",
            "unsafePerformIO",
            "unsafeInterleaveIO",
            "unsafeDupablePerformIO",
            "reallyUnsafePtrEquality",
        ]

        for pattern in unsafe_patterns:
            if pattern in code:
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Security violation: '{pattern}' is not allowed",
                    execution_time=0,
                )

        if not self.docker_available or not self.client:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr="Docker is not available for Haskell service",
                execution_time=0,
            )

        start_time = time.time()

        with tempfile.TemporaryDirectory() as tmpdir:
            code_file = f"{tmpdir}/Main.hs"
            input_file = f"{tmpdir}/input.txt"

            with open(code_file, "w", encoding="utf-8") as f:
                f.write(code)
            with open(input_file, "w", encoding="utf-8") as f:
                f.write(input_data)

            try:
                container = self.client.containers.run(
                    **self._get_container_config(tmpdir)
                )

                try:
                    exit_code = await self._wait_container(container)
                except asyncio.TimeoutError:
                    return ExecutionResult(
                        status=ExecutionStatus.TIMEOUT,
                        stdout="",
                        stderr=f"Timeout after {self.timeout}s",
                        execution_time=self.timeout,
                    )

                logs = container.logs(stdout=True, stderr=True)
                output = logs.decode("utf-8", errors="replace")
                execution_time = time.time() - start_time

                if "[COMPILATION_FAILED]" in output:
                    return ExecutionResult(
                        status=ExecutionStatus.COMPILE_ERROR,
                        stdout="",
                        stderr=self._extract_error(output),
                        execution_time=execution_time,
                    )

                if exit_code != 0:
                    return ExecutionResult(
                        status=ExecutionStatus.RUNTIME_ERROR,
                        stdout=output[:5000],
                        stderr=f"Exit code: {exit_code}",
                        execution_time=execution_time,
                        exit_code=exit_code,
                    )

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS,
                    stdout=output[:10000],
                    stderr="",
                    execution_time=execution_time,
                    exit_code=0,
                )

            except Exception as exc:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=f"Error: {exc}",
                    execution_time=time.time() - start_time,
                )

    def _extract_error(self, output: str) -> str:
        """Extract GHC error from compilation output"""
        lines = output.split("\n")

        # Look for error: lines first
        errors = [line for line in lines if re.search(r"error:", line, re.IGNORECASE)]
        if errors:
            return "\n".join(errors)

        # Look for type error patterns
        type_errors = [line for line in lines if re.search(
            r"(Couldn't match|No instance|Ambiguous|Type mismatch)", line
        )]
        if type_errors:
            return "\n".join(type_errors)

        # Look for syntax errors
        syntax_errors = [line for line in lines if re.search(
            r"(parse error|unexpected|expecting)", line, re.IGNORECASE
        )]
        if syntax_errors:
            return "\n".join(syntax_errors)

        # Return last few lines of output if no specific error found
        return "\n".join(lines[-5:])

    def _get_container_config(self, code_path: str) -> ContainerConfig:
        """Get container config with security settings"""
        config = super()._get_container_config(code_path)
        config["security_opt"] = ["no-new-privileges"]
        return config
