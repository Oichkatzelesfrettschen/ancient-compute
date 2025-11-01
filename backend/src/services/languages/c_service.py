# Ancient Compute - C Language Service (GCC + Sandbox)
"""
C Language Service for ancient_compute.

Executes C code in a sandboxed Docker container with:
- GCC compiler with all warning flags
- seccomp-bpf sandboxing
- Memory and CPU limits via cgroups
- AddressSanitizer (ASAN) for memory safety
- UndefinedBehaviorSanitizer (UBSAN) for undefined behavior detection
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


class CService(BaseExecutor):
    """C language executor using GCC with comprehensive safety checks"""

    def __init__(self, timeout: int = 10) -> None:
        """Initialize C service with GCC image"""
        super().__init__(
            language="c",
            docker_image="ancient-compute/c-service:latest",
            timeout=timeout,
        )
        self.compile_flags = [
            "-Wall",  # All warnings
            "-Wextra",  # Extra warnings
            "-Werror",  # Warnings as errors
            "-std=c99",  # C99 standard
            "-O2",  # Optimize
            "-fstack-protector-all",  # Stack protection
            "-D_FORTIFY_SOURCE=2",  # Fortify source
        ]
        self.sanitizers = [
            "-fsanitize=address",  # Address Sanitizer
            "-fsanitize=undefined",  # UB Sanitizer
        ]

    def _get_command(self, code_path: str) -> str:
        """Get command to compile and run C code"""
        compile_cmd = " ".join([
            "gcc",
            *self.compile_flags,
            *self.sanitizers,
            "/workspace/main.c",
            "-o /tmp/program",
            "2>&1",
        ])
        run_cmd = "/tmp/program < /workspace/input.txt 2>&1"
        return f"({compile_cmd} && {run_cmd}) || echo '[COMPILATION_FAILED]'"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute C code with proper error classification"""
        # Security: Check for forbidden patterns
        forbidden_patterns = [
            "#include <sys/",
            "system(",
            "exec",
            "fork(",
            "socket(",
        ]

        for pattern in forbidden_patterns:
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
                stderr="Docker is not available for C service",
                execution_time=0,
            )

        start_time = time.time()

        with tempfile.TemporaryDirectory() as tmpdir:
            code_file = f"{tmpdir}/main.c"
            input_file = f"{tmpdir}/input.txt"

            with open(code_file, "w", encoding="ascii", errors="replace") as f:
                f.write(code)
            with open(input_file, "w", encoding="ascii", errors="replace") as f:
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
                output = logs.decode("ascii", errors="replace")
                execution_time = time.time() - start_time

                if "[COMPILATION_FAILED]" in output:
                    return ExecutionResult(
                        status=ExecutionStatus.COMPILE_ERROR,
                        stdout="",
                        stderr=self._extract_error(output),
                        execution_time=execution_time,
                    )

                if "AddressSanitizer" in output or "UndefinedBehaviorSanitizer" in output:
                    return ExecutionResult(
                        status=ExecutionStatus.RUNTIME_ERROR,
                        stdout=output[:5000],
                        stderr="Memory safety violation detected",
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
        """Extract GCC error from compilation output"""
        lines = output.split("\n")
        errors = [line for line in lines if re.search(r"error:", line)]
        return "\n".join(errors) if errors else "\n".join(lines[-5:])

    def _get_container_config(self, code_path: str) -> ContainerConfig:
        """Get container config with security settings"""
        config = super()._get_container_config(code_path)
        config["environment"] = {
            "ASAN_OPTIONS": "detect_leaks=1:halt_on_error=0",
            "UBSAN_OPTIONS": "halt_on_error=0:print_stacktrace=1",
        }
        config["security_opt"] = ["no-new-privileges"]
        return config
