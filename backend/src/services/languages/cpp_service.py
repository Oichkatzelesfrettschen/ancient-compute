"""C++ Language Service -- compiles via g++ 15.2.1 (/usr/bin/g++).

Empirically verified (2026-03-20):
  g++ --version: g++ (GCC) 15.2.1 20260209
  g++ -std=c++20 -fsyntax-only file.cpp  -- exit 0 on success, exit 1 on error
  g++ -std=c++20 -fno-exceptions -o out file.cpp  -- full compilation

Gate 3 contract: syntax-check well-formed C++20 programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).

Standard flags:
  -std=c++20       -- C++20 feature set (GCC 15 supports most of C++23 too)
  -Wall -Wextra    -- treat comprehensive warnings as errors in sandbox context
  -fno-exceptions  -- optional; security sandbox typically disables exceptions
  -fsyntax-only    -- check syntax without producing binary output (Gate 3)
"""

from __future__ import annotations

import asyncio
import os
import tempfile
import time
from dataclasses import dataclass
from enum import Enum


class ExecutionStatus(Enum):
    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"


@dataclass
class ExecutionResult:
    status: ExecutionStatus
    stdout: str
    stderr: str
    execution_time: float = 0.0
    exit_code: int = 0


_GPP = "/usr/bin/g++"
_DEFAULT_TIMEOUT = 30.0
_CPP_STD = "c++20"


class CppService:
    """Compiles C++20 programs via g++ 15.2.1.

    Gate 3: syntax-only check via execute().
    Full compilation + run via execute_full().
    """

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Syntax-check C++ source (Gate 3 contract).

        Returns SUCCESS if g++ -fsyntax-only exits 0, COMPILE_ERROR otherwise.
        """
        return await self._run_check(code)

    async def execute_full(self, code: str, input_data: str = "") -> ExecutionResult:
        """Compile and run C++ source."""
        return await self._run_binary(code, input_data)

    async def _run_check(self, code: str) -> ExecutionResult:
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".cpp", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp_path = fh.name

        try:
            args = [
                _GPP,
                f"-std={_CPP_STD}",
                "-Wall",
                "-Wextra",
                "-fsyntax-only",
                tmp_path,
            ]
            return await self._exec(args, None, start)
        finally:
            os.unlink(tmp_path)

    async def _run_binary(self, code: str, input_data: str) -> ExecutionResult:
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".cpp", delete=False, encoding="utf-8"
        ) as src_fh:
            src_fh.write(code)
            src_path = src_fh.name

        out_path = src_path.replace(".cpp", ".out")

        try:
            compile_args = [
                _GPP,
                f"-std={_CPP_STD}",
                "-Wall",
                "-Wextra",
                "-fno-exceptions",
                "-o",
                out_path,
                src_path,
            ]
            compile_result = await self._exec(compile_args, None, start)
            if compile_result.status != ExecutionStatus.SUCCESS:
                return compile_result

            run_result = await self._exec(
                [out_path],
                input_data.encode() if input_data else None,
                start,
            )
            return run_result
        finally:
            os.unlink(src_path)
            if os.path.exists(out_path):
                os.unlink(out_path)

    async def _exec(
        self,
        args: list[str],
        stdin_bytes: bytes | None,
        start: float,
    ) -> ExecutionResult:
        try:
            proc = await asyncio.create_subprocess_exec(
                *args,
                stdin=asyncio.subprocess.PIPE if stdin_bytes else asyncio.subprocess.DEVNULL,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            try:
                stdout_bytes, stderr_bytes = await asyncio.wait_for(
                    proc.communicate(stdin_bytes),
                    timeout=self.timeout,
                )
            except TimeoutError:
                proc.kill()
                await proc.wait()
                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout="",
                    stderr="Execution timed out",
                    execution_time=time.monotonic() - start,
                    exit_code=-1,
                )

            elapsed = time.monotonic() - start
            rc = proc.returncode if proc.returncode is not None else -1
            status = ExecutionStatus.SUCCESS if rc == 0 else ExecutionStatus.COMPILE_ERROR
            return ExecutionResult(
                status=status,
                stdout=stdout_bytes.decode(errors="replace"),
                stderr=stderr_bytes.decode(errors="replace"),
                execution_time=elapsed,
                exit_code=rc,
            )

        except FileNotFoundError:
            return ExecutionResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stdout="",
                stderr=f"g++ not found at {_GPP}",
                execution_time=time.monotonic() - start,
                exit_code=-1,
            )
