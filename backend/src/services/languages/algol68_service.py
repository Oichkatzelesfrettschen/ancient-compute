"""ALGOL68 Language Service -- runs source via a68g (Algol 68 Genie).

a68g 3.10.12 is installed at /usr/bin/a68g.
Empirically verified:
  a68g file.a68          -- full execution, exit 0 on success, exit 1 on error
  a68g --check file.a68  -- syntax check only, exit 0 on success, exit 1 on error

Gate 3 contract: syntax-check well-formed ALGOL68 programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).

Extended API:
  execute(code)                    -- syntax check only (Gate 3 contract)
  execute_full(code, input_data)   -- full execution with optional stdin
  execute_with_input(code, input)  -- full execution, alias for execute_full
  execute_file(path, input_data)   -- run a .a68 file directly (no temp copy)
  execute_file_check(path)         -- syntax-check an existing .a68 file

Error classification:
  COMPILE_ERROR -- a68g reports a syntax/type/parse error (stderr contains
                   "error" at column 0 or "syntax error" or exit 1 from --check)
  RUNTIME_ERROR -- program passed --check but failed during full execution
                   (stderr contains "runtime error", "stack overflow",
                   "division by zero", or other a68g runtime diagnostics)
  TIMEOUT       -- process did not complete within timeout seconds
"""

from __future__ import annotations

import asyncio
import os
import tempfile
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path


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


_A68G = "/usr/bin/a68g"
_DEFAULT_TIMEOUT = 10.0

# a68g runtime diagnostics that distinguish runtime failures from compile errors.
# These patterns appear in stderr when a syntactically valid program crashes.
_RUNTIME_PATTERNS = (
    "runtime error",
    "stack overflow",
    "division by zero",
    "nil pointer",
    "index out of range",
    "value error",
    "heap overflow",
    "open error",
    "transput error",
)


def _classify_error(stderr: str) -> ExecutionStatus:
    """Distinguish compile errors from runtime errors based on a68g stderr text.

    WHY: a68g uses exit code 1 for both syntax errors and runtime aborts.
    Checking stderr text is the only reliable way to separate the two categories.
    """
    lower = stderr.lower()
    for pattern in _RUNTIME_PATTERNS:
        if pattern in lower:
            return ExecutionStatus.RUNTIME_ERROR
    return ExecutionStatus.COMPILE_ERROR


class ALGOL68Service:
    """Executes ALGOL68 programs via a68g 3.10.12.

    Gate 3 uses --check (syntax only).  execute_full() runs the program.
    execute_file() and execute_file_check() operate on existing .a68 files.
    """

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Syntax-check ALGOL68 source (Gate 3 contract).

        Returns SUCCESS if a68g --check exits 0, COMPILE_ERROR otherwise.
        """
        return await self._run(code, check_only=True)

    async def execute_full(self, code: str, input_data: str = "") -> ExecutionResult:
        """Run ALGOL68 source to completion with optional stdin."""
        return await self._run(code, check_only=False, stdin_data=input_data)

    async def execute_with_input(self, code: str, input_data: str) -> ExecutionResult:
        """Run ALGOL68 source with stdin input.  Alias for execute_full."""
        return await self._run(code, check_only=False, stdin_data=input_data)

    async def execute_file(
        self, path: str | Path, input_data: str = ""
    ) -> ExecutionResult:
        """Run an existing .a68 file directly (no temp-file copy).

        WHY: abacus.a68 and other resident programs live in tools/algol68/.
        Running them directly avoids re-copying the source on every call and
        also preserves the file path in a68g error messages.
        """
        return await self._run_path(str(path), check_only=False, stdin_data=input_data)

    async def execute_file_check(self, path: str | Path) -> ExecutionResult:
        """Syntax-check an existing .a68 file (no temp-file copy)."""
        return await self._run_path(str(path), check_only=True)

    async def _run(
        self,
        code: str,
        check_only: bool,
        stdin_data: str = "",
    ) -> ExecutionResult:
        """Write code to a temp file then delegate to _run_path."""
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".a68", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp_path = fh.name

        try:
            return await self._run_path(
                tmp_path, check_only=check_only, stdin_data=stdin_data, start=start
            )
        finally:
            os.unlink(tmp_path)

    async def _run_path(
        self,
        path: str,
        check_only: bool,
        stdin_data: str = "",
        start: float | None = None,
    ) -> ExecutionResult:
        """Core execution: build args, spawn a68g, collect output."""
        if start is None:
            start = time.monotonic()

        args = [_A68G]
        if check_only:
            args.append("--check")
        args.append(path)

        try:
            proc = await asyncio.create_subprocess_exec(
                *args,
                stdin=(
                    asyncio.subprocess.PIPE
                    if stdin_data
                    else asyncio.subprocess.DEVNULL
                ),
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
            )
            stdin_bytes = stdin_data.encode() if stdin_data else None
            try:
                stdout_bytes, stderr_bytes = await asyncio.wait_for(
                    proc.communicate(stdin_bytes),
                    timeout=self.timeout,
                )
            except TimeoutError:
                proc.kill()
                await proc.wait()
                elapsed = time.monotonic() - start
                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout="",
                    stderr="Execution timed out",
                    execution_time=elapsed,
                    exit_code=-1,
                )

            elapsed = time.monotonic() - start
            stdout = stdout_bytes.decode(errors="replace")
            stderr = stderr_bytes.decode(errors="replace")
            rc = proc.returncode if proc.returncode is not None else -1

            if rc == 0:
                status = ExecutionStatus.SUCCESS
            else:
                status = (
                    ExecutionStatus.COMPILE_ERROR
                    if check_only
                    else _classify_error(stderr)
                )

            return ExecutionResult(
                status=status,
                stdout=stdout,
                stderr=stderr,
                execution_time=elapsed,
                exit_code=rc,
            )

        except FileNotFoundError:
            elapsed = time.monotonic() - start
            return ExecutionResult(
                status=ExecutionStatus.COMPILE_ERROR,
                stdout="",
                stderr="a68g not found at /usr/bin/a68g",
                execution_time=elapsed,
                exit_code=-1,
            )
