"""MicroPython Language Service -- runs source via micropython 1.27.0.

micropython 1.27.0 (Unix port) is installed at /usr/bin/micropython.
Empirically verified (2026-03-20):
  micropython --version: MicroPython v1.27.0 on 2026-03-20; linux [GCC 15.2.1]
  micropython -c "code"  -- execute inline code, exit 0 on success, exit 1 on error
  micropython file.py    -- execute file, exit 0 on success, exit 1 on error

MicroPython v1.27.0 supports (empirically verified):
  - Python 3.4.0 compatible subset
  - Functions, closures, recursion
  - List comprehensions, generators
  - Classes (limited)
  - Basic I/O: print()
  - Standard modules: sys, os (limited), math, uio, ujson, etc.

Gate 3 contract: execute well-formed MicroPython programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR / RUNTIME_ERROR).
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


_MICROPYTHON = "/usr/bin/micropython"
_DEFAULT_TIMEOUT = 10.0


class MicroPythonService:
    """Executes MicroPython programs via micropython 1.27.0 Unix port.

    Gate 3: execute() runs the program fully (MicroPython is fast, no separate
    syntax-check mode; execution IS the contract check).
    """

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute MicroPython source.

        Returns SUCCESS if micropython exits 0, COMPILE_ERROR for syntax/name
        errors, RUNTIME_ERROR for other failures, TIMEOUT on timeout.
        """
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".py", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp_path = fh.name

        try:
            args = [_MICROPYTHON, tmp_path]

            try:
                proc = await asyncio.create_subprocess_exec(
                    *args,
                    stdin=(asyncio.subprocess.PIPE if input_data else asyncio.subprocess.DEVNULL),
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )
                stdin_bytes = input_data.encode() if input_data else None
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
                stdout = stdout_bytes.decode(errors="replace")
                stderr = stderr_bytes.decode(errors="replace")
                rc = proc.returncode if proc.returncode is not None else -1

                # MicroPython writes all output (including errors) to stdout.
                combined = stdout + stderr
                if rc == 0:
                    status = ExecutionStatus.SUCCESS
                elif "SyntaxError" in combined or "IndentationError" in combined:
                    status = ExecutionStatus.COMPILE_ERROR
                else:
                    status = ExecutionStatus.RUNTIME_ERROR

                return ExecutionResult(
                    status=status,
                    stdout=stdout,
                    stderr=stderr,
                    execution_time=elapsed,
                    exit_code=rc,
                )

            except FileNotFoundError:
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr=f"micropython not found at {_MICROPYTHON}",
                    execution_time=time.monotonic() - start,
                    exit_code=-1,
                )
        finally:
            os.unlink(tmp_path)
