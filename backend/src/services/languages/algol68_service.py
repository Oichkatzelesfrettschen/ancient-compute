"""ALGOL68 Language Service -- runs source via a68g (Algol 68 Genie).

a68g 3.10.12 is installed at /usr/bin/a68g.
Empirically verified:
  a68g file.a68          -- full execution, exit 0 on success, exit 1 on error
  a68g --check file.a68  -- syntax check only, exit 0 on success, exit 1 on error

Gate 3 contract: syntax-check well-formed ALGOL68 programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).
Full execution is available via execute_full().
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


_A68G = "/usr/bin/a68g"
_DEFAULT_TIMEOUT = 10.0


class ALGOL68Service:
    """Executes ALGOL68 programs via a68g 3.10.12.

    Gate 3 uses --check (syntax only).  execute_full() runs the program.
    """

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Syntax-check ALGOL68 source (Gate 3 contract).

        Returns SUCCESS if a68g --check exits 0, COMPILE_ERROR otherwise.
        """
        return await self._run(code, check_only=True)

    async def execute_full(self, code: str, input_data: str = "") -> ExecutionResult:
        """Run ALGOL68 source to completion."""
        return await self._run(code, check_only=False, stdin_data=input_data)

    async def _run(
        self,
        code: str,
        check_only: bool,
        stdin_data: str = "",
    ) -> ExecutionResult:
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".a68", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp_path = fh.name

        try:
            args = [_A68G]
            if check_only:
                args.append("--check")
            args.append(tmp_path)

            try:
                proc = await asyncio.create_subprocess_exec(
                    *args,
                    stdin=asyncio.subprocess.PIPE if stdin_data else asyncio.subprocess.DEVNULL,
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
                    # a68g writes "syntax error" to stderr for parse failures;
                    # runtime errors also produce non-zero exit.
                    status = ExecutionStatus.COMPILE_ERROR

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
        finally:
            os.unlink(tmp_path)
