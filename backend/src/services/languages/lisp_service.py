"""LISP Language Service -- runs Common Lisp source via SBCL 2.6.2.

SBCL is installed at /usr/bin/sbcl (Steel Bank Common Lisp 2.6.2).
Empirically verified:
  sbcl --script file.lisp  -- execute; exit 0 on success, exit 1 on error
  stderr contains backtrace on syntax/runtime errors.

Gate 3 contract: execute well-formed programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).
"""

from __future__ import annotations

import asyncio
import os
import tempfile
import time

from ..base_executor import ExecutionResult, ExecutionStatus  # re-exported for tests

_SBCL = "/usr/bin/sbcl"
_DEFAULT_TIMEOUT = 10.0


class LISPService:
    """Executes Common Lisp programs via SBCL 2.6.2."""

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Run LISP source via sbcl --script.

        Returns SUCCESS on exit 0, COMPILE_ERROR on non-zero exit.
        """
        start = time.monotonic()

        with tempfile.NamedTemporaryFile(
            mode="w", suffix=".lisp", delete=False, encoding="utf-8"
        ) as fh:
            fh.write(code)
            tmp_path = fh.name

        try:
            try:
                proc = await asyncio.create_subprocess_exec(
                    _SBCL,
                    "--script",
                    tmp_path,
                    stdin=asyncio.subprocess.DEVNULL,
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                )
                try:
                    stdout_bytes, stderr_bytes = await asyncio.wait_for(
                        proc.communicate(),
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

                status = ExecutionStatus.SUCCESS if rc == 0 else ExecutionStatus.COMPILE_ERROR
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
                    stderr="sbcl not found at /usr/bin/sbcl",
                    execution_time=elapsed,
                    exit_code=-1,
                )
        finally:
            os.unlink(tmp_path)
