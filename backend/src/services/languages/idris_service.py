"""IDRIS2 Language Service -- type-checks Idris2 source via idris2 0.8.0.

idris2 is installed at /usr/bin/idris2 (Steel Bank Common Lisp + Chez bootstrap).
Empirically verified:
  idris2 --no-color --check Main.idr  -- type-check only, exit 0 on success

The file must be named Main.idr and run from its containing directory so
idris2 can resolve the module name from the filename.  We create a dedicated
temporary directory per execution and pass cwd= to avoid path conflicts.

Gate 3 contract: type-check well-formed programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).
"""

from __future__ import annotations

import asyncio
import os
import shutil
import tempfile
import time

from ..base_executor import ExecutionResult, ExecutionStatus  # re-exported for tests

_IDRIS2 = "/usr/bin/idris2"
_DEFAULT_TIMEOUT = 30.0  # idris2 type-checking can be slow on first run


class IDRISService:
    """Type-checks Idris2 programs via idris2 0.8.0."""

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.language = "idris"
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Type-check Idris2 source via idris2 --check.

        Returns SUCCESS on exit 0, COMPILE_ERROR on non-zero exit.
        The source is written to a temporary directory as Main.idr so
        idris2 can resolve module Main from the filename.
        """
        start = time.monotonic()

        tmpdir = tempfile.mkdtemp(prefix="idris2_")
        tmp_path = os.path.join(tmpdir, "Main.idr")

        try:
            with open(tmp_path, "w", encoding="utf-8") as fh:
                fh.write(code)

            try:
                proc = await asyncio.create_subprocess_exec(
                    _IDRIS2,
                    "--no-color",
                    "--check",
                    "Main.idr",
                    cwd=tmpdir,
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
                    stderr="idris2 not found at /usr/bin/idris2",
                    execution_time=elapsed,
                    exit_code=-1,
                )
        finally:
            shutil.rmtree(tmpdir, ignore_errors=True)
