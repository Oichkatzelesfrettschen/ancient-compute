"""Java Language Service -- compiles and runs Java source via OpenJDK 17.

javac and java are installed at /usr/lib/jvm/java-17-openjdk/bin/.
Empirically verified (2026-03-21):
  javac -version: javac 17.0.18
  java  -version: openjdk version "17.0.18" 2026-01-20

Two-step pipeline:
  1. javac Main.java   -- compile; exit 0 on success, non-zero on error
  2. java -cp . Main   -- run;     exit 0 on success, non-zero on error

The source file must be named Main.java and declare a public class named Main.
We create a dedicated temporary directory per execution so that javac can write
.class files alongside the source without conflicting across concurrent requests.

Gate 3 contract: compile + execute well-formed programs (ExecutionStatus.SUCCESS)
and reject malformed ones (ExecutionStatus.COMPILE_ERROR).
"""

from __future__ import annotations

import asyncio
import os
import shutil
import tempfile
import time

from ..base_executor import ExecutionResult, ExecutionStatus  # re-exported for tests

_JAVAC = "/usr/lib/jvm/java-17-openjdk/bin/javac"
_JAVA = "/usr/lib/jvm/java-17-openjdk/bin/java"
_DEFAULT_TIMEOUT = 15.0


class JavaService:
    """Compiles and runs Java 17 programs via OpenJDK.

    execute()      -- compile + run (Gate 3 contract)
    execute_check() -- compile only (faster validation)
    """

    def __init__(self, timeout: float = _DEFAULT_TIMEOUT) -> None:
        self.language = "java"
        self.timeout = timeout

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Compile and run Java source (Gate 3 contract).

        Writes source to a temp directory as Main.java, compiles with javac,
        then runs with java.  Returns SUCCESS only if both steps succeed.
        """
        return await self._run(code, input_data, run=True)

    async def execute_check(self, code: str) -> ExecutionResult:
        """Compile-only check (syntax validation without running)."""
        return await self._run(code, "", run=False)

    async def _run(
        self,
        code: str,
        input_data: str,
        run: bool,
    ) -> ExecutionResult:
        start = time.monotonic()
        tmpdir = tempfile.mkdtemp(prefix="java_")
        src_path = os.path.join(tmpdir, "Main.java")

        try:
            with open(src_path, "w", encoding="utf-8") as fh:
                fh.write(code)

            # Step 1: compile
            compile_result = await self._exec(
                [_JAVAC, "Main.java"],
                cwd=tmpdir,
                stdin_bytes=None,
                start=start,
                error_status=ExecutionStatus.COMPILE_ERROR,
            )
            if compile_result.status != ExecutionStatus.SUCCESS:
                return compile_result

            if not run:
                return compile_result

            # Step 2: run
            run_result = await self._exec(
                [
                    _JAVA,
                    "-Xmx128m",  # cap heap
                    "-Xms32m",
                    "-cp",
                    ".",
                    "Main",
                ],
                cwd=tmpdir,
                stdin_bytes=input_data.encode() if input_data else None,
                start=start,
                error_status=ExecutionStatus.RUNTIME_ERROR,
            )
            return ExecutionResult(
                status=run_result.status,
                stdout=run_result.stdout,
                stderr=run_result.stderr,
                compile_output=compile_result.stdout + compile_result.stderr,
                execution_time=run_result.execution_time,
                exit_code=run_result.exit_code,
            )

        finally:
            shutil.rmtree(tmpdir, ignore_errors=True)

    async def _exec(
        self,
        args: list[str],
        cwd: str,
        stdin_bytes: bytes | None,
        start: float,
        error_status: ExecutionStatus,
    ) -> ExecutionResult:
        try:
            proc = await asyncio.create_subprocess_exec(
                *args,
                cwd=cwd,
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
            status = ExecutionStatus.SUCCESS if rc == 0 else error_status
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
                stderr=f"javac not found at {_JAVAC}",
                execution_time=time.monotonic() - start,
                exit_code=-1,
            )
