# Ancient Compute - Base Executor for Language Services
from __future__ import annotations

import asyncio
import os
import tempfile
import time
from dataclasses import dataclass
from enum import Enum
from typing import Any, TypedDict

try:
    import docker  # type: ignore[import-not-found]
except ImportError:  # pragma: no cover - Docker is optional in local environments
    docker = None  # type: ignore[assignment]  # noqa: N816


class ExecutionStatus(Enum):
    """Execution status codes"""

    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"
    MEMORY_EXCEEDED = "memory_exceeded"
    SECURITY_VIOLATION = "security_violation"


@dataclass
class ExecutionResult:
    """Result of code execution"""

    status: ExecutionStatus
    stdout: str
    stderr: str
    compile_output: str | None = None
    execution_time: float = 0.0
    memory_used: int = 0
    exit_code: int = 0


class ContainerConfig(TypedDict, total=False):
    image: str
    command: str
    volumes: dict[str, dict[str, str]]
    working_dir: str
    mem_limit: str
    memswap_limit: str
    cpu_quota: int
    cpu_period: int
    network_mode: str
    read_only: bool
    tmpfs: dict[str, str]
    security_opt: list[str]
    detach: bool
    remove: bool


class BaseExecutor:
    """Base class for language executors with Docker containerization"""

    def __init__(self, language: str, docker_image: str, timeout: int = 10) -> None:
        self.language = language
        self.docker_image = docker_image
        self.timeout = timeout
        self.client: Any | None = None
        self.docker_available = self._check_docker()
        if self.docker_available:
            self._ensure_image()

    def _check_docker(self) -> bool:
        """Check if Docker is available"""
        if docker is None:
            print(f"Warning: Docker module not installed for {self.language}")
            return False
        try:
            self.client = docker.from_env()
            self.client.ping()
            return True
        except Exception as exc:  # pragma: no cover - exercised in environments without Docker
            print(f"Warning: Docker not available for {self.language}: {exc}")
            self.client = None
            return False

    def _ensure_image(self) -> None:
        """Ensure Docker image exists, build if necessary"""
        if not self.client or docker is None:
            return
        try:
            self.client.images.get(self.docker_image)
        except docker.errors.ImageNotFound:  # type: ignore[attr-defined]
            print(f"Building {self.docker_image}...")
            self._build_image()

    def _build_image(self) -> None:
        """Build Docker image from Dockerfile"""
        if not self.client or docker is None:
            return
        dockerfile_path = os.path.join(
            "backend", "src", "services", "containers", self.language.lower()
        )
        if os.path.exists(dockerfile_path):
            self.client.images.build(path=dockerfile_path, tag=self.docker_image, rm=True)

    def _get_container_config(self, code_path: str) -> ContainerConfig:
        """Get Docker container configuration with security settings"""
        return {
            "image": self.docker_image,
            "command": self._get_command(code_path),
            "volumes": {
                code_path: {
                    "bind": "/workspace",
                    "mode": "ro",
                }
            },
            "working_dir": "/workspace",
            "mem_limit": "128m",
            "memswap_limit": "128m",
            "cpu_quota": 50_000,  # 50% of one CPU
            "cpu_period": 100_000,
            "network_mode": "none",
            "read_only": True,
            "tmpfs": {
                "/tmp": "size=32M,mode=1777",
            },
            "security_opt": [
                "no-new-privileges",
            ],
            "detach": True,
            "remove": True,
        }

    def _get_command(self, code_path: str) -> str:  # pragma: no cover - to be implemented
        """Get execution command - must be implemented by subclass"""
        raise NotImplementedError("Subclass must implement _get_command")

    def _get_seccomp_profile(self) -> str:
        """Get seccomp profile path for language"""
        profile_path = f"/etc/docker/seccomp/{self.language.lower()}.json"
        if os.path.exists(profile_path):
            return profile_path
        return "default"

    def _use_gvisor(self) -> bool:
        """Enable gVisor for untrusted languages"""
        return self.language.lower() in ["c", "assembly", "systemf"]

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        """Execute code in sandboxed container"""
        if not self.docker_available or not self.client or docker is None:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=(
                    f"Docker is not available for {self.language}. "
                    "Please ensure Docker Desktop is running."
                ),
                execution_time=0,
            )

        start_time = time.time()

        with tempfile.TemporaryDirectory() as tmpdir:
            code_file = os.path.join(tmpdir, self._get_source_filename())
            input_file = os.path.join(tmpdir, "input.txt")

            # Write code and input (ASCII only)
            with open(code_file, "w", encoding="ascii", errors="replace") as file_handle:
                file_handle.write(code)
            with open(input_file, "w", encoding="ascii", errors="replace") as file_handle:
                file_handle.write(input_data)

            try:
                container = self.client.containers.run(**self._get_container_config(tmpdir))
            except docker.errors.ImageNotFound:  # type: ignore[attr-defined]
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=(
                        f"Docker image '{self.docker_image}' not found. "
                        "Build it with: docker build -t "
                        f"{self.docker_image} backend/src/services/containers/{self.language}"
                    ),
                    execution_time=time.time() - start_time,
                )
            except docker.errors.APIError as exc:  # type: ignore[attr-defined]
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=f"Docker API error: {exc}. Is Docker Desktop running?",
                    execution_time=time.time() - start_time,
                )
            except Exception as exc:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=f"Execution error: {exc}",
                    execution_time=time.time() - start_time,
                )

            try:
                # Wait for container with timeout
                exit_code = await self._wait_container(container)

                # Get logs
                logs = container.logs(stdout=True, stderr=True)
                stdout = logs.decode("ascii", errors="replace")

                execution_time = time.time() - start_time

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS if exit_code == 0 else ExecutionStatus.RUNTIME_ERROR,
                    stdout=stdout[:10_000],  # Limit output to 10KB
                    stderr="",
                    execution_time=execution_time,
                    exit_code=exit_code,
                )

            except TimeoutError:
                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout="",
                    stderr=f"Execution timeout ({self.timeout}s exceeded)",
                    execution_time=self.timeout,
                )
            except Exception as exc:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=str(exc),
                    execution_time=time.time() - start_time,
                )

    async def _wait_container(self, container: Any) -> int:
        """Wait for container to complete with timeout"""
        loop = asyncio.get_event_loop()
        result = await asyncio.wait_for(
            loop.run_in_executor(None, container.wait),
            timeout=self.timeout,
        )
        return result.get("StatusCode", -1)

    def _get_source_filename(self) -> str:
        """Get source filename with appropriate extension"""
        extensions = {
            "c": "main.c",
            "python": "main.py",
            "haskell": "Main.hs",
            "idris": "Main.idr",
            "lisp": "main.lisp",
            "assembly": "main.asm",
            "java": "Main.java",
            "systemf": "main.sf",
        }
        return extensions.get(self.language.lower(), "main.txt")
