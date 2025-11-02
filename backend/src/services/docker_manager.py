"""
Ancient Compute - Docker Manager with Graceful Fallback
Provides Docker execution with fallback to local restricted execution
"""

import asyncio
import subprocess
import tempfile
import os
import platform
from typing import Optional, Dict, Any
from dataclasses import dataclass
from enum import Enum
from pathlib import Path


class ExecutionBackend(Enum):
    """Available execution backends"""

    DOCKER = "docker"
    RESTRICTED_PYTHON = "restricted_python"
    SUBPROCESS = "subprocess"
    UNAVAILABLE = "unavailable"


@dataclass
class BackendInfo:
    """Information about an execution backend"""

    backend_type: ExecutionBackend
    available: bool
    reason: str = ""
    capabilities: list = None

    def __post_init__(self):
        if self.capabilities is None:
            self.capabilities = []


class DockerManager:
    """
    Manages Docker containers with graceful fallback to other execution methods.
    Singleton pattern ensures single Docker client instance.
    """

    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if self._initialized:
            return

        self.docker_client = None
        self.docker_available = False
        self.restricted_python_available = False
        self.backends = self._initialize_backends()
        self._initialized = True

    def _initialize_backends(self) -> Dict[ExecutionBackend, BackendInfo]:
        """Initialize and check availability of execution backends"""
        backends = {}

        # Check Docker availability
        docker_info = self._check_docker()
        backends[ExecutionBackend.DOCKER] = docker_info

        # Check RestrictedPython availability
        restricted_info = self._check_restricted_python()
        backends[ExecutionBackend.RESTRICTED_PYTHON] = restricted_info

        # Subprocess is always available but limited
        backends[ExecutionBackend.SUBPROCESS] = BackendInfo(
            backend_type=ExecutionBackend.SUBPROCESS,
            available=True,
            reason="Native subprocess execution (limited languages)",
            capabilities=["python"],  # Only Python via RestrictedPython
        )

        return backends

    def _check_docker(self) -> BackendInfo:
        """Check if Docker is available and working"""
        try:
            import docker

            self.docker_client = docker.from_env()

            # Test connection
            self.docker_client.ping()

            # Check if we can list images
            images = self.docker_client.images.list()

            self.docker_available = True
            return BackendInfo(
                backend_type=ExecutionBackend.DOCKER,
                available=True,
                reason=f"Docker connected (API version: {self.docker_client.version()['ApiVersion']})",
                capabilities=[
                    "c",
                    "python",
                    "haskell",
                    "java",
                    "assembly",
                    "lisp",
                    "idris",
                    "systemf",
                ],
            )

        except ImportError:
            return BackendInfo(
                backend_type=ExecutionBackend.DOCKER,
                available=False,
                reason="Docker Python package not installed. Run: pip install docker",
            )

        except Exception as e:
            error_msg = str(e)
            if "connection refused" in error_msg.lower():
                return BackendInfo(
                    backend_type=ExecutionBackend.DOCKER,
                    available=False,
                    reason="Docker daemon not running. Please start Docker Desktop.",
                )
            elif "permission denied" in error_msg.lower():
                return BackendInfo(
                    backend_type=ExecutionBackend.DOCKER,
                    available=False,
                    reason="Permission denied. Add user to docker group or run with sudo.",
                )
            else:
                return BackendInfo(
                    backend_type=ExecutionBackend.DOCKER,
                    available=False,
                    reason=f"Docker error: {error_msg}",
                )

    def _check_restricted_python(self) -> BackendInfo:
        """Check if RestrictedPython is available"""
        try:
            import RestrictedPython

            self.restricted_python_available = True
            return BackendInfo(
                backend_type=ExecutionBackend.RESTRICTED_PYTHON,
                available=True,
                reason="RestrictedPython available for sandboxed Python execution",
                capabilities=["python"],
            )
        except ImportError:
            return BackendInfo(
                backend_type=ExecutionBackend.RESTRICTED_PYTHON,
                available=False,
                reason="RestrictedPython not installed. Run: pip install RestrictedPython",
            )

    def get_backend_for_language(self, language: str) -> ExecutionBackend:
        """
        Get the best available backend for a language
        Priority: Docker > RestrictedPython (for Python) > Subprocess > Unavailable
        """
        language = language.lower()

        # First choice: Docker (supports all languages)
        if self.backends[ExecutionBackend.DOCKER].available:
            if language in self.backends[ExecutionBackend.DOCKER].capabilities:
                return ExecutionBackend.DOCKER

        # Second choice for Python: RestrictedPython
        if language == "python":
            if self.backends[ExecutionBackend.RESTRICTED_PYTHON].available:
                return ExecutionBackend.RESTRICTED_PYTHON

            # Fallback to subprocess for Python (using RestrictedPython)
            if self.backends[ExecutionBackend.SUBPROCESS].available:
                return ExecutionBackend.SUBPROCESS

        # No backend available for this language
        return ExecutionBackend.UNAVAILABLE

    def get_status_report(self) -> str:
        """Get a human-readable status report of all backends"""
        lines = ["Docker Manager Status Report", "=" * 40]

        for backend_type, info in self.backends.items():
            status = "AVAILABLE" if info.available else "NOT AVAILABLE"
            lines.append(f"\n{backend_type.value}:")
            lines.append(f"  Status: {status}")
            lines.append(f"  Reason: {info.reason}")
            if info.capabilities:
                lines.append(f"  Supports: {', '.join(info.capabilities)}")

        # Add recommendations
        lines.append("\nRecommendations:")
        if not self.backends[ExecutionBackend.DOCKER].available:
            lines.append("  - Install and start Docker Desktop for full language support")
        if not self.backends[ExecutionBackend.RESTRICTED_PYTHON].available:
            lines.append("  - Install RestrictedPython: pip install RestrictedPython")

        return "\n".join(lines)

    def ensure_docker_image(self, image_name: str, dockerfile_path: str = None) -> bool:
        """
        Ensure a Docker image exists, build if necessary
        Returns True if image is available, False otherwise
        """
        if not self.docker_available:
            return False

        try:
            # Check if image exists
            self.docker_client.images.get(image_name)
            return True
        except:
            # Try to build if dockerfile path provided
            if dockerfile_path and Path(dockerfile_path).exists():
                try:
                    print(f"Building Docker image {image_name}...")
                    self.docker_client.images.build(
                        path=str(Path(dockerfile_path).parent), tag=image_name, rm=True
                    )
                    return True
                except Exception as e:
                    print(f"Failed to build image {image_name}: {e}")
                    return False
            return False

    async def pull_or_build_image(self, image_name: str, language: str) -> bool:
        """
        Asynchronously pull or build a Docker image
        """
        if not self.docker_available:
            return False

        try:
            # First try to get existing image
            self.docker_client.images.get(image_name)
            return True
        except:
            pass

        # Try to pull from registry
        try:
            print(f"Pulling {image_name} from registry...")
            await asyncio.get_event_loop().run_in_executor(
                None, self.docker_client.images.pull, image_name
            )
            return True
        except:
            pass

        # Try to build locally
        dockerfile_path = Path("src/services/containers") / language / "Dockerfile"
        if dockerfile_path.exists():
            return self.ensure_docker_image(image_name, str(dockerfile_path))

        return False

    def get_container_config(self, language: str, tmpdir: str) -> Dict[str, Any]:
        """
        Get Docker container configuration with security settings
        Platform-aware configuration for Windows/Linux
        """
        is_windows = platform.system() == "Windows"

        config = {
            "image": f"ancient-compute/{language}:latest",
            "volumes": {tmpdir: {"bind": "/workspace", "mode": "ro"}},
            "working_dir": "/workspace",
            "mem_limit": "128m",
            "memswap_limit": "128m",
            "cpu_quota": 50000,  # 50% of one CPU
            "cpu_period": 100000,
            "network_mode": "none",
            "read_only": True,
            "detach": True,
            "remove": True,
            "security_opt": ["no-new-privileges"],
        }

        # Platform-specific adjustments
        if not is_windows:
            # Linux-specific security options
            config["tmpfs"] = {"/tmp": "size=32M,mode=1777"}
            config["pids_limit"] = 50
        else:
            # Windows adjustments
            # Windows containers might not support all Linux security features
            config.pop("tmpfs", None)  # Windows doesn't support tmpfs the same way

        return config


def get_docker_manager() -> DockerManager:
    """Get the singleton DockerManager instance"""
    return DockerManager()


# Example usage and testing
if __name__ == "__main__":
    manager = get_docker_manager()
    print(manager.get_status_report())

    # Test backend selection
    for lang in ["python", "c", "haskell", "java"]:
        backend = manager.get_backend_for_language(lang)
        print(f"\nBest backend for {lang}: {backend.value}")
