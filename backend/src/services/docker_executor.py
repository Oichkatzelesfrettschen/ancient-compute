"""
Ancient Compute - Docker Executor for Language Services

Implements proper Docker-based code execution for all supported languages
using BaseExecutor pattern with sandboxing and resource constraints.
"""

import asyncio
from typing import Optional, Any

from .base_executor import BaseExecutor, ExecutionResult, ExecutionStatus


class PythonExecutor(BaseExecutor):
    """Python code executor with Docker containerization."""

    def __init__(self, timeout: int = 10, memory_limit: int = 128):
        super().__init__("python", "ancient-compute/python:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for Python."""
        return "python /workspace/main.py"


class CExecutor(BaseExecutor):
    """C code executor with Docker containerization."""

    def __init__(self, timeout: int = 10, memory_limit: int = 128):
        super().__init__("c", "ancient-compute/c:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for C."""
        return "/bin/bash -c 'gcc -o /tmp/main /workspace/main.c && /tmp/main'"


class HaskellExecutor(BaseExecutor):
    """Haskell code executor with Docker containerization."""

    def __init__(self, timeout: int = 15, memory_limit: int = 256):
        super().__init__("haskell", "ancient-compute/haskell:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for Haskell."""
        return "/bin/bash -c 'ghc -o /tmp/main /workspace/Main.hs && /tmp/main'"


class IdrisExecutor(BaseExecutor):
    """IDRIS2 code executor with Docker containerization."""

    def __init__(self, timeout: int = 20, memory_limit: int = 256):
        super().__init__("idris", "ancient-compute/idris:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for IDRIS2."""
        return "/bin/bash -c 'idris2 --cg javascript -x main /workspace/Main.idr'"


class LispExecutor(BaseExecutor):
    """LISP code executor with Docker containerization."""

    def __init__(self, timeout: int = 10, memory_limit: int = 128):
        super().__init__("lisp", "ancient-compute/lisp:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for LISP."""
        return "sbcl --script /workspace/main.lisp"


class JavaExecutor(BaseExecutor):
    """Java code executor with Docker containerization."""

    def __init__(self, timeout: int = 15, memory_limit: int = 256):
        super().__init__("java", "ancient-compute/java:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for Java."""
        return "/bin/bash -c 'cd /workspace && javac Main.java && java Main'"


class AssemblyExecutor(BaseExecutor):
    """x86-64 Assembly code executor with Docker containerization."""

    def __init__(self, timeout: int = 10, memory_limit: int = 64):
        super().__init__("assembly", "ancient-compute/assembly:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for Assembly."""
        return "/bin/bash -c 'nasm -f elf64 /workspace/main.asm -o /tmp/main.o && ld /tmp/main.o -o /tmp/main && /tmp/main'"


class SystemFExecutor(BaseExecutor):
    """System F code executor with Docker containerization."""

    def __init__(self, timeout: int = 15, memory_limit: int = 128):
        super().__init__("systemf", "ancient-compute/systemf:latest", timeout)
        self.memory_limit = memory_limit

    def _get_command(self, code_path: str) -> str:
        """Get execution command for System F."""
        return "systemf-eval /workspace/main.sf"


class ExecutorRegistry:
    """Registry mapping language names to executor classes."""

    _executors = {
        "python": PythonExecutor,
        "c": CExecutor,
        "haskell": HaskellExecutor,
        "idris": IdrisExecutor,
        "lisp": LispExecutor,
        "java": JavaExecutor,
        "assembly": AssemblyExecutor,
        "systemf": SystemFExecutor,
    }

    @classmethod
    def get_executor(
        cls,
        language: str,
        timeout: Optional[int] = None,
        memory_limit: Optional[int] = None,
    ) -> Optional[BaseExecutor]:
        """
        Get executor instance for specified language.

        Args:
            language: Programming language name
            timeout: Optional execution timeout in seconds
            memory_limit: Optional memory limit in MB

        Returns:
            Executor instance or None if language not supported
        """
        language = language.lower()
        if language not in cls._executors:
            return None

        executor_class = cls._executors[language]
        if timeout is not None and memory_limit is not None:
            return executor_class(timeout=timeout, memory_limit=memory_limit)
        elif timeout is not None:
            return executor_class(timeout=timeout)
        elif memory_limit is not None:
            return executor_class(memory_limit=memory_limit)
        else:
            return executor_class()

    @classmethod
    def register_executor(cls, language: str, executor_class: type) -> None:
        """
        Register custom executor for language.

        Args:
            language: Language identifier
            executor_class: BaseExecutor subclass
        """
        cls._executors[language.lower()] = executor_class

    @classmethod
    def list_supported_languages(cls) -> list:
        """Get list of supported languages."""
        return sorted(list(cls._executors.keys()))

    @classmethod
    def is_supported(cls, language: str) -> bool:
        """Check if language is supported."""
        return language.lower() in cls._executors
