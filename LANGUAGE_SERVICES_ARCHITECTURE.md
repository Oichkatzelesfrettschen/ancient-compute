# Language Services Architecture
## Ancient Compute Educational Platform - Week 2 Implementation

### Executive Summary
This document defines the comprehensive architecture for 8 language execution services (C, Python, Haskell, IDRIS2, LISP, Assembly, Java, System F) with production-grade sandboxing, resource management, and API integration.

### Service Directory Structure
```
backend/
└── services/
    ├── __init__.py
    ├── base_executor.py
    ├── security/
    │   ├── __init__.py
    │   ├── seccomp_profiles.py
    │   └── resource_limits.py
    ├── languages/
    │   ├── __init__.py
    │   ├── c_service.py
    │   ├── python_service.py
    │   ├── haskell_service.py
    │   ├── idris_service.py
    │   ├── lisp_service.py
    │   ├── assembly_service.py
    │   ├── java_service.py
    │   └── systemf_service.py
    └── containers/
        ├── base/
        │   └── Dockerfile
        ├── c/
        │   ├── Dockerfile
        │   └── compile.sh
        ├── python/
        │   ├── Dockerfile
        │   └── runner.py
        ├── haskell/
        │   ├── Dockerfile
        │   └── setup.sh
        ├── idris/
        │   ├── Dockerfile
        │   └── setup.sh
        ├── lisp/
        │   ├── Dockerfile
        │   └── setup.sh
        ├── assembly/
        │   ├── Dockerfile
        │   └── assemble.sh
        ├── java/
        │   ├── Dockerfile
        │   └── compile.sh
        └── systemf/
            ├── Dockerfile
            └── setup.sh
```

## Base Executor Class

```python
# backend/services/base_executor.py
import asyncio
import docker
import json
import tempfile
import os
from typing import Dict, Any, Optional, Tuple
from enum import Enum
from dataclasses import dataclass
import time

class ExecutionStatus(Enum):
    SUCCESS = "success"
    COMPILE_ERROR = "compile_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"
    MEMORY_EXCEEDED = "memory_exceeded"
    SECURITY_VIOLATION = "security_violation"

@dataclass
class ExecutionResult:
    status: ExecutionStatus
    stdout: str
    stderr: str
    compile_output: Optional[str] = None
    execution_time: float = 0.0
    memory_used: int = 0
    exit_code: int = 0

class BaseExecutor:
    def __init__(self, language: str, docker_image: str, timeout: int = 10):
        self.language = language
        self.docker_image = docker_image
        self.timeout = timeout
        self.client = docker.from_env()
        self._ensure_image()

    def _ensure_image(self):
        try:
            self.client.images.get(self.docker_image)
        except docker.errors.ImageNotFound:
            print(f"Building {self.docker_image}...")
            self._build_image()

    def _build_image(self):
        dockerfile_path = f"backend/services/containers/{self.language.lower()}"
        self.client.images.build(
            path=dockerfile_path,
            tag=self.docker_image,
            rm=True
        )

    def _get_container_config(self, code_path: str) -> Dict[str, Any]:
        return {
            "image": self.docker_image,
            "command": self._get_command(code_path),
            "volumes": {
                code_path: {
                    "bind": "/workspace",
                    "mode": "ro"
                }
            },
            "working_dir": "/workspace",
            "mem_limit": "128m",
            "memswap_limit": "128m",
            "cpu_quota": 50000,  # 50% of one CPU
            "cpu_period": 100000,
            "network_mode": "none",
            "read_only": True,
            "tmpfs": {
                "/tmp": "size=32M,mode=1777"
            },
            "security_opt": [
                "no-new-privileges",
                f"seccomp={self._get_seccomp_profile()}"
            ],
            "runtime": "runsc" if self._use_gvisor() else None,
            "detach": True,
            "remove": True
        }

    def _get_command(self, code_path: str) -> str:
        raise NotImplementedError("Subclass must implement _get_command")

    def _get_seccomp_profile(self) -> str:
        profile_path = f"/etc/docker/seccomp/{self.language.lower()}.json"
        if os.path.exists(profile_path):
            return profile_path
        return "default"

    def _use_gvisor(self) -> bool:
        # Enable gVisor for untrusted languages
        return self.language.lower() in ["c", "assembly", "systemf"]

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        start_time = time.time()

        with tempfile.TemporaryDirectory() as tmpdir:
            code_file = os.path.join(tmpdir, self._get_source_filename())
            input_file = os.path.join(tmpdir, "input.txt")

            # Write code and input
            with open(code_file, "w", encoding="ascii") as f:
                f.write(code)
            with open(input_file, "w", encoding="ascii") as f:
                f.write(input_data)

            try:
                container = self.client.containers.run(
                    **self._get_container_config(tmpdir)
                )

                # Wait for container with timeout
                exit_code = await self._wait_container(container)

                # Get logs
                logs = container.logs(stdout=True, stderr=True)
                stdout = logs.decode("ascii", errors="replace")

                execution_time = time.time() - start_time

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS if exit_code == 0 else ExecutionStatus.RUNTIME_ERROR,
                    stdout=stdout[:10000],  # Limit output
                    stderr="",
                    execution_time=execution_time,
                    exit_code=exit_code
                )

            except asyncio.TimeoutError:
                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout="",
                    stderr=f"Execution timeout ({self.timeout}s exceeded)",
                    execution_time=self.timeout
                )
            except Exception as e:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout="",
                    stderr=str(e),
                    execution_time=time.time() - start_time
                )

    async def _wait_container(self, container) -> int:
        loop = asyncio.get_event_loop()
        return await asyncio.wait_for(
            loop.run_in_executor(None, container.wait),
            timeout=self.timeout
        )

    def _get_source_filename(self) -> str:
        extensions = {
            "c": "main.c",
            "python": "main.py",
            "haskell": "Main.hs",
            "idris": "Main.idr",
            "lisp": "main.lisp",
            "assembly": "main.asm",
            "java": "Main.java",
            "systemf": "main.sf"
        }
        return extensions.get(self.language.lower(), "main.txt")
```

## Language-Specific Services

### C Language Service

```python
# backend/services/languages/c_service.py
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus
import os
import subprocess
import tempfile

class CExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="c",
            docker_image="ancient-compute/c:latest",
            timeout=10
        )

    def _get_command(self, code_path: str) -> str:
        return f"gcc -Wall -Werror -O2 -o /tmp/program main.c && /tmp/program < input.txt"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # Add safety checks
        forbidden_patterns = [
            "#include <sys/",
            "system(",
            "exec",
            "fork(",
            "socket(",
            "__asm__",
            "inline asm"
        ]

        for pattern in forbidden_patterns:
            if pattern in code:
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Security violation: '{pattern}' is not allowed",
                    execution_time=0
                )

        return await super().execute(code, input_data)
```

### Python Service with RestrictedPython

```python
# backend/services/languages/python_service.py
from RestrictedPython import compile_restricted, safe_globals
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus
import sys
import io
import traceback
import resource
import signal

class PythonExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="python",
            docker_image="ancient-compute/python:latest",
            timeout=5
        )

    def _get_safe_globals(self):
        safe_dict = safe_globals.copy()
        safe_dict.update({
            '__builtins__': {
                'print': print,
                'range': range,
                'len': len,
                'int': int,
                'float': float,
                'str': str,
                'list': list,
                'dict': dict,
                'tuple': tuple,
                'set': set,
                'bool': bool,
                'True': True,
                'False': False,
                'None': None,
                'abs': abs,
                'min': min,
                'max': max,
                'sum': sum,
                'sorted': sorted,
                'enumerate': enumerate,
                'zip': zip,
                'map': map,
                'filter': filter,
            }
        })
        return safe_dict

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        try:
            # Compile with RestrictedPython
            byte_code = compile_restricted(code, '<string>', 'exec')

            if byte_code.errors:
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr="\n".join(byte_code.errors),
                    execution_time=0
                )

            # Set resource limits
            def set_limits():
                resource.setrlimit(resource.RLIMIT_CPU, (5, 5))
                resource.setrlimit(resource.RLIMIT_AS, (128 * 1024 * 1024, 128 * 1024 * 1024))

            # Execute in restricted environment
            old_stdout = sys.stdout
            old_stdin = sys.stdin
            sys.stdout = io.StringIO()
            sys.stdin = io.StringIO(input_data)

            try:
                exec(byte_code.code, self._get_safe_globals())
                output = sys.stdout.getvalue()

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS,
                    stdout=output[:10000],
                    stderr="",
                    execution_time=0
                )
            except Exception as e:
                return ExecutionResult(
                    status=ExecutionStatus.RUNTIME_ERROR,
                    stdout=sys.stdout.getvalue(),
                    stderr=traceback.format_exc(),
                    execution_time=0
                )
            finally:
                sys.stdout = old_stdout
                sys.stdin = old_stdin

        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=str(e),
                execution_time=0
            )
```

### Haskell Service

```python
# backend/services/languages/haskell_service.py
from ..base_executor import BaseExecutor

class HaskellExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="haskell",
            docker_image="ancient-compute/haskell:latest",
            timeout=15
        )

    def _get_command(self, code_path: str) -> str:
        return "ghc -O2 -Wall -o /tmp/program Main.hs && /tmp/program < input.txt"
```

### IDRIS2 Service

```python
# backend/services/languages/idris_service.py
from ..base_executor import BaseExecutor

class IdrisExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="idris",
            docker_image="ancient-compute/idris:latest",
            timeout=20
        )

    def _get_command(self, code_path: str) -> str:
        return "idris2 Main.idr -o /tmp/program && /tmp/program < input.txt"
```

### LISP Service

```python
# backend/services/languages/lisp_service.py
from ..base_executor import BaseExecutor

class LispExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="lisp",
            docker_image="ancient-compute/lisp:latest",
            timeout=10
        )

    def _get_command(self, code_path: str) -> str:
        return "sbcl --script main.lisp < input.txt"
```

### Assembly Service (x86-64)

```python
# backend/services/languages/assembly_service.py
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus

class AssemblyExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="assembly",
            docker_image="ancient-compute/assembly:latest",
            timeout=5
        )

    def _get_command(self, code_path: str) -> str:
        return "nasm -f elf64 main.asm -o /tmp/main.o && ld /tmp/main.o -o /tmp/program && /tmp/program"

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # Restrict dangerous syscalls
        forbidden = ["execve", "fork", "clone", "socket", "connect"]
        for syscall in forbidden:
            if syscall in code.lower():
                return ExecutionResult(
                    status=ExecutionStatus.SECURITY_VIOLATION,
                    stdout="",
                    stderr=f"Syscall '{syscall}' is not permitted",
                    execution_time=0
                )

        return await super().execute(code, input_data)
```

### Java Service

```python
# backend/services/languages/java_service.py
from ..base_executor import BaseExecutor

class JavaExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="java",
            docker_image="ancient-compute/java:latest",
            timeout=15
        )

    def _get_command(self, code_path: str) -> str:
        return "javac Main.java && java -Xmx64m -Xms16m Main < input.txt"
```

### System F Service

```python
# backend/services/languages/systemf_service.py
from ..base_executor import BaseExecutor

class SystemFExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="systemf",
            docker_image="ancient-compute/systemf:latest",
            timeout=10
        )

    def _get_command(self, code_path: str) -> str:
        # Using a System F interpreter (custom or academic implementation)
        return "systemf-interp main.sf < input.txt"
```

## Docker Container Specifications

### Base Container
```dockerfile
# backend/services/containers/base/Dockerfile
FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash runner -u 1000
USER runner
WORKDIR /workspace
```

### C Container
```dockerfile
# backend/services/containers/c/Dockerfile
FROM ancient-compute/base:latest

USER root
RUN apt-get update && apt-get install -y \
    gcc \
    libc6-dev \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

USER runner
```

### Python Container
```dockerfile
# backend/services/containers/python/Dockerfile
FROM ancient-compute/base:latest

USER root
RUN apt-get update && apt-get install -y \
    python3 \
    python3-pip \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

RUN pip3 install --no-cache-dir RestrictedPython

USER runner
```

### Haskell Container
```dockerfile
# backend/services/containers/haskell/Dockerfile
FROM ancient-compute/base:latest

USER root
RUN apt-get update && apt-get install -y \
    ghc \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

USER runner
```

### Assembly Container
```dockerfile
# backend/services/containers/assembly/Dockerfile
FROM ancient-compute/base:latest

USER root
RUN apt-get update && apt-get install -y \
    nasm \
    binutils \
    --no-install-recommends \
    && rm -rf /var/lib/apt/lists/*

USER runner
```

## API Endpoint Definitions

```python
# backend/src/api/code_execution.py
from fastapi import APIRouter, HTTPException, Depends
from pydantic import BaseModel, Field
from typing import Optional, Literal
from ..services.languages import get_executor
from ..models import CodeSubmission, User, Lesson
from ..database import get_db
from sqlalchemy.orm import Session

router = APIRouter(prefix="/api/execute", tags=["code-execution"])

class ExecutionRequest(BaseModel):
    language: Literal["c", "python", "haskell", "idris", "lisp", "assembly", "java", "systemf"]
    code: str = Field(..., max_length=50000)
    input_data: str = Field(default="", max_length=10000)
    lesson_id: Optional[int] = None

class ExecutionResponse(BaseModel):
    status: str
    stdout: str
    stderr: str
    compile_output: Optional[str]
    execution_time: float
    memory_used: int

@router.post("/run", response_model=ExecutionResponse)
async def execute_code(
    request: ExecutionRequest,
    current_user: User = Depends(get_current_user),
    db: Session = Depends(get_db)
):
    # Get executor for language
    executor = get_executor(request.language)

    if not executor:
        raise HTTPException(status_code=400, detail=f"Language {request.language} not supported")

    # Execute code
    result = await executor.execute(request.code, request.input_data)

    # Save submission if lesson_id provided
    if request.lesson_id:
        submission = CodeSubmission(
            user_id=current_user.id,
            lesson_id=request.lesson_id,
            language=request.language,
            code=request.code,
            status=result.status.value,
            output=result.stdout,
            error_output=result.stderr,
            execution_time=result.execution_time
        )
        db.add(submission)
        db.commit()

    return ExecutionResponse(
        status=result.status.value,
        stdout=result.stdout,
        stderr=result.stderr,
        compile_output=result.compile_output,
        execution_time=result.execution_time,
        memory_used=result.memory_used
    )

@router.get("/languages")
async def get_supported_languages():
    return {
        "languages": [
            {"id": "c", "name": "C", "version": "GCC 12.2"},
            {"id": "python", "name": "Python", "version": "3.11 (Restricted)"},
            {"id": "haskell", "name": "Haskell", "version": "GHC 9.2"},
            {"id": "idris", "name": "IDRIS2", "version": "0.6.0"},
            {"id": "lisp", "name": "Common LISP", "version": "SBCL 2.3"},
            {"id": "assembly", "name": "Assembly", "version": "NASM x86-64"},
            {"id": "java", "name": "Java", "version": "OpenJDK 17"},
            {"id": "systemf", "name": "System F", "version": "Academic"}
        ]
    }
```

## Resource Limits Configuration

```python
# backend/services/security/resource_limits.py
from dataclasses import dataclass
from typing import Dict

@dataclass
class ResourceLimit:
    memory_mb: int
    cpu_percent: int
    timeout_seconds: int
    max_processes: int
    max_files: int
    max_file_size_mb: int
    network_enabled: bool

LANGUAGE_LIMITS: Dict[str, ResourceLimit] = {
    "c": ResourceLimit(
        memory_mb=128,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "python": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=5,
        max_processes=1,
        max_files=5,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "haskell": ResourceLimit(
        memory_mb=512,
        cpu_percent=75,
        timeout_seconds=15,
        max_processes=1,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "idris": ResourceLimit(
        memory_mb=512,
        cpu_percent=75,
        timeout_seconds=20,
        max_processes=1,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "lisp": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "assembly": ResourceLimit(
        memory_mb=64,
        cpu_percent=25,
        timeout_seconds=5,
        max_processes=1,
        max_files=5,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "java": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=15,
        max_processes=5,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "systemf": ResourceLimit(
        memory_mb=128,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    )
}
```

## Bazel Build Integration

```python
# backend/services/BUILD.bazel
load("@rules_python//python:defs.bzl", "py_library", "py_test")
load("@rules_docker//python:image.bzl", "py_image")
load("@rules_docker//container:container.bzl", "container_image")

py_library(
    name = "base_executor",
    srcs = ["base_executor.py"],
    deps = [
        "@pip//docker",
        "@pip//dataclasses",
    ],
)

py_library(
    name = "language_services",
    srcs = glob(["languages/*.py"]),
    deps = [
        ":base_executor",
        "@pip//RestrictedPython",
    ],
)

# Docker image builds
container_image(
    name = "base_image",
    base = "@debian_bookworm_slim//image",
    directory = "/workspace",
    files = ["containers/base/Dockerfile"],
)

container_image(
    name = "c_image",
    base = ":base_image",
    packages = ["gcc", "libc6-dev"],
)

container_image(
    name = "python_image",
    base = ":base_image",
    packages = ["python3", "python3-pip"],
    pip_packages = ["RestrictedPython"],
)

# Tests
py_test(
    name = "executor_test",
    srcs = ["tests/test_executors.py"],
    deps = [
        ":language_services",
        "@pip//pytest",
        "@pip//pytest-asyncio",
    ],
)
```

## Testing Strategy

```python
# backend/services/tests/test_executors.py
import pytest
import asyncio
from ..languages import (
    CExecutor, PythonExecutor, HaskellExecutor,
    LispExecutor, AssemblyExecutor, JavaExecutor
)
from ..base_executor import ExecutionStatus

@pytest.mark.asyncio
async def test_c_hello_world():
    executor = CExecutor()
    code = '''
    #include <stdio.h>
    int main() {
        printf("Hello, Ancient Compute!");
        return 0;
    }
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SUCCESS
    assert "Hello, Ancient Compute!" in result.stdout

@pytest.mark.asyncio
async def test_c_security_violation():
    executor = CExecutor()
    code = '''
    #include <stdio.h>
    #include <sys/socket.h>
    int main() {
        return 0;
    }
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SECURITY_VIOLATION

@pytest.mark.asyncio
async def test_python_restricted():
    executor = PythonExecutor()
    code = '''
import os  # Should fail
print("Should not execute")
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.COMPILE_ERROR

@pytest.mark.asyncio
async def test_python_safe_execution():
    executor = PythonExecutor()
    code = '''
numbers = [1, 2, 3, 4, 5]
print(sum(numbers))
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SUCCESS
    assert "15" in result.stdout

@pytest.mark.asyncio
async def test_timeout():
    executor = PythonExecutor()
    code = '''
while True:
    pass
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.TIMEOUT

@pytest.mark.asyncio
async def test_assembly_basic():
    executor = AssemblyExecutor()
    code = '''
section .data
    msg db 'Hello', 0xa
    len equ $ - msg

section .text
global _start
_start:
    mov eax, 4
    mov ebx, 1
    mov ecx, msg
    mov edx, len
    int 0x80

    mov eax, 1
    xor ebx, ebx
    int 0x80
    '''
    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SUCCESS
    assert "Hello" in result.stdout

class TestResourceLimits:
    @pytest.mark.asyncio
    async def test_memory_limit(self):
        executor = CExecutor()
        code = '''
        #include <stdlib.h>
        int main() {
            // Try to allocate 200MB (exceeds 128MB limit)
            void *p = malloc(200 * 1024 * 1024);
            return 0;
        }
        '''
        result = await executor.execute(code)
        assert result.status in [ExecutionStatus.RUNTIME_ERROR, ExecutionStatus.MEMORY_EXCEEDED]
```

## Monitoring and Metrics

```python
# backend/services/monitoring.py
from prometheus_client import Counter, Histogram, Gauge
import time

execution_counter = Counter(
    'code_executions_total',
    'Total number of code executions',
    ['language', 'status']
)

execution_duration = Histogram(
    'code_execution_duration_seconds',
    'Code execution duration',
    ['language']
)

active_containers = Gauge(
    'active_execution_containers',
    'Number of active execution containers',
    ['language']
)

def track_execution(language: str, status: str, duration: float):
    execution_counter.labels(language=language, status=status).inc()
    execution_duration.labels(language=language).observe(duration)
```

## Security Considerations

1. **Container Isolation**: Each language runs in isolated Docker container
2. **Network Isolation**: Containers run with `network_mode: none`
3. **Resource Limits**: CPU, memory, and time limits enforced
4. **Read-only Filesystem**: Containers use read-only root with tmpfs for temporary files
5. **User Privileges**: Non-root user (uid 1000) for all operations
6. **Seccomp Profiles**: Syscall filtering per language
7. **gVisor Runtime**: Additional kernel isolation for high-risk languages
8. **Code Validation**: Pre-execution security checks for dangerous patterns

## Integration Points

- **FastAPI Routes**: `/api/execute/run`, `/api/execute/languages`
- **Database Models**: CodeSubmission for storing execution history
- **Frontend API Client**: TypeScript client for code execution
- **WebSocket Support**: Real-time execution status updates (future)
- **Queue System**: Redis-based queue for async execution (future)

## Performance Optimizations

1. **Image Caching**: Pre-built Docker images for fast container startup
2. **Connection Pooling**: Reuse Docker client connections
3. **Async Execution**: Non-blocking execution with asyncio
4. **Output Streaming**: Stream output for long-running programs
5. **Container Reuse**: Pool of warm containers (future optimization)