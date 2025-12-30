# Architectural Review: Week 2 Day 6 Implementation
## Ancient Compute Language Services Foundation

---

## Executive Summary

**Review Date**: October 11, 2025
**Review Focus**: Language Services Foundation (Code Execution)
**Overall Assessment**: **BLOCKED - Critical Issues Must Be Resolved**

### Issue Summary
- **Critical Issues**: 3 (Import paths, Docker dependency, RestrictedPython misuse)
- **Important Issues**: 4 (Error handling, package structure, resource limits, Windows compatibility)
- **Minor Issues**: 5 (Type hints, documentation, testing, performance, extensibility)

**Recommendation**: **DO NOT PROCEED to Day 7** until critical issues are resolved. The implementation has fundamental architectural flaws that will cascade into future work.

---

## 1. Critical Issues (Must Fix Immediately)

### 1.1 BROKEN Import Path Architecture

**Issue**: The import path `from ...services.languages import get_executor` in `code_execution.py` is fundamentally broken.

**Why It's Critical**:
- `backend/src/api/code_execution.py` tries to import from `backend/services/`
- These are sibling directories, not parent-child
- The relative import assumes `services` is under `src`, but it's not
- **THE CODE WILL NOT RUN AT ALL**

**Current Structure** (BROKEN):
```
backend/
├── services/           # Sibling to src/
│   └── languages/
└── src/                # Sibling to services/
    └── api/
        └── code_execution.py  # Tries ../../../services (doesn't exist)
```

**Fix Option 1: Restructure** (RECOMMENDED)
```python
# Move services under src
backend/
└── src/
    ├── api/
    │   └── code_execution.py
    └── services/       # Now a child of src
        └── languages/
```

**Fix Option 2: Absolute Imports**
```python
# backend/src/api/code_execution.py
import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../../')))
from services.languages import get_executor
```

**Fix Option 3: Proper Package Setup**
Create `backend/setup.py`:
```python
from setuptools import setup, find_packages

setup(
    name="ancient-compute-backend",
    packages=find_packages(),
    package_dir={
        'ancient_compute': 'src',
        'ancient_compute.services': 'services'
    }
)
```

### 1.2 Docker Hard Dependency Without Graceful Degradation

**Issue**: `BaseExecutor.__init__` immediately calls `docker.from_env()` with no error handling.

**Why It's Critical**:
- Crashes if Docker daemon isn't running
- No fallback mechanism
- Windows users might not have Docker Desktop started
- Development/testing becomes impossible without Docker

**Current Code** (BROKEN):
```python
def __init__(self, language: str, docker_image: str, timeout: int = 10):
    self.client = docker.from_env()  # CRASHES if Docker not available
```

**Fix**:
```python
def __init__(self, language: str, docker_image: str, timeout: int = 10):
    self.language = language
    self.docker_image = docker_image
    self.timeout = timeout
    self.client = None
    self.docker_available = self._check_docker()

def _check_docker(self) -> bool:
    """Check if Docker is available"""
    try:
        import docker
        self.client = docker.from_env()
        self.client.ping()
        return True
    except (ImportError, docker.errors.DockerException) as e:
        print(f"Warning: Docker not available: {e}")
        return False

async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
    if not self.docker_available:
        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stdout="",
            stderr="Docker is not available. Please ensure Docker Desktop is running.",
            execution_time=0
        )
    # ... rest of execution
```

### 1.3 RestrictedPython Not Actually Used

**Issue**: `python_service.py` imports RestrictedPython in comments/docstrings but doesn't use it.

**Why It's Critical**:
- False sense of security
- Python code runs unrestricted in Docker
- RestrictedPython compile_restricted() never called
- Basic string matching for security is inadequate

**Current Code** (MISLEADING):
```python
class PythonExecutor(BaseExecutor):
    """Python executor with RestrictedPython sandboxing"""  # FALSE!

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # Just checks strings, then runs unrestricted in Docker
        if 'import os' in code_lower:
            return ExecutionResult(status=ExecutionStatus.SECURITY_VIOLATION...)
        return await super().execute(code, input_data)  # Regular Docker execution
```

**Fix**:
```python
from RestrictedPython import compile_restricted, safe_globals
from RestrictedPython.Guards import guarded_iter_unpack_sequence

class PythonExecutor(BaseExecutor):
    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # Option 1: Use RestrictedPython for simple cases
        if self._is_simple_code(code):
            return await self._execute_restricted(code, input_data)

        # Option 2: Use Docker for complex cases
        return await super().execute(code, input_data)

    async def _execute_restricted(self, code: str, input_data: str = "") -> ExecutionResult:
        try:
            # Compile with restrictions
            compiled = compile_restricted(code, '<string>', 'exec')
            if compiled.errors:
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr="\n".join(compiled.errors),
                    execution_time=0
                )

            # Set up restricted environment
            restricted_globals = {
                '__builtins__': safe_globals,
                '__metaclass__': type,
                '__import__': self._restricted_import,
                'input': lambda: input_data,
                'print': self._capture_print,
            }

            # Execute
            exec(compiled.code, restricted_globals)

            return ExecutionResult(
                status=ExecutionStatus.SUCCESS,
                stdout=self.captured_output,
                stderr="",
                execution_time=0
            )
        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=str(e),
                execution_time=0
            )
```

---

## 2. Important Issues (Fix Before Day 7)

### 2.1 Missing Error Context in Docker Operations

**Issue**: No informative error messages when Docker operations fail.

**Fix**:
```python
async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
    try:
        container = self.client.containers.run(**self._get_container_config(tmpdir))
    except docker.errors.ImageNotFound:
        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stderr=f"Docker image '{self.docker_image}' not found. Run: docker build -t {self.docker_image} backend/services/containers/{self.language}"
        )
    except docker.errors.APIError as e:
        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stderr=f"Docker API error: {e}. Is Docker Desktop running?"
        )
```

### 2.2 Resource Limits Not Applied

**Issue**: `resource_limits.py` defines limits but `base_executor.py` doesn't use them.

**Fix**:
```python
from .security.resource_limits import get_resource_limit

def _get_container_config(self, code_path: str) -> Dict[str, Any]:
    limits = get_resource_limit(self.language)
    return {
        "mem_limit": f"{limits.memory_mb}m",
        "cpu_quota": limits.cpu_percent * 1000,
        "ulimits": [
            docker.types.Ulimit(name='nproc', soft=limits.max_processes, hard=limits.max_processes),
            docker.types.Ulimit(name='nofile', soft=limits.max_files, hard=limits.max_files),
        ],
        # ... rest of config
    }
```

### 2.3 Windows Path Handling Issues

**Issue**: Unix-style paths hardcoded, will fail on Windows.

**Current** (BROKEN on Windows):
```python
"command": "gcc -o /tmp/program main.c && /tmp/program"  # Unix paths
```

**Fix**:
```python
import platform

def _get_command(self, code_path: str) -> str:
    if platform.system() == 'Windows':
        # Windows containers use Windows paths
        return "gcc -o C:\\temp\\program.exe main.c && C:\\temp\\program.exe < input.txt"
    else:
        return "gcc -o /tmp/program main.c && /tmp/program < input.txt"
```

### 2.4 No Integration Tests

**Issue**: Tests require full Docker setup, no unit tests possible.

**Fix**: Add mock-based unit tests:
```python
# backend/services/tests/test_executors_unit.py
import pytest
from unittest.mock import Mock, patch, AsyncMock
from services.languages.python_service import PythonExecutor

@patch('services.base_executor.docker')
async def test_python_executor_security_check(mock_docker):
    executor = PythonExecutor()
    result = await executor.execute("import os\nprint('hello')", "")

    assert result.status == ExecutionStatus.SECURITY_VIOLATION
    assert "not allowed" in result.stderr
    mock_docker.from_env.assert_not_called()  # Should fail before Docker
```

---

## 3. Architectural Decisions Analysis

### 3.1 Decision: Separate packages (src/ vs services/)

**Assessment**: **Poor Decision**
- **Pros**: Theoretical separation of concerns
- **Cons**: Import complexity, deployment issues, testing complexity
- **Recommendation**: Consolidate under `src/`

### 3.2 Decision: Docker for Everything

**Assessment**: **Overengineered for Educational Platform**
- **Pros**: Strong isolation, consistent environments
- **Cons**: Heavy overhead, complex setup, requires Docker Desktop
- **Recommendation**: Hybrid approach - RestrictedPython for simple cases, Docker for complex

### 3.3 Decision: Async Everywhere

**Assessment**: **Good but Incomplete**
- **Pros**: Scalable, non-blocking
- **Cons**: Complex with Docker's synchronous API
- **Recommendation**: Keep async but add proper executor pool

---

## 4. Refactoring Plan

### Step 1: Fix Package Structure
```bash
# Move services under src
cd backend
mkdir -p src/services
mv services/* src/services/
rmdir services
```

### Step 2: Update Imports
```python
# backend/src/api/code_execution.py
from ..services.languages import get_executor  # Now correct!
```

### Step 3: Add Docker Graceful Degradation
```python
# backend/src/services/docker_utils.py
class DockerManager:
    _instance = None

    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance.client = cls._instance._init_docker()
        return cls._instance

    def _init_docker(self):
        try:
            import docker
            client = docker.from_env()
            client.ping()
            return client
        except:
            return None

    @property
    def available(self):
        return self.client is not None
```

### Step 4: Implement Hybrid Execution
```python
class PythonExecutor:
    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # Try RestrictedPython first for simple cases
        if not self._needs_docker(code):
            return await self._execute_restricted(code, input_data)

        # Fall back to Docker for complex cases
        if DockerManager().available:
            return await self._execute_docker(code, input_data)

        # No execution method available
        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stderr="No execution environment available"
        )
```

---

## 5. Validation Checklist

### Before Continuing to Day 7:

- [ ] Import paths work (`python -m pytest backend/tests`)
- [ ] Docker graceful degradation (`docker stop` then test)
- [ ] RestrictedPython actually restricts (`import os` should fail)
- [ ] Resource limits applied (check container stats)
- [ ] Windows compatibility (test on Windows 11)
- [ ] Error messages informative (kill Docker, run tests)
- [ ] Unit tests pass without Docker
- [ ] Integration tests pass with Docker
- [ ] API endpoints return proper HTTP codes
- [ ] Database models align with API models

---

## 6. Corrected File Structure

```
backend/
├── src/
│   ├── __init__.py
│   ├── api/
│   │   ├── __init__.py
│   │   ├── code_execution.py
│   │   └── router.py
│   ├── models/
│   │   ├── __init__.py
│   │   ├── user.py
│   │   ├── lesson.py
│   │   └── module.py
│   ├── services/
│   │   ├── __init__.py
│   │   ├── base_executor.py
│   │   ├── docker_utils.py
│   │   ├── languages/
│   │   │   ├── __init__.py
│   │   │   ├── c_service.py
│   │   │   ├── python_service.py
│   │   │   └── haskell_service.py
│   │   ├── security/
│   │   │   ├── __init__.py
│   │   │   └── resource_limits.py
│   │   └── containers/
│   │       ├── base/
│   │       ├── c/
│   │       ├── python/
│   │       └── haskell/
│   ├── config.py
│   ├── database.py
│   └── main.py
├── tests/
│   ├── __init__.py
│   ├── unit/
│   │   └── test_executors_unit.py
│   └── integration/
│       └── test_executors_integration.py
├── requirements.txt
├── setup.py
└── Dockerfile
```

---

## 7. Implementation Priority

### Immediate (Block Day 7):
1. Fix import paths - 30 minutes
2. Add Docker error handling - 1 hour
3. Implement RestrictedPython correctly - 2 hours

### Before Day 7:
4. Add resource limit integration - 1 hour
5. Create unit tests - 2 hours
6. Test Windows compatibility - 1 hour

### Can Defer:
7. Performance optimization
8. Additional language support
9. Advanced security features

---

## 8. Code Examples for Critical Fixes

### Fix 1: Complete Package Restructure Script
```python
#!/usr/bin/env python3
# backend/restructure.py
import os
import shutil
from pathlib import Path

def restructure_backend():
    backend = Path(__file__).parent
    src = backend / "src"
    services = backend / "services"

    if services.exists() and not (src / "services").exists():
        print("Moving services under src...")
        shutil.move(str(services), str(src / "services"))
        print("Done! Services moved to src/services")
    else:
        print("Already restructured or services not found")

    # Update imports
    code_exec = src / "api" / "code_execution.py"
    if code_exec.exists():
        content = code_exec.read_text()
        content = content.replace("from ...services", "from ..services")
        code_exec.write_text(content)
        print("Updated imports in code_execution.py")

if __name__ == "__main__":
    restructure_backend()
```

### Fix 2: Docker Manager with Fallback
```python
# backend/src/services/docker_manager.py
import asyncio
from typing import Optional
from dataclasses import dataclass
import subprocess
import tempfile
import os

@dataclass
class ExecutionBackend:
    name: str
    available: bool
    execute_func: callable

class DockerManager:
    def __init__(self):
        self.docker_client = self._init_docker()
        self.backends = self._init_backends()

    def _init_docker(self):
        try:
            import docker
            client = docker.from_env()
            client.ping()
            return client
        except Exception as e:
            print(f"Docker not available: {e}")
            return None

    def _init_backends(self):
        backends = []

        # Docker backend
        backends.append(ExecutionBackend(
            name="docker",
            available=self.docker_client is not None,
            execute_func=self._execute_docker
        ))

        # Local subprocess backend (fallback)
        backends.append(ExecutionBackend(
            name="subprocess",
            available=True,
            execute_func=self._execute_subprocess
        ))

        return backends

    async def execute(self, language: str, code: str, input_data: str = ""):
        for backend in self.backends:
            if backend.available:
                return await backend.execute_func(language, code, input_data)

        raise RuntimeError("No execution backend available")

    async def _execute_docker(self, language: str, code: str, input_data: str):
        # Docker execution logic
        pass

    async def _execute_subprocess(self, language: str, code: str, input_data: str):
        # Fallback subprocess execution
        if language == "python":
            # Use RestrictedPython
            from RestrictedPython import compile_restricted
            # ... implementation
        else:
            raise NotImplementedError(f"Subprocess execution not available for {language}")
```

### Fix 3: Proper RestrictedPython Implementation
```python
# backend/src/services/languages/python_service.py
from RestrictedPython import compile_restricted, safe_globals
from RestrictedPython.Guards import guarded_iter_unpack_sequence, guarded_inplacevar
from io import StringIO
import sys
import asyncio
from ..base_executor import BaseExecutor, ExecutionResult, ExecutionStatus

class PythonExecutor(BaseExecutor):
    def __init__(self):
        super().__init__(
            language="python",
            docker_image="ancient-compute/python:latest",
            timeout=5
        )
        self.use_restricted = True  # Prefer RestrictedPython

    async def execute(self, code: str, input_data: str = "") -> ExecutionResult:
        # First try RestrictedPython for better performance
        if self.use_restricted and self._can_use_restricted(code):
            result = await self._execute_restricted(code, input_data)
            if result.status != ExecutionStatus.SECURITY_VIOLATION:
                return result

        # Fall back to Docker for complex/unsafe code
        if self.docker_available:
            return await self._execute_docker(code, input_data)

        return ExecutionResult(
            status=ExecutionStatus.RUNTIME_ERROR,
            stdout="",
            stderr="No execution environment available",
            execution_time=0
        )

    def _can_use_restricted(self, code: str) -> bool:
        # Check if code is simple enough for RestrictedPython
        forbidden = ['open', 'file', '__import__', 'eval', 'exec', 'compile']
        code_lower = code.lower()
        return not any(f in code_lower for f in forbidden)

    async def _execute_restricted(self, code: str, input_data: str) -> ExecutionResult:
        start_time = asyncio.get_event_loop().time()

        try:
            # Compile with restrictions
            compiled = compile_restricted(code, '<string>', 'exec')

            if compiled.errors:
                return ExecutionResult(
                    status=ExecutionStatus.COMPILE_ERROR,
                    stdout="",
                    stderr="Compilation errors:\n" + "\n".join(compiled.errors),
                    execution_time=0
                )

            # Capture output
            stdout_capture = StringIO()
            stderr_capture = StringIO()

            # Build restricted globals
            restricted_globals = {
                '__builtins__': {
                    **safe_globals,
                    'print': lambda *args, **kwargs: print(*args, file=stdout_capture, **kwargs),
                    'input': lambda prompt="": input_data.split('\n').pop(0) if input_data else "",
                },
                '_iter_unpack_sequence_': guarded_iter_unpack_sequence,
                '_getiter_': iter,
                '__name__': '__main__',
                '__metaclass__': type,
            }

            # Execute with timeout
            try:
                await asyncio.wait_for(
                    asyncio.get_event_loop().run_in_executor(
                        None,
                        exec,
                        compiled.code,
                        restricted_globals
                    ),
                    timeout=self.timeout
                )

                return ExecutionResult(
                    status=ExecutionStatus.SUCCESS,
                    stdout=stdout_capture.getvalue()[:10000],
                    stderr=stderr_capture.getvalue()[:10000],
                    execution_time=asyncio.get_event_loop().time() - start_time
                )

            except asyncio.TimeoutError:
                return ExecutionResult(
                    status=ExecutionStatus.TIMEOUT,
                    stdout=stdout_capture.getvalue()[:10000],
                    stderr=f"Execution timeout ({self.timeout}s exceeded)",
                    execution_time=self.timeout
                )

        except Exception as e:
            return ExecutionResult(
                status=ExecutionStatus.RUNTIME_ERROR,
                stdout="",
                stderr=f"Runtime error: {str(e)}",
                execution_time=asyncio.get_event_loop().time() - start_time
            )
```

---

## 9. Testing Strategy

### Unit Tests (No Docker Required)
```python
# backend/tests/unit/test_python_executor.py
import pytest
from unittest.mock import Mock, patch
from src.services.languages.python_service import PythonExecutor
from src.services.base_executor import ExecutionStatus

@pytest.mark.asyncio
async def test_restricted_python_blocks_imports():
    executor = PythonExecutor()
    executor.use_restricted = True

    code = "import os\nprint(os.listdir())"
    result = await executor.execute(code)

    assert result.status == ExecutionStatus.COMPILE_ERROR
    assert "import" in result.stderr.lower()

@pytest.mark.asyncio
async def test_restricted_python_allows_safe_code():
    executor = PythonExecutor()
    executor.use_restricted = True

    code = "print('Hello, World!')"
    result = await executor.execute(code)

    assert result.status == ExecutionStatus.SUCCESS
    assert "Hello, World!" in result.stdout
```

### Integration Tests (Docker Required)
```python
# backend/tests/integration/test_docker_execution.py
import pytest
from src.services.languages.c_service import CExecutor

@pytest.mark.integration
@pytest.mark.asyncio
async def test_c_compilation_and_execution():
    executor = CExecutor()

    code = '''
    #include <stdio.h>
    int main() {
        printf("Test output");
        return 0;
    }
    '''

    result = await executor.execute(code)
    assert result.status == ExecutionStatus.SUCCESS
    assert "Test output" in result.stdout
```

---

## 10. Final Recommendations

### Go/No-Go Decision: **NO-GO**

**Cannot proceed to Day 7 until:**

1. **Import paths are fixed** - The code literally doesn't run
2. **Docker fallback implemented** - Can't require Docker Desktop for development
3. **RestrictedPython properly integrated** - Security claims must be accurate

### Immediate Action Plan:

1. **Hour 1**: Run restructure script, fix imports
2. **Hour 2**: Implement Docker fallback mechanism
3. **Hour 3-4**: Integrate RestrictedPython correctly
4. **Hour 5**: Write and run unit tests
5. **Hour 6**: Test on Windows 11 with PowerShell

### Success Criteria for Day 7:

- [ ] `python -m pytest backend/tests` passes
- [ ] API starts without Docker running
- [ ] Simple Python code runs via RestrictedPython
- [ ] Complex code falls back to Docker when available
- [ ] Clear error messages when execution fails
- [ ] Works on Windows 11 in PowerShell

---

## Appendix: Complete Fixed Implementation

The complete, working implementation is too large to include here, but is available in the following files:

1. `backend/restructure.py` - Package restructuring script
2. `backend/src/services/docker_manager.py` - Docker abstraction with fallback
3. `backend/src/services/languages/python_service_fixed.py` - Correct RestrictedPython usage
4. `backend/tests/unit/test_executors_unit.py` - Unit tests without Docker
5. `backend/setup.py` - Proper package configuration

Run these commands to apply all fixes:
```bash
cd C:/Users/ericj/Git-Projects/ancient_compute/backend
python restructure.py
pip install -e .
python -m pytest tests/unit -v
```

---

**End of Architectural Review**