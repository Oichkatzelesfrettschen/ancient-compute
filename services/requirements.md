# Services Module - Requirements and Architecture

**Module Path**: `services/`
**Container Runtime**: Docker 24.0+, Docker Compose 2.20+
**Status**: C, Python, Haskell Complete; IDRIS2, LISP, System F, Java Planned

---

## Overview

The services directory contains isolated Docker containers for each programming language. Each container:
- Executes untrusted user code in a sandbox
- Implements a standardized language service interface
- Provides compiler/interpreter toolchains
- Reports execution metrics and resource usage
- Enforces security and resource limits

**Architecture Pattern**: One container per language, orchestrated by backend via Docker API

---

## Directory Structure

```
services/
├── requirements.md                # This file
├── c/                            # C language service
│   ├── Dockerfile
│   ├── requirements.txt
│   ├── src/
│   │   ├── compile.sh            # Compilation wrapper
│   │   └── execute.py            # Service entrypoint
│   └── README.md
├── python/                       # Python language service
│   ├── Dockerfile
│   ├── requirements.txt
│   └── src/
│       └── execute.py
├── haskell/                      # Haskell language service
│   ├── Dockerfile
│   ├── cabal.project
│   └── src/
│       └── execute.hs
├── idris/                        # IDRIS2 language service (TODO)
│   ├── Dockerfile
│   ├── requirements.txt
│   └── src/
│       └── execute.py
├── lisp/                         # LISP language service (TODO)
│   ├── Dockerfile
│   ├── requirements.txt
│   └── src/
│       └── execute.py
├── systemf/                      # System F language service (TODO)
│   ├── Dockerfile
│   ├── requirements.txt
│   └── src/
│       └── execute.py
├── java/                         # Java language service (TODO)
│   ├── Dockerfile
│   ├── pom.xml
│   └── src/
│       └── Executor.java
├── assembly/                     # Babbage assembly service
│   ├── Dockerfile
│   ├── requirements.txt
│   └── src/
│       └── execute.py
└── docker-compose.yml            # Orchestration file
```

---

## Docker Base Images

### C Service

**Base Image**: `gcc:13-bullseye`
**Size**: 1.4 GB
**Toolchain**:
- GCC 13.2.0 (C11 compiler)
- glibc 2.31 (GNU C Library)
- make 4.3 (build tool)
- gdb (debugger, optional)

**Dockerfile**:
```dockerfile
FROM gcc:13-bullseye
RUN apt-get update && apt-get install -y \
    make \
    binutils \
    && rm -rf /var/lib/apt/lists/*
COPY src/ /app/
WORKDIR /app
ENTRYPOINT ["python3", "execute.py"]
```

**Resource Limits**:
- CPU: 1 core
- Memory: 512 MB
- Disk: 100 MB temp storage
- Execution timeout: 10 seconds

### Python Service

**Base Image**: `python:3.12-slim`
**Size**: 450 MB
**Toolchain**:
- CPython 3.12.0
- pip (package manager)
- venv (virtual environments)

**Dockerfile**:
```dockerfile
FROM python:3.12-slim
RUN apt-get update && apt-get install -y \
    gcc \
    && rm -rf /var/lib/apt/lists/*
COPY requirements.txt /app/
RUN pip install -r /app/requirements.txt
COPY src/ /app/
WORKDIR /app
ENTRYPOINT ["python3", "execute.py"]
```

**Python Packages** (restricted environment):
```
RestrictedPython==6.2              # Sandboxing
```

### Haskell Service

**Base Image**: `haskell:9.6`
**Size**: 3.2 GB
**Toolchain**:
- GHC 9.6.2 (Haskell compiler)
- cabal 3.10 (package manager)
- stack (alternative build tool)

**Dockerfile**:
```dockerfile
FROM haskell:9.6
RUN apt-get update && apt-get install -y \
    build-essential \
    && rm -rf /var/lib/apt/lists/*
COPY cabal.project /app/
COPY src/ /app/
WORKDIR /app
RUN cabal update && cabal build
ENTRYPOINT ["python3", "execute.py"]
```

### IDRIS2 Service (Planned)

**Base Image**: `idris/idris2:latest`
**Size**: 2.1 GB
**Toolchain**:
- IDRIS2 (dependent types compiler)
- Scheme runtime (CHEZ Scheme)
- Package manager

**Key Requirement**:
- Support dependent types with compile-time proofs
- Generate Scheme code for execution
- Type checking of historical algorithms

### LISP Service (Planned)

**Base Image**: `debian:bookworm-slim`
**Size**: 90 MB (base) + 250 MB (SBCL)
**Toolchain**:
- SBCL 2.3 (Steel Bank Common Lisp)
- or: Clojure with JVM
- Quicklisp (package manager)

**Key Requirement**:
- S-expression parsing and evaluation
- Symbol manipulation and meta-programming
- Sandboxed macro execution

### System F Service (Planned)

**Base Image**: `haskell:9.6`
**Size**: 3.2 GB
**Toolchain**:
- GHC 9.6.2 (for System F compilation)
- Higher-Rank Polymorphism support

**Key Requirement**:
- Rank-2 polymorphism (forall types)
- Type application and abstraction
- Monomorphization to executable code

### Java Service (Planned)

**Base Image**: `eclipse-temurin:21-jdk`
**Size**: 800 MB
**Toolchain**:
- OpenJDK 21 (compiler & runtime)
- Maven 3.8+ (build tool)
- JAR execution

**Key Requirement**:
- Object-oriented paradigm
- Class hierarchy and method resolution
- Exception handling

### Assembly Service

**Base Image**: `debian:bookworm-slim`
**Size**: 90 MB (base) + 50 MB (tools)
**Toolchain**:
- nasm (x86 assembler)
- binutils (linking & objdump)
- Custom Babbage ISA assembler (Python)

**Key Requirement**:
- Babbage ISA assembly support
- Direct bytecode generation
- No compilation phase (assembly only)

---

## Service Interface

### Standard Request Format

All services receive JSON requests on stdin:

```json
{
  "code": "// or (lambda ...) or main() { ... }",
  "timeout_seconds": 10,
  "input_data": "optional stdin"
}
```

### Standard Response Format

All services return JSON on stdout:

```json
{
  "status": "SUCCESS|COMPILE_ERROR|RUNTIME_ERROR|TIMEOUT",
  "output": "stdout from execution",
  "errors": ["error message 1", "error message 2"],
  "ir": "intermediate representation (if applicable)",
  "assembly": "babbage assembly code",
  "machine_code": "hex-encoded bytecode",
  "compile_time_ms": 45,
  "codegen_time_ms": 23,
  "assembly_time_ms": 12
}
```

### Environment Variables

All services receive:
```bash
TIMEOUT_SECONDS=10           # Execution timeout
MAX_MEMORY_MB=512            # Memory limit
WORK_DIR=/tmp/work           # Temporary workspace
OUTPUT_LIMIT_KB=1000         # Max output size
```

---

## Security Sandbox

### Seccomp-BPF Filters

Whitelist approach - only allow safe syscalls:

**Allowed**:
- read, write (stdio only)
- open, close (temp directory only)
- mmap, mprotect (memory management)
- brk (heap allocation)
- rt_sigaction, rt_sigprocmask (signal handling)
- exit, exit_group (termination)

**Blocked**:
- network: socket, connect, bind, listen
- fork/exec: fork, execve (process creation)
- file I/O: mkdir, unlink, chmod (outside temp)
- raw syscalls: ioctl, prctl

### cgroups v2 Limits

**CPU**:
```bash
cpuset.cpus=0              # Single CPU core
cpu.max=100000:1000000     # 100ms per 1s window (100% utilization)
```

**Memory**:
```bash
memory.max=512M             # Hard limit 512 MB
memory.swap.max=0           # No swap
```

**I/O**:
```bash
io.max=rbps=10M:wbps=10M   # Read: 10 MB/s, Write: 10 MB/s
pids.max=10                 # Max 10 processes
```

### Linux Namespaces

**PID Namespace**: Process isolation
- Container PID 1 is service entrypoint
- User code runs as non-root

**Mount Namespace**: Filesystem isolation
- Only `/tmp/work` writable
- Everything else read-only
- No access to host filesystem

**Network Namespace**: Complete network isolation
- No outbound connections
- No inter-service communication
- Loopback only for IPC with service entrypoint

---

## Docker Compose Orchestration

### Configuration File

**Location**: `docker-compose.yml` (in services/ or root)

**Services Defined**:
```yaml
version: '3.8'

services:
  # Database (required)
  postgres:
    image: postgres:15-alpine
    environment:
      POSTGRES_DB: ancient_compute
      POSTGRES_PASSWORD: password
    volumes:
      - postgres_data:/var/lib/postgresql/data
    ports:
      - "5432:5432"

  # Cache (optional but recommended)
  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

  # Backend API
  backend:
    build:
      context: .
      dockerfile: backend/Dockerfile
    ports:
      - "8000:8000"
    environment:
      DATABASE_URL: postgresql://postgres:password@postgres/ancient_compute
      REDIS_URL: redis://redis:6379/0
    depends_on:
      - postgres
      - redis

  # Language Services (built on-demand)
  c-service:
    build:
      context: services/c
    # Resource limits enforced via cgroups

  python-service:
    build:
      context: services/python

  haskell-service:
    build:
      context: services/haskell

  # ... more services
```

**Start All Services**:
```bash
docker-compose up -d

# Verify services running
docker-compose ps

# View logs
docker-compose logs -f backend
docker-compose logs -f c-service
```

---

## Service Implementation Template

### Python-Based Service (Most Common)

**File**: `services/{language}/src/execute.py`

```python
#!/usr/bin/env python3
import json
import subprocess
import sys
import time
import tempfile
from pathlib import Path

def execute(request: dict) -> dict:
    """Execute code and return results."""
    code = request.get('code', '')
    timeout = request.get('timeout_seconds', 10)

    try:
        # Phase 1: Lexing (if applicable)
        tokens = tokenize(code)

        # Phase 2: Parsing
        ast = parse(tokens)

        # Phase 3: Semantic Analysis
        symbols = analyze(ast)

        # Phase 4: IR Generation
        ir = generate_ir(ast, symbols)

        # Phase 5: Code Generation
        assembly = generate_assembly(ir)

        # Phase 6: Assembly
        machine_code = assemble(assembly)

        # Execute
        start = time.time()
        result = execute_machine_code(machine_code)
        elapsed = time.time() - start

        return {
            'status': 'SUCCESS',
            'output': result.stdout,
            'errors': [],
            'ir': ir,
            'assembly': assembly,
            'machine_code': machine_code,
            'compile_time_ms': 10,
            'codegen_time_ms': 5,
            'assembly_time_ms': 2
        }
    except SyntaxError as e:
        return {
            'status': 'COMPILE_ERROR',
            'output': '',
            'errors': [str(e)],
            'ir': '',
            'assembly': '',
            'machine_code': ''
        }
    except TimeoutError:
        return {
            'status': 'TIMEOUT',
            'output': 'Execution exceeded timeout',
            'errors': [],
            'ir': '',
            'assembly': '',
            'machine_code': ''
        }

if __name__ == '__main__':
    request = json.loads(sys.stdin.read())
    response = execute(request)
    print(json.dumps(response))
```

---

## Implemented Services

### C Service Status

**Completion**: ✓ 100% (Phase 1)

**Components**:
- Lexer: 350+ lines, 40+ token types
- Parser: 500+ lines, operator precedence
- Type System: Static C types
- Compiler: 4-phase pipeline
- Tests: 58 tests, 100% passing

**Features**:
- Function definitions and calls
- Arithmetic and logical operations
- Pointer arithmetic
- Array access
- Control flow (if, loops)

### Python Service Status

**Completion**: ✓ 100% (Phase 1)

**Components**:
- Lexer: 400+ lines, indentation-based
- Parser: 550+ lines, Python syntax
- Type System: Dynamic typing with inference
- Compiler: 4-phase pipeline
- Tests: 58 tests, 100% passing

**Features**:
- Function definitions
- For/while loops with break/continue
- List and dictionary literals
- Exception handling (basic)
- Dynamic type checking

### Haskell Service Status

**Completion**: ✓ 100% (Phase 1)

**Components**:
- Lexer: 400+ lines, indentation syntax
- Parser: 550+ lines, pattern matching
- Type System: Polymorphic with unification
- Compiler: 4-phase pipeline
- Tests: 68 tests, 100% passing

**Features**:
- Pattern matching
- Lambda expressions
- Let/in bindings
- Case expressions
- Guard expressions
- List and tuple operations

---

## Planned Services (Phase 2)

### IDRIS2 Service

**Week**: 10.1
**Effort**: 2,500-3,000 lines
**Tests**: 70+

**Unique Aspects**:
- Dependent types for compile-time proofs
- Type-level natural numbers and vectors
- Interactive type checking with holes
- Type erasure before execution

**Key Files**:
- idris_lexer.py (500-600 lines)
- idris_parser.py (800-900 lines)
- idris_types.py (400-500 lines)
- idris_compiler.py (800-900 lines)

### LISP Service

**Week**: 9.1
**Effort**: 1,800-2,200 lines
**Tests**: 65+

**Unique Aspects**:
- S-expression syntax
- Symbol manipulation
- Meta-programming with macros
- Homoiconicity (code as data)

**Key Files**:
- lisp_lexer.py (400-500 lines)
- lisp_parser.py (550-650 lines)
- lisp_types.py (200-250 lines)
- lisp_compiler.py (600-700 lines)

### System F Service

**Week**: 10.2
**Effort**: 2,000-2,500 lines
**Tests**: 60+

**Unique Aspects**:
- Polymorphic lambda calculus
- Rank-2 polymorphism
- Type application syntax
- Monomorphization strategy

**Key Files**:
- systemf_lexer.py (350-400 lines)
- systemf_parser.py (600-700 lines)
- systemf_types.py (300-400 lines)
- systemf_compiler.py (700-800 lines)

### Java Service

**Week**: 11.1
**Effort**: 2,200-2,800 lines
**Tests**: 70+

**Unique Aspects**:
- Object-oriented paradigm
- Class hierarchy and inheritance
- Method resolution
- Exception handling

**Key Files**:
- java_lexer.py (500-600 lines)
- java_parser.py (1000-1200 lines)
- java_types.py (350-450 lines)
- java_compiler.py (800-900 lines)

---

## Building Docker Images

### Local Build

```bash
# Build specific service
docker build -t ancient-compute-c:latest services/c
docker build -t ancient-compute-python:latest services/python
docker build -t ancient-compute-haskell:latest services/haskell

# Build all (from docker-compose)
docker-compose build --no-cache
```

### Image Sizes

| Service | Base | Compiler | Total (compressed) |
|---------|------|----------|-------------------|
| C | 1.4 GB | 100 MB | 400 MB |
| Python | 450 MB | 100 MB | 220 MB |
| Haskell | 3.2 GB | 200 MB | 900 MB |
| IDRIS2 | 2.1 GB | 150 MB | 700 MB |
| LISP | 90 MB | 250 MB | 180 MB |
| System F | 3.2 GB | 100 MB | 850 MB |
| Java | 800 MB | 50 MB | 350 MB |
| Assembly | 90 MB | 30 MB | 50 MB |

**Total**: ~12 GB uncompressed, ~3.5 GB compressed

---

## Testing Services

### Unit Tests Per Service

```bash
# C service tests
docker run --rm ancient-compute-c:latest \
  python -m pytest tests/ -v --cov

# Python service tests
docker run --rm ancient-compute-python:latest \
  python -m pytest tests/ -v --cov

# Haskell service tests
docker run --rm ancient-compute-haskell:latest \
  cabal test
```

### Integration Tests

**File**: `backend/tests/test_language_services.py`

**Test Flow**:
1. Start all services: `docker-compose up -d`
2. Submit code to backend: `POST /execute`
3. Backend routes to appropriate service
4. Verify output and timing
5. Cleanup: `docker-compose down`

### Sandbox Validation

```bash
# Verify seccomp rules are applied
docker run --rm \
  --security-opt seccomp=profile.json \
  ancient-compute-c:latest \
  /bin/sh -c "ping -c 1 8.8.8.8"
# Should fail: network access blocked

# Verify memory limits
docker run --rm \
  -m 512m \
  ancient-compute-python:latest \
  python -c "x = [0] * (1024 * 1024 * 1024)"
# Should fail: out of memory
```

---

## Common Issues and Solutions

### Issue: "Docker daemon not running"

**Cause**: Docker service not started

**Solution**:
```bash
# Linux
sudo systemctl start docker

# macOS
open /Applications/Docker.app

# Windows
Start-Service Docker
```

### Issue: "No space left on device"

**Cause**: Docker images/containers consuming disk

**Solution**:
```bash
# Remove unused images
docker image prune -a

# Remove all stopped containers
docker container prune

# Clean build cache
docker builder prune
```

### Issue: "Permission denied while trying to connect to Docker daemon"

**Cause**: User not in docker group

**Solution**:
```bash
# Linux
sudo usermod -aG docker $USER
newgrp docker
```

---

## Performance Baselines

### Compilation Times (Babbage Target)

| Language | Lexer | Parser | Semantic | IR Gen | Code Gen | Total |
|----------|-------|--------|----------|--------|----------|-------|
| C | 5ms | 10ms | 15ms | 20ms | 10ms | 60ms |
| Python | 8ms | 12ms | 18ms | 25ms | 10ms | 73ms |
| Haskell | 6ms | 14ms | 20ms | 30ms | 12ms | 82ms |

### Execution Times

- **Simple arithmetic**: < 1ms
- **Fibonacci(20)**: 5-10ms
- **Complex recursion**: 20-50ms
- **Large data structures**: 50-100ms

---

## References

- **Docker**: https://docs.docker.com
- **Docker Compose**: https://docs.docker.com/compose
- **seccomp**: https://man7.org/linux/man-pages/man5/seccomp.5.html
- **cgroups v2**: https://man7.org/linux/man-pages/man7/cgroups.7.html

---

**End of Services Requirements**
