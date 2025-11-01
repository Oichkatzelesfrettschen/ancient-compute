# Language Service Interface Specification

## Abstract

This document defines the standardized interface for language services in the Ancient Compute platform. Each language service must implement this interface to ensure consistent behavior across all supported programming languages and paradigms.

## 1. Service Architecture

### 1.1 Base Service Interface

```python
from abc import ABC, abstractmethod
from typing import Optional, Dict, Any, List
from dataclasses import dataclass
from enum import Enum

class ExecutionStatus(Enum):
    SUCCESS = "success"
    COMPILATION_ERROR = "compilation_error"
    RUNTIME_ERROR = "runtime_error"
    TIMEOUT = "timeout"
    MEMORY_EXCEEDED = "memory_exceeded"
    SECURITY_VIOLATION = "security_violation"

@dataclass
class CompilationResult:
    success: bool
    binary_path: Optional[str]
    errors: List[str]
    warnings: List[str]
    metadata: Dict[str, Any]

@dataclass
class ExecutionResult:
    status: ExecutionStatus
    stdout: str
    stderr: str
    exit_code: int
    execution_time_ms: float
    memory_used_bytes: int
    metadata: Dict[str, Any]

@dataclass
class AnalysisResult:
    syntax_valid: bool
    type_errors: List[str]
    warnings: List[str]
    suggestions: List[str]
    ast: Optional[Dict[str, Any]]
    type_derivation: Optional[Dict[str, Any]]
    metadata: Dict[str, Any]

class LanguageService(ABC):
    """Base interface for all language services"""

    @abstractmethod
    async def compile(
        self,
        source_code: str,
        optimization_level: int = 0,
        options: Optional[Dict[str, Any]] = None
    ) -> CompilationResult:
        """Compile source code to executable format"""
        pass

    @abstractmethod
    async def execute(
        self,
        executable: str,
        input_data: Optional[str] = None,
        timeout_ms: int = 5000,
        memory_limit_bytes: int = 268435456  # 256MB
    ) -> ExecutionResult:
        """Execute compiled code in sandboxed environment"""
        pass

    @abstractmethod
    async def analyze(
        self,
        source_code: str,
        include_ast: bool = False,
        include_types: bool = False
    ) -> AnalysisResult:
        """Perform static analysis on source code"""
        pass

    @abstractmethod
    async def format(
        self,
        source_code: str,
        style: Optional[str] = None
    ) -> str:
        """Format source code according to language conventions"""
        pass

    @abstractmethod
    async def get_capabilities(self) -> Dict[str, Any]:
        """Return service capabilities and metadata"""
        pass
```

## 2. Language-Specific Implementations

### 2.1 C Language Service

```yaml
Service: ancient-compute/lang-c
Compiler: GCC 13.2.0
Flags:
  Base: -Wall -Wextra -Werror -pedantic -std=c11
  Security: -fstack-protector-strong -D_FORTIFY_SOURCE=2
  Debug: -g -O0 -fsanitize=address,undefined
  Release: -O3 -march=native -flto

Capabilities:
  - Memory profiling via Valgrind
  - Static analysis via cppcheck
  - Undefined behavior detection
  - Assembly output generation
  - Link-time optimization

Sandbox:
  - Seccomp-bpf syscall filtering
  - Namespace isolation (PID, NET, MNT)
  - Resource limits via cgroups v2
  - Read-only root filesystem
  - Temporary work directory in tmpfs
```

### 2.2 Python Service

```yaml
Service: ancient-compute/lang-python
Runtime: CPython 3.12.0
Type Checker: mypy 1.8.0
Linter: ruff 0.1.0

Execution Modes:
  - Standard: Direct execution with RestrictedPython
  - Typed: Type checking before execution
  - Interactive: REPL-style evaluation
  - Notebook: Jupyter kernel integration

Security:
  - RestrictedPython AST transformation
  - Import restrictions
  - Builtin function filtering
  - Resource monitoring via resource module
  - Filesystem access restrictions
```

### 2.3 Haskell Service

```yaml
Service: ancient-compute/lang-haskell
Compiler: GHC 9.8.1
Build Tool: Stack LTS 22.0
Extensions:
  - DataKinds
  - GADTs
  - TypeFamilies
  - RankNTypes
  - LinearTypes

Features:
  - Type-level programming examples
  - Property-based testing via QuickCheck
  - Profiling with cost centers
  - Core language output
  - STG intermediate representation

Type Information:
  - Inferred types for all expressions
  - Kind signatures
  - Type family reduction steps
  - Constraint solving visualization
```

### 2.4 IDRIS2 Service

```yaml
Service: ancient-compute/lang-idris2
Compiler: Idris 2.0.0
Backend: Chez Scheme

Proof Features:
  - Totality checking
  - Coverage checking
  - Termination checking
  - Proof obligation tracking
  - Interactive proof development

Type Features:
  - Dependent types
  - Linear types
  - Quantitative types
  - Erasure analysis
  - Implicit arguments

Output Modes:
  - Executable generation
  - Proof term extraction
  - JavaScript compilation
  - Scheme compilation
```

### 2.5 LISP Service

```yaml
Service: ancient-compute/lang-lisp
Implementation: SBCL 2.4.0
Protocol: SLIME/SWANK

Features:
  - Macro expansion visualization
  - S-expression tree display
  - Incremental compilation
  - Image-based development
  - Condition system demonstration

Special Capabilities:
  - Step-by-step evaluation
  - Macro debugging
  - CLOS method dispatch visualization
  - Compiler macro expansion
  - Type inference display
```

### 2.6 Assembly Service

```yaml
Service: ancient-compute/lang-asm
Architectures:
  - x86_64 (Intel syntax)
  - ARM64 (AArch64)
  - RISC-V (RV64I)
  - MIPS (MIPS32)

Assemblers:
  x86_64: NASM 2.16
  ARM64: GNU AS
  RISC-V: GNU AS
  MIPS: GNU AS

Emulation: QEMU user-mode
Debugger: GDB with Python API

Features:
  - Register state tracking
  - Memory visualization
  - Instruction stepping
  - Syscall tracing
  - Performance counter access
```

### 2.7 Java Service

```yaml
Service: ancient-compute/lang-java
JDK: OpenJDK 21 (Temurin)
Build: Gradle 8.5
Testing: JUnit 5.10

Features:
  - Bytecode visualization (javap)
  - JVM flag experimentation
  - Memory model demonstration
  - Garbage collection visualization
  - JIT compilation insights

Security:
  - Security manager policies
  - Class loader restrictions
  - Reflection limitations
  - Resource consumption limits
  - Network access blocking
```

### 2.8 System F Service

```yaml
Service: ancient-compute/lang-systemf
Implementation: Custom interpreter in Haskell

Features:
  - Type abstraction and application
  - Parametric polymorphism
  - Church encodings
  - Type inference (where decidable)
  - Reduction strategy selection

Evaluation Strategies:
  - Call-by-value
  - Call-by-name
  - Call-by-need (lazy)
  - Normal order
  - Applicative order

Visualization:
  - Type derivation trees
  - Reduction sequences
  - Alpha-equivalence checking
  - Beta-reduction steps
  - Eta-conversion display
```

## 3. API Endpoints

### 3.1 REST API Structure

```
POST /api/v1/languages/{language}/compile
POST /api/v1/languages/{language}/execute
POST /api/v1/languages/{language}/analyze
POST /api/v1/languages/{language}/format
GET  /api/v1/languages/{language}/capabilities
GET  /api/v1/languages/{language}/examples
GET  /api/v1/languages/{language}/documentation
```

### 3.2 WebSocket API for Interactive Sessions

```
/ws/v1/languages/{language}/repl

Messages:
  -> {"type": "evaluate", "code": "...", "session_id": "..."}
  <- {"type": "output", "stdout": "...", "stderr": "..."}
  <- {"type": "result", "value": "...", "type": "..."}
  <- {"type": "error", "message": "...", "traceback": "..."}
```

### 3.3 Request/Response Schemas

```typescript
interface CompileRequest {
  source_code: string;
  optimization_level?: number;
  target?: string;
  flags?: string[];
  libraries?: string[];
}

interface CompileResponse {
  success: boolean;
  executable_id?: string;
  errors: CompilationError[];
  warnings: CompilationWarning[];
  statistics: {
    compilation_time_ms: number;
    binary_size_bytes?: number;
    symbols?: number;
  };
}

interface ExecuteRequest {
  executable_id?: string;
  source_code?: string;
  input?: string;
  arguments?: string[];
  environment?: Record<string, string>;
  timeout_ms?: number;
  memory_limit_mb?: number;
}

interface ExecuteResponse {
  status: ExecutionStatus;
  stdout: string;
  stderr: string;
  exit_code: number;
  statistics: {
    execution_time_ms: number;
    memory_peak_bytes: number;
    cpu_time_ms: number;
    syscalls?: number;
  };
}
```

## 4. Security Specifications

### 4.1 Sandbox Requirements

```yaml
Mandatory Isolation:
  - Process isolation via namespaces
  - Network namespace with no external access
  - Filesystem isolation with read-only root
  - Limited syscall access via seccomp-bpf
  - Resource limits enforced by cgroups

Syscall Whitelist (Common):
  - read, write (to specific FDs only)
  - mmap, munmap (with restrictions)
  - brk, sbrk (heap allocation)
  - exit, exit_group
  - clock_gettime
  - gettimeofday

Forbidden Operations:
  - Network operations (socket, connect, bind)
  - Process creation (fork, exec)
  - Filesystem modification (open with O_CREAT, mkdir)
  - Module loading (init_module)
  - Privilege escalation (setuid, setgid)
```

### 4.2 Resource Limits

```yaml
Default Limits:
  CPU Time: 5 seconds
  Wall Time: 10 seconds
  Memory: 256 MB
  Disk Write: 10 MB
  Process Count: 1
  File Descriptors: 64
  Stack Size: 8 MB

Adjustable Limits (Per Language):
  C:
    Memory: up to 512 MB
    Stack: up to 16 MB
  Python:
    Memory: up to 1 GB
    Process Count: up to 4 (for multiprocessing)
  Haskell:
    Memory: up to 1 GB
    Stack: up to 32 MB
  Java:
    Memory: up to 2 GB
    Threads: up to 10
```

## 5. Performance Requirements

### 5.1 Latency Targets

```yaml
Compilation:
  Simple Program (<100 lines): < 500ms
  Medium Program (100-1000 lines): < 2s
  Large Program (>1000 lines): < 10s

Execution:
  Startup Overhead: < 50ms
  Simple Program: < 100ms
  Interactive Response: < 200ms

Analysis:
  Syntax Check: < 100ms
  Type Checking: < 500ms
  Full Analysis: < 2s
```

### 5.2 Throughput Targets

```yaml
Concurrent Requests:
  Per Service: 50 requests/second
  Per Language: 10 compilations/second
  Total System: 200 requests/second

Resource Utilization:
  CPU: < 80% average
  Memory: < 4GB per service
  Disk I/O: < 100 MB/s
```

## 6. Monitoring and Observability

### 6.1 Metrics

```yaml
Service Metrics:
  - Request rate
  - Response time (p50, p95, p99)
  - Error rate
  - Queue depth
  - Active connections

Compilation Metrics:
  - Success rate
  - Average compilation time
  - Error types distribution
  - Cache hit rate

Execution Metrics:
  - Completion rate
  - Timeout rate
  - Memory violation rate
  - Average execution time

Resource Metrics:
  - CPU utilization
  - Memory usage
  - Disk I/O
  - Network traffic
  - Container restarts
```

### 6.2 Logging

```yaml
Log Levels:
  ERROR: Compilation failures, runtime errors, security violations
  WARN: Resource limits approached, deprecated features
  INFO: Request handling, compilation success, execution complete
  DEBUG: Detailed compilation flags, sandbox setup, cleanup

Structured Logging Format:
  {
    "timestamp": "2024-01-15T10:30:00Z",
    "level": "INFO",
    "service": "lang-c",
    "request_id": "uuid",
    "user_id": "uuid",
    "action": "compile",
    "duration_ms": 234,
    "status": "success",
    "metadata": {}
  }
```

## 7. Testing Requirements

### 7.1 Unit Tests

```yaml
Coverage Requirements:
  - Core functionality: 90%
  - Error handling: 95%
  - Security features: 100%

Test Categories:
  - Compilation success cases
  - Compilation failure cases
  - Execution with various inputs
  - Resource limit enforcement
  - Security violation detection
  - Timeout handling
```

### 7.2 Integration Tests

```yaml
Scenarios:
  - Multi-file compilation
  - Library linking
  - Cross-language interaction
  - Long-running programs
  - Memory-intensive programs
  - Concurrent executions
```

### 7.3 Conformance Tests

```yaml
Language Standards:
  C: C11 standard compliance
  Python: CPython behavior
  Haskell: Haskell 2010 + extensions
  Java: Java 21 specification

Test Suites:
  - Language specification tests
  - Compiler test suites
  - Community test suites
  - Custom edge cases
```

## 8. Error Handling

### 8.1 Error Categories

```yaml
Compilation Errors:
  - Syntax errors with line/column
  - Type errors with explanation
  - Linker errors with symbols
  - Resource errors (out of memory)

Runtime Errors:
  - Segmentation faults with backtrace
  - Null pointer dereferences
  - Array bounds violations
  - Stack overflows
  - Arithmetic exceptions

Security Errors:
  - Forbidden syscall attempts
  - Resource limit violations
  - Filesystem access violations
  - Network access attempts
```

### 8.2 Error Response Format

```json
{
  "error": {
    "type": "compilation_error",
    "code": "E0001",
    "message": "Undefined reference to 'foo'",
    "details": {
      "file": "main.c",
      "line": 42,
      "column": 15,
      "suggestion": "Did you mean 'foo_bar'?",
      "documentation": "/docs/errors/E0001"
    }
  }
}
```

## 9. Caching Strategy

### 9.1 Compilation Cache

```yaml
Cache Key: SHA256(source_code + compiler_flags + compiler_version)
Cache Duration: 24 hours
Cache Size: 1 GB per language
Eviction: LRU

Cached Data:
  - Compiled binary
  - Compilation warnings
  - Metadata (time, size)
```

### 9.2 Analysis Cache

```yaml
Cache Key: SHA256(source_code + analysis_options)
Cache Duration: 1 hour
Cache Size: 500 MB per language

Cached Data:
  - AST representation
  - Type information
  - Error messages
  - Suggestions
```

## 10. Service Discovery and Health Checks

### 10.1 Service Registration

```yaml
Registration Endpoint: POST /api/v1/registry/register
Heartbeat Interval: 10 seconds
Deregistration: DELETE /api/v1/registry/deregister

Registration Data:
  {
    "service": "lang-c",
    "version": "1.0.0",
    "endpoint": "http://lang-c:8080",
    "capabilities": {...},
    "status": "healthy"
  }
```

### 10.2 Health Checks

```yaml
Health Endpoint: GET /health
Liveness Probe: Every 5 seconds
Readiness Probe: Every 10 seconds

Health Response:
  {
    "status": "healthy",
    "version": "1.0.0",
    "uptime_seconds": 3600,
    "requests_handled": 1234,
    "active_connections": 5,
    "queue_depth": 2
  }
```

## Conclusion

This specification defines a comprehensive interface for language services in the Ancient Compute platform. Each service must implement these interfaces while maintaining:

1. **Security**: Robust sandboxing and resource isolation
2. **Performance**: Meeting latency and throughput targets
3. **Reliability**: Proper error handling and recovery
4. **Observability**: Comprehensive monitoring and logging
5. **Compatibility**: Adherence to language standards
6. **Extensibility**: Support for future enhancements

The standardized interface ensures consistent behavior across all languages while allowing language-specific optimizations and features.
