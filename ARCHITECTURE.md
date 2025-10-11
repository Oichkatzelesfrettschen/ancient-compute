# Ancient Compute: Technical Architecture Document

## Executive Summary

Ancient Compute is a comprehensive educational webapp designed to teach 12,500+ years of computational history through interactive modules, multi-language programming examples, and rigorous documentation. This architecture prioritizes cross-platform compatibility, extensibility, and pedagogical effectiveness while maintaining technical correctness across diverse programming paradigms.

## 1. Overall Technical Architecture

### 1.1 Core Technology Stack

#### Frontend Framework: SvelteKit
**Rationale:**
- Compiler-based approach minimizes runtime overhead
- Native TypeScript support for type safety
- Server-side rendering (SSR) for performance and SEO
- File-based routing aligns with educational module structure
- Excellent build-time optimizations
- WebAssembly integration support for language runtimes

#### Backend: FastAPI (Python)
**Rationale:**
- Async support for handling concurrent compilation/execution requests
- Pydantic for runtime type validation
- OpenAPI documentation generation
- Native integration with scientific Python ecosystem
- Easy integration with language toolchains via subprocess
- WebSocket support for real-time code execution

#### Database: SQLite + Redis
**Rationale:**
- SQLite: Self-contained, zero-configuration for local deployment
- Redis: Session management and compilation result caching
- No external database server requirements
- Full-text search capabilities in SQLite
- Portable across platforms

### 1.2 System Architecture Layers

```
+-----------------------------------------------------------+
|                    Presentation Layer                     |
|  SvelteKit Frontend + Monaco Editor + D3.js/Three.js     |
+-----------------------------------------------------------+
|                    Application Layer                      |
|   FastAPI + WebSocket Handler + Language Service Proxy    |
+-----------------------------------------------------------+
|                    Service Layer                          |
| Compiler Services | LaTeX Engine | Content Management     |
+-----------------------------------------------------------+
|                    Data Layer                            |
|     SQLite (Content) | Redis (Cache) | File System       |
+-----------------------------------------------------------+
|                    Infrastructure Layer                   |
|   Docker Containers | Language Runtimes | Build Tools    |
+-----------------------------------------------------------+
```

## 2. Multi-Language Integration Architecture

### 2.1 Language Service Architecture

Each language gets its own isolated service container with standardized interfaces:

```
Language Service Interface:
- /compile - Compile source code
- /execute - Execute compiled code
- /analyze - Static analysis and type checking
- /format - Code formatting
- /validate - Syntax validation
```

### 2.2 Language-Specific Implementations

#### C Language Service
- Compiler: GCC 13+ with -Wall -Wextra -Werror -pedantic
- Static Analysis: cppcheck, valgrind
- Sandbox: seccomp-bpf for syscall filtering
- Memory limit enforcement via cgroups

#### Python Service
- Runtime: CPython 3.12+ with isolated venv
- Type Checking: mypy --strict
- Linting: ruff
- Sandbox: RestrictedPython for safe execution

#### Haskell Service
- Compiler: GHC 9.8+ with -Wall -Werror
- Build Tool: Stack for reproducible builds
- Type-level computation examples via DataKinds
- QuickCheck integration for property testing

#### IDRIS2 Service
- Compiler: Idris 2.0+ latest
- Interactive editing support via LSP
- Proof obligation tracking
- Totality checking visualization

#### LISP Service
- Implementation: SBCL for performance
- Macro expansion visualization
- S-expression parser for AST display
- SLIME protocol for interactive development

#### Assembly Service
- Multiple architectures: x86-64, ARM, RISC-V
- Emulation via QEMU user mode
- Instruction-level debugging
- Register/memory visualization

#### Java Service
- JDK: OpenJDK 21 (latest LTS)
- Build: Gradle with incremental compilation
- JVM bytecode viewer
- Security manager for sandboxing

#### System F Service
- Custom interpreter implementation
- Type inference visualization
- Church encodings demonstration
- Normalization step tracking

### 2.3 Execution Sandbox Architecture

```
Container Isolation Stack:
1. Docker container per language service
2. Linux namespaces (PID, Network, Mount, IPC)
3. Seccomp-bpf syscall filtering
4. Resource limits (CPU, Memory, Disk I/O)
5. Read-only filesystem with tmpfs for work
6. Network isolation except for localhost API
```

## 3. LaTeX Documentation System

### 3.1 Document Generation Pipeline

```
Markdown + Metadata -> Pandoc -> LaTeX Template -> XeLaTeX -> PDF
                          |
                          v
                    TikZ/PGFPlots Integration
```

### 3.2 LaTeX Infrastructure

#### Primary Engine: XeLaTeX
- Full Unicode support for historical scripts
- Advanced font handling via fontspec
- Direct PDF generation

#### Diagram Systems:
- **TikZ**: Circuit diagrams, automata, trees
- **pgfplots**: Mathematical function plots
- **CircuiTikZ**: Electronic circuit diagrams
- **tikz-cd**: Category theory diagrams
- **Forest**: Tree structures for parsing

#### Template Architecture:
```latex
\documentclass{ancientcompute}  % Custom class
Packages:
- hyperref (navigation)
- listings (code highlighting)
- algorithm2e (algorithm presentation)
- amsthm (theorem environments)
- babel (multi-language support)
```

### 3.3 Content Processing Pipeline

1. **Source Format**: Extended Markdown with custom directives
2. **Metadata**: YAML frontmatter for categorization
3. **Code Blocks**: Language-aware with execution capability
4. **Math**: Native LaTeX math mode support
5. **Diagrams**: Inline TikZ code blocks
6. **Cross-references**: Automatic theorem/figure numbering

## 4. Cross-Platform Build System

### 4.1 Build Orchestration

#### Primary Build Tool: Bazel
**Rationale:**
- Hermetic builds ensure reproducibility
- Language-agnostic with excellent polyglot support
- Incremental compilation and caching
- Remote build execution capability
- Platform-specific configurations

### 4.2 Platform-Specific Configurations

#### Windows Build Chain:
```
MSYS2 Environment:
- MinGW-w64 for C compilation
- Native Python via pyenv-win
- PowerShell scripts for automation
- Windows Subsystem for Linux (WSL2) fallback
```

#### Debian Build Chain:
```
Native Toolchain:
- GCC/Clang via update-alternatives
- System Python with venv isolation
- Bash scripts for automation
- systemd service definitions
```

### 4.3 Dependency Management

```yaml
Language Dependencies:
  C: vcpkg (cross-platform)
  Python: Poetry with lock files
  Haskell: Stack resolver snapshots
  JavaScript: pnpm with workspace support
  LaTeX: TeX Live with tlmgr
  System: Nix for reproducible environments
```

## 5. Content Management Architecture

### 5.1 Content Structure

```
content/
├── eras/
│   ├── prehistoric/     # 10,500 BCE - 3,500 BCE
│   ├── ancient/         # 3,500 BCE - 500 CE
│   ├── medieval/        # 500 CE - 1,500 CE
│   ├── modern/          # 1,500 CE - 1,900 CE
│   └── contemporary/    # 1,900 CE - Present
├── concepts/
│   ├── logic/          # Boolean, predicate, modal
│   ├── computation/    # Turing machines, lambda calculus
│   ├── types/          # Simple, dependent, linear
│   └── paradigms/      # Functional, imperative, logic
├── languages/
│   └── [language]/
│       ├── syntax/
│       ├── semantics/
│       ├── examples/
│       └── exercises/
└── projects/
    └── [project-name]/
        ├── specification/
        ├── implementation/
        └── tests/
```

### 5.2 Content Metadata Schema

```typescript
interface Module {
  id: string;
  title: string;
  era: Era;
  prerequisites: string[];
  difficulty: 'novice' | 'intermediate' | 'advanced' | 'expert';
  estimatedTime: number; // minutes
  topics: Topic[];
  languages: Language[];
  assessments: Assessment[];
  resources: Resource[];
}

interface CodeExample {
  id: string;
  language: Language;
  concept: Concept;
  code: string;
  explanation: string;
  executionStrategy: 'static' | 'sandbox' | 'visual';
  expectedOutput?: string;
  memoryModel?: MemoryVisualization;
  typeDerivation?: TypeTree;
}
```

### 5.3 Version Control Strategy

- Git with conventional commits
- Semantic versioning for releases
- Content versioning separate from code
- Binary assets in Git LFS
- Automated changelog generation

## 6. Interactive Component Architecture

### 6.1 Code Editor Integration

#### Monaco Editor Configuration:
- Language-specific syntax highlighting
- IntelliSense via Language Server Protocol
- Multi-cursor editing
- Diff view for solutions
- Custom themes for different eras

### 6.2 REPL Architecture

```typescript
class REPLService {
  private websocket: WebSocket;
  private history: Command[];
  private environment: Environment;

  async evaluate(code: string): Promise<Result> {
    const sanitized = this.sandbox.sanitize(code);
    const compiled = await this.compiler.compile(sanitized);
    return this.runtime.execute(compiled, this.environment);
  }
}
```

### 6.3 Visualization Components

#### D3.js Visualizations:
- Directed graphs for automata
- Parse trees and ASTs
- Memory layout diagrams
- Execution traces
- Historical timelines

#### Three.js 3D Visualizations:
- Turing machine simulation
- Register machine operation
- Lambda calculus reduction
- Circuit simulation
- Ancient calculating devices

### 6.4 Exercise System

```typescript
interface Exercise {
  id: string;
  type: 'coding' | 'proof' | 'multiple-choice' | 'diagram';
  problem: string;
  hints: string[];
  solution: Solution;
  validator: (submission: any) => ValidationResult;
  testCases?: TestCase[];
  propertyTests?: PropertyTest[];
}
```

## 7. Documentation Generation Pipeline

### 7.1 Source to Documentation Flow

```
1. Markdown + Code Files
        |
2. Pandoc with Filters
        |
3. LaTeX Intermediate
        |
4. Bibliography + Index Generation
        |
5. XeLaTeX Compilation (3 passes)
        |
6. PDF/HTML/EPUB Output
```

### 7.2 Documentation Types

#### Whitepaper Generation:
- Academic paper template
- Bibliography management via BibLaTeX
- Index generation
- Glossary compilation

#### API Documentation:
- OpenAPI/Swagger for REST APIs
- TypeDoc for TypeScript
- Doxygen for C/C++
- Haddock for Haskell
- Sphinx for Python

#### Tutorial Documentation:
- Progressive disclosure
- Embedded exercises
- Solution reveals
- Progress tracking

### 7.3 Automation Pipeline

```makefile
DOCS_SRC := $(wildcard docs/*.md)
DOCS_PDF := $(DOCS_SRC:.md=.pdf)

%.pdf: %.md
	pandoc $< \
	  --filter pandoc-crossref \
	  --filter pandoc-citeproc \
	  --template=ancient-compute \
	  --pdf-engine=xelatex \
	  -o $@
```

## 8. Testing and Validation Strategy

### 8.1 Test Architecture Layers

#### Unit Tests:
- Property-based testing with Hypothesis (Python)
- QuickCheck (Haskell)
- JUnit (Java)
- Unity framework (C)

#### Integration Tests:
- API contract testing
- Language service integration
- Database migrations
- Build reproducibility

#### End-to-End Tests:
- Playwright for browser automation
- User journey validation
- Performance benchmarks
- Accessibility compliance

### 8.2 Code Example Validation

```python
class CodeValidator:
    def validate_example(self, example: CodeExample) -> ValidationResult:
        # 1. Syntax validation
        syntax_result = self.language_service.validate_syntax(
            example.code,
            example.language
        )

        # 2. Type checking (if applicable)
        if example.language.has_types:
            type_result = self.type_checker.check(example.code)

        # 3. Execution validation
        execution_result = self.sandbox.execute(
            example.code,
            timeout=5000,
            memory_limit="256MB"
        )

        # 4. Output comparison
        if example.expected_output:
            assert execution_result.stdout == example.expected_output

        # 5. Memory safety (for C/C++)
        if example.language in ['c', 'cpp']:
            valgrind_result = self.valgrind.check(example.code)

        return ValidationResult(
            syntax=syntax_result,
            types=type_result,
            execution=execution_result,
            memory=valgrind_result
        )
```

### 8.3 Continuous Validation

```yaml
name: Validate Code Examples
on: [push, pull_request]

jobs:
  validate:
    strategy:
      matrix:
        language: [c, python, haskell, idris2, lisp, java]
    steps:
      - uses: actions/checkout@v3
      - name: Setup language environment
        run: ./scripts/setup-${{ matrix.language }}.sh
      - name: Validate examples
        run: |
          bazel test //examples/${{ matrix.language }}/... \
            --test_output=errors \
            --test_tag_filters=-slow
```

## 9. Deployment and Development Workflow

### 9.1 Development Environment

#### Local Development Setup:
```bash
# Development container with all languages
docker-compose up -d

# Hot reload for frontend
pnpm run dev

# Backend with auto-reload
uvicorn app.main:app --reload

# LaTeX document compilation
make -C docs watch
```

#### VS Code Development Container:
```json
{
  "name": "Ancient Compute Dev",
  "dockerComposeFile": "docker-compose.dev.yml",
  "service": "workspace",
  "extensions": [
    "svelte.svelte-vscode",
    "ms-python.python",
    "haskell.haskell",
    "James-Yu.latex-workshop"
  ],
  "settings": {
    "files.associations": {
      "*.idr": "idris"
    }
  }
}
```

### 9.2 Deployment Architecture

#### Container Orchestration:
```yaml
services:
  frontend:
    image: ancientcompute/frontend:latest
    replicas: 2
    resources:
      limits:
        memory: 512M

  backend:
    image: ancientcompute/backend:latest
    replicas: 3
    resources:
      limits:
        memory: 1G

  language-service-c:
    image: ancientcompute/lang-c:latest
    replicas: 2
    security_opt:
      - seccomp:seccomp-profile.json

  latex-service:
    image: ancientcompute/latex:latest
    replicas: 1
    volumes:
      - texmf-cache:/var/cache/texmf
```

#### Deployment Strategies:

**Local Deployment:**
```bash
# Single binary with embedded resources
bazel build //deploy:bundle --config=release
./bazel-bin/deploy/ancient-compute-server
```

**Cloud Deployment:**
- Kubernetes with Helm charts
- Horizontal pod autoscaling
- Persistent volume claims for user data
- Ingress with TLS termination

### 9.3 CI/CD Pipeline

```yaml
Pipeline Stages:
1. Lint and Format
   - prettier (JS/TS)
   - black (Python)
   - ormolu (Haskell)
   - clang-format (C)

2. Build
   - Bazel build //...
   - Docker image creation
   - LaTeX documentation compilation

3. Test
   - Unit tests per language
   - Integration tests
   - Code example validation

4. Security Scan
   - Dependency vulnerability scanning
   - SAST with CodeQL
   - Container scanning

5. Deploy
   - Staging environment
   - Smoke tests
   - Production with canary

6. Monitor
   - Prometheus metrics
   - Grafana dashboards
   - Sentry error tracking
```

### 9.4 Release Management

```
Version Format: MAJOR.MINOR.PATCH-PRERELEASE

MAJOR: Significant architectural changes
MINOR: New historical eras or languages
PATCH: Bug fixes and content updates
PRERELEASE: beta, rc1, rc2

Release Cadence:
- Patch: As needed for critical fixes
- Minor: Monthly with new content
- Major: Annually with architectural updates
```

## 10. Performance Considerations

### 10.1 Frontend Optimization
- Code splitting by route
- Lazy loading for language services
- WebAssembly for compute-intensive visualizations
- Service Worker for offline capability
- IndexedDB for local progress storage

### 10.2 Backend Optimization
- Redis caching for compilation results
- Connection pooling for database
- Async/await for I/O operations
- Rate limiting per user session
- CDN for static assets

### 10.3 Build Optimization
- Bazel remote caching
- Incremental compilation
- Parallel test execution
- Docker layer caching
- Selective language service loading

## 11. Security Architecture

### 11.1 Code Execution Security
- Mandatory sandboxing for all user code
- Resource limits (CPU, memory, disk, network)
- System call filtering
- Read-only filesystem
- No network access from sandboxes

### 11.2 Application Security
- Content Security Policy headers
- CORS configuration
- Rate limiting
- Input validation
- SQL injection prevention via prepared statements

### 11.3 Authentication and Authorization
- JWT tokens for session management
- OAuth2 for external authentication
- Role-based access control
- API key management for services

## 12. Scalability Considerations

### 12.1 Horizontal Scaling
- Stateless application servers
- Load balancing with health checks
- Database read replicas
- Caching layer expansion
- CDN for global distribution

### 12.2 Vertical Scaling
- Language service resource allocation
- Database connection limits
- Memory management for compilation
- LaTeX compilation queuing

## 13. Monitoring and Observability

### 13.1 Metrics Collection
```
Application Metrics:
- Request latency (p50, p95, p99)
- Compilation success rate
- Language service availability
- User progression tracking
- Error rates by module

System Metrics:
- CPU/Memory usage
- Disk I/O
- Network throughput
- Container health
- Database performance
```

### 13.2 Logging Strategy
- Structured logging (JSON)
- Centralized log aggregation
- Log levels: ERROR, WARN, INFO, DEBUG
- Correlation IDs for request tracing
- Audit logging for code execution

## 14. Accessibility and Internationalization

### 14.1 Accessibility (a11y)
- WCAG 2.1 Level AA compliance
- Screen reader support
- Keyboard navigation
- High contrast themes
- Alternative text for diagrams

### 14.2 Internationalization (i18n)
- Content translation framework
- RTL language support
- Date/time localization
- Number formatting
- Cultural adaptation for examples

## 15. Future Extensibility

### 15.1 Plugin Architecture
```typescript
interface LanguagePlugin {
  name: string;
  version: string;
  compiler: CompilerInterface;
  runtime: RuntimeInterface;
  visualizer?: VisualizerInterface;
  exercises?: ExerciseInterface[];
}
```

### 15.2 Content Contribution
- Git-based workflow
- Markdown with frontmatter
- Automated validation
- Peer review process
- Attribution system

### 15.3 Research Integration
- Academic paper references
- Interactive proof assistants
- Formal verification tools
- Historical artifact modeling
- Archaeological data integration

## Conclusion

This architecture provides a robust foundation for building a comprehensive educational platform covering the complete history of computation. The design prioritizes:

1. **Pedagogical Effectiveness**: Progressive learning paths with interactive examples
2. **Technical Correctness**: Rigorous validation and type safety across languages
3. **Scalability**: Horizontal scaling and caching strategies
4. **Maintainability**: Clear separation of concerns and automated testing
5. **Extensibility**: Plugin architecture for new languages and content
6. **Cross-Platform Support**: Native support for Windows and Linux
7. **Security**: Comprehensive sandboxing and resource isolation

The system can grow from a local single-user installation to a cloud-deployed platform serving thousands of concurrent learners while maintaining performance and educational quality.