# Ancient Compute: Technical Architecture Document

## Executive Summary

Ancient Compute is a comprehensive educational webapp designed to teach 12,500+ years of computational history through interactive modules, multi-language programming examples, and rigorous documentation. This architecture prioritizes cross-platform compatibility, extensibility, and pedagogical effectiveness while maintaining technical correctness across diverse programming paradigms.

**See the original full document in the root directory at: `ARCHITECTURE.md`**

This summary provides key architectural decisions. For complete technical details, reference:
- `PROJECT_STRUCTURE.md` - Directory organization
- `BACKEND_ARCHITECTURE.md` - FastAPI backend specifics
- `FRONTEND_ARCHITECTURE.md` - SvelteKit frontend specifics
- `DATABASE_SCHEMA.md` - Data model
- `API_SPECIFICATION.md` - REST/WebSocket endpoints
- `SECURITY_ARCHITECTURE.md` - Security and sandboxing

## 1. Overall Technical Architecture

### 1.1 Core Technology Stack

#### Frontend Framework: SvelteKit
- Compiler-based approach minimizes runtime overhead
- Native TypeScript support for type safety
- Server-side rendering (SSR) for performance and SEO
- File-based routing aligns with educational module structure
- Excellent build-time optimizations
- WebAssembly integration support for language runtimes

#### Backend: FastAPI (Python)
- Async support for handling concurrent compilation/execution requests
- Pydantic for runtime type validation
- OpenAPI documentation generation
- Native integration with scientific Python ecosystem
- Easy integration with language toolchains via subprocess
- WebSocket support for real-time code execution

#### Database: SQLite + Redis
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

- **C**: GCC 13+ with -Wall -Wextra -Werror, seccomp-bpf sandboxing
- **Python**: CPython 3.12+ with mypy strict type checking
- **Haskell**: GHC 9.8+ with Stack for reproducible builds
- **IDRIS2**: Idris 2.0+ with proof obligation tracking
- **LISP**: SBCL with macro expansion visualization
- **Assembly**: x86-64/ARM with QEMU emulation
- **Java**: OpenJDK 21 with incremental Gradle builds
- **System F**: Custom interpreter with type inference visualization

### 2.3 Execution Sandbox Architecture

1. Docker container per language service
2. Linux namespaces (PID, Network, Mount, IPC)
3. Seccomp-bpf syscall filtering
4. Resource limits (CPU, Memory, Disk I/O)
5. Read-only filesystem with tmpfs for work
6. Network isolation except for localhost API

## 3. LaTeX Documentation System

### 3.1 Document Generation Pipeline

```
Markdown + Metadata -> Pandoc -> LaTeX Template -> XeLaTeX -> PDF
                          |
                          v
                    TikZ/PGFPlots Integration
```

### 3.2 LaTeX Infrastructure

**Primary Engine**: XeLaTeX with full Unicode support

**Diagram Systems**:
- TikZ: Circuit diagrams, automata, trees
- pgfplots: Mathematical function plots
- CircuiTikZ: Electronic circuit diagrams
- tikz-cd: Category theory diagrams
- Forest: Tree structures for parsing

### 3.3 Content Processing Pipeline

1. **Source Format**: Extended Markdown with custom directives
2. **Metadata**: YAML frontmatter for categorization
3. **Code Blocks**: Language-aware with execution capability
4. **Math**: Native LaTeX math mode support
5. **Diagrams**: Inline TikZ code blocks
6. **Cross-references**: Automatic theorem/figure numbering

## 4. Cross-Platform Build System

### 4.1 Build Orchestration

**Primary Build Tool: Bazel**
- Hermetic builds ensure reproducibility
- Language-agnostic with excellent polyglot support
- Incremental compilation and caching
- Remote build execution capability
- Platform-specific configurations

### 4.2 Platform-Specific Configurations

**Windows**: MSYS2 environment with MinGW-w64
**Debian**: Native toolchain with systemd service definitions

### 4.3 Dependency Management

- C: vcpkg (cross-platform)
- Python: Poetry with lock files
- Haskell: Stack resolver snapshots
- JavaScript: pnpm with workspace support
- LaTeX: TeX Live with tlmgr

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
│   ├── logic/
│   ├── computation/
│   ├── types/
│   └── paradigms/
├── languages/
│   └── [language]/
└── projects/
```

### 5.2 Content Metadata Schema

Modules include:
- ID, title, era classification
- Prerequisites and difficulty level
- Estimated time to completion
- Topics covered and languages used
- Assessments and resources

### 5.3 Version Control Strategy

- Git with conventional commits
- Semantic versioning for releases
- Content versioning separate from code
- Binary assets in Git LFS
- Automated changelog generation

## 6. Interactive Component Architecture

### 6.1 Code Editor Integration

- Monaco Editor with language-specific syntax highlighting
- IntelliSense via Language Server Protocol
- Multi-cursor editing and diff view
- Custom themes for different eras

### 6.2 REPL Architecture

Sandboxed execution environment with:
- Code sanitization and compilation
- Environment state tracking
- Result formatting and visualization
- Error handling and timeouts

### 6.3 Visualization Components

**D3.js Visualizations**: Directed graphs, parse trees, ASTs, memory layouts, execution traces, timelines

**Three.js 3D**: Turing machine simulation, register machines, lambda calculus, circuits, ancient devices

### 6.4 Exercise System

- Multiple exercise types: coding, proof, multiple-choice, diagram
- Hint system with progressive disclosure
- Solution validation with test cases
- Property-based testing support

## 7. Testing and Validation Strategy

### 7.1 Test Architecture Layers

- **Unit Tests**: Property-based testing per language
- **Integration Tests**: API contract, language service, database, build reproducibility
- **End-to-End Tests**: Playwright browser automation, user journey validation

### 7.2 Code Example Validation

Comprehensive validation including:
1. Syntax validation
2. Type checking (if applicable)
3. Execution validation
4. Output comparison
5. Memory safety checking (for C/C++)

### 7.3 Continuous Validation

Automated testing on push/PR with matrix strategy per language

## 8. Deployment and Development Workflow

### 8.1 Development Environment

- Local Docker Compose with all language services
- Hot reload for frontend and backend
- LaTeX document compilation watch mode
- VS Code development container configuration

### 8.2 Deployment Architecture

**Local**: Single binary with embedded resources
**Cloud**: Kubernetes with Helm charts, horizontal pod autoscaling

### 8.3 CI/CD Pipeline

1. Lint and Format (prettier, black, ormolu, clang-format)
2. Build (Bazel, Docker, LaTeX)
3. Test (unit, integration, code examples)
4. Security Scan (dependency scanning, SAST, container scan)
5. Deploy (staging, smoke tests, production canary)
6. Monitor (Prometheus, Grafana, Sentry)

### 8.4 Release Management

**Version Format**: MAJOR.MINOR.PATCH-PRERELEASE
**Release Cadence**:
- Patch: As needed for critical fixes
- Minor: Monthly with new content
- Major: Annually with architectural updates

## 9. Performance Considerations

### 9.1 Frontend Optimization
- Code splitting by route
- Lazy loading for language services
- WebAssembly for compute-intensive visualizations
- Service Worker for offline capability
- IndexedDB for local progress storage

### 9.2 Backend Optimization
- Redis caching for compilation results
- Connection pooling for database
- Async/await for I/O operations
- Rate limiting per user session
- CDN for static assets

### 9.3 Build Optimization
- Bazel remote caching
- Incremental compilation
- Parallel test execution
- Docker layer caching

## 10. Security Architecture

### 10.1 Code Execution Security
- Mandatory sandboxing for all user code
- Resource limits (CPU, memory, disk, network)
- System call filtering with seccomp-bpf
- Read-only filesystem with tmpfs work directories
- No network access from sandboxes

### 10.2 Application Security
- Content Security Policy headers
- CORS configuration
- Rate limiting
- Input validation
- SQL injection prevention via prepared statements

### 10.3 Authentication and Authorization
- JWT tokens for session management
- OAuth2 for external authentication
- Role-based access control
- API key management for services

## 11. Scalability Considerations

### 11.1 Horizontal Scaling
- Stateless application servers
- Load balancing with health checks
- Database read replicas
- Caching layer expansion
- CDN for global distribution

### 11.2 Vertical Scaling
- Language service resource allocation
- Database connection limits
- Memory management for compilation
- LaTeX compilation queuing

## 12. Monitoring and Observability

### 12.1 Metrics Collection
- Request latency (p50, p95, p99)
- Compilation success rate
- Language service availability
- User progression tracking
- Error rates by module

### 12.2 Logging Strategy
- Structured logging (JSON)
- Centralized log aggregation
- Log levels: ERROR, WARN, INFO, DEBUG
- Correlation IDs for request tracing
- Audit logging for code execution

## 13. Accessibility and Internationalization

### 13.1 Accessibility (a11y)
- WCAG 2.1 Level AA compliance
- Screen reader support
- Keyboard navigation
- High contrast themes
- Alternative text for diagrams

### 13.2 Internationalization (i18n)
- Content translation framework
- RTL language support
- Date/time localization
- Number formatting
- Cultural adaptation for examples

## 14. Future Extensibility

### 14.1 Plugin Architecture
Support for adding new languages via standardized plugin interface with compiler, runtime, and optional visualizer.

### 14.2 Content Contribution
- Git-based workflow
- Markdown with frontmatter
- Automated validation
- Peer review process
- Attribution system

### 14.3 Research Integration
- Academic paper references
- Interactive proof assistants
- Formal verification tools
- Historical artifact modeling
- Archaeological data integration

## Conclusion

This architecture provides a robust foundation for building a comprehensive educational platform covering the complete history of computation while prioritizing:

1. **Pedagogical Effectiveness**: Progressive learning paths with interactive examples
2. **Technical Correctness**: Rigorous validation and type safety across languages
3. **Scalability**: Horizontal scaling and caching strategies
4. **Maintainability**: Clear separation of concerns and automated testing
5. **Extensibility**: Plugin architecture for new languages and content
6. **Cross-Platform Support**: Native support for Windows and Linux
7. **Security**: Comprehensive sandboxing and resource isolation

---

**Related Documents**:
- [PROJECT_STRUCTURE.md](PROJECT_STRUCTURE.md) - Directory organization
- [IMPLEMENTATION_ROADMAP.md](IMPLEMENTATION_ROADMAP.md) - 52-week development plan
- [WEEK_1_COMPLETION_STATUS.md](WEEK_1_COMPLETION_STATUS.md) - Current progress
- [BACKEND_ARCHITECTURE.md](BACKEND_ARCHITECTURE.md) - FastAPI details
- [FRONTEND_ARCHITECTURE.md](FRONTEND_ARCHITECTURE.md) - SvelteKit details
- [SECURITY_ARCHITECTURE.md](SECURITY_ARCHITECTURE.md) - Security model

**Last Updated**: October 31, 2025
