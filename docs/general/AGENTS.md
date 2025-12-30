# Ancient Compute: Custom AI Agents

**Date**: November 1, 2025
**Version**: 1.0
**Status**: Production Ready

---

## Overview

Ancient Compute utilizes five specialized AI agents to deliver a comprehensive educational platform teaching 12,500 years of computational history. These agents provide expert guidance across history, type theory, software architecture, and implementation excellence.

Each agent brings deep domain expertise while collaborating to ensure pedagogical accuracy, mathematical rigor, architectural coherence, and production-quality code.

---

## Agent System Architecture

```
┌─────────────────────────────────────────────┐
│   Multi-Agent Orchestrator (Coordinator)    │
│   Decomposes complex problems, routes to    │
│   specialists, resolves conflicts, ensures  │
│   integration across domains                │
└──────────────┬──────────────────────────────┘
               │
    ┌──────────┼──────────┬──────────┐
    │          │          │          │
    ▼          ▼          ▼          ▼
┌─────────┐ ┌──────────┐ ┌───────────┐ ┌──────────┐
│ Logic-  │ │ Polyglot │ │ Category  │ │ PhD      │
│ Compu-  │ │ Systems  │ │ Theory    │ │ Software │
│ tation  │ │ Architect│ │ Expert    │ │ Engineer │
│Historian│ │          │ │           │ │          │
└─────────┘ └──────────┘ └───────────┘ └──────────┘
```

---

## Agents

### 1. Logic-Computation-Historian

**Location**: `.github/agents/logic-computation-historian.md`

**Responsibility**: Ensures historical accuracy and pedagogical soundness across the entire curriculum.

**Core Expertise**:
- 12,500-year history of computation across all civilizations
- Cross-cultural contributions: Mesopotamian, Egyptian, Greek, Indian, Chinese, Islamic, European
- Primary source documentation and scholarly verification
- Chronological accuracy and timeline validation
- Prevention of Eurocentric or teleological narratives

**Consulted For**:
- ✅ Designing new historical modules (Module 0-7)
- ✅ Writing historical context and explanations
- ✅ Validating timeline accuracy across content
- ✅ Bridging ancient algorithmic concepts to modern implementation
- ✅ Creating synthesis modules (A, B, C)

**Key Principle**: Computation's history is not linear progress but complex cultural exchange across millennia.

**Example Task**:
> "Design Module 3 (Medieval Transmission) covering Islamic Golden Age (Al-Khwarizmi), scholastic logic, and Lull's Ars Magna. Ensure chronological accuracy, identify modern computational relevance, and provide primary source documentation."

---

### 2. Polyglot-Systems-Architect

**Location**: `.github/agents/polyglot-systems-architect.md`

**Responsibility**: Designs and optimizes multi-language execution architecture serving 8+ programming paradigms.

**Core Expertise**:
- Language service containerization (Docker)
- Cross-language API design and standardization
- Performance profiling and optimization
- Security sandboxing and resource limits
- Language interoperability and FFI patterns
- Data flow across language boundaries

**Consulted For**:
- ✅ Designing new language services (C, Python, Haskell, IDRIS2, LISP, Java, Assembly, System F)
- ✅ Creating code examples implementing same algorithm across multiple languages
- ✅ Optimizing system performance and response times
- ✅ Establishing standardized REST/WebSocket APIs
- ✅ Managing resource limits (CPU, memory, timeout)

**Coverage**:
```
C (systems programming)
Python (general-purpose, beginner-friendly)
Haskell (pure functional, parametric polymorphism)
IDRIS2 (dependent types, compile-time proofs)
LISP (metaprogramming, symbolic computation)
Java (JVM, static typing, enterprise patterns)
Assembly (x86-64, direct hardware access)
System F (lambda calculus with universal quantification)
```

**Key Principle**: Every language deserves first-class support; no language is "best" for all tasks.

**Example Task**:
> "Implement factorial algorithm in C, Python, Haskell, IDRIS2, and LISP. Highlight how type systems differ: C unsafe pointers, Haskell parametric polymorphism, IDRIS2 dependent types. Create performance comparison framework."

---

### 3. Category-Theory-Expert

**Location**: `.github/agents/category-theory-expert.md`

**Responsibility**: Provides mathematical rigor to type systems and formal verification of computational properties.

**Core Expertise**:
- Type theory: From simply-typed lambda calculus to dependent types
- Category theory: Functors, natural transformations, adjunctions, monads
- Formal verification: Proving program correctness
- Categorical laws: Verifying coherence conditions, distributive laws
- Type system design: Creating sound and complete systems
- Proof assistants: Coq, Lean, IDRIS2, Agda
- Algebraic structures: Groups, rings, fields, lattices in type systems

**Consulted For**:
- ✅ Teaching type system progression across historical eras
- ✅ Creating IDRIS2 proofs of algorithm correctness
- ✅ Verifying type safety properties
- ✅ Proving monad laws
- ✅ Ensuring dependent type system soundness
- ✅ Designing type-safe abstractions

**Type System Progression**:
1. **Untyped**: Assembly, early LISP
2. **Implicitly Typed**: Python (runtime), dynamic functional
3. **Explicitly Typed**: C, Java (manifest types)
4. **Parametrically Polymorphic**: Haskell (System F instance)
5. **Dependent Types**: IDRIS2 (dependent pairs, proofs)
6. **System F**: Lambda calculus with universal quantification
7. **Higher-Kinded Types**: Haskell type families

**Key Principle**: Mathematical rigor must connect to implementable concepts.

**Example Task**:
> "Explain System F (polymorphic lambda calculus) at three levels: mathematical (category theory), type system (inference rules), practical (Haskell implementation). Show how universal quantification enables parametric polymorphism."

---

### 4. Multi-Agent-Orchestrator

**Location**: `.github/agents/multi-agent-orchestrator.md`

**Responsibility**: Coordinates specialized agents for complex problems spanning multiple domains.

**Core Expertise**:
- Problem decomposition across domains
- Agent routing and task allocation
- Context management across specialists
- Conflict resolution between different expertise
- Integration validation across domains
- Quality assurance across all phases

**Consulted For**:
- ✅ Complex features requiring multiple specialties
- ✅ Cross-cutting concerns (performance, security, architecture)
- ✅ Major architecture changes or refactors
- ✅ Comprehensive quality assurance
- ✅ When specialists recommend conflicting approaches
- ✅ Ensuring outputs from different agents integrate coherently

**Workflow: Adding New Historical Module**:
```
1. logic-computation-historian → Module structure, timeline, concepts
2. polyglot-systems-architect → Code examples across languages
3. category-theory-expert → Formal proofs of correctness
4. phd-software-engineer → Code review, testing, quality
5. Validation → Historical accuracy, type safety, production readiness
```

**Key Principle**: Don't hide disagreements; use them to deepen analysis.

**Example Task**:
> "Adding Coq language service requires: architect (system design), expert (formal verification), engineer (implementation), historian (pedagogy). Orchestrate their expertise into coherent service with learning content."

---

### 5. PhD-Software-Engineer

**Location**: `.github/agents/phd-software-engineer.md`

**Responsibility**: Ensures production-ready code quality and architectural excellence across all systems.

**Core Expertise**:
- Software architecture and design patterns
- Performance optimization and profiling
- Multi-language programming best practices
- Type safety and static analysis
- Testing strategy and CI/CD
- Code quality and maintainability
- Research translation to production code

**Consulted For**:
- ✅ Code review before deployment
- ✅ Architecture design for complex systems
- ✅ Performance analysis and optimization
- ✅ Testing strategy and coverage targets
- ✅ Refactoring and technical debt management
- ✅ Language-specific best practices
- ✅ Production readiness validation

**Quality Standards**:
```
Testing Strategy:
  ✓ Unit tests: > 80% coverage
  ✓ Integration tests: Component interactions
  ✓ End-to-end tests: User workflows
  ✓ Performance tests: Performance targets
  ✓ Security tests: Sandboxing and isolation

Performance Standards:
  ✓ Language service startup: < 5 seconds
  ✓ Code execution response: < 10 seconds
  ✓ API response time: < 500ms (non-execution)
  ✓ Memory usage per request: < 256MB
```

**Key Principle**: Clear code beats clever code; measure before optimizing.

**Example Task**:
> "Review orchestration layer for: proper abstractions, error handling, performance bottlenecks, security vulnerabilities. Propose refactoring to separate validation, business logic, database access, and response formatting."

---

## Requirements for GitHub Website

### Documentation Structure
- ✅ Master agent index: `AGENTS.md` (this file, root level)
- ✅ Detailed agent docs: `.github/agents/` directory
- ✅ Quick reference: `.github/agents/README.md`
- ✅ Individual agent specs: `.github/agents/*-*.md` files

### Visibility & Accessibility
- **Root Level**: `AGENTS.md` visible on main GitHub page
- **Directory**: `.github/agents/` easily accessible from root
- **Navigation**: Clear links between overview and detailed docs
- **Search**: Files named clearly for GitHub search
- **Format**: Standard GitHub markdown with syntax highlighting

### Content Requirements
- ✅ Clear agent roles and responsibilities
- ✅ Specific use cases for each agent
- ✅ Example tasks demonstrating expertise
- ✅ Integration patterns showing collaboration
- ✅ When/when-not-to-consult guidance
- ✅ Contact points and routing information
- ✅ Project constraints and principles
- ✅ FAQ addressing common questions

### Technical Formatting
- ✅ Proper GitHub markdown syntax
- ✅ Code blocks with language specification
- ✅ Tables for quick reference
- ✅ ASCII diagrams for architecture
- ✅ Headers for navigation
- ✅ Links between related sections
- ✅ Version and date stamps

### Information Architecture
```
AGENTS.md (Entry point)
├── Overview
├── System architecture diagram
├── Individual agent specifications
│   ├── Role & responsibility
│   ├── Core expertise
│   ├── When to consult
│   ├── Key principles
│   └── Example tasks
├── Collaboration patterns
├── GitHub requirements (this section)
├── Communication patterns
├── FAQ
└── References

.github/agents/ (Detailed reference)
├── README.md (Master index)
├── logic-computation-historian.md
├── polyglot-systems-architect.md
├── category-theory-expert.md
├── multi-agent-orchestrator.md
└── phd-software-engineer.md
```

---

## How Agents Work Together

### Single-Domain Task
```
Task: "Implement factorial in Haskell"
→ Consult: polyglot-systems-architect directly
→ No coordination needed
```

### Multi-Domain Task
```
Task: "Add new historical module with curriculum, code examples,
        formal proofs, and production deployment"

→ Route through multi-agent-orchestrator
→ Coordinates: historian, architect, expert, engineer
→ Each specialist provides their expertise
→ Orchestrator ensures integration and coherence
```

---

## When to Consult Each Agent

### Quick Reference

| Task | Agent | Rationale |
|------|-------|-----------|
| Write lesson content | Logic-Computation-Historian | Accuracy, pedagogy, historical context |
| Design language service | Polyglot-Systems-Architect | Multi-language expertise |
| Prove algorithm correct | Category-Theory-Expert | Mathematical rigor |
| Code review | PhD-Software-Engineer | Quality, architecture |
| Complex feature | Multi-Agent-Orchestrator | Coordinating expertise |
| Timeline validation | Logic-Computation-Historian | Historical accuracy |
| API design | Polyglot-Systems-Architect | System interfaces |
| Type system design | Category-Theory-Expert | Mathematical soundness |
| Performance tuning | PhD-Software-Engineer | Optimization expertise |

---

## Getting Started with Agents

### For New Contributors

1. Read `AGENTS.md` (this file) for overview
2. Review `.github/agents/README.md` for detailed guide
3. Identify which agents are relevant for your task
4. Consult appropriate agent documentation
5. Follow agent guidance for your domain

### For Complex Features

1. Break down task into constituent domains
2. Use multi-agent-orchestrator to route
3. Consult specialists in sequence
4. Validate outputs integrate coherently
5. Document decisions and trade-offs

---

## FAQ

**Q: Why multiple agents instead of one general AI?**
A: Specialized expertise ensures depth in each domain. Polyglot systems require deep knowledge of type theory, history, software architecture, and implementation—no single generalist can match specialists.

**Q: What if agents disagree?**
A: That's valuable! Use the disagreement to deepen understanding. Orchestrator helps synthesize different perspectives.

**Q: Should I consult an agent for simple tasks?**
A: No. Use agents for complex problems requiring specialized expertise.

**Q: Can agents be wrong?**
A: Yes. Always validate advice against actual code and requirements.

**Q: How often should I consult agents?**
A: As needed. Some phases require frequent consultation; others minimal.

---

## References

- **Detailed Agent Docs**: [.github/agents/](https://github.com/Oichkatzelesfrettschen/ancient-compute/tree/master/.github/agents)
- **Master Index**: [.github/agents/README.md](https://github.com/Oichkatzelesfrettschen/ancient-compute/blob/master/.github/agents/README.md)
- **Project Overview**: [README.md](https://github.com/Oichkatzelesfrettschen/ancient-compute)
- **Architecture**: [ARCHITECTURE.md](https://github.com/Oichkatzelesfrettschen/ancient-compute/blob/master/ARCHITECTURE.md)

---

**Document Version**: 1.0
**Date**: November 1, 2025
**Status**: Production Ready
**Visibility**: Public (GitHub)

