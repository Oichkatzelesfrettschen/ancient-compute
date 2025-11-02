# Ancient Compute Custom Agents

This directory contains documentation for specialized AI agents that support development of the Ancient Compute educational platform. These agents provide deep expertise across multiple domains essential for the project.

## Project Overview

Ancient Compute is a comprehensive educational platform teaching the 12,500-year history of computation and logic, from prehistoric tally marks through modern type theory and quantum computing. The project requires expertise spanning:

- **History**: Accurate computational history across cultures and centuries
- **Type Systems**: From untyped code to dependent types, with mathematical rigor
- **Software Architecture**: Multi-language execution services, API design, performance
- **Pedagogy**: Teaching progression that respects both history and learning psychology
- **Implementation**: Production-ready code across 8+ programming languages

## Available Agents

### 1. [logic-computation-historian](./logic-computation-historian.md)

**Role**: Ensures historical accuracy and pedagogical soundness of curriculum

**Use When**:
- Designing new curriculum modules
- Writing historical context and explanations
- Validating timeline accuracy
- Bridging ancient concepts to modern implementation
- Preventing Eurocentric or false teleological narratives

**Key Expertise**:
- 12,500 years of computational and logical thought
- Cross-cultural contributions to computation
- Primary source verification
- Chronological accuracy
- Connection of historical concepts to modern programming

**Example**: Designing Module 2 (Medieval Transmission) covering Islamic algorithms and their modern relevance.

---

### 2. [polyglot-systems-architect](./polyglot-systems-architect.md)

**Role**: Designs multi-language execution system and service architecture

**Use When**:
- Creating new language services (Docker containers)
- Designing code examples across multiple languages
- Optimizing system performance
- Creating API contracts and standardized interfaces
- Implementing orchestration layer

**Key Expertise**:
- Cross-language system design
- Language service containerization
- API design across language boundaries
- Performance profiling and optimization
- Security sandboxing and isolation

**Example**: Implementing factorial in C, Python, Haskell, IDRIS2, LISP showing paradigm differences.

---

### 3. [category-theory-expert](./category-theory-expert.md)

**Role**: Provides mathematical rigor to type systems and formal verification

**Use When**:
- Teaching type system progression
- Proving algorithm correctness with IDRIS2
- Verifying type system soundness
- Explaining categorical structures in code
- Designing type-safe abstractions

**Key Expertise**:
- Type theory and lambda calculus
- Category theory and categorical logic
- Formal proof and verification
- Dependent types and type system design
- Monads, functors, and algebraic structures

**Example**: Creating IDRIS2 proofs showing medieval algorithms are correct, connecting to modern type-theoretic guarantees.

---

### 4. [multi-agent-orchestrator](./multi-agent-orchestrator.md)

**Role**: Coordinates specialized agents for complex, multi-faceted problems

**Use When**:
- Working on features requiring multiple domains
  - New module (history, types, implementation, testing)
  - Language service expansion
  - Major architecture changes
- Resolving conflicts between different domain expertise
- Ensuring outputs from different agents integrate coherently
- Complex problem decomposition

**Key Expertise**:
- Problem decomposition across domains
- Agent coordination and routing
- Context management and integration
- Conflict resolution
- Quality assurance across domains

**Example**: Adding Coq language service requires historian (pedagogy), architect (system design), expert (formal verification), engineer (implementation).

---

### 5. [phd-software-engineer](./phd-software-engineer.md)

**Role**: Ensures production-ready code quality and architectural excellence

**Use When**:
- Code review before deployment
- Architecture design for complex systems
- Performance analysis and optimization
- Testing strategy and quality assurance
- Refactoring and technical debt management
- Language-specific best practices

**Key Expertise**:
- Software architecture and design patterns
- Performance optimization and profiling
- Multi-language programming
- Testing strategy and CI/CD
- Code quality and maintainability
- Production readiness

**Example**: Profiling language service startup time and proposing caching strategy that maintains security isolation.

---

## How Agents Work Together

### Agent Collaboration Model

```
        Multi-Agent
       Orchestrator
            |
            |-- Routes to -->
            |
      +-----+-----+-----+-----+
      |     |     |     |     |
    Logic  Poly  Cate  PhD   TikZ
    Hist   glot  gory  Soft  (when
          Arch  Theo  Eng    needed)
```

### Common Workflows

#### Adding a New Historical Module

1. **logic-computation-historian**: Design module structure, timeline, concepts
2. **polyglot-systems-architect**: Create code examples in multiple languages
3. **category-theory-expert**: Develop proofs for algorithm correctness
4. **phd-software-engineer**: Review code quality, testing, production readiness
5. **Validation**: Check historical accuracy, type safety, implementation quality

#### Optimizing Language Service Performance

1. **phd-software-engineer**: Profile bottlenecks, identify optimization targets
2. **polyglot-systems-architect**: Propose architectural improvements
3. **Validation**: Ensure security isolation maintained, performance targets met

#### Resolving Design Conflicts

1. **logic-computation-historian**: "Should we teach System F or dependent types?"
2. **category-theory-expert**: "Mathematically, dependent types are more expressive"
3. **phd-software-engineer**: "Practically, System F is simpler to implement"
4. **multi-agent-orchestrator**: "Teach both with clear progression and historical context"

---

## When to Consult Each Agent

### Quick Reference

| Task | Agent | Rationale |
|------|-------|-----------|
| Write lesson content | logic-computation-historian | Accuracy, pedagogy, context |
| Design language service | polyglot-systems-architect | Multi-language expertise |
| Prove algorithm correct | category-theory-expert | Mathematical rigor |
| Code review | phd-software-engineer | Quality, architecture |
| Complex feature | multi-agent-orchestrator | Coordinated expertise |
| Timeline validation | logic-computation-historian | Historical accuracy |
| API design | polyglot-systems-architect | System interfaces |
| Type system design | category-theory-expert | Mathematical soundness |
| Performance tuning | phd-software-engineer | Optimization expertise |
| New module | multi-agent-orchestrator | All domains involved |

---

## Best Practices

### 1. Choose the Right Agent

- **Single domain**: Consult specialist directly
- **Multiple domains**: Use multi-agent-orchestrator to route
- **Conflicting advice**: Use orchestrator to synthesize resolution
- **Uncertainty**: Orchestrator can clarify which expertise needed

### 2. Provide Context

When consulting an agent:
- Describe the problem clearly
- Provide relevant project context
- State success criteria
- Mention constraints (schedule, resources, technical)
- Include relevant code/documentation references

### 3. Validate Integration

When using multiple agents:
- Check that outputs cohere
- Resolve any conflicts identified
- Ensure specialist recommendations serve overall project goals
- Document decisions and trade-offs

### 4. Iterate

- Agents provide initial guidance, not final answers
- Refine through iteration and validation
- Measure outcomes against objectives
- Adjust approach based on results

---

## Communication Patterns

### With Domain Specialists

```
"I need to [task]. Here's the context: [background].
Success looks like: [criteria]. Constraints: [limits].
What do you recommend?"
```

### With Multi-Agent Orchestrator

```
"I need to [complex task] involving [domain1] and [domain2].
Here's the constraint: [limit].
How should I decompose this? Which agents should I consult?"
```

---

## Project Constraints Agents Should Know

### Technical Constraints
- **Languages**: C, Python, Haskell, IDRIS2, LISP, Java, Assembly, System F
- **Architecture**: FastAPI backend, SvelteKit frontend, Docker language services
- **Performance**: Language startup < 5s, API response < 500ms
- **Security**: Sandboxed code execution, resource limits, no network access

### Educational Constraints
- **Audience**: Developers familiar with some languages, but not historical context
- **Progression**: Should work for complete novices to becoming experts
- **Rigor**: Historical accuracy AND technical correctness non-negotiable
- **Pedagogy**: Respect learning psychology, scaffold complexity

### Project Constraints
- **Timeline**: Phase-based 52-week rollout
- **Scope**: 7 historical modules + 3 synthesis modules
- **Quality**: > 90% test coverage, no compiler warnings
- **Documentation**: Every feature documented with code examples

---

## Contributing to Agent Documentation

If you extend or modify these agents:

1. Update the relevant .md file
2. Keep examples current with codebase
3. Document new capabilities or constraints
4. Reference project files and structures
5. Maintain consistency across all agent docs

---

## References

- Project CLAUDE.md: Overall project philosophy and structure
- Project README: High-level overview and getting started
- ARCHITECTURE.md: Technical system design
- Curriculum modules: content/modules/module-*/
- Language services: backend/services/*/
- Test suite: backend/tests/

---

## FAQ

**Q: Can I consult multiple agents for the same task?**
A: Yes, especially for complex tasks. Use multi-agent-orchestrator to coordinate.

**Q: What if agents disagree?**
A: That's valuable! Use orchestrator to synthesize disagreement into deeper understanding.

**Q: Should I use an agent for trivial tasks?**
A: No. Agents are for complex problems requiring specialized expertise. Use them judiciously.

**Q: Can agents be wrong?**
A: Yes. Always validate advice against actual code/requirements. Agents provide guidance, not gospel.

**Q: How often should I consult agents?**
A: As needed. Some phases require frequent consultation; others minimal. Let project needs guide.

---

**Last Updated**: 2025-11-01
**Agent Set Version**: 1.0 (Ancient Compute Optimized)

