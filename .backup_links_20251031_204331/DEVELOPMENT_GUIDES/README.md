# DEVELOPMENT_GUIDES

**Purpose**: Guides for developers building and testing the Ancient Compute platform.

**Audience**: Backend developers, frontend developers, DevOps engineers, QA engineers

---

## What's in This Directory

Development guides covering:
- **Build and compilation**: How to build the entire project
- **Security implementation**: Security layers and sandboxing
- **Language services**: How language execution services work
- **Development workflow**: Setting up dev environment, testing, debugging
- **Implementation checklists**: Weekly checklists for Phase 1-2

---

## Files

### BUILD.md
How to build the entire project with:
- System requirements and setup
- Bazel build commands
- Make development targets
- Docker build configuration
- Cross-platform build process
- Build troubleshooting

**Read this for**: Getting the project to build on your machine.

### SECURITY_LAYERS_IMPLEMENTATION.md
Security architecture and implementation with:
- Docker container isolation (seccomp-bpf, cgroups, namespaces)
- Read-only filesystem restrictions
- Network isolation strategy
- Resource limit enforcement
- Attack surface analysis
- Penetration testing requirements

**Read this for**: Understanding security design, implementing secure features, security code review.

### LANGUAGE_SERVICE_SPECIFICATION.md
Technical specification for language execution services:
- Service interface definition
- REST API endpoints
- WebSocket protocol
- Error handling
- Resource limits per execution
- Timeout behavior
- Output capture and streaming

**Read this for**: Implementing new language service, debugging service issues, service integration.

### WEEK_1_CHECKLIST.md
Week 1 development checklist covering:
- Environment setup tasks
- Initial build verification
- Test execution
- Documentation reading
- Team onboarding

**Read this for**: First week as a contributor, initial setup verification.

### WEEK_2_IMPLEMENTATION_PLAN.md
Week 2 development plan with:
- Daily tasks (Monday-Friday)
- Code review focus areas
- Testing requirements
- Documentation updates
- Sprint planning

**Read this for**: Week 2 work planning, understanding sprint structure.

---

## Development Workflow

### Setting Up Development Environment

```bash
# 1. Clone repository
git clone https://github.com/oaich/ancient_compute.git
cd ancient_compute

# 2. Install dependencies
./scripts/setup-<platform>.sh   # Linux or Windows script

# 3. Build project
bazel build //...               # Full build

# 4. Run tests
bazel test //...                # All tests

# 5. Start dev servers
docker-compose up -d            # Start containers
cd frontend && npm run dev      # Frontend dev server
cd backend && uvicorn main:app --reload  # Backend dev server
```

### Build Targets

**Common Bazel targets**:
```bash
bazel build //frontend:app              # Build frontend bundle
bazel build //backend:api               # Build backend container
bazel build //services/python:service   # Build language service
bazel test //backend/tests:*            # Run all backend tests
```

**Common Make targets**:
```bash
make dev                                # Full dev environment
make frontend                           # Frontend only
make backend                            # Backend only
make test                               # Run tests
make docs                               # Build documentation
make clean                              # Clean build artifacts
```

### Code Review Process

All PRs require:
1. ✓ Passing tests (> 90% coverage, backend)
2. ✓ No compiler warnings (warnings = errors)
3. ✓ Code review approval from tech lead
4. ✓ Documentation updates (if applicable)
5. ✓ Security review (if touching security layers)

### Testing Requirements

**Backend tests**:
- Unit tests: Test individual functions
- Integration tests: Test API endpoints
- Security tests: Test sandbox isolation
- Performance tests: Benchmark critical paths

**Frontend tests**:
- Component tests: Test UI components
- Integration tests: Test component interactions
- E2E tests: Test full user workflows

**Language services**:
- Execution tests: Test code execution
- Security tests: Test sandbox restrictions
- Performance tests: Test execution latency
- Error handling tests: Test error messages

---

## Common Development Tasks

### Adding a New Language Service

1. **Create service directory**:
   ```bash
   mkdir -p services/new_language
   ```

2. **Implement service interface**:
   - See `LANGUAGE_SERVICE_SPECIFICATION.md`
   - Implement `/execute`, `/validate`, `/capabilities` endpoints

3. **Create Dockerfile**:
   - Copy from existing service as template
   - Install language runtime and tools
   - Set up execution environment

4. **Add tests**:
   - Create `tests/test_execution.py`
   - Test basic execution, error handling, resource limits

5. **Register in orchestrator**:
   - Update backend routing to recognize new language
   - Add to service discovery

6. **Document**:
   - Create README in service directory
   - Add to LANGUAGE_SERVICE_SPECIFICATION.md
   - Document any special requirements

### Debugging a Failing Test

1. **Identify failing test**:
   ```bash
   bazel test //backend/tests:test_language_service --test_output=all
   ```

2. **Run in debug mode**:
   ```bash
   python -m pdb tests/test_language_service.py
   ```

3. **Check logs**:
   - Backend: `journalctl -u backend -f`
   - Services: `docker logs <container>`

4. **Add debug output**:
   - Insert `print()` or logging statements
   - Re-run test
   - Remove before commit

### Updating Documentation

1. **Identify what changed**:
   - Architecture change → Update `ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md`
   - API change → Update relevant spec
   - Build change → Update `BUILD.md`

2. **Find affected cross-references**:
   - Search for old content in other files
   - Update all references

3. **Update GETTING_STARTED/ navigation**:
   - Update `DOCUMENT_FINDER.md` if new doc
   - Update `SITE_MAP.md` if directory structure changes

4. **Commit with docs**:
   ```bash
   git commit -m "Feature: Add X, update docs"
   ```

---

## Common Issues and Solutions

| Issue | Solution |
|-------|----------|
| Build fails with "command not found: bazel" | Run `./scripts/setup-<platform>.sh` |
| Docker service won't start | Check Docker daemon: `docker ps` |
| Tests fail randomly | Check for race conditions, use `--test_strategy=exclusive` |
| Language service hangs | Check timeout settings in `LANGUAGE_SERVICE_SPECIFICATION.md` |
| Link validation fails | Run `./scripts/validate-links.sh`, check for moved files |

---

## Performance Guidelines

**Target latencies**:
- API response: < 100ms (p95)
- Code execution: < 500ms (cold start) / < 100ms (warm)
- Frontend render: < 16ms per frame (60fps)

**Memory targets**:
- Backend process: < 256 MB
- Language service: < 512 MB
- Frontend bundle: < 500 KB (gzip)

**Build time targets**:
- Incremental build: < 10 seconds
- Full rebuild: < 5 minutes
- Test run: < 2 minutes

---

## Security Checklist

Before committing code:
- [ ] No hardcoded credentials
- [ ] No unsafe system calls (in service code)
- [ ] No user input passed unsanitized to exec
- [ ] Resource limits enforced
- [ ] Error messages don't leak information
- [ ] Security review completed

---

## Team Structure

**Backend Team**:
- Lead: [Assign]
- Members: [Assign]
- Focus: FastAPI, language services, orchestration

**Frontend Team**:
- Lead: [Assign]
- Members: [Assign]
- Focus: SvelteKit, Monaco Editor, visualizations

**DevOps Team**:
- Lead: [Assign]
- Members: [Assign]
- Focus: Docker, Kubernetes, CI/CD

**QA Team**:
- Lead: [Assign]
- Members: [Assign]
- Focus: Testing, security, performance

---

## Resources

- [BUILD.md](./BUILD.md) - Build instructions
- [LANGUAGE_SERVICE_SPECIFICATION.md](./LANGUAGE_SERVICE_SPECIFICATION.md) - Service API
- [../ARCHITECTURE_AND_DESIGN/SECURITY_LAYERS_IMPLEMENTATION.md](../ARCHITECTURE_AND_DESIGN/SECURITY_LAYERS_IMPLEMENTATION.md) - Security design
- [../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md) - System architecture

---

## FAQ

**Q: How do I run the dev environment?**
A: Follow "Setting Up Development Environment" section above. Usually: `./scripts/setup-<platform>.sh` then `docker-compose up -d`.

**Q: How do I add a new language?**
A: See "Adding a New Language Service" section. Template provided in `services/_template/`.

**Q: What are the compiler warning policies?**
A: All warnings must be resolved. No `-Wno-*` suppression without approval from tech lead and comment explaining why.

**Q: How do I know if my code is secure?**
A: See "Security Checklist" section. All code touching user input or execution must pass security review.

---

**Last Updated**: October 31, 2025
**Status**: Development Phase - Phase 2 Language Services Ready
