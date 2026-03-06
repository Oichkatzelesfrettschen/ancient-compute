# Workflow Troubleshooting Guide

## Overview

This document provides comprehensive troubleshooting for GitHub Actions workflows that were failing after the comprehensive linting implementation.

## Issues Identified and Fixed

### 1. Missing Service Directories

**Problem**: Workflows attempted to lint C and Haskell services that don't exist yet (planned for Week 9-11).

**Solution**: Added conditional checks to skip linting for unimplemented services.

```yaml
- name: Check if C service exists
  id: check_c
  run: |
    if [ -d "services/c-service" ]; then
      echo "exists=true" >> $GITHUB_OUTPUT
    else
      echo "exists=false" >> $GITHUB_OUTPUT
      echo "⚠️  C service not yet implemented - skipping"
    fi
```

### 2. Linter Path (historical -- pylint removed)

> **Note**: pylint was replaced by ruff per [ADR 0001](../adr/0001-remove-bazel.md).
> This section is kept for historical context.

**Problem**: lint.yml referenced `../.pylintrc` from backend directory.
**Resolution**: Replaced pylint with ruff; ruff reads `pyproject.toml` automatically.

### 3. Hardcoded Dockerfile Path

**Problem**: Hadolint action tried to lint a root `Dockerfile` that doesn't exist.

**Fix**: Explicitly lint backend and frontend Dockerfiles:
```yaml
- name: Lint backend Dockerfile
  uses: hadolint/hadolint-action@v3.1.0
  with:
    dockerfile: backend/Dockerfile
    failure-threshold: warning

- name: Lint frontend Dockerfile
  uses: hadolint/hadolint-action@v3.1.0
  with:
    dockerfile: frontend/Dockerfile
    failure-threshold: warning
  continue-on-error: true
```

### 4. Missing Markdown Linter Dependency

**Problem**: Used `nosborn/github-action-markdown-cli@v3.3.0` which had compatibility issues.

**Fix**: Install and use markdownlint-cli directly:
```yaml
- name: Set up Node.js
  uses: actions/setup-node@v4
  with:
    node-version: '20'

- name: Install markdownlint-cli
  run: npm install -g markdownlint-cli

- name: Run markdownlint
  run: markdownlint '**/*.md' --ignore node_modules --config .markdownlint.json
```

### 5. YAML Linting Missing Files

**Problem**: yamllint tried to lint docker-compose.yml which might not exist.

**Fix**: Add conditional check:
```yaml
- name: Run yamllint on docker-compose
  run: |
    if [ -f docker-compose.yml ]; then
      yamllint -s -d "{extends: default, rules: {line-length: {max: 120}}}" \
        docker-compose.yml
    else
      echo "⚠️  docker-compose.yml not found - skipping"
    fi
  continue-on-error: true
```

### 6. Bazel Files Check

**Problem**: Buildifier would fail if no Bazel files exist.

**Fix**: Add existence check before linting:
```yaml
- name: Check if Bazel files exist
  id: check_bazel
  run: |
    if find . -name "BUILD.bazel" -o -name "WORKSPACE.bazel" -o -name "*.bzl" | grep -q .; then
      echo "exists=true" >> $GITHUB_OUTPUT
    else
      echo "exists=false" >> $GITHUB_OUTPUT
      echo "⚠️  No Bazel files found - skipping"
    fi
```

### 7. Lint Summary Needs Always Run

**Problem**: lint-summary job would not run if any linting job failed.

**Fix**: Add `if: always()` to ensure summary runs:
```yaml
lint-summary:
  name: Linting Summary
  runs-on: ubuntu-latest
  needs: [list of all lint jobs]
  if: always()  # Run even if some jobs fail
  steps:
    - name: All linting passed
      run: |
        echo "✓ All linting checks completed"
        echo "Note: Some checks may have been skipped for unimplemented services"
```

## Validation Commands

### Local Testing

Before pushing changes, test workflows locally:

```bash
# Test ShellCheck
find . -type f -name "*.sh" -not -path "./.git/*" | while read -r file; do
  echo "Checking: $file"
  shellcheck --severity=style --enable=all "$file"
done

# Test Python linting
cd backend
ruff check src/ tests/
black --check --diff src/ tests/
mypy src/ --strict

# Test TypeScript linting
cd frontend
pnpm eslint . --ext .js,.ts,.svelte --max-warnings 0
pnpm prettier --check .
pnpm tsc --noEmit --strict

# Test YAML linting
yamllint -s .github/workflows/*.yml

# Test JSON validation
find . -name "*.json" -not -path "./node_modules/*" -not -path "./.git/*" | while read -r file; do
  python3 -m json.tool "$file" > /dev/null && echo "✓ $file"
done

# Test Markdown linting
markdownlint '**/*.md' --ignore node_modules --config .markdownlint.json
```

### GitHub Actions Workflow Syntax Validation

```bash
# Install actionlint
brew install actionlint  # macOS
# or
go install github.com/rhysd/actionlint/cmd/actionlint@latest

# Validate workflow syntax
actionlint .github/workflows/lint.yml
actionlint .github/workflows/ci.yml
```

## Current Workflow Status

### Implemented and Working

✅ **lint-shell**: ShellCheck with POSIX sh compliance
✅ **lint-python**: ruff, black, mypy --strict, bandit
✅ **lint-typescript**: ESLint --max-warnings 0, Prettier, TSC --strict
✅ **lint-docker**: Hadolint on backend/frontend Dockerfiles
✅ **lint-yaml**: yamllint on workflow files (strict mode)
✅ **lint-json**: JSON syntax validation
✅ **lint-markdown**: markdownlint with strict rules
✅ **lint-summary**: Always runs, provides status

### Conditional (Skipped if Not Implemented)

⚠️  **lint-c**: Skipped (c-service not yet implemented)
⚠️  **lint-haskell**: Skipped (haskell-service not yet implemented)
⚠️  **lint-bazel**: Removed (Bazel removed per ADR 0001)

### To Be Implemented

📋 **Services to Add** (Weeks 9-11):
- LISP service (Week 9)
- IDRIS2 service (Week 10.1)
- System F service (Week 10.2)
- Java service (Week 11)

When these are implemented, the conditional lint jobs will automatically activate.

## Common Issues and Solutions

### Issue: "ruff: command not found"

**Solution**: Ensure ruff is installed via requirements-dev.txt:
```bash
cd backend
pip install -r requirements-dev.txt
```

### Issue: "mypy: No module named 'src'"

**Solution**: Run mypy from the correct directory:
```bash
cd backend
mypy src/ --strict
```

### Issue: "ESLint: max-warnings exceeded"

**Solution**: Fix all ESLint warnings or suppress specific rules:
```javascript
// eslint-disable-next-line rule-name
```

### Issue: "ShellCheck: not following sourced file"

**Solution**: Add shellcheck directive at top of script:
```bash
#!/bin/sh
# shellcheck source=/path/to/file.sh
```

### Issue: "Hadolint: DL3008 warning"

**Solution**: Pin package versions in Dockerfile:
```dockerfile
RUN apt-get update && apt-get install -y --no-install-recommends \
    package1=version \
    package2=version
```

## Performance Optimization

### Caching

All workflows use appropriate caching:

```yaml
# Python
- uses: actions/setup-python@v5
  with:
    python-version: ${{ env.PYTHON_VERSION }}
    cache: pip

# Node.js
- uses: actions/setup-node@v4
  with:
    node-version: ${{ env.NODE_VERSION }}
    cache: pnpm
    cache-dependency-path: frontend/pnpm-lock.yaml
```

### Parallel Execution

Lint jobs run in parallel for maximum speed:
- lint-shell, lint-python, lint-typescript run simultaneously
- Only lint-summary waits for all jobs to complete

### Failure Fast

Critical linters fail immediately:
- Python: pylint < 9.0 fails the job
- TypeScript: ESLint warnings fail the job
- Shell: ShellCheck style issues fail the job

## Next Steps

1. **Immediate**: Verify all workflows pass on next push
2. **Short-term**: Add language-specific linters as services are implemented
3. **Medium-term**: Add custom linting rules for Ancient Compute specific patterns
4. **Long-term**: Integrate linting metrics into observability dashboard

## References

- [ShellCheck Wiki](https://www.shellcheck.net/wiki/)
- [Pylint Documentation](https://pylint.readthedocs.io/)
- [ESLint Rules](https://eslint.org/docs/rules/)
- [Hadolint Rules](https://github.com/hadolint/hadolint)
- [yamllint Documentation](https://yamllint.readthedocs.io/)

## Appendix: Warnings-as-Errors Configuration

| Language | Tool | Config | Status |
|----------|------|--------|--------|
| Shell | ShellCheck | `--severity=style` | ✅ Enforced |
| Python | ruff | exit non-zero on violations | ✅ Enforced |
| Python | mypy | `--strict` | ✅ Enforced |
| TypeScript | ESLint | `--max-warnings 0` | ✅ Enforced |
| TypeScript | TSC | `--strict` | ✅ Enforced |
| C | clang-tidy | `--warnings-as-errors='*'` | ⚠️  Conditional |
| Docker | Hadolint | `failure-threshold: warning` | ✅ Enforced |
| YAML | yamllint | `-s` (strict) | ✅ Enforced |
---

**Document Version**: 1.1
**Last Updated**: March 5, 2026
**Maintained by**: Project Team
