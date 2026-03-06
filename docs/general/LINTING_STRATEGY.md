# Comprehensive Linting Strategy - Warnings as Errors

> **Note**: Python linter consolidated to **ruff** per [ADR 0001](../adr/0001-remove-bazel.md).
> References to pylint, flake8, and isort below are historical; ruff replaces all three.

## Version: 1.1
## Date: March 5, 2026
## Status: Production-Ready

---

## Overview

This document outlines the **comprehensive linting strategy** for the Ancient Compute project, implementing **warnings-as-errors** across all programming languages and file types to ensure the highest code quality standards.

**Key Principle**: Every warning is treated as an error. Code must pass all linting checks before merging.

---

## Table of Contents

1. [Shell Scripts (sh/bash)](#shell-scripts)
2. [Python Backend](#python-backend)
3. [TypeScript/JavaScript Frontend](#typescript-frontend)
4. [C Language Services](#c-language-services)
5. [Haskell Language Services](#haskell-language-services)
6. [Docker and Containers](#docker-containers)
7. [YAML Configuration](#yaml-files)
8. [Markdown Documentation](#markdown-documentation)
9. [JSON Configuration](#json-files)
10. [Bazel BUILD Files](#bazel-files)
11. [CI/CD Integration](#cicd-integration)
12. [Local Development](#local-development)
13. [Pre-Commit Hooks](#pre-commit-hooks)

---

## Shell Scripts

### Tool: ShellCheck

**Configuration**: `.shellcheckrc`

```bash
# Prefer POSIX sh over bash
shell=sh
enable=all
severity=style  # Treat style issues as errors
```

### Standards

- **Preferred**: `#!/bin/sh` (POSIX compliance)
- **Fallback**: `#!/bin/bash` (only when bash-specific features required)
- **Set options**: `set -eu` (sh) or `set -euo pipefail` (bash)

### Linting Command

```bash
# Single file
shellcheck --severity=style --enable=all script.sh

# All files
find . -name "*.sh" -exec shellcheck --severity=style --enable=all {} \;
```

### Common Issues Fixed

- **SC2086**: Quote variables to prevent word splitting
- **SC2006**: Use `$(...)` instead of backticks
- **SC2034**: Unused variables
- **SC2164**: Use `cd ... || exit` for safety
- **SC2046**: Quote command substitution

### Example: Before/After

**Before** (warnings):
```bash
#!/bin/bash
cd $DIR
FILES=`ls *.txt`
```

**After** (clean):
```bash
#!/bin/sh
cd "$DIR" || exit 1
FILES=$(find . -name "*.txt")
```

---

## Python Backend

### Tools

1. **ruff** - Lint + import sort (replaces pylint, flake8, isort per ADR 0001)
2. **black** - Code formatting (PEP 8)
3. **mypy** - Type checking (strict mode)
4. **bandit** - Security scanning

### Configuration Files

- **`pyproject.toml`**: ruff, black, mypy, isort (legacy), pytest config

### Linting Commands

```bash
cd backend

# Lint + import order (single tool)
ruff check src/ tests/

# Auto-fix safe violations
ruff check --fix src/ tests/

# Format check
black --check --diff src/ tests/

# Type checking (strict)
mypy src/ --strict --warn-return-any --warn-unused-configs \
  --disallow-untyped-defs --disallow-incomplete-defs

# Security
bandit -r src/ -ll
```

### Standards

- **Line length**: 100 characters
- **Type hints**: Required for all functions
- **Complexity**: Max cyclomatic complexity 10
- **Ruff violations**: 0 (warnings as errors)
- **Test coverage**: ≥ 90%

### Common Fixes

- Add type hints: `def func(x: int) -> str:`
- Remove unused imports
- Fix complexity by extracting functions
- Add docstrings (Google style)

---

## TypeScript Frontend

### Tools

1. **ESLint** - Code quality
2. **Prettier** - Formatting
3. **TypeScript Compiler** - Type checking
4. **svelte-check** - Svelte-specific checks

### Configuration Files

- **`.eslintrc.json`**: ESLint rules
- **`.prettierrc`**: Prettier config
- **`tsconfig.json`**: TypeScript config

### Linting Commands

```bash
cd frontend

# ESLint (max-warnings 0)
pnpm eslint . --ext .js,.ts,.svelte --max-warnings 0

# Prettier
pnpm prettier --check .

# TypeScript
pnpm tsc --noEmit --strict

# Svelte
pnpm svelte-check --fail-on-warnings
```

### Standards

- **Max warnings**: 0 (all warnings are errors)
- **Type strictness**: Enabled
- **Unused variables**: Error
- **Console statements**: Warning (remove in production)

---

## C Language Services

### Tools

1. **clang-format** - Code formatting
2. **clang-tidy** - Static analysis

### Linting Commands

```bash
# Format check
clang-format --dry-run --Werror *.c *.h

# Static analysis
clang-tidy *.c --warnings-as-errors='*' -- -I./include
```

### Standards

- **Style**: LLVM style
- **Warnings**: All enabled (-Wall -Wextra -Wpedantic)
- **Compile**: `-Werror` (warnings as errors)

---

## Haskell Language Services

### Tool: HLint

### Linting Commands

```bash
# All files
find . -name "*.hs" -exec hlint {} \;

# With report
hlint src/ --report
```

### Standards

- **Suggestions**: All applied
- **Warnings**: Treated as errors in CI

---

## Docker Containers

### Tool: Hadolint

**Configuration**: `.hadolint.yaml`

```yaml
failure-threshold: warning  # Warnings as errors
strict-labels: true
```

### Linting Commands

```bash
# Single Dockerfile
hadolint Dockerfile

# All Dockerfiles
find . -name "Dockerfile*" -exec hadolint {} \;
```

### Standards

- Pin base image versions
- Use specific package versions
- Multi-stage builds for size reduction
- No root user in production

---

## YAML Files

### Tool: yamllint

### Linting Commands

```bash
yamllint -s .github/workflows/*.yml docker-compose.yml
```

### Standards

- Consistent indentation (2 spaces)
- Line length ≤ 120
- No trailing spaces
- Proper document markers

---

## Markdown Documentation

### Tool: markdownlint

**Configuration**: `.markdownlint.json`

```json
{
  "MD013": { "line_length": 120 },
  "MD033": false  # Allow HTML
}
```

### Linting Commands

```bash
markdownlint *.md docs/**/*.md
```

---

## JSON Files

### Tool: Python json.tool

### Linting Commands

```bash
find . -name "*.json" -exec python3 -m json.tool {} \; > /dev/null
```

### Standards

- Valid JSON syntax
- Consistent indentation
- No trailing commas

---

## Bazel Files

> **Removed**: Bazel was removed per [ADR 0001](../adr/0001-remove-bazel.md).
> This section is kept for historical reference only.

---

## CI/CD Integration

### Workflows

1. **`.github/workflows/lint.yml`**: Comprehensive linting (all languages)
2. **`.github/workflows/ci.yml`**: Main CI with integrated linting

### Execution

```yaml
# All linting must pass before tests run
jobs:
  lint-all:
    # ... lint jobs ...
  
  test:
    needs: [lint-all]  # Depends on linting
    # ... test jobs ...
```

### Failure Policy

- **Any lint failure**: ❌ Build fails, PR blocked
- **All lint pass**: ✅ Proceed to tests
- **No exceptions**: Warnings-as-errors enforced

---

## Local Development

### Setup

```bash
# Install all linting tools
cd backend && pip install -r requirements-dev.txt  # includes ruff, black, mypy
cd frontend && pnpm install  # Includes ESLint, Prettier
sudo apt-get install shellcheck hadolint yamllint
```

### Pre-Push Checklist

```bash
# Backend
cd backend
ruff check src/ tests/
black --check src/ tests/
mypy src/

# Frontend
cd frontend
pnpm lint --fix
pnpm format
pnpm check

# Shell scripts
shellcheck scripts/*.sh

# Docker
hadolint Dockerfile
```

---

## Pre-Commit Hooks

### Configuration: `.pre-commit-config.yaml`

```yaml
repos:
  - repo: https://github.com/psf/black
    rev: 23.10.0
    hooks:
      - id: black

  - repo: https://github.com/pycqa/isort
    rev: 5.12.0
    hooks:
      - id: isort

  - repo: https://github.com/koalaman/shellcheck-precommit
    rev: v0.9.0
    hooks:
      - id: shellcheck
        args: [--severity=style]

  - repo: https://github.com/hadolint/hadolint
    rev: v2.12.0
    hooks:
      - id: hadolint
```

### Installation

```bash
pip install pre-commit
pre-commit install
```

### Usage

Hooks run automatically on `git commit`. To run manually:

```bash
pre-commit run --all-files
```

---

## Warnings-as-Errors Configuration Summary

| Language | Tool | Config | Warnings-as-Errors Flag |
|----------|------|--------|------------------------|
| **Shell** | ShellCheck | `.shellcheckrc` | `severity=style` |
| **Python** | ruff | `pyproject.toml` | exit non-zero on violations |
| **Python** | mypy | `pyproject.toml` | `--strict` |
| **TypeScript** | ESLint | `.eslintrc.json` | `--max-warnings 0` |
| **TypeScript** | TSC | `tsconfig.json` | `--strict` |
| **C** | clang-tidy | N/A | `--warnings-as-errors='*'` |
| **Docker** | Hadolint | `.hadolint.yaml` | `failure-threshold: warning` |
| **YAML** | yamllint | CLI | `-s` (strict) |

---

## Enforcement in CI/CD

### CI Workflow Structure

```yaml
lint-all:
  steps:
    - name: Lint (fail on warning)
      run: <linter> --warnings-as-errors
      # No continue-on-error - must pass

test:
  needs: [lint-all]  # Blocked until linting passes
```

### Pull Request Requirements

1. ✅ All linting checks pass
2. ✅ All tests pass (≥90% coverage)
3. ✅ Security scan clean
4. ✅ Build succeeds

**Merge blocked** until all checks pass.

---

## Quality Metrics

### Current Status

| Category | Tool | Pass Rate | Warnings | Errors |
|----------|------|-----------|----------|--------|
| Shell | ShellCheck | 100% | 0 | 0 |
| Python | pylint | 100% | 0 | 0 |
| Python | mypy | 100% | 0 | 0 |
| TypeScript | ESLint | 100% | 0 | 0 |
| Docker | Hadolint | 100% | 0 | 0 |
| YAML | yamllint | 100% | 0 | 0 |

**Overall**: ✅ 100% compliance with warnings-as-errors

---

## Continuous Improvement

### Monthly Reviews

- Review linting rules
- Update tool versions
- Add new checks as needed
- Remove obsolete rules

### New Language Support

When adding new languages:

1. Research best linting tool
2. Configure warnings-as-errors
3. Add to CI/CD workflow
4. Document in this guide
5. Add pre-commit hook

---

## Troubleshooting

### Common Issues

**Issue**: Linting fails in CI but passes locally
**Solution**: Ensure same tool versions (check `requirements.txt`, `package.json`)

**Issue**: Too many warnings to fix
**Solution**: Incremental approach - fix file-by-file, use `# noqa` sparingly

**Issue**: False positives
**Solution**: Inline suppressions with explanation:
```python
# pylint: disable=line-too-long  # API URL cannot be split
```

---

## References

- [ShellCheck Wiki](https://www.shellcheck.net/wiki/)
- [Pylint Documentation](https://pylint.pycqa.org/)
- [ESLint Rules](https://eslint.org/docs/rules/)
- [Hadolint Rules](https://github.com/hadolint/hadolint)

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-11-02 | Initial comprehensive linting strategy |
| 1.1 | 2026-03-05 | Consolidated pylint/flake8/isort to ruff per ADR 0001; removed Bazel section |

---

**Status**: ✅ Production-Ready
**Compliance**: 100% warnings-as-errors enforcement
**Quality**: All code passes strict linting before merge
