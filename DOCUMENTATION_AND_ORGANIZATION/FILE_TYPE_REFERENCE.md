# File Type Reference and Linting Guide

## Version: 1.0
## Date: November 2, 2025

---

## Overview

This document catalogs **all file types** in the Ancient Compute repository, their purpose, linting tools, and quality standards.

---

## File Type Catalog

### Shell Scripts (`.sh`)

**Purpose**: System setup, automation, deployment scripts

**Linter**: ShellCheck
- Config: `.shellcheckrc`
- Command: `shellcheck --severity=style --enable=all *.sh`
- Standard: POSIX sh preferred, bash when necessary

**Files**:
- `scripts/setup-debian.sh` - Debian/Linux setup
- `scripts/setup-windows.ps1` - Windows setup (PowerShell)

**Quality Standards**:
- ✅ POSIX compliance preferred
- ✅ `set -eu` for error handling
- ✅ Quote all variables
- ✅ Use `$(...)` not backticks
- ✅ ShellCheck severity=style (all warnings)

---

### Python (`.py`)

**Purpose**: Backend API, language services, build scripts, tests

**Linters**:
1. **black** - Formatting
2. **isort** - Import order
3. **flake8** - Style + errors
4. **pylint** - Quality (≥9.0)
5. **mypy** - Type checking (strict)
6. **bandit** - Security

**Files**:
- `backend/src/**/*.py` - Backend code
- `backend/tests/**/*.py` - Backend tests
- `backend/alembic/**/*.py` - Database migrations
- `services/*/src/**/*.py` - Language service implementations

**Quality Standards**:
- ✅ 100-char line limit
- ✅ Type hints required
- ✅ Docstrings (Google style)
- ✅ Pylint score ≥9.0
- ✅ Cyclomatic complexity ≤10
- ✅ Test coverage ≥90%

---

### TypeScript (`.ts`)

**Purpose**: Frontend application, type definitions

**Linters**:
1. **ESLint** - Code quality
2. **Prettier** - Formatting
3. **TSC** - Type checking

**Files**:
- `frontend/src/**/*.ts` - TypeScript source
- `frontend/src/**/*.svelte` - Svelte components (TypeScript)
- `frontend/tests/**/*.ts` - Frontend tests
- `playwright.config.ts` - E2E test config

**Quality Standards**:
- ✅ Strict mode enabled
- ✅ No implicit any
- ✅ Max warnings: 0
- ✅ Unused variables are errors
- ✅ Consistent naming (camelCase)

---

### JavaScript (`.js`, `.mjs`, `.cjs`)

**Purpose**: Configuration files, build scripts

**Linters**:
1. **ESLint**
2. **Prettier**

**Files**:
- `frontend/svelte.config.js` - Svelte configuration
- `frontend/vite.config.js` - Vite build config

**Quality Standards**:
- ✅ ES6+ syntax
- ✅ Max warnings: 0
- ✅ Consistent formatting

---

### Svelte (`.svelte`)

**Purpose**: Frontend UI components

**Linters**:
1. **svelte-check** - Svelte-specific
2. **ESLint** (with Svelte plugin)
3. **Prettier**

**Files**:
- `frontend/src/**/*.svelte` - UI components

**Quality Standards**:
- ✅ TypeScript in `<script lang="ts">`
- ✅ Accessibility checks
- ✅ No unused CSS
- ✅ Fail on warnings

---

### C (`.c`, `.h`)

**Purpose**: C language service implementation

**Linters**:
1. **clang-format** - Formatting
2. **clang-tidy** - Static analysis

**Files**:
- `services/c-service/src/**/*.c` - C source
- `services/c-service/include/**/*.h` - C headers

**Quality Standards**:
- ✅ LLVM style
- ✅ `-Wall -Wextra -Wpedantic -Werror`
- ✅ No memory leaks (valgrind)
- ✅ MISRA C compliance (subset)

---

### Haskell (`.hs`)

**Purpose**: Haskell language service

**Linters**:
1. **HLint** - Suggestions
2. **GHC** - Compiler warnings

**Files**:
- `services/haskell-service/src/**/*.hs`

**Quality Standards**:
- ✅ All HLint suggestions applied
- ✅ `-Wall -Werror`
- ✅ Haddock documentation

---

### Dockerfile

**Purpose**: Container definitions

**Linter**: Hadolint
- Config: `.hadolint.yaml`
- Command: `hadolint Dockerfile`

**Files**:
- `backend/Dockerfile`
- `frontend/Dockerfile`
- `services/*/Dockerfile`

**Quality Standards**:
- ✅ Pin base image versions
- ✅ Pin package versions
- ✅ Multi-stage builds
- ✅ No root user
- ✅ Minimal layers

---

### Docker Compose (`.yml`)

**Purpose**: Multi-container orchestration

**Linter**: yamllint

**Files**:
- `docker-compose.yml`
- `docker-compose.dev.yml`

**Quality Standards**:
- ✅ Version 3.8+
- ✅ Health checks defined
- ✅ Resource limits set
- ✅ Networks configured

---

### YAML (`.yaml`, `.yml`)

**Purpose**: Configuration files, CI/CD workflows

**Linter**: yamllint
- Command: `yamllint -s *.yml`

**Files**:
- `.github/workflows/*.yml` - GitHub Actions
- `.pre-commit-config.yaml` - Pre-commit hooks
- `.hadolint.yaml` - Hadolint config

**Quality Standards**:
- ✅ 2-space indentation
- ✅ Line length ≤120
- ✅ No trailing spaces
- ✅ Consistent key order

---

### JSON (`.json`)

**Purpose**: Configuration, package manifests

**Linter**: Python json.tool

**Files**:
- `frontend/package.json` - NPM dependencies
- `frontend/tsconfig.json` - TypeScript config
- `frontend/.eslintrc.json` - ESLint config
- `.markdownlint.json` - Markdown lint config

**Quality Standards**:
- ✅ Valid syntax
- ✅ 2-space indentation
- ✅ No trailing commas
- ✅ Sorted keys (where appropriate)

---

### Markdown (`.md`)

**Purpose**: Documentation

**Linter**: markdownlint
- Config: `.markdownlint.json`

**Files**:
- `README.md` - Project overview
- `CLAUDE.md` - AI agent instructions
- `AGENTS.md` - Multi-agent system
- `MASTER_ROADMAP.md` - Project roadmap
- `API_DOCUMENTATION.md` - API reference
- `DEPLOYMENT_GUIDE.md` - Deployment instructions
- `*.md` - All documentation

**Quality Standards**:
- ✅ Line length ≤120
- ✅ Consistent headers
- ✅ No bare URLs
- ✅ Code blocks have language
- ✅ Lists have consistent bullets

---

### Bazel Build Files (`.bazel`, `.bzl`)

**Purpose**: Build system configuration

**Linter**: Buildifier
- Command: `buildifier -mode=check -lint=warn -warnings=all`

**Files**:
- `BUILD.bazel` - Build targets
- `WORKSPACE.bazel` - Workspace config
- `MODULE.bazel` - Module config

**Quality Standards**:
- ✅ Consistent formatting
- ✅ Sorted lists
- ✅ Proper load statements
- ✅ No unused dependencies

---

### LaTeX (`.tex`)

**Purpose**: Academic documentation, whitepapers

**Linter**: ChkTeX (optional)

**Files**:
- `whitepaper/*.tex` - LaTeX source
- `BABBAGE_COMPLETE_WHITEPAPER.tex`
- `PEDAGOGICAL_WHITEPAPER.tex`

**Quality Standards**:
- ✅ Consistent macro usage
- ✅ Proper citations
- ✅ Clean build (no warnings)

---

### Sed Scripts (`.sed`)

**Purpose**: Text transformation scripts

**Linter**: Manual review

**Files**:
- `COMPREHENSIVE_FIX_MALFORMED.sed`
- `FIX_MALFORMED_PATHS.sed`
- `PATH_MAPPING.sed`

**Quality Standards**:
- ✅ Commented operations
- ✅ Tested on sample data
- ✅ Idempotent when possible

---

### PowerShell (`.ps1`)

**Purpose**: Windows setup scripts

**Linter**: PSScriptAnalyzer

**Files**:
- `scripts/setup-windows.ps1`

**Quality Standards**:
- ✅ Approved verbs
- ✅ Error handling
- ✅ Parameter validation
- ✅ Help comments

---

### TOML (`.toml`)

**Purpose**: Python project configuration

**Linter**: tomli (Python library)

**Files**:
- `pyproject.toml` - Python project config

**Quality Standards**:
- ✅ Valid TOML syntax
- ✅ Consistent sections
- ✅ Complete metadata

---

### INI (`.ini`)

**Purpose**: Configuration files

**Linter**: ConfigParser (Python)

**Files**:
- `backend/alembic.ini` - Alembic config
- `backend/pytest.ini` - Pytest config

**Quality Standards**:
- ✅ Valid syntax
- ✅ Documented options
- ✅ No sensitive data

---

### Git Configuration

**Files**:
- `.gitignore` - Ignore patterns
- `.gitattributes` - File attributes

**Quality Standards**:
- ✅ Comprehensive patterns
- ✅ No accidental commits
- ✅ Consistent line endings (LF)

---

## File Type Summary Table

| Extension | Language | Linter(s) | Config File | Warnings-as-Errors |
|-----------|----------|-----------|-------------|--------------------|
| `.sh` | Shell | ShellCheck | `.shellcheckrc` | ✅ Yes |
| `.py` | Python | black, isort, flake8, pylint, mypy | `.pylintrc`, `pyproject.toml` | ✅ Yes |
| `.ts` | TypeScript | ESLint, Prettier, TSC | `tsconfig.json`, `.eslintrc.json` | ✅ Yes |
| `.js` | JavaScript | ESLint, Prettier | `.eslintrc.json` | ✅ Yes |
| `.svelte` | Svelte | svelte-check, ESLint | `svelte.config.js` | ✅ Yes |
| `.c`, `.h` | C | clang-format, clang-tidy | N/A | ✅ Yes |
| `.hs` | Haskell | HLint, GHC | N/A | ✅ Yes |
| `Dockerfile` | Docker | Hadolint | `.hadolint.yaml` | ✅ Yes |
| `.yml`, `.yaml` | YAML | yamllint | N/A | ✅ Yes |
| `.json` | JSON | json.tool | N/A | ✅ Yes |
| `.md` | Markdown | markdownlint | `.markdownlint.json` | ✅ Yes |
| `.bazel`, `.bzl` | Starlark | Buildifier | N/A | ✅ Yes |
| `.tex` | LaTeX | ChkTeX | N/A | ⚠️ Optional |
| `.ps1` | PowerShell | PSScriptAnalyzer | N/A | ✅ Yes |
| `.toml` | TOML | tomli | N/A | ✅ Yes |

---

## Installation Commands

### All Linters

```bash
# Python tools
pip install black isort pylint mypy flake8 bandit safety

# System tools
sudo apt-get install -y shellcheck yamllint

# Node.js tools (installed via pnpm in frontend/)
cd frontend && pnpm install

# Docker linter
wget https://github.com/hadolint/hadolint/releases/download/v2.12.0/hadolint-Linux-x86_64
chmod +x hadolint-Linux-x86_64
sudo mv hadolint-Linux-x86_64 /usr/local/bin/hadolint

# Bazel linter
wget https://github.com/bazelbuild/buildtools/releases/download/v6.4.0/buildifier-linux-amd64
chmod +x buildifier-linux-amd64
sudo mv buildifier-linux-amd64 /usr/local/bin/buildifier

# C linters (usually pre-installed with clang)
sudo apt-get install -y clang-format clang-tidy

# Haskell linter
cabal install hlint

# PowerShell linter (Windows)
Install-Module -Name PSScriptAnalyzer -Scope CurrentUser
```

---

## Quick Lint Commands

```bash
# Lint everything
./scripts/lint-all.sh

# Lint by language
shellcheck scripts/*.sh
cd backend && pylint src/ && mypy src/
cd frontend && pnpm lint
hadolint Dockerfile
yamllint .github/workflows/*.yml
markdownlint *.md
```

---

## CI/CD Integration

All file types are linted in GitHub Actions:
- **Workflow**: `.github/workflows/lint.yml`
- **Trigger**: On push and PR
- **Policy**: Fail on any warnings
- **Coverage**: 100% of file types

---

## References

- [LINTING_STRATEGY.md](LINTING_STRATEGY.md) - Comprehensive linting guide
- [.pre-commit-config.yaml](.pre-commit-config.yaml) - Pre-commit hooks
- [.github/workflows/lint.yml](.github/workflows/lint.yml) - CI linting workflow

---

**Status**: ✅ Complete coverage for all file types
**Enforcement**: Warnings-as-errors enabled across all languages
