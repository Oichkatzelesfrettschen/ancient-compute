# Multi-Repository Management Scripts

Scripts for managing the Ancient Compute multi-repository project.

## Available Scripts

### sync-submodules.sh

Synchronize all submodules to their latest versions.

**Usage**:
```bash
./scripts/multi-repo/sync-submodules.sh
```

**What it does**:
1. Initializes submodules (if not already done)
2. Checks out main/master branch in each submodule
3. Pulls latest changes from origin
4. Updates submodule references in parent repo

**When to use**:
- After someone updates a submodule repository
- When setting up a development environment
- Before starting new work to ensure you have latest code

### check-compatibility.sh

Verify version compatibility across all repositories.

**Usage**:
```bash
./scripts/multi-repo/check-compatibility.sh
```

**What it checks**:
- Node.js version (frontend)
- Python version (backend)
- Bazel version (build system)
- Docker availability
- Submodule initialization status

**When to use**:
- Setting up new development environment
- After updating dependencies
- Before deployment
- Debugging environment issues

### cross-repo-search.sh

Search for a pattern across all repositories.

**Usage**:
```bash
./scripts/multi-repo/cross-repo-search.sh <pattern>
```

**Examples**:
```bash
# Find all TODOs
./scripts/multi-repo/cross-repo-search.sh "TODO:"

# Find all uses of a function
./scripts/multi-repo/cross-repo-search.sh "executeCode"

# Find security issues
./scripts/multi-repo/cross-repo-search.sh "password"
```

**What it does**:
- Searches each submodule repository
- Uses git grep for efficiency (respects .gitignore)
- Falls back to regular grep if not a git repo
- Shows file paths and line numbers

**When to use**:
- Finding all occurrences of a pattern
- Tracking down function usage
- Auditing code for patterns
- Finding TODOs or FIXMEs

## Prerequisites

All scripts require:
- Bash 4.0+
- Git 2.30+
- Initialized submodules

Individual script requirements:
- `check-compatibility.sh`: node, python3, docker, bazel (checks if available)
- `cross-repo-search.sh`: grep (built-in on most systems)

## Common Workflows

### Initial Setup

```bash
# Clone repository with submodules
git clone --recurse-submodules https://github.com/Oichkatzelesfrettschen/ancient-compute.git
cd ancient-compute

# Verify everything is compatible
./scripts/multi-repo/check-compatibility.sh
```

### Daily Development

```bash
# Start of day: sync to latest
./scripts/multi-repo/sync-submodules.sh

# Work on your feature...

# Search for a function you need to update
./scripts/multi-repo/cross-repo-search.sh "myFunction"

# End of day: verify compatibility before committing
./scripts/multi-repo/check-compatibility.sh
```

### Before Pull Request

```bash
# Sync to latest
./scripts/multi-repo/sync-submodules.sh

# Check compatibility
./scripts/multi-repo/check-compatibility.sh

# Search for debugging code you might have left in
./scripts/multi-repo/cross-repo-search.sh "console.log"
./scripts/multi-repo/cross-repo-search.sh "print("
```

## Troubleshooting

### Submodule Not Initialized

**Problem**: Script reports "not found (submodule not initialized?)"

**Solution**:
```bash
git submodule update --init --recursive
```

### Permission Denied

**Problem**: `bash: permission denied`

**Solution**:
```bash
chmod +x scripts/multi-repo/*.sh
```

### Git Grep Not Working

**Problem**: Search returning no results but you know the pattern exists

**Solution**:
```bash
# Git grep only searches committed files
# Commit your changes or use regular grep instead
cd /path/to/repo
grep -r "pattern" .
```

## Contributing

When adding new scripts to this directory:

1. Make them executable: `chmod +x script.sh`
2. Add usage documentation in header comments
3. Add error handling (`set -e`)
4. Add help text (`-h` or `--help` flag)
5. Update this README
6. Test on both macOS and Linux

## Script Template

```bash
#!/bin/bash
# Description of what this script does
# Usage: ./script.sh [options]

set -e  # Exit on error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

# Help text
if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
    echo "Usage: $0 [options]"
    echo ""
    echo "Description of script"
    echo ""
    echo "Options:"
    echo "  -h, --help    Show this help message"
    exit 0
fi

# Main script logic here
echo "Doing something useful..."

# Success message
echo "Done!"
```

## Future Scripts

Planned additions:
- `sync-labels.sh` - Sync GitHub labels across all repos
- `create-release.sh` - Coordinated release across repos
- `run-tests.sh` - Run tests in all repos
- `lint-all.sh` - Lint all repositories
- `check-security.sh` - Security audit across repos

---

**Last Updated**: November 2, 2025  
**Maintainer**: Ancient Compute Team
