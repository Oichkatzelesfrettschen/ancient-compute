# ADR 0001: Remove Bazel Build System

## Status

Accepted (2026-02-26)

## Context

The repository contained Bazel build files (BUILD.bazel, WORKSPACE.bazel,
MODULE.bazel) that were never functional.  No contributor has a working
Bazel installation, and the project's actual build workflow uses Makefile
targets, pytest, and Docker Compose.

Keeping non-functional Bazel files:
- Confused new contributors about the canonical build path.
- Added a lint-bazel CI job that downloaded buildifier for stubs.
- Created documentation that referenced `bazel build //...` and
  `bazel test //...` as primary commands.

## Decision

Remove all Bazel files and references.  The canonical build system is:
- **Backend**: `make test-unit`, `make verify`, `pytest`
- **Frontend**: `pnpm install && pnpm dev`
- **CI**: GitHub Actions workflows (ci.yml, lint.yml)
- **Containers**: `docker compose up`

## Consequences

- CI no longer downloads buildifier or runs lint-bazel.
- CLAUDE.md build commands updated to reflect Makefile targets.
- If Bazel is needed in the future, it should be introduced as a
  complete, tested configuration rather than aspirational stubs.
