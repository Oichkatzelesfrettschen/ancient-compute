# Project Status Dashboard

**Auto-generated**: 2026-03-20 20:47 UTC
**Branch**: `main`
**Last commit**: `0ca6fa0 feat: upgrade IDRIS2 service from Babbage IR stub to real idris2 0.8.0 execution`

> Regenerate with: `make status`

---

## Metrics

| Metric | Value |
|--------|-------|
| Unit tests collected | 2141 |
| Collection errors | 0 |
| Python source lines (backend/src/) | 38,330 |
| Active docs (non-archive .md) | 197 |

## Phase Status

| Phase | Status | Notes |
|-------|--------|-------|
| A: Program Charter & Sources | COMPLETE | CHARTER.md, SOURCE_MAP.md created |
| B: CI Strict Gating | COMPLETE | continue-on-error removed from test/lint gates |
| C: Status Dashboard | COMPLETE | This file; `make status` target |
| D: Card Grammar v0.1 | COMPLETE | CARD_STANDARD.md, OPCODES.yaml, card compiler |
| E: Hardware Twin | COMPLETE | DE tabulator, printer formatter, golden traces |
| F: BOM Schema | COMPLETE | Schema, seed CSV, validation script |
| G: Hardware Specs | COMPLETE | 5 spec docs with source citations |
| H: Onboarding Cleanup | COMPLETE | README fixed, DOCUMENT_INDEX.md, platform matrix |

## Known Issues

- DB-dependent tests skip when no database is available (by design)
- Linter: ruff replaces flake8+pylint+isort (consolidated)

## Links

- [CHARTER.md](../program/CHARTER.md) -- scope and governance
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- source traceability
- [DOCUMENT_INDEX.md](DOCUMENT_INDEX.md) -- master doc index
