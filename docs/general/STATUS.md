# Project Status Dashboard

**Auto-generated**: 2026-02-26 10:24 UTC
**Branch**: `main`
**Last commit**: `e9d9697 Add initial Alembic migration and test DB documentation (Phase 12)`

> Regenerate with: `make status`

---

## Metrics

| Metric | Value |
|--------|-------|
| Unit tests collected | 1120 |
| Collection errors | 0 |
| Python source lines (backend/src/) | 32,886 |
| Active docs (non-archive .md) | 209 |

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

- 3 pre-existing test failures: leibniz_reckoner, 2x pascaline
- 3 collection errors: test_tools_router.py, test_cross_language.py, test_phase4_w1_api.py
- 33 pre-existing DB fixture errors (excluded from CI gate)
- flake8/pylint overlap: consolidation to ruff planned

## Links

- [CHARTER.md](../program/CHARTER.md) -- scope and governance
- [SOURCE_MAP.md](../sources/SOURCE_MAP.md) -- source traceability
- [DOCUMENT_INDEX.md](DOCUMENT_INDEX.md) -- master doc index
