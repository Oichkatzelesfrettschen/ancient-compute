# Deferred Work Registry

**Last updated**: 2026-03-20
**Purpose**: Resolution log for work items that were deferred and later resolved,
plus any items currently in active deferral.  Long-term aspirational items that
have no activation timeline live in [WISHLIST.md](WISHLIST.md).

---

## Active

### ruff: pre-existing violations (per-sprint cleanup)

**WHY**: The ignored ruff rules in `pyproject.toml` represent pre-existing
instances that require line-by-line review.  Bulk auto-fix risks changing
semantics (e.g., E741 ambiguous variable names in physics code where `l`
means length).

**WHAT**: `pyproject.toml [tool.ruff.lint] ignore` lists the ignored rules.
UP042 (10 `str+Enum` sites, migrate to `StrEnum`) was added 2026-03-20.

**WHEN**: Address one rule per sprint.

---

## Resolved

### mypy: 0 errors -- RESOLVED 2026-03-20

`python3 -m mypy backend/src/` reports 0 errors across 187 source files.
`python3 -m mypy backend/src/ --strict --ignore-missing-imports` also reports
0 errors.  Fixes: no-untyped-def + no-any-return sprint; SQLAlchemy Base
type: ignore[misc]; explicit re-exports in c_ast.py and engine.py;
lisp_compiler list[ASTNode] type param; haskell_parser comparison-overlap.

---

### Test coverage sprint -- RESOLVED 2026-03-20

- `test_rate_limiting.py`: 25 tests
- `test_docker_manager.py`: 16 tests (mocks Docker, no daemon required)
- `test_execution_cache.py`: 22 tests
- `test_query_cache.py`: 17 tests
- `test_metrics.py`: 22 tests

TUI tests deferred to WISHLIST (requires `textual.testing` framework setup).

---

### Gate 3: Language Promotion -- RESOLVED 2026-03-20

9 languages: LISP (SBCL 2.6.2), Haskell, IDRIS2 (idris2 0.8.0 --check),
SystemF (Babbage IR), ALGOL68 (a68g), C (Babbage IR), C++ (g++), Python
(Babbage IR), MicroPython.  Java deferred to WISHLIST.  1722 unit tests.

---

### Gate 2: C Freestanding Subset -- RESOLVED 2026-03-20

C freestanding subset compiles end-to-end to Babbage ISA.  4 code-gen bugs
fixed (emitter label addresses, trailing labels, comparison encoding, return
terminator).  for-loop init declarations added to C parser.

---

### `ancient-compute` CLI entry point -- RESOLVED 2026-03-19

`make setup` runs `pip install -e .`; CLI available system-wide after setup.

---

### Opcode-coupled physics (`ancient-compute run --physics`) -- RESOLVED 2026-03-19

`SimulationBridge` wired to `Engine` for all `.basm` programs.  Deck physics
path (`run_note_g_exact`) moved to WISHLIST.

---
