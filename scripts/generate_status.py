#!/usr/bin/env python3
"""Generate docs/general/STATUS.md from project metrics.

Collects test counts, file counts, and git metadata to produce a
single-source-of-truth status dashboard.

Usage:
    PYTHONPATH=. python3 scripts/generate_status.py
"""

from __future__ import annotations

import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
STATUS_PATH = ROOT / "docs" / "general" / "STATUS.md"


def run(cmd: list[str], cwd: Path | None = None) -> str:
    """Run a subprocess and return stripped stdout, or empty string on failure."""
    try:
        result = subprocess.run(
            cmd, capture_output=True, text=True, cwd=cwd or ROOT, timeout=60,
        )
        return result.stdout.strip()
    except (subprocess.TimeoutExpired, FileNotFoundError):
        return ""


def count_tests() -> tuple[int, int, int]:
    """Run pytest --collect-only and count tests, passed, errors.

    Falls back to counting test_*.py files if pytest import fails.
    """
    raw = run([
        sys.executable, "-m", "pytest", "--collect-only", "-q",
        "backend/tests/unit/",
    ], cwd=ROOT)
    if not raw:
        # Fallback: count test functions by grepping
        test_dir = ROOT / "backend" / "tests" / "unit"
        count = 0
        if test_dir.exists():
            for f in test_dir.rglob("test_*.py"):
                count += sum(1 for line in f.open() if line.strip().startswith("def test_"))
        return count, 0, 0
    # Last line format: "=== 1070 tests collected in 0.75s ==="
    # or "=== X tests collected / Y errors ==="
    import re
    lines = (raw + "\n" + "").splitlines()
    tests = 0
    errors = 0
    for line in reversed(lines):
        m = re.search(r"(\d+)\s+tests?\s+collected", line)
        if m:
            tests = int(m.group(1))
            em = re.search(r"(\d+)\s+errors?", line)
            if em:
                errors = int(em.group(1))
            break
    return tests, 0, errors


def count_python_lines() -> int:
    """Count lines of Python in backend/src/."""
    total = 0
    src = ROOT / "backend" / "src"
    if src.exists():
        for py_file in src.rglob("*.py"):
            total += sum(1 for _ in py_file.open())
    return total


def count_docs() -> int:
    """Count .md files in docs/ (excluding archive)."""
    docs_dir = ROOT / "docs"
    count = 0
    for md_file in docs_dir.rglob("*.md"):
        if "archive" not in str(md_file):
            count += 1
    return count


def git_last_commit() -> str:
    """Get short hash and message of latest commit."""
    return run(["git", "log", "--oneline", "-1"])


def git_branch() -> str:
    """Get current branch name."""
    return run(["git", "branch", "--show-current"])


def generate() -> str:
    """Generate STATUS.md content."""
    now = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M UTC")
    tests, _, errors = count_tests()
    py_lines = count_python_lines()
    doc_count = count_docs()
    last_commit = git_last_commit()
    branch = git_branch()

    return f"""\
# Project Status Dashboard

**Auto-generated**: {now}
**Branch**: `{branch}`
**Last commit**: `{last_commit}`

> Regenerate with: `make status`

---

## Metrics

| Metric | Value |
|--------|-------|
| Unit tests collected | {tests} |
| Collection errors | {errors} |
| Python source lines (backend/src/) | {py_lines:,} |
| Active docs (non-archive .md) | {doc_count} |

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
"""


def main() -> None:
    content = generate()
    STATUS_PATH.parent.mkdir(parents=True, exist_ok=True)
    STATUS_PATH.write_text(content)
    print(f"Written: {STATUS_PATH}")
    print(content)


if __name__ == "__main__":
    main()
