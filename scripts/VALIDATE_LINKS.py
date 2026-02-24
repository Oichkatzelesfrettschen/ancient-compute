#!/usr/bin/env python3
"""Validate markdown cross-references with repo-aware path resolution."""

from __future__ import annotations

import argparse
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
DOCS_ROOT = ROOT / "docs"
ARCHIVE_ROOT = DOCS_ROOT / "archive"

SKIP_DIRS = {
    ".git",
    "node_modules",
    "__pycache__",
    "venv",
    ".venv",
    ".pytest_cache",
    ".mypy_cache",
    ".ruff_cache",
    "dist",
    "build",
    "target",
    ".backup_20251031_192302",
    ".backup_links_20251031_204331",
}

MARKDOWN_LINK_RE = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
BACKTICK_MD_RE = re.compile(r"`([^`\n]*\.md(?:#[^`\n]+)?)`")
FENCED_CODE_RE = re.compile(r"```.*?```", re.DOTALL)

ACTIVE_DOCS = [
    "README.md",
    "docs/general/INDEX.md",
    "docs/general/MASTER_ROADMAP.md",
    "docs/general/TODO_TRACKER.md",
    "docs/general/PLANNING_CANONICAL_MAP.md",
    "docs/general/HARDWARE_LANGUAGE_BRINGUP_PLAN.md",
    "docs/general/EXECUTION_RESCOPED_PLAN_2026-02.md",
    "docs/general/TEST_GATE_MATRIX.md",
    "docs/general/TEST_QUARANTINE.md",
    "docs/simulation/PARAMETERS.md",
    "docs/simulation/CITATIONS.md",
    "docs/simulation/GAP_REPORT.md",
]

def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--scope",
        choices=("active", "archive", "all"),
        default="active",
        help="Validation scope (default: active canonical docs).",
    )
    return parser.parse_args()


def iter_markdown_files(scope: str) -> list[Path]:
    if scope == "active":
        files = []
        for rel in ACTIVE_DOCS:
            path = ROOT / rel
            if path.exists():
                files.append(path)
        return sorted(set(files))

    if scope == "archive":
        files = []
        for path in ARCHIVE_ROOT.rglob("*.md"):
            if any(part in SKIP_DIRS for part in path.parts):
                continue
            files.append(path)
        return sorted(set(files))

    files: list[Path] = []
    if (ROOT / "README.md").exists():
        files.append(ROOT / "README.md")

    for path in DOCS_ROOT.rglob("*.md"):
        if any(part in SKIP_DIRS for part in path.parts):
            continue
        files.append(path)
    return sorted(set(files))


def extract_links(markdown_path: Path) -> list[str]:
    text = markdown_path.read_text(encoding="utf-8", errors="ignore")
    # Ignore code examples; we only validate prose/documentation references.
    text = FENCED_CODE_RE.sub("", text)
    links: list[str] = []

    for match in MARKDOWN_LINK_RE.finditer(text):
        links.append(match.group(1).strip())
    for match in BACKTICK_MD_RE.finditer(text):
        candidate = match.group(1).strip()
        # Ignore compound values from ledger cells.
        if ";" in candidate or "," in candidate or " " in candidate:
            continue
        links.append(candidate)

    return links


def normalize_target(source: Path, raw_link: str) -> Path | None:
    # Drop optional title fragments and anchors.
    link = raw_link.split(" ", 1)[0]
    link = link.split("#", 1)[0]
    link = link.split("?", 1)[0].strip()

    if not link:
        return None

    if link.startswith(("http://", "https://", "mailto:")):
        return None

    if link.startswith("/"):
        return ROOT / link.lstrip("/")

    if link.startswith("./") or link.startswith("../"):
        return (source.parent / link).resolve()

    # Treat repo-root style markdown references as root-relative.
    if "/" in link:
        return (ROOT / link).resolve()

    # Fallback: same-directory file.
    return (source.parent / link).resolve()


def main() -> int:
    args = parse_args()
    markdown_files = iter_markdown_files(args.scope)
    broken: list[tuple[Path, str, Path]] = []
    valid = 0

    for md_file in markdown_files:
        for raw_link in extract_links(md_file):
            target = normalize_target(md_file, raw_link)
            if target is None:
                continue
            if target.exists():
                valid += 1
            else:
                broken.append((md_file, raw_link, target))

    total_checked = valid + len(broken)
    success_rate = 100.0 if total_checked == 0 else (100.0 * valid / total_checked)

    print(f"Scope: {args.scope}")
    print(f"Found {len(markdown_files)} markdown files")
    print("\n=== LINK VALIDATION RESULTS ===")
    print(f"Valid links: {valid}")
    print(f"Invalid/Broken links: {len(broken)}")
    print(f"Success rate: {success_rate:.1f}%\n")

    if broken:
        print(f"Found {len(broken)} broken links:\n")
        for source, link, resolved in broken[:50]:
            print(f"File: {source.relative_to(ROOT)}")
            print(f"  Link: {link}")
            print(f"  Resolved to: {resolved}")
            print()
        if len(broken) > 50:
            print(f"... and {len(broken) - 50} more broken links")
        return 1

    print("No broken links found! -> OK")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
