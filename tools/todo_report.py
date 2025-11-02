#!/usr/bin/env python3
"""
Generate a consolidated TODO/FIXME report for the repository.

Scans for common placeholders (TODO, FIXME, XXX, TBD, HACK) and outputs
`TODO_REPORT.md` at the repo root with a grouped index by file.

Usage:
  python tools/todo_report.py
"""

from __future__ import annotations

import re
import os
from pathlib import Path
from typing import Dict, List, Tuple

ROOT = Path(__file__).resolve().parents[1]
OUTPUT = ROOT / "TODO_REPORT.md"

PATTERNS = [r"TODO", r"FIXME", r"XXX", r"TBD", r"HACK"]
REGEX = re.compile(r"|".join(PATTERNS), re.IGNORECASE)


def iter_project_files() -> List[Path]:
    ignore_dirs = {".git", "venv", "node_modules", "dist", "build", "__pycache__"}
    files: List[Path] = []
    for root, dirs, filenames in os.walk(ROOT):
        # prune ignored directories
        dirs[:] = [d for d in dirs if d not in ignore_dirs]
        for name in filenames:
            p = Path(root) / name
            # skip binary-like files
            if p.suffix.lower() in {".png", ".jpg", ".jpeg", ".gif", ".pdf", ".svg", ".ico"}:
                continue
            files.append(p)
    return files


def scan_file(path: Path) -> List[Tuple[int, str]]:
    results: List[Tuple[int, str]] = []
    try:
        with path.open("r", encoding="utf-8", errors="ignore") as f:
            for i, line in enumerate(f, start=1):
                if REGEX.search(line):
                    results.append((i, line.rstrip()))
    except Exception:
        pass
    return results


def main() -> None:
    index: Dict[str, List[Tuple[int, str]]] = {}
    for p in iter_project_files():
        matches = scan_file(p)
        if matches:
            rel = str(p.relative_to(ROOT))
            index[rel] = matches

    total = sum(len(v) for v in index.values())

    with OUTPUT.open("w", encoding="utf-8") as out:
        out.write("# TODO / FIXME Report\n\n")
        out.write(f"Generated from repository scan. Total items: {total}.\n\n")
        out.write("Note: Treat warnings as errors — prioritize critical items first.\n\n")
        for rel, items in sorted(index.items()):
            out.write(f"## {rel}\n")
            for line_no, text in items:
                out.write(f"- L{line_no}: {text}\n")
            out.write("\n")

    print(f"Report written to {OUTPUT}")


if __name__ == "__main__":
    main()

