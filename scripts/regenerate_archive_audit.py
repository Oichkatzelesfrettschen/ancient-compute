#!/usr/bin/env python3
"""Regenerate archive audit ledger and quarantine from current tree state."""

from __future__ import annotations

import csv
import re
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
ARCHIVE_DIR = ROOT / "docs/archive"
CSV_PATH = ARCHIVE_DIR / "audit_ledger.csv"
AUDIT_MD_PATH = ARCHIVE_DIR / "AUDIT_LEDGER.md"
QUARANTINE_MD_PATH = ARCHIVE_DIR / "QUARANTINE.md"

MARKDOWN_LINK_RE = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
BACKTICK_MD_RE = re.compile(r"`([^`\n]*\.md(?:#[^`\n]+)?)`")
FENCED_CODE_RE = re.compile(r"```.*?```", re.DOTALL)

CSV_FIELDS = [
    "file",
    "class",
    "archive_reason",
    "canonical_successors",
    "active_ref_count",
    "active_refs",
    "broken_link_count",
    "broken_links_sample",
    "metadata_present",
    "quarantine",
]


def _active_docs() -> list[Path]:
    active: list[Path] = []
    if (ROOT / "README.md").exists():
        active.append(ROOT / "README.md")
    active.extend((ROOT / "docs/general").rglob("*.md"))
    active.extend((ROOT / "docs/history").rglob("*.md"))
    active.extend((ROOT / "docs/minix").rglob("*.md"))
    active.extend((ROOT / "docs/simulation").rglob("*.md"))
    active.extend((ROOT / "docs/sources").rglob("*.md"))
    # Exclude archive itself from active refs.
    return sorted(set(p for p in active if "docs/archive" not in str(p)))


def _load_existing_rows() -> dict[str, dict[str, str]]:
    if not CSV_PATH.exists():
        return {}
    with CSV_PATH.open("r", encoding="utf-8", newline="") as handle:
        reader = csv.DictReader(handle)
        return {row["file"]: row for row in reader}


def _extract_md_links(text: str) -> list[str]:
    # Ignore code examples when auditing link integrity.
    text = FENCED_CODE_RE.sub("", text)
    links: list[str] = []
    for match in MARKDOWN_LINK_RE.finditer(text):
        links.append(match.group(1).strip())
    for match in BACKTICK_MD_RE.finditer(text):
        candidate = match.group(1).strip()
        if ";" in candidate or "," in candidate or " " in candidate:
            continue
        links.append(candidate)
    return links


def _resolve_md_link(source: Path, raw_link: str) -> Path | None:
    link = raw_link.split(" ", 1)[0]
    link = link.split("#", 1)[0]
    link = link.split("?", 1)[0].strip()

    if not link:
        return None
    if link.startswith(("http://", "https://", "mailto:")):
        return None

    if link.startswith("/"):
        return (ROOT / link.lstrip("/")).resolve()
    if link.startswith("./") or link.startswith("../"):
        return (source.parent / link).resolve()
    if "/" in link:
        return (ROOT / link).resolve()
    return (source.parent / link).resolve()


def _default_class(file_name: str) -> str:
    if any(token in file_name for token in ("ROADMAP", "PLAN", "TRACKER", "AUDIT", "INDEX")):
        return "functional_planning"
    return "historical_only"


def _default_reason(row_class: str) -> str:
    if row_class == "functional_planning":
        return "migration_or_audit_artifact"
    return "historical_context_retained"


def _build_rows() -> list[dict[str, str]]:
    existing = _load_existing_rows()
    active_docs = _active_docs()
    rows: list[dict[str, str]] = []

    for path in sorted(ARCHIVE_DIR.rglob("*")):
        if not path.is_file():
            continue
        if path.suffix.lower() not in {".md", ".txt"}:
            continue
        if path.name in {"AUDIT_LEDGER.md", "QUARANTINE.md"}:
            continue

        rel = path.relative_to(ROOT).as_posix()
        row = existing.get(rel, {})
        text = path.read_text(encoding="utf-8", errors="ignore")

        links = _extract_md_links(text) if path.suffix.lower() == ".md" else []
        broken_links: list[str] = []
        for link in links:
            resolved = _resolve_md_link(path, link)
            if resolved is None:
                continue
            if not resolved.exists():
                broken_links.append(link)

        active_refs = []
        for active_path in active_docs:
            if rel in active_path.read_text(encoding="utf-8", errors="ignore"):
                active_refs.append(active_path.relative_to(ROOT).as_posix())

        row_class = row.get("class") or _default_class(path.name)
        archive_reason = row.get("archive_reason") or _default_reason(row_class)
        canonical_successors = row.get("canonical_successors", "")
        metadata_present = "yes" if "archive metadata" in text.lower() else "no"
        broken_count = len(broken_links)
        rows.append(
            {
                "file": rel,
                "class": row_class,
                "archive_reason": archive_reason,
                "canonical_successors": canonical_successors,
                "active_ref_count": str(len(active_refs)),
                "active_refs": ";".join(active_refs),
                "broken_link_count": str(broken_count),
                "broken_links_sample": ";".join(broken_links[:3]),
                "metadata_present": metadata_present,
                "quarantine": "yes" if broken_count >= 5 else "no",
            }
        )

    return rows


def _write_csv(rows: list[dict[str, str]]) -> None:
    with CSV_PATH.open("w", encoding="utf-8", newline="") as handle:
        writer = csv.DictWriter(handle, fieldnames=CSV_FIELDS)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)


def _write_audit_markdown(rows: list[dict[str, str]]) -> None:
    class_counts = Counter(row["class"] for row in rows)
    metadata_count = sum(1 for row in rows if row["metadata_present"] == "yes")
    broken_files = [row for row in rows if int(row["broken_link_count"]) > 0]
    quarantine_rows = [row for row in rows if row["quarantine"] == "yes"]
    total_broken = sum(int(row["broken_link_count"]) for row in rows)
    active_refd = sum(1 for row in rows if int(row["active_ref_count"]) > 0)

    lines = [
        "# Archive Audit Ledger",
        "",
        "Date: 2026-02-24",
        "Status: Active",
        "Scope: Full inventory of legacy files under `docs/archive`.",
        "",
        "## Summary",
        "",
        f"- Total legacy archive files audited: {len(rows)}",
        f"- Class `functional_planning`: {class_counts.get('functional_planning', 0)}",
        f"- Class `functional_design_code`: {class_counts.get('functional_design_code', 0)}",
        f"- Class `historical_only`: {class_counts.get('historical_only', 0)}",
        f"- Files with archive metadata section: {metadata_count}",
        f"- Files referenced by active docs: {active_refd}",
        f"- Files with one or more broken internal links: {len(broken_files)}",
        f"- Total broken internal links (markdown files only): {total_broken}",
        f"- Quarantine candidates (`broken_link_count >= 5`): {len(quarantine_rows)}",
        "",
        "## Quarantine Candidates",
        "",
        "| File | Broken Links | Canonical Successor(s) |",
        "|---|---:|---|",
    ]
    if quarantine_rows:
        for row in quarantine_rows:
            lines.append(
                f"| `{row['file']}` | {row['broken_link_count']} | "
                f"`{row['canonical_successors'] or 'n/a'}` |"
            )
    else:
        lines.append("| _none_ | 0 | n/a |")

    lines.extend(
        [
            "",
            "## Full Ledger",
            "",
            "Source of truth: `docs/archive/audit_ledger.csv`",
            "",
            "| File | Class | Reason | Metadata | Active Refs | Broken Links | Quarantine |",
            "|---|---|---|---|---:|---:|---|",
        ]
    )
    for row in rows:
        lines.append(
            f"| `{row['file']}` | `{row['class']}` | `{row['archive_reason']}` | "
            f"{row['metadata_present']} | {row['active_ref_count']} | "
            f"{row['broken_link_count']} | {row['quarantine']} |"
        )

    AUDIT_MD_PATH.write_text("\n".join(lines) + "\n", encoding="utf-8")


def _write_quarantine_markdown(rows: list[dict[str, str]]) -> None:
    quarantine_rows = [row for row in rows if row["quarantine"] == "yes"]
    lines = [
        "# Archive Quarantine",
        "",
        "Date: 2026-02-24",
        "Status: Active",
        "Purpose: Identify archived files with degraded internal link integrity so they are not used as build/planning design inputs.",
        "",
        "## Criteria",
        "",
        "A file is quarantined when either condition is true:",
        "",
        "1. `broken_link_count >= 5` in `docs/archive/audit_ledger.csv`.",
        "2. Path patterns are malformed enough to mislead navigation during active implementation work.",
        "",
        "## Current Quarantine Set",
        "",
        "| File | Risk | Why Quarantined | Use Instead |",
        "|---|---|---|---|",
    ]

    if quarantine_rows:
        for row in quarantine_rows:
            risk = "high" if int(row["broken_link_count"]) >= 15 else "medium"
            successors = row["canonical_successors"] or "docs/general/PLANNING_CANONICAL_MAP.md"
            lines.append(
                f"| `{row['file']}` | {risk} | "
                f"{row['broken_link_count']} broken internal links from current audit | "
                f"`{successors}` |"
            )
    else:
        lines.append("| _none_ | low | no files exceed threshold | n/a |")

    lines.extend(
        [
            "",
            "## Handling Rules",
            "",
            "1. Do not cite quarantined files as canonical implementation guidance.",
            "2. Keep quarantined files for historical provenance only.",
            "3. When practical, repair links and remove from quarantine after re-audit.",
            "",
            "## Exit Criteria",
            "",
            "A quarantined file can be removed from this list when:",
            "",
            "1. Broken internal links are reduced below 5.",
            "2. Replacement pointers remain explicit in the archive metadata.",
            "3. `docs/archive/audit_ledger.csv` and `docs/archive/AUDIT_LEDGER.md` are regenerated.",
        ]
    )
    QUARANTINE_MD_PATH.write_text("\n".join(lines) + "\n", encoding="utf-8")


def main() -> int:
    rows = _build_rows()
    _write_csv(rows)
    _write_audit_markdown(rows)
    _write_quarantine_markdown(rows)
    print(f"Regenerated archive audit artifacts for {len(rows)} files")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
