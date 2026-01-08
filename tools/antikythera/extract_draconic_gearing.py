#!/usr/bin/env python3
"""Extract a minimal Draconic gearing parameter set from arXiv:2104.06181.

This is a first-pass extractor that targets the explicit tooth counts stated
in the PDF text (a1, r1, b1, s1). It emits a small YAML file suitable for
driving higher-fidelity Antikythera emulator models.
"""

from __future__ import annotations

import argparse
import re
import subprocess
from pathlib import Path

import yaml


def pdftotext(pdf_path: Path) -> str:
    result = subprocess.run(
        ["pdftotext", str(pdf_path), "-"],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
    )
    return result.stdout


_A1_R1_RE = re.compile(r"\ba1\s*=\s*(\d+)\s*teeth\b.*?\br1\s*=\s*(\d+)\s*teeth\b", re.IGNORECASE)
_B1_TEETH_RE = re.compile(r"\bgear\s+teeth\s+number\s+of\s+b1\s*=\s*(\d+)", re.IGNORECASE)
_B1_FALLBACK_RE = re.compile(r"(?<!/)\bb1\s*=\s*(\d+)", re.IGNORECASE)
_S1_TEETH_RE = re.compile(r"\bs1\s*=\s*([0-9]+(?:\.[0-9]+)?)\s*teeth\b", re.IGNORECASE)


def extract(text: str) -> dict:
    match = _A1_R1_RE.search(text)
    if not match:
        raise RuntimeError("Could not find a1/r1 tooth counts in PDF text")
    a1, r1 = (int(match.group(1)), int(match.group(2)))

    # The paper states b1 and then solves for s1. Prefer the first clear occurrences.
    b1_match = _B1_TEETH_RE.search(text) or _B1_FALLBACK_RE.search(text)
    s1_match = _S1_TEETH_RE.search(text)
    if not b1_match or not s1_match:
        raise RuntimeError("Could not find b1/s1 tooth counts in PDF text")
    b1 = int(b1_match.group(1))
    s1 = int(round(float(s1_match.group(1))))

    return {
        "source": {
            "id": "arxiv:2104.06181",
            "note": "Explicit tooth counts from text near equations (v)-(vi).",
        },
        "gears": {
            "a1": {"teeth": a1},
            "r1": {"teeth": r1},
            "b1": {"teeth": b1},
            "s1": {"teeth": s1},
        },
        "remarks": [
            "This is a partial parameter set; full train factors (b3/e1, e6/k2, ...) are not yet extracted.",
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pdf", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    args = parser.parse_args()

    text = pdftotext(args.pdf)
    payload = extract(text)

    args.out.parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w", encoding="utf-8") as handle:
        yaml.safe_dump(payload, handle, sort_keys=False)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
