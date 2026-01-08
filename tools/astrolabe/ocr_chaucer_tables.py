#!/usr/bin/env python3
"""OCR helper for the 1872 Chaucer astrolabe treatise scan.

Many Google Books PDFs are image-only. This tool converts selected pages to PNG
with `pdftoppm` and runs Tesseract via pytesseract.

Output is raw text intended for manual cleanup into structured tables.
"""

from __future__ import annotations

import argparse
import subprocess
from pathlib import Path

import pytesseract
from PIL import Image


def render_pages(pdf: Path, out_dir: Path, first: int, last: int, dpi: int) -> list[Path]:
    out_dir.mkdir(parents=True, exist_ok=True)
    prefix = out_dir / "page"
    subprocess.run(
        [
            "pdftoppm",
            "-f",
            str(first),
            "-l",
            str(last),
            "-r",
            str(dpi),
            "-png",
            str(pdf),
            str(prefix),
        ],
        check=True,
    )
    return sorted(out_dir.glob("page-*.png"))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--pdf", required=True, type=Path)
    parser.add_argument("--out-dir", required=True, type=Path)
    parser.add_argument("--first", type=int, required=True)
    parser.add_argument("--last", type=int, required=True)
    parser.add_argument("--dpi", type=int, default=300)
    args = parser.parse_args()

    images = render_pages(args.pdf, args.out_dir, args.first, args.last, args.dpi)
    texts = []
    for img_path in images:
        img = Image.open(img_path)
        text = pytesseract.image_to_string(img)
        texts.append(f"===== {img_path.name} =====\n{text}\n")

    out_text = args.out_dir / "ocr.txt"
    out_text.write_text("\n".join(texts), encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

