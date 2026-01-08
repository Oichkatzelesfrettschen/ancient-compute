#!/usr/bin/env python3
"""Download referenced Wikimedia upload images from a Wikipedia page.

This is useful when upstream museum sites are blocked, but Wikipedia links to
Wikimedia-hosted object photos which can serve as reference material.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path
from urllib.parse import urlparse

import requests


UPLOAD_RE = re.compile(r"https://upload\.wikimedia\.org/wikipedia/commons/[^\"]+")


def download(url: str, out_path: Path) -> None:
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with requests.get(url, stream=True, timeout=30) as resp:
        resp.raise_for_status()
        with open(out_path, "wb") as handle:
            for chunk in resp.iter_content(chunk_size=1024 * 64):
                if chunk:
                    handle.write(chunk)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--page", required=True, help="Wikipedia page URL")
    parser.add_argument("--out-dir", required=True, type=Path)
    parser.add_argument("--limit", type=int, default=1, help="Max images to download")
    args = parser.parse_args()

    html = requests.get(args.page, timeout=30).text
    urls = list(dict.fromkeys(UPLOAD_RE.findall(html)))
    if not urls:
        raise SystemExit("No Wikimedia upload URLs found on page")

    for url in urls[: args.limit]:
        name = Path(urlparse(url).path).name
        out = args.out_dir / name
        download(url, out)
        print(out)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

