#!/usr/bin/env python3
"""Card designer CLI -- produce machine-loadable JSON payloads for punch cards.

Supports three historical card formats:
  hollerith  -- IBM/Hollerith 12-row x 80-col format (hollerith-tabulator)
  jacquard   -- N-hook binary row format (jacquard-loom)
  ae         -- Analytical Engine assembly source (analytical-engine)

Usage:
  python3 tools/card_designer.py hollerith "HELLO WORLD" [--cols 80]
  python3 tools/card_designer.py jacquard "10101010" "01010101" [--hooks 8]
  python3 tools/card_designer.py jacquard --stripe 4 [--hooks 8]
  python3 tools/card_designer.py ae --source "LOAD A, 5\\nHALT"
  python3 tools/card_designer.py ae < program.deck

Flags:
  --ascii      Print ASCII rendering before the JSON payload
  --out FILE   Write JSON to FILE instead of stdout
  --cols N     Number of columns for Hollerith card (default 80)
  --hooks N    Number of hooks per row for Jacquard card (default 8)
  --stripe N   Add N alternating twill pattern cards (jacquard only)

Output is a JSON object with a "payload" key, ready for:
  POST /machines/{id}/load    body: <JSON>
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path

# Ensure backend package is importable when run from project root or tools/
_REPO_ROOT = Path(__file__).resolve().parent.parent
if str(_REPO_ROOT) not in sys.path:
    sys.path.insert(0, str(_REPO_ROOT))

from backend.src.emulator.card_designer import (
    AECardDesigner,
    HollerithCardDesigner,
    JacquardCardDesigner,
)


def cmd_hollerith(args: argparse.Namespace) -> None:
    d = HollerithCardDesigner(cols=args.cols)
    if args.text:
        d.encode_text(" ".join(args.text), start_col=0)
    if args.ascii:
        print(d.render_ascii())
        print()
    payload_json = d.to_json()
    _output(payload_json, args.out)


def cmd_jacquard(args: argparse.Namespace) -> None:
    d = JacquardCardDesigner(hooks=args.hooks)
    if args.stripe:
        d.stripe(args.stripe)
    for pat in args.patterns or []:
        if set(pat) <= {"0", "1"}:
            d.add_row_from_string(pat)
        else:
            # Treat as comma-separated integers
            d.add_card([int(x) for x in pat.split(",")])
    if args.ascii:
        print(d.render_ascii())
        print()
    payload_json = d.to_json()
    _output(payload_json, args.out)


def cmd_ae(args: argparse.Namespace) -> None:
    if args.source:
        # Unescape literal \n sequences entered on the command line
        source = args.source.replace("\\n", "\n")
    else:
        source = sys.stdin.read()
    d = AECardDesigner(source)
    if args.ascii:
        print(d.render_ascii())
        print()
    payload_json = d.to_json()
    _output(payload_json, args.out)


def _output(payload_json: str, out_file: str | None) -> None:
    if out_file:
        Path(out_file).write_text(payload_json, encoding="utf-8")
        # Print a confirmation to stderr so stdout stays clean
        print(f"Written to {out_file}", file=sys.stderr)
    else:
        print(payload_json)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Punch card designer -- produce JSON payloads for the machines API.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    sub = parser.add_subparsers(dest="format", required=True)

    def _add_common(p: argparse.ArgumentParser) -> None:
        p.add_argument("--ascii", action="store_true", help="Print ASCII rendering first")
        p.add_argument("--out", metavar="FILE", help="Write JSON to FILE instead of stdout")

    # --- hollerith ---
    p_h = sub.add_parser("hollerith", help="IBM/Hollerith 12x80 card")
    p_h.add_argument("text", nargs="*", help="Text to encode (words joined with spaces)")
    p_h.add_argument("--cols", type=int, default=80, help="Number of columns (default 80)")
    _add_common(p_h)

    # --- jacquard ---
    p_j = sub.add_parser("jacquard", help="Jacquard N-hook binary card deck")
    p_j.add_argument(
        "patterns",
        nargs="*",
        help="Binary strings (e.g. '10101010') -- one per card row",
    )
    p_j.add_argument("--hooks", type=int, default=8, help="Hooks per row (default 8)")
    p_j.add_argument("--stripe", type=int, default=0, metavar="N", help="Add N twill cards")
    _add_common(p_j)

    # --- ae ---
    p_ae = sub.add_parser("ae", help="Analytical Engine assembly source")
    p_ae.add_argument(
        "--source",
        metavar="PROG",
        help="Assembly source string (use \\\\n for newlines). Reads stdin if omitted.",
    )
    _add_common(p_ae)

    args = parser.parse_args()

    if args.format == "hollerith":
        cmd_hollerith(args)
    elif args.format == "jacquard":
        cmd_jacquard(args)
    elif args.format == "ae":
        cmd_ae(args)


if __name__ == "__main__":
    main()
