#!/usr/bin/env python3
"""Validate that card_compiler.py Opcode enum matches OPCODES.yaml.

Exits 0 if in sync, 1 on mismatch.
"""

from __future__ import annotations

import sys
from pathlib import Path

import yaml

REPO_ROOT = Path(__file__).resolve().parent.parent
OPCODES_YAML = REPO_ROOT / "docs" / "hardware" / "OPCODES.yaml"

# Import Opcode enum from card_compiler
sys.path.insert(0, str(REPO_ROOT / "tools"))
from card_compiler import Opcode  # noqa: E402


def main() -> int:
    with open(OPCODES_YAML) as f:
        data = yaml.safe_load(f)

    yaml_mnemonics = {entry["mnemonic"] for entry in data["opcodes"]}
    enum_mnemonics = {op.name for op in Opcode}

    if yaml_mnemonics == enum_mnemonics:
        print(f"OK: {len(yaml_mnemonics)} opcodes in sync")
        return 0

    only_yaml = yaml_mnemonics - enum_mnemonics
    only_enum = enum_mnemonics - yaml_mnemonics
    if only_yaml:
        print(f"In OPCODES.yaml but not in Opcode enum: {sorted(only_yaml)}")
    if only_enum:
        print(f"In Opcode enum but not in OPCODES.yaml: {sorted(only_enum)}")
    return 1


if __name__ == "__main__":
    sys.exit(main())
