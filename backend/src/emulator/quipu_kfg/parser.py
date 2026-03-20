"""Parser for Khipu Field Guide (KFG) normalized JSON."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any


@dataclass(frozen=True)
class KFGArtifact:
    investigator_num: str
    khipu: dict[str, Any]
    primary_cord: dict[str, Any]
    clusters: list[str]
    cords: list[dict[str, Any]]


def load_kfg_normalized(path: Path) -> KFGArtifact:
    data = json.loads(path.read_text(encoding="utf-8"))
    return KFGArtifact(
        investigator_num=data["investigator_num"],
        khipu=data.get("khipu", {}),
        primary_cord=data.get("primary_cord", {}),
        clusters=data.get("clusters", []),
        cords=data.get("cords", []),
    )
