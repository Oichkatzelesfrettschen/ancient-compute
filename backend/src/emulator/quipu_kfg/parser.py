"""Parser for Khipu Field Guide (KFG) normalized JSON."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class KFGArtifact:
    investigator_num: str
    khipu: dict
    primary_cord: dict
    clusters: list[str]
    cords: list[dict]


def load_kfg_normalized(path: Path) -> KFGArtifact:
    data = json.loads(path.read_text(encoding="utf-8"))
    return KFGArtifact(
        investigator_num=data["investigator_num"],
        khipu=data.get("khipu", {}),
        primary_cord=data.get("primary_cord", {}),
        clusters=data.get("clusters", []),
        cords=data.get("cords", []),
    )
