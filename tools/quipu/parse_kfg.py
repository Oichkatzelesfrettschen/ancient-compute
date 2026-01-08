#!/usr/bin/env python3

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

import pandas as pd


@dataclass(frozen=True)
class KFGArtifact:
    investigator_num: str
    khipu: dict
    primary_cord: dict
    clusters: list[str]
    cords: list[dict]


def _parse_kv_sheet(xlsx: pd.ExcelFile, sheet: str) -> dict:
    df = xlsx.parse(sheet, header=None)
    items: dict[str, str] = {}
    for raw in df[0].dropna().astype(str).tolist():
        line = raw.strip()
        if not line or line.startswith('!-') or line.startswith('!--') or line.startswith('!'):
            continue
        if ':' in line:
            key, value = line.split(':', 1)
            items[key.strip()] = value.strip()
    return items


def parse_kfg_xlsx(path: Path) -> KFGArtifact:
    xlsx = pd.ExcelFile(path)

    khipu = _parse_kv_sheet(xlsx, 'Khipu')
    primary_cord = _parse_kv_sheet(xlsx, 'PrimaryCord')

    clusters_df = xlsx.parse('Clusters', header=None)
    clusters = [str(v).strip() for v in clusters_df[0].dropna().tolist() if str(v).strip()]
    clusters = [c for c in clusters if not c.startswith('!')]

    cords_df = xlsx.parse('Cords')
    cords = cords_df.fillna('').to_dict(orient='records')

    investigator_num = khipu.get('Investigator_Num')
    if not investigator_num:
        inv_raw = next((str(v) for v in xlsx.parse('Khipu', header=None)[0].dropna().tolist() if 'Investigator_Num' in str(v)), '')
        investigator_num = inv_raw.split(':', 1)[1].strip() if ':' in inv_raw else path.stem

    return KFGArtifact(
        investigator_num=investigator_num,
        khipu=khipu,
        primary_cord=primary_cord,
        clusters=clusters,
        cords=cords,
    )


def main() -> None:
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('xlsx', type=Path)
    parser.add_argument('--out', type=Path, required=True)
    args = parser.parse_args()

    artifact = parse_kfg_xlsx(args.xlsx)

    payload = {
        'investigator_num': artifact.investigator_num,
        'khipu': artifact.khipu,
        'primary_cord': artifact.primary_cord,
        'clusters': artifact.clusters,
        'cords': artifact.cords,
    }

    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(payload, indent=2, sort_keys=True), encoding='utf-8')


if __name__ == '__main__':
    main()
