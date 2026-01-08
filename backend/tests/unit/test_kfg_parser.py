"""Tests for KFG normalized parser."""

from pathlib import Path

from backend.src.emulator.quipu_kfg import load_kfg_normalized


def test_kfg_parser_qu001_loads():
    repo_root = Path(__file__).resolve().parents[3]
    path = repo_root / "docs/sources/quipu/kfg_normalized/QU001.json"
    artifact = load_kfg_normalized(path)
    assert artifact.investigator_num == "QU001"
    assert len(artifact.cords) == 33
