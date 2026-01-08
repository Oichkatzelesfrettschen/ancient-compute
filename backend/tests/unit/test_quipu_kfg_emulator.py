"""Tests for KFG-driven quipu emulator."""

from pathlib import Path

from backend.src.emulator.quipu_kfg import QuipuKFGEmulator, load_kfg_normalized


def test_quipu_kfg_summary_qu001():
    repo_root = Path(__file__).resolve().parents[3]
    path = repo_root / "docs/sources/quipu/kfg_normalized/QU001.json"
    artifact = load_kfg_normalized(path)

    emu = QuipuKFGEmulator(artifact)
    summary = emu.summarize()

    assert summary.investigator_num == "QU001"
    assert summary.cord_count == 33
