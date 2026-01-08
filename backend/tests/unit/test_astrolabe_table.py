"""Tests for table-driven astrolabe emulator."""

from pathlib import Path

import pytest

from backend.src.emulator.astrolabe import AstrolabeEmulator, AstrolabeQuery


def test_astrolabe_lookup_hit():
    repo_root = Path(__file__).resolve().parents[3]
    table = repo_root / "docs/sources/astrolabe/reference_table.json"
    emu = AstrolabeEmulator(table)

    q = AstrolabeQuery(latitude_deg=51.5, date="1872-01-01", time="00:00", target="demo")
    assert emu.read_altitude(q) == 45.0


def test_astrolabe_lookup_miss():
    repo_root = Path(__file__).resolve().parents[3]
    table = repo_root / "docs/sources/astrolabe/reference_table.json"
    emu = AstrolabeEmulator(table)

    q = AstrolabeQuery(latitude_deg=0.0, date="1872-01-01", time="00:00", target="demo")
    with pytest.raises(KeyError):
        emu.read_altitude(q)
