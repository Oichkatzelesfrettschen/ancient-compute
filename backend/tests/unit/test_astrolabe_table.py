"""Tests for astrolabe emulator."""

from backend.src.emulator.astrolabe import AstrolabeEmulator, AstrolabeQuery


def test_astrolabe_solar_altitude_equinox_equator_noon():
    emu = AstrolabeEmulator()
    q = AstrolabeQuery(latitude_deg=0.0, date="2026-03-20", time="12:00", target="sun")
    assert emu.read_altitude(q) > 85.0
