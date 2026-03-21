"""Tests for KFG-driven quipu emulator."""

from pathlib import Path

from backend.src.emulator.quipu_kfg import (
    KFGArtifact,
    QuipuKFGEmulator,
    QuipuSummary,
    load_kfg_normalized,
)

_REPO_ROOT = Path(__file__).resolve().parents[3]
_KFG_DIR = _REPO_ROOT / "docs/sources/quipu/kfg_normalized"


def test_quipu_kfg_summary_qu001():
    path = _KFG_DIR / "QU001.json"
    artifact = load_kfg_normalized(path)
    emu = QuipuKFGEmulator(artifact)
    summary = emu.summarize()
    assert summary.investigator_num == "QU001"
    assert summary.cord_count == 33


class TestQuipuKFGEmulatorQU001:
    def _emu(self) -> QuipuKFGEmulator:
        return QuipuKFGEmulator(load_kfg_normalized(_KFG_DIR / "QU001.json"))

    def test_total_value_returns_int(self) -> None:
        assert isinstance(self._emu().total_value(), int)

    def test_total_value_non_negative(self) -> None:
        assert self._emu().total_value() >= 0

    def test_total_value_zero_for_qu001(self) -> None:
        # QU001 cords all have Value=0
        assert self._emu().total_value() == 0

    def test_summarize_returns_quipu_summary(self) -> None:
        s = self._emu().summarize()
        assert isinstance(s, QuipuSummary)

    def test_summarize_investigator_num(self) -> None:
        assert self._emu().summarize().investigator_num == "QU001"

    def test_summarize_cord_count(self) -> None:
        assert self._emu().summarize().cord_count == 33

    def test_summarize_total_value_matches_total_value(self) -> None:
        emu = self._emu()
        assert emu.summarize().total_value == emu.total_value()

    def test_artifact_stored_on_emulator(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        emu = QuipuKFGEmulator(a)
        assert emu.artifact is a

    def test_summary_is_frozen(self) -> None:
        import pytest

        s = self._emu().summarize()
        with pytest.raises((AttributeError, TypeError)):
            s.cord_count = 0  # type: ignore[misc]


class TestQuipuKFGEmulatorUR001:
    def _emu(self) -> QuipuKFGEmulator:
        return QuipuKFGEmulator(load_kfg_normalized(_KFG_DIR / "UR001.json"))

    def test_cord_count_572(self) -> None:
        assert self._emu().summarize().cord_count == 572

    def test_total_value_positive(self) -> None:
        # UR001 has numeric cord values
        assert self._emu().total_value() > 0

    def test_total_value_known(self) -> None:
        assert self._emu().total_value() == 93145

    def test_investigator_num(self) -> None:
        assert self._emu().summarize().investigator_num == "UR001"

    def test_summary_cord_count_matches_artifact(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        emu = QuipuKFGEmulator(a)
        assert emu.summarize().cord_count == len(a.cords)


class TestQuipuKFGEmulatorSyntheticArtifact:
    """Test emulator with a handcrafted artifact for precise value assertions."""

    def _make(self, cords: list[dict]) -> QuipuKFGEmulator:
        a = KFGArtifact(
            investigator_num="SYN001",
            khipu={},
            primary_cord={},
            clusters=[],
            cords=cords,
        )
        return QuipuKFGEmulator(a)

    def test_empty_cords_total_zero(self) -> None:
        emu = self._make([])
        assert emu.total_value() == 0

    def test_single_cord_value(self) -> None:
        emu = self._make([{"Value": 42}])
        assert emu.total_value() == 42

    def test_multiple_cords_sum(self) -> None:
        emu = self._make([{"Value": 10}, {"Value": 20}, {"Value": 30}])
        assert emu.total_value() == 60

    def test_missing_value_key_treated_as_zero(self) -> None:
        emu = self._make([{"Color": "red"}, {"Value": 5}])
        assert emu.total_value() == 5

    def test_none_value_treated_as_zero(self) -> None:
        emu = self._make([{"Value": None}, {"Value": 7}])
        assert emu.total_value() == 7

    def test_non_numeric_value_skipped(self) -> None:
        emu = self._make([{"Value": "XYZ"}, {"Value": 3}])
        assert emu.total_value() == 3

    def test_summarize_synthetic(self) -> None:
        emu = self._make([{"Value": 1}, {"Value": 2}])
        s = emu.summarize()
        assert s.investigator_num == "SYN001"
        assert s.cord_count == 2
        assert s.total_value == 3
