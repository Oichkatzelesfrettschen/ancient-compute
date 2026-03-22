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

    def test_large_number_of_cords(self) -> None:
        cords = [{"Value": i} for i in range(100)]
        emu = self._make(cords)
        assert emu.total_value() == sum(range(100))  # 4950

    def test_all_zero_values(self) -> None:
        cords = [{"Value": 0}] * 10
        emu = self._make(cords)
        assert emu.total_value() == 0
        assert emu.summarize().cord_count == 10

    def test_float_value_truncated_or_added(self) -> None:
        emu = self._make([{"Value": 1.9}])
        # Should not raise; value treated as numeric
        val = emu.total_value()
        assert isinstance(val, int)

    def test_summary_cord_count_matches_cords_length(self) -> None:
        cords = [{"Value": 1}] * 7
        emu = self._make(cords)
        assert emu.summarize().cord_count == 7

    def test_synthetic_total_equals_summary_total(self) -> None:
        cords = [{"Value": 10}, {"Value": 20}, {"Value": 30}]
        emu = self._make(cords)
        assert emu.total_value() == emu.summarize().total_value

    def test_empty_artifact_summary(self) -> None:
        emu = self._make([])
        s = emu.summarize()
        assert s.cord_count == 0
        assert s.total_value == 0


class TestQuipuKFGArtifactLoading:
    """Verify KFGArtifact fields from load_kfg_normalized."""

    def _artifact(self, filename: str) -> KFGArtifact:
        return load_kfg_normalized(_KFG_DIR / filename)

    def test_qu001_investigator_num(self) -> None:
        a = self._artifact("QU001.json")
        assert a.investigator_num == "QU001"

    def test_qu001_cords_is_list(self) -> None:
        a = self._artifact("QU001.json")
        assert isinstance(a.cords, list)

    def test_qu001_cord_count_33(self) -> None:
        a = self._artifact("QU001.json")
        assert len(a.cords) == 33

    def test_ur001_cord_count_572(self) -> None:
        a = self._artifact("UR001.json")
        assert len(a.cords) == 572

    def test_artifact_has_primary_cord(self) -> None:
        a = self._artifact("QU001.json")
        assert isinstance(a.primary_cord, dict)

    def test_artifact_has_khipu(self) -> None:
        a = self._artifact("QU001.json")
        assert isinstance(a.khipu, dict)

    def test_artifact_clusters_is_list(self) -> None:
        a = self._artifact("QU001.json")
        assert isinstance(a.clusters, list)


class TestQuipuKFGEmulatorSummary:
    """Summary field value checks."""

    def _emu(self, filename: str) -> "QuipuKFGEmulator":
        a = load_kfg_normalized(_KFG_DIR / filename)
        return QuipuKFGEmulator(a)

    def test_qu001_cord_count_in_summary(self) -> None:
        emu = self._emu("QU001.json")
        assert emu.summarize().cord_count == 33

    def test_ur001_cord_count_in_summary(self) -> None:
        emu = self._emu("UR001.json")
        assert emu.summarize().cord_count == 572

    def test_summary_investigator_num_qu001(self) -> None:
        emu = self._emu("QU001.json")
        assert emu.summarize().investigator_num == "QU001"

    def test_summary_investigator_num_ur001(self) -> None:
        emu = self._emu("UR001.json")
        assert emu.summarize().investigator_num == "UR001"

    def test_total_value_is_int_or_float(self) -> None:
        emu = self._emu("QU001.json")
        assert isinstance(emu.summarize().total_value, (int, float))

    def test_total_value_method_matches_summary(self) -> None:
        emu = self._emu("QU001.json")
        assert emu.total_value() == emu.summarize().total_value


class TestQuipuKFGEmulatorAPI:
    """Emulator API and artifact access."""

    def _emu(self, filename: str) -> "QuipuKFGEmulator":
        a = load_kfg_normalized(_KFG_DIR / filename)
        return QuipuKFGEmulator(a)

    def test_artifact_is_kfg_artifact_instance(self) -> None:
        emu = self._emu("QU001.json")
        assert isinstance(emu.artifact, KFGArtifact)

    def test_summarize_returns_quipu_summary_instance(self) -> None:
        emu = self._emu("QU001.json")
        assert isinstance(emu.summarize(), QuipuSummary)

    def test_total_value_non_negative(self) -> None:
        emu = self._emu("QU001.json")
        assert emu.total_value() >= 0

    def test_two_emus_from_same_artifact_same_summary(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        e1 = QuipuKFGEmulator(a)
        e2 = QuipuKFGEmulator(a)
        assert e1.summarize().cord_count == e2.summarize().cord_count

    def test_ur001_total_value_is_numeric(self) -> None:
        emu = self._emu("UR001.json")
        assert isinstance(emu.total_value(), (int, float))


class TestQuipuKFGSummaryFields:
    """QuipuSummary field type and value invariants."""

    def _emu(self, filename: str) -> "QuipuKFGEmulator":
        a = load_kfg_normalized(_KFG_DIR / filename)
        return QuipuKFGEmulator(a)

    def test_cord_count_is_non_negative_int(self) -> None:
        s = self._emu("QU001.json").summarize()
        assert isinstance(s.cord_count, int)
        assert s.cord_count >= 0

    def test_investigator_num_is_string(self) -> None:
        s = self._emu("QU001.json").summarize()
        assert isinstance(s.investigator_num, str)

    def test_total_value_is_numeric(self) -> None:
        s = self._emu("QU001.json").summarize()
        assert isinstance(s.total_value, (int, float))

    def test_ur001_investigator_num_not_empty(self) -> None:
        s = self._emu("UR001.json").summarize()
        assert len(s.investigator_num) > 0
