"""Tests for KFG normalized parser."""

from pathlib import Path

from backend.src.emulator.quipu_kfg import KFGArtifact, load_kfg_normalized

_REPO_ROOT = Path(__file__).resolve().parents[3]
_KFG_DIR = _REPO_ROOT / "docs/sources/quipu/kfg_normalized"


def test_kfg_parser_qu001_loads():
    path = _KFG_DIR / "QU001.json"
    artifact = load_kfg_normalized(path)
    assert artifact.investigator_num == "QU001"
    assert len(artifact.cords) == 33


class TestKFGParserQU001:
    def _artifact(self) -> KFGArtifact:
        return load_kfg_normalized(_KFG_DIR / "QU001.json")

    def test_investigator_num_is_string(self) -> None:
        a = self._artifact()
        assert isinstance(a.investigator_num, str)

    def test_khipu_is_dict(self) -> None:
        a = self._artifact()
        assert isinstance(a.khipu, dict)

    def test_primary_cord_is_dict(self) -> None:
        a = self._artifact()
        assert isinstance(a.primary_cord, dict)

    def test_clusters_is_list(self) -> None:
        a = self._artifact()
        assert isinstance(a.clusters, list)

    def test_cords_is_list(self) -> None:
        a = self._artifact()
        assert isinstance(a.cords, list)

    def test_cluster_count(self) -> None:
        a = self._artifact()
        assert len(a.clusters) == 12

    def test_cords_are_dicts(self) -> None:
        a = self._artifact()
        assert all(isinstance(c, dict) for c in a.cords)

    def test_artifact_is_frozen(self) -> None:
        import pytest

        a = self._artifact()
        with pytest.raises((AttributeError, TypeError)):
            a.investigator_num = "CHANGED"  # type: ignore[misc]

    def test_load_is_deterministic(self) -> None:
        a1 = self._artifact()
        a2 = self._artifact()
        assert a1.investigator_num == a2.investigator_num
        assert len(a1.cords) == len(a2.cords)


class TestKFGParserUR001:
    def _artifact(self) -> KFGArtifact:
        return load_kfg_normalized(_KFG_DIR / "UR001.json")

    def test_investigator_num(self) -> None:
        a = self._artifact()
        assert a.investigator_num == "UR001"

    def test_cord_count(self) -> None:
        a = self._artifact()
        assert len(a.cords) == 572

    def test_clusters_non_empty(self) -> None:
        a = self._artifact()
        assert len(a.clusters) > 0

    def test_clusters_are_strings(self) -> None:
        a = self._artifact()
        assert all(isinstance(c, str) for c in a.clusters)

    def test_cords_have_value_key_where_present(self) -> None:
        # At least some cords should have a Value field
        a = self._artifact()
        has_value = any("Value" in c for c in a.cords)
        assert has_value
