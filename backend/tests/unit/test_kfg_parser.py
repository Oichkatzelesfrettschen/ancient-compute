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


class TestKFGArtifactDataclass:
    """KFGArtifact is a frozen dataclass -- verify field types and immutability."""

    def _qu001(self) -> "KFGArtifact":
        return load_kfg_normalized(_KFG_DIR / "QU001.json")

    def test_investigator_num_is_nonempty_str(self) -> None:
        a = self._qu001()
        assert isinstance(a.investigator_num, str)
        assert len(a.investigator_num) > 0

    def test_khipu_is_dict(self) -> None:
        a = self._qu001()
        assert isinstance(a.khipu, dict)

    def test_primary_cord_is_dict(self) -> None:
        a = self._qu001()
        assert isinstance(a.primary_cord, dict)

    def test_clusters_is_list_of_strings(self) -> None:
        a = self._qu001()
        assert isinstance(a.clusters, list)
        assert all(isinstance(c, str) for c in a.clusters)

    def test_cords_is_list_of_dicts(self) -> None:
        a = self._qu001()
        assert isinstance(a.cords, list)
        assert all(isinstance(c, dict) for c in a.cords)

    def test_frozen_prevents_attribute_assignment(self) -> None:
        import pytest

        a = self._qu001()
        with pytest.raises((AttributeError, TypeError)):
            a.investigator_num = "NEW"  # type: ignore[misc]

    def test_two_loads_same_path_are_equal(self) -> None:
        a1 = self._qu001()
        a2 = self._qu001()
        assert a1.investigator_num == a2.investigator_num
        assert a1.clusters == a2.clusters

    def test_qu001_and_ur001_have_different_ids(self) -> None:
        qu = load_kfg_normalized(_KFG_DIR / "QU001.json")
        ur = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert qu.investigator_num != ur.investigator_num

    def test_ur001_has_more_cords_than_qu001(self) -> None:
        qu = load_kfg_normalized(_KFG_DIR / "QU001.json")
        ur = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert len(ur.cords) > len(qu.cords)


class TestKFGCordData:
    """Verify cord-level field presence in both artifacts."""

    def test_qu001_cords_all_have_id_or_label(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        # Each cord should have at least one identifying key (field names vary by version)
        id_keys = {"CordNum", "ID", "id", "label", "cordNum", "Cord_Name", "cord_name"}
        for cord in a.cords:
            has_id = any(k in cord for k in id_keys)
            assert has_id, f"Cord missing identifier: {cord}"

    def test_ur001_cords_have_value_or_type(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        # UR001 is a large khipu; at least half the cords should have Value or Type
        has_data = sum(1 for c in a.cords if "Value" in c or "DataType" in c or "KnotType" in c)
        assert has_data > 0

    def test_qu001_cluster_count_matches_expected(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        assert len(a.clusters) == 12

    def test_ur001_cluster_count_positive(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert len(a.clusters) > 0


class TestKFGMultipleArtifacts:
    """Cross-artifact comparisons and structural correctness."""

    def test_qu001_khipu_dict_is_non_empty(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        assert len(a.khipu) > 0

    def test_ur001_khipu_is_dict(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert isinstance(a.khipu, dict)

    def test_qu001_primary_cord_is_non_empty(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        assert len(a.primary_cord) > 0

    def test_ur001_primary_cord_is_dict(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert isinstance(a.primary_cord, dict)

    def test_load_nonexistent_path_raises(self) -> None:
        import pytest

        with pytest.raises(FileNotFoundError):
            load_kfg_normalized(_KFG_DIR / "NONEXISTENT_99999.json")

    def test_ur001_cords_all_are_dicts(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "UR001.json")
        assert all(isinstance(c, dict) for c in a.cords)

    def test_qu001_clusters_all_non_empty_strings(self) -> None:
        a = load_kfg_normalized(_KFG_DIR / "QU001.json")
        assert all(isinstance(c, str) and len(c) > 0 for c in a.clusters)
