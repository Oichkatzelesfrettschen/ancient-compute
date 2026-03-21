"""Unit tests for language capability metadata and normalization."""

from __future__ import annotations

from backend.src.services.languages import (
    get_executor,
    language_status_summary,
    list_language_capabilities,
    normalize_language_id,
)
from backend.src.services.languages.babbage_assembly_service import BabbageAssemblyService


def test_language_aliases_normalize_to_canonical_ids() -> None:
    assert normalize_language_id("c") == "c"
    assert normalize_language_id("PyThOn") == "python"
    assert normalize_language_id("idris2") == "idris"
    assert normalize_language_id("system-f") == "systemf"
    assert normalize_language_id("babbage-assembly") == "assembly"
    assert normalize_language_id("fortran77") is None


def test_capability_entries_have_required_fields() -> None:
    capabilities = list_language_capabilities()

    assert capabilities
    required = {
        "id",
        "name",
        "version",
        "description",
        "aliases",
        "timeout",
        "memory_limit_mb",
        "implementation_status",
        "execution_mode",
    }

    for capability in capabilities:
        assert required.issubset(capability.keys())


def test_language_status_summary_matches_capability_list() -> None:
    capabilities = list_language_capabilities()
    summary = language_status_summary()

    implemented = sum(1 for cap in capabilities if cap["implementation_status"] == "implemented")
    partial = sum(1 for cap in capabilities if cap["implementation_status"] == "partial")
    stub = sum(1 for cap in capabilities if cap["implementation_status"] == "stub")

    assert summary["total"] == len(capabilities)
    assert summary["implemented"] == implemented
    assert summary["partial"] == partial
    assert summary["stub"] == stub
    assert summary["non_stub"] == implemented + partial


def test_get_executor_uses_canonical_alias_mapping() -> None:
    assembly_executor = get_executor("babbage-assembly")
    assert isinstance(assembly_executor, BabbageAssemblyService)


class TestNormalizeLanguageId:
    def test_canonical_id_unchanged(self) -> None:
        assert normalize_language_id("c") == "c"
        assert normalize_language_id("python") == "python"
        assert normalize_language_id("haskell") == "haskell"

    def test_case_insensitive(self) -> None:
        assert normalize_language_id("C") == "c"
        assert normalize_language_id("PYTHON") == "python"
        assert normalize_language_id("HASKELL") == "haskell"

    def test_cpp_alias(self) -> None:
        assert normalize_language_id("c++") == "cpp"
        assert normalize_language_id("cpp") == "cpp"

    def test_assembly_alias(self) -> None:
        assert normalize_language_id("assembly") == "assembly"
        assert normalize_language_id("babbage-assembly") == "assembly"

    def test_idris_aliases(self) -> None:
        assert normalize_language_id("idris") == "idris"
        assert normalize_language_id("idris2") == "idris"

    def test_systemf_aliases(self) -> None:
        assert normalize_language_id("systemf") == "systemf"
        assert normalize_language_id("system-f") == "systemf"

    def test_unknown_returns_none(self) -> None:
        assert normalize_language_id("cobol") is None
        assert normalize_language_id("fortran77") is None
        assert normalize_language_id("") is None

    def test_micropython_alias(self) -> None:
        assert normalize_language_id("micropython") == "micropython"


class TestListLanguageCapabilities:
    def test_returns_non_empty_list(self) -> None:
        caps = list_language_capabilities()
        assert len(caps) >= 9

    def test_all_ids_are_strings(self) -> None:
        for cap in list_language_capabilities():
            assert isinstance(cap["id"], str)

    def test_timeout_positive(self) -> None:
        for cap in list_language_capabilities():
            assert cap["timeout"] > 0

    def test_memory_limit_positive(self) -> None:
        for cap in list_language_capabilities():
            assert cap["memory_limit_mb"] > 0

    def test_implementation_status_valid_values(self) -> None:
        valid = {"implemented", "partial", "stub"}
        for cap in list_language_capabilities():
            assert cap["implementation_status"] in valid

    def test_execution_mode_valid_values(self) -> None:
        valid = {"native_assembler", "compile_only", "native_subprocess"}
        for cap in list_language_capabilities():
            assert cap["execution_mode"] in valid

    def test_aliases_are_lists(self) -> None:
        for cap in list_language_capabilities():
            assert isinstance(cap["aliases"], list)

    def test_known_languages_present(self) -> None:
        ids = {cap["id"] for cap in list_language_capabilities()}
        for lang in ["c", "python", "haskell", "assembly"]:
            assert lang in ids


class TestLanguageStatusSummary:
    def test_non_stub_equals_implemented_plus_partial(self) -> None:
        s = language_status_summary()
        assert s["non_stub"] == s["implemented"] + s["partial"]

    def test_total_is_sum_of_all_statuses(self) -> None:
        s = language_status_summary()
        assert s["total"] == s["implemented"] + s["partial"] + s["stub"]

    def test_returns_dict(self) -> None:
        assert isinstance(language_status_summary(), dict)

    def test_no_stub_languages(self) -> None:
        # All 11 languages are implemented or partial, none are stubs
        s = language_status_summary()
        assert s["stub"] == 0
