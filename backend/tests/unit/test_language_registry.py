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


class TestGetExecutorAllLanguages:
    """get_executor() coverage for all 11 registered languages."""

    def test_algol68_returns_service(self) -> None:
        from backend.src.services.languages.algol68_service import ALGOL68Service

        assert isinstance(get_executor("algol68"), ALGOL68Service)

    def test_cpp_returns_service(self) -> None:
        from backend.src.services.languages.cpp_service import CppService

        assert isinstance(get_executor("cpp"), CppService)

    def test_cpp_alias_cxx_returns_service(self) -> None:
        from backend.src.services.languages.cpp_service import CppService

        assert isinstance(get_executor("c++"), CppService)

    def test_micropython_returns_service(self) -> None:
        from backend.src.services.languages.micropython_service import MicroPythonService

        assert isinstance(get_executor("micropython"), MicroPythonService)

    def test_c_and_cpp_are_different_service_types(self) -> None:
        assert type(get_executor("c")) is not type(get_executor("cpp"))

    def test_normalize_strips_surrounding_whitespace(self) -> None:
        assert normalize_language_id(" python ") == "python"

    def test_all_eleven_languages_non_none(self) -> None:
        for lang in [
            "c", "python", "haskell", "assembly", "lisp",
            "idris", "systemf", "java", "algol68", "cpp", "micropython",
        ]:
            assert get_executor(lang) is not None, f"get_executor({lang!r}) returned None"


class TestLanguageCapabilityFields:
    """list_language_capabilities() entry field types and values."""

    def test_version_field_is_string(self) -> None:
        for cap in list_language_capabilities():
            assert isinstance(cap["version"], str), f"{cap['id']} version not str"

    def test_name_field_is_string(self) -> None:
        for cap in list_language_capabilities():
            assert isinstance(cap["name"], str), f"{cap['id']} name not str"

    def test_description_field_is_nonempty_string(self) -> None:
        for cap in list_language_capabilities():
            assert isinstance(cap["description"], str)
            assert len(cap["description"]) > 0

    def test_assembly_id_in_capabilities(self) -> None:
        ids = {cap["id"] for cap in list_language_capabilities()}
        assert "assembly" in ids


class TestLanguageStatusSummaryExtended:
    """language_status_summary() structure and values."""

    def test_summary_has_total_key(self) -> None:
        s = language_status_summary()
        assert "total" in s

    def test_summary_total_is_eleven(self) -> None:
        s = language_status_summary()
        assert s["total"] == 11

    def test_summary_has_implemented_key(self) -> None:
        s = language_status_summary()
        assert "implemented" in s

    def test_implemented_at_least_five(self) -> None:
        s = language_status_summary()
        assert s["implemented"] >= 5

    def test_non_stub_equals_total(self) -> None:
        s = language_status_summary()
        assert s["non_stub"] == s["total"]

    def test_summary_has_partial_key(self) -> None:
        s = language_status_summary()
        assert "partial" in s


class TestCapabilityEntryDetails:
    """Per-capability entry field value checks."""

    def test_python_capability_has_correct_id(self) -> None:
        cap = next(c for c in list_language_capabilities() if c["id"] == "python")
        assert cap["id"] == "python"

    def test_c_capability_has_execution_mode(self) -> None:
        cap = next(c for c in list_language_capabilities() if c["id"] == "c")
        assert "execution_mode" in cap

    def test_haskell_has_aliases(self) -> None:
        cap = next(c for c in list_language_capabilities() if c["id"] == "haskell")
        assert isinstance(cap["aliases"], list)

    def test_timeout_is_positive_number(self) -> None:
        for cap in list_language_capabilities():
            assert cap["timeout"] > 0, f"{cap['id']} has non-positive timeout"

    def test_memory_limit_is_positive(self) -> None:
        for cap in list_language_capabilities():
            assert cap["memory_limit_mb"] > 0

    def test_all_ids_are_lowercase(self) -> None:
        for cap in list_language_capabilities():
            assert cap["id"] == cap["id"].lower()

    def test_normalize_idris2_alias(self) -> None:
        assert normalize_language_id("idris2") == "idris"

    def test_normalize_system_f_alias(self) -> None:
        assert normalize_language_id("system-f") == "systemf"
