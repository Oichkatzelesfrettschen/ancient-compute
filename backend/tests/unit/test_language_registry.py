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
