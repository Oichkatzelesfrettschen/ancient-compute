"""Ancient Compute language-service registry and executor factory."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, Literal, Optional

from .babbage_assembly_service import BabbageAssemblyService
from .c_service import CService
from .haskell_service import HaskellService
from .idris_service import IDRISService
from .java_service import JavaService
from .lisp_service import LISPService
from .python_service import PythonService
from .systemf_service import SystemFService

ImplementationStatus = Literal["implemented", "partial", "stub"]


@dataclass(frozen=True)
class LanguageCapability:
    """Public metadata and implementation truth for one language service."""

    id: str
    name: str
    version: str
    description: str
    aliases: tuple[str, ...]
    timeout: int
    memory_limit_mb: int
    implementation_status: ImplementationStatus
    execution_mode: str
    service_cls: type[Any]

    def to_public_dict(self) -> dict[str, Any]:
        """Convert to API-safe dictionary."""
        return {
            "id": self.id,
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "aliases": list(self.aliases),
            "timeout": self.timeout,
            "memory_limit_mb": self.memory_limit_mb,
            "implementation_status": self.implementation_status,
            "execution_mode": self.execution_mode,
        }


_LANGUAGE_CAPABILITIES: tuple[LanguageCapability, ...] = (
    LanguageCapability(
        id="c",
        name="C",
        version="GCC 12.2",
        description="C language service targeting Babbage ISA",
        aliases=("c",),
        timeout=30,
        memory_limit_mb=128,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=CService,
    ),
    LanguageCapability(
        id="python",
        name="Python",
        version="3.11",
        description="Python language service targeting Babbage ISA",
        aliases=("python",),
        timeout=30,
        memory_limit_mb=256,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=PythonService,
    ),
    LanguageCapability(
        id="haskell",
        name="Haskell",
        version="GHC 9.2",
        description="Haskell language service targeting Babbage ISA",
        aliases=("haskell",),
        timeout=10,
        memory_limit_mb=512,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=HaskellService,
    ),
    LanguageCapability(
        id="assembly",
        name="Babbage Assembly",
        version="Assembler v1",
        description="Babbage assembly service with native assembler backend",
        aliases=("assembly", "babbage-assembly"),
        timeout=10,
        memory_limit_mb=64,
        implementation_status="implemented",
        execution_mode="native_assembler",
        service_cls=BabbageAssemblyService,
    ),
    LanguageCapability(
        id="lisp",
        name="Common LISP",
        version="SBCL 2.3 (partial)",
        description="LISP parser/compiler path targeting Babbage IR",
        aliases=("lisp",),
        timeout=10,
        memory_limit_mb=256,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=LISPService,
    ),
    LanguageCapability(
        id="idris",
        name="IDRIS2",
        version="0.6.0 (partial)",
        description="IDRIS2 compiler targeting Babbage IR",
        aliases=("idris", "idris2"),
        timeout=10,
        memory_limit_mb=512,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=IDRISService,
    ),
    LanguageCapability(
        id="systemf",
        name="System F",
        version="Academic (partial)",
        description="System F compiler targeting Babbage IR with type erasure",
        aliases=("systemf", "system-f"),
        timeout=10,
        memory_limit_mb=128,
        implementation_status="partial",
        execution_mode="compile_only",
        service_cls=SystemFService,
    ),
    LanguageCapability(
        id="java",
        name="Java",
        version="OpenJDK 17 (stub)",
        description="Java service skeleton; execution path not implemented",
        aliases=("java",),
        timeout=15,
        memory_limit_mb=256,
        implementation_status="stub",
        execution_mode="placeholder",
        service_cls=JavaService,
    ),
)

_CANONICAL_BY_ALIAS: dict[str, str] = {
    alias: spec.id
    for spec in _LANGUAGE_CAPABILITIES
    for alias in spec.aliases
}
_CAPABILITY_BY_ID: dict[str, LanguageCapability] = {
    spec.id: spec for spec in _LANGUAGE_CAPABILITIES
}

__all__ = [
    "CService",
    "PythonService",
    "HaskellService",
    "BabbageAssemblyService",
    "LISPService",
    "IDRISService",
    "SystemFService",
    "JavaService",
    "LanguageCapability",
    "normalize_language_id",
    "get_executor",
    "list_language_capabilities",
    "language_status_summary",
]


def normalize_language_id(language: str) -> str | None:
    """Normalize user language input to a canonical language id."""
    if not language:
        return None
    return _CANONICAL_BY_ALIAS.get(language.strip().lower())


def get_executor(language: str) -> Any | None:
    """Factory function returning an executor instance for a supported language."""
    canonical = normalize_language_id(language)
    if canonical is None:
        return None
    return _CAPABILITY_BY_ID[canonical].service_cls()


def list_language_capabilities() -> list[dict[str, Any]]:
    """Return capability metadata in deterministic order."""
    return [spec.to_public_dict() for spec in _LANGUAGE_CAPABILITIES]


def language_status_summary() -> dict[str, Any]:
    """Return aggregate language counts by implementation status."""
    counts: dict[str, int] = {"implemented": 0, "partial": 0, "stub": 0}
    for spec in _LANGUAGE_CAPABILITIES:
        counts[spec.implementation_status] += 1
    return {
        "total": len(_LANGUAGE_CAPABILITIES),
        "implemented": counts["implemented"],
        "partial": counts["partial"],
        "stub": counts["stub"],
        "non_stub": counts["implemented"] + counts["partial"],
    }
