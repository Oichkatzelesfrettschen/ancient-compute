"""
Pytest configuration for Phase 2 integration tests

Provides fixtures and configuration for cross-language testing.
pytest.ini pythonpath=. handles import resolution; no sys.path hacks needed.
"""

import pytest


@pytest.fixture
def java_compiler():
    """Provide Java compiler instance"""
    from backend.src.compilers.java_compiler import JavaCompiler
    return JavaCompiler(verbose=False)


@pytest.fixture
def java_type_system():
    """Provide Java type system instance"""
    from backend.src.compilers.java_types import JavaTypeSystem
    return JavaTypeSystem()


@pytest.fixture
def idris_compiler():
    """Provide IDRIS2 compiler instance"""
    from backend.src.compilers.idris_compiler import IDRISCompiler
    return IDRISCompiler(verbose=False)


@pytest.fixture
def idris_type_system():
    """Provide IDRIS2 type system instance"""
    from backend.src.compilers.idris_types import IDRISTypeSystem
    return IDRISTypeSystem()


@pytest.fixture
def systemf_compiler():
    """Provide System F compiler instance"""
    from backend.src.compilers.systemf_compiler import SystemFCompiler
    return SystemFCompiler(verbose=False)


@pytest.fixture
def systemf_type_system():
    """Provide System F type system instance"""
    from backend.src.compilers.systemf_types import SystemFTypeSystem
    return SystemFTypeSystem()


@pytest.fixture
async def java_service():
    """Provide Java language service instance"""
    from backend.src.services.languages.java_service import JavaService
    return JavaService()


@pytest.fixture
async def idris_service():
    """Provide IDRIS2 language service instance"""
    from backend.src.services.languages.idris_service import IDRISService
    return IDRISService()


@pytest.fixture
async def systemf_service():
    """Provide System F language service instance"""
    from backend.src.services.languages.systemf_service import SystemFService
    return SystemFService()
