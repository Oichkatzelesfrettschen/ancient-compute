"""Gate 2 freestanding C subset tests and diagnostics."""

from __future__ import annotations

import asyncio

import pytest

from backend.src.services.languages.c_service import CService, ExecutionStatus


def _execute(code: str):
    return asyncio.run(CService().execute(code))


def test_freestanding_subset_accepts_scalar_core_program() -> None:
    code = """
    int main() {
        int x;
        x = 10;
        return x;
    }
    """
    result = _execute(code)
    assert result.status == ExecutionStatus.SUCCESS


@pytest.mark.parametrize(
    "expected_label, code",
    [
        (
            "switch statements",
            """
            int classify(int x) {
                switch (x) {
                    case 1: return 1;
                    default: return 0;
                }
            }
            """,
        ),
        (
            "struct/union/enum types",
            """
            int main() {
                struct S { int x; };
                return 0;
            }
            """,
        ),
        (
            "preprocessor directives",
            """
            #include <stdio.h>
            int main() {
                return 0;
            }
            """,
        ),
        (
            "pointer declarators",
            """
            int deref(int *p) {
                return 0;
            }
            """,
        ),
    ],
)
def test_freestanding_subset_rejects_unsupported_features(
    expected_label: str, code: str
) -> None:
    result = _execute(code)
    assert result.status == ExecutionStatus.COMPILE_ERROR
    assert "Unsupported C feature(s) for freestanding subset" in result.stderr
    assert expected_label in result.stderr
    assert "FREESTANDING_C_SUBSET_PROFILE.md" in result.stderr
