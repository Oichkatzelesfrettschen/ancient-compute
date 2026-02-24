"""Execution API integration tests aligned with the current route contract.

This suite validates language-catalog truth, health summary consistency, and
basic `/api/v1/execute/run` behavior for representative languages and aliases.
"""

from __future__ import annotations

import httpx
import pytest
import pytest_asyncio
from fastapi import FastAPI

from backend.src.api.code_execution import router as code_execution_router


@pytest.fixture(scope="module")
def execution_app() -> FastAPI:
    """Minimal app wiring only code-execution routes for integration tests."""
    app = FastAPI()
    app.include_router(code_execution_router, prefix="/api/v1")
    return app


@pytest_asyncio.fixture
async def api_client(execution_app: FastAPI) -> httpx.AsyncClient:
    """In-process async client backed by FastAPI ASGI transport."""
    transport = httpx.ASGITransport(app=execution_app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as client:
        yield client


@pytest.mark.integration
@pytest.mark.asyncio
async def test_languages_endpoint_exposes_capability_metadata(
    api_client: httpx.AsyncClient,
) -> None:
    response = await api_client.get("/api/v1/execute/languages")

    assert response.status_code == 200
    payload = response.json()

    assert "languages" in payload
    assert "summary" in payload
    assert payload["summary"]["total"] >= 1

    first = payload["languages"][0]
    for key in (
        "id",
        "name",
        "version",
        "description",
        "aliases",
        "timeout",
        "memory_limit_mb",
        "implementation_status",
        "execution_mode",
    ):
        assert key in first


@pytest.mark.integration
@pytest.mark.asyncio
async def test_health_summary_matches_language_catalog(
    api_client: httpx.AsyncClient,
) -> None:
    languages_response = await api_client.get("/api/v1/execute/languages")
    health_response = await api_client.get("/api/v1/execute/health")

    assert languages_response.status_code == 200
    assert health_response.status_code == 200

    languages_payload = languages_response.json()
    health_payload = health_response.json()

    summary = languages_payload["summary"]
    assert health_payload["languages_total"] == summary["total"]
    assert health_payload["languages_available"] == summary["non_stub"]
    assert health_payload["languages_by_status"]["implemented"] == summary["implemented"]
    assert health_payload["languages_by_status"]["partial"] == summary["partial"]
    assert health_payload["languages_by_status"]["stub"] == summary["stub"]


@pytest.mark.integration
@pytest.mark.asyncio
@pytest.mark.parametrize(
    "language,code,expected_status",
    [
        (
            "assembly",
            ".global main\n.text\nmain:\n  LOAD A, 1\n  RET\n",
            "success",
        ),
        (
            "babbage-assembly",
            ".global main\n.text\nmain:\n  LOAD A, 2\n  RET\n",
            "success",
        ),
        ("idris2", "main : Nat", "success"),
        ("system-f", "42", "success"),
        ("lisp", "(defun broken (x) (+ x 1)", "compile_error"),
    ],
)
async def test_execute_run_supports_aliases_and_normalization(
    api_client: httpx.AsyncClient,
    language: str,
    code: str,
    expected_status: str,
) -> None:
    response = await api_client.post(
        "/api/v1/execute/run",
        json={
            "language": language,
            "code": code,
            "input_data": "",
        },
    )

    assert response.status_code == 200
    payload = response.json()
    assert payload["status"] == expected_status


@pytest.mark.integration
@pytest.mark.asyncio
async def test_execute_run_rejects_unknown_language(api_client: httpx.AsyncClient) -> None:
    response = await api_client.post(
        "/api/v1/execute/run",
        json={
            "language": "fortran77",
            "code": "PRINT *, 'hello'",
        },
    )

    assert response.status_code == 400
    assert "not currently supported" in response.json()["detail"]
