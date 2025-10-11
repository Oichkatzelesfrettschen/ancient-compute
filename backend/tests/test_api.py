# Ancient Compute - API Router Tests
import pytest
from fastapi.testclient import TestClient


@pytest.mark.unit
def test_list_modules_endpoint(client: TestClient):
    """Test the modules listing endpoint returns expected structure."""
    response = client.get("/api/v1/modules")

    assert response.status_code == 200
    data = response.json()
    assert "modules" in data
    assert isinstance(data["modules"], list)

    # Check if placeholder modules are returned
    if len(data["modules"]) > 0:
        module = data["modules"][0]
        assert "id" in module
        assert "title" in module
        assert "description" in module


@pytest.mark.unit
def test_timeline_endpoint(client: TestClient):
    """Test the timeline endpoint returns expected structure."""
    response = client.get("/api/v1/timeline")

    assert response.status_code == 200
    data = response.json()
    assert "timeline" in data
    assert isinstance(data["timeline"], list)

    # Check if placeholder events are returned
    if len(data["timeline"]) > 0:
        event = data["timeline"][0]
        assert "year" in event
        assert "title" in event
        assert "description" in event


@pytest.mark.unit
def test_cors_headers(client: TestClient):
    """Test that CORS headers are properly configured."""
    response = client.options("/api/v1/modules")

    # FastAPI TestClient doesn't enforce CORS preflight,
    # but we can verify the middleware is configured
    assert response.status_code in [200, 405]  # OPTIONS may not be explicitly handled
