# Ancient Compute - Health Endpoint Tests
import pytest
from fastapi.testclient import TestClient


@pytest.mark.unit
def test_health_check(client: TestClient):
    """Test the health check endpoint returns healthy status."""
    response = client.get("/health")

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "healthy"
    assert data["service"] == "ancient-compute-backend"


@pytest.mark.unit
def test_readiness_check(client: TestClient):
    """Test the readiness check endpoint."""
    response = client.get("/ready")

    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "ready"


@pytest.mark.unit
def test_metrics_endpoint(client: TestClient):
    """Test the metrics endpoint returns Prometheus-format monitoring data."""
    response = client.get("/metrics")

    assert response.status_code == 200
    # Prometheus metrics are returned as text, not JSON
    text = response.text
    assert "uptime_seconds" in text
    assert "requests" in text


@pytest.mark.unit
def test_root_endpoint(client: TestClient):
    """Test the root endpoint returns service information."""
    response = client.get("/")

    assert response.status_code == 200
    data = response.json()
    assert data["service"] == "Ancient Compute API"
    assert data["version"] == "0.1.0"
    assert "description" in data
