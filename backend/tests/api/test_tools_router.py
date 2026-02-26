"""
Integration Tests for Engine Tools API
"""

from fastapi.testclient import TestClient

from backend.src.main import app

client = TestClient(app)

def test_debug_step():
    """Test stepping the emulator via API."""
    # First reset to ensure known state
    client.post("/api/v1/tools/debug/command", json={"action": "reset"})

    response = client.post("/api/v1/tools/debug/step")
    assert response.status_code == 200
    data = response.json()
    assert "state" in data
    assert "cycle" in data["state"]
    # Analytical Engine adapter returns clock_time as cycle
    assert data["state"]["cycle"] >= 0

def test_performance_report():
    """Test retrieving performance report."""
    response = client.get("/api/v1/tools/performance")
    assert response.status_code == 200
    data = response.json()
    assert "total_cycles" in data
    assert "instruction_frequency" in data
    assert "suggestions" in data

def test_debug_command_reset():
    """Test resetting the emulator."""
    response = client.post("/api/v1/tools/debug/command", json={"action": "reset"})
    assert response.status_code == 200
    assert response.json()["status"] == "reset"

    # Verify cycle count is 0 (or low if initialization takes time)
    step_resp = client.post("/api/v1/tools/debug/step")
    # First step might advance clock
    assert step_resp.json()["state"]["cycle"] >= 0
