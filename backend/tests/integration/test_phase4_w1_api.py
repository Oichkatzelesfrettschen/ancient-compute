"""
Phase 4.W1 Backend API Integration Tests

Tests for:
- Emulator initialization and reset
- Polynomial execution endpoints
- Debugging endpoints (breakpoints, variables)
- State inspection
- Error handling
"""

import pytest
from fastapi.testclient import TestClient
from typing import Dict, Any

# Import the app
from backend.src.main import app

client = TestClient(app)


class TestEmulatorInitialization:
    """Test emulator initialization and reset endpoints"""

    def test_initialize_emulator(self):
        """POST /api/initialize should create new emulator instance"""
        response = client.post("/api/initialize")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "Emulator initialized" in data["message"]

    def test_reset_emulator(self):
        """POST /api/reset should reset emulator to initial state"""
        # First initialize
        client.post("/api/initialize")
        # Then reset
        response = client.post("/api/reset")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_get_state_requires_initialization(self):
        """GET /api/state should return error if emulator not initialized"""
        # Clear any existing state by starting fresh
        response = client.get("/api/state")
        # Should either work (if initialized) or fail gracefully
        if response.status_code == 400:
            assert "Emulator not initialized" in response.json()["detail"]

    def test_get_initial_state_after_init(self):
        """GET /api/state should return valid state after initialization"""
        client.post("/api/initialize")
        response = client.get("/api/state")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        state = data["state"]

        # Verify state structure
        assert "cycle" in state
        assert "phase" in state
        assert "angle" in state
        assert "columns" in state
        assert "carrySignals" in state
        assert "accumulator" in state
        assert "totalOperations" in state

        # Initial values
        assert state["cycle"] == 0
        assert isinstance(state["columns"], list)
        assert len(state["columns"]) == 8


class TestPolynomialExecution:
    """Test polynomial execution endpoints"""

    def test_execute_linear_polynomial(self):
        """POST /api/execute should evaluate linear polynomial f(x) = 2x + 1"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "results" in data
        assert len(data["results"]) == 5  # 5 values from x=1 to x=5
        assert "totalCycles" in data

        # Verify results: f(x) = 2x + 1
        expected = [3, 5, 7, 9, 11]
        for i, result in enumerate(data["results"]):
            assert result["x"] == i + 1
            assert result["result"] == expected[i]

    def test_execute_quadratic_polynomial(self):
        """POST /api/execute should evaluate quadratic polynomial f(x) = x² + 1"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 0, 1],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

        # Verify results: f(x) = x² + 1
        expected = [2, 5, 10, 17, 26]
        for i, result in enumerate(data["results"]):
            assert result["x"] == i + 1
            assert result["result"] == expected[i]

    def test_execute_cubic_polynomial(self):
        """POST /api/execute should evaluate cubic polynomial f(x) = x³"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [0, 0, 0, 1],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

        # Verify results: f(x) = x³
        expected = [1, 8, 27, 64, 125]
        for i, result in enumerate(data["results"]):
            assert result["x"] == i + 1
            assert result["result"] == expected[i]

    def test_execute_single_value(self):
        """POST /api/execute should handle single x value (x_start == x_end)"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [3, 3],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert len(data["results"]) == 1
        assert data["results"][0]["x"] == 3
        assert data["results"][0]["result"] == 7  # f(3) = 2*3 + 1

    def test_execute_invalid_x_range_negative(self):
        """POST /api/execute should reject negative x values"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [-5, 5],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is False
        assert "X range values must be non-negative" in data["error"]

    def test_execute_invalid_x_range_reversed(self):
        """POST /api/execute should reject x_start > x_end"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [10, 5],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is False
        assert "X start must be less than or equal to X end" in data["error"]

    def test_execute_includes_phase_info(self):
        """POST /api/execute should include mechanical phase in results"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 2],
                "execution_speed": 1.0
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

        for result in data["results"]:
            assert "phase" in result
            assert result["phase"] in ["IDLE", "ADDITION", "CARRY", "TABLE", "OUTPUT"]

    def test_get_results_endpoint(self):
        """GET /api/results should return execution history"""
        client.post("/api/initialize")
        client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        response = client.get("/api/results")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "results" in data


class TestDebugger:
    """Test debugging endpoints (breakpoints, variables, step)"""

    def test_debug_step_single_cycle(self):
        """POST /api/debug/step should advance one mechanical cycle"""
        client.post("/api/initialize")

        # Get initial state
        response1 = client.get("/api/state")
        initial_cycle = response1.json()["state"]["cycle"]

        # Step once
        response2 = client.post("/api/debug/step")
        assert response2.status_code == 200
        data = response2.json()
        assert data["success"] is True
        assert "state" in data
        assert data["state"]["cycle"] > initial_cycle

    def test_set_cycle_breakpoint(self):
        """POST /api/debug/breakpoint should create cycle breakpoint"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/breakpoint",
            json={
                "type": "CYCLE",
                "cycle_target": 100
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "breakpointId" in data
        assert isinstance(data["breakpointId"], int)

    def test_set_phase_breakpoint(self):
        """POST /api/debug/breakpoint should create phase breakpoint"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/breakpoint",
            json={
                "type": "PHASE",
                "phase_target": "CARRY"
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "breakpointId" in data

    def test_set_value_change_breakpoint(self):
        """POST /api/debug/breakpoint should create value change breakpoint"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/breakpoint",
            json={
                "type": "VALUE_CHANGE",
                "variable_name": "accumulator"
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "breakpointId" in data

    def test_enable_breakpoint(self):
        """POST /api/debug/breakpoint/{id}/enable should enable breakpoint"""
        client.post("/api/initialize")

        # Create breakpoint
        bp_response = client.post(
            "/api/debug/breakpoint",
            json={"type": "CYCLE", "cycle_target": 50}
        )
        bp_id = bp_response.json()["breakpointId"]

        # Enable it
        response = client.post(f"/api/debug/breakpoint/{bp_id}/enable")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_disable_breakpoint(self):
        """POST /api/debug/breakpoint/{id}/disable should disable breakpoint"""
        client.post("/api/initialize")

        # Create breakpoint
        bp_response = client.post(
            "/api/debug/breakpoint",
            json={"type": "CYCLE", "cycle_target": 50}
        )
        bp_id = bp_response.json()["breakpointId"]

        # Disable it
        response = client.post(f"/api/debug/breakpoint/{bp_id}/disable")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_delete_breakpoint(self):
        """DELETE /api/debug/breakpoint/{id} should remove breakpoint"""
        client.post("/api/initialize")

        # Create breakpoint
        bp_response = client.post(
            "/api/debug/breakpoint",
            json={"type": "CYCLE", "cycle_target": 50}
        )
        bp_id = bp_response.json()["breakpointId"]

        # Delete it
        response = client.delete(f"/api/debug/breakpoint/{bp_id}")
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_define_variable(self):
        """POST /api/debug/variable should define debugger variable"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/variable",
            json={
                "name": "myvar",
                "value": 42
            }
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_set_variable_value(self):
        """PUT /api/debug/variable/{name} should update variable"""
        client.post("/api/initialize")

        # Define variable
        client.post(
            "/api/debug/variable",
            json={"name": "myvar", "value": 42}
        )

        # Update it
        response = client.put(
            "/api/debug/variable/myvar",
            json={"name": "myvar", "value": 100}
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True

    def test_debug_continue_execution(self):
        """POST /api/debug/continue should run until breakpoint"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/continue",
            json={"max_cycles": 50}
        )

        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "cyclesRun" in data
        assert "state" in data


class TestStateConsistency:
    """Test state consistency across operations"""

    def test_state_persists_after_execution(self):
        """Emulator state should persist after polynomial execution"""
        client.post("/api/initialize")

        # Execute polynomial
        client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 3],
                "execution_speed": 1.0
            }
        )

        # Get state
        response = client.get("/api/state")
        data = response.json()
        assert data["success"] is True
        state = data["state"]

        # Should show cycles from execution
        assert state["cycle"] > 0

    def test_reset_clears_state(self):
        """POST /api/reset should clear execution state"""
        client.post("/api/initialize")

        # Execute polynomial
        client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        # Get state before reset
        response1 = client.get("/api/state")
        cycle_before = response1.json()["state"]["cycle"]
        assert cycle_before > 0

        # Reset
        client.post("/api/reset")

        # Get state after reset
        response2 = client.get("/api/state")
        cycle_after = response2.json()["state"]["cycle"]
        assert cycle_after == 0

    def test_step_increments_cycle(self):
        """POST /api/debug/step should increment cycle counter"""
        client.post("/api/initialize")

        # Get initial cycle
        response1 = client.get("/api/state")
        initial_cycle = response1.json()["state"]["cycle"]

        # Step 5 times
        for _ in range(5):
            client.post("/api/debug/step")

        # Get final cycle
        response2 = client.get("/api/state")
        final_cycle = response2.json()["state"]["cycle"]

        assert final_cycle == initial_cycle + 5


class TestErrorHandling:
    """Test error handling and edge cases"""

    def test_execute_without_initialization(self):
        """POST /api/execute should handle uninitialized emulator gracefully"""
        # This test depends on whether endpoint auto-initializes
        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )
        # Should either work (auto-init) or fail gracefully
        assert response.status_code in [200, 400]

    def test_breakpoint_on_uninitialized_emulator(self):
        """POST /api/debug/breakpoint should handle uninitialized emulator"""
        response = client.post(
            "/api/debug/breakpoint",
            json={"type": "CYCLE", "cycle_target": 50}
        )
        # Should either work or fail gracefully
        assert response.status_code in [200, 400]

    def test_invalid_breakpoint_type(self):
        """POST /api/debug/breakpoint should reject invalid type"""
        client.post("/api/initialize")

        response = client.post(
            "/api/debug/breakpoint",
            json={"type": "INVALID_TYPE"}
        )

        # Should return error
        assert response.status_code in [200, 400]
        if response.status_code == 200:
            data = response.json()
            assert data.get("success") is False

    def test_empty_coefficient_array(self):
        """POST /api/execute should handle empty coefficients"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [],
                "x_range": [1, 5],
                "execution_speed": 1.0
            }
        )

        # Should handle gracefully
        assert response.status_code in [200, 400]

    def test_large_coefficient_values(self):
        """POST /api/execute should handle large coefficients"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1000000, 999999],
                "x_range": [1, 2],
                "execution_speed": 1.0
            }
        )

        # Should handle gracefully
        assert response.status_code in [200, 400]

    def test_large_x_range(self):
        """POST /api/execute should handle large x ranges"""
        client.post("/api/initialize")

        response = client.post(
            "/api/execute",
            json={
                "coefficients": [1, 2],
                "x_range": [1, 1000],
                "execution_speed": 1.0
            }
        )

        # Should complete (may be slow)
        assert response.status_code in [200, 400]


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])
