"""Unit tests for machine_registry.py and the machines FastAPI router."""

from __future__ import annotations

from typing import Any

import pytest

from backend.src.emulator.machine_registry import (
    MACHINE_BY_ID,
    MACHINES,
    build_machine,
    get_machine,
    list_machines,
)

# ---------------------------------------------------------------------------
# Registry metadata
# ---------------------------------------------------------------------------


class TestMachineRegistry:
    def test_all_machines_have_required_fields(self) -> None:
        for m in MACHINES:
            assert m.id, f"Missing id: {m}"
            assert m.name, f"Missing name: {m.id}"
            assert m.country, f"Missing country: {m.id}"
            assert m.inventor, f"Missing inventor: {m.id}"
            assert m.category, f"Missing category: {m.id}"
            assert m.brief, f"Missing brief: {m.id}"
            assert m.program_input_type, f"Missing program_input_type: {m.id}"
            assert m.factory is not None, f"Missing factory: {m.id}"

    def test_no_duplicate_ids(self) -> None:
        ids = [m.id for m in MACHINES]
        assert len(ids) == len(set(ids)), "Duplicate machine IDs found"

    def test_machine_by_id_lookup(self) -> None:
        for m in MACHINES:
            assert MACHINE_BY_ID[m.id] is m

    def test_get_machine_known(self) -> None:
        entry = get_machine("zuse-z3")
        assert entry is not None
        assert entry.name == "Zuse Z3"

    def test_get_machine_unknown_returns_none(self) -> None:
        assert get_machine("does-not-exist") is None

    def test_list_machines_all(self) -> None:
        all_machines = list_machines()
        assert len(all_machines) == len(MACHINES)

    def test_list_machines_by_category(self) -> None:
        calculators = list_machines("calculator")
        assert all(m.category == "calculator" for m in calculators)
        assert len(calculators) >= 3  # pascaline, leibniz, thomas, ...

    def test_list_machines_unknown_category_empty(self) -> None:
        result = list_machines("nonexistent_category")
        assert result == []

    def test_all_machines_have_nonempty_manual(self) -> None:
        empty = [m.id for m in MACHINES if not m.manual.strip()]
        assert empty == [], f"Machines with empty manuals: {empty}"

    def test_example_payloads_are_dicts(self) -> None:
        for m in MACHINES:
            assert isinstance(
                m.example_payload, dict
            ), f"{m.id}: example_payload must be a dict, got {type(m.example_payload)}"

    def test_machine_count(self) -> None:
        # The registry should have at least 20 machines.
        assert len(MACHINES) >= 20

    def test_categories_present(self) -> None:
        found = {m.category for m in MACHINES}
        expected = {
            "astronomical",
            "calculator",
            "difference_engine",
            "analytical_engine",
            "cipher",
            "stored_program",
            "tabulator",
        }
        assert expected.issubset(found), f"Missing categories: {expected - found}"

    def test_year_range(self) -> None:
        years = [m.year for m in MACHINES]
        assert min(years) < 0, "Expected at least one BCE machine (Antikythera)"
        assert max(years) < 2000, "Expected all machines before 2000"


# ---------------------------------------------------------------------------
# Factory: all machines construct without error
# ---------------------------------------------------------------------------


class TestMachineFactories:
    @pytest.mark.parametrize("machine_id", [m.id for m in MACHINES])
    def test_factory_constructs_machine(self, machine_id: str) -> None:
        result = build_machine(machine_id)
        assert result is not None
        machine, adapter = result
        assert machine is not None
        assert adapter is not None

    def test_build_unknown_returns_none(self) -> None:
        assert build_machine("unicorn-computer") is None

    @pytest.mark.parametrize("machine_id", [m.id for m in MACHINES])
    def test_adapter_get_register_values_returns_dict(self, machine_id: str) -> None:
        _machine, adapter = build_machine(machine_id)  # type: ignore[misc]
        regs = adapter.get_register_values()
        assert isinstance(regs, dict)

    @pytest.mark.parametrize("machine_id", [m.id for m in MACHINES])
    def test_adapter_get_column_values_returns_list(self, machine_id: str) -> None:
        _machine, adapter = build_machine(machine_id)  # type: ignore[misc]
        cols = adapter.get_column_values()
        assert isinstance(cols, list)

    @pytest.mark.parametrize("machine_id", [m.id for m in MACHINES])
    def test_adapter_get_snapshot_returns_something(self, machine_id: str) -> None:
        _machine, adapter = build_machine(machine_id)  # type: ignore[misc]
        snap = adapter.get_snapshot()
        assert snap is not None

    @pytest.mark.parametrize("machine_id", [m.id for m in MACHINES])
    def test_adapter_get_cycle_count_is_int(self, machine_id: str) -> None:
        _machine, adapter = build_machine(machine_id)  # type: ignore[misc]
        assert isinstance(adapter.get_cycle_count(), int)


# ---------------------------------------------------------------------------
# Step: a few key machines can step without crashing on fresh instance
# ---------------------------------------------------------------------------


class TestMachineStep:
    def test_pascaline_step_increments(self) -> None:
        machine, adapter = build_machine("pascaline")  # type: ignore[misc]
        machine.add(5)
        before = machine.get_value()
        adapter.step()
        after = machine.get_value()
        # step() adds 1 to units wheel; value should increase by 1
        assert after == before + 1

    def test_zuse_z3_step(self) -> None:
        machine, adapter = build_machine("zuse-z3")  # type: ignore[misc]
        from backend.src.emulator.zuse_z3 import Z3Instruction, Z3Op

        machine.load_memory(0, 4.0)
        machine.load_program(
            [
                Z3Instruction(Z3Op.LOAD, 0),
                Z3Instruction(Z3Op.SQRT),
                Z3Instruction(Z3Op.PRINT),
                Z3Instruction(Z3Op.HALT),
            ]
        )
        for _ in range(4):
            adapter.step()
        snap = adapter.get_snapshot()
        assert snap["output_tape"] == [2.0]

    def test_antikythera_step_advances_date(self) -> None:
        _machine, adapter = build_machine("antikythera")  # type: ignore[misc]
        adapter.step()
        assert adapter._date > 0  # type: ignore[attr-defined]
        assert adapter.get_cycle_count() == 1

    def test_enigma_step_enciphers_char(self) -> None:
        machine, adapter = build_machine("enigma")  # type: ignore[misc]
        adapter.load_input("HELLO")  # type: ignore[attr-defined]
        adapter.step()
        assert adapter.get_cycle_count() == 1
        output = adapter.get_output()  # type: ignore[attr-defined]
        assert len(output) == 1
        assert output.isalpha()

    def test_scheutz_step_cranks(self) -> None:
        machine, adapter = build_machine("scheutz-de")  # type: ignore[misc]
        before = adapter.get_cycle_count()
        adapter.step()
        assert adapter.get_cycle_count() == before + 1

    def test_colossus_step_advances_tape(self) -> None:
        _machine, adapter = build_machine("colossus")  # type: ignore[misc]
        tape = [[1, 0, 1, 0, 1]] * 10
        adapter.load_tape(tape)  # type: ignore[attr-defined]
        adapter.step()
        assert adapter.get_cycle_count() == 1


# ---------------------------------------------------------------------------
# API router (FastAPI TestClient)
# ---------------------------------------------------------------------------


@pytest.fixture
def api_client():  # type: ignore[no-untyped-def]
    from fastapi import FastAPI
    from fastapi.testclient import TestClient

    from backend.src.api.machines import router

    app = FastAPI()
    app.include_router(router)
    return TestClient(app)


class TestMachinesAPI:
    def test_list_returns_all_machines(self, api_client: Any) -> None:
        resp = api_client.get("/machines")
        assert resp.status_code == 200
        data = resp.json()
        assert isinstance(data, list)
        assert len(data) >= 20

    def test_list_category_filter(self, api_client: Any) -> None:
        resp = api_client.get("/machines?category=calculator")
        assert resp.status_code == 200
        data = resp.json()
        assert all(m["category"] == "calculator" for m in data)
        assert len(data) >= 3

    def test_get_machine_detail(self, api_client: Any) -> None:
        resp = api_client.get("/machines/zuse-z3")
        assert resp.status_code == 200
        data = resp.json()
        assert data["id"] == "zuse-z3"
        assert "manual" in data
        assert len(data["manual"]) > 100
        assert "example_payload" in data

    def test_get_unknown_machine_404(self, api_client: Any) -> None:
        resp = api_client.get("/machines/nonexistent")
        assert resp.status_code == 404

    def test_reset_machine(self, api_client: Any) -> None:
        resp = api_client.post("/machines/zuse-z3/reset")
        assert resp.status_code == 200
        data = resp.json()
        assert "cycle_count" in data
        assert "registers" in data

    def test_state_returns_registers(self, api_client: Any) -> None:
        api_client.post("/machines/pascaline/reset")
        resp = api_client.get("/machines/pascaline/state")
        assert resp.status_code == 200
        data = resp.json()
        assert "registers" in data
        assert "columns" in data

    def test_load_and_step_zuse_z3(self, api_client: Any) -> None:
        api_client.post("/machines/zuse-z3/reset")
        payload = {
            "payload": {
                "memory": {"0": 9.0},
                "input_tape": [],
                "program": [
                    {"op": "LOAD", "address": 0},
                    {"op": "SQRT"},
                    {"op": "PRINT"},
                    {"op": "HALT"},
                ],
            }
        }
        resp = api_client.post("/machines/zuse-z3/load", json=payload)
        assert resp.status_code == 200
        # Step 4 times (LOAD, SQRT, PRINT, HALT)
        for _ in range(4):
            api_client.post("/machines/zuse-z3/step")
        snap_resp = api_client.get("/machines/zuse-z3/state")
        snap = snap_resp.json()["snapshot"]
        assert snap["output_tape"] == [3.0]

    def test_run_machine_halts(self, api_client: Any) -> None:
        api_client.post("/machines/zuse-z3/reset")
        payload = {
            "payload": {
                "memory": {"0": 16.0},
                "input_tape": [],
                "program": [
                    {"op": "LOAD", "address": 0},
                    {"op": "SQRT"},
                    {"op": "PRINT"},
                    {"op": "HALT"},
                ],
            }
        }
        api_client.post("/machines/zuse-z3/load", json=payload)
        resp = api_client.post("/machines/zuse-z3/run?max_steps=100")
        assert resp.status_code == 200
        data = resp.json()
        assert data["halted"] is True
        assert data["snapshot"]["output_tape"] == [4.0]

    def test_list_machine_items_have_required_fields(self, api_client: Any) -> None:
        resp = api_client.get("/machines")
        for item in resp.json():
            for field in ("id", "name", "year", "country", "inventor", "category", "brief"):
                assert field in item, f"Missing field {field!r} in {item['id']}"

    def test_unknown_machine_step_404(self, api_client: Any) -> None:
        resp = api_client.post("/machines/fake-machine/step")
        assert resp.status_code == 404

    def test_jacquard_load_and_step(self, api_client: Any) -> None:
        """Load a 2-card Jacquard deck via the API and step through it."""
        api_client.post("/machines/jacquard-loom/reset")
        payload = {
            "payload": {
                "cards": [
                    [1, 0, 1, 0, 1, 0, 1, 0],
                    [0, 1, 0, 1, 0, 1, 0, 1],
                ]
            }
        }
        resp = api_client.post("/machines/jacquard-loom/load", json=payload)
        assert resp.status_code == 200
        # Step once -- processes card 0
        resp = api_client.post("/machines/jacquard-loom/step")
        assert resp.status_code == 200
        data = resp.json()
        assert data["cycle_count"] == 1
        assert data["columns"] == [1, 0, 1, 0, 1, 0, 1, 0]
        # Step again -- processes card 1
        resp = api_client.post("/machines/jacquard-loom/step")
        assert resp.json()["columns"] == [0, 1, 0, 1, 0, 1, 0, 1]
        assert resp.json()["cycle_count"] == 2

    def test_jacquard_snapshot_accumulates_pattern(self, api_client: Any) -> None:
        """After 2 steps, get_snapshot() has the full 2-row pattern."""
        api_client.post("/machines/jacquard-loom/reset")
        payload = {
            "payload": {
                "cards": [
                    [1, 1, 0, 0, 1, 1, 0, 0],
                    [0, 0, 1, 1, 0, 0, 1, 1],
                ]
            }
        }
        api_client.post("/machines/jacquard-loom/load", json=payload)
        api_client.post("/machines/jacquard-loom/step")
        api_client.post("/machines/jacquard-loom/step")
        snap_resp = api_client.get("/machines/jacquard-loom/state")
        assert snap_resp.status_code == 200
        snap = snap_resp.json()["snapshot"]
        assert snap["pattern"] == [
            [1, 1, 0, 0, 1, 1, 0, 0],
            [0, 0, 1, 1, 0, 0, 1, 1],
        ]

    def test_materials_in_detail_response(self, api_client: Any) -> None:
        resp = api_client.get("/machines/pascaline")
        assert resp.status_code == 200
        data = resp.json()
        assert "materials" in data
        assert data["materials"]["gears"] == "brass"

    def test_operation_time_ms_in_detail_response(self, api_client: Any) -> None:
        resp = api_client.get("/machines/eniac")
        assert resp.status_code == 200
        data = resp.json()
        assert "operation_time_ms" in data
        assert abs(data["operation_time_ms"]["add"] - 0.2) < 0.01

    def test_list_machines_includes_materials_and_timing(self, api_client: Any) -> None:
        resp = api_client.get("/machines")
        assert resp.status_code == 200
        for item in resp.json():
            assert "materials" in item, f"Missing 'materials' on {item['id']}"
            assert "operation_time_ms" in item, f"Missing 'operation_time_ms' on {item['id']}"

    def test_astrolabe_load_sets_date_and_latitude(self, api_client: Any) -> None:
        """POST /machines/astrolabe/load with a date payload configures the adapter."""
        api_client.post("/machines/astrolabe/reset")
        payload = {"payload": {"date": "2026-06-21", "latitude": 48.8}}
        resp = api_client.post("/machines/astrolabe/load", json=payload)
        assert resp.status_code == 200
        # Step once -- adapter should use the new date
        step_resp = api_client.post("/machines/astrolabe/step")
        assert step_resp.status_code == 200
        assert step_resp.json()["cycle_count"] == 1

    def test_astrolabe_state_has_altitude(self, api_client: Any) -> None:
        """After loading and stepping, state snapshot includes altitude_deg."""
        api_client.post("/machines/astrolabe/reset")
        api_client.post(
            "/machines/astrolabe/load", json={"payload": {"date": "2026-03-20", "latitude": 0.0}}
        )
        api_client.post("/machines/astrolabe/step")
        state_resp = api_client.get("/machines/astrolabe/state")
        snap = state_resp.json()["snapshot"]
        assert "altitude_deg" in snap
        assert -90.0 <= snap["altitude_deg"] <= 90.0

    def test_hollerith_load_deck_via_api(self, api_client: Any) -> None:
        """POST /machines/hollerith-tabulator/load with a data_deck payload."""
        api_client.post("/machines/hollerith-tabulator/reset")
        payload = {"payload": {"cards": [{"row": 0, "columns": [1, 3, 5, 7]}]}}
        resp = api_client.post("/machines/hollerith-tabulator/load", json=payload)
        assert resp.status_code == 200

    def test_antikythera_load_years_via_api(self, api_client: Any) -> None:
        """POST /machines/antikythera/load with years payload."""
        api_client.post("/machines/antikythera/reset")
        payload = {"payload": {"years": 4.0}}
        resp = api_client.post("/machines/antikythera/load", json=payload)
        assert resp.status_code == 200
        step_resp = api_client.post("/machines/antikythera/step")
        assert step_resp.status_code == 200

    def test_analytical_engine_assembly_load_and_step(self, api_client: Any) -> None:
        """POST /machines/analytical-engine/load with assembly source + run."""
        api_client.post("/machines/analytical-engine/reset")
        payload = {"payload": {"source": "LOAD A, 3\nLOAD B, 4\nADD A, B\nHALT"}}
        resp = api_client.post("/machines/analytical-engine/load", json=payload)
        assert resp.status_code == 200

    def test_load_unknown_machine_404(self, api_client: Any) -> None:
        resp = api_client.post("/machines/nonexistent/load", json={"payload": {}})
        assert resp.status_code == 404

    def test_run_unknown_machine_404(self, api_client: Any) -> None:
        resp = api_client.post("/machines/ghost/run")
        assert resp.status_code == 404

    def test_step_returns_cycle_count(self, api_client: Any) -> None:
        """step() response always includes cycle_count."""
        api_client.post("/machines/pascaline/reset")
        resp = api_client.post("/machines/pascaline/step")
        assert resp.status_code == 200
        assert "cycle_count" in resp.json()

    def test_list_returns_30_machines(self, api_client: Any) -> None:
        resp = api_client.get("/machines")
        assert resp.status_code == 200
        assert len(resp.json()) == 30


# ---------------------------------------------------------------------------
# Registry data: materials and timing constants
# ---------------------------------------------------------------------------


_SYMBOLIC_MACHINES = {"tally-marks", "clay-tokens", "quipu"}


class TestMaterialsAndTiming:
    def test_all_non_symbolic_machines_have_materials(self) -> None:
        for m in MACHINES:
            if m.id in _SYMBOLIC_MACHINES:
                continue
            assert len(m.materials) >= 1, f"{m.id}: no materials assigned"

    def test_known_material_assignments(self) -> None:
        from backend.src.emulator.machine_registry import MACHINE_BY_ID

        pascaline = MACHINE_BY_ID["pascaline"]
        antikythera = MACHINE_BY_ID["antikythera"]
        eniac = MACHINE_BY_ID["eniac"]

        assert pascaline.materials["gears"] == "brass"
        assert antikythera.materials["frame"] == "corinthian_bronze"
        assert eniac.materials["panels"] == "aluminum_alloy_1940s"

    def test_operation_time_ms_plausible(self) -> None:
        for m in MACHINES:
            for op, ms in m.operation_time_ms.items():
                assert ms > 0, f"{m.id} op={op}: timing must be positive"

    def test_edsac_timing_ms_instruction(self) -> None:
        from backend.src.emulator.machine_registry import MACHINE_BY_ID

        edsac = MACHINE_BY_ID["edsac"]
        assert abs(edsac.operation_time_ms["instruction"] - 1.5) < 0.01

    def test_eniac_add_faster_than_mark_i_add(self) -> None:
        from backend.src.emulator.machine_registry import MACHINE_BY_ID

        eniac = MACHINE_BY_ID["eniac"]
        mark1 = MACHINE_BY_ID["harvard-mark-i"]
        assert eniac.operation_time_ms["add"] < mark1.operation_time_ms["add"]

    def test_colossus_char_read_faster_than_eniac_add(self) -> None:
        from backend.src.emulator.machine_registry import MACHINE_BY_ID

        colossus = MACHINE_BY_ID["colossus"]
        eniac = MACHINE_BY_ID["eniac"]
        # Both are 0.2 ms (5000 ops/sec); Colossus char_read <= ENIAC add
        assert colossus.operation_time_ms["char_read"] <= eniac.operation_time_ms["add"]

    def test_materials_are_valid_library_keys_or_note(self) -> None:
        from backend.src.emulator.materials import MaterialLibrary

        lib = MaterialLibrary()
        valid_keys = set(lib.names())
        for m in MACHINES:
            for role, mat_key in m.materials.items():
                if role == "note":
                    continue
                assert (
                    mat_key in valid_keys
                ), f"{m.id}: material role '{role}' -> '{mat_key}' not in MaterialLibrary"

    def test_adapter_get_operation_time_ms(self) -> None:
        """Adapters with timing constants return the right values."""
        _, eniac_adapter = build_machine("eniac")  # type: ignore[misc]
        timing = eniac_adapter.get_operation_time_ms()
        assert isinstance(timing, dict)
        assert abs(timing["add"] - 0.2) < 0.01

    def test_adapter_base_returns_empty_dict_when_no_timing(self) -> None:
        """Adapters without timing overrides return empty dict from base."""
        _, tally_adapter = build_machine("tally-marks")  # type: ignore[misc]
        timing = tally_adapter.get_operation_time_ms()
        assert isinstance(timing, dict)
        # tally-marks has no timing constants (symbolic)
        assert len(timing) == 0
