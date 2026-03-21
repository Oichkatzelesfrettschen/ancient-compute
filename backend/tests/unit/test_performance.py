"""Unit tests for PerformanceAnalyzer and PerformanceMetrics.

Tests cover:
- PerformanceMetrics dataclass defaults and field types
- PerformanceAnalyzer record_step() with/without instruction
- PerformanceAnalyzer get_report() structure and hotspot detection
- _generate_suggestions() thresholds for MULT, DIV, and memory bottlenecks
- Memory address parsing from operands
- Integration with a mock adapter
"""

from __future__ import annotations

from unittest.mock import MagicMock

from backend.src.emulator.performance import PerformanceAnalyzer, PerformanceMetrics

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _make_adapter(cycle_count: int = 0) -> MagicMock:
    adapter = MagicMock()
    adapter.get_cycle_count.return_value = cycle_count
    return adapter


def _make_instr(opcode: str, operands: list[str] | None = None) -> MagicMock:
    instr = MagicMock()
    instr.opcode = opcode
    instr.operands = operands or []
    return instr


# ---------------------------------------------------------------------------
# PerformanceMetrics dataclass
# ---------------------------------------------------------------------------


class TestPerformanceMetrics:
    def test_default_total_cycles_zero(self) -> None:
        m = PerformanceMetrics()
        assert m.total_cycles == 0

    def test_default_instruction_counts_empty(self) -> None:
        m = PerformanceMetrics()
        assert m.instruction_counts == {}

    def test_default_memory_access_counts_empty(self) -> None:
        m = PerformanceMetrics()
        assert m.memory_access_counts == {}

    def test_default_branch_stats_keys(self) -> None:
        m = PerformanceMetrics()
        assert "taken" in m.branch_stats
        assert "not_taken" in m.branch_stats

    def test_default_branch_stats_zero(self) -> None:
        m = PerformanceMetrics()
        assert m.branch_stats["taken"] == 0
        assert m.branch_stats["not_taken"] == 0

    def test_instruction_counts_is_dict(self) -> None:
        m = PerformanceMetrics()
        assert isinstance(m.instruction_counts, dict)

    def test_memory_access_counts_is_dict(self) -> None:
        m = PerformanceMetrics()
        assert isinstance(m.memory_access_counts, dict)

    def test_independent_instances(self) -> None:
        m1 = PerformanceMetrics()
        m2 = PerformanceMetrics()
        m1.instruction_counts["ADD"] = 5
        assert "ADD" not in m2.instruction_counts


# ---------------------------------------------------------------------------
# PerformanceAnalyzer -- construction
# ---------------------------------------------------------------------------


class TestPerformanceAnalyzerInit:
    def test_initial_total_cycles_zero(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        assert pa.metrics.total_cycles == 0

    def test_initial_instruction_counts_empty(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        assert pa.metrics.instruction_counts == {}

    def test_initial_history_empty(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        assert pa.history == []

    def test_adapter_stored(self) -> None:
        adapter = _make_adapter(5)
        pa = PerformanceAnalyzer(adapter)
        assert pa.adapter is adapter


# ---------------------------------------------------------------------------
# record_step -- no instruction
# ---------------------------------------------------------------------------


class TestRecordStepNoInstruction:
    def test_no_instruction_syncs_cycle_count(self) -> None:
        adapter = _make_adapter(cycle_count=42)
        pa = PerformanceAnalyzer(adapter)
        pa.record_step()
        assert pa.metrics.total_cycles == 42

    def test_no_instruction_leaves_counts_empty(self) -> None:
        adapter = _make_adapter(cycle_count=10)
        pa = PerformanceAnalyzer(adapter)
        pa.record_step()
        assert pa.metrics.instruction_counts == {}

    def test_repeated_no_instruction_uses_latest_cycle(self) -> None:
        adapter = _make_adapter(0)
        pa = PerformanceAnalyzer(adapter)
        pa.record_step()
        adapter.get_cycle_count.return_value = 100
        pa.record_step()
        assert pa.metrics.total_cycles == 100


# ---------------------------------------------------------------------------
# record_step -- with instruction
# ---------------------------------------------------------------------------


class TestRecordStepWithInstruction:
    def test_opcode_counted(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("ADD"))
        assert pa.metrics.instruction_counts["ADD"] == 1

    def test_opcode_accumulated(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(5):
            pa.record_step(_make_instr("MULT"))
        assert pa.metrics.instruction_counts["MULT"] == 5

    def test_multiple_opcodes_tracked_independently(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("ADD"))
        pa.record_step(_make_instr("SUB"))
        pa.record_step(_make_instr("ADD"))
        assert pa.metrics.instruction_counts["ADD"] == 2
        assert pa.metrics.instruction_counts["SUB"] == 1

    def test_memory_operand_parsed(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("LOAD", ["[5]"]))
        assert pa.metrics.memory_access_counts.get(5, 0) == 1

    def test_memory_operand_accumulated(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(3):
            pa.record_step(_make_instr("STOR", ["[10]"]))
        assert pa.metrics.memory_access_counts[10] == 3

    def test_non_memory_operand_not_tracked(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("ADD", ["A", "B"]))
        assert pa.metrics.memory_access_counts == {}

    def test_invalid_memory_address_skipped(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("MOV", ["[notanumber]"]))
        assert pa.metrics.memory_access_counts == {}


# ---------------------------------------------------------------------------
# get_report -- structure
# ---------------------------------------------------------------------------


class TestGetReportStructure:
    def test_report_has_total_cycles(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter(7))
        pa.record_step()
        report = pa.get_report()
        assert "total_cycles" in report
        assert report["total_cycles"] == 7

    def test_report_has_instruction_frequency(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        report = pa.get_report()
        assert "instruction_frequency" in report

    def test_report_has_hot_spots(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        report = pa.get_report()
        assert "hot_spots" in report
        assert "opcode" in report["hot_spots"]
        assert "memory" in report["hot_spots"]

    def test_report_has_suggestions(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        report = pa.get_report()
        assert "suggestions" in report
        assert isinstance(report["suggestions"], list)

    def test_empty_analyzer_most_used_opcode_is_none(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        report = pa.get_report()
        assert report["hot_spots"]["opcode"] is None

    def test_most_used_opcode_detected(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        pa.record_step(_make_instr("ADD"))
        pa.record_step(_make_instr("ADD"))
        pa.record_step(_make_instr("SUB"))
        report = pa.get_report()
        assert report["hot_spots"]["opcode"] == "ADD"

    def test_hot_memory_top_5_only(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for addr in range(10):
            pa.record_step(_make_instr("LOAD", [f"[{addr}]"]))
        report = pa.get_report()
        assert len(report["hot_spots"]["memory"]) <= 5


# ---------------------------------------------------------------------------
# _generate_suggestions -- thresholds
# ---------------------------------------------------------------------------


class TestGenerateSuggestions:
    def test_no_suggestions_for_empty_metrics(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        assert pa._generate_suggestions() == []

    def test_high_mult_triggers_suggestion(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(11):
            pa.record_step(_make_instr("MULT"))
        suggestions = pa._generate_suggestions()
        assert any("MULT" in s for s in suggestions)

    def test_low_mult_no_suggestion(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(5):
            pa.record_step(_make_instr("MULT"))
        suggestions = pa._generate_suggestions()
        assert not any("MULT" in s for s in suggestions)

    def test_high_div_triggers_suggestion(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(6):
            pa.record_step(_make_instr("DIV"))
        suggestions = pa._generate_suggestions()
        assert any("DIV" in s for s in suggestions)

    def test_low_div_no_suggestion(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(3):
            pa.record_step(_make_instr("DIV"))
        suggestions = pa._generate_suggestions()
        assert not any("DIV" in s for s in suggestions)

    def test_memory_bottleneck_detected(self) -> None:
        """Address accessed >50% of cycles triggers bottleneck suggestion."""
        adapter = _make_adapter(cycle_count=10)
        pa = PerformanceAnalyzer(adapter)
        # 8 accesses to addr 3, total_cycles will sync to 10 on next record_step
        for _ in range(8):
            pa.record_step(_make_instr("LOAD", ["[3]"]))
        pa.record_step()  # sync cycles to 10
        suggestions = pa._generate_suggestions()
        assert any("[3]" in s and "bottleneck" in s for s in suggestions)

    def test_suggestions_is_list_of_strings(self) -> None:
        pa = PerformanceAnalyzer(_make_adapter())
        for _ in range(15):
            pa.record_step(_make_instr("MULT"))
        suggestions = pa._generate_suggestions()
        assert all(isinstance(s, str) for s in suggestions)
