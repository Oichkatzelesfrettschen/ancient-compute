"""Tests for quipu emulator."""

from backend.src.emulator.quipu import QuipuEmulator


def test_quipu_encode_decode():
    emu = QuipuEmulator()
    emu.encode_number("A", 123)
    emu.encode_number("A", 7)
    assert emu.decode_number("A") == [123, 7]
    assert emu.sum_by_category("A") == 130


class TestQuipuEmulator:
    def test_initial_records_empty(self) -> None:
        emu = QuipuEmulator()
        assert emu.state()["records"] == []

    def test_encode_single_value(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("tributes", 42)
        assert emu.decode_number("tributes") == [42]

    def test_encode_multiple_same_category(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("llamas", 10)
        emu.encode_number("llamas", 20)
        emu.encode_number("llamas", 30)
        assert emu.decode_number("llamas") == [10, 20, 30]

    def test_encode_multiple_categories(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("corn", 5)
        emu.encode_number("potatoes", 8)
        assert emu.decode_number("corn") == [5]
        assert emu.decode_number("potatoes") == [8]

    def test_decode_unknown_category_returns_empty(self) -> None:
        emu = QuipuEmulator()
        assert emu.decode_number("phantom") == []

    def test_sum_single_value(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("gold", 99)
        assert emu.sum_by_category("gold") == 99

    def test_sum_multiple_values(self) -> None:
        emu = QuipuEmulator()
        for v in [10, 20, 30, 40]:
            emu.encode_number("silver", v)
        assert emu.sum_by_category("silver") == 100

    def test_sum_empty_category_zero(self) -> None:
        emu = QuipuEmulator()
        assert emu.sum_by_category("missing") == 0

    def test_decode_filters_by_category(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 1)
        emu.encode_number("B", 2)
        emu.encode_number("A", 3)
        assert emu.decode_number("A") == [1, 3]
        assert emu.decode_number("B") == [2]

    def test_encode_zero_value(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("empty", 0)
        assert emu.decode_number("empty") == [0]
        assert emu.sum_by_category("empty") == 0

    def test_state_records_count(self) -> None:
        emu = QuipuEmulator()
        for i in range(5):
            emu.encode_number("x", i)
        assert len(emu.state()["records"]) == 5

    def test_state_records_structure(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("knots", 7)
        records = emu.state()["records"]
        assert len(records) == 1
        assert records[0]["category"] == "knots"
        assert records[0]["value"] == 7

    def test_reset_clears_records(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 1)
        emu.encode_number("B", 2)
        emu.reset()
        assert emu.state()["records"] == []

    def test_sum_after_reset_is_zero(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 100)
        emu.reset()
        assert emu.sum_by_category("A") == 0

    def test_large_value_encodes_correctly(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("census", 10_000)
        assert emu.sum_by_category("census") == 10_000


class TestQuipuEmulatorStateConsistency:
    """State consistency and multi-operation correctness."""

    def test_state_keys_present(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("x", 1)
        s = emu.state()
        assert "records" in s

    def test_state_record_has_category_and_value(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("wool", 55)
        rec = emu.state()["records"][0]
        assert rec["category"] == "wool"
        assert rec["value"] == 55

    def test_encode_order_preserved(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("X", 100)
        emu.encode_number("X", 200)
        emu.encode_number("X", 300)
        vals = emu.decode_number("X")
        assert vals == [100, 200, 300]

    def test_sum_large_series(self) -> None:
        emu = QuipuEmulator()
        for i in range(1, 11):
            emu.encode_number("seq", i)
        assert emu.sum_by_category("seq") == 55

    def test_multiple_categories_sum_independent(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 10)
        emu.encode_number("B", 20)
        emu.encode_number("A", 30)
        assert emu.sum_by_category("A") == 40
        assert emu.sum_by_category("B") == 20

    def test_reset_then_re_encode(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("Q", 99)
        emu.reset()
        emu.encode_number("Q", 1)
        assert emu.sum_by_category("Q") == 1

    def test_decode_after_reset_is_empty(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("Z", 7)
        emu.reset()
        assert emu.decode_number("Z") == []

    def test_negative_value_encodes(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("debt", -50)
        assert emu.decode_number("debt") == [-50]
        assert emu.sum_by_category("debt") == -50


class TestQuipuEdgeCases:
    """Edge cases: special category names, extreme values, many records."""

    def test_category_with_spaces(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("llama wool", 42)
        assert emu.decode_number("llama wool") == [42]

    def test_category_with_numbers(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("row_1", 5)
        assert emu.decode_number("row_1") == [5]

    def test_encode_many_records_same_category(self) -> None:
        emu = QuipuEmulator()
        for i in range(50):
            emu.encode_number("K", i)
        vals = emu.decode_number("K")
        assert len(vals) == 50
        assert vals == list(range(50))

    def test_encode_large_positive(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("tribute", 999_999_999)
        assert emu.sum_by_category("tribute") == 999_999_999

    def test_encode_large_negative(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("debt", -999_999_999)
        assert emu.sum_by_category("debt") == -999_999_999

    def test_mixed_sign_sum(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("balance", 100)
        emu.encode_number("balance", -30)
        emu.encode_number("balance", 50)
        assert emu.sum_by_category("balance") == 120

    def test_many_categories_do_not_interfere(self) -> None:
        emu = QuipuEmulator()
        categories = [f"cat_{i}" for i in range(20)]
        for i, cat in enumerate(categories):
            emu.encode_number(cat, i * 10)
        for i, cat in enumerate(categories):
            assert emu.sum_by_category(cat) == i * 10

    def test_state_records_count_matches_encode_count(self) -> None:
        emu = QuipuEmulator()
        n = 15
        for i in range(n):
            emu.encode_number("X", i)
        assert len(emu.state()["records"]) == n

    def test_decode_empty_after_reset_all_categories(self) -> None:
        emu = QuipuEmulator()
        for c in ["A", "B", "C"]:
            emu.encode_number(c, 1)
        emu.reset()
        for c in ["A", "B", "C"]:
            assert emu.decode_number(c) == []


class TestQuipuStateCopy:
    """Verify state() returns a snapshot, not a live reference."""

    def test_state_records_is_independent_copy(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("X", 1)
        snapshot = emu.state()["records"]
        # Mutate the snapshot; internal state should be unaffected.
        snapshot.append({"category": "INJECT", "value": 999})
        assert len(emu.state()["records"]) == 1

    def test_state_record_dict_structure(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("corn", 7)
        rec = emu.state()["records"][0]
        assert set(rec.keys()) >= {"category", "value"}

    def test_state_record_category_is_string(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("wool", 3)
        rec = emu.state()["records"][0]
        assert isinstance(rec["category"], str)

    def test_state_record_value_matches_encoded(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("tribute", 77)
        rec = emu.state()["records"][0]
        assert int(rec["value"]) == 77

    def test_fresh_state_after_reset(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 5)
        emu.encode_number("B", 10)
        emu.reset()
        s = emu.state()
        assert s["records"] == []


class TestQuipuSumProperties:
    """Sum semantics: empty, independent categories, order independence."""

    def test_sum_no_records_is_zero(self) -> None:
        emu = QuipuEmulator()
        assert emu.sum_by_category("any") == 0

    def test_sum_single_record(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("gold", 42)
        assert emu.sum_by_category("gold") == 42

    def test_sum_two_categories_independent(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("gold", 10)
        emu.encode_number("silver", 20)
        assert emu.sum_by_category("gold") == 10
        assert emu.sum_by_category("silver") == 20

    def test_sum_sequence_1_to_100(self) -> None:
        emu = QuipuEmulator()
        for i in range(1, 101):
            emu.encode_number("seq", i)
        assert emu.sum_by_category("seq") == 5050

    def test_sum_all_zeros_is_zero(self) -> None:
        emu = QuipuEmulator()
        for _ in range(5):
            emu.encode_number("z", 0)
        assert emu.sum_by_category("z") == 0

    def test_decode_length_matches_encode_count(self) -> None:
        emu = QuipuEmulator()
        for i in range(8):
            emu.encode_number("cat", i)
        assert len(emu.decode_number("cat")) == 8

    def test_sum_after_many_mixed_categories(self) -> None:
        emu = QuipuEmulator()
        emu.encode_number("A", 3)
        emu.encode_number("B", 7)
        emu.encode_number("A", 4)
        emu.encode_number("C", 10)
        emu.encode_number("A", 3)
        assert emu.sum_by_category("A") == 10
        assert emu.sum_by_category("B") == 7
        assert emu.sum_by_category("C") == 10
