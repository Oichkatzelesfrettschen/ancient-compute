"""Tests for tally mark emulator."""

from backend.src.emulator.tally_marks import TallyMarksEmulator


def test_tally_marks_basic_counts():
    emu = TallyMarksEmulator()
    assert emu.step(4) == 4
    assert emu.render() == "||||"
    assert emu.step(1) == 5
    assert emu.render() == "|||||"


def test_tally_marks_underflow():
    emu = TallyMarksEmulator()
    emu.step(3)
    assert emu.step(-10) == 0
    assert emu.render() == ""


class TestTallyMarksEmulator:
    def test_initial_state_zero(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.state()["count"] == 0

    def test_initial_render_empty(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.render() == ""

    def test_step_returns_new_count(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.step(7) == 7

    def test_step_accumulates(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(3)
        assert emu.step(4) == 7

    def test_step_negative_partial(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(10)
        assert emu.step(-3) == 7

    def test_step_negative_clamps_at_zero(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(5)
        assert emu.step(-100) == 0

    def test_step_zero_delta_no_change(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(8)
        assert emu.step(0) == 8

    def test_render_single_group(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(5)
        assert emu.render() == "|||||"

    def test_render_two_groups(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(10)
        assert emu.render() == "||||||||||"

    def test_render_group_plus_remainder(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(7)
        # 7 = 1 full group of 5 + 2 singles
        assert emu.render() == "|||||" + "||"

    def test_render_length_equals_count(self) -> None:
        emu = TallyMarksEmulator()
        for n in [0, 1, 5, 11, 20]:
            emu.reset()
            emu.step(n)
            assert len(emu.render()) == n

    def test_run_empty_list(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([]) == 0

    def test_run_positive_deltas(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([1, 2, 3]) == 6

    def test_run_mixed_deltas(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([10, -3, 2]) == 9

    def test_run_all_negative_clamps(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([-5, -5]) == 0

    def test_state_has_count_and_tally(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(6)
        s = emu.state()
        assert "count" in s
        assert "tally" in s

    def test_state_count_matches_step(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(13)
        assert emu.state()["count"] == 13

    def test_state_tally_matches_render(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(8)
        s = emu.state()
        assert s["tally"] == emu.render()

    def test_reset_zeroes_count(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(15)
        emu.reset()
        assert emu.state()["count"] == 0

    def test_reset_clears_render(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(15)
        emu.reset()
        assert emu.render() == ""

    def test_step_after_reset(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(10)
        emu.reset()
        assert emu.step(3) == 3


class TestTallyMarksRenderGroups:
    """Render output structure: groupings of five and remainders."""

    def test_render_zero_is_empty(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.render() == ""

    def test_render_exactly_five_is_five_pipes(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(5)
        assert emu.render() == "|||||"

    def test_render_ten_is_two_groups(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(10)
        assert emu.render() == "||||||||||"

    def test_render_fifteen_is_three_groups(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(15)
        assert emu.render() == "|||||||||||||||"

    def test_render_six_has_one_group_plus_one(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(6)
        rendered = emu.render()
        assert len(rendered) == 6
        assert rendered.count("|") == 6

    def test_render_one_is_single_pipe(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(1)
        assert emu.render() == "|"

    def test_render_length_always_equals_count(self) -> None:
        emu = TallyMarksEmulator()
        for n in [0, 1, 4, 5, 6, 9, 10, 11, 20, 25, 26]:
            emu.reset()
            emu.step(n)
            assert len(emu.render()) == n

    def test_render_contains_only_pipes(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(17)
        rendered = emu.render()
        assert all(ch == "|" for ch in rendered)


class TestTallyMarksStepProperties:
    """Invariants that should hold for any step() call."""

    def test_step_result_always_non_negative(self) -> None:
        emu = TallyMarksEmulator()
        for delta in [-100, -50, -1, 0, 1, 50, 100]:
            result = emu.step(delta)
            assert result >= 0

    def test_step_positive_is_monotone_increasing(self) -> None:
        emu = TallyMarksEmulator()
        prev = 0
        for n in [1, 3, 5, 2, 4]:
            curr = emu.step(n)
            assert curr >= prev
            prev = curr

    def test_step_zero_preserves_count(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(12)
        before = emu.state()["count"]
        emu.step(0)
        assert emu.state()["count"] == before

    def test_step_large_negative_clamps_to_zero(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(5)
        assert emu.step(-9999) == 0

    def test_step_large_positive(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.step(10_000) == 10_000

    def test_step_returns_same_as_state_count(self) -> None:
        emu = TallyMarksEmulator()
        result = emu.step(9)
        assert result == emu.state()["count"]

    def test_multiple_resets_between_steps(self) -> None:
        emu = TallyMarksEmulator()
        for _ in range(3):
            emu.step(100)
            emu.reset()
        assert emu.state()["count"] == 0


class TestTallyMarksRunBehavior:
    """run() accumulates steps and handles edge cases."""

    def test_run_empty_returns_zero(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([]) == 0

    def test_run_single_positive(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([7]) == 7

    def test_run_modifies_internal_state(self) -> None:
        emu = TallyMarksEmulator()
        emu.run([5, 3])
        assert emu.state()["count"] == 8

    def test_run_clamps_at_each_step(self) -> None:
        # run(-10, 5): clamps to 0 after -10, then +5 = 5
        emu = TallyMarksEmulator()
        result = emu.run([-10, 5])
        assert result == 5

    def test_run_after_existing_state(self) -> None:
        emu = TallyMarksEmulator()
        emu.step(10)
        result = emu.run([5, -3])
        assert result == 12

    def test_run_all_zeros_returns_zero(self) -> None:
        emu = TallyMarksEmulator()
        assert emu.run([0, 0, 0, 0]) == 0

    def test_run_returns_final_count(self) -> None:
        emu = TallyMarksEmulator()
        result = emu.run([1, 2, 3, 4, 5])
        assert result == 15
        assert result == emu.state()["count"]

    def test_run_render_consistent_after_run(self) -> None:
        emu = TallyMarksEmulator()
        emu.run([3, 4])
        assert len(emu.render()) == 7
