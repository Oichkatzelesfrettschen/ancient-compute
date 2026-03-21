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
