"""Tests for generic Antikythera gear-train propagation."""

import math

from backend.src.emulator.antikythera import GearEdge, GearTrain


def test_gear_train_propagates_chain():
    train = GearTrain(
        [
            GearEdge(src="A", dst="B", ratio=2.0),
            GearEdge(src="B", dst="C", ratio=0.5),
        ]
    )

    angles = train.propagate(input_angle=10.0, input_gear="A")
    assert angles["A"] == 10.0
    assert angles["B"] == 20.0
    assert angles["C"] == 10.0


class TestGearTrain:
    def test_single_edge_propagation(self) -> None:
        train = GearTrain([GearEdge(src="X", dst="Y", ratio=3.0)])
        angles = train.propagate(5.0, "X")
        assert angles["X"] == 5.0
        assert math.isclose(angles["Y"], 15.0)

    def test_input_gear_always_in_result(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1.0)])
        angles = train.propagate(7.0, "A")
        assert "A" in angles

    def test_zero_input_stays_zero(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=999.0)])
        angles = train.propagate(0.0, "A")
        assert angles["B"] == 0.0

    def test_negative_ratio_reverses_direction(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=-2.0)])
        angles = train.propagate(5.0, "A")
        assert math.isclose(angles["B"], -10.0)

    def test_identity_ratio(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1.0)])
        angles = train.propagate(42.0, "A")
        assert math.isclose(angles["B"], 42.0)

    def test_unreachable_gear_not_in_result(self) -> None:
        # D is not connected from A
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=2.0),
                GearEdge(src="C", dst="D", ratio=2.0),
            ]
        )
        angles = train.propagate(1.0, "A")
        assert "D" not in angles
        assert "C" not in angles

    def test_branching_from_single_source(self) -> None:
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=2.0),
                GearEdge(src="A", dst="C", ratio=3.0),
            ]
        )
        angles = train.propagate(10.0, "A")
        assert math.isclose(angles["B"], 20.0)
        assert math.isclose(angles["C"], 30.0)

    def test_long_chain_five_gears(self) -> None:
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=2.0),
                GearEdge(src="B", dst="C", ratio=2.0),
                GearEdge(src="C", dst="D", ratio=2.0),
                GearEdge(src="D", dst="E", ratio=2.0),
            ]
        )
        angles = train.propagate(1.0, "A")
        assert math.isclose(angles["E"], 16.0)

    def test_each_node_visited_once(self) -> None:
        # Diamond shape: A->B, A->C, B->D, C->D
        # D should only be reachable from B first (BFS order)
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=2.0),
                GearEdge(src="A", dst="C", ratio=3.0),
                GearEdge(src="B", dst="D", ratio=1.0),
                GearEdge(src="C", dst="D", ratio=1.0),
            ]
        )
        angles = train.propagate(1.0, "A")
        # D should appear exactly once (BFS visits it first via B)
        assert "D" in angles
        assert math.isclose(angles["D"], 2.0)  # via A->B (2.0) -> D (2.0*1.0)

    def test_empty_train_returns_only_input(self) -> None:
        train = GearTrain([])
        angles = train.propagate(5.0, "solo")
        assert angles == {"solo": 5.0}

    def test_fractional_ratios(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=224.0 / 50.0)])
        angles = train.propagate(1.0, "A")
        assert math.isclose(angles["B"], 224.0 / 50.0, rel_tol=1e-9)


class TestGearEdge:
    """Unit tests for the GearEdge dataclass."""

    def test_gear_edge_stores_src(self) -> None:
        e = GearEdge(src="A", dst="B", ratio=2.0)
        assert e.src == "A"

    def test_gear_edge_stores_dst(self) -> None:
        e = GearEdge(src="A", dst="B", ratio=2.0)
        assert e.dst == "B"

    def test_gear_edge_stores_ratio(self) -> None:
        e = GearEdge(src="A", dst="B", ratio=3.5)
        assert e.ratio == 3.5

    def test_gear_edge_with_negative_ratio(self) -> None:
        e = GearEdge(src="X", dst="Y", ratio=-4.0)
        assert e.ratio == -4.0

    def test_gear_edge_ratio_zero_propagates_zero(self) -> None:
        e = GearEdge(src="A", dst="B", ratio=0.0)
        train = GearTrain([e])
        angles = train.propagate(10.0, "A")
        assert math.isclose(angles["B"], 0.0)


class TestGearTrainBehavior:
    """Additional GearTrain propagation behavior tests."""

    def test_two_negative_ratios_give_positive_product(self) -> None:
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=-2.0),
                GearEdge(src="B", dst="C", ratio=-3.0),
            ]
        )
        angles = train.propagate(1.0, "A")
        assert math.isclose(angles["C"], 6.0)

    def test_large_ratio_accumulates(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=100.0)])
        angles = train.propagate(3.0, "A")
        assert math.isclose(angles["B"], 300.0)

    def test_all_gears_in_result_for_linear_chain(self) -> None:
        train = GearTrain(
            [
                GearEdge(src="A", dst="B", ratio=1.0),
                GearEdge(src="B", dst="C", ratio=1.0),
            ]
        )
        angles = train.propagate(1.0, "A")
        assert "A" in angles
        assert "B" in angles
        assert "C" in angles

    def test_input_angle_preserved(self) -> None:
        train = GearTrain([GearEdge(src="P", dst="Q", ratio=999.0)])
        angles = train.propagate(42.0, "P")
        assert angles["P"] == 42.0

    def test_metonic_gear_ratio_precision(self) -> None:
        """235/19 Metonic ratio should be preserved to float precision."""
        train = GearTrain([GearEdge(src="b1", dst="lunar", ratio=235.0 / 19.0)])
        angles = train.propagate(1.0, "b1")
        assert math.isclose(angles["lunar"], 235.0 / 19.0, rel_tol=1e-12)
