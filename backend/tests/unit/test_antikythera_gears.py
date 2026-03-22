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


class TestGearTrainEdgeCases:
    """Edge cases for GearTrain propagation."""

    def test_float_input_angle_preserved(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1.0)])
        angles = train.propagate(3.14159, "A")
        assert math.isclose(angles["A"], 3.14159)

    def test_very_large_ratio(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1000.0)])
        angles = train.propagate(1.0, "A")
        assert math.isclose(angles["B"], 1000.0)

    def test_very_small_ratio(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=0.001)])
        angles = train.propagate(1000.0, "A")
        assert math.isclose(angles["B"], 1.0)

    def test_negative_input_angle(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=2.0)])
        angles = train.propagate(-5.0, "A")
        assert math.isclose(angles["B"], -10.0)

    def test_result_keys_are_strings(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1.0)])
        angles = train.propagate(1.0, "A")
        for key in angles:
            assert isinstance(key, str)

    def test_result_values_are_numeric(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=2.0)])
        angles = train.propagate(1.0, "A")
        for value in angles.values():
            assert isinstance(value, (int, float))

    def test_star_topology_all_children_reachable(self) -> None:
        train = GearTrain([
            GearEdge(src="center", dst="A", ratio=2.0),
            GearEdge(src="center", dst="B", ratio=3.0),
            GearEdge(src="center", dst="C", ratio=4.0),
        ])
        angles = train.propagate(1.0, "center")
        assert math.isclose(angles["A"], 2.0)
        assert math.isclose(angles["B"], 3.0)
        assert math.isclose(angles["C"], 4.0)


class TestGearEdgeComparisons:
    """GearEdge field type and equality tests."""

    def test_two_edges_same_fields_equal(self) -> None:
        e1 = GearEdge(src="A", dst="B", ratio=2.0)
        e2 = GearEdge(src="A", dst="B", ratio=2.0)
        assert e1 == e2

    def test_edges_different_ratio_not_equal(self) -> None:
        e1 = GearEdge(src="A", dst="B", ratio=2.0)
        e2 = GearEdge(src="A", dst="B", ratio=3.0)
        assert e1 != e2

    def test_gear_edge_src_type(self) -> None:
        e = GearEdge(src="G1", dst="G2", ratio=1.5)
        assert isinstance(e.src, str)

    def test_gear_edge_dst_type(self) -> None:
        e = GearEdge(src="G1", dst="G2", ratio=1.5)
        assert isinstance(e.dst, str)

    def test_gear_edge_ratio_type(self) -> None:
        e = GearEdge(src="A", dst="B", ratio=5.0)
        assert isinstance(e.ratio, (int, float))

    def test_gear_edge_with_long_names(self) -> None:
        e = GearEdge(src="b1_driving", dst="a1_driven", ratio=224.0 / 50.0)
        train = GearTrain([e])
        angles = train.propagate(1.0, "b1_driving")
        assert math.isclose(angles["a1_driven"], 224.0 / 50.0)

    def test_ratio_preserved_exactly(self) -> None:
        ratio = 7.0 / 3.0
        train = GearTrain([GearEdge(src="X", dst="Y", ratio=ratio)])
        angles = train.propagate(1.0, "X")
        assert math.isclose(angles["Y"], ratio, rel_tol=1e-12)


class TestGearTrainPropagationExtra:
    """Extra propagation correctness and numeric tests."""

    def test_two_stage_ratio_product(self) -> None:
        train = GearTrain([
            GearEdge(src="A", dst="B", ratio=4.0),
            GearEdge(src="B", dst="C", ratio=3.0),
        ])
        angles = train.propagate(1.0, "A")
        assert math.isclose(angles["C"], 12.0)

    def test_chain_of_three_halving_ratios(self) -> None:
        train = GearTrain([
            GearEdge(src="A", dst="B", ratio=0.5),
            GearEdge(src="B", dst="C", ratio=0.5),
            GearEdge(src="C", dst="D", ratio=0.5),
        ])
        angles = train.propagate(8.0, "A")
        assert math.isclose(angles["D"], 1.0)

    def test_gear_not_connected_to_input_not_in_result(self) -> None:
        train = GearTrain([
            GearEdge(src="A", dst="B", ratio=2.0),
            GearEdge(src="X", dst="Y", ratio=5.0),
        ])
        angles = train.propagate(1.0, "A")
        assert "X" not in angles
        assert "Y" not in angles

    def test_input_gear_without_edges_returns_only_self(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=2.0)])
        angles = train.propagate(7.0, "B")
        # B is a destination; propagating from B has no outgoing edges -> just B
        assert "B" in angles
        assert angles["B"] == 7.0

    def test_ratio_of_one_is_identity(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=1.0)])
        for v in (0.0, 1.0, -5.5, 1000.0):
            angles = train.propagate(v, "A")
            assert math.isclose(angles["B"], v)

    def test_negative_times_negative_gives_positive_at_third_gear(self) -> None:
        train = GearTrain([
            GearEdge(src="A", dst="B", ratio=-1.0),
            GearEdge(src="B", dst="C", ratio=-1.0),
        ])
        angles = train.propagate(5.0, "A")
        assert math.isclose(angles["C"], 5.0)

    def test_precise_antikythera_ratio(self) -> None:
        """224/50 * 60/20 = 13.44 exactly as computed by gear chain."""
        train = GearTrain([
            GearEdge(src="b1", dst="a1", ratio=224.0 / 50.0),
            GearEdge(src="r1", dst="s1", ratio=60.0 / 20.0),
        ])
        a1 = train.propagate(1.0, "b1")["a1"]
        s1 = train.propagate(a1, "r1")["s1"]
        assert math.isclose(s1, 13.44, rel_tol=1e-9)

    def test_result_type_values_float(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=2.0)])
        angles = train.propagate(3.0, "A")
        assert isinstance(angles["A"], float)
        assert isinstance(angles["B"], float)


class TestGearEdgeImmutability:
    """GearEdge field access and equality edge cases."""

    def test_different_src_not_equal(self) -> None:
        e1 = GearEdge(src="A", dst="B", ratio=1.0)
        e2 = GearEdge(src="X", dst="B", ratio=1.0)
        assert e1 != e2

    def test_different_dst_not_equal(self) -> None:
        e1 = GearEdge(src="A", dst="B", ratio=1.0)
        e2 = GearEdge(src="A", dst="Z", ratio=1.0)
        assert e1 != e2

    def test_same_object_is_equal_to_itself(self) -> None:
        e = GearEdge(src="P", dst="Q", ratio=7.0)
        assert e == e

    def test_ratio_one_half(self) -> None:
        train = GearTrain([GearEdge(src="A", dst="B", ratio=0.5)])
        angles = train.propagate(10.0, "A")
        assert math.isclose(angles["B"], 5.0)

    def test_very_small_ratio_precision(self) -> None:
        ratio = 1.0 / 1000.0
        train = GearTrain([GearEdge(src="A", dst="B", ratio=ratio)])
        angles = train.propagate(1000.0, "A")
        assert math.isclose(angles["B"], 1.0, rel_tol=1e-9)


class TestGearTrainSingleEdge:
    """Single-edge gear train invariants."""

    def test_unit_ratio_identity(self) -> None:
        import math
        from backend.src.emulator.antikythera import GearEdge, GearTrain
        train = GearTrain([GearEdge(src="X", dst="Y", ratio=1.0)])
        angles = train.propagate(45.0, "X")
        assert math.isclose(angles["Y"], 45.0)
