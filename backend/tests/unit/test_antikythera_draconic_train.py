import math

from backend.src.emulator.antikythera import (
    AntikytheraDraconicModel,
    GearEdge,
    GearTrain,
    draconic_pointer_train_from_arxiv_2104_06181,
)


def test_draconic_train_matches_paper_ratio_magnitude():
    train = draconic_pointer_train_from_arxiv_2104_06181()

    # Assume one full revolution of b1.
    angles = train.propagate(1.0, "b1")
    # Without shaft-coupling modeling, a1 is directly derived.
    assert abs(angles["a1"]) > 4.0

    # If r1 shares a1's shaft, then r1 angle == a1 angle. Encode that coupling
    # externally for now: compute s1 from r1 with a second propagation.
    angles2 = train.propagate(angles["a1"], "r1")
    s1_per_b1 = angles2["s1"]

    # Paper equation (v) gives (b1/a1)*(r1/s1) ≈ 13.422..., and with two meshes
    # the direction flips twice, yielding a positive ratio.
    assert abs(s1_per_b1 - 13.422) < 0.05


def test_draconic_model_helper_matches_train():
    model = AntikytheraDraconicModel()
    assert abs(model.draconic_pointer_rotations_per_b1_rotation() - 13.422) < 0.05


class TestDraconicTrain:
    def test_b1_to_a1_ratio_exact(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert math.isclose(angles["a1"], 224.0 / 50.0, rel_tol=1e-9)

    def test_r1_to_s1_ratio_exact(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "r1")
        assert math.isclose(angles["s1"], 60.0 / 20.0, rel_tol=1e-9)

    def test_compound_ratio_close_to_draconic_months_per_year(self) -> None:
        # True draconic months per year = 365.242/27.2122 = 13.4219...
        model = AntikytheraDraconicModel()
        ratio = model.draconic_pointer_rotations_per_b1_rotation()
        true_draconic = 365.242 / 27.2122
        # Machine ratio (13.44) vs true (13.4219) -- within 0.02
        assert abs(ratio - true_draconic) < 0.02

    def test_model_returns_float(self) -> None:
        model = AntikytheraDraconicModel()
        result = model.draconic_pointer_rotations_per_b1_rotation()
        assert isinstance(result, float)

    def test_model_result_positive(self) -> None:
        model = AntikytheraDraconicModel()
        assert model.draconic_pointer_rotations_per_b1_rotation() > 0

    def test_three_b1_revolutions_scales_linearly(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_one = train.propagate(1.0, "b1")["a1"]
        a1_three = train.propagate(3.0, "b1")["a1"]
        assert math.isclose(a1_three, 3.0 * a1_one, rel_tol=1e-9)

    def test_train_has_four_gears(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        # Train contains b1, a1 (from b1 edge) + r1, s1 (from r1 edge)
        all_gears: set[str] = set()
        for e in train.edges:
            all_gears.add(e.src)
            all_gears.add(e.dst)
        assert all_gears == {"b1", "a1", "r1", "s1"}


class TestDraconicModelAPI:
    """AntikytheraDraconicModel internal structure tests."""

    def test_model_has_train_attribute(self) -> None:
        model = AntikytheraDraconicModel()
        assert hasattr(model, "train")
        assert isinstance(model.train, GearTrain)

    def test_train_has_exactly_two_edges(self) -> None:
        model = AntikytheraDraconicModel()
        assert len(model.train.edges) == 2

    def test_train_edges_are_gear_edge_instances(self) -> None:
        model = AntikytheraDraconicModel()
        for edge in model.train.edges:
            assert isinstance(edge, GearEdge)

    def test_b1_a1_edge_ratio_is_224_over_50(self) -> None:
        model = AntikytheraDraconicModel()
        b1_edge = next(e for e in model.train.edges if e.src == "b1")
        assert math.isclose(b1_edge.ratio, 224.0 / 50.0, rel_tol=1e-12)

    def test_r1_s1_edge_ratio_is_3(self) -> None:
        model = AntikytheraDraconicModel()
        r1_edge = next(e for e in model.train.edges if e.src == "r1")
        assert math.isclose(r1_edge.ratio, 3.0, rel_tol=1e-12)

    def test_two_independent_model_instances_give_same_result(self) -> None:
        m1 = AntikytheraDraconicModel()
        m2 = AntikytheraDraconicModel()
        r1 = m1.draconic_pointer_rotations_per_b1_rotation()
        r2 = m2.draconic_pointer_rotations_per_b1_rotation()
        assert math.isclose(r1, r2, rel_tol=1e-12)

    def test_factory_function_matches_model_train(self) -> None:
        """Factory and model should produce identical gear ratios."""
        factory_train = draconic_pointer_train_from_arxiv_2104_06181()
        model = AntikytheraDraconicModel()
        for fe, me in zip(factory_train.edges, model.train.edges, strict=True):
            assert fe.src == me.src
            assert fe.dst == me.dst
            assert math.isclose(fe.ratio, me.ratio, rel_tol=1e-12)


class TestDraconicModelComposition:
    """Compound draconic ratio and linearity tests."""

    def test_zero_b1_gives_zero_draconic(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1 = train.propagate(0.0, "b1")["a1"]
        s1 = train.propagate(a1, "r1")["s1"]
        assert s1 == 0.0

    def test_two_b1_revolutions_doubles_draconic(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_1 = train.propagate(1.0, "b1")["a1"]
        s1_1 = train.propagate(a1_1, "r1")["s1"]
        a1_2 = train.propagate(2.0, "b1")["a1"]
        s1_2 = train.propagate(a1_2, "r1")["s1"]
        assert math.isclose(s1_2, 2.0 * s1_1, rel_tol=1e-9)

    def test_compound_ratio_exact_value(self) -> None:
        """224/50 * 60/20 = 13.44 exactly."""
        expected = (224.0 / 50.0) * (60.0 / 20.0)
        model = AntikytheraDraconicModel()
        ratio = model.draconic_pointer_rotations_per_b1_rotation()
        assert math.isclose(ratio, expected, rel_tol=1e-9)

    def test_ratio_within_historical_bounds(self) -> None:
        """Draconic month ratio must be between 13.0 and 14.0."""
        model = AntikytheraDraconicModel()
        ratio = model.draconic_pointer_rotations_per_b1_rotation()
        assert 13.0 < ratio < 14.0

    def test_model_result_is_abs_of_chain(self) -> None:
        """Model applies abs() -- result is never negative."""
        model = AntikytheraDraconicModel()
        ratio = model.draconic_pointer_rotations_per_b1_rotation()
        assert ratio > 0

    def test_five_b1_revolutions_five_times_draconic(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_1 = train.propagate(1.0, "b1")["a1"]
        s1_1 = abs(train.propagate(a1_1, "r1")["s1"])
        a1_5 = train.propagate(5.0, "b1")["a1"]
        s1_5 = abs(train.propagate(a1_5, "r1")["s1"])
        assert math.isclose(s1_5, 5.0 * s1_1, rel_tol=1e-9)

    def test_b1_drives_a1_not_s1_directly(self) -> None:
        """b1 is not directly connected to s1 in the gear train."""
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert "s1" not in angles  # b1 only reaches a1 via first stage


class TestDraconicTrainGearNames:
    """Structural and naming properties of the draconic train edges."""

    def test_factory_returns_gear_train_instance(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        assert isinstance(train, GearTrain)

    def test_edges_attribute_is_list(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        assert isinstance(train.edges, list)

    def test_all_edge_sources_are_strings(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        for edge in train.edges:
            assert isinstance(edge.src, str)

    def test_all_edge_destinations_are_strings(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        for edge in train.edges:
            assert isinstance(edge.dst, str)

    def test_all_edge_ratios_are_numeric(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        for edge in train.edges:
            assert isinstance(edge.ratio, (int, float))

    def test_b1_to_a1_edge_exists(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        b1_to_a1 = [e for e in train.edges if e.src == "b1" and e.dst == "a1"]
        assert len(b1_to_a1) == 1

    def test_r1_to_s1_edge_exists(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        r1_to_s1 = [e for e in train.edges if e.src == "r1" and e.dst == "s1"]
        assert len(r1_to_s1) == 1


class TestDraconicModelConsistency:
    """Additional consistency and linearity properties of AntikytheraDraconicModel."""

    def test_half_revolution_gives_half_draconic_pointer(self) -> None:
        m = AntikytheraDraconicModel()
        full = m.draconic_pointer_rotations_per_b1_rotation()
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_half = train.propagate(0.5, "b1")["a1"]
        s1_half = abs(train.propagate(a1_half, "r1")["s1"])
        assert math.isclose(s1_half, full / 2.0, rel_tol=1e-9)

    def test_b1_to_a1_ratio_greater_than_one(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert angles["a1"] > 1.0

    def test_r1_propagation_does_not_reach_a1(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "r1")
        assert "a1" not in angles

    def test_b1_propagation_does_not_reach_s1(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert "s1" not in angles

    def test_b1_propagation_result_has_two_gears(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert len(angles) == 2  # b1 and a1 only

    def test_r1_propagation_result_has_two_gears(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "r1")
        assert len(angles) == 2  # r1 and s1 only


class TestDraconicTrainNumerics:
    """Numerical precision for individual gear stages."""

    def test_b1_to_a1_ratio_float(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert isinstance(angles["a1"], float)

    def test_r1_to_s1_ratio_float(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "r1")
        assert isinstance(angles["s1"], float)


class TestDraconicTrainEdgeProperties:
    """GearEdge attribute tests."""

    def test_gear_edge_ratio_is_positive(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        for edge in train.edges:
            assert edge.ratio > 0

    def test_gear_edge_src_and_dst_differ(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        for edge in train.edges:
            assert edge.src != edge.dst

    def test_b1_a1_edge_numerator_224(self) -> None:
        # The b1->a1 ratio is 224/50; check numerator encodes 224 teeth
        b1_edge = next(
            e for e in draconic_pointer_train_from_arxiv_2104_06181().edges
            if e.src == "b1"
        )
        # ratio = 224/50 = 4.48; numerator proportional test
        assert math.isclose(b1_edge.ratio * 50, 224.0, rel_tol=1e-9)

    def test_r1_s1_edge_ratio_equals_three(self) -> None:
        r1_edge = next(
            e for e in draconic_pointer_train_from_arxiv_2104_06181().edges
            if e.src == "r1"
        )
        assert math.isclose(r1_edge.ratio, 3.0, rel_tol=1e-9)

    def test_propagate_with_fractional_input(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(0.25, "b1")
        assert math.isclose(angles["a1"], 0.25 * (224.0 / 50.0), rel_tol=1e-9)

    def test_gear_train_has_no_duplicate_edges(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        pairs = [(e.src, e.dst) for e in train.edges]
        assert len(pairs) == len(set(pairs))

    def test_propagate_negative_input_reflects_sign(self) -> None:
        # Negative rotation -> negative output
        train = draconic_pointer_train_from_arxiv_2104_06181()
        pos = train.propagate(1.0, "b1")["a1"]
        neg = train.propagate(-1.0, "b1")["a1"]
        assert math.isclose(neg, -pos, rel_tol=1e-9)

    def test_zero_b1_gives_zero_a1(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(0.0, "b1")
        assert angles["a1"] == 0.0

    def test_negative_b1_gives_negative_a1(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(-1.0, "b1")
        # ratio is positive, so negative input gives negative output
        assert angles["a1"] < 0.0

    def test_large_b1_scales_a1_proportionally(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_1 = train.propagate(1.0, "b1")["a1"]
        a1_100 = train.propagate(100.0, "b1")["a1"]
        assert math.isclose(a1_100, 100.0 * a1_1, rel_tol=1e-9)

    def test_draconic_model_ratio_matches_compound(self) -> None:
        """Model result = (b1->a1 ratio) * (r1->s1 ratio)."""
        train = draconic_pointer_train_from_arxiv_2104_06181()
        a1_per_b1 = train.propagate(1.0, "b1")["a1"]
        s1_per_r1 = train.propagate(1.0, "r1")["s1"]
        compound = abs(a1_per_b1) * abs(s1_per_r1)
        model = AntikytheraDraconicModel()
        expected = model.draconic_pointer_rotations_per_b1_rotation()
        assert math.isclose(compound, expected, rel_tol=1e-9)

    def test_b1_a1_ratio_is_224_over_50(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "b1")
        assert math.isclose(angles["a1"], 224.0 / 50.0, rel_tol=1e-9)

    def test_r1_s1_ratio_is_3(self) -> None:
        train = draconic_pointer_train_from_arxiv_2104_06181()
        angles = train.propagate(1.0, "r1")
        assert math.isclose(angles["s1"], 3.0, rel_tol=1e-9)
