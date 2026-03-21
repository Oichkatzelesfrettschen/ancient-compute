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
