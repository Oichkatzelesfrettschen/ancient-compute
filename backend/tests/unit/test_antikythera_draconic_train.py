import math

from backend.src.emulator.antikythera import (
    AntikytheraDraconicModel,
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
