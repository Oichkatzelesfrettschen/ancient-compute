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
