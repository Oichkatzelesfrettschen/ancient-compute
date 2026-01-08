"""Tests for generic Antikythera gear-train propagation."""

from backend.src.emulator.antikythera import GearEdge, GearTrain


def test_gear_train_propagates_chain():
    train = GearTrain([
        GearEdge(src="A", dst="B", ratio=2.0),
        GearEdge(src="B", dst="C", ratio=0.5),
    ])

    angles = train.propagate(input_angle=10.0, input_gear="A")
    assert angles["A"] == 10.0
    assert angles["B"] == 20.0
    assert angles["C"] == 10.0
