"""Subsystem specification dataclasses for the hybrid engine.

Each dataclass represents the engineering parameters of one subsystem,
with source traceability annotations.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List


@dataclass
class FigureWheel:
    """Single decimal digit wheel (0-9) on a column axis."""
    outer_diameter_mm: float = 30.0  # ASSUMPTION
    thickness_mm: float = 5.0  # ASSUMPTION
    material: str = "brass"  # SOURCE:SWADE-2001
    detent_positions: int = 10  # SOURCE:SMG-DE2-TECH
    detent_force_N: float = 0.5  # ASSUMPTION


@dataclass
class Column:
    """One store column of stacked figure wheels."""
    digits: int = 31  # SOURCE:SMG-DE2-TECH
    axis_diameter_mm: float = 8.0  # ASSUMPTION
    axis_material: str = "steel"  # ASSUMPTION
    wheel: FigureWheel = field(default_factory=FigureWheel)

    @property
    def axis_length_mm(self) -> float:
        """Total axis length including spacers."""
        spacer_mm = 1.0  # ASSUMPTION: 1mm spacer between wheels
        return self.digits * (self.wheel.thickness_mm + spacer_mm)


@dataclass
class Store:
    """Variable memory: array of columns."""
    num_columns: int = 8  # Decision D-3
    column: Column = field(default_factory=Column)


@dataclass
class Printer:
    """Type-wheel printer apparatus."""
    type_wheels: int = 8  # SOURCE:SMG-DE2-TECH
    digits_per_wheel: int = 10  # SOURCE:SMG-DE2-TECH
    lines_per_page: int = 50  # ASSUMPTION
    wheel_diameter_mm: float = 25.0  # ASSUMPTION


@dataclass
class Stereotyper:
    """Soft-metal mold stereotyper."""
    mold_material: str = "type_metal"  # SOURCE:SWADE-2000
    mold_width_mm: float = 120.0  # ASSUMPTION
    mold_height_mm: float = 80.0  # ASSUMPTION
    punch_depth_mm: float = 0.5  # ASSUMPTION
    x_positions: int = 8  # SOURCE:SMG-DE2-TECH
    y_positions: int = 50  # ASSUMPTION


@dataclass
class CamPair:
    """One conjugate cam pair on the main shaft."""
    cam_number: int = 0
    function: str = ""
    active_start_deg: float = 0.0
    active_end_deg: float = 0.0
    source: str = "ASSUMPTION"


@dataclass
class TimingSystem:
    """Cam timing system."""
    cam_pairs: List[CamPair] = field(default_factory=lambda: [
        CamPair(1, "Column latch", 0, 30, "ASSUMPTION"),
        CamPair(2, "Addition engage", 30, 60, "ASSUMPTION"),
        CamPair(3, "Carry eval 1", 60, 90, "SOURCE:BROMLEY-1990"),
        CamPair(4, "Column advance", 90, 150, "ASSUMPTION"),
        CamPair(5, "Carry eval 2", 120, 150, "SOURCE:BROMLEY-1990"),
        CamPair(6, "Print setup", 180, 240, "ASSUMPTION"),
        CamPair(7, "Print strike", 240, 300, "ASSUMPTION"),
        CamPair(8, "Stereo + reset", 300, 360, "ASSUMPTION"),
    ])


@dataclass
class PowerTrain:
    """Prime mover specification."""
    prime_mover: str = "hand_crank"
    crank_radius_mm: float = 200.0  # ASSUMPTION
    target_rpm: float = 30.0  # ASSUMPTION
    gear_ratio: float = 4.0  # ASSUMPTION
    flywheel_mass_kg: float = 25.0  # ASSUMPTION
    flywheel_diameter_mm: float = 600.0  # ASSUMPTION


@dataclass
class HybridEngine:
    """Top-level specification for the complete hybrid engine."""
    store: Store = field(default_factory=Store)
    printer: Printer = field(default_factory=Printer)
    stereotyper: Stereotyper = field(default_factory=Stereotyper)
    timing: TimingSystem = field(default_factory=TimingSystem)
    power_train: PowerTrain = field(default_factory=PowerTrain)
