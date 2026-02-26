"""
Kinematic Analysis Module for Babbage Analytical Engine

Models angular velocities, accelerations, and forces for every major moving
component. Includes:
- Gear mesh analysis with Lewis bending stress (Shigley Ch.13-14)
- Cam profiles with displacement, velocity, acceleration, jerk (Norton/Shigley Ch.3)
- Grubler-Kutzbach DOF verification
- Main shaft rotation model

All equations are annotated with Shigley's chapter/equation references.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import yaml

from backend.src.emulator.materials import MaterialLibrary


# ---------------------------------------------------------------------------
# Dataclasses
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class GearPair:
    """A meshing gear pair with geometric and material properties."""
    name: str
    tooth_count_driver: int
    tooth_count_driven: int
    module_mm: float
    pressure_angle_deg: float
    face_width_mm: float
    material: str

    @property
    def ratio(self) -> float:
        """Speed ratio (driven/driver tooth count)."""
        return self.tooth_count_driven / self.tooth_count_driver

    @property
    def pitch_diameter_driver_mm(self) -> float:
        """Pitch diameter of driver gear [mm]."""
        return self.module_mm * self.tooth_count_driver

    @property
    def pitch_diameter_driven_mm(self) -> float:
        """Pitch diameter of driven gear [mm]."""
        return self.module_mm * self.tooth_count_driven

    @property
    def addendum_mm(self) -> float:
        """Standard addendum = 1.0 * module [mm]."""
        return self.module_mm

    @property
    def dedendum_mm(self) -> float:
        """Standard dedendum = 1.25 * module [mm]."""
        return 1.25 * self.module_mm

    @property
    def center_distance_mm(self) -> float:
        """Center distance [mm]."""
        return self.module_mm * (self.tooth_count_driver + self.tooth_count_driven) / 2.0


@dataclass(frozen=True)
class CamFollower:
    """Cam mechanism with modified trapezoidal profile."""
    name: str
    rise_angle_deg: float
    dwell_angle_deg: float
    return_angle_deg: float
    total_lift_mm: float
    follower_mass_kg: float
    spring_rate_N_mm: float


@dataclass
class KinematicChain:
    """Complete kinematic chain from drive to output."""
    gear_pairs: List[GearPair] = field(default_factory=list)
    cam_followers: List[CamFollower] = field(default_factory=list)
    input_rpm: float = 30.0
    link_count: int = 0
    joint_count_full: int = 0
    joint_count_half: int = 0


# ---------------------------------------------------------------------------
# Gear Analysis (Shigley Ch.13-14)
# ---------------------------------------------------------------------------

class GearAnalysis:
    """Gear mesh analysis: velocity ratio, contact ratio, Lewis bending stress."""

    @staticmethod
    def velocity_ratio(gear: GearPair) -> float:
        """omega_driven / omega_driver = N_driver / N_driven (Shigley Eq.13-1)."""
        return gear.tooth_count_driver / gear.tooth_count_driven

    @staticmethod
    def output_rpm(gear: GearPair, input_rpm: float) -> float:
        """Output RPM given input RPM."""
        return input_rpm * GearAnalysis.velocity_ratio(gear)

    @staticmethod
    def pitch_line_velocity_m_s(gear: GearPair, driver_rpm: float) -> float:
        """Pitch line velocity V = pi * d * n / 60000 [m/s] (Shigley Eq.13-34)."""
        d_mm = gear.pitch_diameter_driver_mm
        return math.pi * d_mm * driver_rpm / 60000.0

    @staticmethod
    def contact_ratio(gear: GearPair) -> float:
        """Contact ratio CR (Shigley Eq.13-6).

        CR = (sqrt(r_a1^2 - r_b1^2) + sqrt(r_a2^2 - r_b2^2) - c*sin(phi))
             / (pi * m * cos(phi))

        where r_a = pitch_radius + addendum, r_b = pitch_radius * cos(phi),
        c = center distance, phi = pressure angle.
        """
        phi = math.radians(gear.pressure_angle_deg)
        cos_phi = math.cos(phi)
        sin_phi = math.sin(phi)

        r1 = gear.pitch_diameter_driver_mm / 2.0
        r2 = gear.pitch_diameter_driven_mm / 2.0
        a = gear.addendum_mm

        r_a1 = r1 + a
        r_a2 = r2 + a
        r_b1 = r1 * cos_phi
        r_b2 = r2 * cos_phi
        c = gear.center_distance_mm

        numerator = (
            math.sqrt(r_a1**2 - r_b1**2)
            + math.sqrt(r_a2**2 - r_b2**2)
            - c * sin_phi
        )
        denominator = math.pi * gear.module_mm * cos_phi
        return numerator / denominator

    @staticmethod
    def lewis_form_factor(teeth: int) -> float:
        """Lewis form factor Y for 20-degree full-depth teeth (Shigley Table 14-2).

        Approximation: Y = 0.484 - (2.87 / N) for N >= 12.
        """
        if teeth < 12:
            return 0.245
        return 0.484 - (2.87 / teeth)

    @staticmethod
    def lewis_bending_stress_MPa(
        gear: GearPair, tangential_force_N: float
    ) -> float:
        """Lewis bending stress sigma_b = W_t / (b * m * Y) [MPa] (Shigley Eq.14-3).

        W_t: tangential force [N]
        b: face width [mm]
        m: module [mm]
        Y: Lewis form factor
        """
        b = gear.face_width_mm
        m = gear.module_mm
        y = GearAnalysis.lewis_form_factor(gear.tooth_count_driver)
        return tangential_force_N / (b * m * y)

    @staticmethod
    def tangential_force_N(power_W: float, pitch_velocity_m_s: float) -> float:
        """Tangential force W_t = P / V [N] (Shigley Eq.13-35)."""
        if pitch_velocity_m_s <= 0:
            return 0.0
        return power_W / pitch_velocity_m_s


# ---------------------------------------------------------------------------
# Cam Analysis (Norton / Shigley Ch.3)
# ---------------------------------------------------------------------------

class CamAnalysis:
    """Modified trapezoidal cam profile analysis."""

    @staticmethod
    def displacement_mm(cam: CamFollower, theta_deg: float) -> float:
        """Modified trapezoidal displacement (Norton Ch.8).

        s(theta) = h * [theta/beta - (1/(2*pi)) * sin(2*pi*theta/beta)]

        during rise phase. Returns 0 during dwell, symmetric return during
        return phase.
        """
        h = cam.total_lift_mm
        rise = cam.rise_angle_deg
        dwell = cam.dwell_angle_deg
        ret = cam.return_angle_deg

        if theta_deg < 0:
            theta_deg = theta_deg % (rise + dwell + ret)

        if theta_deg <= rise:
            # Rise phase
            ratio = theta_deg / rise
            return h * (ratio - (1.0 / (2.0 * math.pi)) * math.sin(2.0 * math.pi * ratio))
        elif theta_deg <= rise + dwell:
            # Dwell at full lift
            return h
        elif theta_deg <= rise + dwell + ret:
            # Return phase (mirror of rise)
            theta_ret = theta_deg - rise - dwell
            ratio = theta_ret / ret
            return h * (1.0 - ratio + (1.0 / (2.0 * math.pi)) * math.sin(2.0 * math.pi * ratio))
        else:
            return 0.0

    @staticmethod
    def velocity_mm_per_deg(cam: CamFollower, theta_deg: float, d_theta: float = 0.01) -> float:
        """Numerical first derivative ds/d(theta) [mm/deg]."""
        s1 = CamAnalysis.displacement_mm(cam, theta_deg - d_theta)
        s2 = CamAnalysis.displacement_mm(cam, theta_deg + d_theta)
        return (s2 - s1) / (2.0 * d_theta)

    @staticmethod
    def acceleration_mm_per_deg2(cam: CamFollower, theta_deg: float, d_theta: float = 0.01) -> float:
        """Numerical second derivative d2s/d(theta)^2 [mm/deg^2]."""
        v1 = CamAnalysis.velocity_mm_per_deg(cam, theta_deg - d_theta, d_theta)
        v2 = CamAnalysis.velocity_mm_per_deg(cam, theta_deg + d_theta, d_theta)
        return (v2 - v1) / (2.0 * d_theta)

    @staticmethod
    def jerk_mm_per_deg3(cam: CamFollower, theta_deg: float, d_theta: float = 0.1) -> float:
        """Numerical third derivative d3s/d(theta)^3 [mm/deg^3]."""
        a1 = CamAnalysis.acceleration_mm_per_deg2(cam, theta_deg - d_theta, d_theta)
        a2 = CamAnalysis.acceleration_mm_per_deg2(cam, theta_deg + d_theta, d_theta)
        return (a2 - a1) / (2.0 * d_theta)

    @staticmethod
    def follower_force_N(
        cam: CamFollower, theta_deg: float, omega_rad_s: float
    ) -> float:
        """Total follower force: spring + inertia [N].

        F = k*s + m*a*(omega^2)
        where a is angular acceleration converted to linear.
        """
        s = CamAnalysis.displacement_mm(cam, theta_deg) / 1000.0  # m
        # Convert angular acceleration to linear
        accel_mm_deg2 = CamAnalysis.acceleration_mm_per_deg2(cam, theta_deg)
        # Convert deg^2 to rad^2: multiply by (pi/180)^2
        accel_m_rad2 = accel_mm_deg2 / 1000.0 * (math.pi / 180.0) ** 2

        f_spring = cam.spring_rate_N_mm * CamAnalysis.displacement_mm(cam, theta_deg)
        f_inertia = cam.follower_mass_kg * accel_m_rad2 * omega_rad_s**2
        return f_spring + f_inertia


# ---------------------------------------------------------------------------
# DOF Analysis (Grubler-Kutzbach)
# ---------------------------------------------------------------------------

class DOFAnalysis:
    """Grubler-Kutzbach mobility criterion for planar mechanisms.

    M = 3*(n-1) - 2*j1 - j2

    n = total links (including ground)
    j1 = full joints (1 DOF: revolute, prismatic)
    j2 = half joints (2 DOF: rolling contact, cam pair)
    """

    @staticmethod
    def grubler_kutzbach(n_links: int, j1_full: int, j2_half: int = 0) -> int:
        """Compute planar mobility M = 3*(n-1) - 2*j1 - j2."""
        return 3 * (n_links - 1) - 2 * j1_full - j2_half


# ---------------------------------------------------------------------------
# Main Shaft Model
# ---------------------------------------------------------------------------

class MainShaftModel:
    """Main shaft rotation model: moment of inertia, torque, angular velocity."""

    def __init__(
        self,
        diameter_mm: float,
        length_mm: float,
        density_kg_m3: float,
        rpm: float,
    ) -> None:
        self.diameter_mm = diameter_mm
        self.length_mm = length_mm
        self.density_kg_m3 = density_kg_m3
        self.rpm = rpm

    @property
    def radius_m(self) -> float:
        return self.diameter_mm / 2000.0

    @property
    def length_m(self) -> float:
        return self.length_mm / 1000.0

    @property
    def mass_kg(self) -> float:
        """Mass = rho * pi * r^2 * L."""
        return self.density_kg_m3 * math.pi * self.radius_m**2 * self.length_m

    @property
    def moment_of_inertia_kg_m2(self) -> float:
        """Solid cylinder: I = (1/2) * m * r^2."""
        return 0.5 * self.mass_kg * self.radius_m**2

    @property
    def angular_velocity_rad_s(self) -> float:
        """omega = 2*pi*n/60 [rad/s]."""
        return 2.0 * math.pi * self.rpm / 60.0

    @property
    def rotational_ke_J(self) -> float:
        """KE = (1/2) * I * omega^2 [J]."""
        return 0.5 * self.moment_of_inertia_kg_m2 * self.angular_velocity_rad_s**2

    def torque_at_power_Nm(self, power_W: float) -> float:
        """T = P / omega [N.m]."""
        if self.angular_velocity_rad_s <= 0:
            return 0.0
        return power_W / self.angular_velocity_rad_s


# ---------------------------------------------------------------------------
# Schema Loader
# ---------------------------------------------------------------------------

def load_kinematic_chain(schema_path: Optional[str] = None) -> KinematicChain:
    """Load kinematic chain parameters from sim_schema.yaml."""
    path = Path(schema_path) if schema_path else (
        Path(__file__).resolve().parents[3] / "docs" / "simulation" / "sim_schema.yaml"
    )
    with path.open("r", encoding="utf-8") as fh:
        data = yaml.safe_load(fh)

    chain = KinematicChain()

    mech = data.get("mechanisms", {})
    drive = mech.get("drive", {})
    chain.input_rpm = float(drive.get("rpm", 30))

    # Gear pairs
    gear_train = mech.get("gear_train", {})
    for stage in gear_train.get("stages", []):
        gp = GearPair(
            name=stage.get("name", "unnamed"),
            tooth_count_driver=int(stage.get("tooth_count_driver", 20)),
            tooth_count_driven=int(stage.get("tooth_count_driven", 60)),
            module_mm=float(stage.get("module_mm", 2.5)),
            pressure_angle_deg=float(stage.get("pressure_angle_deg", 20)),
            face_width_mm=float(stage.get("face_width_mm", 15)),
            material=stage.get("material", "brass"),
        )
        chain.gear_pairs.append(gp)

    # Cam profiles
    cams = mech.get("cam_profiles", {})
    for cam_name, cam_data in cams.items():
        cf = CamFollower(
            name=cam_name,
            rise_angle_deg=float(cam_data.get("rise_angle_deg", 60)),
            dwell_angle_deg=float(cam_data.get("dwell_angle_deg", 120)),
            return_angle_deg=float(cam_data.get("return_angle_deg", 60)),
            total_lift_mm=float(cam_data.get("total_lift_mm", 5)),
            follower_mass_kg=float(cam_data.get("follower_mass_kg", 0.2)),
            spring_rate_N_mm=float(cam_data.get("spring_rate_N_mm", 50)),
        )
        chain.cam_followers.append(cf)

    # DOF parameters -- Babbage AE kinematic chain
    # Links: ground(1) + main_shaft(1) + 2 gear pairs(4 gears) + 2 cam followers + connecting links
    # Simplified: n=12 links, j1=15 revolute joints, j2=2 cam half-joints
    chain.link_count = 12
    chain.joint_count_full = 15
    chain.joint_count_half = 2

    return chain
