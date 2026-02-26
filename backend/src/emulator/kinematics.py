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

import yaml

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
    gear_pairs: list[GearPair] = field(default_factory=list)
    cam_followers: list[CamFollower] = field(default_factory=list)
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

def load_kinematic_chain(schema_path: str | None = None) -> KinematicChain:
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


# ---------------------------------------------------------------------------
# Gear Surface Fatigue / Hertzian Contact (Shigley Ch.14)
# ---------------------------------------------------------------------------

class HertzianContact:
    """Hertzian contact stress and AGMA pitting resistance."""

    @staticmethod
    def elastic_coefficient(
        E1_GPa: float, nu1: float,
        E2_GPa: float, nu2: float,
    ) -> float:
        """AGMA elastic coefficient C_p (Shigley Eq.14-14).

        C_p = sqrt(1 / (pi * ((1-nu1^2)/E1 + (1-nu2^2)/E2)))

        Units: E in MPa -> C_p in sqrt(MPa).
        """
        E1 = E1_GPa * 1000.0  # GPa to MPa
        E2 = E2_GPa * 1000.0
        denom = math.pi * ((1.0 - nu1**2) / E1 + (1.0 - nu2**2) / E2)
        if denom <= 0:
            return 0.0
        return math.sqrt(1.0 / denom)

    @staticmethod
    def contact_stress_MPa(
        tangential_force_N: float,
        Kv: float,
        Ko: float,
        face_width_mm: float,
        pitch_diameter_mm: float,
        geometry_factor_I: float,
        Cp: float,
    ) -> float:
        """AGMA contact (Hertzian) stress (Shigley Eq.14-16).

        sigma_H = C_p * sqrt(W_t * K_v * K_o / (b * d_p * I))

        W_t: tangential force [N]
        K_v: dynamic factor
        K_o: overload factor (typically 1.0 for uniform loading)
        b: face width [mm]
        d_p: pitch diameter [mm]
        I: geometry factor (typically 0.08-0.12 for spur gears)
        C_p: elastic coefficient [sqrt(MPa)]
        """
        if face_width_mm <= 0 or pitch_diameter_mm <= 0 or geometry_factor_I <= 0:
            return float("inf")
        product = tangential_force_N * Kv * Ko / (
            face_width_mm * pitch_diameter_mm * geometry_factor_I
        )
        if product < 0:
            return 0.0
        return Cp * math.sqrt(product)

    @staticmethod
    def geometry_factor_I(
        tooth_count_pinion: int,
        tooth_count_gear: int,
        pressure_angle_deg: float = 20.0,
    ) -> float:
        """AGMA geometry factor I for external spur gears (Shigley Eq.14-23).

        I = cos(phi)*sin(phi) / 2 * m_G/(m_G+1)

        where m_G = N_G/N_P (gear ratio).
        """
        phi = math.radians(pressure_angle_deg)
        if tooth_count_pinion <= 0:
            return 0.0
        m_G = tooth_count_gear / tooth_count_pinion
        return math.cos(phi) * math.sin(phi) / 2.0 * m_G / (m_G + 1.0)

    @staticmethod
    def pitting_safety_factor(
        allowable_contact_stress_MPa: float,
        actual_contact_stress_MPa: float,
    ) -> float:
        """AGMA pitting resistance SF: SF = (S_H / sigma_H)^2."""
        if actual_contact_stress_MPa <= 0:
            return float("inf")
        return (allowable_contact_stress_MPa / actual_contact_stress_MPa) ** 2


# ---------------------------------------------------------------------------
# Torsional Vibration (Shigley Ch.7)
# ---------------------------------------------------------------------------

class TorsionalVibration:
    """Torsional natural frequency and vibration margin."""

    @staticmethod
    def natural_frequency_rad_s(
        shear_modulus_GPa: float,
        shaft_diameter_mm: float,
        shaft_length_mm: float,
        disk_inertia_kg_m2: float,
    ) -> float:
        """Torsional natural frequency (Shigley Ch.7).

        omega_n = sqrt(G * J_p / (L * J_disk))

        G: shear modulus [Pa]
        J_p: polar moment of area [m^4] = pi*d^4/32
        L: shaft length [m]
        J_disk: disk moment of inertia [kg.m^2]
        """
        G_Pa = shear_modulus_GPa * 1e9
        d_m = shaft_diameter_mm / 1000.0
        L_m = shaft_length_mm / 1000.0
        J_p = math.pi * d_m**4 / 32.0

        if L_m <= 0 or disk_inertia_kg_m2 <= 0:
            return float("inf")
        return math.sqrt(G_Pa * J_p / (L_m * disk_inertia_kg_m2))

    @staticmethod
    def torsional_frequency_rpm(omega_n_rad_s: float) -> float:
        """Convert torsional frequency to RPM."""
        return omega_n_rad_s * 60.0 / (2.0 * math.pi)

    @staticmethod
    def vibration_margin(natural_rpm: float, operating_rpm: float) -> float:
        """Torsional vibration margin (must be > 3)."""
        if operating_rpm <= 0:
            return float("inf")
        return natural_rpm / operating_rpm


# ---------------------------------------------------------------------------
# Cam Torque Ripple
# ---------------------------------------------------------------------------

class CamTorqueRipple:
    """Instantaneous cam torque and ripple analysis."""

    @staticmethod
    def instantaneous_torque_Nm(
        follower_force_N: float,
        dh_dtheta_mm_deg: float,
        omega_rad_s: float,
    ) -> float:
        """Instantaneous cam torque (Shigley Ch.3).

        T = F_follower * (dh/dtheta) / (omega * 1000)

        dh/dtheta in mm/deg -> convert to m/rad.
        """
        if omega_rad_s <= 0:
            return 0.0
        # Convert mm/deg to m/rad: mm/deg * (1m/1000mm) * (180deg/pi rad)
        dh_dtheta_m_rad = dh_dtheta_mm_deg / 1000.0 * 180.0 / math.pi
        return follower_force_N * dh_dtheta_m_rad

    @staticmethod
    def torque_envelope(
        cam: CamFollower,
        omega_rad_s: float,
        resolution_deg: float = 1.0,
    ) -> tuple[float, float, float]:
        """Compute torque envelope over one cam cycle.

        Returns (min_torque_Nm, max_torque_Nm, peak_to_peak_Nm).
        """
        total_angle = cam.rise_angle_deg + cam.dwell_angle_deg + cam.return_angle_deg
        torques = []
        theta = 0.0
        while theta <= total_angle:
            F = CamAnalysis.follower_force_N(cam, theta, omega_rad_s)
            dh = CamAnalysis.velocity_mm_per_deg(cam, theta)
            T = CamTorqueRipple.instantaneous_torque_Nm(F, dh, omega_rad_s)
            torques.append(T)
            theta += resolution_deg

        if not torques:
            return (0.0, 0.0, 0.0)
        t_min = min(torques)
        t_max = max(torques)
        return (t_min, t_max, t_max - t_min)


# ---------------------------------------------------------------------------
# Shaft Lateral Dynamics
# ---------------------------------------------------------------------------

class ShaftLateralDynamics:
    """Shaft lateral natural frequency and bearing reaction forces."""

    @staticmethod
    def lateral_natural_frequency_rad_s(
        youngs_modulus_GPa: float,
        diameter_mm: float,
        span_mm: float,
        density_kg_m3: float,
    ) -> float:
        """Lateral natural frequency (= critical speed).

        Same as ShaftCriticalSpeed.first_critical_speed_rad_s but accessible
        from the kinematics module.
        """
        d_m = diameter_mm / 1000.0
        L_m = span_mm / 1000.0
        E_Pa = youngs_modulus_GPa * 1e9
        I = math.pi * d_m**4 / 64.0
        A = math.pi * d_m**2 / 4.0
        if L_m <= 0 or A <= 0 or density_kg_m3 <= 0:
            return float("inf")
        return (math.pi**2 / L_m**2) * math.sqrt(E_Pa * I / (density_kg_m3 * A))

    @staticmethod
    def bearing_reactions_N(
        total_load_N: float,
        bearing_count: int,
    ) -> list[float]:
        """Bearing reaction forces assuming uniform load distribution.

        Returns list of reaction forces [N] for each bearing.
        """
        if bearing_count <= 0:
            return []
        load_per = total_load_N / bearing_count
        return [load_per] * bearing_count

    @staticmethod
    def bearing_reactions_sum_check(
        reactions: list[float],
        total_load_N: float,
        tolerance: float = 1e-6,
    ) -> bool:
        """Verify reactions sum to total load (equilibrium check)."""
        return abs(sum(reactions) - total_load_N) < tolerance
