"""Tests for Kinematic Analysis Module.

Validates:
- Gear velocity ratios and contact ratios
- Lewis bending stress below yield
- Cam displacement, velocity, acceleration, jerk
- Grubler-Kutzbach DOF = 1
- Main shaft model
- Schema loading
"""

import math
import pytest

from backend.src.emulator.kinematics import (
    CamAnalysis,
    CamFollower,
    DOFAnalysis,
    GearAnalysis,
    GearPair,
    KinematicChain,
    MainShaftModel,
    load_kinematic_chain,
)
from backend.src.emulator.materials import MaterialLibrary


@pytest.fixture
def lib():
    return MaterialLibrary()


@pytest.fixture
def chain():
    return load_kinematic_chain()


@pytest.fixture
def primary_gear():
    return GearPair(
        name="primary",
        tooth_count_driver=20,
        tooth_count_driven=60,
        module_mm=2.5,
        pressure_angle_deg=20,
        face_width_mm=15,
        material="brass",
    )


@pytest.fixture
def secondary_gear():
    return GearPair(
        name="secondary",
        tooth_count_driver=30,
        tooth_count_driven=60,
        module_mm=2.5,
        pressure_angle_deg=20,
        face_width_mm=12,
        material="brass",
    )


@pytest.fixture
def column_latch_cam():
    return CamFollower(
        name="column_latch",
        rise_angle_deg=60,
        dwell_angle_deg=120,
        return_angle_deg=60,
        total_lift_mm=5.0,
        follower_mass_kg=0.2,
        spring_rate_N_mm=50,
    )


# -- Gear Velocity Ratio --

class TestGearVelocityRatio:
    def test_primary_ratio(self, primary_gear):
        vr = GearAnalysis.velocity_ratio(primary_gear)
        assert vr == pytest.approx(1.0 / 3.0)

    def test_secondary_ratio(self, secondary_gear):
        vr = GearAnalysis.velocity_ratio(secondary_gear)
        assert vr == pytest.approx(0.5)

    def test_output_rpm_primary(self, primary_gear):
        rpm_out = GearAnalysis.output_rpm(primary_gear, 30.0)
        assert rpm_out == pytest.approx(10.0)

    def test_output_rpm_secondary(self, secondary_gear):
        rpm_out = GearAnalysis.output_rpm(secondary_gear, 10.0)
        assert rpm_out == pytest.approx(5.0)

    def test_total_ratio(self, primary_gear, secondary_gear):
        r1 = GearAnalysis.velocity_ratio(primary_gear)
        r2 = GearAnalysis.velocity_ratio(secondary_gear)
        assert r1 * r2 == pytest.approx(1.0 / 6.0)


# -- Contact Ratio --

class TestContactRatio:
    def test_primary_cr_above_minimum(self, primary_gear):
        cr = GearAnalysis.contact_ratio(primary_gear)
        assert cr >= 1.2, f"Contact ratio {cr} below minimum 1.2"

    def test_secondary_cr_above_minimum(self, secondary_gear):
        cr = GearAnalysis.contact_ratio(secondary_gear)
        assert cr >= 1.2

    def test_cr_positive(self, primary_gear):
        cr = GearAnalysis.contact_ratio(primary_gear)
        assert cr > 0


# -- Pitch Line Velocity --

class TestPitchLineVelocity:
    def test_positive(self, primary_gear):
        v = GearAnalysis.pitch_line_velocity_m_s(primary_gear, 30.0)
        assert v > 0

    def test_proportional_to_rpm(self, primary_gear):
        v30 = GearAnalysis.pitch_line_velocity_m_s(primary_gear, 30.0)
        v60 = GearAnalysis.pitch_line_velocity_m_s(primary_gear, 60.0)
        assert v60 == pytest.approx(2.0 * v30)


# -- Lewis Bending Stress --

class TestLewisBendingStress:
    def test_lewis_form_factor_positive(self):
        for n in [12, 20, 30, 60, 100]:
            y = GearAnalysis.lewis_form_factor(n)
            assert y > 0, f"Y({n}) = {y}"

    def test_form_factor_increases_with_teeth(self):
        y20 = GearAnalysis.lewis_form_factor(20)
        y60 = GearAnalysis.lewis_form_factor(60)
        assert y60 > y20

    def test_bending_stress_below_yield(self, primary_gear, lib):
        brass = lib.get("brass")
        # Realistic transmitted power through gear mesh: ~50W (computation
        # load is a fraction of 500W total engine output; most power is
        # dissipated in friction and inertial loads, not through gear teeth).
        v = GearAnalysis.pitch_line_velocity_m_s(primary_gear, 30.0)
        wt = GearAnalysis.tangential_force_N(50.0, v)
        sigma = GearAnalysis.lewis_bending_stress_MPa(primary_gear, wt)
        sy_min = brass.yield_strength_MPa[0]
        assert sigma < sy_min, f"Bending stress {sigma} MPa >= yield {sy_min} MPa"

    def test_stress_positive_for_positive_force(self, primary_gear):
        sigma = GearAnalysis.lewis_bending_stress_MPa(primary_gear, 100.0)
        assert sigma > 0


# -- Cam Displacement --

class TestCamDisplacement:
    def test_zero_at_start(self, column_latch_cam):
        s = CamAnalysis.displacement_mm(column_latch_cam, 0.0)
        assert s == pytest.approx(0.0, abs=0.01)

    def test_full_lift_at_end_of_rise(self, column_latch_cam):
        s = CamAnalysis.displacement_mm(column_latch_cam, 60.0)
        assert s == pytest.approx(5.0, abs=0.01)

    def test_full_lift_during_dwell(self, column_latch_cam):
        s = CamAnalysis.displacement_mm(column_latch_cam, 120.0)
        assert s == pytest.approx(5.0, abs=0.01)

    def test_zero_after_return(self, column_latch_cam):
        s = CamAnalysis.displacement_mm(column_latch_cam, 240.0)
        assert s == pytest.approx(0.0, abs=0.01)

    def test_monotonic_rise(self, column_latch_cam):
        prev = 0.0
        for theta in range(1, 61):
            s = CamAnalysis.displacement_mm(column_latch_cam, float(theta))
            assert s >= prev - 0.001, f"Non-monotonic at {theta} deg"
            prev = s


# -- Cam Velocity/Acceleration/Jerk --

class TestCamDerivatives:
    def test_velocity_positive_during_rise(self, column_latch_cam):
        v = CamAnalysis.velocity_mm_per_deg(column_latch_cam, 30.0)
        assert v > 0

    def test_velocity_near_zero_at_dwell(self, column_latch_cam):
        v = CamAnalysis.velocity_mm_per_deg(column_latch_cam, 120.0)
        assert abs(v) < 0.1

    def test_acceleration_finite(self, column_latch_cam):
        for theta in [10, 30, 50]:
            a = CamAnalysis.acceleration_mm_per_deg2(column_latch_cam, float(theta))
            assert math.isfinite(a), f"Infinite acceleration at {theta} deg"

    def test_jerk_finite(self, column_latch_cam):
        for theta in [10, 30, 50]:
            j = CamAnalysis.jerk_mm_per_deg3(column_latch_cam, float(theta))
            assert math.isfinite(j), f"Infinite jerk at {theta} deg"


# -- Follower Force --

class TestFollowerForce:
    def test_force_positive_at_full_lift(self, column_latch_cam):
        omega = 2.0 * math.pi * 30 / 60.0  # 30 RPM
        f = CamAnalysis.follower_force_N(column_latch_cam, 60.0, omega)
        assert f > 0, "Force should be positive at full lift (spring preload)"


# -- DOF Analysis --

class TestDOFAnalysis:
    def test_simple_four_bar(self):
        # 4 links, 4 full joints: M = 3*(4-1) - 2*4 = 9-8 = 1
        assert DOFAnalysis.grubler_kutzbach(4, 4) == 1

    def test_babbage_engine_dof_1(self):
        # Babbage AE: 12 links, 15 full joints, 2 half joints
        # M = 3*(12-1) - 2*15 - 2 = 33 - 30 - 2 = 1
        m = DOFAnalysis.grubler_kutzbach(12, 15, 2)
        assert m == 1, f"DOF should be 1, got {m}"


# -- Main Shaft Model --

class TestMainShaftModel:
    @pytest.fixture
    def shaft(self):
        return MainShaftModel(
            diameter_mm=50.0,
            length_mm=1500.0,
            density_kg_m3=7850.0,
            rpm=30.0,
        )

    def test_mass_positive(self, shaft):
        assert shaft.mass_kg > 0

    def test_mass_reasonable(self, shaft):
        # Steel shaft 50mm dia x 1.5m should be ~23 kg
        assert 15 < shaft.mass_kg < 35

    def test_moi_positive(self, shaft):
        assert shaft.moment_of_inertia_kg_m2 > 0

    def test_angular_velocity(self, shaft):
        expected = 2.0 * math.pi * 30 / 60.0
        assert shaft.angular_velocity_rad_s == pytest.approx(expected)

    def test_ke_positive(self, shaft):
        assert shaft.rotational_ke_J > 0

    def test_torque_at_power(self, shaft):
        t = shaft.torque_at_power_Nm(500.0)
        assert t > 0
        expected = 500.0 / shaft.angular_velocity_rad_s
        assert t == pytest.approx(expected)


# -- Schema Loading --

class TestSchemaLoading:
    def test_chain_loads(self, chain):
        assert chain is not None

    def test_has_gear_pairs(self, chain):
        assert len(chain.gear_pairs) == 2

    def test_has_cam_followers(self, chain):
        assert len(chain.cam_followers) >= 1

    def test_input_rpm(self, chain):
        assert chain.input_rpm == 30.0

    def test_dof_from_schema(self, chain):
        m = DOFAnalysis.grubler_kutzbach(
            chain.link_count, chain.joint_count_full, chain.joint_count_half
        )
        assert m == 1


# -- Integration: Full Kinematic Chain --

class TestKinematicChainIntegration:
    def test_gear_train_reduces_rpm(self, chain):
        rpm = chain.input_rpm
        for gp in chain.gear_pairs:
            rpm = GearAnalysis.output_rpm(gp, rpm)
        assert rpm < chain.input_rpm

    def test_final_rpm(self, chain):
        rpm = chain.input_rpm
        for gp in chain.gear_pairs:
            rpm = GearAnalysis.output_rpm(gp, rpm)
        assert rpm == pytest.approx(5.0)

    def test_all_contact_ratios_valid(self, chain):
        for gp in chain.gear_pairs:
            cr = GearAnalysis.contact_ratio(gp)
            assert cr >= 1.2, f"{gp.name}: CR={cr} below 1.2"

    def test_all_lewis_stress_below_yield(self, chain, lib):
        brass = lib.get("brass")
        rpm = chain.input_rpm
        # Realistic transmitted power per gear stage: ~50W
        for gp in chain.gear_pairs:
            v = GearAnalysis.pitch_line_velocity_m_s(gp, rpm)
            wt = GearAnalysis.tangential_force_N(50.0, v)
            sigma = GearAnalysis.lewis_bending_stress_MPa(gp, wt)
            assert sigma < brass.yield_strength_MPa[0], (
                f"{gp.name}: Lewis stress {sigma:.1f} MPa >= yield"
            )
            rpm = GearAnalysis.output_rpm(gp, rpm)
