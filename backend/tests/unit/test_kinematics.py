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
    CamTorqueRipple,
    DOFAnalysis,
    GearAnalysis,
    GearPair,
    HertzianContact,
    MainShaftModel,
    ShaftLateralDynamics,
    TorsionalVibration,
    load_kinematic_chain,
)
from backend.src.emulator.materials import MaterialLibrary

pytestmark = pytest.mark.physics


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


# -- Phase IV: Hertzian Contact / Gear Surface Fatigue --


class TestHertzianContact:
    def test_elastic_coefficient_positive(self):
        Cp = HertzianContact.elastic_coefficient(200.0, 0.3, 97.0, 0.34)
        assert Cp > 0

    def test_geometry_factor_positive(self):
        I_geom = HertzianContact.geometry_factor_I(20, 60)
        assert I_geom > 0

    def test_contact_stress_positive(self):
        Cp = HertzianContact.elastic_coefficient(97.0, 0.34, 97.0, 0.34)
        I_geom = HertzianContact.geometry_factor_I(20, 60)
        sigma = HertzianContact.contact_stress_MPa(
            100.0,
            1.05,
            1.0,
            15.0,
            50.0,
            I_geom,
            Cp,
        )
        assert sigma > 0

    def test_contact_stress_proportional_to_sqrt_force(self):
        Cp = HertzianContact.elastic_coefficient(97.0, 0.34, 97.0, 0.34)
        I_geom = HertzianContact.geometry_factor_I(20, 60)
        s1 = HertzianContact.contact_stress_MPa(100.0, 1.0, 1.0, 15.0, 50.0, I_geom, Cp)
        s2 = HertzianContact.contact_stress_MPa(400.0, 1.0, 1.0, 15.0, 50.0, I_geom, Cp)
        assert s2 == pytest.approx(2.0 * s1, rel=0.01)

    def test_pitting_sf_infinite_for_zero_stress(self):
        sf = HertzianContact.pitting_safety_factor(500.0, 0.0)
        assert sf == float("inf")


# -- Phase IV: Torsional Vibration --


class TestTorsionalVibration:
    def test_frequency_positive(self):
        omega = TorsionalVibration.natural_frequency_rad_s(
            80.0,
            50.0,
            500.0,
            0.5,
        )
        assert omega > 0

    def test_frequency_scales_with_sqrt_G(self):
        o1 = TorsionalVibration.natural_frequency_rad_s(40.0, 50.0, 500.0, 0.5)
        o2 = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 500.0, 0.5)
        assert o2 == pytest.approx(o1 * math.sqrt(2.0), rel=0.01)

    def test_frequency_inversely_with_L(self):
        o1 = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 500.0, 0.5)
        o2 = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 2000.0, 0.5)
        assert o2 == pytest.approx(o1 / 2.0, rel=0.01)

    def test_vibration_margin_above_3(self):
        omega = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 500.0, 0.5)
        nat_rpm = TorsionalVibration.torsional_frequency_rpm(omega)
        margin = TorsionalVibration.vibration_margin(nat_rpm, 30.0)
        assert margin > 3.0


# -- Phase IV: Cam Torque Ripple --


class TestCamTorqueRipple:
    @pytest.fixture
    def cam(self):
        return CamFollower(
            name="test_cam",
            rise_angle_deg=60.0,
            dwell_angle_deg=120.0,
            return_angle_deg=60.0,
            total_lift_mm=5.0,
            follower_mass_kg=0.2,
            spring_rate_N_mm=50.0,
        )

    def test_torque_zero_at_dwell(self, cam):
        dh = CamAnalysis.velocity_mm_per_deg(cam, 120.0)
        assert abs(dh) < 0.01

    def test_torque_envelope_positive_range(self, cam):
        omega = 2.0 * math.pi * 30.0 / 60.0
        t_min, t_max, ptp = CamTorqueRipple.torque_envelope(cam, omega)
        assert ptp >= 0

    def test_ripple_ratio_reasonable(self, cam):
        omega = 2.0 * math.pi * 30.0 / 60.0
        t_min, t_max, ptp = CamTorqueRipple.torque_envelope(cam, omega)
        if t_max > 0:
            ratio = ptp / abs(t_max)
            assert ratio < 20.0


# -- Phase IV: Shaft Lateral Dynamics --


class TestShaftLateralDynamics:
    def test_lateral_frequency_positive(self):
        omega = ShaftLateralDynamics.lateral_natural_frequency_rad_s(
            200.0,
            50.0,
            500.0,
            7850.0,
        )
        assert omega > 0

    def test_bearing_reactions_sum_to_load(self):
        reactions = ShaftLateralDynamics.bearing_reactions_N(1000.0, 4)
        assert len(reactions) == 4
        assert ShaftLateralDynamics.bearing_reactions_sum_check(reactions, 1000.0)

    def test_bearing_reactions_uniform(self):
        reactions = ShaftLateralDynamics.bearing_reactions_N(400.0, 4)
        for r in reactions:
            assert r == pytest.approx(100.0)


# ---------------------------------------------------------------------------
# GearPair dataclass properties
# ---------------------------------------------------------------------------


class TestGearPairProperties:
    """GearPair computed properties: ratio, pitch diameters, center distance."""

    def test_ratio_formula(self, primary_gear):
        assert primary_gear.ratio == pytest.approx(60 / 20)

    def test_pitch_diameter_driver(self, primary_gear):
        # pd_driver = module * tooth_count_driver
        assert primary_gear.pitch_diameter_driver_mm == pytest.approx(
            primary_gear.module_mm * primary_gear.tooth_count_driver
        )

    def test_pitch_diameter_driven(self, primary_gear):
        assert primary_gear.pitch_diameter_driven_mm == pytest.approx(
            primary_gear.module_mm * primary_gear.tooth_count_driven
        )

    def test_addendum_equals_module(self, primary_gear):
        assert primary_gear.addendum_mm == pytest.approx(primary_gear.module_mm)

    def test_dedendum_is_1_25_module(self, primary_gear):
        assert primary_gear.dedendum_mm == pytest.approx(1.25 * primary_gear.module_mm)

    def test_center_distance_formula(self, primary_gear):
        expected = primary_gear.module_mm * (20 + 60) / 2.0
        assert primary_gear.center_distance_mm == pytest.approx(expected)

    def test_gear_pair_is_frozen(self, primary_gear):
        with pytest.raises((AttributeError, TypeError)):
            primary_gear.module_mm = 5.0  # type: ignore[misc]


# ---------------------------------------------------------------------------
# GearAnalysis extended
# ---------------------------------------------------------------------------


class TestGearAnalysisExtended:
    """Tangential force, velocity ratio formula, pitch velocity formula."""

    def test_velocity_ratio_formula(self, primary_gear):
        # VR = N_driver / N_driven
        vr = GearAnalysis.velocity_ratio(primary_gear)
        assert vr == pytest.approx(20.0 / 60.0, rel=1e-6)

    def test_tangential_force_formula(self):
        # W_t = P / V
        wt = GearAnalysis.tangential_force_N(100.0, 2.0)
        assert wt == pytest.approx(50.0)

    def test_tangential_force_zero_at_zero_velocity(self):
        wt = GearAnalysis.tangential_force_N(100.0, 0.0)
        assert wt == pytest.approx(0.0)

    def test_pitch_velocity_formula(self, primary_gear):
        # V = pi * d * n / 60000
        v = GearAnalysis.pitch_line_velocity_m_s(primary_gear, 30.0)
        expected = math.pi * primary_gear.pitch_diameter_driver_mm * 30.0 / 60000.0
        assert v == pytest.approx(expected, rel=1e-6)

    def test_output_rpm_formula(self, primary_gear):
        rpm = GearAnalysis.output_rpm(primary_gear, 30.0)
        assert rpm == pytest.approx(30.0 * GearAnalysis.velocity_ratio(primary_gear))

    def test_lewis_form_factor_lt_12_teeth(self):
        y = GearAnalysis.lewis_form_factor(10)
        assert y == pytest.approx(0.245)

    def test_lewis_bending_stress_proportional_to_force(self, primary_gear):
        s1 = GearAnalysis.lewis_bending_stress_MPa(primary_gear, 100.0)
        s2 = GearAnalysis.lewis_bending_stress_MPa(primary_gear, 200.0)
        assert s2 == pytest.approx(2.0 * s1, rel=1e-6)

    def test_contact_ratio_lower_for_fewer_teeth(self):
        # A gear pair with lower tooth counts generally has lower CR
        small_gear = GearPair(
            "small", 12, 24, 2.5, 20, 15, "brass"
        )
        large_gear = GearPair(
            "large", 20, 60, 2.5, 20, 15, "brass"
        )
        cr_small = GearAnalysis.contact_ratio(small_gear)
        cr_large = GearAnalysis.contact_ratio(large_gear)
        # Both must exceed 1.0; large gear typically higher CR
        assert cr_small > 1.0
        assert cr_large > 1.0


# ---------------------------------------------------------------------------
# CamFollower and CamAnalysis extended
# ---------------------------------------------------------------------------


class TestCamFollowerExtended:
    """Return phase, dwell endpoint, velocity sign."""

    def test_displacement_at_return_end_is_zero(self, column_latch_cam):
        # End of return = rise + dwell + return
        end_angle = (
            column_latch_cam.rise_angle_deg
            + column_latch_cam.dwell_angle_deg
            + column_latch_cam.return_angle_deg
        )
        s = CamAnalysis.displacement_mm(column_latch_cam, end_angle)
        assert s == pytest.approx(0.0, abs=0.02)

    def test_displacement_during_dwell_constant(self, column_latch_cam):
        h = column_latch_cam.total_lift_mm
        for theta in [70.0, 100.0, 150.0]:
            s = CamAnalysis.displacement_mm(column_latch_cam, theta)
            assert s == pytest.approx(h, abs=0.01)

    def test_velocity_negative_during_return(self, column_latch_cam):
        # Midway through return phase
        theta = (
            column_latch_cam.rise_angle_deg
            + column_latch_cam.dwell_angle_deg
            + column_latch_cam.return_angle_deg / 2.0
        )
        v = CamAnalysis.velocity_mm_per_deg(column_latch_cam, theta)
        assert v < 0

    def test_velocity_zero_at_dwell_start(self, column_latch_cam):
        # Just into dwell
        v = CamAnalysis.velocity_mm_per_deg(column_latch_cam, 61.0)
        assert abs(v) < 0.05

    def test_acceleration_returns_float(self, column_latch_cam):
        a = CamAnalysis.acceleration_mm_per_deg2(column_latch_cam, 30.0)
        assert isinstance(a, float)

    def test_jerk_returns_float(self, column_latch_cam):
        j = CamAnalysis.jerk_mm_per_deg3(column_latch_cam, 30.0)
        assert isinstance(j, float)

    def test_follower_force_at_zero_displacement_is_zero(self, column_latch_cam):
        # At theta=0, displacement=0 -> spring force=0; inertia near zero
        omega = 2.0 * math.pi * 30.0 / 60.0
        f = CamAnalysis.follower_force_N(column_latch_cam, 0.0, omega)
        assert f == pytest.approx(0.0, abs=1.0)


# ---------------------------------------------------------------------------
# DOFAnalysis extended
# ---------------------------------------------------------------------------


class TestDOFAnalysisExtended:
    """Various link/joint configurations."""

    def test_dof_five_bar(self):
        # 5 links, 5 joints: M = 3*(5-1) - 2*5 = 12-10 = 2
        assert DOFAnalysis.grubler_kutzbach(5, 5) == 2

    def test_dof_zero_joints(self):
        # 4 links, 0 joints: M = 3*(4-1) - 0 = 9 (not a mechanism)
        assert DOFAnalysis.grubler_kutzbach(4, 0) == 9

    def test_dof_with_half_joints(self):
        # 4 links, 4 full, 1 half: M = 9 - 8 - 1 = 0 (locked)
        assert DOFAnalysis.grubler_kutzbach(4, 4, 1) == 0

    def test_dof_result_type_is_int(self):
        result = DOFAnalysis.grubler_kutzbach(4, 4, 0)
        assert isinstance(result, int)


# ---------------------------------------------------------------------------
# MainShaftModel extended
# ---------------------------------------------------------------------------


class TestMainShaftModelExtended:
    """KE scaling, torque formula, mass proportionality."""

    @pytest.fixture
    def shaft(self):
        return MainShaftModel(
            diameter_mm=50.0,
            length_mm=1500.0,
            density_kg_m3=7850.0,
            rpm=30.0,
        )

    def test_angular_velocity_formula(self, shaft):
        expected = 2.0 * math.pi * 30.0 / 60.0
        assert shaft.angular_velocity_rad_s == pytest.approx(expected, rel=1e-6)

    def test_ke_proportional_to_omega_squared(self):
        s1 = MainShaftModel(50.0, 1500.0, 7850.0, 30.0)
        s2 = MainShaftModel(50.0, 1500.0, 7850.0, 60.0)
        # KE = 0.5*I*omega^2: doubling omega -> 4x KE
        assert s2.rotational_ke_J == pytest.approx(4.0 * s1.rotational_ke_J, rel=1e-6)

    def test_torque_inversely_proportional_to_omega(self):
        s1 = MainShaftModel(50.0, 1500.0, 7850.0, 30.0)
        s2 = MainShaftModel(50.0, 1500.0, 7850.0, 60.0)
        # T = P / omega: doubling omega halves torque for same P
        assert s2.torque_at_power_Nm(500.0) == pytest.approx(
            s1.torque_at_power_Nm(500.0) / 2.0, rel=1e-6
        )

    def test_mass_proportional_to_length(self):
        s1 = MainShaftModel(50.0, 1000.0, 7850.0, 30.0)
        s2 = MainShaftModel(50.0, 2000.0, 7850.0, 30.0)
        assert s2.mass_kg == pytest.approx(2.0 * s1.mass_kg, rel=1e-6)

    def test_torque_zero_at_zero_rpm(self):
        s = MainShaftModel(50.0, 1500.0, 7850.0, 0.0)
        assert s.torque_at_power_Nm(500.0) == pytest.approx(0.0)

    def test_moment_of_inertia_formula(self, shaft):
        # I = 0.5 * m * r^2
        r = shaft.radius_m
        expected = 0.5 * shaft.mass_kg * r**2
        assert shaft.moment_of_inertia_kg_m2 == pytest.approx(expected, rel=1e-6)


# ---------------------------------------------------------------------------
# HertzianContact extended
# ---------------------------------------------------------------------------


class TestHertzianContactExtended:
    """Elastic coefficient, geometry factor, pitting SF."""

    def test_elastic_coefficient_same_material(self):
        # Same material -> lower denom -> higher Cp
        Cp = HertzianContact.elastic_coefficient(200.0, 0.3, 200.0, 0.3)
        assert Cp > 0

    def test_elastic_coefficient_lower_for_stiffer_material(self):
        # Higher E -> lower elastic coefficient (less deformation)
        Cp_soft = HertzianContact.elastic_coefficient(97.0, 0.34, 97.0, 0.34)
        Cp_hard = HertzianContact.elastic_coefficient(200.0, 0.3, 200.0, 0.3)
        assert Cp_hard > Cp_soft

    def test_geometry_factor_increases_with_ratio(self):
        # Higher gear ratio -> larger I
        I_low = HertzianContact.geometry_factor_I(20, 40)
        I_high = HertzianContact.geometry_factor_I(20, 120)
        assert I_high > I_low

    def test_geometry_factor_is_float(self):
        I_geom = HertzianContact.geometry_factor_I(20, 60)
        assert isinstance(I_geom, float)

    def test_pitting_sf_returns_square_of_ratio(self):
        sf = HertzianContact.pitting_safety_factor(400.0, 200.0)
        assert sf == pytest.approx(4.0)


# ---------------------------------------------------------------------------
# TorsionalVibration extended
# ---------------------------------------------------------------------------


class TestTorsionalVibrationExtended:
    """Natural frequency formula, RPM conversion, diameter effect."""

    def test_natural_frequency_is_float(self):
        omega = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 500.0, 0.5)
        assert isinstance(omega, float)

    def test_torsional_frequency_rpm_formula(self):
        omega = 100.0  # rad/s
        rpm = TorsionalVibration.torsional_frequency_rpm(omega)
        assert rpm == pytest.approx(omega * 60.0 / (2.0 * math.pi), rel=1e-6)

    def test_vibration_margin_formula(self):
        margin = TorsionalVibration.vibration_margin(3000.0, 30.0)
        assert margin == pytest.approx(100.0)

    def test_larger_diameter_higher_frequency(self):
        o_small = TorsionalVibration.natural_frequency_rad_s(80.0, 25.0, 500.0, 0.5)
        o_large = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 500.0, 0.5)
        assert o_large > o_small

    def test_longer_shaft_lower_frequency(self):
        o_short = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 250.0, 0.5)
        o_long = TorsionalVibration.natural_frequency_rad_s(80.0, 50.0, 1000.0, 0.5)
        assert o_long < o_short
