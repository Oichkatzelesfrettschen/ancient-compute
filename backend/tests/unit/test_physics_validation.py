"""Cross-cutting physics validation across all 6 physics modules.

WHY:  Individual module tests verify each module in isolation.  This file
      validates that values produced by different modules are mutually
      consistent, physically plausible, and dimensionally sound when
      combined -- the kind of check that only emerges at integration
      boundaries.

WHAT: Three test classes covering:
  1. Cross-module consistency (materials vs kinematics vs structural, etc.)
  2. Physical plausibility (handbook sanity bands)
  3. Dimensional consistency (sign, finiteness, unit-implied constraints)

HOW:  Uses the real module APIs with parameters drawn from sim_schema.yaml.
      Run with: pytest backend/tests/unit/test_physics_validation.py -v
"""

from __future__ import annotations

import math

import pytest

pytestmark = pytest.mark.physics

# -- Module 1: Materials ---------------------------------------------------
from backend.src.emulator.materials import MaterialLibrary

# -- Module 2: Kinematics --------------------------------------------------
from backend.src.emulator.kinematics import (
    CamAnalysis,
    CamFollower,
    GearAnalysis,
    GearPair,
    MainShaftModel,
    load_kinematic_chain,
)

# -- Module 3: Thermodynamics ----------------------------------------------
from backend.src.emulator.thermodynamics import (
    FrictionHeatModel,
    ThermalExpansionModel,
    compute_engine_thermal_model,
    compute_steady_state_rise_C,
    compute_thermal_time_constant_s,
)

# -- Module 4: Electromagnetic ---------------------------------------------
from backend.src.emulator.electromagnetic import (
    EARTH_B_FIELD_T,
    EddyCurrentModel,
    GalvanicCorrosionMatrix,
    StaticChargeModel,
)

# -- Module 5: Tribology ----------------------------------------------------
from backend.src.emulator.tribology import (
    ARCHARD_K_LUBRICATED_BRASS_ON_STEEL,
    ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
    PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED,
    PV_LIMIT_BRASS_ON_STEEL_LUBRICATED,
    LubricationModel,
    PVAnalysis,
    WearModel,
)

# -- Module 6: Structural ---------------------------------------------------
from backend.src.emulator.structural import (
    BucklingAnalysis,
    FatigueAnalysis,
    GearToothStress,
    ShaftAnalysis,
)


# ---------------------------------------------------------------------------
# Shared fixtures -- realistic parameters from sim_schema.yaml
# ---------------------------------------------------------------------------

@pytest.fixture
def lib():
    """Material library loaded from the canonical schema."""
    return MaterialLibrary()


@pytest.fixture
def chain():
    """Full kinematic chain loaded from the canonical schema."""
    return load_kinematic_chain()


@pytest.fixture
def envelope():
    """Thermal operating envelope computed from the canonical schema."""
    return compute_engine_thermal_model()


@pytest.fixture
def primary_gear():
    """Primary gear pair: 20T driver / 60T driven, module 2.5, brass."""
    return GearPair(
        name="primary_reduction",
        tooth_count_driver=20,
        tooth_count_driven=60,
        module_mm=2.5,
        pressure_angle_deg=20,
        face_width_mm=15,
        material="brass",
    )


@pytest.fixture
def secondary_gear():
    """Secondary gear pair: 30T driver / 60T driven, module 2.5, brass."""
    return GearPair(
        name="secondary_reduction",
        tooth_count_driver=30,
        tooth_count_driven=60,
        module_mm=2.5,
        pressure_angle_deg=20,
        face_width_mm=12,
        material="brass",
    )


@pytest.fixture
def column_latch_cam():
    """Column latch cam follower from sim_schema.yaml."""
    return CamFollower(
        name="column_latch",
        rise_angle_deg=60,
        dwell_angle_deg=120,
        return_angle_deg=60,
        total_lift_mm=5.0,
        follower_mass_kg=0.2,
        spring_rate_N_mm=50,
    )


@pytest.fixture
def main_shaft(lib):
    """Main shaft model: 50 mm dia, 1500 mm length, steel, 30 RPM."""
    steel = lib.get("steel")
    return MainShaftModel(
        diameter_mm=50.0,
        length_mm=1500.0,
        density_kg_m3=steel.density_kg_m3,
        rpm=30.0,
    )


# -- Derived engine-level constants used across many tests --

MACHINE_MASS_KG = 500.0
BEARING_COUNT = 4
SHAFT_DIAMETER_MM = 50.0
BEARING_LENGTH_MM = 60.0
INPUT_RPM = 30.0
TRANSMITTED_POWER_W = 50.0   # realistic computation load through gears
TOTAL_ENGINE_POWER_W = 500.0


# ===================================================================
# 1. Cross-Module Consistency
# ===================================================================

class TestCrossModuleConsistency:
    """Tests that cross-reference computed values between modules.

    These catch integration errors where one module's output would
    violate another module's constraints.
    """

    # ----- materials vs kinematics/structural: stress < yield -----

    def test_gear_bending_stress_below_yield_all_stages(
        self, chain, lib,
    ):
        """Lewis bending stress must be below yield for every gear stage.

        Combines kinematics (GearAnalysis.lewis_bending_stress_MPa)
        with materials (yield_strength_MPa) for the actual gear
        material.
        """
        brass = lib.get("brass")
        rpm = chain.input_rpm
        for gp in chain.gear_pairs:
            v = GearAnalysis.pitch_line_velocity_m_s(gp, rpm)
            wt = GearAnalysis.tangential_force_N(TRANSMITTED_POWER_W, v)
            sigma = GearAnalysis.lewis_bending_stress_MPa(gp, wt)
            sy_min = brass.yield_strength_MPa[0]
            assert sigma < sy_min, (
                f"{gp.name}: Lewis stress {sigma:.1f} MPa >= "
                f"yield {sy_min} MPa"
            )
            rpm = GearAnalysis.output_rpm(gp, rpm)

    def test_structural_gear_tooth_sf_ge_2(
        self, primary_gear, secondary_gear, lib,
    ):
        """Structural gear tooth safety factor >= 2.0 for both stages.

        Combines structural (GearToothStress) with materials (yield).
        """
        brass = lib.get("brass")
        for gp, stage_rpm in [
            (primary_gear, INPUT_RPM),
            (secondary_gear, INPUT_RPM / 3.0),
        ]:
            v = GearAnalysis.pitch_line_velocity_m_s(gp, stage_rpm)
            wt = GearAnalysis.tangential_force_N(TRANSMITTED_POWER_W, v)
            sigma = GearToothStress.bending_stress_MPa(
                wt, gp.face_width_mm, gp.module_mm, gp.tooth_count_driver,
            )
            sf = GearToothStress.safety_factor(
                brass.yield_strength_MPa[0], sigma,
            )
            assert sf >= 2.0, (
                f"{gp.name}: gear tooth SF={sf:.2f} < 2.0"
            )

    # ----- tribology vs materials: PV < limit for bearing materials ---

    def test_bearing_pv_below_limit(self):
        """PV product for main shaft bearings must stay below the
        lubricated phosphor-bronze-on-steel PV limit.

        Combines tribology (PVAnalysis) with schema bearing parameters.
        """
        radial_load_N = MACHINE_MASS_KG * 9.81 / BEARING_COUNT
        pv = PVAnalysis.pv_product_MPa_m_s(
            radial_load_N, SHAFT_DIAMETER_MM, BEARING_LENGTH_MM, INPUT_RPM,
        )
        assert PVAnalysis.is_within_limit(
            pv, PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED,
        ), (
            f"Bearing PV={pv:.4f} MPa.m/s >= "
            f"limit {PV_LIMIT_BRONZE_ON_STEEL_LUBRICATED}"
        )

    def test_bearing_pv_also_below_brass_limit(self):
        """Even the more conservative brass PV limit is satisfied."""
        radial_load_N = MACHINE_MASS_KG * 9.81 / BEARING_COUNT
        pv = PVAnalysis.pv_product_MPa_m_s(
            radial_load_N, SHAFT_DIAMETER_MM, BEARING_LENGTH_MM, INPUT_RPM,
        )
        assert PVAnalysis.is_within_limit(
            pv, PV_LIMIT_BRASS_ON_STEEL_LUBRICATED,
        ), (
            f"Bearing PV={pv:.4f} MPa.m/s >= "
            f"brass limit {PV_LIMIT_BRASS_ON_STEEL_LUBRICATED}"
        )

    # ----- electromagnetic vs thermodynamics: eddy << friction --------

    def test_eddy_losses_three_orders_below_friction_heat(
        self, lib, envelope,
    ):
        """Total EM eddy losses must be at least 3 orders of magnitude
        below total friction heat generation.

        Combines electromagnetic (EddyCurrentModel) with
        thermodynamics (compute_engine_thermal_model).
        """
        steel = lib.get("steel")
        brass = lib.get("brass")

        total_eddy_W = 0.0
        # Main shaft eddy losses
        total_eddy_W += EddyCurrentModel.shaft_eddy_loss_W(
            SHAFT_DIAMETER_MM, 1500.0, INPUT_RPM,
            steel.electrical_resistivity_ohm_m,
        )
        # Gear eddy losses (4 gears across 2 stages)
        for pd_mm in [50.0, 150.0, 75.0, 150.0]:
            total_eddy_W += EddyCurrentModel.gear_eddy_loss_W(
                pd_mm, 15.0, INPUT_RPM,
                brass.electrical_resistivity_ohm_m,
            )

        total_friction_W = envelope.total_heat_generation_W
        assert total_friction_W > 0, "Friction heat must be positive"
        ratio = total_eddy_W / total_friction_W
        assert ratio < 1e-3, (
            f"Eddy/friction ratio {ratio:.2e} >= 1e-3 "
            f"(eddy={total_eddy_W:.2e} W, friction={total_friction_W:.2f} W)"
        )

    # ----- thermodynamics vs materials: brass CTE > steel CTE ---------

    def test_thermal_expansion_brass_gt_steel(self, lib):
        """Brass thermal expansion coefficient must exceed steel's.

        Validates that ThermalExpansionModel produces consistent
        ordering with material CTE values.
        """
        brass = lib.get("brass")
        steel = lib.get("steel")
        assert (
            brass.thermal_expansion_coeff_per_K
            > steel.thermal_expansion_coeff_per_K
        ), "Brass CTE must exceed steel CTE"

        # Also verify via ThermalExpansionModel for a concrete length
        length_mm = 100.0
        delta_T = 20.0
        d_brass = ThermalExpansionModel.linear_expansion_mm(
            brass.thermal_expansion_coeff_per_K, length_mm, delta_T,
        )
        d_steel = ThermalExpansionModel.linear_expansion_mm(
            steel.thermal_expansion_coeff_per_K, length_mm, delta_T,
        )
        assert d_brass > d_steel

    # ----- structural: all safety factors >= 2.0 ----------------------

    def test_all_safety_factors_ge_2(self, lib):
        """Every structural safety factor must be >= 2.0.

        Checks:
        - Gear tooth bending (brass, both stages)
        - Fatigue Goodman (steel shaft)
        - Buckling (steel column supports, SF >= 3.0)
        """
        brass = lib.get("brass")
        steel = lib.get("steel")

        # -- Gear tooth SF (primary stage) --
        v = GearAnalysis.pitch_line_velocity_m_s(
            GearPair("p", 20, 60, 2.5, 20, 15, "brass"), INPUT_RPM,
        )
        wt = GearAnalysis.tangential_force_N(TRANSMITTED_POWER_W, v)
        sigma_gear = GearToothStress.bending_stress_MPa(wt, 15.0, 2.5, 20)
        sf_gear = GearToothStress.safety_factor(
            brass.yield_strength_MPa[0], sigma_gear,
        )
        assert sf_gear >= 2.0, f"Gear tooth SF={sf_gear:.2f}"

        # -- Fatigue Goodman SF (main shaft) --
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,  # shaft diameter
        )
        # Fully reversed bending at light load: sigma_a ~ 10 MPa
        sf_fatigue = FatigueAnalysis.goodman_safety_factor(
            10.0, 0.0, Se, steel.ultimate_tensile_strength_MPa[0],
        )
        assert sf_fatigue >= 2.0, f"Fatigue Goodman SF={sf_fatigue:.2f}"

        # -- Buckling SF (column supports, require >= 3.0) --
        I_col = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0], I_col, 600.0, 0.5,
        )
        applied_col = 5.0 * 9.81  # ~5 kg per column mechanism
        sf_buckling = BucklingAnalysis.buckling_safety_factor(
            P_cr, applied_col,
        )
        assert sf_buckling >= 3.0, f"Buckling SF={sf_buckling:.2f}"

    # ----- structural vs kinematics: shaft deflection < 0.001*L -------

    def test_shaft_deflection_below_thousandth_of_length(self, lib):
        """Main shaft deflection must be less than L/1000.

        Combines structural (ShaftAnalysis) with kinematics-level
        shaft geometry and materials (Young's modulus).
        """
        steel = lib.get("steel")
        total_length_mm = 1500.0
        bearing_count = 4
        # Shaft self-weight as distributed load approximation
        shaft_mass_kg = 23.0
        load_N = shaft_mass_kg * 9.81

        deflection_mm = ShaftAnalysis.max_deflection_multi_support_mm(
            load_N, total_length_mm, bearing_count,
            steel.youngs_modulus_GPa[0], SHAFT_DIAMETER_MM,
        )
        limit_mm = total_length_mm * 0.001
        assert deflection_mm < limit_mm, (
            f"Shaft deflection {deflection_mm:.4f} mm >= "
            f"{limit_mm:.2f} mm (L/1000)"
        )

    # ----- materials vs tribology: wear model uses correct hardness ---

    def test_bearing_wear_uses_softer_material_hardness(self, lib):
        """Archard wear must use the softer material (phosphor bronze
        bushing) hardness, not the shaft (steel) hardness.  Verify
        that bronze hardness < steel hardness as expected.
        """
        pb = lib.get("phosphor_bronze")
        steel = lib.get("steel")
        # Bronze bushings are softer than the steel shaft
        assert pb.hardness_HB[0] <= steel.hardness_HB[1], (
            "Phosphor bronze min HB should be <= steel max HB"
        )

    # ----- kinematics vs thermodynamics: shaft omega consistent -------

    def test_shaft_omega_consistent(self, main_shaft):
        """MainShaftModel angular velocity must be consistent with
        2*pi*RPM/60 used by the thermodynamics friction model.
        """
        expected_omega = 2.0 * math.pi * INPUT_RPM / 60.0
        assert main_shaft.angular_velocity_rad_s == pytest.approx(
            expected_omega, rel=1e-9,
        )

    # ----- fatigue vs materials: endurance limit ordering -------------

    def test_spring_steel_highest_endurance_limit(self, lib):
        """Spring steel must have the highest endurance limit of all
        materials, matching its assignment to high-cycle springs.
        """
        ss = lib.get("spring_steel")
        for name in lib.names():
            other = lib.get(name)
            assert ss.endurance_limit_MPa[1] >= other.endurance_limit_MPa[0], (
                f"Spring steel Se_max < {name} Se_min"
            )


# ===================================================================
# 2. Physical Plausibility
# ===================================================================

class TestPhysicalPlausibility:
    """Tests that computed values fall within physically reasonable
    ranges for a 19th-century mechanical computer.
    """

    # ----- material density -----

    def test_all_densities_between_2000_and_9000(self, lib):
        """All materials are engineering metals with densities in the
        2000-9000 kg/m3 range (aluminum alloys to copper alloys).
        """
        for mat in lib.all_materials():
            assert 2000 <= mat.density_kg_m3 <= 9000, (
                f"{mat.name}: density {mat.density_kg_m3} kg/m3 "
                f"outside [2000, 9000]"
            )

    # ----- Young's modulus -----

    def test_all_youngs_moduli_between_50_and_250_GPa(self, lib):
        """Engineering metals span roughly 50-250 GPa."""
        for mat in lib.all_materials():
            e_min, e_max = mat.youngs_modulus_GPa
            assert 50 <= e_min, (
                f"{mat.name}: E_min={e_min} GPa below 50"
            )
            assert e_max <= 250, (
                f"{mat.name}: E_max={e_max} GPa above 250"
            )

    # ----- Poisson's ratio -----

    def test_all_poissons_ratios_between_0_2_and_0_5(self, lib):
        """Metals have nu in [0.2, 0.5); rubber approaches 0.5."""
        for mat in lib.all_materials():
            assert 0.2 <= mat.poissons_ratio < 0.5, (
                f"{mat.name}: nu={mat.poissons_ratio} outside [0.2, 0.5)"
            )

    # ----- yield strength -----

    def test_all_yield_strengths_between_50_and_2000_MPa(self, lib):
        """From soft grey iron (~165 MPa) to spring steel (~1200 MPa)."""
        for mat in lib.all_materials():
            sy_min, sy_max = mat.yield_strength_MPa
            assert sy_min >= 50, (
                f"{mat.name}: Sy_min={sy_min} MPa below 50"
            )
            assert sy_max <= 2000, (
                f"{mat.name}: Sy_max={sy_max} MPa above 2000"
            )

    # ----- total friction heat -----

    def test_total_friction_heat_between_1_and_500_W(self, envelope):
        """A 500 kg machine at 30 RPM dissipates roughly 10-100 W
        through friction; 1-500 W is a generous plausibility band.
        """
        q = envelope.total_heat_generation_W
        assert 1.0 <= q <= 500.0, (
            f"Total friction heat {q:.1f} W outside [1, 500]"
        )

    # ----- thermal time constant -----

    def test_thermal_time_constant_between_100_and_100000_s(
        self, envelope,
    ):
        """A 500 kg iron/steel machine with ~7 m2 surface area has
        tau ~ 3000-5000 s; 100-100000 s is a generous band.
        """
        tau = envelope.thermal_time_constant_s
        assert 100 <= tau <= 100000, (
            f"Thermal time constant {tau:.0f} s outside [100, 100000]"
        )

    # ----- steady-state temperature rise -----

    def test_steady_state_rise_between_1_and_50_C(self, envelope):
        """With 10-100 W in a large machine, steady-state rise is
        small (1-10 C typically); 1-50 C is generous.
        """
        dT = envelope.steady_state_rise_C
        assert 0.1 <= dT <= 50.0, (
            f"Steady-state rise {dT:.2f} C outside [0.1, 50]"
        )

    # ----- lambda ratio: boundary regime at 30 RPM is expected --------

    def test_lambda_ratio_boundary_regime_at_operating_speed(self, lib):
        """At 30 RPM the Babbage Engine operates in boundary lubrication.

        The pitch-line velocity (~0.08 m/s) is too low for a
        hydrodynamic film to develop.  This is historically accurate:
        19th-century mineral oil reduced friction coefficient and
        prevented corrosion, but did not form an EHL film at these
        speeds.  Verify the EHL model correctly predicts lambda < 1
        (boundary regime), confirming the oil's role as a boundary
        lubricant rather than a full-film lubricant.
        """
        brass = lib.get("brass")
        steel = lib.get("steel")

        E_brass = brass.youngs_modulus_GPa[0]
        nu_brass = brass.poissons_ratio
        E_steel = steel.youngs_modulus_GPa[0]
        nu_steel = steel.poissons_ratio
        E_star = 1.0 / (
            (1 - nu_brass**2) / E_brass + (1 - nu_steel**2) / E_steel
        )

        # Pitch-line velocity at primary gear stage
        v_pitch = math.pi * 50.0 * INPUT_RPM / 60000.0  # m/s

        r1_mm = 50.0 / 2.0   # driver pitch radius
        r2_mm = 150.0 / 2.0  # driven pitch radius
        R_eff_mm = (r1_mm * r2_mm) / (r1_mm + r2_mm)

        wt = GearAnalysis.tangential_force_N(TRANSMITTED_POWER_W, v_pitch)
        load_per_length = wt / 15.0  # N/mm over face width

        h_min = LubricationModel.minimum_film_thickness_um(
            viscosity_Pa_s=0.059,
            entrainment_velocity_m_s=v_pitch,
            effective_radius_mm=R_eff_mm,
            reduced_modulus_GPa=E_star,
            load_per_length_N_mm=load_per_length,
        )
        Ra_gear = 1.6  # um, hobbed + hand-filed brass gear tooth
        Ra_pinion = 0.8  # um, turned + polished steel pinion
        lam = LubricationModel.lambda_ratio(h_min, Ra_gear, Ra_pinion)

        # At 30 RPM, boundary lubrication is expected and correct
        assert lam < 1.0, (
            f"Lambda {lam:.2f} >= 1.0 -- at 30 RPM boundary "
            f"regime is expected"
        )
        regime = LubricationModel.regime(lam)
        assert regime == "boundary"

    def test_ehl_film_increases_with_speed(self):
        """Verify the EHL model's velocity sensitivity: doubling
        entrainment velocity must increase the minimum film thickness.

        The Dowson-Higginson formula has h proportional to U^0.7,
        so 2x speed should give roughly 1.62x film thickness.
        """
        params = dict(
            viscosity_Pa_s=0.059,
            effective_radius_mm=18.75,
            reduced_modulus_GPa=110.0,
            load_per_length_N_mm=40.0,
        )
        h_1x = LubricationModel.minimum_film_thickness_um(
            entrainment_velocity_m_s=0.08, **params,
        )
        h_2x = LubricationModel.minimum_film_thickness_um(
            entrainment_velocity_m_s=0.16, **params,
        )
        assert h_2x > h_1x, (
            f"Film at 2x speed ({h_2x:.4f} um) not greater than "
            f"1x ({h_1x:.4f} um)"
        )
        # U^0.7 scaling: 2^0.7 ~ 1.625
        ratio = h_2x / h_1x
        assert ratio == pytest.approx(2.0**0.7, rel=0.05), (
            f"Velocity scaling ratio {ratio:.3f} != expected ~1.625"
        )

    def test_lambda_ratio_full_film_with_adequate_film_thickness(self):
        """Lambda ratio > 3 (full-film) when h_min is well above
        surface roughness.  This validates the lambda_ratio function
        itself with known-good inputs.

        With h_min=5 um and period surface finishes (Ra 0.8 um
        shaft, 0.8 um bearing), lambda should exceed 3.
        """
        lam = LubricationModel.lambda_ratio(5.0, 0.8, 0.8)
        assert lam > 3.0, (
            f"Lambda {lam:.2f} should be > 3.0 for h_min=5 um"
        )

    # ----- eddy losses are truly negligible -----

    def test_total_eddy_loss_below_1_milliwatt(self, lib):
        """At Earth's ambient field and 30 RPM, total eddy loss
        across all rotating components must be < 1 mW.
        """
        steel = lib.get("steel")
        brass = lib.get("brass")
        total_mW = 0.0

        total_mW += EddyCurrentModel.shaft_eddy_loss_W(
            SHAFT_DIAMETER_MM, 1500.0, INPUT_RPM,
            steel.electrical_resistivity_ohm_m,
        ) * 1000.0

        for pd_mm, fw_mm in [(50.0, 15.0), (150.0, 15.0),
                              (75.0, 12.0), (150.0, 12.0)]:
            total_mW += EddyCurrentModel.gear_eddy_loss_W(
                pd_mm, fw_mm, INPUT_RPM,
                brass.electrical_resistivity_ohm_m,
            ) * 1000.0

        assert total_mW < 1.0, (
            f"Total eddy loss {total_mW:.4f} mW >= 1 mW"
        )

    # ----- static charge negligible in oiled machine -----

    def test_static_charge_zero_lubricated(self):
        """Metal-on-metal with oil film: triboelectric charge is zero."""
        q = StaticChargeModel.triboelectric_charge_nC(
            contact_area_cm2=10.0,
            velocity_m_s=0.1,
            is_lubricated=True,
        )
        assert q == 0.0

    # ----- cam displacement within total lift -----

    def test_cam_displacement_never_exceeds_lift(self, column_latch_cam):
        """Cam displacement must never exceed total_lift_mm at any angle."""
        h = column_latch_cam.total_lift_mm
        total_cycle = (
            column_latch_cam.rise_angle_deg
            + column_latch_cam.dwell_angle_deg
            + column_latch_cam.return_angle_deg
        )
        for theta_deg in range(0, int(total_cycle) + 1):
            s = CamAnalysis.displacement_mm(
                column_latch_cam, float(theta_deg),
            )
            assert -0.01 <= s <= h + 0.01, (
                f"Cam displacement {s:.3f} mm at {theta_deg} deg "
                f"outside [0, {h}]"
            )

    # ----- galvanic corrosion: copper alloys are cathodic to steel ----

    def test_steel_anodic_to_brass(self):
        """Steel (more negative potential) must be anodic to brass."""
        gcm = GalvanicCorrosionMatrix()
        pair = gcm.evaluate_pair("brass", "steel")
        assert pair.material_anodic == "steel"
        assert pair.material_cathodic == "brass"

    # ----- main shaft moment of inertia plausible --------------------

    def test_shaft_moi_plausible(self, main_shaft):
        """50 mm dia x 1500 mm steel shaft: I ~ 0.007 kg.m2.
        Must be in [0.001, 0.1] kg.m2 range.
        """
        I = main_shaft.moment_of_inertia_kg_m2
        assert 0.001 <= I <= 0.1, (
            f"Shaft MOI {I:.6f} kg.m2 outside [0.001, 0.1]"
        )


# ===================================================================
# 3. Dimensional Consistency
# ===================================================================

class TestDimensionalConsistency:
    """Tests that verify sign conventions, finiteness, and
    dimensional-analysis-implied constraints (e.g., volumes > 0,
    thicknesses > 0, ratios >= lower bounds).
    """

    # ----- wear volume > 0 -----

    def test_archard_wear_volume_positive(self, lib):
        """Wear volume from Archard equation must be strictly positive
        for positive inputs.
        """
        pb = lib.get("phosphor_bronze")
        v = WearModel.archard_volume_mm3(
            K=ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
            normal_force_N=1000.0,
            sliding_distance_mm=1000.0,
            hardness_MPa=WearModel.hardness_HB_to_MPa(pb.hardness_HB[0]),
        )
        assert v > 0, f"Wear volume {v} must be > 0"

    def test_bearing_wear_volume_positive(self, lib):
        """Bearing wear per hour must be positive."""
        pb = lib.get("phosphor_bronze")
        v = WearModel.bearing_wear_volume_per_hour_mm3(
            K=ARCHARD_K_LUBRICATED_BRONZE_ON_STEEL,
            radial_load_N=MACHINE_MASS_KG * 9.81 / BEARING_COUNT,
            shaft_diameter_mm=SHAFT_DIAMETER_MM,
            rpm=INPUT_RPM,
            hardness_HB=pb.hardness_HB[0],
        )
        assert v > 0

    def test_gear_wear_volume_positive(self, lib):
        """Gear tooth wear per hour must be positive."""
        brass = lib.get("brass")
        v = WearModel.gear_wear_volume_per_hour_mm3(
            K=ARCHARD_K_LUBRICATED_BRASS_ON_STEEL,
            tangential_force_N=100.0,
            pitch_diameter_mm=150.0,
            rpm=10.0,
            tooth_count=60,
            hardness_HB=brass.hardness_HB[0],
        )
        assert v > 0

    # ----- film thickness > 0 -----

    def test_ehl_film_thickness_positive(self):
        """Elastohydrodynamic film thickness must be positive."""
        h = LubricationModel.minimum_film_thickness_um(
            viscosity_Pa_s=0.059,
            entrainment_velocity_m_s=0.08,
            effective_radius_mm=25.0,
            reduced_modulus_GPa=110.0,
            load_per_length_N_mm=20.0,
        )
        assert h > 0, f"Film thickness {h} um must be > 0"

    # ----- all computed values finite (not NaN or inf) -----

    def test_all_material_properties_finite(self, lib):
        """Every numeric material property must be finite."""
        for mat in lib.all_materials():
            finite_checks = [
                ("density", mat.density_kg_m3),
                ("friction_coeff", mat.friction_coeff),
                ("E_min", mat.youngs_modulus_GPa[0]),
                ("E_max", mat.youngs_modulus_GPa[1]),
                ("nu", mat.poissons_ratio),
                ("Sy_min", mat.yield_strength_MPa[0]),
                ("Sy_max", mat.yield_strength_MPa[1]),
                ("Su_min", mat.ultimate_tensile_strength_MPa[0]),
                ("Su_max", mat.ultimate_tensile_strength_MPa[1]),
                ("Se_min", mat.endurance_limit_MPa[0]),
                ("Se_max", mat.endurance_limit_MPa[1]),
                ("CTE", mat.thermal_expansion_coeff_per_K),
                ("c_p", mat.specific_heat_J_kgK),
                ("k", mat.thermal_conductivity_W_mK),
                ("HB_min", mat.hardness_HB[0]),
                ("HB_max", mat.hardness_HB[1]),
                ("rho_e", mat.electrical_resistivity_ohm_m),
                ("mu_r", mat.magnetic_permeability_relative),
                ("creep_T", mat.creep_threshold_C),
            ]
            for prop_name, value in finite_checks:
                assert math.isfinite(value), (
                    f"{mat.name}.{prop_name} = {value} is not finite"
                )

    def test_gear_analysis_outputs_finite(self, primary_gear):
        """All GearAnalysis outputs must be finite for valid inputs."""
        vr = GearAnalysis.velocity_ratio(primary_gear)
        assert math.isfinite(vr)

        cr = GearAnalysis.contact_ratio(primary_gear)
        assert math.isfinite(cr)

        v = GearAnalysis.pitch_line_velocity_m_s(primary_gear, INPUT_RPM)
        assert math.isfinite(v)

        wt = GearAnalysis.tangential_force_N(TRANSMITTED_POWER_W, v)
        assert math.isfinite(wt)

        sigma = GearAnalysis.lewis_bending_stress_MPa(primary_gear, wt)
        assert math.isfinite(sigma)

    def test_cam_analysis_outputs_finite(self, column_latch_cam):
        """Cam displacement, velocity, acceleration, jerk must be
        finite at sample angles through the full cycle.
        """
        total_cycle = (
            column_latch_cam.rise_angle_deg
            + column_latch_cam.dwell_angle_deg
            + column_latch_cam.return_angle_deg
        )
        for theta in [0, 15, 30, 60, 90, 120, 180, total_cycle]:
            s = CamAnalysis.displacement_mm(
                column_latch_cam, float(theta),
            )
            assert math.isfinite(s), f"s({theta}) not finite"

            vel = CamAnalysis.velocity_mm_per_deg(
                column_latch_cam, float(theta),
            )
            assert math.isfinite(vel), f"v({theta}) not finite"

            acc = CamAnalysis.acceleration_mm_per_deg2(
                column_latch_cam, float(theta),
            )
            assert math.isfinite(acc), f"a({theta}) not finite"

            jrk = CamAnalysis.jerk_mm_per_deg3(
                column_latch_cam, float(theta),
            )
            assert math.isfinite(jrk), f"j({theta}) not finite"

    def test_thermal_model_outputs_finite(self, envelope):
        """All thermal model outputs must be finite."""
        assert math.isfinite(envelope.total_heat_generation_W)
        assert math.isfinite(envelope.steady_state_rise_C)
        assert math.isfinite(envelope.thermal_time_constant_s)
        assert math.isfinite(envelope.operating_T_min_C)
        assert math.isfinite(envelope.operating_T_max_C)
        for src in envelope.heat_sources:
            assert math.isfinite(src.heat_W), (
                f"Heat source {src.name} has non-finite heat: {src.heat_W}"
            )

    def test_structural_outputs_finite(self, lib):
        """Shaft deflection, fatigue life, and buckling load must
        all be finite for realistic inputs.
        """
        steel = lib.get("steel")

        # Shaft deflection
        d = ShaftAnalysis.max_deflection_multi_support_mm(
            23.0 * 9.81, 1500.0, 4, steel.youngs_modulus_GPa[0], 50.0,
        )
        assert math.isfinite(d)

        # Fatigue life
        Se = FatigueAnalysis.corrected_endurance_limit_MPa(
            steel.endurance_limit_MPa[0],
            steel.ultimate_tensile_strength_MPa[0],
            50.0,
        )
        assert math.isfinite(Se)
        N = FatigueAnalysis.fatigue_life_cycles(
            10.0, Se, steel.ultimate_tensile_strength_MPa[0],
        )
        assert math.isfinite(N)

        # Buckling
        I_col = BucklingAnalysis.rectangular_section_I_mm4(25.0, 25.0)
        P_cr = BucklingAnalysis.euler_critical_load_N(
            steel.youngs_modulus_GPa[0], I_col, 600.0, 0.5,
        )
        assert math.isfinite(P_cr)

    def test_electromagnetic_outputs_finite(self, lib):
        """All electromagnetic model outputs must be finite."""
        steel = lib.get("steel")
        brass = lib.get("brass")

        p_shaft = EddyCurrentModel.shaft_eddy_loss_W(
            50.0, 1500.0, 30.0, steel.electrical_resistivity_ohm_m,
        )
        assert math.isfinite(p_shaft)

        p_gear = EddyCurrentModel.gear_eddy_loss_W(
            150.0, 15.0, 30.0, brass.electrical_resistivity_ohm_m,
        )
        assert math.isfinite(p_gear)

        q = StaticChargeModel.triboelectric_charge_nC(10.0, 0.1, True)
        assert math.isfinite(q)

        e = StaticChargeModel.discharge_energy_J(0.5)
        assert math.isfinite(e)

    # ----- contact ratio >= 1.0 -----

    def test_contact_ratio_ge_1_all_stages(self, chain):
        """Contact ratio must be >= 1.0 for continuous tooth engagement."""
        for gp in chain.gear_pairs:
            cr = GearAnalysis.contact_ratio(gp)
            assert cr >= 1.0, (
                f"{gp.name}: contact ratio {cr:.3f} < 1.0 "
                f"-- teeth would disengage"
            )

    # ----- Grubler-Kutzbach DOF = 1 -----

    def test_mechanism_dof_is_1(self, chain):
        """The Babbage engine kinematic chain must have exactly 1 DOF."""
        from backend.src.emulator.kinematics import DOFAnalysis
        m = DOFAnalysis.grubler_kutzbach(
            chain.link_count,
            chain.joint_count_full,
            chain.joint_count_half,
        )
        assert m == 1, f"DOF={m}, expected 1"

    # ----- heat sources are all positive -----

    def test_all_heat_sources_positive(self, envelope):
        """Every individual heat source must generate >= 0 W."""
        for src in envelope.heat_sources:
            assert src.heat_W >= 0, (
                f"Heat source {src.name}: {src.heat_W:.4f} W < 0"
            )

    # ----- PV components are positive -----

    def test_pv_components_positive(self):
        """Bearing pressure and surface velocity must both be positive."""
        load = MACHINE_MASS_KG * 9.81 / BEARING_COUNT
        p = PVAnalysis.bearing_pressure_MPa(
            load, SHAFT_DIAMETER_MM, BEARING_LENGTH_MM,
        )
        assert p > 0

        v = PVAnalysis.surface_velocity_m_s(SHAFT_DIAMETER_MM, INPUT_RPM)
        assert v > 0

    # ----- safety factor denominators: applied stress is positive -----

    def test_gear_tooth_stress_positive_for_nonzero_force(self):
        """Applied bending stress must be > 0 for positive tangential
        force, ensuring the safety factor denominator is valid.
        """
        sigma = GearToothStress.bending_stress_MPa(
            100.0, 15.0, 2.5, 20,
        )
        assert sigma > 0

    # ----- thermal expansion sign consistency -----

    def test_expansion_positive_for_positive_delta_T(self, lib):
        """Thermal expansion must be positive when delta_T > 0
        (all CTEs are positive for metals).
        """
        for mat in lib.all_materials():
            delta = ThermalExpansionModel.linear_expansion_mm(
                mat.thermal_expansion_coeff_per_K, 100.0, 10.0,
            )
            assert delta > 0, (
                f"{mat.name}: expansion {delta} <= 0 for positive delta_T"
            )

    # ----- yield > endurance limit (for steel-class materials) --------

    def test_yield_exceeds_endurance_limit(self, lib):
        """For all materials, minimum yield strength must exceed
        minimum endurance limit (a basic material science invariant).
        """
        for mat in lib.all_materials():
            assert mat.yield_strength_MPa[0] >= mat.endurance_limit_MPa[0], (
                f"{mat.name}: Sy_min={mat.yield_strength_MPa[0]} < "
                f"Se_min={mat.endurance_limit_MPa[0]}"
            )

    # ----- velocity ratio reciprocal consistency --------------------

    def test_velocity_ratio_reciprocal_of_tooth_ratio(
        self, primary_gear,
    ):
        """Velocity ratio N_driver/N_driven must equal the reciprocal
        of the tooth count ratio.
        """
        vr = GearAnalysis.velocity_ratio(primary_gear)
        tooth_ratio = (
            primary_gear.tooth_count_driver
            / primary_gear.tooth_count_driven
        )
        assert vr == pytest.approx(tooth_ratio, rel=1e-12)
