"""
Simulation Engine -- Time-Stepping Orchestrator for Analytical Engine Physics

Advances the complete simulation state through discrete time steps:
  shaft angle -> forces -> heat -> temperature -> clearances -> wear

Each step couples all physics modules dynamically through CouplingFunctions.
Long-duration runs accumulate state history for post-analysis.
"""

from __future__ import annotations

import math
from dataclasses import dataclass

from backend.src.emulator.electromagnetic import EddyCurrentModel, GalvanicCorrosionMatrix
from backend.src.emulator.materials import MaterialLibrary
from backend.src.emulator.simulation.coupling import CouplingFunctions
from backend.src.emulator.simulation.state import SimulationConfig, SimulationState
from backend.src.emulator.thermodynamics import (
    FrictionHeatModel,
)
from backend.src.emulator.tribology import (
    LubricationModel,
    PVAnalysis,
)


@dataclass
class StepResult:
    """Diagnostic output from a single simulation step."""

    time_s: float
    temperature_C: float
    max_clearance_mm: float
    max_wear_volume_mm3: float
    shaft_deflection_mm: float
    total_heat_W: float
    friction_coeff: float
    lubrication_regime: str
    lambda_ratio: float


@dataclass
class SimulationResult:
    """Complete result from a long-duration simulation run."""

    duration_s: float
    steps: int
    final_state: SimulationState
    history: list[StepResult]
    limiting_component: str = ""
    failure_time_s: float = float("inf")


class SimulationEngine:
    """Time-stepping orchestrator coupling all physics modules.

    Execution order per step:
      1. Advance shaft angle and time
      2. Compute friction coefficient from lubrication state
      3. Compute bearing friction heat
      4. Add gear mesh heat
      5. Advance temperature (Crank-Nicolson)
      6. Update oil viscosity from temperature
      7. Update lubrication film thickness and regime
      8. Compute bearing wear increments
      9. Accumulate sliding distance
     10. Update bearing clearances (thermal + wear)
     11. Redistribute bearing loads from clearances
     12. Compute shaft deflection
     13. Check failure limits
    """

    def __init__(
        self,
        config: SimulationConfig,
        lib: MaterialLibrary | None = None,
    ) -> None:
        self.config = config
        self.lib = lib if lib is not None else MaterialLibrary()
        self.state = self._initial_state()
        self._failed = False
        self._failure_reason = ""

    def _initial_state(self) -> SimulationState:
        """Build initial state from config."""
        n = self.config.bearing_count
        initial_load = self._total_gravity_load_N() / max(n, 1)
        return SimulationState(
            temperature_C=self.config.ambient_temperature_C,
            ambient_temperature_C=self.config.ambient_temperature_C,
            bearing_clearances_mm=[self.config.initial_clearance_mm] * n,
            bearing_wear_volumes_mm3=[0.0] * n,
            bearing_loads_N=[initial_load] * n,
            gear_backlash_mm=self.config.initial_gear_backlash_mm,
            oil_viscosity_Pa_s=0.1,  # Mineral oil at 40 C
        )

    def _total_gravity_load_N(self) -> float:
        """Dead weight of the machine [N]."""
        return self.config.machine_mass_kg * 9.81

    # ------------------------------------------------------------------
    # Single Step
    # ------------------------------------------------------------------

    def step(self) -> StepResult:
        """Advance simulation by one time step dt_s.

        Returns a StepResult with diagnostic quantities.
        """
        cfg = self.config
        st = self.state
        dt = cfg.dt_s
        omega = cfg.omega_rad_s

        # 1. Advance time and shaft angle
        st.time_s += dt
        angle_advance_deg = 360.0 * cfg.rpm / 60.0 * dt
        st.shaft_angle_deg = (st.shaft_angle_deg + angle_advance_deg) % 360.0
        st.rotation_count = int(st.time_s * cfg.rpm / 60.0)

        # 2. Friction coefficient from lubrication regime
        mu = self._effective_friction_coeff()

        # 3. Bearing friction heat
        Q_bearing = CouplingFunctions.friction_heat_from_bearings(
            st.bearing_loads_N, cfg.shaft_diameter_mm, omega, mu,
        )

        # 4. Gear mesh heat (assume 98% efficiency for lubricated spur gears)
        Q_gear = FrictionHeatModel.gear_mesh_heat_W(
            cfg.transmitted_power_W, 0.98,
        )

        # 4.1 Electromagnetic losses (Eddy currents)
        rho_shaft = self.lib.get(cfg.shaft_material).electrical_resistivity_ohm_m
        Q_eddy = EddyCurrentModel.shaft_eddy_loss_W(
            cfg.shaft_diameter_mm, cfg.shaft_length_mm, cfg.rpm, rho_shaft
        )

        Q_total = Q_bearing + Q_gear + Q_eddy
        st.total_heat_generation_W = Q_total

        # 4.2 Galvanic corrosion risk accumulation
        # Risk factor = sum of potential differences between contacting materials
        # Multiplied by dt and a "humidity factor" (simulated as 1.0 for now)
        galvanic = GalvanicCorrosionMatrix()
        # Contacting pairs: shaft-bearing, gear-shaft
        dv1 = galvanic.potential_difference_V(cfg.shaft_material, cfg.bearing_material)
        dv2 = galvanic.potential_difference_V(cfg.gear_material, cfg.shaft_material)
        st.galvanic_risk_accumulator += (dv1 + dv2) * dt

        # 4.5 Energy consumption tracking
        # Work = Torque * Delta_Theta
        d_theta_rad = (angle_advance_deg * math.pi / 180.0)
        # Total torque from bearings and gears (approximate from heat)
        # Power = Torque * omega -> Torque = Power / omega
        torque_Nm = Q_total / cfg.omega_rad_s if cfg.omega_rad_s > 0 else 0.0
        st.total_torque_Nm = torque_Nm
        st.energy_consumed_J += torque_Nm * d_theta_rad

        # 4.6 Ambient temperature fluctuation (Day/Night cycle)
        # T_amb = T_base + Amplitude * sin(2*pi * t / Period)
        # 24-hour period = 86400s
        amplitude = 5.0 # +/- 5 degrees
        st.ambient_temperature_C = cfg.ambient_temperature_C + amplitude * math.sin(2 * math.pi * st.time_s / 86400.0)

        # 5. Temperature step (Crank-Nicolson via coupling)
        st.temperature_C = CouplingFunctions.thermal_step(st, cfg, Q_total)

        # 6. Update oil viscosity from temperature
        st.oil_viscosity_Pa_s = CouplingFunctions.viscosity_at_temperature(
            0.1, st.temperature_C,
        )

        # 7. Lubrication film thickness and regime
        lam, regime = self._update_lubrication()
        st.lambda_ratio = lam
        st.lubrication_regime = regime

        # 8. Bearing wear increments
        wear_increments = CouplingFunctions.bearing_wear_step(st, cfg, self.lib)
        for i, dV in enumerate(wear_increments):
            if i < len(st.bearing_wear_volumes_mm3):
                st.bearing_wear_volumes_mm3[i] += dV

        # 9. Sliding distance accumulation
        ds = math.pi * cfg.shaft_diameter_mm * cfg.rpm / 60.0 * dt
        st.total_sliding_distance_mm += ds

        # 10. Update bearing clearances (thermal + wear)
        st.bearing_clearances_mm = CouplingFunctions.update_bearing_clearances(
            st, cfg, self.lib,
        )

        # 11. Redistribute bearing loads from clearances
        total_load = self._total_gravity_load_N()
        st.bearing_loads_N = CouplingFunctions.redistribute_bearing_loads(
            total_load, cfg.bearing_count, st.bearing_clearances_mm,
        )

        # 12. Shaft deflection
        st.shaft_deflection_mm = CouplingFunctions.shaft_deflection_mm(
            st, cfg, self.lib,
        )

        # 13. Check failure limits
        self._check_failures()

        # Build diagnostic result
        max_c = max(st.bearing_clearances_mm) if st.bearing_clearances_mm else 0.0
        max_w = max(st.bearing_wear_volumes_mm3) if st.bearing_wear_volumes_mm3 else 0.0

        return StepResult(
            time_s=st.time_s,
            temperature_C=st.temperature_C,
            max_clearance_mm=max_c,
            max_wear_volume_mm3=max_w,
            shaft_deflection_mm=st.shaft_deflection_mm,
            total_heat_W=Q_total,
            friction_coeff=mu,
            lubrication_regime=regime,
            lambda_ratio=lam,
        )

    # ------------------------------------------------------------------
    # Long-Duration Run
    # ------------------------------------------------------------------

    def run(
        self,
        duration_s: float,
        record_interval_s: float = 60.0,
    ) -> SimulationResult:
        """Run simulation for a given duration.

        Args:
            duration_s: Total simulation time [s].
            record_interval_s: Interval between recorded history points [s].

        Returns:
            SimulationResult with final state and sampled history.
        """
        history: list[StepResult] = []
        steps = 0
        next_record = 0.0

        while self.state.time_s < duration_s and not self._failed:
            result = self.step()
            steps += 1

            if self.state.time_s >= next_record:
                history.append(result)
                next_record += record_interval_s

        return SimulationResult(
            duration_s=self.state.time_s,
            steps=steps,
            final_state=self.state.copy(),
            history=history,
            limiting_component=self._failure_reason,
            failure_time_s=(
                self.state.time_s if self._failed else float("inf")
            ),
        )

    def predict_maintenance(
        self,
        max_hours: float = 10000.0,
    ) -> SimulationResult:
        """Run until a failure limit is reached or max_hours elapsed.

        Args:
            max_hours: Upper bound on simulation time [hours].

        Returns:
            SimulationResult with limiting_component and failure_time_s.
        """
        max_s = max_hours * 3600.0
        return self.run(max_s, record_interval_s=3600.0)

    # ------------------------------------------------------------------
    # Internal Helpers
    # ------------------------------------------------------------------

    def _effective_friction_coeff(self) -> float:
        """Friction coefficient adjusted by lubrication regime.

        Full-film: mu ~ 0.001-0.01 (hydrodynamic)
        Mixed:     mu ~ 0.01-0.05
        Boundary:  mu ~ 0.08-0.15 (dry metal-on-metal)
        """
        bearing_mat = self.lib.get(self.config.bearing_material)
        base_mu = bearing_mat.friction_coeff  # dry/boundary value

        regime = self.state.lubrication_regime
        if regime == "full_film":
            return base_mu * 0.05  # ~95% reduction
        elif regime == "mixed":
            return base_mu * 0.3  # ~70% reduction
        else:
            return base_mu  # boundary/dry

    def _update_lubrication(self) -> tuple[float, str]:
        """Compute lambda ratio and lubrication regime.

        Uses Hamrock-Dowson film thickness with current viscosity,
        comparing to composite surface roughness.
        """
        cfg = self.config
        st = self.state

        # Entrainment velocity (shaft surface speed)
        u = PVAnalysis.surface_velocity_m_s(cfg.shaft_diameter_mm, cfg.rpm)

        # Average bearing load
        avg_load = (
            sum(st.bearing_loads_N) / len(st.bearing_loads_N)
            if st.bearing_loads_N else 0.0
        )
        if avg_load <= 0 or u <= 0:
            return (3.0, "full_film")

        # Load per unit length of bearing
        w_per_length = avg_load / cfg.bearing_length_mm if cfg.bearing_length_mm > 0 else 0.0

        # Reduced modulus (steel shaft + bronze bearing)
        shaft_mat = self.lib.get(cfg.shaft_material)
        bearing_mat = self.lib.get(cfg.bearing_material)
        E1 = shaft_mat.youngs_modulus_GPa[0]
        nu1 = shaft_mat.poissons_ratio
        E2 = bearing_mat.youngs_modulus_GPa[0]
        nu2 = bearing_mat.poissons_ratio
        E_reduced = 1.0 / ((1 - nu1**2) / E1 + (1 - nu2**2) / E2)

        # Effective radius (journal bearing: R = d/2)
        R_eff = cfg.shaft_diameter_mm / 2.0

        h_min = LubricationModel.minimum_film_thickness_um(
            st.oil_viscosity_Pa_s, u, R_eff, E_reduced, w_per_length,
        )

        # Surface roughness: machined shaft ~0.8 um, bearing ~1.6 um
        Ra_shaft = 0.8
        Ra_bearing = 1.6
        lam = LubricationModel.lambda_ratio(h_min, Ra_shaft, Ra_bearing)
        regime = LubricationModel.regime(lam)
        return (lam, regime)

    def _check_failures(self) -> None:
        """Check all failure limits and set _failed if exceeded."""
        cfg = self.config
        st = self.state

        # Temperature limit
        if st.temperature_C >= cfg.temperature_limit_C:
            self._failed = True
            self._failure_reason = "temperature"
            return

        # Bearing clearance limit
        if st.bearing_clearances_mm:
            max_c = max(st.bearing_clearances_mm)
            if max_c >= cfg.clearance_limit_mm:
                self._failed = True
                self._failure_reason = "bearing_clearance"
                return

        # Bearing seizure (clearance <= 0)
        if st.bearing_clearances_mm:
            min_c = min(st.bearing_clearances_mm)
            if min_c <= 0.0:
                self._failed = True
                self._failure_reason = "bearing_seizure"
                return

        # Gear backlash limit
        if st.gear_backlash_mm >= cfg.backlash_limit_mm:
            self._failed = True
            self._failure_reason = "gear_backlash"
            return

    @property
    def failed(self) -> bool:
        """Whether any failure limit has been exceeded."""
        return self._failed

    @property
    def failure_reason(self) -> str:
        """Name of the limiting component, if failed."""
        return self._failure_reason

    def reset(self) -> None:
        """Reset to initial state."""
        self.state = self._initial_state()
        self._failed = False
        self._failure_reason = ""
