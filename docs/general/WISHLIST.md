# Wishlist

Items here are aspirational -- no activation timeline, no blocking dependency on
current work.  They are recorded so good ideas are not lost, not because they are
planned.  If one becomes actionable, move it to the active TODO tracker.

---

## Language Services

**Java execution**: Full JVM sandbox requires a seccomp-bpf profile validated
against the JVM's JIT and internal syscall patterns.  The language readiness
matrix marks Java as NOT READY; the compiler exists but the service is a stub.

---

## Physical Hardware Access

**Valve timing (DE2)**: Precise lap/lead/cutoff angles for the Difference Engine
No. 2 steam valves require direct measurement from the Science Museum Group
physical machine.  The simulation uses conservative secondary-source estimates.
See `docs/hardware/CAM_TIMING.md`.

**Steam drive calibration**: Operating power of the physical DE2 may differ from
the design spec (~500 W).  `docs/hardware/POWER_TRAIN.md` uses the design figure.
Requires the same SMG access as valve timing.

---

## Infrastructure

**Kubernetes deployment**: K8s manifests, Vault secrets management, network
policies, and PVC configuration.  Blocked on production readiness and a target
cluster.

**Load testing**: API contract must be frozen for at least one sprint before
meaningful baselines can be established.

**Per-endpoint Prometheus metrics**: Latency histograms, error-rate counters, and
per-language execution metrics.  Basic request_count and uptime_seconds are live
at `/metrics`.

---

## Physics Model

**Full Hamrock-Dowson EHL formulation**: Dimensionless ellipticity ratio and
contact modulus for DE2 gears are not in published sources.  The simplified
HertzianContact model is adequate for 30 RPM low-load operation.

**Running-in s_0 from material properties**: Deriving the sliding-distance
constant from first-principles requires Abbott-Firestone surface texture data for
DE2-replica bearings.  Currently validated as `s_0_mm = 500.0` (empirical).

---

## CLI / TUI / Emulator

**Web-based TUI**: `textual serve` wrapper for browser delivery needs ASGI
integration and viewport sizing work on top of the terminal TUI.

**Audio output for PLAY opcode**: The PLAY barrel produces note events but
discards them.  PortAudio/pyaudio integration is platform-dependent and
incompatible with headless CI.

**Deck physics coupling**: `ancient-compute deck --physics` prints an
informational message; `run_note_g_exact` uses a state-dict loop that
`SimulationBridge` cannot intercept.  Would require refactoring the deck
runner to use `Engine` with `instruction_cards`.

**TUI test suite**: `cli/tui/` tests require `textual.testing` framework
wiring that has not been set up.

---

*Last updated: 2026-03-20*
