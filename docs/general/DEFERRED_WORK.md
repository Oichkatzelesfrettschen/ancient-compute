# Deferred Work Registry

**Date**: 2026-02-26
**Status**: Active Registry -- update when items are resolved or re-scoped.
**Purpose**: Explicit record of work items that are intentionally deferred,
not forgotten.  Every entry has a WHY (why deferred), WHAT (what is deferred),
and WHEN (condition or milestone that would trigger re-activation).

---

## Language Services

### Java Language Service (stub only)

**WHY**: The Java service requires a full JVM sandbox configuration that
has not been validated for security properties equal to the C/Python/Haskell
services.  The sandbox-bpf seccomp profile for JVM is significantly more
complex due to JIT compilation and the JVM's internal syscall patterns.

**WHAT**: `services/java/` is a stub.  The backend includes a Java stub
in the language readiness matrix but the service does not execute code.

**WHEN**: Re-activate after the existing services have been stress-tested
in production and a JVM seccomp profile can be validated.

---

### Gate 3: Language Promotion (LISP, IDRIS2, SystemF, Java)

**WHY**: Gate 3 requires each language to pass a comprehensive integration
test suite including round-trip compilation to Babbage ISA.  LISP is at
~60% completion; IDRIS2 and SystemF require dependent-type infrastructure
not yet wired to the card compiler.  Java is a stub (see above).

**WHAT**: The language readiness matrix shows Gate 3 blocked for these
four languages.  See `docs/general/LANGUAGE_READINESS_MATRIX.md` and
`docs/hardware/TEST_PLAN.md` for gate criteria.

**WHEN**: Re-activate after Gate 2 (C freestanding subset) is fully green
and after card_compiler.py supports opcode emission for each target language.

---

### Gate 2: C Freestanding Subset Completion

**WHY**: The C freestanding subset requires a complete, formally defined
subset of C that compiles to Babbage ISA without a hosted runtime.  The
subset definition is partially complete but the formal grammar has gaps.

**WHAT**: `docs/general/FREESTANDING_C_SUBSET_PROFILE.md` documents the
current state.  Approximately 30% of the subset grammar is unspecified.

**WHEN**: Re-activate when the card compiler has demonstrated end-to-end
compilation for at least 5 non-trivial programs using the existing subset.

---

## Physical Hardware Access

### Valve Timing (lap/lead/cutoff)

**WHY**: Precise valve timing parameters (lap, lead, cutoff angles for the
steam inlet/exhaust valves) require direct measurement from the physical
Difference Engine No. 2 at the Science Museum Group, London.  No published
primary-source measurements are available in the literature accessible to
this project.

**WHAT**: The simulation uses conservative estimates for valve timing based
on secondary sources.  The `docs/hardware/CAM_TIMING.md` documents the
current estimates and marks them as unvalidated.

**WHEN**: Re-activate when a researcher gains access to the physical DE2
for direct measurement, or when the Science Museum Group publishes detailed
engineering drawings with valve timing specifications.

---

### Steam Drive Calibration

**WHY**: Same constraint as valve timing -- calibration requires the physical
machine.  The current power model uses the design specification of ~500 W
total, which may differ from measured operating power.

**WHAT**: `docs/hardware/POWER_TRAIN.md` uses an estimated 500 W figure.
The operating envelope sweep (`scripts/operational_envelope.py`) uses this
estimate throughout.

**WHEN**: Re-activate together with valve timing work above.

---

## Infrastructure and Operations

### Kubernetes Deployment

**WHY**: Kubernetes configuration requires production-grade secrets management
(Vault or equivalent), network policies, and persistent volume claims that
cannot be validated without a target cluster.  The project is not yet at
production readiness.

**WHAT**: No Kubernetes manifests exist.  The deployment guide
(`docs/general/DEPLOYMENT_GUIDE.md`) describes Docker Compose only.

**WHEN**: Re-activate at production readiness milestone: when all Gate 2
languages pass, the frontend is feature-complete, and a staging environment
is available.

---

### Load Testing

**WHY**: Load testing requires the API to be stable.  The API is still
evolving with new language services being added.  Premature load tests
would need to be re-run after each major API change.

**WHAT**: No load test suite exists.  Performance baselines are not
established.

**WHEN**: Re-activate after Gate 2 is complete and the API contract is
frozen for at least one sprint without breaking changes.

---

### Advanced Prometheus Metrics (per-endpoint instrumentation)

**WHY**: Basic Prometheus metrics (request count, uptime gauge) are
instrumented in `backend/src/main.py` and a `/metrics` endpoint is
exposed.  However, per-endpoint latency histograms, error-rate
counters, and language-service execution metrics are not yet wired.
Adding these requires the API contract to stabilize (post-Gate 2).

**WHAT**: `backend/src/main.py` exposes `/metrics` with request_count
and uptime_seconds.  Per-endpoint and per-language instrumentation is
deferred.

**WHEN**: Re-activate after Gate 2 is complete and the API surface is
stable enough to define meaningful SLIs.

---

## Physics Model Refinements

### Full Hamrock-Dowson Dimensionless EHL Formulation

**WHY**: The current tribology model uses a simplified Hamrock-Dowson
approach appropriate for the operating envelope.  The full dimensionless
formulation requires exact gear geometry (ellipticity ratio, contact
modulus) that is not available from published sources for DE2.

**WHAT**: `backend/src/emulator/tribology.py` uses the simplified
HertzianContact model.  The operating envelope Monte Carlo validation
(`scripts/monte_carlo_tolerance.py`) shows this is adequate for the
30 RPM low-load operating regime.

**WHEN**: Re-activate if the operating envelope is extended to higher
RPM or load conditions, or if geometry data becomes available from the
Science Museum Group.

---

### Running-In s_0 from Material Properties

**WHY**: The running-in sliding distance parameter s_0 is currently an
empirical constant (validated by Monte Carlo sensitivity analysis).
Deriving s_0 from first-principles material science requires surface
asperity statistics (Abbott-Firestone curve) that are not available for
the DE2 bearings.

**WHAT**: `backend/src/emulator/tribology.py` uses `s_0_mm = 500.0` as
a validated empirical value.  See `docs/simulation/GAP_REPORT.md` for
the validation record.

**WHEN**: Re-activate if surface texture measurement data for DE2-replica
bearings becomes available from machining partners.

---

## CLI / TUI / Emulator Extensions

### Web-Based TUI

**WHY**: Textual supports browser rendering via `textual serve` but requires
additional configuration and testing for production use.  The terminal TUI
shipped first as the lower-risk path.

**WHAT**: `backend/src/emulator/cli/tui/app.py` is a Textual app that runs
in the terminal.  A `textual serve` wrapper for browser delivery would need
ASGI integration and viewport sizing work.

**WHEN**: Re-activate after the terminal TUI has been used in testing and
the interaction model (keybindings, animation frame rate) is stable.

---

### `ancient-compute` Entry Point (system-wide install)

**RESOLVED**: 2026-03-19 (Third Debt Resolution Phase 3)

`make setup` now runs `pip install -e .` from the backend directory, so the
`ancient-compute` CLI entry point is available system-wide after running
`make setup`.  No further action required.

---

### Sound Output for PLAY Opcode

**WHY**: The PLAY barrel opcode produces note events but no actual audio.
Audio output requires OS-level integration (PortAudio, pyaudio, or similar)
that is platform-dependent and not appropriate for a headless CI environment.

**WHAT**: `backend/src/emulator/barrels.py` defines a PLAY barrel.
`backend/src/emulator/analytical_engine.py` executes it but discards output.

**WHEN**: Re-activate as an optional audio plugin when the project has a
desktop-focused use case or interactive demo mode.

---

### Opcode-Coupled Physics Simulation (Note G with evolving physics)

**PARTIALLY RESOLVED**: 2026-03-19 (Fourth Debt Resolution Phase 3)

`ancient-compute run --physics` now wires `SimulationBridge` to the `Engine`
for all `.basm` programs, advancing the physics model per opcode and printing
a full physics report (temperature, shaft deflection, lubrication regime,
energy) after execution.

**STILL DEFERRED**: The `deck --physics` path (Note G `run_note_g_exact`)
uses a state-dict execution loop, not the `Engine` class, so `SimulationBridge`
cannot intercept its opcodes.  The deck command prints an informational message
noting that full physics coupling for the deck runner is deferred.

**WHEN**: Re-activate the deck physics path when `run_note_g_exact` is
refactored to use `Engine` with `instruction_cards` (tracked under Gate 2
simulation integration).

---

## Code Quality

### mypy: ~974 errors across 118 files

**WHY**: The mypy error count is too large for a single debt resolution cycle.
Each module requires individual review to add correct type annotations without
changing runtime behavior.

**WHAT**: `cd backend && mypy src/ --strict --ignore-missing-imports` reports
~974 errors in 118 files (updated 2026-03-19; was 737 before `explicit_package_bases`
and additional test files were added).  The verify target now includes a
non-blocking mypy summary to track progress.

**WHEN**: Dedicate sprint(s) to type annotation.  Fourth Debt Resolution Sprints
A-F target: physics/types, analytical_engine/barrels, CLI/TUI, compilers,
services/API, and attr-defined/assignment/no-any-return cleanup.

---

### ruff: 434 pre-existing violations (23 rules)

**WHY**: The 23 ignored ruff rules in `pyproject.toml` represent 434 instances
that require line-by-line review.  Bulk auto-fix would risk changing semantics
(e.g., E741 ambiguous variable names in physics code where `l` means length).

**WHAT**: `pyproject.toml [tool.ruff.lint] ignore` lists 23 rules with instance
counts.  Violations are stable (no new ones added since 2nd debt resolution).

**WHEN**: Address one rule per sprint, starting with the most impactful (E501
line-too-long: 109 instances, E702 multiple-statements: 142 instances).

---

### Test coverage: docker_manager, caches, CLI/TUI

**PARTIALLY RESOLVED**: 2026-03-19 (Fourth Debt Resolution Phase 5)

CLI command coverage added via `tests/unit/test_cli_commands.py` (15 tests)
and assembler coverage via `tests/unit/test_cli_assembler.py` (21 tests).
`SimulationBridge` covered by `tests/unit/test_simulation_bridge.py` (21 tests).

**STILL DEFERRED**: `docker_manager.py` requires a running Docker daemon;
`execution_cache.py`/`query_cache.py` require Redis; `rate_limiting.py`,
`metrics.py`, and `cli/tui/` require integration test infrastructure.

**WHEN**: After API stabilization (post-Gate 2).  TUI tests should use
Textual's built-in test framework (`textual.testing`).

---

## How to Re-Activate a Deferred Item

1. Verify the triggering condition is met.
2. Create a task in the active TODO tracker (`docs/general/TODO_TRACKER.md`).
3. Update this file: remove the item from the deferred list and note
   the activation date and triggering condition.
4. Reference the original deferred entry in the commit message for
   traceability.

---

*Last validated: 2026-03-19 (4th Debt Resolution: entry-point resolved, physics coupling partial, mypy count updated, CLI coverage partial)*
