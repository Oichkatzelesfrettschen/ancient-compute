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

**STATUS**: RESOLVED 2026-03-20.
9 languages at Gate 3: LISP, Haskell, IDRIS2 (Babbage IR stub; real execution
pending idris2 AUR build), SystemF (Babbage IR), ALGOL68 (a68g), C (Babbage IR),
C++ (g++), Python (Babbage IR), MicroPython.  1620 unit tests passing.
Java language service remains a stub (see Java section above).

---

### Gate 2: C Freestanding Subset Completion

**STATUS**: RESOLVED 2026-03-20.
C freestanding subset fully compiles to Babbage ISA end-to-end.  Four code
generator bugs fixed (emitter label addresses, trailing labels, comparison
BinaryOp encoding, return terminator overwrite).  for-loop init declarations
added to C parser.  AEExecutionService connects compilation to AE engine.

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

### mypy: RESOLVED 2026-03-20

**STATUS**: `python -m mypy backend/src/` reports 0 errors across all files.
The no-untyped-def and no-any-return rules were fixed in a dedicated sprint
covering metrics, adapter, codegen, emulator, models, and assembler modules.
Enigma implicit Optional parameters also fixed.

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

**RESOLVED**: 2026-03-20.

- `test_rate_limiting.py`: 25 tests (token bucket, middleware routing, client ID)
- `test_docker_manager.py`: 16 tests (singleton, backend selection, container config,
  status report) -- mocks Docker so no daemon required
- `test_execution_cache.py`: 22 tests (CacheEntry, key generation, get/set/TTL,
  hit/miss counters, eviction, cleanup)
- `test_query_cache.py`: 17 tests (key gen, _store, invalidation, TTL, stats)
- `test_metrics.py`: 22 tests (utility functions, ExecutionContext, middleware path skip)

**STILL DEFERRED**: `cli/tui/` requires Textual's built-in test framework
(`textual.testing`).  TUI tests deferred until Textual test infra is wired.

---

## How to Re-Activate a Deferred Item

1. Verify the triggering condition is met.
2. Create a task in the active TODO tracker (`docs/general/TODO_TRACKER.md`).
3. Update this file: remove the item from the deferred list and note
   the activation date and triggering condition.
4. Reference the original deferred entry in the commit message for
   traceability.

---

*Last validated: 2026-03-20 (mypy sprint complete: 0 errors; infrastructure coverage sprint complete: 1722 unit tests)*
