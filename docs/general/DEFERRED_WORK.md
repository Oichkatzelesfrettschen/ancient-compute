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

### Prometheus Metrics

**WHY**: Prometheus scraping requires a stable deployment target.  The
project uses `prometheus-client` in requirements.txt but no metrics
are currently instrumented in the API endpoints.

**WHAT**: The prometheus-client dependency is present but unused.  No
/metrics endpoint is exposed.

**WHEN**: Re-activate together with Kubernetes deployment above.

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

## How to Re-Activate a Deferred Item

1. Verify the triggering condition is met.
2. Create a task in the active TODO tracker (`docs/general/TODO_TRACKER.md`).
3. Update this file: remove the item from the deferred list and note
   the activation date and triggering condition.
4. Reference the original deferred entry in the commit message for
   traceability.

---

*Last validated: 2026-02-26*
