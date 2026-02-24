Simulation Parameter Workflow (Babbage hard gate)

Extraction pipeline:

1. Ensure source cache exists under `docs/sources/cache`.
2. Run `tools/simulation/extract_params.py` to refresh `docs/simulation/extracted.yaml`.
3. Maintain canonical values in `docs/simulation/sim_schema.yaml`.
4. Maintain evidence in `docs/simulation/CITATIONS.md`.
5. Validate with `tools/simulation/verify_babbage_params.py`.

Parameter mapping:

1. Materials and friction: DE2 construction and maintenance context.
2. Valve timing (lap/lead/cutoff): DE2 timing-clearance references plus provisional first-pass values.
3. Gear train and drive cadence: DE2 4:1 reduction and crank-rate guidance.
4. Bearings and lubrication: DE2 maintenance schedule and oil-class guidance.
5. Card feed and jam profile: DE2 paper-feed and jam diagnostics sections.

Validation criteria:

1. Required Babbage fields are present and non-`TBD`.
2. Required citation keys are present and non-placeholder.
3. Numeric fields are within sanity ranges used by tests.
