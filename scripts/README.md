# scripts/

Utility scripts for development, measurement, benchmarking, and MINIX analysis.
These are standalone scripts -- not part of the installed package.
Run from the project root unless noted.

## Emulator Measurement

| Script | Purpose |
|--------|---------|
| `measure_emulator.py` | Measures emulator instruction throughput (instructions/sec) |
| `measure_emulator_clock.py` | Measures emulator clock accuracy vs wall time |
| `measure_emulator_real.py` | End-to-end program execution timing |
| `measure_emulator_resources.py` | CPU and memory usage per emulator run (requires psutil) |
| `measure_emulator_throughput.py` | Sustained throughput benchmark over long runs |
| `measure_longrun.py` | Long-running stability test measuring drift |
| `benchmark_emulator_programs.py` | Runs suite of reference programs and records timing |

## Backend Measurement

| Script | Purpose |
|--------|---------|
| `measure_backend.sh` | Shell wrapper: starts backend, fires test requests, records latency |
| `measure_backend_resources.py` | CPU and memory profiling for the FastAPI backend |

## MINIX Analysis

Scripts for analyzing MINIX 3 boot and runtime metrics. Used for curriculum content
on operating system history (Volume 5).

| Script | Purpose |
|--------|---------|
| `minix_aggregate.py` | Aggregates multiple MINIX metric runs into summary statistics |
| `minix_compare_summary.py` | Compares two MINIX metric summaries (before/after) |
| `minix_merge_metrics.py` | Merges per-run metric files into a single dataset |
| `minix_regression_check.py` | Flags metric regressions against a stored baseline |
| `minix_set_baseline.py` | Records current MINIX metrics as the regression baseline |
| `minix_host_qemu.sh` | Starts QEMU with MINIX 3 image; used with measure scripts |
| `minix_install_interactive.sh` | Interactive MINIX 3 install helper (for initial setup) |
| `minix_metrics.sh` | Shell script to collect boot time and memory metrics from MINIX |
| `minix_verify_boot.sh` | Verifies MINIX 3 boots cleanly within timeout |

## Documentation and Environment

| Script | Purpose |
|--------|---------|
| `generate_status.py` | Generates docs/general/STATUS.md with current test/lint metrics. Run via `make status`. |
| `gen_doc_index.py` | Scans docs/ and regenerates the document index table |
| `env_fingerprint.sh` | Records tool versions (Python, Node, Docker, etc.) for reproducibility |
| `analyze_all.sh` | Runs ruff + mypy + tests and writes a combined report |
| `analyze_csv.py` | Parses and summarizes CSV output from measurement scripts |
| `fetch_sources.sh` | Downloads primary source PDFs referenced in SOURCE_MAP.md |

## SED Path Utilities

| File | Purpose |
|------|---------|
| `PATH_MAPPING.sed` | sed rules mapping old file paths to new locations (post Phase 9 consolidation) |
| `FIX_MALFORMED_PATHS.sed` | Fixes malformed path references in docs |
| `COMPREHENSIVE_FIX_MALFORMED.sed` | Extended version of FIX_MALFORMED_PATHS.sed |

## Usage Example

```bash
# Regenerate STATUS.md
make status

# Profile emulator resources
PYTHONPATH=. python3 scripts/measure_emulator_resources.py

# Run MINIX regression check
python3 scripts/minix_regression_check.py

# Capture environment fingerprint
bash scripts/env_fingerprint.sh
```
