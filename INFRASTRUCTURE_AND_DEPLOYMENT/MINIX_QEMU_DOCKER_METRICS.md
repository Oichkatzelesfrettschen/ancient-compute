## MINIX 3.4.0 RC6 — QEMU-in-Docker Tooling Metrics

Status: Ready. Uses existing minix-analysis/docker as source-of-truth; adds orchestration, metrics merge, and artifact contract.

Objectives
- Measure boot-to-login time, CPU %, RSS, disk I/O while running MINIX 3.4.0 RC6 under QEMU inside Docker.
- Produce reproducible CSV/JSON artifacts for CI comparison.

Requirements
- Host: Linux with Docker (privileged), `perf` optional.
- Assets: MINIX ISO available at `MINIX_ISO_PATH` (bind-mounted read-only).

Metrics
- Boot time (seconds) from QEMU start to login prompt detection.
- QEMU process CPU % and RSS over time (1s sampling).
- Disk I/O bytes read/written (from `/proc/<pid>/io`, per-second deltas).

Artifacts (per arch)
- `metrics/minix/<arch>/boot_time.json`          # total + markers + meta
- `metrics/minix/<arch>/resource_timeseries.csv` # CPU%/RSS timeseries
- `metrics/minix/<arch>/boot.log`                # raw serial log
- `metrics/minix/<arch>/qemu-debug.log`          # QEMU debug log

Quick Start
```bash
# 1) Build and run with ISO, collect artifacts under metrics/minix/i386
./scripts/minix_metrics.sh \
  --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso

# Optional: reuse prebuilt image, disable KVM
./scripts/minix_metrics.sh --iso /path/to/iso --no-build --no-kvm

# Multiple iterations with labeled run id
./scripts/minix_metrics.sh --iso /path/to/iso --iterations 3 --label nightly
```

Details
- Reuses `/home/eirikr/Playground/minix-analysis/docker` Dockerfiles and scripts.
- Headless serial console with QEMU writes logs to `/measurements` inside container; bind-mounted out to `metrics/minix/`.
- Post-processing runs `boot-profiler.py` (from minix image) and merges results via `scripts/minix_merge_metrics.py`.
- Optional CPU/RSS sampling runs inside the container until QEMU exits.
  - Aggregates recent runs via `scripts/minix_aggregate.py` into `summary.json`.
  - CSV columns: `timestamp,cpu_percent,rss_kb,read_bytes,write_bytes,delta_read_bytes,delta_write_bytes`.

CI (Self-hosted runner with /dev/kvm)
```yaml
name: Minix Metrics
on: [workflow_dispatch]
jobs:
  run:
    runs-on: [self-hosted, kvm]
    steps:
      - uses: actions/checkout@v4
      - name: Run metrics
        run: |
          ./scripts/minix_metrics.sh --iso "$MINIX_ISO_PATH" --iterations 3 --label ci
      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        with:
          name: minix-metrics
          path: metrics/minix

Regression Gate (optional)
```bash
python3 scripts/minix_regression_check.py \
  --metrics metrics/minix/i386/boot_time.json \
  --baseline-ms 65000 \
  --tolerance 20

# Or compare averages against repo baselines
python3 scripts/minix_compare_summary.py \
  --summary metrics/minix/i386/summary.json \
  --baseline INFRASTRUCTURE_AND_DEPLOYMENT/MINIX_BASELINES/i386.json \
  --tolerance 20
```

API (optional)
- `GET /api/v1/infra/minix/metrics?arch=i386` → latest `boot_time.json`
- `GET /api/v1/infra/minix/runs?arch=i386` → run list
- `GET /api/v1/infra/minix/run/{runId}?arch=i386` → specific run details

ARM Variant
- Use `--arch arm` in scripts/minix_metrics.sh. Requires suitable ARM MINIX ISO and your runner may need to rely on TCG (no KVM).
- Example:
```bash
./scripts/minix_metrics.sh --iso /path/to/minix_arm.iso --arch arm --no-kvm --label arm-test
```
```
