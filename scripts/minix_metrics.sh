#!/usr/bin/env bash
set -euo pipefail

ARCH="i386"        # i386 | arm
ISO=""
OUT="metrics/minix"
NAME_BASE="minix-profiler"
DOCKER_DIR="/home/eirikr/Playground/minix-analysis/docker"
IMAGE="minix-rc6-i386"
BUILD=1
USE_KVM=1
ITERATIONS=1
RUN_ID=""
LABEL=""

usage() {
  cat >&2 <<USAGE
Usage: $0 --iso /path/to/minix.iso [--arch i386|arm] [--out metrics/minix] [--name base_name] [--docker-dir /path/to/minix-analysis/docker] [--image image_name] [--no-build] [--no-kvm] [--iterations N] [--run-id ID] [--label LABEL]

Example:
  $0 --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso --iterations 3 --label smoke
USAGE
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --iso) ISO="$2"; shift 2;;
    --arch) ARCH="$2"; shift 2;;
    --out) OUT="$2"; shift 2;;
    --name) NAME_BASE="$2"; shift 2;;
    --docker-dir) DOCKER_DIR="$2"; shift 2;;
    --image) IMAGE="$2"; shift 2;;
    --no-build) BUILD=0; shift 1;;
    --no-kvm) USE_KVM=0; shift 1;;
    --iterations) ITERATIONS="$2"; shift 2;;
    --run-id) RUN_ID="$2"; shift 2;;
    --label) LABEL="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -f "$ISO" ]] || { echo "ISO not found: $ISO" >&2; exit 2; }

# Resolve paths
ISO_ABS="$(readlink -f "$ISO")"
OUT_ABS="$(readlink -f "$OUT")"
MEAS_DIR="$OUT_ABS/$ARCH"
RUNTIME_DIR="$MEAS_DIR/runtime"
mkdir -p "$MEAS_DIR/runs" "$RUNTIME_DIR"

# Prepare runtime dir with ISO where the entrypoint expects it
cp -f "$ISO_ABS" "$RUNTIME_DIR/minix_R3.4.0-rc6.iso"

# Determine run id
if [[ -z "$RUN_ID" ]]; then
  TS="$(date -u +%Y%m%dT%H%M%SZ)"
  if [[ -n "$LABEL" ]]; then RUN_ID="$LABEL-$TS"; else RUN_ID="$TS"; fi
fi

# Pick Dockerfile based on arch
case "$ARCH" in
  i386) DOCKERFILE="$DOCKER_DIR/Dockerfile.i386" ; IMAGE="${IMAGE}" ; ;;
  arm)  DOCKERFILE="$DOCKER_DIR/Dockerfile.arm"  ; IMAGE="${IMAGE/rc6-i386/rc6-arm}" ; ;;
  *) echo "Unsupported arch: $ARCH" >&2; exit 3;;
esac

if [[ $BUILD -eq 1 ]]; then
  echo "==> Building image $IMAGE from $DOCKERFILE"
  docker build -t "$IMAGE" -f "$DOCKERFILE" "$DOCKER_DIR"
else
  echo "==> Skipping image build for $IMAGE"
fi

KVM_FLAGS=()
if [[ $USE_KVM -eq 1 && -e /dev/kvm ]]; then
  KVM_FLAGS+=("--device" "/dev/kvm")
fi

for i in $(seq 1 "$ITERATIONS"); do
  NAME="${NAME_BASE}-${RUN_ID}-${i}"
  RUN_DIR="$MEAS_DIR/runs/${RUN_ID}-${i}"
  mkdir -p "$RUN_DIR"

  echo "==> Starting container $NAME (arch=$ARCH, iter=$i/$ITERATIONS)"
  set +e; docker rm -f "$NAME" >/dev/null 2>&1; set -e
  docker run -d --name "$NAME" "${KVM_FLAGS[@]}" \
    -v "$RUNTIME_DIR":/minix-runtime \
    -v "$OUT_ABS":/measurements \
    "$IMAGE" >/dev/null

  echo "==> Sampling QEMU CPU/RSS/IO inside container"
  docker exec "$NAME" bash -lc "\
    set -euo pipefail; \
    ARCH_DIR=/measurements/$ARCH; mkdir -p \$ARCH_DIR; \
    ARCH=$ARCH python3 - << 'PY' 
import os, time, subprocess, json, sys
from datetime import datetime
arch = os.environ.get('ARCH','')
arch_dir = os.path.join('/measurements', arch)
os.makedirs(arch_dir, exist_ok=True)
csv = os.path.join(arch_dir, 'resource_timeseries.csv')
with open(csv, 'w') as f:
    f.write('timestamp,cpu_percent,rss_kb,read_bytes,write_bytes,delta_read_bytes,delta_write_bytes\n')
prev_rb = prev_wb = None
for _ in range(600):
    try:
        pid = subprocess.check_output(['pgrep','-f','qemu-system']).decode().strip().split('\n')[0]
    except Exception:
        time.sleep(1); continue
    if not pid:
        time.sleep(1); continue
    try:
        os.kill(int(pid), 0)
    except Exception:
        break
    ts = datetime.utcnow().strftime('%Y-%m-%dT%H:%M:%SZ')
    try:
        out = subprocess.check_output(['ps','-p',pid,'-o','%cpu=','-o','rss=']).decode().strip().split()
        cpu = out[0]
        rss = out[1]
    except Exception:
        cpu = '0.0'; rss = '0'
    rb = wb = None
    try:
        with open(f'/proc/{pid}/io') as io:
            vals = {}
            for line in io:
                key,val = line.split(':')
                vals[key.strip()] = int(val.strip())
            rb = vals.get('read_bytes')
            wb = vals.get('write_bytes')
    except Exception:
        pass
    drb = dwb = ''
    if rb is not None and wb is not None:
        if prev_rb is not None and prev_wb is not None:
            drb = str(max(0, rb - prev_rb))
            dwb = str(max(0, wb - prev_wb))
        prev_rb, prev_wb = rb, wb
    line = ','.join([
        ts, cpu, rss,
        '' if rb is None else str(rb),
        '' if wb is None else str(wb),
        '' if drb=='' else drb,
        '' if dwb=='' else dwb,
    ])
    with open(csv,'a') as f:
        f.write(line + '\n')
    time.sleep(1)
PY
" >/dev/null 2>&1 &

  echo "==> Waiting for MINIX boot/profiling to complete"
  EXIT_CODE=0
  docker wait "$NAME" >/dev/null || EXIT_CODE=$?

  echo "==> Generating boot marker report via boot-profiler"
  docker run --rm -v "$OUT_ABS":/measurements "$IMAGE" \
    python3 /minix-runtime/boot-profiler.py --arch "$ARCH" --output-dir /measurements >/dev/null 2>&1 || true

  echo "==> Merging metrics and collecting artifacts"
  python3 scripts/minix_merge_metrics.py \
    --measurements "$OUT_ABS" \
    --arch "$ARCH" \
    --out "$RUN_DIR"

  # Update canonical latest copies
  cp -f "$RUN_DIR/boot_time.json" "$MEAS_DIR/boot_time.json" 2>/dev/null || true
  cp -f "$RUN_DIR/resource_timeseries.csv" "$MEAS_DIR/resource_timeseries.csv" 2>/dev/null || true
  cp -f "$RUN_DIR/boot.log" "$MEAS_DIR/boot.log" 2>/dev/null || true
  cp -f "$RUN_DIR/qemu-debug.log" "$MEAS_DIR/qemu-debug.log" 2>/dev/null || true
done

echo "==> Aggregating run statistics"
python3 scripts/minix_aggregate.py --root "$OUT_ABS" --arch "$ARCH" --out "$MEAS_DIR/summary.json"

echo "Artifacts (latest):"
echo "  - $MEAS_DIR/boot_time.json"
echo "  - $MEAS_DIR/resource_timeseries.csv"
echo "  - $MEAS_DIR/boot.log"
echo "  - $MEAS_DIR/qemu-debug.log"
echo "Run history: $MEAS_DIR/runs/"
