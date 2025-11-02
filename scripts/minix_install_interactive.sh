#!/usr/bin/env bash
set -euo pipefail

# Interactive MINIX installation helper.
# Launches the minix-analysis Docker image with VNC port exposed for manual setup.

ARCH="i386"       # i386 | arm
ISO=""
OUT="metrics/minix"
NAME="minix-install"
DOCKER_DIR="/home/eirikr/Playground/minix-analysis/docker"
IMAGE="minix-rc6-i386"
BUILD=1
VNC_PORT=5900
SSH_PORT=2222
METRICS_PORT=9000

usage() {
  cat >&2 <<USAGE
Usage: $0 --iso /path/to/minix.iso [--arch i386|arm] [--out metrics/minix] [--name container_name] [--docker-dir /path/to/minix-analysis/docker] [--image image_name] [--no-build] [--vnc-port 5900]

This starts QEMU with VNC (:0) inside a Docker container and publishes host port 5900.
Connect your VNC client to localhost:5900 and complete the MINIX installer.
USAGE
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --iso) ISO="$2"; shift 2;;
    --arch) ARCH="$2"; shift 2;;
    --out) OUT="$2"; shift 2;;
    --name) NAME="$2"; shift 2;;
    --docker-dir) DOCKER_DIR="$2"; shift 2;;
    --image) IMAGE="$2"; shift 2;;
    --no-build) BUILD=0; shift 1;;
    --vnc-port) VNC_PORT="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -f "$ISO" ]] || { echo "ISO not found: $ISO" >&2; exit 2; }

# Resolve paths
ISO_ABS="$(readlink -f "$ISO")"
OUT_ABS="$(readlink -f "$OUT")"
MEAS_DIR="$OUT_ABS/$ARCH"
RUNTIME_DIR="$MEAS_DIR/runtime"
mkdir -p "$MEAS_DIR" "$RUNTIME_DIR"

# Prepare runtime directory with ISO copy
cp -f "$ISO_ABS" "$RUNTIME_DIR/minix_R3.4.0-rc6.iso"

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
  echo "==> Skipping image build for ${IMAGE}"
fi

echo "==> Starting interactive installation container: ${NAME}"
echo "    VNC: localhost:${VNC_PORT} (passwordless)"
echo "    ISO: ${ISO_ABS}"
echo "    Measurements: ${OUT_ABS}"
set +e; docker rm -f "${NAME}" >/dev/null 2>&1; set -e

# Publish VNC and optional forwarded ports; mount prepared runtime dir + measurements
docker run --name "${NAME}" \
  -p "${VNC_PORT}":5900 -p "${SSH_PORT}":2222 -p "${METRICS_PORT}":9000 \
  -v "${RUNTIME_DIR}":/minix-runtime \
  -v "${OUT_ABS}":/measurements \
  "${IMAGE}"

echo "Container exited. If installation completed, a larger qcow2 should be present in /measurements/${ARCH}."
