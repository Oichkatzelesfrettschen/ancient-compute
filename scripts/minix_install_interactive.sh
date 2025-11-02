#!/usr/bin/env bash
# shellcheck disable=SC3010,SC3040,SC3060
set -euo pipefail

# Interactive MINIX installation helper.
# Launches the minix-analysis Docker image with VNC port exposed for manual setup.

# --- Configuration ---
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

ARCH="i386"       # i386 | arm
ISO=""
OUT="metrics/minix"
NAME="minix-install"
# Default assumes 'docker' dir is sibling to 'scripts' dir, e.g., ../docker
DOCKER_DIR_DEFAULT="${SCRIPT_DIR}/../docker"
DOCKER_DIR="${DOCKER_DIR_DEFAULT}"
IMAGE="minix-rc6-i386"
BUILD=1
VNC_PORT=5900
SSH_PORT=2222
METRICS_PORT=9000

usage() {
  cat >&2 <<USAGE
Usage: $0 --iso /path/to/minix.iso [--arch i386|arm] [--out metrics/minix] [--name container_name] \\
          [--docker-dir ${DOCKER_DIR_DEFAULT}] [--image image_name] [--no-build] \\
          [--vnc-port 5900] [--ssh-port 2222] [--metrics-port 9000]

This starts QEMU with VNC (:0) inside a Docker container and publishes host port 5900.
Connect your VNC client to localhost:5900 and complete the MINIX installer.

Port forwarding:
  VNC:       \${VNC_PORT} (localhost) -> 5900 (container) : QEMU VNC console
  SSH:       \${SSH_PORT} (localhost) -> 2222 (container) : Optional SSH access to running VM
  METRICS:   \${METRICS_PORT} (localhost) -> 9000 (container) : Optional metrics server in VM
USAGE
  exit 1
}

# --- Argument Parsing ---
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
    --ssh-port) SSH_PORT="$2"; shift 2;;
    --metrics-port) METRICS_PORT="$2"; shift 2;;
    *) usage;;
  esac
done

if [[ -z "${ISO}" ]]; then
    echo "Error: --iso path must be provided." >&2
    usage
fi

# --- Path Resolution (Portable) ---

# Check for ISO file
if [[ ! -f "${ISO}" ]]; then
    echo "ISO not found: ${ISO}" >&2; exit 2;
fi

# Resolve absolute path for ISO
ISO_ABS="$(cd "$(dirname "${ISO}")" && pwd)/$(basename "${ISO}")"

# Resolve absolute path for Output directory
mkdir -p "${OUT}" # Ensure OUT dir exists before cd'ing
OUT_ABS="$(cd "${OUT}" && pwd)"

MEAS_DIR="${OUT_ABS}/${ARCH}"
RUNTIME_DIR="${MEAS_DIR}/runtime"
mkdir -p "${MEAS_DIR}" "${RUNTIME_DIR}"

# --- Preparation ---

# Prepare runtime directory with ISO copy
echo "==> Preparing runtime dir: ${RUNTIME_DIR}"
cp -f "${ISO_ABS}" "${RUNTIME_DIR}/minix_R3.4.0-rc6.iso"

# Pick Dockerfile based on arch
case "${ARCH}" in
  i386) DOCKERFILE="${DOCKER_DIR}/Dockerfile.i386" ;;
  arm)  DOCKERFILE="${DOCKER_DIR}/Dockerfile.arm" ; IMAGE="${IMAGE/rc6-i386/rc6-arm}" ;;
  *) echo "Unsupported arch: ${ARCH}" >&2; exit 3;;
esac

if [[ ! -f "${DOCKERFILE}" ]]; then
    echo "Error: Dockerfile not found at ${DOCKERFILE}" >&2
    echo "Please verify --docker-dir is correct." >&2
    exit 4
fi

# --- Docker Build ---
if [[ ${BUILD} -eq 1 ]]; then
  echo "==> Building image ${IMAGE} from ${DOCKERFILE}"
  docker build -t "${IMAGE}" -f "${DOCKERFILE}" "${DOCKER_DIR}"
else
  echo "==> Skipping image build for ${IMAGE}"
fi

# --- Docker Run ---
echo "==> Starting interactive installation container: ${NAME}"
echo "    VNC:       localhost:${VNC_PORT} (passwordless)"
echo "    SSH:       localhost:${SSH_PORT} (post-install VM access)"
echo "    METRICS:   localhost:${METRICS_PORT} (post-install VM metrics)"
echo "    ISO:       ${ISO_ABS}"
echo "    Output:    ${OUT_ABS}"

set +e; docker rm -f "${NAME}" >/dev/null 2>&1; set -e

# Publish VNC and optional forwarded ports; mount prepared runtime dir + measurements
docker run --name "${NAME}" \
  -p "${VNC_PORT}":5900 -p "${SSH_PORT}":2222 -p "${METRICS_PORT}":9000 \
  -v "${RUNTIME_DIR}":/minix-runtime \
  -v "${OUT_ABS}":/measurements \
  "${IMAGE}"

echo "Container exited. If installation completed, a larger qcow2 should be present in ${MEAS_DIR}."

