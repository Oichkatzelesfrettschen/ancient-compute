#!/usr/bin/env bash
set -euo pipefail

# Launch QEMU directly on host for users avoiding Docker.
# Supports VNC and serial logging to metrics directory.

ARCH="i386"
DISK=""
ISO=""
OUT="metrics/minix"
MEM="512M"
SMP="2"
USE_KVM=1
VNC_PORT=5900

usage() {
  cat >&2 <<USAGE
Usage: $0 --disk /path/to/minix.qcow2 [--iso /path/to/minix.iso] [--arch i386|arm] [--out metrics/minix] [--mem 512M] [--smp 2] [--no-kvm] [--vnc-port 5900]

Examples:
  # Boot installed disk headless (with VNC)
  $0 --disk metrics/minix/i386/runtime/minix-i386.qcow2

  # Boot ISO for installation
  $0 --iso /path/to/minix.iso --disk metrics/minix/i386/runtime/minix-i386.qcow2
USAGE
  exit 1
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --arch) ARCH="$2"; shift 2;;
    --disk) DISK="$2"; shift 2;;
    --iso) ISO="$2"; shift 2;;
    --out) OUT="$2"; shift 2;;
    --mem) MEM="$2"; shift 2;;
    --smp) SMP="$2"; shift 2;;
    --no-kvm) USE_KVM=0; shift 1;;
    --vnc-port) VNC_PORT="$2"; shift 2;;
    *) usage;;
  esac
done

[[ -n "$DISK" || -n "$ISO" ]] || usage

OUT_ABS="$(readlink -f "$OUT")"
MEAS_DIR="$OUT_ABS/$ARCH"
mkdir -p "$MEAS_DIR"

BOOT_LOG="$MEAS_DIR/boot-$(date -u +%Y%m%dT%H%M%SZ).log"
DBG_LOG="$MEAS_DIR/qemu-debug-$(date -u +%Y%m%dT%H%M%SZ).log"

QEMU_BIN="qemu-system-i386"
if [[ "$ARCH" == "arm" ]]; then
  QEMU_BIN="qemu-system-arm"
fi

OPTS=(
  -m "$MEM" -smp "$SMP"
  -serial file:"$BOOT_LOG"
  -display none -vnc 0.0.0.0:0
  -D "$DBG_LOG"
)

if [[ $USE_KVM -eq 1 && -e /dev/kvm ]]; then
  OPTS+=( -enable-kvm -cpu host )
fi

if [[ -n "$DISK" ]]; then
  OPTS+=( -hda "$DISK" )
fi

if [[ -n "$ISO" ]]; then
  OPTS+=( -cdrom "$ISO" -boot d )
else
  OPTS+=( -boot c )
fi

echo "==> Starting QEMU ($QEMU_BIN). VNC: localhost:${VNC_PORT}"
"$QEMU_BIN" "${OPTS[@]}"

echo "Logs: $BOOT_LOG, $DBG_LOG"

