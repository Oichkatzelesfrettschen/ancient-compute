# MINIX on Host QEMU (No Docker)

For environments where Docker is undesirable, you can boot MINIX directly on the host using QEMU. This integrates with the same metrics folder for consistency.

## Launch on Host
```bash
# Boot installed disk headless (VNC on :0, serial logs captured)
./scripts/minix_host_qemu.sh \
  --disk metrics/minix/i386/runtime/minix-i386.qcow2 \
  --arch i386

# Boot ISO to perform installation interactively (VNC)
./scripts/minix_host_qemu.sh \
  --iso /path/to/minix.iso \
  --disk metrics/minix/i386/runtime/minix-i386.qcow2 \
  --arch i386
```

- Serial output is saved to `metrics/minix/<arch>/boot-*.log`.
- QEMU debug log is saved to `metrics/minix/<arch>/qemu-debug-*.log`.

## Verify Boot
```bash
./scripts/minix_verify_boot.sh metrics/minix/i386/boot.log
```

Exit code 0 indicates markers (login prompt or scheduler) were found.

## Notes
- KVM is used if `/dev/kvm` is available; otherwise, TCG fallback is used (slower).
- Adjust memory and CPU via `--mem` and `--smp` flags.

