# MINIX Interactive Installation (VNC)

Use this guide when the automated installer cannot complete unattended. You will connect via VNC and complete setup.

## Prerequisites
- ISO file: MINIX 3.4.0 RC6 ISO (or variant)
- VNC client (e.g., `vncviewer`, Remmina)
- Docker with permissions to run containers

## Start Interactive Installer
```bash
./scripts/minix_install_interactive.sh \
  --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso \
  --arch i386 \
  --vnc-port 5900
```

Notes:
- The container exposes VNC on `localhost:5900` (QEMU `:0`).
- Logs and qcow2 are written to `metrics/minix/<arch>/` via bind mount.

## Connect via VNC
- Open your VNC client and connect to `localhost:5900`.
- You should see the MINIX installer console.

## Installation Steps (Typical)
1. At the login prompt, type: `root` (no password) and press Enter.
2. At the shell prompt (`#`), type: `setup` and press Enter.
3. Keyboard: accept default (us-std) or choose your layout.
4. Partitioning: select automatic (not expert).
5. Select the first drive; confirm partition destruction (`yes`).
6. Choose full install; accept defaults for sizes and block size.
7. Wait for file copy to complete (2–5 minutes with KVM, longer without).
8. Skip ethernet or configure as desired.
9. When finished, type: `shutdown` to exit.

The container will exit once QEMU exits. If successful, a qcow2 disk image will be present.

## Verify Installation
- Check qcow2 image size under `metrics/minix/<arch>/` (should be >100 MB, not ~193 KB).
- Boot from installed disk (headless): run the orchestrator without ISO boot mode; or repeat interactive script and select disk boot.

### Core Functionality Test (Headless)
After installation, run metrics orchestrator once; it will boot from the installed disk and capture logs. Verify in `boot.log`:
- Presence of login prompt (e.g., `login:`)
- Kernel and scheduler messages
You can also run an interactive container and choose disk boot to manually log in and run:
```
uname -a
ls /
df -h
```

## Next: Metrics and Boot Profiling
Once installation is complete, run the metrics orchestrator to profile boot and collect artifacts:

```bash
./scripts/minix_metrics.sh \
  --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso \
  --arch i386 --iterations 3 --label post-install
```

Artifacts:
- `metrics/minix/<arch>/boot_time.json`
- `metrics/minix/<arch>/resource_timeseries.csv`
- `metrics/minix/<arch>/boot.log`, `qemu-debug.log`

## Troubleshooting
- If VNC shows nothing, ensure host port 5900 is free and your VNC client supports VNC without encryption.
- If qcow2 remains tiny (~193 KB), installation did not complete; rerun and verify each prompt.
- Without `/dev/kvm`, installation and boot are slower (TCG fallback).

## Alternative (No Docker)
You can run the installer directly on the host using the minix-analysis scripts:
```bash
python3 /home/eirikr/Playground/minix-analysis/docker/minix_auto_install.py \
  --iso /home/eirikr/Playground/minix-analysis/docker/minix_R3.4.0rc6-d5e4fc0.iso \
  --disk /home/eirikr/Playground/minix-analysis/docker/minix_installed.qcow2 \
  --size 2G --memory 512M
```
Then boot from disk via QEMU as shown in the minix-analysis guide.

## Verbose Logging
- QEMU debug logs are written to `qemu-debug-*.log` in `metrics/minix/<arch>/` by the minix-analysis run script.
- Serial output is captured in `boot-*.log`. Use these when reporting issues.
