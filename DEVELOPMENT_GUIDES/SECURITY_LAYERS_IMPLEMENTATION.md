# 5-Layer Security Architecture Implementation
## Ancient Compute Educational Platform - Production Security

### Executive Summary
This document defines a comprehensive 5-layer defense-in-depth security architecture for executing untrusted code across 8 programming languages. Each layer provides independent security guarantees, ensuring multiple failures would be required for a complete breach.

### Security Layer Overview
```
Layer 5: Read-only Filesystem + tmpfs     [Application Level]
   |
Layer 4: Cgroups v2 Resource Limits       [Resource Control]
   |
Layer 3: Seccomp-BPF Syscall Filtering    [Syscall Level]
   |
Layer 2: gVisor Runtime (runsc)           [Kernel Isolation]
   |
Layer 1: Docker Container Isolation       [Process Level]
```

## Layer 1: Docker Container Isolation

### Base Configuration
```yaml
# docker-compose.security.yml
version: '3.8'

x-security-opts: &security-defaults
  security_opt:
    - no-new-privileges:true
    - apparmor:docker-default
  cap_drop:
    - ALL
  cap_add: []  # No capabilities
  read_only: true
  user: "1000:1000"

services:
  executor-base:
    <<: *security-defaults
    image: ancient-compute/base:latest
    network_mode: none
    pids_limit: 10
    restart: "no"
    stop_grace_period: 1s
    sysctls:
      - net.ipv4.ip_forward=0
      - net.ipv6.conf.all.disable_ipv6=1
      - kernel.dmesg_restrict=1
```

### Docker Daemon Configuration
```json
# /etc/docker/daemon.json
{
  "default-runtime": "runc",
  "runtimes": {
    "runsc": {
      "path": "/usr/local/bin/runsc",
      "runtimeArgs": [
        "--platform=ptrace",
        "--network=none",
        "--overlay",
        "--file-access=shared"
      ]
    }
  },
  "default-address-pools": [
    {"base": "172.240.0.0/16", "size": 24}
  ],
  "icc": false,
  "userland-proxy": false,
  "no-new-privileges": true,
  "selinux-enabled": false,
  "userns-remap": "default",
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "10m",
    "max-file": "3"
  }
}
```

### Container Hardening Script
```bash
#!/bin/bash
# backend/services/security/harden_container.sh

set -euo pipefail

# Function to create hardened container
create_hardened_container() {
    local LANGUAGE=$1
    local IMAGE=$2

    docker run \
        --rm \
        --detach \
        --name "executor-${LANGUAGE}" \
        --hostname "sandbox" \
        --network none \
        --user 1000:1000 \
        --read-only \
        --tmpfs /tmp:rw,noexec,nosuid,size=32m \
        --tmpfs /var/tmp:rw,noexec,nosuid,size=8m \
        --cap-drop ALL \
        --security-opt no-new-privileges:true \
        --security-opt apparmor:docker-default \
        --security-opt seccomp=/etc/docker/seccomp/${LANGUAGE}.json \
        --pids-limit 10 \
        --memory 128m \
        --memory-swap 128m \
        --cpus 0.5 \
        --cpu-shares 512 \
        --ulimit nofile=64:64 \
        --ulimit nproc=10:10 \
        --ulimit core=0:0 \
        --env PATH=/usr/local/bin:/usr/bin:/bin \
        ${IMAGE}
}
```

## Layer 2: gVisor Runtime Integration

### gVisor Installation and Configuration
```bash
#!/bin/bash
# backend/services/security/install_gvisor.sh

set -euo pipefail

# Download and install gVisor
ARCH=$(uname -m)
URL="https://storage.googleapis.com/gvisor/releases/release/latest/${ARCH}"

wget ${URL}/runsc ${URL}/runsc.sha512 \
    ${URL}/containerd-shim-runsc-v1 ${URL}/containerd-shim-runsc-v1.sha512

sha512sum -c runsc.sha512 \
    -c containerd-shim-runsc-v1.sha512

chmod a+rx runsc containerd-shim-runsc-v1
sudo mv runsc containerd-shim-runsc-v1 /usr/local/bin/

# Configure runsc
sudo tee /etc/runsc/config.toml > /dev/null <<EOF
[runsc_config]
  # Platform
  platform = "ptrace"

  # Network
  network = "none"

  # Filesystem
  overlay = true
  file-access = "shared"

  # Security
  rootless = true

  # Debugging
  debug = false
  debug-log = "/var/log/runsc/"

  # Performance
  cpu-num-from-quota = true

  # Strace
  strace = false
EOF
```

### Language-Specific gVisor Policies
```python
# backend/services/security/gvisor_config.py
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class GVisorConfig:
    platform: str = "ptrace"  # or "kvm" for better performance
    network: str = "none"
    overlay: bool = True
    file_access: str = "shared"
    vfs2: bool = True
    rootless: bool = True
    debug: bool = False
    strace: bool = False
    profile: Optional[str] = None

LANGUAGE_GVISOR_CONFIGS: Dict[str, GVisorConfig] = {
    "c": GVisorConfig(
        platform="ptrace",
        file_access="exclusive",
        profile="strict"
    ),
    "assembly": GVisorConfig(
        platform="ptrace",
        file_access="exclusive",
        profile="strict"
    ),
    "systemf": GVisorConfig(
        platform="ptrace",
        file_access="exclusive",
        profile="strict"
    ),
    "python": GVisorConfig(
        platform="ptrace",
        file_access="shared",
        profile="default"
    ),
    "haskell": GVisorConfig(
        platform="ptrace",
        file_access="shared",
        profile="default"
    ),
    "idris": GVisorConfig(
        platform="ptrace",
        file_access="shared",
        profile="default"
    ),
    "lisp": GVisorConfig(
        platform="ptrace",
        file_access="shared",
        profile="default"
    ),
    "java": GVisorConfig(
        platform="ptrace",
        file_access="shared",
        profile="default"
    )
}

def get_runsc_flags(language: str) -> List[str]:
    config = LANGUAGE_GVISOR_CONFIGS.get(language, GVisorConfig())

    flags = [
        f"--platform={config.platform}",
        f"--network={config.network}",
        f"--file-access={config.file_access}"
    ]

    if config.overlay:
        flags.append("--overlay")
    if config.vfs2:
        flags.append("--vfs2")
    if config.rootless:
        flags.append("--rootless")
    if config.debug:
        flags.append("--debug")
        flags.append("--debug-log=/var/log/runsc/")
    if config.strace:
        flags.append("--strace")
    if config.profile:
        flags.append(f"--profile={config.profile}")

    return flags
```

## Layer 3: Seccomp-BPF Syscall Filtering

### Base Seccomp Profile
```json
# backend/services/security/seccomp/base.json
{
  "defaultAction": "SCMP_ACT_ERRNO",
  "defaultErrnoRet": 1,
  "archMap": [
    {
      "architecture": "SCMP_ARCH_X86_64",
      "subArchitectures": ["SCMP_ARCH_X86", "SCMP_ARCH_X32"]
    }
  ],
  "syscalls": [
    {
      "names": [
        "read", "write", "close", "fstat", "lseek",
        "mmap", "mprotect", "munmap", "brk", "rt_sigaction",
        "rt_sigprocmask", "ioctl", "pread64", "pwrite64",
        "access", "pipe", "select", "sched_yield", "mremap",
        "msync", "mincore", "madvise", "shmget", "shmat",
        "dup", "dup2", "nanosleep", "getitimer", "setitimer",
        "getpid", "sendfile", "socket", "connect", "accept",
        "getuid", "getgid", "geteuid", "getegid", "setuid",
        "setgid", "getgroups", "setgroups", "uname",
        "fcntl", "flock", "fsync", "fdatasync", "truncate",
        "ftruncate", "getdents", "getcwd", "chdir", "fchdir",
        "readlink", "fchmod", "fchown", "umask",
        "gettimeofday", "getrlimit", "getrusage", "sysinfo",
        "times", "ptrace", "syslog", "capget", "capset",
        "prctl", "arch_prctl", "mount", "umount2", "swapon",
        "swapoff", "reboot", "sethostname", "setdomainname",
        "ioperm", "iopl", "init_module", "delete_module",
        "quotactl", "personality", "afs_syscall", "setfsuid",
        "setfsgid", "_llseek", "getdents64", "_newselect",
        "fadvise64", "exit_group", "lookup_dcookie",
        "epoll_create", "epoll_ctl", "epoll_wait",
        "remap_file_pages", "getdents64", "set_tid_address",
        "restart_syscall", "semtimedop", "fadvise64",
        "timer_create", "timer_settime", "timer_gettime",
        "timer_getoverrun", "timer_delete", "clock_settime",
        "clock_gettime", "clock_getres", "clock_nanosleep",
        "exit_group", "epoll_wait", "epoll_ctl", "tgkill",
        "utimes", "mbind", "set_mempolicy", "get_mempolicy",
        "mq_open", "mq_unlink", "mq_timedsend",
        "mq_timedreceive", "mq_notify", "mq_getsetattr",
        "waitid", "add_key", "request_key", "keyctl",
        "ioprio_set", "ioprio_get", "inotify_init",
        "inotify_add_watch", "inotify_rm_watch", "mkdirat",
        "mknodat", "fchownat", "futimesat", "newfstatat",
        "unlinkat", "renameat", "linkat", "symlinkat",
        "readlinkat", "fchmodat", "faccessat", "pselect6",
        "ppoll", "unshare", "set_robust_list",
        "get_robust_list", "splice", "tee", "sync_file_range",
        "vmsplice", "move_pages", "utimensat", "epoll_pwait",
        "signalfd", "timerfd_create", "eventfd", "fallocate",
        "timerfd_settime", "timerfd_gettime", "accept4",
        "signalfd4", "eventfd2", "epoll_create1", "dup3",
        "pipe2", "inotify_init1", "preadv", "pwritev",
        "rt_tgsigqueueinfo", "perf_event_open", "recvmmsg",
        "fanotify_init", "fanotify_mark", "prlimit64"
      ],
      "action": "SCMP_ACT_ALLOW"
    }
  ]
}
```

### C Language Seccomp Profile
```json
# backend/services/security/seccomp/c.json
{
  "defaultAction": "SCMP_ACT_ERRNO",
  "defaultErrnoRet": 1,
  "architectures": ["SCMP_ARCH_X86_64"],
  "syscalls": [
    {
      "names": [
        "read", "write", "open", "openat", "close",
        "stat", "fstat", "lstat", "lseek", "mmap",
        "mprotect", "munmap", "brk", "rt_sigaction",
        "rt_sigprocmask", "rt_sigreturn", "ioctl",
        "pread64", "pwrite64", "readv", "writev",
        "access", "pipe", "select", "sched_yield",
        "mremap", "msync", "mincore", "madvise",
        "dup", "dup2", "pause", "nanosleep",
        "getitimer", "alarm", "setitimer", "getpid",
        "getppid", "getuid", "geteuid", "getgid",
        "getegid", "getgroups", "setgroups",
        "exit", "exit_group", "wait4", "uname",
        "fcntl", "flock", "fsync", "fdatasync",
        "truncate", "ftruncate", "getcwd", "chdir",
        "fchdir", "fchmod", "fchown", "umask",
        "gettimeofday", "getrlimit", "getrusage",
        "times", "futex", "sched_getaffinity",
        "set_tid_address", "clock_gettime",
        "clock_getres", "arch_prctl", "set_robust_list"
      ],
      "action": "SCMP_ACT_ALLOW"
    },
    {
      "names": [
        "clone", "fork", "vfork", "execve", "execveat",
        "kill", "tkill", "tgkill", "socket", "bind",
        "connect", "listen", "accept", "accept4",
        "ptrace", "process_vm_readv", "process_vm_writev",
        "mount", "umount", "umount2", "pivot_root",
        "chroot", "setuid", "setgid", "setreuid",
        "setregid", "setresuid", "setresgid",
        "personality", "prctl", "capget", "capset"
      ],
      "action": "SCMP_ACT_KILL_PROCESS"
    }
  ]
}
```

### Python Seccomp Profile
```json
# backend/services/security/seccomp/python.json
{
  "defaultAction": "SCMP_ACT_ERRNO",
  "defaultErrnoRet": 1,
  "architectures": ["SCMP_ARCH_X86_64"],
  "syscalls": [
    {
      "names": [
        "read", "write", "open", "openat", "close",
        "stat", "fstat", "lstat", "poll", "lseek",
        "mmap", "mprotect", "munmap", "brk",
        "rt_sigaction", "rt_sigprocmask", "rt_sigreturn",
        "ioctl", "pread64", "pwrite64", "readv", "writev",
        "access", "pipe", "pipe2", "select", "pselect6",
        "sched_yield", "mremap", "msync", "mincore",
        "madvise", "dup", "dup2", "dup3", "pause",
        "nanosleep", "getitimer", "alarm", "setitimer",
        "getpid", "getppid", "getuid", "geteuid",
        "getgid", "getegid", "getgroups", "exit",
        "exit_group", "uname", "fcntl", "flock",
        "fsync", "fdatasync", "truncate", "ftruncate",
        "getdents", "getdents64", "getcwd", "chdir",
        "fchdir", "fchmod", "umask", "gettimeofday",
        "getrlimit", "getrusage", "sysinfo", "times",
        "futex", "sched_getaffinity", "sched_setaffinity",
        "set_tid_address", "clock_gettime", "clock_getres",
        "clock_nanosleep", "epoll_create", "epoll_create1",
        "epoll_ctl", "epoll_wait", "epoll_pwait",
        "eventfd", "eventfd2", "signalfd", "signalfd4",
        "timerfd_create", "timerfd_settime", "timerfd_gettime",
        "getrandom", "memfd_create", "prlimit64"
      ],
      "action": "SCMP_ACT_ALLOW"
    },
    {
      "names": [
        "clone", "fork", "vfork", "execve", "execveat",
        "kill", "socket", "bind", "connect", "listen",
        "accept", "sendto", "recvfrom", "sendmsg",
        "recvmsg", "shutdown", "mount", "umount",
        "chroot", "pivot_root", "setuid", "setgid",
        "ptrace", "process_vm_readv", "process_vm_writev"
      ],
      "action": "SCMP_ACT_KILL_PROCESS"
    }
  ]
}
```

### Assembly Seccomp Profile (Most Restrictive)
```json
# backend/services/security/seccomp/assembly.json
{
  "defaultAction": "SCMP_ACT_KILL_PROCESS",
  "architectures": ["SCMP_ARCH_X86_64"],
  "syscalls": [
    {
      "names": [
        "read", "write", "exit", "exit_group",
        "brk", "mmap", "munmap", "close"
      ],
      "action": "SCMP_ACT_ALLOW"
    },
    {
      "names": ["open", "openat"],
      "action": "SCMP_ACT_ALLOW",
      "args": [
        {
          "index": 1,
          "value": 0,
          "op": "SCMP_CMP_EQ",
          "comment": "O_RDONLY"
        }
      ]
    }
  ]
}
```

### Seccomp Profile Loader
```python
# backend/services/security/seccomp_loader.py
import json
import os
from typing import Dict, Any
import ctypes
import ctypes.util

# Load libseccomp
_lib = ctypes.CDLL(ctypes.util.find_library("seccomp"))

class SeccompLoader:
    PROFILES_DIR = "/etc/docker/seccomp"

    @staticmethod
    def load_profile(language: str) -> Dict[str, Any]:
        profile_path = os.path.join(
            SeccompLoader.PROFILES_DIR,
            f"{language}.json"
        )

        if not os.path.exists(profile_path):
            # Fall back to base profile
            profile_path = os.path.join(
                SeccompLoader.PROFILES_DIR,
                "base.json"
            )

        with open(profile_path, 'r') as f:
            return json.load(f)

    @staticmethod
    def validate_profile(profile: Dict[str, Any]) -> bool:
        required_keys = ["defaultAction", "architectures", "syscalls"]

        for key in required_keys:
            if key not in profile:
                return False

        # Validate syscall rules
        for rule in profile.get("syscalls", []):
            if "names" not in rule or "action" not in rule:
                return False

        return True

    @staticmethod
    def generate_docker_seccomp(language: str) -> str:
        profile = SeccompLoader.load_profile(language)

        if not SeccompLoader.validate_profile(profile):
            raise ValueError(f"Invalid seccomp profile for {language}")

        # Convert to Docker-compatible format
        docker_profile = {
            "defaultAction": profile["defaultAction"],
            "architectures": profile.get("architectures", ["SCMP_ARCH_X86_64"]),
            "syscalls": []
        }

        for rule in profile["syscalls"]:
            docker_rule = {
                "names": rule["names"],
                "action": rule["action"]
            }

            if "args" in rule:
                docker_rule["args"] = rule["args"]

            docker_profile["syscalls"].append(docker_rule)

        output_path = f"/tmp/seccomp_{language}.json"
        with open(output_path, 'w') as f:
            json.dump(docker_profile, f, indent=2)

        return output_path
```

## Layer 4: Cgroups v2 Resource Limits

### Cgroups Configuration Manager
```python
# backend/services/security/cgroups_manager.py
import os
import subprocess
from dataclasses import dataclass
from typing import Optional

@dataclass
class CgroupsConfig:
    memory_limit_bytes: int
    memory_swap_limit_bytes: int
    cpu_quota_us: int
    cpu_period_us: int
    pids_max: int
    io_max_bandwidth_bytes: Optional[int] = None
    io_max_iops: Optional[int] = None

class CgroupsV2Manager:
    CGROUP_ROOT = "/sys/fs/cgroup"

    def __init__(self, group_name: str):
        self.group_name = group_name
        self.cgroup_path = os.path.join(self.CGROUP_ROOT, "ancient_compute", group_name)

    def create_cgroup(self) -> bool:
        try:
            os.makedirs(self.cgroup_path, exist_ok=True)

            # Enable controllers
            controllers = "+cpu +memory +pids +io"
            subtree_control = os.path.join(
                os.path.dirname(self.cgroup_path),
                "cgroup.subtree_control"
            )

            with open(subtree_control, 'w') as f:
                f.write(controllers)

            return True
        except Exception as e:
            print(f"Failed to create cgroup: {e}")
            return False

    def apply_limits(self, config: CgroupsConfig):
        # Memory limits
        self._write_cgroup_file("memory.max", str(config.memory_limit_bytes))
        self._write_cgroup_file("memory.swap.max", str(config.memory_swap_limit_bytes))

        # CPU limits
        cpu_max = f"{config.cpu_quota_us} {config.cpu_period_us}"
        self._write_cgroup_file("cpu.max", cpu_max)

        # PID limits
        self._write_cgroup_file("pids.max", str(config.pids_max))

        # IO limits (if specified)
        if config.io_max_bandwidth_bytes:
            # Format: "major:minor rbps=<bytes> wbps=<bytes>"
            io_max = f"8:0 rbps={config.io_max_bandwidth_bytes} wbps={config.io_max_bandwidth_bytes}"
            self._write_cgroup_file("io.max", io_max)

    def add_process(self, pid: int):
        self._write_cgroup_file("cgroup.procs", str(pid))

    def cleanup(self):
        try:
            # Kill all processes in the cgroup
            self._write_cgroup_file("cgroup.kill", "1")

            # Remove the cgroup
            os.rmdir(self.cgroup_path)
        except Exception:
            pass

    def _write_cgroup_file(self, filename: str, value: str):
        filepath = os.path.join(self.cgroup_path, filename)
        with open(filepath, 'w') as f:
            f.write(value)

    def get_memory_usage(self) -> int:
        return int(self._read_cgroup_file("memory.current"))

    def get_cpu_usage(self) -> int:
        cpu_stat = self._read_cgroup_file("cpu.stat")
        for line in cpu_stat.split('\n'):
            if line.startswith("usage_usec"):
                return int(line.split()[1])
        return 0

    def _read_cgroup_file(self, filename: str) -> str:
        filepath = os.path.join(self.cgroup_path, filename)
        with open(filepath, 'r') as f:
            return f.read().strip()
```

### Language-Specific Cgroup Limits
```python
# backend/services/security/cgroup_limits.py
from typing import Dict
from .cgroups_manager import CgroupsConfig

LANGUAGE_CGROUP_LIMITS: Dict[str, CgroupsConfig] = {
    "c": CgroupsConfig(
        memory_limit_bytes=128 * 1024 * 1024,      # 128MB
        memory_swap_limit_bytes=128 * 1024 * 1024,  # No swap
        cpu_quota_us=50000,                         # 50% of one CPU
        cpu_period_us=100000,
        pids_max=10,
        io_max_bandwidth_bytes=10 * 1024 * 1024,    # 10MB/s
        io_max_iops=100
    ),
    "python": CgroupsConfig(
        memory_limit_bytes=256 * 1024 * 1024,
        memory_swap_limit_bytes=256 * 1024 * 1024,
        cpu_quota_us=50000,
        cpu_period_us=100000,
        pids_max=5,
        io_max_bandwidth_bytes=5 * 1024 * 1024,
        io_max_iops=50
    ),
    "assembly": CgroupsConfig(
        memory_limit_bytes=64 * 1024 * 1024,
        memory_swap_limit_bytes=64 * 1024 * 1024,
        cpu_quota_us=25000,  # 25% of one CPU
        cpu_period_us=100000,
        pids_max=1,
        io_max_bandwidth_bytes=1 * 1024 * 1024,
        io_max_iops=10
    ),
    "haskell": CgroupsConfig(
        memory_limit_bytes=512 * 1024 * 1024,
        memory_swap_limit_bytes=512 * 1024 * 1024,
        cpu_quota_us=75000,  # 75% of one CPU
        cpu_period_us=100000,
        pids_max=20,
        io_max_bandwidth_bytes=20 * 1024 * 1024,
        io_max_iops=200
    ),
    "idris": CgroupsConfig(
        memory_limit_bytes=512 * 1024 * 1024,
        memory_swap_limit_bytes=512 * 1024 * 1024,
        cpu_quota_us=75000,
        cpu_period_us=100000,
        pids_max=20,
        io_max_bandwidth_bytes=20 * 1024 * 1024,
        io_max_iops=200
    ),
    "lisp": CgroupsConfig(
        memory_limit_bytes=256 * 1024 * 1024,
        memory_swap_limit_bytes=256 * 1024 * 1024,
        cpu_quota_us=50000,
        cpu_period_us=100000,
        pids_max=10,
        io_max_bandwidth_bytes=10 * 1024 * 1024,
        io_max_iops=100
    ),
    "java": CgroupsConfig(
        memory_limit_bytes=256 * 1024 * 1024,
        memory_swap_limit_bytes=256 * 1024 * 1024,
        cpu_quota_us=50000,
        cpu_period_us=100000,
        pids_max=20,  # JVM threads
        io_max_bandwidth_bytes=10 * 1024 * 1024,
        io_max_iops=100
    ),
    "systemf": CgroupsConfig(
        memory_limit_bytes=128 * 1024 * 1024,
        memory_swap_limit_bytes=128 * 1024 * 1024,
        cpu_quota_us=50000,
        cpu_period_us=100000,
        pids_max=5,
        io_max_bandwidth_bytes=5 * 1024 * 1024,
        io_max_iops=50
    )
}
```

### Cgroup Monitoring
```bash
#!/bin/bash
# backend/services/security/monitor_cgroups.sh

CGROUP_ROOT="/sys/fs/cgroup/ancient_compute"

monitor_cgroup() {
    local LANGUAGE=$1
    local CGROUP_PATH="${CGROUP_ROOT}/${LANGUAGE}"

    if [ ! -d "$CGROUP_PATH" ]; then
        echo "Cgroup for $LANGUAGE not found"
        return 1
    fi

    echo "=== Cgroup Monitor: $LANGUAGE ==="

    # Memory usage
    echo -n "Memory: "
    cat "${CGROUP_PATH}/memory.current" | numfmt --to=iec

    # CPU usage
    echo -n "CPU Usage: "
    grep usage_usec "${CGROUP_PATH}/cpu.stat" | awk '{print $2/1000000 " seconds"}'

    # Process count
    echo -n "Processes: "
    wc -l < "${CGROUP_PATH}/cgroup.procs"

    # IO stats
    if [ -f "${CGROUP_PATH}/io.stat" ]; then
        echo "IO Stats:"
        head -5 "${CGROUP_PATH}/io.stat"
    fi
}

# Monitor all language cgroups
for lang in c python haskell idris lisp assembly java systemf; do
    monitor_cgroup "$lang"
    echo
done
```

## Layer 5: Read-only Filesystem with tmpfs

### Filesystem Layout Manager
```python
# backend/services/security/filesystem_manager.py
import os
import tempfile
import shutil
from typing import List, Dict, Tuple
from contextlib import contextmanager

class FilesystemManager:
    def __init__(self, language: str):
        self.language = language
        self.base_path = f"/var/lib/ancient_compute/sandboxes/{language}"

    def prepare_sandbox(self) -> str:
        # Create sandbox directory structure
        sandbox_id = os.urandom(8).hex()
        sandbox_path = os.path.join(self.base_path, sandbox_id)

        # Create directory structure
        dirs = [
            f"{sandbox_path}/workspace",  # User code
            f"{sandbox_path}/tmp",         # Temporary files
            f"{sandbox_path}/bin",         # Binaries (read-only)
            f"{sandbox_path}/lib",         # Libraries (read-only)
        ]

        for dir_path in dirs:
            os.makedirs(dir_path, mode=0o755, exist_ok=True)

        # Set permissions
        os.chmod(f"{sandbox_path}/workspace", 0o700)
        os.chmod(f"{sandbox_path}/tmp", 0o1777)

        return sandbox_path

    def create_overlay_mount(self, sandbox_path: str) -> Dict[str, str]:
        # Create OverlayFS mount points
        lower_dir = "/usr"  # Read-only system files
        upper_dir = f"{sandbox_path}/tmp/upper"
        work_dir = f"{sandbox_path}/tmp/work"
        merged_dir = f"{sandbox_path}/usr"

        os.makedirs(upper_dir, exist_ok=True)
        os.makedirs(work_dir, exist_ok=True)
        os.makedirs(merged_dir, exist_ok=True)

        return {
            "lower": lower_dir,
            "upper": upper_dir,
            "work": work_dir,
            "merged": merged_dir
        }

    def apply_filesystem_limits(self, sandbox_path: str):
        # Apply filesystem quotas
        quota_cmd = f"setquota -u sandbox_{self.language} 0 50M 0 100 {sandbox_path}"
        os.system(quota_cmd)

    def cleanup_sandbox(self, sandbox_path: str):
        # Unmount any overlay filesystems
        os.system(f"umount {sandbox_path}/usr 2>/dev/null")

        # Remove sandbox directory
        shutil.rmtree(sandbox_path, ignore_errors=True)

    @contextmanager
    def sandbox_context(self):
        sandbox_path = self.prepare_sandbox()
        try:
            yield sandbox_path
        finally:
            self.cleanup_sandbox(sandbox_path)
```

### tmpfs Configuration
```python
# backend/services/security/tmpfs_config.py
from dataclasses import dataclass
from typing import Dict, List

@dataclass
class TmpfsMount:
    path: str
    size: str
    mode: str
    options: List[str]

LANGUAGE_TMPFS_MOUNTS: Dict[str, List[TmpfsMount]] = {
    "c": [
        TmpfsMount("/tmp", "32M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/var/tmp", "8M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/run", "4M", "755", ["noexec", "nosuid", "nodev"])
    ],
    "python": [
        TmpfsMount("/tmp", "64M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/var/tmp", "16M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/home/runner/.cache", "32M", "700", ["noexec", "nosuid", "nodev"])
    ],
    "haskell": [
        TmpfsMount("/tmp", "128M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/var/tmp", "32M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/home/runner/.cabal", "64M", "700", ["noexec", "nosuid", "nodev"])
    ],
    "assembly": [
        TmpfsMount("/tmp", "16M", "1777", ["noexec", "nosuid", "nodev"]),
        TmpfsMount("/var/tmp", "4M", "1777", ["noexec", "nosuid", "nodev"])
    ]
}

def get_docker_tmpfs_config(language: str) -> Dict[str, str]:
    mounts = LANGUAGE_TMPFS_MOUNTS.get(language, [])
    tmpfs_config = {}

    for mount in mounts:
        options = ",".join([
            f"size={mount.size}",
            f"mode={mount.mode}",
            *mount.options
        ])
        tmpfs_config[mount.path] = options

    return tmpfs_config
```

## Security Validation Framework

### Security Test Suite
```python
# backend/services/security/tests/test_security_layers.py
import pytest
import docker
import subprocess
import os
from typing import List

class SecurityLayerTests:

    @pytest.fixture
    def docker_client(self):
        return docker.from_env()

    def test_network_isolation(self, docker_client):
        """Test that containers cannot access network"""
        container = docker_client.containers.run(
            "ancient-compute/python:latest",
            "python -c 'import socket; s=socket.socket(); s.connect((\"8.8.8.8\", 53))'",
            network_mode="none",
            detach=False,
            remove=True
        )
        assert container.exit_code != 0

    def test_filesystem_readonly(self, docker_client):
        """Test that root filesystem is read-only"""
        container = docker_client.containers.run(
            "ancient-compute/c:latest",
            "touch /test_file",
            read_only=True,
            detach=False,
            remove=True
        )
        assert container.exit_code != 0

    def test_seccomp_restrictions(self, docker_client):
        """Test that dangerous syscalls are blocked"""
        test_cases = [
            ("c", "#include <unistd.h>\nint main() { fork(); return 0; }"),
            ("python", "import os; os.fork()"),
            ("assembly", "mov rax, 57\nsyscall")  # fork syscall
        ]

        for language, code in test_cases:
            # Execute code that attempts forbidden syscall
            result = self._execute_code(language, code)
            assert result["status"] in ["SECURITY_VIOLATION", "RUNTIME_ERROR"]

    def test_resource_limits(self, docker_client):
        """Test that resource limits are enforced"""
        # Test memory limit
        memory_bomb = """
        #include <stdlib.h>
        int main() {
            while(1) malloc(1024*1024);
            return 0;
        }
        """
        result = self._execute_code("c", memory_bomb)
        assert result["status"] in ["MEMORY_EXCEEDED", "RUNTIME_ERROR"]

        # Test CPU limit (infinite loop should timeout)
        cpu_bomb = "while True: pass"
        result = self._execute_code("python", cpu_bomb)
        assert result["status"] == "TIMEOUT"

    def test_gvisor_isolation(self):
        """Test gVisor runtime isolation"""
        # Check if gVisor is properly configured
        result = subprocess.run(
            ["docker", "run", "--runtime=runsc", "--rm",
             "ancient-compute/c:latest", "echo", "test"],
            capture_output=True
        )
        assert result.returncode == 0

    def test_privilege_escalation_prevention(self, docker_client):
        """Test that privilege escalation is prevented"""
        escalation_attempts = [
            "python -c 'import os; os.setuid(0)'",
            "gcc -o /tmp/test test.c && chmod u+s /tmp/test"
        ]

        for cmd in escalation_attempts:
            container = docker_client.containers.run(
                "ancient-compute/python:latest",
                cmd,
                user="1000:1000",
                security_opt=["no-new-privileges:true"],
                detach=False,
                remove=True
            )
            assert container.exit_code != 0

class PenetrationTests:
    """Simulated attacks to validate security"""

    def test_container_escape_attempt(self):
        """Test various container escape techniques"""
        escape_vectors = [
            # Attempting to access Docker socket
            "ls -la /var/run/docker.sock",
            # Attempting to access host filesystem
            "cat /proc/1/environ",
            # Attempting to load kernel modules
            "insmod /tmp/evil.ko"
        ]

        for vector in escape_vectors:
            result = self._execute_in_sandbox("c", vector)
            assert result["exit_code"] != 0

    def test_denial_of_service_prevention(self):
        """Test DoS attack prevention"""
        dos_attacks = [
            # Fork bomb
            ":(){ :|:& };:",
            # Memory exhaustion
            "dd if=/dev/zero of=/tmp/bigfile bs=1G count=10",
            # CPU exhaustion
            "yes > /dev/null &" * 100
        ]

        for attack in dos_attacks:
            result = self._execute_in_sandbox("bash", attack)
            # Should either fail or be killed by resource limits
            assert result["terminated_by_limit"] == True
```

## Threat Model and Attack Surface Analysis

### Threat Categories

1. **Code Execution Threats**
   - Arbitrary code execution
   - Shellcode injection
   - Command injection
   - Path traversal

2. **Resource Exhaustion**
   - Memory bombs
   - Fork bombs
   - CPU spinning
   - Disk filling

3. **Information Disclosure**
   - File system enumeration
   - Process listing
   - Network scanning
   - Environment variable leakage

4. **Privilege Escalation**
   - SUID/SGID exploitation
   - Capability abuse
   - Kernel exploitation
   - Container escape

5. **Side-Channel Attacks**
   - Timing attacks
   - Cache attacks
   - Speculative execution

### Mitigation Matrix

| Threat | Layer 1 | Layer 2 | Layer 3 | Layer 4 | Layer 5 |
|--------|---------|---------|---------|---------|---------|
| Code Execution | Process isolation | Kernel isolation | Syscall filtering | N/A | Read-only FS |
| Resource Exhaustion | Basic limits | Resource isolation | N/A | Hard limits | Quota enforcement |
| Info Disclosure | Namespace isolation | Virtual kernel | Limited syscalls | N/A | Restricted paths |
| Privilege Escalation | User namespaces | Prevented | Capability filtering | N/A | No SUID |
| Side-Channel | Limited | Timing normalization | N/A | CPU isolation | N/A |

## Security Monitoring and Alerting

```python
# backend/services/security/monitor.py
import logging
import json
from datetime import datetime
from typing import Dict, Any
from prometheus_client import Counter, Histogram

# Metrics
security_violations = Counter(
    'security_violations_total',
    'Total security violations detected',
    ['language', 'violation_type']
)

execution_attempts = Counter(
    'execution_attempts_total',
    'Total code execution attempts',
    ['language', 'status']
)

class SecurityMonitor:
    def __init__(self):
        self.logger = logging.getLogger('security')

    def log_violation(self, language: str, violation_type: str, details: Dict[str, Any]):
        security_violations.labels(
            language=language,
            violation_type=violation_type
        ).inc()

        self.logger.warning(json.dumps({
            "event": "security_violation",
            "timestamp": datetime.utcnow().isoformat(),
            "language": language,
            "violation_type": violation_type,
            "details": details
        }))

    def log_execution(self, language: str, status: str, metadata: Dict[str, Any]):
        execution_attempts.labels(
            language=language,
            status=status
        ).inc()

        self.logger.info(json.dumps({
            "event": "code_execution",
            "timestamp": datetime.utcnow().isoformat(),
            "language": language,
            "status": status,
            "metadata": metadata
        }))
```

## Deployment Script

```bash
#!/bin/bash
# backend/services/security/deploy_security.sh

set -euo pipefail

echo "Deploying 5-layer security architecture..."

# 1. Install gVisor
echo "Installing gVisor runtime..."
./install_gvisor.sh

# 2. Configure Docker daemon
echo "Configuring Docker daemon..."
sudo cp daemon.json /etc/docker/daemon.json
sudo systemctl restart docker

# 3. Deploy seccomp profiles
echo "Installing seccomp profiles..."
sudo mkdir -p /etc/docker/seccomp
for lang in c python haskell idris lisp assembly java systemf; do
    sudo cp "seccomp/${lang}.json" "/etc/docker/seccomp/"
done

# 4. Setup cgroups v2
echo "Configuring cgroups v2..."
if [ ! -d "/sys/fs/cgroup/ancient_compute" ]; then
    sudo mkdir -p /sys/fs/cgroup/ancient_compute
    echo "+cpu +memory +pids +io" | sudo tee /sys/fs/cgroup/ancient_compute/cgroup.subtree_control
fi

# 5. Build secure containers
echo "Building secure containers..."
for lang in c python haskell idris lisp assembly java systemf; do
    docker build -t "ancient-compute/${lang}:latest" "containers/${lang}/"
done

# 6. Test security layers
echo "Running security validation tests..."
pytest tests/test_security_layers.py -v

echo "Security deployment complete!"
```
