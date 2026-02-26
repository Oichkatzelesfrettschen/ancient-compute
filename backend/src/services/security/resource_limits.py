# Ancient Compute - Resource Limits Configuration
from dataclasses import dataclass


@dataclass
class ResourceLimit:
    """Resource limits for language execution"""
    memory_mb: int
    cpu_percent: int
    timeout_seconds: int
    max_processes: int
    max_files: int
    max_file_size_mb: int
    network_enabled: bool


LANGUAGE_LIMITS: dict[str, ResourceLimit] = {
    "c": ResourceLimit(
        memory_mb=128,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "python": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=5,
        max_processes=1,
        max_files=5,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "haskell": ResourceLimit(
        memory_mb=512,
        cpu_percent=75,
        timeout_seconds=15,
        max_processes=1,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "idris": ResourceLimit(
        memory_mb=512,
        cpu_percent=75,
        timeout_seconds=20,
        max_processes=1,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "lisp": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "assembly": ResourceLimit(
        memory_mb=64,
        cpu_percent=25,
        timeout_seconds=5,
        max_processes=1,
        max_files=5,
        max_file_size_mb=1,
        network_enabled=False
    ),
    "java": ResourceLimit(
        memory_mb=256,
        cpu_percent=50,
        timeout_seconds=15,
        max_processes=5,
        max_files=20,
        max_file_size_mb=5,
        network_enabled=False
    ),
    "systemf": ResourceLimit(
        memory_mb=128,
        cpu_percent=50,
        timeout_seconds=10,
        max_processes=1,
        max_files=10,
        max_file_size_mb=1,
        network_enabled=False
    )
}


def get_resource_limit(language: str) -> ResourceLimit:
    """Get resource limits for a language"""
    return LANGUAGE_LIMITS.get(language.lower(), LANGUAGE_LIMITS["python"])
