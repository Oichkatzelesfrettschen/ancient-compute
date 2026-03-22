"""Unit tests for DockerManager backend selection, container config, and status report.

Docker daemon is NOT required.  We mock the docker Python package so all
tests run offline and do not mutate the singleton state permanently.
"""

from __future__ import annotations

import platform
from unittest.mock import patch

import pytest

from backend.src.services.docker_manager import (
    BackendInfo,
    DockerManager,
    ExecutionBackend,
    get_docker_manager,
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------


def _reset_singleton() -> None:
    """Clear the DockerManager singleton so each test gets a fresh instance."""
    DockerManager._instance = None
    DockerManager._initialized = False


# ---------------------------------------------------------------------------
# Singleton behaviour
# ---------------------------------------------------------------------------


class TestDockerManagerSingleton:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def test_same_instance_returned_twice(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as mock_docker,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mock_rp,
        ):
            mock_docker.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "mocked")
            mock_rp.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "mocked")
            a = DockerManager()
            b = DockerManager()
            assert a is b

    def test_get_docker_manager_returns_singleton(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as mock_docker,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mock_rp,
        ):
            mock_docker.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "mocked")
            mock_rp.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "mocked")
            a = get_docker_manager()
            b = get_docker_manager()
            assert a is b


# ---------------------------------------------------------------------------
# Backend selection
# ---------------------------------------------------------------------------


class TestGetBackendForLanguage:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def _make_manager(self, docker_ok: bool, rp_ok: bool) -> DockerManager:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(
                ExecutionBackend.DOCKER,
                docker_ok,
                "ok" if docker_ok else "unavailable",
                capabilities=(
                    ["c", "python", "haskell", "java", "lisp", "idris", "assembly", "systemf"]
                    if docker_ok
                    else []
                ),
            )
            mr.return_value = BackendInfo(
                ExecutionBackend.RESTRICTED_PYTHON,
                rp_ok,
                "ok" if rp_ok else "unavailable",
                capabilities=["python"] if rp_ok else [],
            )
            return DockerManager()

    def test_docker_available_returns_docker_for_c(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=False)
        assert mgr.get_backend_for_language("c") == ExecutionBackend.DOCKER

    def test_docker_available_returns_docker_for_python(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=True)
        assert mgr.get_backend_for_language("python") == ExecutionBackend.DOCKER

    def test_docker_unavailable_python_falls_back_to_restricted(self) -> None:
        mgr = self._make_manager(docker_ok=False, rp_ok=True)
        assert mgr.get_backend_for_language("python") == ExecutionBackend.RESTRICTED_PYTHON

    def test_docker_unavailable_python_falls_back_to_subprocess_when_no_rp(self) -> None:
        mgr = self._make_manager(docker_ok=False, rp_ok=False)
        assert mgr.get_backend_for_language("python") == ExecutionBackend.SUBPROCESS

    def test_docker_unavailable_c_returns_unavailable(self) -> None:
        mgr = self._make_manager(docker_ok=False, rp_ok=True)
        assert mgr.get_backend_for_language("c") == ExecutionBackend.UNAVAILABLE

    def test_unsupported_language_returns_unavailable(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=True)
        assert mgr.get_backend_for_language("brainfuck") == ExecutionBackend.UNAVAILABLE

    def test_language_lookup_is_case_insensitive(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=False)
        assert mgr.get_backend_for_language("PYTHON") == ExecutionBackend.DOCKER


# ---------------------------------------------------------------------------
# Container config
# ---------------------------------------------------------------------------


class TestGetContainerConfig:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def _make_manager(self) -> DockerManager:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "mocked")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "mocked")
            return DockerManager()

    def test_image_name_derived_from_language(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("python", "/tmp/workdir")
        assert cfg["image"] == "ancient-compute/python:latest"

    def test_volume_mounted_read_only(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("c", "/tmp/workdir")
        assert "/tmp/workdir" in cfg["volumes"]
        assert cfg["volumes"]["/tmp/workdir"]["mode"] == "ro"

    def test_network_mode_is_none(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("haskell", "/tmp/workdir")
        assert cfg["network_mode"] == "none"

    def test_memory_limits_are_set(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("c", "/tmp/workdir")
        assert cfg["mem_limit"] == "128m"
        assert cfg["memswap_limit"] == "128m"

    def test_no_new_privileges_security_opt(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("python", "/tmp/workdir")
        assert "no-new-privileges" in cfg["security_opt"]

    @pytest.mark.skipif(platform.system() == "Windows", reason="Linux-only config")
    def test_linux_config_includes_tmpfs_and_pids(self) -> None:
        mgr = self._make_manager()
        with patch("backend.src.services.docker_manager.platform.system", return_value="Linux"):
            cfg = mgr.get_container_config("python", "/tmp/workdir")
        assert "tmpfs" in cfg
        assert cfg["pids_limit"] == 50

    def test_windows_config_excludes_tmpfs(self) -> None:
        mgr = self._make_manager()
        with patch("backend.src.services.docker_manager.platform.system", return_value="Windows"):
            cfg = mgr.get_container_config("python", "/tmp/workdir")
        assert "tmpfs" not in cfg


# ---------------------------------------------------------------------------
# Status report
# ---------------------------------------------------------------------------


class TestGetStatusReport:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def test_status_report_mentions_all_backend_types(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "not running")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, True, "ok")
            mgr = DockerManager()
        report = mgr.get_status_report()
        assert "docker" in report.lower()
        assert "restricted_python" in report.lower()
        assert "subprocess" in report.lower()

    def test_status_report_includes_availability(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "not running")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, True, "ok")
            mgr = DockerManager()
        report = mgr.get_status_report()
        assert "NOT AVAILABLE" in report
        assert "AVAILABLE" in report

    def test_status_report_recommends_docker_when_unavailable(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "not running")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "missing")
            mgr = DockerManager()
        report = mgr.get_status_report()
        assert "Docker" in report
        assert "RestrictedPython" in report


# ---------------------------------------------------------------------------
# ensure_docker_image with Docker unavailable
# ---------------------------------------------------------------------------


class TestEnsureDockerImage:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def test_returns_false_when_docker_unavailable(self) -> None:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "unavailable")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "missing")
            mgr = DockerManager()
        result = mgr.ensure_docker_image("some-image:latest")
        assert result is False


# ---------------------------------------------------------------------------
# BackendInfo dataclass
# ---------------------------------------------------------------------------


class TestBackendInfo:
    """BackendInfo dataclass initializes correctly."""

    def test_available_true(self) -> None:
        bi = BackendInfo(ExecutionBackend.DOCKER, True, "ok")
        assert bi.available is True

    def test_available_false(self) -> None:
        bi = BackendInfo(ExecutionBackend.DOCKER, False, "not running")
        assert bi.available is False

    def test_reason_stored(self) -> None:
        bi = BackendInfo(ExecutionBackend.SUBPROCESS, True, "always available")
        assert bi.reason == "always available"

    def test_capabilities_default_empty_list(self) -> None:
        bi = BackendInfo(ExecutionBackend.DOCKER, False, "mocked")
        assert bi.capabilities == []

    def test_capabilities_explicit(self) -> None:
        bi = BackendInfo(ExecutionBackend.DOCKER, True, "ok", capabilities=["python", "c"])
        assert "python" in bi.capabilities
        assert "c" in bi.capabilities

    def test_backend_type_stored(self) -> None:
        bi = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, True, "ok")
        assert bi.backend_type == ExecutionBackend.RESTRICTED_PYTHON


# ---------------------------------------------------------------------------
# ExecutionBackend enum
# ---------------------------------------------------------------------------


class TestExecutionBackendEnum:
    """ExecutionBackend values match expected strings."""

    def test_docker_value(self) -> None:
        assert ExecutionBackend.DOCKER.value == "docker"

    def test_restricted_python_value(self) -> None:
        assert ExecutionBackend.RESTRICTED_PYTHON.value == "restricted_python"

    def test_subprocess_value(self) -> None:
        assert ExecutionBackend.SUBPROCESS.value == "subprocess"

    def test_unavailable_value(self) -> None:
        assert ExecutionBackend.UNAVAILABLE.value == "unavailable"

    def test_four_backends_exist(self) -> None:
        assert len(ExecutionBackend) == 4


# ---------------------------------------------------------------------------
# Additional container config coverage
# ---------------------------------------------------------------------------


class TestContainerConfigExtended:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def _make_manager(self) -> DockerManager:
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(ExecutionBackend.DOCKER, False, "mocked")
            mr.return_value = BackendInfo(ExecutionBackend.RESTRICTED_PYTHON, False, "mocked")
            return DockerManager()

    def test_java_image_name(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("java", "/tmp/work")
        assert cfg["image"] == "ancient-compute/java:latest"

    def test_haskell_image_name(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("haskell", "/tmp/work")
        assert cfg["image"] == "ancient-compute/haskell:latest"

    def test_config_has_volumes_key(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("c", "/tmp/work")
        assert "volumes" in cfg

    def test_config_has_image_key(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("python", "/tmp/work")
        assert "image" in cfg

    def test_config_has_network_mode(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("python", "/tmp/work")
        assert "network_mode" in cfg

    def test_config_has_security_opt(self) -> None:
        mgr = self._make_manager()
        cfg = mgr.get_container_config("python", "/tmp/work")
        assert "security_opt" in cfg


# ---------------------------------------------------------------------------
# Backend selection: additional languages
# ---------------------------------------------------------------------------


class TestGetBackendForLanguageExtended:
    def setup_method(self) -> None:
        _reset_singleton()

    def teardown_method(self) -> None:
        _reset_singleton()

    def _make_manager(self, docker_ok: bool, rp_ok: bool) -> DockerManager:
        caps = (
            ["c", "python", "haskell", "java", "lisp", "idris", "assembly", "systemf"]
            if docker_ok
            else []
        )
        with (
            patch("backend.src.services.docker_manager.DockerManager._check_docker") as md,
            patch(
                "backend.src.services.docker_manager.DockerManager._check_restricted_python"
            ) as mr,
        ):
            md.return_value = BackendInfo(
                ExecutionBackend.DOCKER, docker_ok, "ok", capabilities=caps
            )
            rp_caps = ["python"] if rp_ok else []
            mr.return_value = BackendInfo(
                ExecutionBackend.RESTRICTED_PYTHON, rp_ok, "ok", capabilities=rp_caps
            )
            return DockerManager()

    def test_haskell_docker_available(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=False)
        assert mgr.get_backend_for_language("haskell") == ExecutionBackend.DOCKER

    def test_java_docker_available(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=False)
        assert mgr.get_backend_for_language("java") == ExecutionBackend.DOCKER

    def test_haskell_no_docker_is_unavailable(self) -> None:
        mgr = self._make_manager(docker_ok=False, rp_ok=True)
        assert mgr.get_backend_for_language("haskell") == ExecutionBackend.UNAVAILABLE

    def test_empty_language_string_is_unavailable(self) -> None:
        mgr = self._make_manager(docker_ok=True, rp_ok=True)
        assert mgr.get_backend_for_language("") == ExecutionBackend.UNAVAILABLE
