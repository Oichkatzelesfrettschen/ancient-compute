"""Integration check for Babbage simulation profile verifier."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest


@pytest.mark.integration
def test_babbage_parameter_verifier_passes() -> None:
    repo_root = Path(__file__).resolve().parents[3]
    verifier = repo_root / "tools/simulation/verify_babbage_params.py"

    result = subprocess.run(
        [sys.executable, str(verifier)],
        cwd=repo_root,
        capture_output=True,
        text=True,
        check=False,
    )

    assert result.returncode == 0, (
        "verifier failed\n"
        f"stdout:\n{result.stdout}\n"
        f"stderr:\n{result.stderr}\n"
    )
