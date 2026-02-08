"""
Ancient Compute - Unit Tests for Executors (No Docker Required)

NOTE (Jan 2026 Audit): This test module uses stale class names (PythonExecutor,
RestrictedPythonRunner, CExecutor, HaskellExecutor) that no longer exist.
The current implementation uses PythonService, CService, HaskellService with
a different API. This module needs to be rewritten to match the current API.

SKIPPED until rewrite is complete.
"""
import pytest

# Skip entire module at collection time
pytest.skip("Stale class names - needs rewrite for current API (Jan 2026 audit)", allow_module_level=True)
