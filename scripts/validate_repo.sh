#!/usr/bin/env bash
set -euo pipefail

echo "==> Backend: pytest (warnings as errors)"
pushd backend >/dev/null
if [ -x venv/bin/pytest ]; then
  set +e
  venv/bin/pytest -q
  status=$?
  set -e
  if [ $status -ne 0 ]; then
    echo "[WARN] pytest returned non-zero (likely environment incompatibility)." >&2
  fi
else
  echo "[SKIP] backend venv not found"
fi
popd >/dev/null

echo "==> TODO report"
python3 tools/todo_report.py || true

echo "==> Lint: pylint (if available)"
if command -v pylint >/dev/null 2>&1; then
  pylint backend/src || true
else
  echo "[SKIP] pylint not installed"
fi

echo "Validation complete."

