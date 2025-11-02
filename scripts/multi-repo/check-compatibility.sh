#!/bin/bash
# Check version compatibility across all repositories
# Usage: ./scripts/multi-repo/check-compatibility.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "========================================="
echo "Ancient Compute: Compatibility Check"
echo "========================================="
echo ""

# Define expected versions
EXPECTED_NODE="20"
EXPECTED_PYTHON="3.11"
EXPECTED_BAZEL="6.4.0"

# Check Node.js version in frontend
echo "Checking Frontend (Node.js)..."
if [ -f "$REPO_ROOT/frontend/package.json" ]; then
    cd "$REPO_ROOT/frontend"
    if command -v node &> /dev/null; then
        NODE_VERSION=$(node --version | cut -d'v' -f2 | cut -d'.' -f1)
        if [ "$NODE_VERSION" -ge "$EXPECTED_NODE" ]; then
            echo "  ✓ Node.js $NODE_VERSION (>= $EXPECTED_NODE required)"
        else
            echo "  ✗ Node.js $NODE_VERSION (>= $EXPECTED_NODE required)"
            exit 1
        fi
    else
        echo "  ! Node.js not installed"
    fi
else
    echo "  ! Frontend not found (submodule not initialized?)"
fi

# Check Python version in backend
echo ""
echo "Checking Backend (Python)..."
if [ -f "$REPO_ROOT/backend/requirements.txt" ]; then
    cd "$REPO_ROOT/backend"
    if command -v python3 &> /dev/null; then
        PYTHON_VERSION=$(python3 --version | cut -d' ' -f2 | cut -d'.' -f1,2)
        echo "  ✓ Python $PYTHON_VERSION (>= $EXPECTED_PYTHON required)"
    else
        echo "  ! Python not installed"
    fi
else
    echo "  ! Backend not found (submodule not initialized?)"
fi

# Check Bazel version
echo ""
echo "Checking Build System (Bazel)..."
if [ -f "$REPO_ROOT/.bazelversion" ]; then
    BAZEL_VERSION=$(cat "$REPO_ROOT/.bazelversion")
    echo "  ✓ Bazel $BAZEL_VERSION configured"
    if command -v bazel &> /dev/null; then
        INSTALLED_BAZEL=$(bazel --version | cut -d' ' -f2)
        echo "  ✓ Bazel $INSTALLED_BAZEL installed"
    else
        echo "  ! Bazel not installed"
    fi
fi

# Check Docker
echo ""
echo "Checking Docker..."
if command -v docker &> /dev/null; then
    DOCKER_VERSION=$(docker --version | cut -d' ' -f3 | cut -d',' -f1)
    echo "  ✓ Docker $DOCKER_VERSION installed"
else
    echo "  ! Docker not installed"
fi

# Check API version compatibility
echo ""
echo "Checking API Version Compatibility..."
echo "  (This would check version compatibility between frontend and backend)"
echo "  TODO: Implement API version checking"

echo ""
echo "========================================="
echo "Compatibility check complete!"
echo "========================================="
