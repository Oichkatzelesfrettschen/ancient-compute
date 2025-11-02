#!/bin/bash
# Sync all submodules to their latest main/master branches
# Usage: ./scripts/multi-repo/sync-submodules.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "========================================="
echo "Ancient Compute: Sync Submodules"
echo "========================================="
echo ""

cd "$REPO_ROOT"

# Initialize submodules if not already done
echo "Initializing submodules..."
git submodule update --init --recursive

# Update each submodule to latest
echo ""
echo "Updating submodules to latest..."
git submodule foreach 'git checkout main || git checkout master'
git submodule foreach 'git pull origin $(git branch --show-current)'

# Update submodule references in parent repo
echo ""
echo "Updating submodule references..."
git submodule update --remote --merge

echo ""
echo "========================================="
echo "Submodules synchronized successfully!"
echo "========================================="
echo ""
echo "Current submodule status:"
git submodule status

echo ""
echo "To commit these changes:"
echo "  git add ."
echo "  git commit -m 'chore: update submodules to latest'"
echo "  git push origin main"
