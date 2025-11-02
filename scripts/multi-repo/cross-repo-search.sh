#!/bin/bash
# Search for a pattern across all repositories
# Usage: ./scripts/multi-repo/cross-repo-search.sh <pattern>

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

if [ $# -eq 0 ]; then
    echo "Usage: $0 <search-pattern>"
    echo "Example: $0 'TODO:'"
    exit 1
fi

PATTERN="$1"

echo "========================================="
echo "Ancient Compute: Cross-Repo Search"
echo "Pattern: $PATTERN"
echo "========================================="
echo ""

# Repos to search
REPOS=(
    "frontend"
    "backend"
    "services"
    "babbage"
    "curriculum"
    "docs"
)

for repo in "${REPOS[@]}"; do
    repo_path="$REPO_ROOT/$repo"
    
    if [ -d "$repo_path" ]; then
        echo "Searching in $repo..."
        cd "$repo_path"
        
        # Use git grep if available (faster and respects .gitignore)
        if git rev-parse --git-dir > /dev/null 2>&1; then
            results=$(git grep -n "$PATTERN" 2>/dev/null || true)
            if [ -n "$results" ]; then
                echo "$results"
                echo ""
            else
                echo "  (no matches)"
                echo ""
            fi
        else
            # Fallback to regular grep
            results=$(grep -r -n "$PATTERN" . 2>/dev/null || true)
            if [ -n "$results" ]; then
                echo "$results"
                echo ""
            else
                echo "  (no matches)"
                echo ""
            fi
        fi
    else
        echo "  ! $repo not found (submodule not initialized?)"
        echo ""
    fi
done

echo "========================================="
echo "Search complete!"
echo "========================================="
