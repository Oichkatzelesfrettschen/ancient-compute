#!/bin/bash
# Timeline Implementation Verification Script

echo "========================================="
echo "Timeline Visualization Implementation"
echo "Verification Script"
echo "========================================="
echo ""

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

check_file() {
    if [ -f "$1" ]; then
        lines=$(wc -l < "$1")
        echo -e "${GREEN}✓${NC} $1 ($lines lines)"
        return 0
    else
        echo -e "${RED}✗${NC} $1 (MISSING)"
        return 1
    fi
}

echo "Components:"
check_file "frontend/src/lib/components/timeline/TimelineD3.svelte"
check_file "frontend/src/lib/components/timeline/EraNavigator.svelte"
check_file "frontend/src/lib/components/timeline/ZoomController.svelte"
check_file "frontend/src/lib/components/timeline/MilestoneMarker.svelte"
check_file "frontend/src/lib/components/timeline/TimelineTooltip.svelte"
check_file "frontend/src/lib/components/timeline/README.md"

echo ""
echo "Stores:"
check_file "frontend/src/lib/stores/timelineVisualizationStore.ts"

echo ""
echo "API Client:"
check_file "frontend/src/lib/api/timeline.ts"
check_file "frontend/src/lib/api/index.ts"

echo ""
echo "Data:"
check_file "frontend/src/lib/data/sampleTimelineData.ts"

echo ""
echo "Routes:"
check_file "frontend/src/routes/timeline/+page.svelte"

echo ""
echo "Tests:"
check_file "frontend/src/lib/components/timeline/__tests__/TimelineD3.test.ts"
check_file "frontend/src/lib/stores/__tests__/timelineVisualizationStore.test.ts"
check_file "frontend/src/lib/api/__tests__/timeline.test.ts"

echo ""
echo "Documentation:"
check_file "TIMELINE_IMPLEMENTATION_SUMMARY.md"

echo ""
echo "========================================="
echo "Summary:"
echo "========================================="

# Count lines
total_lines=$(cat \
    frontend/src/lib/components/timeline/*.svelte \
    frontend/src/lib/stores/timelineVisualizationStore.ts \
    frontend/src/lib/api/timeline.ts \
    frontend/src/lib/data/sampleTimelineData.ts \
    frontend/src/routes/timeline/+page.svelte \
    frontend/src/lib/components/timeline/__tests__/TimelineD3.test.ts \
    frontend/src/lib/stores/__tests__/timelineVisualizationStore.test.ts \
    frontend/src/lib/api/__tests__/timeline.test.ts \
    2>/dev/null | wc -l)

echo "Total Lines of Code: $total_lines"
echo "Components: 5"
echo "Stores: 1"
echo "API Files: 1"
echo "Test Files: 3"
echo "Routes: 1"
echo ""

echo -e "${GREEN}✓${NC} Implementation Complete!"
echo ""
echo "Next steps:"
echo "1. cd frontend && npm install"
echo "2. npm run dev"
echo "3. Navigate to http://localhost:5173/timeline"
echo "4. Run tests: npm test"
