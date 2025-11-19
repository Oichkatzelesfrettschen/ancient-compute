# Ancient Compute: Phase 4 Execution Roadmap

**Document Version**: 1.0
**Created**: November 19, 2025
**Status**: AUTHORITATIVE - Day-by-Day Execution Plan
**Phase**: Phase 4 (Frontend & Visualization) - 80% → 100%
**Duration**: 5 Weeks (25 working days)
**Target Completion**: December 24, 2025

---

## Executive Summary

This roadmap provides a **day-by-day execution plan** to complete the remaining 20% of Phase 4 (Frontend & Visualization). Phase 4 is currently at 80% with weeks 1-3 complete (frontend skeleton, 3D visualization, WebSocket integration). This plan covers the remaining 5 weeks to achieve 100% completion.

### Current Status (Phase 4)
- ✅ **Week 1-3 Complete (80%)**:
  - Frontend skeleton (SvelteKit + TypeScript)
  - 3D Babbage Engine visualization (Three.js)
  - WebSocket real-time updates
  - State management (Svelte stores)
  - E2E test framework (Playwright)

- 🔄 **Weeks 4-8 Remaining (20%)**:
  - Interactive timeline visualization (D3.js)
  - Educational module system
  - Code playground with Monaco editor
  - Enhanced emulator UI
  - User dashboard and progress tracking

### Success Metrics
- **Lines of Code**: 6,561 → 16,000 (add 9,439 lines)
- **Components**: 19 → 80+ (add 61 components)
- **Unit Tests**: 850+ → 1,200+ (add 350 tests)
- **E2E Tests**: 19 → 40+ (add 21 tests)
- **Integration Tests**: 150+ → 250+ (add 100 tests)

---

## Table of Contents

1. [Week 1: Timeline Visualization (D3.js)](#week-1-timeline-visualization-d3js)
2. [Week 2: Educational Module System](#week-2-educational-module-system)
3. [Week 3: Code Playground](#week-3-code-playground)
4. [Week 4: Emulator Visualization Enhancements](#week-4-emulator-visualization-enhancements)
5. [Week 5: User Dashboard & Polish](#week-5-user-dashboard--polish)
6. [Testing Strategy](#testing-strategy)
7. [Performance Benchmarks](#performance-benchmarks)
8. [Accessibility Requirements](#accessibility-requirements)
9. [Deployment Checklist](#deployment-checklist)
10. [Risk Mitigation](#risk-mitigation)

---

## Week 1: Timeline Visualization (D3.js)

**Dates**: Week of Nov 25, 2025
**Effort**: 40 hours (5 days × 8 hours)
**Lines of Code**: 2,100 lines
**Components**: 8 new components
**Tests**: 75 tests

### Day 1-2: D3.js Timeline Component (Monday-Tuesday)

#### Components to Build

**File**: `frontend/src/lib/components/timeline/TimelineD3.svelte` (400 lines)
```typescript
/**
 * Interactive D3.js timeline spanning 12,500 years
 * Features:
 * - Horizontal scrollable timeline with zoom
 * - 8 historical eras with color coding
 * - Milestone markers for key inventions
 * - Hover tooltips with contextual information
 * - Click-to-navigate to era details
 */
```

**Features**:
- SVG-based timeline with responsive width
- Logarithmic time scale for ancient dates (20,000 BC to 2025 AD)
- Era bands with gradient fills
- Interactive milestone markers (Ishango bone, Antikythera, Babbage, Turing, etc.)
- Zoom controls (in/out, reset, fit-to-view)
- Brush selection for date range filtering

**File**: `frontend/src/lib/components/timeline/TimelineControls.svelte` (150 lines)
```typescript
/**
 * Timeline navigation controls
 * - Zoom in/out buttons
 * - Time period selector (Ancient, Medieval, Modern, Contemporary)
 * - Era quick-jump buttons
 * - Date range search
 */
```

**File**: `frontend/src/lib/visualization/d3/timelineScale.ts` (200 lines)
```typescript
/**
 * D3 scale utilities for timeline
 * - Logarithmic scale for ancient dates
 * - Linear scale for modern dates (1500-2025)
 * - Scale transition functions
 * - Date formatting utilities (BC/AD, formatting)
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/timeline/full`
```typescript
// Response type
interface TimelineFullResponse {
  eras: Era[];
  milestones: Milestone[];
  modules: Module[];
  total_events: number;
}
```

**Endpoint**: `GET /api/v1/timeline/eras`
```typescript
// Response type
interface ErasResponse {
  eras: {
    id: string;
    name: string;
    start_year: number;
    end_year: number;
    color: string;
    description: string;
    modules_count: number;
  }[];
}
```

**Endpoint**: `GET /api/v1/timeline/milestones?start_year={start}&end_year={end}`
```typescript
// Response type
interface MilestonesResponse {
  milestones: {
    id: string;
    year: number;
    title: string;
    description: string;
    category: string; // "invention" | "theory" | "person" | "civilization"
    civilization: string;
    importance: number; // 1-5 scale
  }[];
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/timeline/__tests__/TimelineD3.test.ts` (25 tests)
```typescript
describe('TimelineD3', () => {
  // Rendering tests (5)
  it('renders timeline SVG with correct dimensions')
  it('displays all 8 eras with correct colors')
  it('renders milestone markers at correct positions')
  it('shows era labels with correct formatting')
  it('displays zoom controls')

  // Interaction tests (10)
  it('zooms in when zoom-in button clicked')
  it('zooms out when zoom-out button clicked')
  it('resets view when reset button clicked')
  it('navigates to era when era marker clicked')
  it('shows tooltip on milestone hover')
  it('hides tooltip on milestone mouseout')
  it('pans timeline with drag gesture')
  it('scrolls timeline with mouse wheel')
  it('selects date range with brush')
  it('clears brush selection')

  // Scale tests (5)
  it('uses logarithmic scale for ancient dates (< 0 AD)')
  it('uses linear scale for modern dates (> 1500 AD)')
  it('transitions smoothly between scale types')
  it('formats BC dates correctly')
  it('formats AD dates correctly')

  // Accessibility tests (5)
  it('has ARIA labels for all interactive elements')
  it('supports keyboard navigation (arrow keys)')
  it('announces era changes to screen readers')
  it('provides text alternatives for visual markers')
  it('meets WCAG 2.1 AA contrast requirements')
});
```

**File**: `frontend/src/lib/visualization/d3/__tests__/timelineScale.test.ts` (15 tests)

#### Dependencies
- ✅ D3.js library (already installed: `d3: ^7.8.5`)
- ✅ Backend timeline API endpoints (implemented in `backend/src/api/timeline.py`)
- ✅ Timeline store (exists: `frontend/src/lib/stores/timelineStore.ts`)

#### Success Criteria
- ✅ Timeline renders 12,500 years of history with smooth scrolling
- ✅ Zoom levels: 1x (full view) to 50x (decade view)
- ✅ All 8 eras visible with distinct colors
- ✅ 30+ key milestones rendered with tooltips
- ✅ Responsive: works on desktop (1920px) and tablet (768px)
- ✅ Performance: renders < 100ms on modern hardware
- ✅ Accessibility: WCAG 2.1 AA compliant

#### Risk Factors
- ⚠️ **Risk**: D3.js learning curve for complex time scales
  - **Mitigation**: Use D3 scaleLog and scaleTime, reference examples from Observable
- ⚠️ **Risk**: Performance with 100+ milestone markers
  - **Mitigation**: Virtual scrolling, only render visible markers
- ⚠️ **Risk**: Timeline data not seeded in database
  - **Mitigation**: Use mock data for development, create seeder script

#### Estimated Effort
| Task | Hours |
|------|-------|
| TimelineD3 component | 8h |
| D3 scale utilities | 4h |
| Timeline controls | 3h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **20h** |

---

### Day 3-4: Era Navigation and Zoom (Wednesday-Thursday)

#### Components to Build

**File**: `frontend/src/lib/components/timeline/EraNavigator.svelte` (250 lines)
```typescript
/**
 * Era navigation sidebar
 * - List of 8 historical eras
 * - Click to jump to era on timeline
 * - Highlight current visible era
 * - Show progress for each era (completed modules)
 */
```

**File**: `frontend/src/lib/components/timeline/EraDetailPanel.svelte` (300 lines)
```typescript
/**
 * Detailed view of selected era
 * - Era name, date range, description
 * - List of modules in era
 * - Key milestones in era
 * - Civilizations active in era
 * - Entry point to modules
 */
```

**File**: `frontend/src/lib/components/timeline/ZoomController.svelte` (180 lines)
```typescript
/**
 * Advanced zoom controls
 * - Zoom slider (1x to 50x)
 * - Preset zoom levels (Overview, Century, Decade)
 * - Zoom to selection (brush region)
 * - Minimap overview
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/timeline/eras/{era_id}`
```typescript
interface EraDetailResponse {
  era: {
    id: string;
    name: string;
    start_year: number;
    end_year: number;
    description: string;
    modules: Module[];
    milestones: Milestone[];
    civilizations: string[];
    key_concepts: string[];
  };
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/timeline/__tests__/EraNavigator.test.ts` (12 tests)
**File**: `frontend/src/lib/components/timeline/__tests__/EraDetailPanel.test.ts` (15 tests)
**File**: `frontend/src/lib/components/timeline/__tests__/ZoomController.test.ts` (8 tests)

#### Success Criteria
- ✅ Click era in navigator → timeline scrolls and zooms to era
- ✅ Era detail panel shows all modules and milestones
- ✅ Zoom slider updates timeline smoothly (no lag)
- ✅ Minimap shows current viewport position
- ✅ Keyboard shortcuts work (Z = zoom in, X = zoom out, R = reset)

#### Estimated Effort
| Task | Hours |
|------|-------|
| Era navigator | 4h |
| Era detail panel | 5h |
| Zoom controller | 4h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **18h** |

---

### Day 5: Historical Milestone Markers (Friday)

#### Components to Build

**File**: `frontend/src/lib/components/timeline/MilestoneMarker.svelte` (120 lines)
```typescript
/**
 * Individual milestone marker on timeline
 * - SVG icon (category-based: invention, theory, person, civilization)
 * - Hover tooltip with details
 * - Click to expand full description
 * - Color-coded by importance (1-5 scale)
 */
```

**File**: `frontend/src/lib/components/timeline/MilestoneTooltip.svelte` (200 lines)
```typescript
/**
 * Tooltip for milestone hover
 * - Title, year, civilization
 * - Brief description (50 words)
 * - Category icon
 * - "Learn more" link
 */
```

**File**: `frontend/src/lib/components/timeline/MilestoneModal.svelte` (250 lines)
```typescript
/**
 * Expanded modal for milestone details
 * - Full description (500+ words)
 * - Images/diagrams (if available)
 * - Related modules
 * - Historical context
 * - Sources and citations
 */
```

#### Tests to Write

**File**: `frontend/src/lib/components/timeline/__tests__/MilestoneMarker.test.ts` (10 tests)

#### Success Criteria
- ✅ 30+ milestones rendered on timeline
- ✅ Tooltips appear on hover within 200ms
- ✅ Modal opens with full details
- ✅ Icons distinguish categories visually
- ✅ Importance scale affects marker size

#### Estimated Effort
| Task | Hours |
|------|-------|
| Milestone marker | 3h |
| Tooltip component | 2h |
| Modal component | 2h |
| Unit tests | 1h |
| **Total** | **8h** |

---

### Day 6-7: Animations and Transitions (Weekend - Optional Buffer)

#### Components to Build

**File**: `frontend/src/lib/visualization/d3/transitions.ts` (150 lines)
```typescript
/**
 * D3 transition utilities
 * - Era change transitions (fade, slide)
 * - Zoom transitions (easeInOut)
 * - Milestone appearance animations
 * - Timeline scroll animations
 */
```

#### Success Criteria
- ✅ Smooth transitions between eras (500ms duration)
- ✅ Zoom animations feel natural (not jarring)
- ✅ Milestones fade in on timeline load
- ✅ No animation jank (maintain 60fps)

#### Estimated Effort
| Task | Hours |
|------|-------|
| Transition utilities | 3h |
| Integration with timeline | 2h |
| Performance optimization | 2h |
| **Total** | **7h** |

---

### Week 1 Summary

#### Deliverables
- ✅ 8 new components (2,100 lines)
- ✅ Interactive D3.js timeline
- ✅ Era navigation and zoom
- ✅ Milestone markers with tooltips
- ✅ 75 unit tests

#### Files Created
```
frontend/src/lib/components/timeline/
├── TimelineD3.svelte (400 lines)
├── TimelineControls.svelte (150 lines)
├── EraNavigator.svelte (250 lines)
├── EraDetailPanel.svelte (300 lines)
├── ZoomController.svelte (180 lines)
├── MilestoneMarker.svelte (120 lines)
├── MilestoneTooltip.svelte (200 lines)
├── MilestoneModal.svelte (250 lines)
└── __tests__/
    ├── TimelineD3.test.ts (25 tests)
    ├── EraNavigator.test.ts (12 tests)
    ├── EraDetailPanel.test.ts (15 tests)
    ├── ZoomController.test.ts (8 tests)
    └── MilestoneMarker.test.ts (10 tests)

frontend/src/lib/visualization/d3/
├── timelineScale.ts (200 lines)
├── transitions.ts (150 lines)
└── __tests__/
    └── timelineScale.test.ts (15 tests)
```

#### API Endpoints Used
- `GET /api/v1/timeline/full`
- `GET /api/v1/timeline/eras`
- `GET /api/v1/timeline/eras/{era_id}`
- `GET /api/v1/timeline/milestones`

#### Test Coverage
- 75 unit tests
- Coverage target: >90%

---

## Week 2: Educational Module System

**Dates**: Week of Dec 2, 2025
**Effort**: 40 hours (5 days × 8 hours)
**Lines of Code**: 2,200 lines
**Components**: 12 new components
**Tests**: 85 tests

### Day 1-2: Module Grid and List Components (Monday-Tuesday)

#### Components to Build

**File**: `frontend/src/lib/components/education/ModuleGrid.svelte` (350 lines)
```typescript
/**
 * Grid view of educational modules
 * Features:
 * - Card-based layout (responsive grid)
 * - Module metadata (title, description, difficulty, duration)
 * - Progress indicator (percentage complete)
 * - Filter by era, difficulty, completion status
 * - Sort by name, difficulty, progress
 */
```

**File**: `frontend/src/lib/components/education/ModuleCard.svelte` (250 lines)
```typescript
/**
 * Individual module card
 * - Module icon/image
 * - Title and description (truncated)
 * - Era badge
 * - Difficulty stars (1-5)
 * - Estimated hours
 * - Progress bar
 * - "Start" or "Continue" button
 */
```

**File**: `frontend/src/lib/components/education/ModuleFilters.svelte` (200 lines)
```typescript
/**
 * Filter controls for module grid
 * - Era selector (multi-select)
 * - Difficulty range slider (1-5)
 * - Completion status checkboxes (Not Started, In Progress, Completed)
 * - Search bar (fuzzy search on title/description)
 * - Sort dropdown (Name, Difficulty, Progress)
 * - Clear filters button
 */
```

**File**: `frontend/src/lib/components/education/ModuleListView.svelte` (280 lines)
```typescript
/**
 * Alternative list view for modules
 * - Compact list with columns (Name, Era, Difficulty, Progress, Actions)
 * - Sortable columns
 * - Pagination (20 per page)
 * - Bulk actions (mark complete, reset progress)
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/modules`
```typescript
interface ModulesResponse {
  modules: {
    id: string;
    slug: string;
    title: string;
    description: string;
    era: string;
    difficulty_level: number; // 1-5
    estimated_hours: number;
    sequence_order: number;
    lessons_count: number;
    completed_lessons_count: number;
    progress_percentage: number;
  }[];
  total: number;
}
```

**Endpoint**: `GET /api/v1/modules?era={era}&difficulty_min={min}&difficulty_max={max}&status={status}`
```typescript
// Filtered modules endpoint
// Query params:
// - era: string (era slug)
// - difficulty_min: number (1-5)
// - difficulty_max: number (1-5)
// - status: "not_started" | "in_progress" | "completed"
// - search: string (fuzzy search)
// - sort: "name" | "difficulty" | "progress"
// - order: "asc" | "desc"
// - page: number
// - limit: number
```

#### Tests to Write

**File**: `frontend/src/lib/components/education/__tests__/ModuleGrid.test.ts` (20 tests)
```typescript
describe('ModuleGrid', () => {
  // Rendering tests (5)
  it('renders module cards in grid layout')
  it('displays empty state when no modules')
  it('shows loading skeleton while fetching')
  it('applies filters correctly')
  it('updates when sort order changes')

  // Interaction tests (10)
  it('navigates to module on card click')
  it('filters by era when era selected')
  it('filters by difficulty when slider changed')
  it('searches modules by title')
  it('clears all filters')
  it('sorts by name ascending')
  it('sorts by difficulty descending')
  it('paginates to next page')
  it('shows "no results" when filters match nothing')
  it('resets to page 1 when filters change')

  // Accessibility tests (5)
  it('has proper heading hierarchy')
  it('announces filter changes to screen readers')
  it('keyboard navigable (tab, enter)')
  it('focus management after filter')
  it('meets color contrast requirements')
});
```

**File**: `frontend/src/lib/components/education/__tests__/ModuleCard.test.ts` (15 tests)
**File**: `frontend/src/lib/components/education/__tests__/ModuleFilters.test.ts` (12 tests)

#### Success Criteria
- ✅ Module grid displays all modules from API
- ✅ Filters work correctly (era, difficulty, status)
- ✅ Search is fast (< 200ms for fuzzy match)
- ✅ Grid responsive: 4 cols (desktop), 2 cols (tablet), 1 col (mobile)
- ✅ Progress bars accurate
- ✅ Click card → navigate to module detail page

#### Estimated Effort
| Task | Hours |
|------|-------|
| Module grid component | 5h |
| Module card component | 3h |
| Filter controls | 4h |
| List view component | 3h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **20h** |

---

### Day 3-4: Lesson Viewer with Markdown (Wednesday-Thursday)

#### Components to Build

**File**: `frontend/src/lib/components/education/LessonViewer.svelte` (400 lines)
```typescript
/**
 * Markdown-based lesson content viewer
 * Features:
 * - Markdown rendering with syntax highlighting
 * - Table of contents (auto-generated from headings)
 * - Code blocks with copy button
 * - LaTeX math rendering (KaTeX)
 * - Diagrams (Mermaid.js)
 * - Interactive examples (embedded code playground)
 * - "Next lesson" navigation
 */
```

**File**: `frontend/src/lib/components/education/LessonNavigation.svelte` (180 lines)
```typescript
/**
 * Lesson navigation sidebar
 * - Module name and progress
 * - List of lessons with checkmarks
 * - Current lesson highlighted
 * - "Previous" and "Next" buttons
 * - "Mark as complete" button
 */
```

**File**: `frontend/src/lib/components/education/TableOfContents.svelte` (150 lines)
```typescript
/**
 * Auto-generated TOC from lesson markdown
 * - Extract H2, H3 headings
 * - Anchor links to sections
 * - Highlight current section on scroll
 * - Collapsible nested sections
 */
```

**File**: `frontend/src/lib/markdown/renderer.ts` (250 lines)
```typescript
/**
 * Custom markdown renderer
 * - Marked.js with extensions
 * - Syntax highlighting (Prism.js)
 * - KaTeX for LaTeX math
 * - Mermaid for diagrams
 * - Custom directives (:::info, :::warning, :::code-playground)
 */
```

#### Dependencies (New)
- **marked**: Markdown parser (`npm install marked @types/marked`)
- **prismjs**: Syntax highlighting (`npm install prismjs`)
- **katex**: LaTeX rendering (`npm install katex`)
- **mermaid**: Diagram rendering (`npm install mermaid`)

#### APIs to Integrate

**Endpoint**: `GET /api/v1/lessons/{lesson_id}`
```typescript
interface LessonResponse {
  lesson: {
    id: number;
    slug: string;
    title: string;
    content: string; // Markdown
    module_id: string;
    sequence_order: number;
    estimated_minutes: number;
    objectives: string[];
    prerequisites: string[];
    exercises: Exercise[];
  };
}
```

**Endpoint**: `POST /api/v1/lessons/{lesson_id}/complete`
```typescript
interface LessonCompleteRequest {
  time_spent_minutes: number;
}
interface LessonCompleteResponse {
  success: boolean;
  progress_updated: boolean;
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/education/__tests__/LessonViewer.test.ts` (18 tests)
**File**: `frontend/src/lib/markdown/__tests__/renderer.test.ts` (12 tests)

#### Success Criteria
- ✅ Markdown renders correctly with all features
- ✅ Syntax highlighting works for all 8 languages
- ✅ LaTeX math renders inline and block
- ✅ Code blocks have "Copy" button
- ✅ TOC updates on scroll
- ✅ "Mark complete" button works
- ✅ Navigation to next/previous lesson

#### Estimated Effort
| Task | Hours |
|------|-------|
| Lesson viewer component | 6h |
| Markdown renderer | 4h |
| Lesson navigation | 3h |
| Table of contents | 2h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **20h** |

---

### Day 5: Exercise Submission UI (Friday)

#### Components to Build

**File**: `frontend/src/lib/components/education/ExerciseView.svelte` (350 lines)
```typescript
/**
 * Exercise submission interface
 * Features:
 * - Exercise description (markdown)
 * - Code editor (Monaco) with language selector
 * - "Run Tests" button
 * - Test results display (pass/fail for each test)
 * - "Submit" button
 * - Submission history
 */
```

**File**: `frontend/src/lib/components/education/TestResults.svelte` (200 lines)
```typescript
/**
 * Test results display
 * - List of tests with pass/fail status
 * - Expected vs. actual output diff
 * - Execution time per test
 * - Overall pass percentage
 * - Error messages (if any)
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/exercises/{exercise_id}`
```typescript
interface ExerciseResponse {
  exercise: {
    id: number;
    title: string;
    description: string; // Markdown
    starter_code: string;
    language: string;
    test_cases: {
      id: number;
      description: string;
      input: string;
      expected_output: string;
      points: number;
    }[];
  };
}
```

**Endpoint**: `POST /api/v1/exercises/{exercise_id}/submit`
```typescript
interface ExerciseSubmitRequest {
  code: string;
  language: string;
}
interface ExerciseSubmitResponse {
  submission_id: number;
  test_results: {
    test_id: number;
    passed: boolean;
    actual_output: string;
    execution_time_ms: number;
    error?: string;
  }[];
  total_points: number;
  max_points: number;
  percentage: number;
  passed: boolean; // true if all tests pass
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/education/__tests__/ExerciseView.test.ts` (15 tests)

#### Success Criteria
- ✅ Exercise loads with starter code
- ✅ "Run Tests" executes code and shows results
- ✅ Test results show pass/fail status
- ✅ "Submit" button enabled only if all tests pass
- ✅ Submission saved to user's history

#### Estimated Effort
| Task | Hours |
|------|-------|
| Exercise view component | 4h |
| Test results component | 2h |
| API integration | 1h |
| Unit tests | 1h |
| **Total** | **8h** |

---

### Week 2 Summary

#### Deliverables
- ✅ 12 new components (2,200 lines)
- ✅ Module grid with filters
- ✅ Lesson viewer with markdown
- ✅ Exercise submission UI
- ✅ 85 unit tests

#### Files Created
```
frontend/src/lib/components/education/
├── ModuleGrid.svelte (350 lines)
├── ModuleCard.svelte (250 lines)
├── ModuleFilters.svelte (200 lines)
├── ModuleListView.svelte (280 lines)
├── LessonViewer.svelte (400 lines)
├── LessonNavigation.svelte (180 lines)
├── TableOfContents.svelte (150 lines)
├── ExerciseView.svelte (350 lines)
├── TestResults.svelte (200 lines)
└── __tests__/
    ├── ModuleGrid.test.ts (20 tests)
    ├── ModuleCard.test.ts (15 tests)
    ├── ModuleFilters.test.ts (12 tests)
    ├── LessonViewer.test.ts (18 tests)
    └── ExerciseView.test.ts (15 tests)

frontend/src/lib/markdown/
├── renderer.ts (250 lines)
└── __tests__/
    └── renderer.test.ts (12 tests)
```

#### Dependencies Added
```json
{
  "dependencies": {
    "marked": "^11.0.0",
    "prismjs": "^1.29.0",
    "katex": "^0.16.9",
    "mermaid": "^10.6.0"
  },
  "devDependencies": {
    "@types/marked": "^6.0.0"
  }
}
```

#### API Endpoints Used
- `GET /api/v1/modules`
- `GET /api/v1/modules?era=...&difficulty_min=...&status=...`
- `GET /api/v1/lessons/{lesson_id}`
- `POST /api/v1/lessons/{lesson_id}/complete`
- `GET /api/v1/exercises/{exercise_id}`
- `POST /api/v1/exercises/{exercise_id}/submit`

---

## Week 3: Code Playground

**Dates**: Week of Dec 9, 2025
**Effort**: 40 hours (5 days × 8 hours)
**Lines of Code**: 2,000 lines
**Components**: 10 new components
**Tests**: 70 tests

### Day 1-2: Monaco Editor Integration (Monday-Tuesday)

#### Components to Build

**File**: `frontend/src/lib/components/playground/CodeEditor.svelte` (450 lines)
```typescript
/**
 * Monaco Editor wrapper component
 * Features:
 * - Multi-language support (8 languages)
 * - Syntax highlighting and IntelliSense
 * - Auto-completion
 * - Error markers (linting)
 * - Theme support (light/dark/custom)
 * - Font size control
 * - Line numbers and minimap
 * - Keyboard shortcuts (Ctrl+Enter to run)
 */
```

**File**: `frontend/src/lib/playground/monacoConfig.ts` (200 lines)
```typescript
/**
 * Monaco editor configuration
 * - Language definitions for all 8 languages
 * - Theme configurations
 * - IntelliSense providers
 * - Linting rules
 * - Formatter settings
 */
```

**File**: `frontend/src/lib/playground/languageProviders.ts` (300 lines)
```typescript
/**
 * Language-specific Monaco providers
 * - C: GCC error parsing
 * - Python: PEP8 linting
 * - Haskell: Type inference hints
 * - Java: Import suggestions
 * - LISP: Parenthesis matching
 * - IDRIS2: Proof assistance
 * - System F: Type annotation
 * - Assembly: Instruction hints
 */
```

#### Dependencies
- ✅ Monaco Editor (already installed: `monaco-editor: ^0.44.0`)

#### APIs to Integrate

**Endpoint**: `POST /api/v1/execute/run`
```typescript
interface ExecuteRequest {
  language: "c" | "python" | "haskell" | "java" | "lisp" | "idris" | "systemf" | "assembly";
  code: string;
  input_data?: string;
  lesson_id?: number;
}
interface ExecuteResponse {
  status: "success" | "error" | "timeout";
  stdout: string;
  stderr: string;
  compile_output?: string;
  execution_time: number; // ms
  memory_used: number; // bytes
}
```

**Endpoint**: `GET /api/v1/execute/languages`
```typescript
interface LanguagesResponse {
  languages: {
    id: string;
    name: string;
    version: string;
    file_extension: string;
    monaco_language: string;
  }[];
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/playground/__tests__/CodeEditor.test.ts` (20 tests)
```typescript
describe('CodeEditor', () => {
  // Rendering tests (5)
  it('initializes Monaco editor instance')
  it('loads with correct language mode')
  it('displays line numbers')
  it('applies theme correctly')
  it('shows minimap on large files')

  // Functionality tests (10)
  it('updates code on user input')
  it('switches language when language changed')
  it('provides auto-completion suggestions')
  it('highlights syntax errors')
  it('formats code on command (Shift+Alt+F)')
  it('runs code on Ctrl+Enter')
  it('changes font size')
  it('toggles minimap')
  it('undo/redo works correctly')
  it('finds and replaces text')

  // Accessibility tests (5)
  it('keyboard navigable')
  it('screen reader accessible')
  it('high contrast mode support')
  it('focus management')
  it('ARIA labels present')
});
```

#### Success Criteria
- ✅ Monaco editor loads in < 500ms
- ✅ All 8 languages have syntax highlighting
- ✅ Auto-completion works for common patterns
- ✅ Error markers appear for syntax errors
- ✅ Keyboard shortcuts work (run, format, find)
- ✅ Responsive: works on desktop and tablet

#### Estimated Effort
| Task | Hours |
|------|-------|
| CodeEditor component | 8h |
| Monaco configuration | 4h |
| Language providers | 6h |
| Unit tests | 2h |
| **Total** | **20h** |

---

### Day 3: Multi-Language Support (Wednesday)

#### Components to Build

**File**: `frontend/src/lib/components/playground/LanguageSelector.svelte` (180 lines)
```typescript
/**
 * Language selector dropdown
 * - 8 language options with icons
 * - Shows language version
 * - Keyboard shortcut hints
 * - Recently used languages
 * - "Change language" confirmation if code present
 */
```

**File**: `frontend/src/lib/components/playground/LanguageInfo.svelte` (120 lines)
```typescript
/**
 * Language information panel
 * - Current language name and version
 * - Compiler/interpreter used
 * - Link to language documentation
 * - Keyboard shortcuts for language
 */
```

#### Tests to Write

**File**: `frontend/src/lib/components/playground/__tests__/LanguageSelector.test.ts` (12 tests)

#### Success Criteria
- ✅ All 8 languages selectable
- ✅ Language switch preserves code (with confirmation)
- ✅ Language icons distinguish visually
- ✅ Language info accurate

#### Estimated Effort
| Task | Hours |
|------|-------|
| Language selector | 3h |
| Language info | 2h |
| Unit tests | 1h |
| **Total** | **6h** |

---

### Day 4: Execute API Integration (Thursday)

#### Components to Build

**File**: `frontend/src/lib/components/playground/ExecutionPanel.svelte` (300 lines)
```typescript
/**
 * Execution control panel
 * Features:
 * - "Run" button (with loading state)
 * - Input data textarea
 * - Execution options (timeout, memory limit)
 * - Stop/cancel button (for long-running code)
 * - Execution history (last 5 runs)
 */
```

**File**: `frontend/src/lib/components/playground/OutputConsole.svelte` (280 lines)
```typescript
/**
 * Output console display
 * Features:
 * - Tabbed view (stdout, stderr, compile output)
 * - ANSI color support
 * - Auto-scroll to bottom
 * - Copy output button
 * - Clear console button
 * - Execution time and memory usage display
 * - Error highlighting
 */
```

**File**: `frontend/src/lib/components/playground/ErrorDisplay.svelte` (150 lines)
```typescript
/**
 * Error message display
 * - Parse compiler errors
 * - Line number extraction
 * - Click error → jump to line in editor
 * - Syntax error hints
 * - Stack trace formatting
 */
```

#### Tests to Write

**File**: `frontend/src/lib/components/playground/__tests__/ExecutionPanel.test.ts` (15 tests)
**File**: `frontend/src/lib/components/playground/__tests__/OutputConsole.test.ts` (18 tests)

#### Success Criteria
- ✅ "Run" button executes code via API
- ✅ Loading state shows during execution
- ✅ Output appears in console (stdout, stderr)
- ✅ Errors highlighted in console
- ✅ Click error → jump to line in editor
- ✅ Execution time displayed

#### Estimated Effort
| Task | Hours |
|------|-------|
| Execution panel | 4h |
| Output console | 4h |
| Error display | 2h |
| API integration | 2h |
| Unit tests | 2h |
| **Total** | **14h** |

---

### Day 5: Example Library and UX Polish (Friday)

#### Components to Build

**File**: `frontend/src/lib/components/playground/ExampleLibrary.svelte` (250 lines)
```typescript
/**
 * Pre-built code examples
 * Features:
 * - Categorized examples (Beginner, Intermediate, Advanced)
 * - Filter by language
 * - Search examples
 * - Load example into editor
 * - Example metadata (description, author, tags)
 */
```

**File**: `frontend/src/lib/components/playground/ShareCodeDialog.svelte` (180 lines)
```typescript
/**
 * Share code dialog
 * - Generate shareable URL
 * - Copy to clipboard
 * - QR code (optional)
 * - Embed code snippet
 */
```

**File**: `frontend/src/lib/playground/codeExamples.ts` (200 lines)
```typescript
/**
 * Code example definitions
 * - 5 examples per language (40 total)
 * - Categories: Hello World, Functions, Data Structures, Algorithms, Advanced
 */
```

#### Tests to Write

**File**: `frontend/src/lib/components/playground/__tests__/ExampleLibrary.test.ts` (10 tests)

#### Success Criteria
- ✅ 40+ examples available
- ✅ Examples load into editor on click
- ✅ Search finds examples by keyword
- ✅ Share URLs generate correctly

#### Estimated Effort
| Task | Hours |
|------|-------|
| Example library | 3h |
| Share dialog | 2h |
| Example content | 2h |
| Unit tests | 1h |
| **Total** | **8h** |

---

### Week 3 Summary

#### Deliverables
- ✅ 10 new components (2,000 lines)
- ✅ Monaco editor fully integrated
- ✅ Multi-language support (8 languages)
- ✅ Execute API integration
- ✅ Example library (40+ examples)
- ✅ 70 unit tests

#### Files Created
```
frontend/src/lib/components/playground/
├── CodeEditor.svelte (450 lines)
├── LanguageSelector.svelte (180 lines)
├── LanguageInfo.svelte (120 lines)
├── ExecutionPanel.svelte (300 lines)
├── OutputConsole.svelte (280 lines)
├── ErrorDisplay.svelte (150 lines)
├── ExampleLibrary.svelte (250 lines)
├── ShareCodeDialog.svelte (180 lines)
└── __tests__/
    ├── CodeEditor.test.ts (20 tests)
    ├── LanguageSelector.test.ts (12 tests)
    ├── ExecutionPanel.test.ts (15 tests)
    ├── OutputConsole.test.ts (18 tests)
    └── ExampleLibrary.test.ts (10 tests)

frontend/src/lib/playground/
├── monacoConfig.ts (200 lines)
├── languageProviders.ts (300 lines)
└── codeExamples.ts (200 lines)
```

#### API Endpoints Used
- `POST /api/v1/execute/run`
- `GET /api/v1/execute/languages`

---

## Week 4: Emulator Visualization Enhancements

**Dates**: Week of Dec 16, 2025
**Effort**: 40 hours (5 days × 8 hours)
**Lines of Code**: 1,900 lines
**Components**: 11 new components
**Tests**: 65 tests

### Day 1-3: Three.js 3D Engine Model Enhancement (Monday-Wednesday)

**Note**: 3D Babbage Engine already exists (`frontend/src/routes/emulator/3d/+page.svelte`). These tasks enhance the existing implementation.

#### Components to Build

**File**: `frontend/src/lib/components/emulator/MechanicalVisualization.svelte` (400 lines)
```typescript
/**
 * Enhanced 3D mechanical visualization
 * Features:
 * - Animated gear rotations during execution
 * - Column highlighting during operations
 * - Carry propagation visualization
 * - Shaft rotation timing
 * - Interactive camera controls (orbit, zoom, pan)
 * - Component labels (on hover)
 * - Exploded view mode
 * - Cross-section view
 */
```

**File**: `frontend/src/lib/3d/components/DigitColumn.ts` (200 lines)
```typescript
/**
 * 3D model of digit column
 * - 10 position wheels (0-9)
 * - Rotation animation
 * - Carry indicator
 * - Material: brass-like shader
 */
```

**File**: `frontend/src/lib/3d/components/AnticipatingCarriage.ts` (250 lines)
```typescript
/**
 * 3D model of anticipating carriage mechanism
 * - Carry chain visualization
 * - Propagation animation (sequential or simultaneous)
 * - Timing visualization
 */
```

**File**: `frontend/src/lib/3d/animations/operationAnimations.ts` (300 lines)
```typescript
/**
 * Animation sequences for operations
 * - Addition animation (column add + carry)
 * - Subtraction animation
 * - Multiplication animation (repeated addition)
 * - Division animation
 * - Timing: sync with emulator cycle count
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/emulator/state/{execution_id}`
```typescript
interface EmulatorStateResponse {
  execution_id: string;
  cycle: number;
  registers: {
    A: string; // 50-digit decimal
    B: string;
    C: string;
    D: string;
  };
  flags: {
    zero: boolean;
    carry: boolean;
    negative: boolean;
    overflow: boolean;
  };
  columns: {
    column_id: number;
    value: string; // 50-digit
    carry_in: boolean;
    carry_out: boolean;
  }[];
  current_operation: string;
}
```

**WebSocket**: `ws://api/v1/emulator/live/{execution_id}`
```typescript
// Real-time state updates during execution
interface EmulatorLiveUpdate {
  type: "state_update" | "operation_start" | "operation_complete" | "carry_propagate";
  data: EmulatorStateResponse;
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/emulator/__tests__/MechanicalVisualization.test.ts` (15 tests)
**File**: `frontend/src/lib/3d/animations/__tests__/operationAnimations.test.ts` (12 tests)

#### Success Criteria
- ✅ 3D model animates during code execution
- ✅ Columns rotate to show values
- ✅ Carry propagation visually shown
- ✅ Camera controls smooth
- ✅ Performance: 60fps on modern hardware
- ✅ WebSocket updates reflected in 3D model

#### Estimated Effort
| Task | Hours |
|------|-------|
| Mechanical visualization | 8h |
| Digit column model | 4h |
| Carriage mechanism | 5h |
| Operation animations | 6h |
| WebSocket integration | 3h |
| Unit tests | 2h |
| **Total** | **28h** |

---

### Day 4-5: Debugger UI (Thursday-Friday)

#### Components to Build

**File**: `frontend/src/lib/components/emulator/DebuggerPanel.svelte` (350 lines)
```typescript
/**
 * Integrated debugger interface
 * Features:
 * - Breakpoint list
 * - Step controls (step, step over, continue)
 * - Execution pause/resume
 * - Current instruction display
 * - Call stack view
 * - Variable watch list
 */
```

**File**: `frontend/src/lib/components/emulator/RegisterInspector.svelte` (200 lines)
```typescript
/**
 * Register and flag inspector
 * - 4 registers (A, B, C, D) with 50-digit display
 * - Flag bits (zero, carry, negative, overflow)
 * - Decimal, hex, binary views
 * - Editable values (for debugging)
 */
```

**File**: `frontend/src/lib/components/emulator/MemoryInspector.svelte` (280 lines)
```typescript
/**
 * Memory viewer and inspector
 * Features:
 * - 2000-word memory display
 * - Hexdump view
 * - ASCII view
 * - Jump to address
 * - Search memory
 * - Highlight modified addresses
 */
```

**File**: `frontend/src/lib/components/emulator/BreakpointManager.svelte` (180 lines)
```typescript
/**
 * Breakpoint management
 * - Add breakpoint (address or condition)
 * - Enable/disable breakpoints
 * - Remove breakpoints
 * - Conditional breakpoints (register value, flag state)
 * - Breakpoint hit count
 */
```

#### APIs to Integrate

**Endpoint**: `POST /api/v1/emulator/debug/breakpoint`
```typescript
interface AddBreakpointRequest {
  execution_id: string;
  type: "address" | "condition";
  address?: number;
  condition?: string; // e.g., "A == 42"
}
```

**Endpoint**: `POST /api/v1/emulator/debug/step`
```typescript
interface StepRequest {
  execution_id: string;
  mode: "step" | "step_over" | "continue";
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/emulator/__tests__/DebuggerPanel.test.ts` (15 tests)
**File**: `frontend/src/lib/components/emulator/__tests__/RegisterInspector.test.ts` (10 tests)
**File**: `frontend/src/lib/components/emulator/__tests__/MemoryInspector.test.ts` (13 tests)

#### Success Criteria
- ✅ Breakpoints trigger correctly
- ✅ Step controls advance execution
- ✅ Registers display accurately
- ✅ Memory view updates on change
- ✅ Conditional breakpoints work

#### Estimated Effort
| Task | Hours |
|------|-------|
| Debugger panel | 4h |
| Register inspector | 3h |
| Memory inspector | 4h |
| Breakpoint manager | 2h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **18h** |

---

### Week 4 Summary

#### Deliverables
- ✅ 11 new components (1,900 lines)
- ✅ Enhanced 3D visualization
- ✅ Debugger UI
- ✅ Register and memory inspectors
- ✅ 65 unit tests

#### Files Created
```
frontend/src/lib/components/emulator/
├── MechanicalVisualization.svelte (400 lines)
├── DebuggerPanel.svelte (350 lines)
├── RegisterInspector.svelte (200 lines)
├── MemoryInspector.svelte (280 lines)
├── BreakpointManager.svelte (180 lines)
└── __tests__/
    ├── MechanicalVisualization.test.ts (15 tests)
    ├── DebuggerPanel.test.ts (15 tests)
    ├── RegisterInspector.test.ts (10 tests)
    └── MemoryInspector.test.ts (13 tests)

frontend/src/lib/3d/components/
├── DigitColumn.ts (200 lines)
└── AnticipatingCarriage.ts (250 lines)

frontend/src/lib/3d/animations/
├── operationAnimations.ts (300 lines)
└── __tests__/
    └── operationAnimations.test.ts (12 tests)
```

#### API Endpoints Used
- `GET /api/v1/emulator/state/{execution_id}`
- `ws://api/v1/emulator/live/{execution_id}`
- `POST /api/v1/emulator/debug/breakpoint`
- `POST /api/v1/emulator/debug/step`

---

## Week 5: User Dashboard & Polish

**Dates**: Week of Dec 23, 2025
**Effort**: 40 hours (5 days × 8 hours)
**Lines of Code**: 1,700 lines
**Components**: 13 new components
**Tests**: 80 tests

### Day 1-2: User Dashboard (Monday-Tuesday)

#### Components to Build

**File**: `frontend/src/lib/components/dashboard/DashboardOverview.svelte` (350 lines)
```typescript
/**
 * User dashboard main view
 * Features:
 * - Welcome message with user name
 * - Overall progress (percentage, hours spent)
 * - Recently accessed modules
 * - Recommended next lessons
 * - Achievement badges
 * - Learning streak (days active)
 * - Weekly activity chart
 */
```

**File**: `frontend/src/lib/components/dashboard/ProgressSummary.svelte` (250 lines)
```typescript
/**
 * Progress summary panel
 * - Modules completed / total
 * - Lessons completed / total
 * - Exercises completed / total
 * - Progress by era (bar chart)
 * - Skills acquired (tag cloud)
 */
```

**File**: `frontend/src/lib/components/dashboard/ActivityChart.svelte` (280 lines)
```typescript
/**
 * Activity visualization (D3.js)
 * - Bar chart: hours per day (last 30 days)
 * - Line chart: cumulative progress
 * - Heatmap: activity calendar (GitHub-style)
 * - Filter by date range
 */
```

**File**: `frontend/src/lib/components/dashboard/AchievementBadges.svelte` (180 lines)
```typescript
/**
 * Achievement badges display
 * - Earned badges (with unlock date)
 * - Locked badges (with unlock criteria)
 * - Badge categories (Completion, Mastery, Exploration, Speed)
 * - Share badge on social media
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/users/me/dashboard`
```typescript
interface DashboardResponse {
  user: {
    id: number;
    name: string;
    joined_date: string;
    last_active: string;
  };
  progress: {
    modules_completed: number;
    modules_total: number;
    lessons_completed: number;
    lessons_total: number;
    exercises_completed: number;
    exercises_total: number;
    total_hours: number;
    overall_percentage: number;
  };
  recent_modules: Module[];
  recommended_lessons: Lesson[];
  activity: {
    date: string;
    hours: number;
    lessons_completed: number;
  }[];
  achievements: Achievement[];
  learning_streak_days: number;
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/dashboard/__tests__/DashboardOverview.test.ts` (18 tests)
**File**: `frontend/src/lib/components/dashboard/__tests__/ProgressSummary.test.ts` (12 tests)
**File**: `frontend/src/lib/components/dashboard/__tests__/ActivityChart.test.ts` (15 tests)

#### Success Criteria
- ✅ Dashboard loads in < 1 second
- ✅ Progress accurate
- ✅ Activity chart updates daily
- ✅ Badges unlock correctly
- ✅ Responsive design

#### Estimated Effort
| Task | Hours |
|------|-------|
| Dashboard overview | 5h |
| Progress summary | 3h |
| Activity chart | 4h |
| Achievement badges | 3h |
| API integration | 2h |
| Unit tests | 3h |
| **Total** | **20h** |

---

### Day 3: Code Submission History (Wednesday)

#### Components to Build

**File**: `frontend/src/lib/components/dashboard/SubmissionHistory.svelte` (300 lines)
```typescript
/**
 * Code submission history
 * Features:
 * - List of all code submissions
 * - Filter by language, module, date
 * - View submission details (code, results)
 * - Re-run previous submission
 * - Compare submissions (diff view)
 * - Export submissions (JSON, CSV)
 */
```

**File**: `frontend/src/lib/components/dashboard/SubmissionDetail.svelte` (220 lines)
```typescript
/**
 * Detailed view of single submission
 * - Submitted code (read-only editor)
 * - Test results
 * - Execution time and memory
 * - Timestamp and lesson
 * - "Re-run" button
 * - "View in playground" button
 */
```

#### APIs to Integrate

**Endpoint**: `GET /api/v1/users/me/submissions`
```typescript
interface SubmissionsResponse {
  submissions: {
    id: number;
    lesson_id: number;
    lesson_title: string;
    submitted_code: string;
    language: string;
    execution_output: string;
    execution_error: string;
    is_successful: boolean;
    created_at: string;
  }[];
  total: number;
}
```

#### Tests to Write

**File**: `frontend/src/lib/components/dashboard/__tests__/SubmissionHistory.test.ts` (10 tests)

#### Success Criteria
- ✅ Submission history loads
- ✅ Filters work correctly
- ✅ Submission detail modal opens
- ✅ Re-run works
- ✅ Export to JSON/CSV

#### Estimated Effort
| Task | Hours |
|------|-------|
| Submission history | 4h |
| Submission detail | 3h |
| API integration | 1h |
| Unit tests | 1h |
| **Total** | **9h** |

---

### Day 4-5: E2E Testing, Performance, Accessibility (Thursday-Friday)

#### E2E Tests to Write

**File**: `frontend/e2e/timeline.spec.ts` (10 tests)
```typescript
describe('Timeline E2E', () => {
  it('loads timeline page')
  it('displays all 8 eras')
  it('zooms timeline on zoom button click')
  it('navigates to era detail on era click')
  it('shows milestone tooltip on hover')
  it('filters milestones by date range')
  it('searches milestones by keyword')
  it('timeline state persists on refresh')
  it('handles API errors gracefully')
  it('works on mobile viewport (responsive)')
});
```

**File**: `frontend/e2e/modules.spec.ts` (12 tests)
```typescript
describe('Modules E2E', () => {
  it('loads module grid')
  it('filters modules by era')
  it('filters modules by difficulty')
  it('searches modules by title')
  it('navigates to module detail')
  it('loads lesson content')
  it('marks lesson as complete')
  it('submits exercise code')
  it('views test results')
  it('navigates to next lesson')
  it('module progress updates')
  it('handles backend errors')
});
```

**File**: `frontend/e2e/playground.spec.ts` (10 tests)
```typescript
describe('Playground E2E', () => {
  it('loads code playground')
  it('switches language')
  it('loads example code')
  it('executes code successfully')
  it('displays output in console')
  it('shows error messages')
  it('shares code URL')
  it('saves code to local storage')
  it('restores code on refresh')
  it('handles execution timeout')
});
```

**File**: `frontend/e2e/emulator.spec.ts` (8 tests)
```typescript
describe('Emulator E2E', () => {
  it('loads emulator page')
  it('loads 3D visualization')
  it('executes assembly code')
  it('animates mechanical components')
  it('sets breakpoint')
  it('steps through execution')
  it('inspects registers')
  it('views memory')
});
```

#### Performance Optimization

**Tasks**:
1. **Code Splitting** (2h)
   - Split routes into separate bundles
   - Lazy load Monaco editor
   - Lazy load Three.js on emulator page

2. **Asset Optimization** (2h)
   - Compress images (WebP format)
   - Minify CSS/JS
   - Tree-shake unused dependencies

3. **Caching Strategy** (2h)
   - Service worker for offline support
   - Cache API responses (Redis)
   - Cache-Control headers

4. **Bundle Analysis** (1h)
   - Use `vite-plugin-bundle-analyzer`
   - Identify large dependencies
   - Optimize imports

#### Accessibility Audit

**Tasks**:
1. **WCAG 2.1 AA Compliance** (3h)
   - Color contrast ratios (4.5:1 for text)
   - Keyboard navigation for all interactive elements
   - ARIA labels and roles
   - Focus management

2. **Screen Reader Testing** (2h)
   - Test with NVDA (Windows)
   - Test with VoiceOver (macOS)
   - Ensure all content announced

3. **Semantic HTML** (1h)
   - Proper heading hierarchy
   - Landmark regions (header, nav, main, footer)
   - Alt text for images

#### Tests to Write

**Total E2E Tests**: 40 tests (10 + 12 + 10 + 8)

#### Success Criteria
- ✅ All E2E tests pass
- ✅ Lighthouse score: 90+ (Performance, Accessibility, Best Practices, SEO)
- ✅ Bundle size < 500KB (gzipped)
- ✅ Time to Interactive < 3s (on 4G)
- ✅ WCAG 2.1 AA compliant

#### Estimated Effort
| Task | Hours |
|------|-------|
| E2E tests (40 tests) | 8h |
| Performance optimization | 7h |
| Accessibility audit | 6h |
| Bug fixes | 3h |
| **Total** | **24h** |

---

### Week 5 Summary

#### Deliverables
- ✅ 13 new components (1,700 lines)
- ✅ User dashboard
- ✅ Submission history
- ✅ 40 E2E tests
- ✅ Performance optimizations
- ✅ Accessibility audit

#### Files Created
```
frontend/src/lib/components/dashboard/
├── DashboardOverview.svelte (350 lines)
├── ProgressSummary.svelte (250 lines)
├── ActivityChart.svelte (280 lines)
├── AchievementBadges.svelte (180 lines)
├── SubmissionHistory.svelte (300 lines)
├── SubmissionDetail.svelte (220 lines)
└── __tests__/
    ├── DashboardOverview.test.ts (18 tests)
    ├── ProgressSummary.test.ts (12 tests)
    ├── ActivityChart.test.ts (15 tests)
    └── SubmissionHistory.test.ts (10 tests)

frontend/e2e/
├── timeline.spec.ts (10 tests)
├── modules.spec.ts (12 tests)
├── playground.spec.ts (10 tests)
└── emulator.spec.ts (8 tests)
```

#### API Endpoints Used
- `GET /api/v1/users/me/dashboard`
- `GET /api/v1/users/me/submissions`

---

## Testing Strategy

### Test Coverage Targets

| Test Type | Current | Target | Increase |
|-----------|---------|--------|----------|
| **Unit Tests** | 850 | 1,200 | +350 |
| **Integration Tests** | 150 | 250 | +100 |
| **E2E Tests** | 19 | 40 | +21 |
| **Total Tests** | 1,019 | 1,490 | +471 |

### Testing Pyramid

```
        E2E Tests (40)
       /              \
      /                \
  Integration (250)
    /                    \
   /                      \
 Unit Tests (1,200)
```

### Test Breakdown by Week

| Week | Unit Tests | Integration | E2E | Total |
|------|-----------|-------------|-----|-------|
| **Week 1: Timeline** | 75 | 15 | 5 | 95 |
| **Week 2: Modules** | 85 | 20 | 6 | 111 |
| **Week 3: Playground** | 70 | 25 | 5 | 100 |
| **Week 4: Emulator** | 65 | 20 | 4 | 89 |
| **Week 5: Dashboard** | 55 | 20 | 21 | 96 |
| **Total** | **350** | **100** | **41** | **491** |

### Unit Testing Guidelines

**Framework**: Vitest + Testing Library

**Example Test Structure**:
```typescript
import { render, fireEvent } from '@testing-library/svelte';
import { describe, it, expect, vi } from 'vitest';
import ComponentName from '../ComponentName.svelte';

describe('ComponentName', () => {
  it('renders correctly with props', () => {
    const { getByText } = render(ComponentName, { props: { title: 'Test' } });
    expect(getByText('Test')).toBeInTheDocument();
  });

  it('handles user interaction', async () => {
    const { getByRole } = render(ComponentName);
    const button = getByRole('button');
    await fireEvent.click(button);
    expect(button).toHaveAttribute('aria-pressed', 'true');
  });

  it('calls API on submit', async () => {
    const mockApi = vi.fn().mockResolvedValue({ success: true });
    const { getByRole } = render(ComponentName, { props: { onSubmit: mockApi } });
    await fireEvent.submit(getByRole('form'));
    expect(mockApi).toHaveBeenCalled();
  });
});
```

### Integration Testing Guidelines

**Focus**: Component interaction, state management, API integration

**Example**:
```typescript
describe('Module Grid Integration', () => {
  it('filters modules and updates URL query params', async () => {
    const { getByLabelText } = render(ModuleGrid);

    // Change era filter
    const eraSelect = getByLabelText('Era');
    await fireEvent.change(eraSelect, { target: { value: 'ancient' } });

    // Verify URL updated
    expect(window.location.search).toContain('era=ancient');

    // Verify API called with filter
    expect(mockApi).toHaveBeenCalledWith('/api/v1/modules?era=ancient');
  });
});
```

### E2E Testing Guidelines

**Framework**: Playwright

**Example**:
```typescript
import { test, expect } from '@playwright/test';

test.describe('Timeline Page', () => {
  test('user can navigate timeline and view era details', async ({ page }) => {
    // Navigate to timeline
    await page.goto('/timeline');

    // Wait for timeline to load
    await expect(page.locator('svg.timeline-svg')).toBeVisible();

    // Click era marker
    await page.locator('[data-era-id="ancient"]').click();

    // Verify era detail panel opens
    await expect(page.locator('.era-detail-panel')).toBeVisible();
    await expect(page.locator('.era-detail-panel h3')).toContainText('Ancient Foundations');

    // Verify modules listed
    const moduleCards = page.locator('.module-card');
    await expect(moduleCards).toHaveCount.greaterThan(0);
  });
});
```

---

## Performance Benchmarks

### Page Load Performance Targets

| Page | First Contentful Paint | Time to Interactive | Lighthouse Score |
|------|------------------------|---------------------|------------------|
| **Home** | < 1.0s | < 2.0s | 95+ |
| **Timeline** | < 1.5s | < 3.0s | 90+ |
| **Modules** | < 1.2s | < 2.5s | 92+ |
| **Playground** | < 2.0s | < 3.5s | 88+ (Monaco heavy) |
| **Emulator** | < 2.5s | < 4.0s | 85+ (Three.js heavy) |
| **Dashboard** | < 1.0s | < 2.0s | 95+ |

### Bundle Size Targets

| Bundle | Size (gzipped) | Notes |
|--------|---------------|-------|
| **Main Bundle** | < 150 KB | Core app shell |
| **Monaco Editor** | < 250 KB | Lazy loaded |
| **Three.js** | < 200 KB | Lazy loaded |
| **D3.js** | < 50 KB | Tree-shaken |
| **Total (initial)** | < 200 KB | Without lazy chunks |
| **Total (full)** | < 600 KB | All assets loaded |

### Runtime Performance Targets

| Metric | Target | Measurement |
|--------|--------|-------------|
| **Timeline Render** | < 100ms | 12,500 years rendered |
| **Module Grid** | < 50ms | 100 modules rendered |
| **Monaco Editor Load** | < 500ms | Editor ready for input |
| **3D Scene FPS** | 60 fps | Smooth animations |
| **Code Execution** | < 2s | Round-trip API call |
| **Memory Usage** | < 200 MB | Chrome DevTools |

### Optimization Strategies

1. **Code Splitting**
   ```javascript
   // Route-based splitting
   const Timeline = () => import('./routes/timeline/+page.svelte');
   const Playground = () => import('./routes/playground/+page.svelte');
   ```

2. **Lazy Loading**
   ```javascript
   // Monaco Editor
   let monaco;
   onMount(async () => {
     monaco = await import('monaco-editor');
   });
   ```

3. **Virtual Scrolling**
   ```javascript
   // For long lists (module grid, memory inspector)
   import VirtualList from 'svelte-virtual-list';
   ```

4. **Image Optimization**
   ```html
   <!-- Use WebP with fallback -->
   <picture>
     <source srcset="image.webp" type="image/webp">
     <img src="image.jpg" alt="Description">
   </picture>
   ```

5. **Caching**
   ```javascript
   // Service Worker cache strategy
   workbox.routing.registerRoute(
     /\/api\/v1\/modules/,
     new workbox.strategies.CacheFirst({
       cacheName: 'api-cache',
       plugins: [
         new workbox.expiration.ExpirationPlugin({
           maxEntries: 50,
           maxAgeSeconds: 60 * 60 * 24, // 24 hours
         }),
       ],
     })
   );
   ```

---

## Accessibility Requirements

### WCAG 2.1 AA Compliance Checklist

#### Perceivable

- [ ] **1.1 Text Alternatives**: All images have alt text
- [ ] **1.2 Time-based Media**: Captions for videos (N/A - no videos)
- [ ] **1.3 Adaptable**: Content structure is semantic (headings, lists, tables)
- [ ] **1.4 Distinguishable**:
  - [ ] Color contrast 4.5:1 for text, 3:1 for UI components
  - [ ] Text resizable to 200% without loss of functionality
  - [ ] No reliance on color alone for information

#### Operable

- [ ] **2.1 Keyboard Accessible**:
  - [ ] All functionality available via keyboard
  - [ ] No keyboard traps
  - [ ] Keyboard shortcuts documented
- [ ] **2.2 Enough Time**:
  - [ ] No time limits (or adjustable/disableable)
  - [ ] Pause/stop for auto-updating content
- [ ] **2.3 Seizures**: No flashing content > 3 times per second
- [ ] **2.4 Navigable**:
  - [ ] Skip links for repeated content
  - [ ] Page titles descriptive
  - [ ] Focus order logical
  - [ ] Link purpose clear from context
  - [ ] Multiple ways to find pages (nav, sitemap, search)

#### Understandable

- [ ] **3.1 Readable**:
  - [ ] Language of page specified (lang="en")
  - [ ] Language of parts specified if different
- [ ] **3.2 Predictable**:
  - [ ] Navigation consistent across pages
  - [ ] Components behave consistently
  - [ ] No unexpected context changes
- [ ] **3.3 Input Assistance**:
  - [ ] Error messages clear and helpful
  - [ ] Labels or instructions for inputs
  - [ ] Error suggestions provided

#### Robust

- [ ] **4.1 Compatible**:
  - [ ] Valid HTML
  - [ ] ARIA roles, states, properties correct
  - [ ] Status messages announced to assistive tech

### Keyboard Shortcuts

| Shortcut | Action | Scope |
|----------|--------|-------|
| **Tab** | Next focusable element | Global |
| **Shift+Tab** | Previous focusable element | Global |
| **Enter** | Activate button/link | Global |
| **Space** | Toggle checkbox/button | Global |
| **Esc** | Close modal/dialog | Global |
| **Arrow Keys** | Navigate timeline/grid | Timeline, Module Grid |
| **Ctrl+Enter** | Run code | Playground |
| **Ctrl+/** | Toggle comment | Playground |
| **Ctrl+F** | Find in editor | Playground |
| **F9** | Toggle breakpoint | Emulator |
| **F10** | Step over | Emulator |
| **F11** | Step into | Emulator |

### Screen Reader Testing

**Tools**:
- **NVDA** (Windows): Free, popular
- **JAWS** (Windows): Industry standard (paid)
- **VoiceOver** (macOS): Built-in

**Test Cases**:
1. Navigate timeline using screen reader
2. Filter modules by era (announced correctly)
3. Submit code exercise (feedback announced)
4. View emulator state (registers announced)

### Focus Management

**Guidelines**:
- Focus should be visible (outline: 2px solid)
- Focus order follows visual layout
- Modal dialogs trap focus (Esc to close)
- On modal close, focus returns to trigger element

**Example**:
```typescript
// Focus management for modal
let modalOpen = false;
let triggerElement: HTMLElement;

function openModal(event: Event) {
  triggerElement = event.target as HTMLElement;
  modalOpen = true;
  // Wait for modal to render, then focus first element
  tick().then(() => {
    modalElement.querySelector('button')?.focus();
  });
}

function closeModal() {
  modalOpen = false;
  // Return focus to trigger
  tick().then(() => triggerElement.focus());
}
```

---

## Deployment Checklist

### Pre-Deployment

#### Code Quality
- [ ] All unit tests pass (1,200+ tests)
- [ ] All integration tests pass (250+ tests)
- [ ] All E2E tests pass (40+ tests)
- [ ] Linter warnings resolved (eslint --max-warnings 0)
- [ ] TypeScript errors resolved (tsc --noEmit)
- [ ] Code coverage > 90%

#### Performance
- [ ] Lighthouse scores > 90 (Performance, Accessibility, Best Practices, SEO)
- [ ] Bundle size < 500KB (gzipped)
- [ ] Page load times meet targets
- [ ] No memory leaks (Chrome DevTools)
- [ ] Images optimized (WebP format)

#### Accessibility
- [ ] WCAG 2.1 AA compliant
- [ ] Screen reader tested (NVDA, VoiceOver)
- [ ] Keyboard navigation works
- [ ] Color contrast verified
- [ ] Focus management correct

#### Security
- [ ] Dependencies updated (npm audit fix)
- [ ] No XSS vulnerabilities
- [ ] CSRF protection enabled
- [ ] Content Security Policy (CSP) configured
- [ ] HTTPS enforced

#### Documentation
- [ ] README.md updated
- [ ] API documentation complete
- [ ] Component Storybook stories (optional)
- [ ] Deployment guide written
- [ ] Changelog updated

### Deployment Steps

#### 1. Build Production Assets (5 min)
```bash
# Frontend build
cd frontend
npm run build

# Backend build (if needed)
cd ../backend
poetry build
```

#### 2. Docker Image Build (10 min)
```bash
# Build all service images
docker-compose -f docker-compose.prod.yml build

# Tag images
docker tag ancient-compute-frontend:latest ancient-compute-frontend:v1.0.0
docker tag ancient-compute-backend:latest ancient-compute-backend:v1.0.0
```

#### 3. Database Migration (2 min)
```bash
# Run migrations
docker-compose -f docker-compose.prod.yml run backend alembic upgrade head

# Seed database (if needed)
docker-compose -f docker-compose.prod.yml run backend python -m src.seeder
```

#### 4. Deploy to Staging (15 min)
```bash
# Deploy to staging environment
docker-compose -f docker-compose.staging.yml up -d

# Verify deployment
curl https://staging.ancient-compute.io/api/v1/health
```

#### 5. Smoke Tests (10 min)
```bash
# Run smoke tests against staging
npm run test:e2e -- --config=playwright.staging.config.ts
```

#### 6. Deploy to Production (15 min)
```bash
# Deploy to production
docker-compose -f docker-compose.prod.yml up -d

# Verify deployment
curl https://ancient-compute.io/api/v1/health

# Monitor logs
docker-compose -f docker-compose.prod.yml logs -f
```

#### 7. Post-Deployment Validation (10 min)
- [ ] Homepage loads correctly
- [ ] Timeline visualization works
- [ ] Code playground executes code
- [ ] Emulator runs
- [ ] User login/registration works
- [ ] Database accessible
- [ ] Monitoring dashboards show healthy metrics

### Rollback Plan

If issues detected:
```bash
# Rollback to previous version
docker-compose -f docker-compose.prod.yml down
docker tag ancient-compute-frontend:v0.9.0 ancient-compute-frontend:latest
docker tag ancient-compute-backend:v0.9.0 ancient-compute-backend:latest
docker-compose -f docker-compose.prod.yml up -d

# Rollback database (if needed)
docker-compose -f docker-compose.prod.yml run backend alembic downgrade -1
```

### Monitoring

**Metrics to Monitor**:
- Response times (p50, p95, p99)
- Error rates (4xx, 5xx)
- Request throughput (req/s)
- Database query times
- Memory usage
- CPU usage
- Disk I/O

**Alerting**:
- Error rate > 5% → Page on-call
- Response time p95 > 2s → Warning
- CPU usage > 80% for 5 min → Warning
- Memory usage > 90% → Critical

---

## Risk Mitigation

### Risk Matrix

| Risk | Likelihood | Impact | Severity | Mitigation |
|------|-----------|--------|----------|------------|
| **D3.js learning curve** | Medium | Medium | Medium | Use examples from Observable, allocate extra time |
| **Monaco editor performance** | Low | High | Medium | Lazy load, code split, test on low-end devices |
| **Three.js performance** | Medium | High | High | Optimize geometry, use LOD, test on mobile |
| **Backend API downtime** | Low | High | Medium | Graceful error handling, retry logic, fallback data |
| **Database not seeded** | High | Medium | Medium | Mock data for development, create seeder script |
| **Browser compatibility** | Low | Medium | Low | Test on Chrome, Firefox, Safari, Edge |
| **Accessibility issues** | Medium | High | High | Regular audits, screen reader testing |
| **Performance regression** | Medium | Medium | Medium | Performance budgets, CI/CD benchmarks |
| **Timeline data complexity** | High | Medium | Medium | Simplify scale, use logarithmic transformation |
| **E2E test flakiness** | Medium | Low | Low | Retry failed tests, explicit waits, isolate tests |

### Mitigation Strategies

#### 1. D3.js Learning Curve
**Risk**: Team unfamiliar with D3.js advanced features (scales, transitions)
**Mitigation**:
- Allocate 2 extra hours for learning
- Use Observable examples as reference
- Pair programming for complex visualizations

#### 2. Monaco Editor Performance
**Risk**: Monaco editor slow to load (250KB bundle)
**Mitigation**:
- Lazy load Monaco on playground page only
- Code split by language (only load needed language support)
- Use Web Workers for syntax highlighting
- Test on low-end devices (4GB RAM)

#### 3. Three.js Performance
**Risk**: 3D visualization laggy on low-end devices
**Mitigation**:
- Use Level of Detail (LOD) for complex meshes
- Limit geometry complexity (< 10,000 triangles)
- Implement quality settings (Low, Medium, High)
- Test on integrated graphics (Intel HD)
- Fallback to 2D view on mobile

#### 4. Backend API Downtime
**Risk**: Backend unavailable during frontend development
**Mitigation**:
- Mock API responses for development
- Use MSW (Mock Service Worker) for testing
- Graceful error handling (retry with exponential backoff)
- Offline fallback data

#### 5. Database Not Seeded
**Risk**: Timeline/module data missing in database
**Mitigation**:
- Use mock data for development
- Create seeder script Week 1 Day 1
- Seeder populates 8 eras, 50 modules, 200 lessons, 30 milestones
- Document seeder usage in README

#### 6. Browser Compatibility
**Risk**: Features break in Safari or Firefox
**Mitigation**:
- Test on Chrome, Firefox, Safari, Edge weekly
- Use Babel for ES6+ transpilation
- Polyfills for missing features
- Autoprefixer for CSS

#### 7. Accessibility Issues
**Risk**: WCAG violations discovered late
**Mitigation**:
- Weekly accessibility audits (axe-core)
- Screen reader testing every 2 weeks
- Keyboard navigation tested on all components
- Color contrast checked in design phase

#### 8. Performance Regression
**Risk**: New features slow down app
**Mitigation**:
- Performance budgets in CI/CD
- Lighthouse CI on pull requests
- Bundle size monitoring (bundlesize.io)
- Weekly performance testing

#### 9. Timeline Data Complexity
**Risk**: 12,500 years of data hard to visualize
**Mitigation**:
- Use logarithmic scale for ancient dates
- Separate scales for ancient (< 0) and modern (> 1500)
- Virtual scrolling for milestone markers
- Simplify data model if needed

#### 10. E2E Test Flakiness
**Risk**: E2E tests fail intermittently
**Mitigation**:
- Explicit waits (await page.waitForSelector)
- Retry failed tests (3 attempts)
- Isolate tests (no shared state)
- Use Playwright's auto-wait features

---

## Summary: Phase 4 Completion Roadmap

### Timeline Overview

```
Week 1 (Nov 25-29): Timeline Visualization (D3.js)
  └─ 2,100 lines, 8 components, 75 tests

Week 2 (Dec 2-6): Educational Module System
  └─ 2,200 lines, 12 components, 85 tests

Week 3 (Dec 9-13): Code Playground
  └─ 2,000 lines, 10 components, 70 tests

Week 4 (Dec 16-20): Emulator Visualization Enhancements
  └─ 1,900 lines, 11 components, 65 tests

Week 5 (Dec 23-27): User Dashboard & Polish
  └─ 1,700 lines, 13 components, 80 tests + 40 E2E
```

### Total Deliverables

| Metric | Current | Target | Added |
|--------|---------|--------|-------|
| **Lines of Code** | 6,561 | 16,000 | **9,439** |
| **Components** | 19 | 73 | **54** |
| **Unit Tests** | 850 | 1,225 | **375** |
| **Integration Tests** | 150 | 250 | **100** |
| **E2E Tests** | 19 | 59 | **40** |

### Success Criteria

- ✅ **Phase 4 Completion**: 80% → 100%
- ✅ **All 54 components** implemented and tested
- ✅ **515 new tests** written and passing
- ✅ **Performance**: Lighthouse score > 90
- ✅ **Accessibility**: WCAG 2.1 AA compliant
- ✅ **Deployment**: Production-ready by Dec 24, 2025

### Next Steps After Phase 4

**Phase 5: Educational Content Production** (8 weeks)
- Seed database with full curriculum (7 volumes, 200+ lessons)
- Create 100+ exercises with automated validation
- Generate LaTeX documentation (PDFs)
- Finalize historical accuracy review

**Phase 6: Advanced Features** (8 weeks)
- User authentication and authorization
- Multi-user learning paths
- Achievements and gamification
- Social features (code sharing, discussions)

**Phase 7: Research Extensions** (10 weeks)
- Formal verification integration (Coq, Lean)
- Quantum computing module
- Additional language services (APL, Prolog, Agda)

---

## Appendix A: File Structure After Phase 4

```
frontend/
├── src/
│   ├── routes/
│   │   ├── +page.svelte (home)
│   │   ├── +layout.svelte
│   │   ├── timeline/
│   │   │   └── +page.svelte
│   │   ├── modules/
│   │   │   ├── +page.svelte (grid)
│   │   │   └── [slug]/
│   │   │       └── +page.svelte (detail)
│   │   ├── lessons/
│   │   │   └── [id]/
│   │   │       └── +page.svelte
│   │   ├── playground/
│   │   │   └── +page.svelte
│   │   ├── emulator/
│   │   │   ├── +page.svelte
│   │   │   └── 3d/
│   │   │       └── +page.svelte
│   │   ├── dashboard/
│   │   │   └── +page.svelte
│   │   └── about/
│   │       └── +page.svelte
│   ├── lib/
│   │   ├── components/
│   │   │   ├── timeline/ (8 components, 1,850 lines)
│   │   │   ├── education/ (12 components, 2,200 lines)
│   │   │   ├── playground/ (10 components, 2,000 lines)
│   │   │   ├── emulator/ (11 components, 1,900 lines)
│   │   │   ├── dashboard/ (13 components, 1,700 lines)
│   │   │   ├── visualization/ (3 existing)
│   │   │   └── common/ (2 existing)
│   │   ├── stores/ (timeline, user, module stores)
│   │   ├── api/ (client.ts, types.ts)
│   │   ├── visualization/
│   │   │   ├── d3/ (timeline scales, transitions)
│   │   │   └── performance/ (quality, device tier)
│   │   ├── 3d/
│   │   │   ├── components/ (DigitColumn, AnticipatingCarriage)
│   │   │   └── animations/ (operation animations)
│   │   ├── playground/ (monaco, language providers, examples)
│   │   └── markdown/ (renderer, syntax highlighting)
│   └── app.html
├── e2e/ (40 E2E tests)
├── tests/ (1,225 unit tests)
└── package.json

Total: 73 components, 16,000 lines, 1,515 tests
```

---

## Appendix B: API Endpoints Reference

### Timeline API
```
GET  /api/v1/timeline/full
GET  /api/v1/timeline/eras
GET  /api/v1/timeline/eras/{era_id}
GET  /api/v1/timeline/milestones?start_year={start}&end_year={end}
```

### Modules API
```
GET  /api/v1/modules
GET  /api/v1/modules?era={era}&difficulty_min={min}&difficulty_max={max}&status={status}
GET  /api/v1/modules/{slug}
```

### Lessons API
```
GET  /api/v1/lessons/{lesson_id}
POST /api/v1/lessons/{lesson_id}/complete
```

### Exercises API
```
GET  /api/v1/exercises/{exercise_id}
POST /api/v1/exercises/{exercise_id}/submit
```

### Code Execution API
```
POST /api/v1/execute/run
GET  /api/v1/execute/languages
```

### Emulator API
```
GET  /api/v1/emulator/state/{execution_id}
WS   ws://api/v1/emulator/live/{execution_id}
POST /api/v1/emulator/debug/breakpoint
POST /api/v1/emulator/debug/step
```

### User API
```
GET  /api/v1/users/me/dashboard
GET  /api/v1/users/me/submissions
```

---

## Appendix C: Component Dependencies

### npm Dependencies to Add

```json
{
  "dependencies": {
    // Existing
    "d3": "^7.8.5",
    "monaco-editor": "^0.44.0",
    "three": "^0.158.0",

    // NEW - Week 2
    "marked": "^11.0.0",
    "prismjs": "^1.29.0",
    "katex": "^0.16.9",
    "mermaid": "^10.6.0",

    // NEW - Week 5 (optional)
    "svelte-virtual-list": "^1.4.0",
    "qrcode": "^1.5.3"
  },
  "devDependencies": {
    // NEW
    "@types/marked": "^6.0.0",
    "vite-plugin-bundle-analyzer": "^1.0.0",
    "workbox-cli": "^7.0.0"
  }
}
```

---

**END OF PHASE 4 EXECUTION ROADMAP**

**Total Pages**: 45
**Total Lines**: 1,950
**Estimated Reading Time**: 30 minutes
**Estimated Execution Time**: 5 weeks (200 hours)

**Document Status**: READY FOR EXECUTION
**Next Action**: Begin Week 1, Day 1 (Timeline D3.js Component)
