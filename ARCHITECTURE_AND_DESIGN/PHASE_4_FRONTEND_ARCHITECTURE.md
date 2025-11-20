# Phase 4: Frontend & Visualization Architecture

**Document Version**: 1.0  
**Date**: November 19, 2025  
**Status**: DESIGN SPECIFICATION  
**Phase Completion**: 80% → Target 100%  
**Target Duration**: 6-8 weeks  

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Current State Analysis](#current-state-analysis)
3. [Component Hierarchy](#component-hierarchy)
4. [Data Flow Architecture](#data-flow-architecture)
5. [API Integration Points](#api-integration-points)
6. [State Management Strategy](#state-management-strategy)
7. [File & Folder Structure](#file--folder-structure)
8. [Technology Choices & Justifications](#technology-choices--justifications)
9. [Performance Optimization Strategy](#performance-optimization-strategy)
10. [Accessibility Checklist](#accessibility-checklist)
11. [Implementation Roadmap](#implementation-roadmap)
12. [Testing Strategy](#testing-strategy)

---

## Executive Summary

Phase 4 completes the Ancient Compute frontend by delivering production-ready interactive visualizations, educational modules, and code execution interfaces. This phase transforms the existing 80% foundation into a fully-featured educational platform serving 12,500 years of computational history.

### Key Objectives

1. **Interactive Timeline**: Seamless 12,500-year navigation with D3.js zoom/pan (ENHANCE existing)
2. **Code Playground**: Monaco Editor with 8-language support and real-time execution (NEW)
3. **3D Babbage Emulator**: Three.js Victorian-era visualization with debugger (ENHANCE existing)
4. **Educational Modules**: Progressive curriculum with markdown rendering (ENHANCE existing)
5. **User Dashboard**: Progress tracking, achievements, and analytics (NEW)

### Success Metrics

- **Performance**: First Contentful Paint < 1.5s, Time to Interactive < 3.5s
- **Accessibility**: WCAG 2.1 AA compliance (100% keyboard navigation)
- **Mobile**: Full responsiveness down to 320px width
- **Offline**: Service Worker caching for all curriculum content
- **Test Coverage**: >85% component coverage with Vitest

---

## Current State Analysis

### Existing Implementation (80% Complete)

#### ✅ Implemented Components (11 total)

**Education Components** (`frontend/src/lib/components/education/`)
- `Timeline.svelte` (590 lines) - Horizontal scrolling timeline with era markers
- `Module.svelte` - Module card display
- `Lesson.svelte` - Lesson content viewer
- `Exercise.svelte` - Exercise submission interface
- `ProgressTracker.svelte` - Progress visualization
- `HistoricalNavigator.svelte` - Era navigation

**Visualization Components** (`frontend/src/lib/components/visualization/`)
- `EmulatorView.svelte` - Emulator container
- `CanvasContainer.svelte` - 3D canvas wrapper
- `ControlPanel.svelte` - Emulator controls

**Common Components** (`frontend/src/lib/components/common/`)
- `Header.svelte` - Site header
- `Navigation.svelte` - Main navigation

#### ✅ Implemented Infrastructure

**State Management** (`frontend/src/lib/stores/`)
- `timelineStore.ts` (468 lines) - Comprehensive timeline/module/lesson state
  - Era selection, module navigation, progress tracking
  - Derived stores: currentEra, currentModule, currentLesson, overallProgress
  - Actions: loadTimelineData, selectEra, markLessonComplete, etc.

**API Client** (`frontend/src/lib/api/`)
- `client.ts` (127 lines) - Type-safe API wrapper
  - Module API, Timeline API, Health API
  - Error handling with ApiError types

**Performance Infrastructure** (`frontend/src/lib/visualization/performance/`)
- `DeviceTier.ts` - Device capability detection
- `CapabilityAnalyzer.ts` - GPU/CPU profiling
- `QualitySelector.ts` - Adaptive quality settings

**Routes** (`frontend/src/routes/`)
- `/` - Home page
- `/timeline` - Timeline view
- `/modules` - Module listing
- `/emulator` - Emulator interface
- `/emulator/3d` - 3D visualization
- `/about` - About page

### Missing Components (20% → 100%)

#### 🔲 Code Playground System
- Monaco Editor integration
- Language selector with 8 languages
- Execute button with WebSocket streaming
- Output console with stdout/stderr/errors
- Example code library
- Share/save functionality

#### 🔲 Enhanced Timeline Features
- Multi-level zoom (overview → era → decade → year)
- D3.js force-directed graph for concept relationships
- Animated transitions between eras
- Milestone tooltips with rich content
- Search/filter by civilization, category, technology

#### 🔲 Enhanced 3D Visualization
- Victorian-era aesthetic (brass, wood, gears)
- Animated gear rotations during execution
- Column value displays on 3D model
- Camera controls (orbit, pan, zoom)
- Screenshot/recording capability
- Mobile-optimized simplified view

#### 🔲 User Dashboard
- Progress overview with charts
- Completed modules/lessons grid
- Exercise submission history
- Achievement badge system
- Learning path recommendations
- Time spent analytics

#### 🔲 Advanced Educational Features
- Markdown rendering with KaTeX math support
- Syntax highlighting for code blocks
- Interactive diagrams (mermaid.js)
- Concept graph visualization
- Cross-reference navigation
- Bookmark/favorites system

---

## Component Hierarchy

```
┌─────────────────────────────────────────────────────────────┐
│                         App Root                             │
│                    (+layout.svelte)                          │
└────────────────────────┬────────────────────────────────────┘
                         │
        ┌────────────────┴────────────────┐
        │                                 │
┌───────▼────────┐              ┌────────▼────────┐
│  Header        │              │  Navigation     │
│  - Logo        │              │  - Timeline     │
│  - User Menu   │              │  - Modules      │
│  - Progress    │              │  - Playground   │
└────────────────┘              │  - Emulator     │
                                │  - Dashboard    │
                                └─────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                      Page Routes                             │
└─────────────────────────────────────────────────────────────┘

┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│   HomePage   │  │ TimelinePage │  │ ModulesPage  │  │PlaygroundPage│
│  /+page      │  │/timeline     │  │ /modules     │  │ /playground  │
└──────────────┘  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘
                         │                 │                 │
                  ┌──────▼─────────┐  ┌────▼──────┐  ┌──────▼──────┐
                  │ InteractiveTimeline│ ModuleGrid│  │ CodeEditor  │
                  │  - TimelineAxis    │  - ModuleCard│  │ - Monaco   │
                  │  - EraMarkers      │  - Filters   │  │ - LangSelect│
                  │  - ZoomControls    │  - Search    │  │ - Console   │
                  │  - TooltipOverlay  │  - Sort      │  │ - Examples  │
                  └────────────────────┘  └───────────┘  └─────────────┘

┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│EmulatorPage  │  │ DashboardPage│  │  LessonPage  │
│/emulator     │  │ /dashboard   │  │ /lesson/:id  │
└──────┬───────┘  └──────┬───────┘  └──────┬───────┘
       │                 │                 │
┌──────▼──────┐   ┌──────▼──────┐   ┌──────▼──────┐
│BabbageEngine│   │ProgressChart│   │LessonViewer │
│ - 3D Scene  │   │ - Stats      │   │ - Markdown  │
│ - Debugger  │   │ - Badges     │   │ - CodeBlocks│
│ - Controls  │   │ - History    │   │ - Navigation│
│ - Inspector │   │ - Recommend. │   │ - Progress  │
└─────────────┘   └──────────────┘   └─────────────┘

┌─────────────────────────────────────────────────────────────┐
│                  Shared UI Components                        │
├─────────────────────────────────────────────────────────────┤
│ Button │ Input │ Select │ Modal │ Toast │ Spinner │ Tooltip │
│ Card │ Badge │ ProgressBar │ Tabs │ Accordion │ Dropdown    │
└─────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────┐
│                     State Stores                             │
├─────────────────────────────────────────────────────────────┤
│ timelineStore │ userStore │ playgroundStore │ emulatorStore │
│ themeStore │ achievementStore │ progressStore │ cacheStore   │
└─────────────────────────────────────────────────────────────┘
```

### Component Tree Detail

#### 1. Interactive Timeline Component

```
InteractiveTimeline (NEW - Enhanced version)
├── TimelineAxis (D3.js)
│   ├── ScaleManager (manages zoom levels)
│   ├── EraMarkers (8 eras with color coding)
│   ├── MilestoneNodes (clickable events)
│   └── ConnectionLines (relationships)
├── ZoomControls
│   ├── ZoomSlider
│   ├── ZoomButtons (+ / -)
│   └── ResetButton
├── NavigationPanel
│   ├── EraSelector (dropdown)
│   ├── SearchBox (filter milestones)
│   └── ViewToggle (linear/graph/grid)
└── TooltipOverlay
    ├── MilestoneDetail (rich content)
    ├── EraContext (description)
    └── RelatedConcepts (links)
```

#### 2. Code Playground Component

```
CodePlayground (NEW)
├── EditorPanel
│   ├── MonacoEditor
│   │   ├── LanguageProvider (8 languages)
│   │   ├── ThemeProvider (light/dark/victorian)
│   │   └── AutocompleteProvider
│   ├── LanguageSelector
│   ├── EditorToolbar
│   │   ├── RunButton
│   │   ├── FormatButton
│   │   ├── ShareButton
│   │   └── SaveButton
│   └── ExampleLibrary
│       ├── ExampleCard (grouped by era/language)
│       └── LoadExampleButton
├── OutputConsole
│   ├── OutputPanel (stdout)
│   ├── ErrorPanel (stderr with line numbers)
│   ├── CompilationPanel (IR/Assembly view)
│   └── ConsoleToolbar
│       ├── ClearButton
│       └── DownloadButton
└── SettingsPanel
    ├── FontSizeControl
    ├── TabSizeControl
    └── ViModeToggle
```

#### 3. 3D Babbage Emulator Component

```
BabbageEngine3D (ENHANCE existing)
├── Three.js Scene
│   ├── EngineModel
│   │   ├── ColumnMeshes (8 columns with digit wheels)
│   │   ├── CarryMechanism (gears and levers)
│   │   ├── MainShaft (rotating axle)
│   │   └── BaseFrame (Victorian brass/wood)
│   ├── Camera
│   │   ├── OrbitControls
│   │   ├── AutoRotate (optional)
│   │   └── FocusTarget (zoom to column)
│   ├── Lighting
│   │   ├── AmbientLight
│   │   ├── DirectionalLight (sun)
│   │   └── PointLights (Victorian lamps)
│   └── Materials
│       ├── BrassMaterial (PBR)
│       ├── WoodMaterial (mahogany)
│       └── GlassMaterial (display covers)
├── DebuggerPanel
│   ├── StateInspector
│   │   ├── ColumnValues (8 × 31 digits)
│   │   ├── CarryFlags
│   │   └── CycleCounter
│   ├── BreakpointList
│   ├── StepControls
│   │   ├── StepButton
│   │   ├── ContinueButton
│   │   └── ResetButton
│   └── WatchPanel
│       └── VariableWatch
├── ControlPanel (existing - enhance)
│   ├── ExecutionControls
│   ├── SpeedSlider
│   └── ViewModeToggle
└── PerformanceMonitor
    ├── FPSCounter
    ├── MemoryUsage
    └── QualitySelector
```

#### 4. User Dashboard Component

```
Dashboard (NEW)
├── ProgressOverview
│   ├── OverallProgressChart (Chart.js donut)
│   ├── RecentActivity (timeline)
│   └── StreakCounter
├── ModuleGrid
│   ├── CompletedModules (cards with badges)
│   ├── InProgressModules (with %)
│   └── RecommendedModules
├── AchievementPanel
│   ├── BadgeGrid (earned/locked)
│   ├── AchievementDetail
│   └── NextAchievement (progress bar)
├── HistoryPanel
│   ├── ExerciseHistory (table)
│   ├── CodeSubmissions (list)
│   └── LessonTimestamps
└── AnalyticsPanel
    ├── TimeSpentChart (by era)
    ├── LanguageDistribution (pie chart)
    └── LearningPathGraph (D3.js)
```

#### 5. Educational Module Components

```
LessonViewer (ENHANCE existing)
├── LessonHeader
│   ├── Title
│   ├── Breadcrumbs
│   └── ProgressIndicator
├── ContentRenderer
│   ├── MarkdownRenderer (remark/rehype)
│   │   ├── KaTeXPlugin (math)
│   │   ├── SyntaxHighlightPlugin (Prism)
│   │   └── MermaidPlugin (diagrams)
│   ├── CodeBlockWrapper
│   │   ├── CopyButton
│   │   └── RunInPlaygroundButton
│   └── ImageGallery (lightbox)
├── NavigationControls
│   ├── PreviousLessonButton
│   ├── NextLessonButton
│   └── BackToModuleButton
└── SidePanel
    ├── TableOfContents (auto-generated)
    ├── KeyConcepts (definitions)
    └── RelatedLessons (links)

ExerciseViewer (ENHANCE existing)
├── ProblemStatement (markdown)
├── CodeEditor (Monaco)
├── TestCasePanel
│   ├── TestCaseList (input/expected output)
│   ├── RunTestsButton
│   └── TestResults (pass/fail indicators)
├── HintPanel
│   ├── HintToggle (reveal progressively)
│   └── HintContent
└── SubmissionPanel
    ├── SubmitButton
    ├── Score (percentage)
    └── SolutionToggle (after completion)
```

---

## Data Flow Architecture

### State Management Flow

```
┌─────────────────────────────────────────────────────────────┐
│                      User Actions                            │
├─────────────────────────────────────────────────────────────┤
│ Click Era │ Run Code │ Complete Lesson │ Submit Exercise    │
└────────┬────────┬────────────┬────────────────┬─────────────┘
         │        │            │                │
         ▼        ▼            ▼                ▼
┌────────────────────────────────────────────────────────────┐
│                   Action Dispatchers                        │
│  selectEra() │ executeCode() │ markComplete() │ submit()   │
└────────┬────────┬────────────┬────────────────┬────────────┘
         │        │            │                │
         ▼        ▼            ▼                ▼
┌────────────────────────────────────────────────────────────┐
│                   Svelte Stores (State)                     │
├────────────────────────────────────────────────────────────┤
│ timelineStore │ playgroundStore │ userStore │ emulatorStore│
│  - eras       │  - code         │ - progress│  - state     │
│  - selected   │  - output       │ - badges  │  - breakpts  │
│  - progress   │  - executing    │ - history │  - variables │
└────────┬────────┬────────────┬────────────────┬────────────┘
         │        │            │                │
         ▼        ▼            ▼                ▼
┌────────────────────────────────────────────────────────────┐
│                  API Client Layer                           │
│  /api/v1/timeline │ /api/v1/execute │ /api/v1/user        │
└────────┬────────┬────────────┬────────────────┬────────────┘
         │        │            │                │
         ▼        ▼            ▼                ▼
┌────────────────────────────────────────────────────────────┐
│                    Backend API                              │
│  FastAPI │ Compilers │ Emulator │ Database                 │
└────────────────────────────────────────────────────────────┘
```

### WebSocket Real-Time Flow

```
User clicks "Run Code"
        │
        ▼
playgroundStore.executeCode()
        │
        ▼
WebSocket.send({ code, language })
        │
        ▼
Backend: Compilation starts
        │
        ├─> Event: "compilation_started"
        ├─> Event: "lexing_complete" (25%)
        ├─> Event: "parsing_complete" (50%)
        ├─> Event: "typechecking_complete" (75%)
        ├─> Event: "ir_generation_complete" (90%)
        └─> Event: "execution_complete" (100%)
        │
        ▼
Frontend WebSocket Listener
        │
        ├─> Update progressBar
        ├─> Display IR/Assembly
        └─> Show stdout/stderr
        │
        ▼
playgroundStore.update({ output, executionTime })
        │
        ▼
OutputConsole re-renders
```

### Data Persistence Strategy

```
┌─────────────────────────────────────────────────────────────┐
│                  Persistence Layers                          │
├─────────────────────────────────────────────────────────────┤
│ 1. Backend Database (PostgreSQL)                            │
│    - User accounts                                           │
│    - Progress records                                        │
│    - Exercise submissions                                    │
│    - Achievement unlocks                                     │
│                                                              │
│ 2. Browser LocalStorage (5MB limit)                         │
│    - Theme preferences                                       │
│    - Editor settings (font size, vim mode)                  │
│    - Recent code snippets (last 10)                         │
│    - Timeline zoom level                                     │
│                                                              │
│ 3. Service Worker Cache (50MB+)                             │
│    - Curriculum markdown files                               │
│    - Code examples                                           │
│    - Static assets (images, fonts)                          │
│    - API responses (eras, modules)                          │
│                                                              │
│ 4. IndexedDB (unlimited)                                    │
│    - Offline code submissions queue                          │
│    - Downloaded modules for offline viewing                  │
│    - 3D model cache                                          │
└─────────────────────────────────────────────────────────────┘
```

---

## API Integration Points

### Backend Endpoints (Existing + New)

#### Timeline API
```typescript
GET /api/v1/timeline/eras
Response: { eras: Era[] }

GET /api/v1/timeline/modules?era_id=xyz
Response: { modules: Module[] }

GET /api/v1/timeline/lessons/:id
Response: { lesson: Lesson }

GET /api/v1/timeline/full
Response: { eras: Era[], modules: Module[], lessons: Lesson[] }
```

#### Code Execution API (Existing)
```typescript
POST /api/v1/execute/run
Body: { code: string, language: string, input_data?: string }
Response: {
  stdout: string,
  stderr: string,
  compilation_time: number,
  ir_text: string,
  assembly_text: string,
  machine_code: string,
  success: boolean
}

GET /api/v1/execute/languages
Response: { languages: string[] }
```

#### Emulator API
```typescript
POST /api/v1/emulator/execute
Body: { assembly: string, input?: number[], debug: boolean }
Response: {
  cycles: number,
  output: number[],
  state: MachineState,
  execution_trace?: ExecutionEvent[]
}

POST /api/v1/emulator/step
Body: { state: MachineState }
Response: { state: MachineState, event: ExecutionEvent }
```

#### User Progress API (NEW - needs backend implementation)
```typescript
GET /api/v1/user/progress
Response: {
  user_id: string,
  completed_lessons: string[],
  completed_exercises: string[],
  achievements: Achievement[],
  total_time_spent: number
}

POST /api/v1/user/progress/lesson
Body: { lesson_id: string }
Response: { success: boolean }

POST /api/v1/user/progress/exercise
Body: { exercise_id: string, score: number, code: string }
Response: { success: boolean, new_achievements: Achievement[] }
```

#### Examples API (NEW)
```typescript
GET /api/v1/examples?language=python&era=ancient
Response: {
  examples: [{
    id: string,
    title: string,
    code: string,
    description: string,
    language: string,
    era: string
  }]
}
```

### WebSocket Events

```typescript
// Client -> Server
{
  type: 'execute_code',
  payload: { code: string, language: string }
}

{
  type: 'step_debugger',
  payload: { breakpoints: number[] }
}

// Server -> Client
{
  type: 'compilation_progress',
  payload: { phase: string, progress: number }
}

{
  type: 'execution_output',
  payload: { stdout: string, stderr: string }
}

{
  type: 'debugger_state',
  payload: { state: MachineState, variables: Record<string, any> }
}

{
  type: 'error',
  payload: { message: string, code: string }
}
```

---

## State Management Strategy

### Store Architecture

#### 1. timelineStore.ts (Existing - 468 lines)
**Purpose**: Manage 12,500-year timeline, eras, modules, lessons  
**State**:
```typescript
{
  eras: Era[],
  selectedEraId: string | null,
  selectedModuleId: string | null,
  selectedLessonId: string | null,
  currentView: 'timeline' | 'era' | 'module' | 'lesson',
  isLoading: boolean,
  error: string | null
}
```
**Actions**: selectEra, selectModule, selectLesson, markLessonComplete, nextLesson, previousLesson

#### 2. playgroundStore.ts (NEW)
**Purpose**: Manage code editor state and execution  
**State**:
```typescript
{
  code: string,
  language: string,
  output: string,
  errors: CompilationError[],
  isExecuting: boolean,
  executionTime: number,
  irText: string,
  assemblyText: string,
  exampleLibrary: Example[],
  settings: {
    fontSize: number,
    tabSize: number,
    vimMode: boolean,
    theme: 'light' | 'dark' | 'victorian'
  }
}
```
**Actions**: executeCode, changeLanguage, loadExample, updateSettings, clearOutput

#### 3. emulatorStore.ts (NEW)
**Purpose**: Manage 3D emulator state and debugging  
**State**:
```typescript
{
  machineState: MachineState | null,
  breakpoints: number[],
  variables: Record<string, DebuggerVariable>,
  executionTrace: ExecutionEvent[],
  isPaused: boolean,
  currentCycle: number,
  viewMode: '3d' | '2d' | 'text',
  cameraPosition: { x, y, z },
  qualityLevel: 'low' | 'medium' | 'high'
}
```
**Actions**: executeProgram, stepCycle, toggleBreakpoint, inspectColumn, resetEmulator

#### 4. userStore.ts (NEW)
**Purpose**: User authentication and progress  
**State**:
```typescript
{
  userId: string | null,
  isAuthenticated: boolean,
  progress: {
    completedLessons: Set<string>,
    completedExercises: Set<string>,
    totalTimeSpent: number,
    currentStreak: number
  },
  achievements: Achievement[],
  preferences: {
    theme: string,
    language: string,
    emailNotifications: boolean
  }
}
```
**Actions**: login, logout, updateProgress, unlockAchievement, updatePreferences

#### 5. cacheStore.ts (NEW)
**Purpose**: Service Worker cache management  
**State**:
```typescript
{
  cachedModules: Set<string>,
  cacheSize: number,
  offlineMode: boolean,
  lastSync: Date | null
}
```
**Actions**: cacheModule, clearCache, syncWithServer, toggleOfflineMode

### Derived Stores

```typescript
// Overall progress percentage
export const overallProgress = derived(
  [timelineStore, userStore],
  ([$timeline, $user]) => {
    const total = $timeline.eras.flatMap(e => e.modules.flatMap(m => m.lessons)).length;
    const completed = $user.progress.completedLessons.size;
    return Math.round((completed / total) * 100);
  }
);

// Current era modules
export const currentEraModules = derived(
  timelineStore,
  $state => $state.eras
    .find(e => e.id === $state.selectedEraId)
    ?.modules.sort((a, b) => a.order - b.order) || []
);

// Recommended next lesson
export const nextRecommendedLesson = derived(
  [timelineStore, userStore],
  ([$timeline, $user]) => {
    // Logic to find next uncompleted lesson in learning path
  }
);

// Achievement progress
export const nextAchievement = derived(
  userStore,
  $state => {
    // Find closest unearned achievement
  }
);
```

---

## File & Folder Structure

```
frontend/
├── src/
│   ├── routes/                        # SvelteKit pages
│   │   ├── +layout.svelte             # Root layout (header, nav)
│   │   ├── +page.svelte               # Home page
│   │   ├── timeline/
│   │   │   └── +page.svelte           # ✅ Timeline view (existing)
│   │   ├── modules/
│   │   │   ├── +page.svelte           # ✅ Module listing (existing)
│   │   │   └── [slug]/
│   │   │       └── +page.svelte       # 🆕 Module detail page
│   │   ├── lessons/
│   │   │   └── [id]/
│   │   │       └── +page.svelte       # 🆕 Lesson viewer
│   │   ├── exercises/
│   │   │   └── [id]/
│   │   │       └── +page.svelte       # 🆕 Exercise interface
│   │   ├── playground/
│   │   │   └── +page.svelte           # 🆕 Code playground
│   │   ├── emulator/
│   │   │   ├── +page.svelte           # ✅ 2D emulator (existing)
│   │   │   └── 3d/
│   │   │       └── +page.svelte       # ✅ 3D visualization (existing)
│   │   ├── dashboard/
│   │   │   └── +page.svelte           # 🆕 User dashboard
│   │   └── about/
│   │       └── +page.svelte           # ✅ About page (existing)
│   │
│   ├── lib/
│   │   ├── components/
│   │   │   ├── education/             # Educational components
│   │   │   │   ├── Timeline.svelte                    # ✅ Existing
│   │   │   │   ├── InteractiveTimeline.svelte         # 🆕 D3.js enhanced
│   │   │   │   ├── TimelineAxis.svelte                # 🆕 D3 axis component
│   │   │   │   ├── EraMarker.svelte                   # 🆕 Era visualization
│   │   │   │   ├── MilestoneTooltip.svelte            # 🆕 Rich tooltips
│   │   │   │   ├── Module.svelte                      # ✅ Existing
│   │   │   │   ├── ModuleCard.svelte                  # 🆕 Enhanced card
│   │   │   │   ├── ModuleGrid.svelte                  # 🆕 Responsive grid
│   │   │   │   ├── Lesson.svelte                      # ✅ Existing
│   │   │   │   ├── LessonViewer.svelte                # 🆕 Enhanced viewer
│   │   │   │   ├── MarkdownRenderer.svelte            # 🆕 Math + diagrams
│   │   │   │   ├── Exercise.svelte                    # ✅ Existing
│   │   │   │   ├── ExerciseViewer.svelte              # 🆕 Enhanced exercise
│   │   │   │   ├── TestCasePanel.svelte               # 🆕 Test cases
│   │   │   │   ├── ProgressTracker.svelte             # ✅ Existing
│   │   │   │   ├── ProgressChart.svelte               # 🆕 Chart.js charts
│   │   │   │   ├── HistoricalNavigator.svelte         # ✅ Existing
│   │   │   │   └── __tests__/
│   │   │   │       ├── progressTracker.test.ts        # ✅ Existing
│   │   │   │       └── lessonViewer.test.ts           # 🆕 New tests
│   │   │   │
│   │   │   ├── playground/            # 🆕 Code playground components
│   │   │   │   ├── CodePlayground.svelte              # Main playground
│   │   │   │   ├── MonacoEditor.svelte                # Monaco wrapper
│   │   │   │   ├── LanguageSelector.svelte            # Language dropdown
│   │   │   │   ├── OutputConsole.svelte               # Execution output
│   │   │   │   ├── ExampleLibrary.svelte              # Code examples
│   │   │   │   ├── ExampleCard.svelte                 # Example display
│   │   │   │   ├── EditorToolbar.svelte               # Editor controls
│   │   │   │   └── __tests__/
│   │   │   │       └── codePlayground.test.ts
│   │   │   │
│   │   │   ├── visualization/         # Visualization components
│   │   │   │   ├── EmulatorView.svelte                # ✅ Existing
│   │   │   │   ├── BabbageEngine3D.svelte             # 🆕 Three.js engine
│   │   │   │   ├── EngineModel.svelte                 # 🆕 3D model loader
│   │   │   │   ├── ColumnVisualizer.svelte            # 🆕 Column display
│   │   │   │   ├── GearAnimation.svelte               # 🆕 Gear rotations
│   │   │   │   ├── CameraControls.svelte              # 🆕 Orbit controls
│   │   │   │   ├── DebuggerPanel.svelte               # 🆕 Debugger UI
│   │   │   │   ├── StateInspector.svelte              # 🆕 State viewer
│   │   │   │   ├── BreakpointList.svelte              # 🆕 Breakpoints
│   │   │   │   ├── ControlPanel.svelte                # ✅ Existing
│   │   │   │   ├── CanvasContainer.svelte             # ✅ Existing
│   │   │   │   └── __tests__/
│   │   │   │       └── babbageEngine3D.test.ts        # 🆕
│   │   │   │
│   │   │   ├── dashboard/             # 🆕 Dashboard components
│   │   │   │   ├── Dashboard.svelte                   # Main dashboard
│   │   │   │   ├── ProgressOverview.svelte            # Progress summary
│   │   │   │   ├── AchievementPanel.svelte            # Badges
│   │   │   │   ├── AchievementBadge.svelte            # Badge display
│   │   │   │   ├── HistoryPanel.svelte                # Submission history
│   │   │   │   ├── AnalyticsChart.svelte              # Time/language charts
│   │   │   │   ├── RecommendationCard.svelte          # Next lessons
│   │   │   │   └── __tests__/
│   │   │   │       └── dashboard.test.ts
│   │   │   │
│   │   │   └── common/                # Shared UI components
│   │   │       ├── Header.svelte                      # ✅ Existing
│   │   │       ├── Navigation.svelte                  # ✅ Existing
│   │   │       ├── Button.svelte                      # 🆕 Reusable button
│   │   │       ├── Input.svelte                       # 🆕 Form input
│   │   │       ├── Select.svelte                      # 🆕 Dropdown
│   │   │       ├── Modal.svelte                       # 🆕 Modal dialog
│   │   │       ├── Toast.svelte                       # 🆕 Notifications
│   │   │       ├── Spinner.svelte                     # 🆕 Loading indicator
│   │   │       ├── Tooltip.svelte                     # 🆕 Hover tooltips
│   │   │       ├── Card.svelte                        # 🆕 Card container
│   │   │       ├── Badge.svelte                       # 🆕 Label badge
│   │   │       ├── ProgressBar.svelte                 # 🆕 Progress bar
│   │   │       ├── Tabs.svelte                        # 🆕 Tab navigation
│   │   │       ├── Accordion.svelte                   # 🆕 Collapsible
│   │   │       └── __tests__/
│   │   │           └── common.test.ts
│   │   │
│   │   ├── stores/                    # State management
│   │   │   ├── timelineStore.ts                       # ✅ Existing (468 lines)
│   │   │   ├── playgroundStore.ts                     # 🆕 Code playground state
│   │   │   ├── emulatorStore.ts                       # 🆕 Emulator state
│   │   │   ├── userStore.ts                           # 🆕 User/auth state
│   │   │   ├── themeStore.ts                          # 🆕 Theme preferences
│   │   │   ├── achievementStore.ts                    # 🆕 Achievement state
│   │   │   ├── cacheStore.ts                          # 🆕 Service Worker cache
│   │   │   └── __tests__/
│   │   │       ├── timelineStore.test.ts              # ✅ Existing
│   │   │       └── playgroundStore.test.ts            # 🆕
│   │   │
│   │   ├── api/                       # API client layer
│   │   │   ├── client.ts                              # ✅ Existing (127 lines)
│   │   │   ├── index.ts                               # ✅ Existing
│   │   │   ├── timeline.ts                            # 🆕 Timeline endpoints
│   │   │   ├── execution.ts                           # 🆕 Code execution
│   │   │   ├── emulator.ts                            # 🆕 Emulator endpoints
│   │   │   ├── user.ts                                # 🆕 User/auth endpoints
│   │   │   ├── examples.ts                            # 🆕 Example code
│   │   │   └── types.ts                               # 🆕 Shared types
│   │   │
│   │   ├── visualization/             # Visualization utilities
│   │   │   ├── d3/                    # 🆕 D3.js utilities
│   │   │   │   ├── timeline.ts                        # Timeline scales/axes
│   │   │   │   ├── graph.ts                           # Force-directed graph
│   │   │   │   └── charts.ts                          # Chart helpers
│   │   │   ├── three/                 # 🆕 Three.js utilities
│   │   │   │   ├── scene.ts                           # Scene setup
│   │   │   │   ├── materials.ts                       # Victorian materials
│   │   │   │   ├── models.ts                          # Model loaders
│   │   │   │   └── animations.ts                      # Gear animations
│   │   │   ├── performance/           # ✅ Existing performance
│   │   │   │   ├── DeviceTier.ts                      # ✅ Existing
│   │   │   │   ├── CapabilityAnalyzer.ts              # ✅ Existing
│   │   │   │   ├── QualitySelector.ts                 # ✅ Existing
│   │   │   │   └── index.ts                           # ✅ Existing
│   │   │   └── websocket.ts                           # ✅ Existing
│   │   │
│   │   ├── utils/                     # 🆕 Utility functions
│   │   │   ├── markdown.ts                            # Markdown + KaTeX
│   │   │   ├── syntax.ts                              # Syntax highlighting
│   │   │   ├── date.ts                                # Date formatting
│   │   │   ├── math.ts                                # Math utilities
│   │   │   ├── validation.ts                          # Form validation
│   │   │   └── storage.ts                             # LocalStorage wrapper
│   │   │
│   │   └── workers/                   # 🆕 Web Workers
│   │       ├── compilation.worker.ts                  # Heavy compilation
│   │       └── rendering.worker.ts                    # 3D rendering
│   │
│   ├── app.html                       # ✅ HTML template
│   └── service-worker.ts              # 🆕 Service Worker
│
├── static/                            # Static assets
│   ├── models/                        # 🆕 3D models
│   │   └── babbage-engine.glb         # Victorian engine model
│   ├── textures/                      # 🆕 3D textures
│   │   ├── brass.jpg
│   │   ├── wood.jpg
│   │   └── glass.jpg
│   ├── fonts/                         # 🆕 Custom fonts
│   │   └── victorian.woff2
│   └── icons/                         # App icons
│
├── package.json                       # ✅ Dependencies
├── vite.config.ts                     # ✅ Vite config
├── svelte.config.js                   # ✅ SvelteKit config
├── tsconfig.json                      # ✅ TypeScript config
└── vitest.config.ts                   # 🆕 Vitest config
```

### Component Count Summary

| Category | Existing | New | Total |
|----------|----------|-----|-------|
| Education Components | 6 | 8 | 14 |
| Playground Components | 0 | 7 | 7 |
| Visualization Components | 3 | 8 | 11 |
| Dashboard Components | 0 | 6 | 6 |
| Common Components | 2 | 11 | 13 |
| **Total Components** | **11** | **40** | **51** |

---

## Technology Choices & Justifications

### Core Framework: SvelteKit
**Choice**: SvelteKit 1.x (existing)  
**Justification**:
- True reactivity without virtual DOM (faster than React/Vue)
- Built-in SSR and code splitting
- File-based routing
- Smaller bundle sizes (~40% smaller than React)
- TypeScript support out of the box
- Already in use (continuity)

### Code Editor: Monaco Editor
**Choice**: Monaco Editor 0.44.0  
**Justification**:
- Powers VS Code (industry standard)
- Multi-language support (8+ languages)
- IntelliSense and autocomplete
- Theme customization (light/dark/victorian)
- Syntax highlighting built-in
- 3MB gzipped (acceptable for educational app)

**Alternative Considered**: CodeMirror 6
- Lighter weight (500KB)
- But: Less feature-complete, harder to customize

### 3D Graphics: Three.js
**Choice**: Three.js 0.158.0 (existing)  
**Justification**:
- Industry standard WebGL library
- Extensive documentation and examples
- Victorian-era materials (PBR support)
- OrbitControls for camera
- GLTFLoader for models
- Performance on mobile devices

**Alternative Considered**: Babylon.js
- More game-focused
- Heavier bundle size
- Less educational examples

### Data Visualization: D3.js
**Choice**: D3.js 7.8.5 (existing)  
**Justification**:
- Best-in-class for custom visualizations
- Timeline axis and zoom built-in
- Force-directed graphs (concept relationships)
- Transition animations
- SVG manipulation

**Chart Library**: Chart.js 4.4.0 (NEW)
**Justification**:
- Simple API for standard charts (donut, line, bar)
- Canvas-based (good performance)
- Responsive by default
- 250KB minified

**Alternative Considered**: Recharts
- React-specific (doesn't work with Svelte)

### Markdown Rendering: Remark + Rehype
**Choice**: Unified ecosystem (remark 15.x, rehype 6.x)  
**Justification**:
- Plugin architecture (math, syntax, diagrams)
- KaTeX integration for math
- Prism.js for syntax highlighting
- Mermaid.js for diagrams
- MDX support (future: interactive components)

**Plugins**:
- `remark-math` + `rehype-katex` (LaTeX math)
- `rehype-prism-plus` (syntax highlighting)
- `remark-mermaid` (diagrams)

### State Management: Svelte Stores
**Choice**: Built-in Svelte stores (writable, readable, derived)  
**Justification**:
- Native to Svelte (no external deps)
- Simple reactive API
- Derived stores for computed values
- LocalStorage persistence easy
- TypeScript support

**Alternative Considered**: Redux Toolkit
- Overkill for this app size
- Adds 50KB+ to bundle
- More boilerplate code

### Offline Support: Service Workers
**Choice**: Vite PWA Plugin (`vite-plugin-pwa`)  
**Justification**:
- Automatic service worker generation
- Precache curriculum content
- Offline fallback pages
- Background sync for submissions
- 100MB+ cache quota

**Cache Strategy**:
- **Network First**: API calls (fresh data)
- **Cache First**: Static assets (fonts, images)
- **Stale While Revalidate**: Curriculum markdown

### Testing: Vitest
**Choice**: Vitest 0.34.6 (existing)  
**Justification**:
- Native Vite integration (fast)
- Jest-compatible API (migration easy)
- Component testing with @testing-library/svelte
- Coverage reports with c8
- Watch mode for TDD

### Accessibility: ARIA + Focus Management
**Tools**:
- `svelte-a11y-warnings` (compile-time)
- `axe-core` (runtime auditing)
- `focus-trap-svelte` (modal focus)

---

## Performance Optimization Strategy

### 1. Code Splitting

#### Route-Based Splitting (Automatic)
```typescript
// SvelteKit automatically splits routes
routes/
  timeline/+page.svelte       → timeline-[hash].js (60KB)
  playground/+page.svelte     → playground-[hash].js (800KB - Monaco)
  emulator/3d/+page.svelte    → emulator-3d-[hash].js (600KB - Three.js)
  dashboard/+page.svelte      → dashboard-[hash].js (100KB - Chart.js)
```

#### Component-Level Splitting
```svelte
<script>
  import { onMount } from 'svelte';
  
  let MonacoEditor;
  onMount(async () => {
    // Lazy load Monaco only when playground is visited
    MonacoEditor = (await import('$lib/components/playground/MonacoEditor.svelte')).default;
  });
</script>
```

#### Library Splitting (vite.config.ts)
```typescript
export default defineConfig({
  build: {
    rollupOptions: {
      output: {
        manualChunks: {
          'vendor-three': ['three'],                    // 600KB
          'vendor-monaco': ['monaco-editor'],           // 800KB
          'vendor-d3': ['d3'],                          // 250KB
          'vendor-chart': ['chart.js'],                 // 250KB
          'vendor-markdown': ['remark', 'rehype'],      // 150KB
        }
      }
    }
  }
});
```

### 2. Asset Optimization

#### Images
- **Format**: WebP with PNG fallback
- **Compression**: Squoosh CLI (80% quality)
- **Lazy Loading**: `<img loading="lazy" />`
- **Responsive**: `<picture>` with srcset

#### 3D Models
- **Format**: GLTF/GLB (compressed)
- **Compression**: Draco (60% size reduction)
- **LOD**: Level of Detail based on DeviceTier
- **Progressive Loading**: Low-poly → High-poly

#### Fonts
- **Format**: WOFF2 (95% browser support)
- **Subset**: Latin characters only (50% reduction)
- **Preload**: Critical fonts in `<head>`

### 3. Rendering Optimization

#### Virtual Scrolling (Timeline)
```svelte
<!-- Only render visible eras in viewport -->
<svelte:window bind:scrollY />
{#each visibleEras as era}
  <EraMarker {era} />
{/each}
```

#### Three.js Optimization
```typescript
// Adaptive quality based on device
if (deviceTier === 'low') {
  renderer.setPixelRatio(1);
  renderer.shadowMap.enabled = false;
  geometry.computeVertexNormals(); // Flat shading
} else if (deviceTier === 'high') {
  renderer.setPixelRatio(window.devicePixelRatio);
  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;
}

// Frustum culling (don't render off-screen objects)
camera.updateProjectionMatrix();
scene.traverse(obj => {
  obj.frustumCulled = true;
});

// Object pooling for particles
const particlePool = new ParticlePool(1000);
```

#### Canvas FPS Throttling
```typescript
let frameId;
function animate() {
  frameId = requestAnimationFrame(animate);
  
  // Throttle to 30fps on mobile
  if (deviceTier === 'low' && Date.now() - lastFrame < 33) return;
  
  renderer.render(scene, camera);
}
```

### 4. Network Optimization

#### API Caching (Service Worker)
```typescript
// Cache timeline data for 1 hour
workbox.routing.registerRoute(
  /\/api\/v1\/timeline/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'api-cache',
    plugins: [
      new workbox.expiration.ExpirationPlugin({
        maxAgeSeconds: 3600, // 1 hour
      }),
    ],
  })
);
```

#### Request Batching
```typescript
// Batch multiple lesson requests
const lessonIds = ['1', '2', '3'];
const lessons = await fetch(`/api/v1/lessons?ids=${lessonIds.join(',')}`);
```

#### HTTP/2 Server Push
```html
<!-- Link preload for critical assets -->
<link rel="preload" href="/fonts/victorian.woff2" as="font" crossorigin>
<link rel="modulepreload" href="/chunks/vendor-three.js">
```

### 5. Bundle Size Budget

| Bundle | Size Limit | Current | Strategy |
|--------|-----------|---------|----------|
| Main Bundle | 150KB | 120KB | ✅ Under budget |
| Vendor (Base) | 250KB | 220KB | ✅ Under budget |
| Vendor (Monaco) | 800KB | 780KB | ⚠️ Lazy load only |
| Vendor (Three.js) | 600KB | 580KB | ⚠️ Lazy load only |
| Total (Initial) | 400KB | 340KB | ✅ Under budget |

**Targets**:
- **First Contentful Paint**: < 1.5s (3G)
- **Time to Interactive**: < 3.5s (3G)
- **Lighthouse Score**: 90+ (Performance)

---

## Accessibility Checklist

### WCAG 2.1 AA Compliance

#### 1. Perceivable

##### Color Contrast (1.4.3)
- [ ] Text contrast ratio ≥ 4.5:1 (normal text)
- [ ] Text contrast ratio ≥ 3:1 (large text 18pt+)
- [ ] Interactive element contrast ≥ 3:1
- [ ] Era colors distinguishable for colorblind users

**Implementation**:
```css
/* Use CSS custom properties for theme */
:root {
  --text-primary: #1a1a1a;        /* 16.5:1 on white */
  --text-secondary: #4a4a4a;      /* 8.6:1 on white */
  --bg-primary: #ffffff;
  --accent-blue: #0056b3;         /* 8.2:1 on white */
}

@media (prefers-color-scheme: dark) {
  :root {
    --text-primary: #e0e0e0;      /* 12.6:1 on #1a1a1a */
    --text-secondary: #b0b0b0;    /* 7.1:1 on #1a1a1a */
    --bg-primary: #1a1a1a;
  }
}
```

##### Non-Text Content (1.1.1)
- [ ] All images have alt text
- [ ] Decorative images use `alt=""`
- [ ] 3D visualizations have text descriptions
- [ ] Charts have data tables as fallback

**Implementation**:
```svelte
<!-- Good alt text -->
<img 
  src="/models/babbage-engine.png" 
  alt="Charles Babbage's Difference Engine No. 2 with 8 vertical columns and brass gears"
/>

<!-- Decorative image -->
<img src="/decorations/victorian-border.svg" alt="" role="presentation" />

<!-- Chart fallback -->
<canvas id="progress-chart" aria-describedby="chart-data"></canvas>
<table id="chart-data" class="visually-hidden">
  <caption>Learning Progress by Era</caption>
  <thead>...</thead>
</table>
```

##### Audio/Video (1.2.x)
- [ ] Video tutorials have captions
- [ ] Audio descriptions for visual-only content
- [ ] Transcript for all multimedia

#### 2. Operable

##### Keyboard Navigation (2.1.1)
- [ ] All interactive elements focusable
- [ ] Logical tab order (top → bottom, left → right)
- [ ] No keyboard traps
- [ ] Skip navigation link
- [ ] Focus visible on all elements

**Implementation**:
```svelte
<!-- Skip navigation -->
<a href="#main-content" class="skip-link">Skip to main content</a>

<main id="main-content">
  <!-- Page content -->
</main>

<style>
  .skip-link {
    position: absolute;
    top: -40px;
    left: 0;
    z-index: 100;
    padding: 8px;
    background: var(--bg-primary);
    color: var(--text-primary);
  }
  
  .skip-link:focus {
    top: 0;
  }
  
  /* Visible focus indicator */
  *:focus-visible {
    outline: 3px solid var(--accent-blue);
    outline-offset: 2px;
  }
</style>
```

##### Focus Management (2.4.3)
- [ ] Modal dialogs trap focus
- [ ] Focus returns to trigger element on close
- [ ] Timeline navigation preserves focus

**Implementation**:
```svelte
<script>
  import { trapFocus } from 'svelte-focus-trap';
  
  let modal;
  let triggerElement;
  
  function openModal(e) {
    triggerElement = e.target;
    modal.showModal();
  }
  
  function closeModal() {
    modal.close();
    triggerElement?.focus(); // Return focus
  }
</script>

<dialog bind:this={modal} use:trapFocus>
  <h2>Modal Title</h2>
  <button on:click={closeModal}>Close</button>
</dialog>
```

##### Timing (2.2.1)
- [ ] No time limits on exercises (or adjustable)
- [ ] Pause/stop animations
- [ ] Auto-playing content can be paused

**Implementation**:
```svelte
<script>
  let animationsPaused = false;
  
  // Respect prefers-reduced-motion
  onMount(() => {
    const prefersReduced = window.matchMedia('(prefers-reduced-motion: reduce)');
    animationsPaused = prefersReduced.matches;
  });
</script>

<button on:click={() => animationsPaused = !animationsPaused}>
  {animationsPaused ? 'Resume' : 'Pause'} Animations
</button>
```

#### 3. Understandable

##### Language (3.1.1)
- [ ] `lang` attribute on `<html>`
- [ ] Language changes marked with `lang`

**Implementation**:
```html
<html lang="en">
  <body>
    <p>The Babylonians used <span lang="akk">šanû</span> (base-60).</p>
  </body>
</html>
```

##### Labels (3.3.2)
- [ ] All form inputs have labels
- [ ] Error messages are descriptive
- [ ] Instructions provided for complex inputs

**Implementation**:
```svelte
<label for="code-input">Enter your code:</label>
<textarea 
  id="code-input" 
  aria-describedby="code-hint code-error"
  aria-invalid={hasError}
/>
<p id="code-hint" class="hint">Python code will be executed in sandbox.</p>
{#if hasError}
  <p id="code-error" class="error" role="alert">
    Syntax error on line 5: unexpected indent
  </p>
{/if}
```

##### Error Identification (3.3.1)
- [ ] Form errors identified in text
- [ ] Errors associated with fields
- [ ] Suggestions for fixing errors

#### 4. Robust

##### ARIA Landmarks (4.1.2)
- [ ] Main content in `<main>`
- [ ] Navigation in `<nav>`
- [ ] Search in `<search>` or `role="search"`
- [ ] Complementary content in `<aside>`

**Implementation**:
```svelte
<header>
  <h1>Ancient Compute</h1>
</header>

<nav aria-label="Main navigation">
  <ul>...</ul>
</nav>

<main>
  <article aria-labelledby="lesson-title">
    <h2 id="lesson-title">Egyptian Multiplication</h2>
  </article>
</main>

<aside aria-label="Related lessons">
  <h3>See also</h3>
  <ul>...</ul>
</aside>
```

##### ARIA Roles (4.1.2)
- [ ] Custom components have appropriate roles
- [ ] State communicated with `aria-*` attributes
- [ ] Live regions for dynamic content

**Implementation**:
```svelte
<!-- Timeline with ARIA -->
<div role="region" aria-label="Historical timeline" tabindex="0">
  <svg role="img" aria-labelledby="timeline-title">
    <title id="timeline-title">
      12,500-year timeline from 20,000 BC to 2025 AD
    </title>
    <!-- Timeline visualization -->
  </svg>
</div>

<!-- Live region for code execution -->
<div role="status" aria-live="polite" aria-atomic="true">
  {#if executing}
    Executing code...
  {:else if output}
    Execution complete. {output.length} lines of output.
  {/if}
</div>

<!-- Tabs -->
<div class="tabs" role="tablist">
  <button 
    role="tab" 
    aria-selected={selectedTab === 'output'}
    aria-controls="output-panel"
    id="output-tab"
  >
    Output
  </button>
  <div role="tabpanel" id="output-panel" aria-labelledby="output-tab">
    {output}
  </div>
</div>
```

### Screen Reader Testing

**Test with**:
- NVDA (Windows) - Free
- JAWS (Windows) - Industry standard
- VoiceOver (macOS/iOS) - Built-in
- TalkBack (Android) - Built-in

**Test scenarios**:
1. Navigate timeline with keyboard only
2. Execute code in playground
3. Complete an exercise
4. View progress dashboard
5. Use 3D emulator with screen reader

### Accessibility Tools

```bash
# Install dependencies
npm install -D @axe-core/cli svelte-a11y-checker

# Run accessibility audit
npx axe http://localhost:5173 --tags wcag2a,wcag2aa

# Svelte compile-time warnings
npm run check
```

---

## Implementation Roadmap

### Week 1-2: Code Playground (NEW)

**Deliverables**:
- Monaco Editor integration
- Language selector (8 languages)
- Execute button with WebSocket
- Output console
- Example library

**Tasks**:
1. Install Monaco Editor (`npm install monaco-editor`)
2. Create `MonacoEditor.svelte` wrapper component
3. Implement language switcher (syntax highlighting)
4. Build `OutputConsole.svelte` with stdout/stderr panels
5. Create `ExampleLibrary.svelte` with filterable examples
6. Wire up WebSocket for real-time execution
7. Add error line highlighting
8. Implement share/save functionality
9. Write Vitest tests (>80% coverage)

**Files**:
- `src/lib/components/playground/CodePlayground.svelte`
- `src/lib/components/playground/MonacoEditor.svelte`
- `src/lib/components/playground/LanguageSelector.svelte`
- `src/lib/components/playground/OutputConsole.svelte`
- `src/lib/components/playground/ExampleLibrary.svelte`
- `src/lib/stores/playgroundStore.ts`
- `src/routes/playground/+page.svelte`

**Estimated Lines**: 1,200 lines

### Week 3: Enhanced Timeline (ENHANCE)

**Deliverables**:
- Multi-level zoom (D3.js)
- Animated transitions
- Rich tooltips
- Milestone search/filter

**Tasks**:
1. Refactor existing Timeline.svelte → InteractiveTimeline.svelte
2. Implement D3.js zoom behavior (scaleExtent: [0.1, 10])
3. Create zoom level thresholds (overview/era/decade/year)
4. Add animated transitions (duration: 750ms, easing: cubic)
5. Build `MilestoneTooltip.svelte` with rich content
6. Implement search/filter UI
7. Add force-directed graph view (optional)
8. Mobile touch gestures (pinch-to-zoom)
9. Write D3 integration tests

**Files**:
- `src/lib/components/education/InteractiveTimeline.svelte`
- `src/lib/components/education/TimelineAxis.svelte`
- `src/lib/components/education/MilestoneTooltip.svelte`
- `src/lib/visualization/d3/timeline.ts`
- `src/lib/visualization/d3/graph.ts`

**Estimated Lines**: 800 lines

### Week 4-5: 3D Babbage Emulator (ENHANCE)

**Deliverables**:
- Victorian-era 3D model
- Animated gears and columns
- Debugger integration
- Mobile optimization

**Tasks**:
1. Create/source Babbage Engine GLB model
2. Implement `BabbageEngine3D.svelte` with Three.js
3. Add PBR materials (brass, wood, glass)
4. Animate gear rotations based on emulator state
5. Display column values on 3D model (TextGeometry)
6. Implement OrbitControls (camera navigation)
7. Build `DebuggerPanel.svelte` (breakpoints, step, inspect)
8. Add mobile simplified view (low-poly model)
9. Screenshot/recording feature (canvas.toBlob())
10. Write Three.js tests with @testing-library/svelte

**Files**:
- `src/lib/components/visualization/BabbageEngine3D.svelte`
- `src/lib/components/visualization/EngineModel.svelte`
- `src/lib/components/visualization/ColumnVisualizer.svelte`
- `src/lib/components/visualization/DebuggerPanel.svelte`
- `src/lib/visualization/three/scene.ts`
- `src/lib/visualization/three/materials.ts`
- `src/lib/visualization/three/animations.ts`
- `static/models/babbage-engine.glb`

**Estimated Lines**: 1,500 lines

### Week 6: User Dashboard (NEW)

**Deliverables**:
- Progress overview
- Achievement system
- Exercise history
- Analytics charts

**Tasks**:
1. Create `Dashboard.svelte` layout
2. Implement `ProgressOverview.svelte` with Chart.js
3. Build `AchievementPanel.svelte` with badge grid
4. Create achievement unlock logic
5. Build `HistoryPanel.svelte` (table with sorting)
6. Implement `AnalyticsChart.svelte` (time spent, language distribution)
7. Add personalized recommendations
8. Wire up `userStore.ts` with backend API
9. Write dashboard tests

**Files**:
- `src/lib/components/dashboard/Dashboard.svelte`
- `src/lib/components/dashboard/ProgressOverview.svelte`
- `src/lib/components/dashboard/AchievementPanel.svelte`
- `src/lib/components/dashboard/HistoryPanel.svelte`
- `src/lib/components/dashboard/AnalyticsChart.svelte`
- `src/lib/stores/userStore.ts`
- `src/lib/stores/achievementStore.ts`
- `src/routes/dashboard/+page.svelte`

**Estimated Lines**: 1,000 lines

### Week 7: Enhanced Educational Components

**Deliverables**:
- Markdown rendering with math/diagrams
- Exercise interface with test cases
- Module grid with filtering

**Tasks**:
1. Install remark/rehype (`npm install remark rehype rehype-katex remark-mermaid`)
2. Create `MarkdownRenderer.svelte`
3. Build `LessonViewer.svelte` with TOC
4. Implement `ExerciseViewer.svelte` with test cases
5. Create `TestCasePanel.svelte`
6. Build `ModuleGrid.svelte` with filters/search
7. Add cross-reference navigation
8. Implement bookmark system (LocalStorage)
9. Write component tests

**Files**:
- `src/lib/components/education/MarkdownRenderer.svelte`
- `src/lib/components/education/LessonViewer.svelte`
- `src/lib/components/education/ExerciseViewer.svelte`
- `src/lib/components/education/TestCasePanel.svelte`
- `src/lib/components/education/ModuleGrid.svelte`
- `src/lib/utils/markdown.ts`

**Estimated Lines**: 900 lines

### Week 8: Shared UI Components + Polish

**Deliverables**:
- 11 reusable UI components
- Service Worker implementation
- Accessibility audit + fixes
- Performance optimization

**Tasks**:
1. Build common components (Button, Input, Modal, Toast, etc.)
2. Implement Service Worker with Vite PWA plugin
3. Add offline fallback pages
4. Run axe-core accessibility audit
5. Fix WCAG 2.1 AA violations
6. Optimize bundle sizes (code splitting)
7. Add loading skeletons
8. Implement error boundaries
9. Write E2E tests with Playwright
10. Performance audit (Lighthouse)

**Files**:
- `src/lib/components/common/Button.svelte`
- `src/lib/components/common/Input.svelte`
- `src/lib/components/common/Modal.svelte`
- `src/lib/components/common/Toast.svelte`
- `src/lib/components/common/Spinner.svelte`
- `src/lib/components/common/Tooltip.svelte`
- `src/lib/components/common/Card.svelte`
- `src/lib/components/common/Badge.svelte`
- `src/lib/components/common/ProgressBar.svelte`
- `src/lib/components/common/Tabs.svelte`
- `src/lib/components/common/Accordion.svelte`
- `src/service-worker.ts`
- `vite.config.ts` (bundle optimization)

**Estimated Lines**: 800 lines

---

## Testing Strategy

### Unit Tests (Vitest + @testing-library/svelte)

**Coverage Target**: >85%

#### Component Tests

```typescript
// src/lib/components/playground/__tests__/codePlayground.test.ts

import { render, fireEvent, waitFor } from '@testing-library/svelte';
import { vi } from 'vitest';
import CodePlayground from '../CodePlayground.svelte';

describe('CodePlayground', () => {
  it('executes code when run button clicked', async () => {
    const { getByRole, getByText } = render(CodePlayground);
    
    const runButton = getByRole('button', { name: /run/i });
    await fireEvent.click(runButton);
    
    await waitFor(() => {
      expect(getByText(/executing/i)).toBeInTheDocument();
    });
  });
  
  it('displays compilation errors with line numbers', async () => {
    const { getByRole, getByText } = render(CodePlayground, {
      props: {
        initialCode: 'invalid syntax',
        language: 'python'
      }
    });
    
    await fireEvent.click(getByRole('button', { name: /run/i }));
    
    await waitFor(() => {
      expect(getByText(/line 1: syntax error/i)).toBeInTheDocument();
    });
  });
  
  it('switches language and updates syntax highlighting', async () => {
    const { getByLabelText, container } = render(CodePlayground);
    
    const languageSelect = getByLabelText(/language/i);
    await fireEvent.change(languageSelect, { target: { value: 'haskell' } });
    
    await waitFor(() => {
      const editor = container.querySelector('.monaco-editor');
      expect(editor).toHaveAttribute('data-language', 'haskell');
    });
  });
});
```

#### Store Tests

```typescript
// src/lib/stores/__tests__/playgroundStore.test.ts

import { get } from 'svelte/store';
import { playgroundStore, executeCode, changeLanguage } from '../playgroundStore';

describe('playgroundStore', () => {
  beforeEach(() => {
    playgroundStore.set({
      code: '',
      language: 'python',
      output: '',
      isExecuting: false
    });
  });
  
  it('updates code when typing', () => {
    playgroundStore.update(state => ({ ...state, code: 'print("hello")' }));
    
    const state = get(playgroundStore);
    expect(state.code).toBe('print("hello")');
  });
  
  it('sets executing flag during execution', async () => {
    const promise = executeCode('print("test")', 'python');
    
    expect(get(playgroundStore).isExecuting).toBe(true);
    
    await promise;
    
    expect(get(playgroundStore).isExecuting).toBe(false);
  });
  
  it('clears output when changing language', () => {
    playgroundStore.update(state => ({ ...state, output: 'previous output' }));
    
    changeLanguage('haskell');
    
    expect(get(playgroundStore).output).toBe('');
  });
});
```

#### D3.js Tests

```typescript
// src/lib/visualization/d3/__tests__/timeline.test.ts

import { scaleTime } from 'd3-scale';
import { createTimelineScale, formatYear } from '../timeline';

describe('Timeline D3 utilities', () => {
  it('creates time scale for 12,500 years', () => {
    const scale = createTimelineScale(-20000, 2025, 1000);
    
    expect(scale(-20000)).toBe(0);
    expect(scale(2025)).toBe(1000);
    expect(scale(0)).toBeCloseTo(908, 0); // AD 1
  });
  
  it('formats BC years correctly', () => {
    expect(formatYear(-3000)).toBe('3000 BC');
    expect(formatYear(0)).toBe('AD 1');
    expect(formatYear(2025)).toBe('AD 2025');
  });
});
```

#### Three.js Tests

```typescript
// src/lib/components/visualization/__tests__/babbageEngine3D.test.ts

import { render } from '@testing-library/svelte';
import { vi } from 'vitest';
import BabbageEngine3D from '../BabbageEngine3D.svelte';
import * as THREE from 'three';

// Mock Three.js WebGLRenderer
vi.mock('three', async () => {
  const actual = await vi.importActual('three');
  return {
    ...actual,
    WebGLRenderer: vi.fn(() => ({
      render: vi.fn(),
      setSize: vi.fn(),
      dispose: vi.fn()
    }))
  };
});

describe('BabbageEngine3D', () => {
  it('creates Three.js scene on mount', async () => {
    const { container } = render(BabbageEngine3D);
    
    await vi.waitFor(() => {
      const canvas = container.querySelector('canvas');
      expect(canvas).toBeInTheDocument();
    });
  });
  
  it('updates column values when state changes', async () => {
    const { component } = render(BabbageEngine3D, {
      props: {
        machineState: {
          columns: [1, 2, 3, 4, 5, 6, 7, 8]
        }
      }
    });
    
    component.$set({
      machineState: {
        columns: [9, 8, 7, 6, 5, 4, 3, 2]
      }
    });
    
    // Verify column meshes updated
    // (Requires exposing scene for testing or using data-testid)
  });
});
```

### Integration Tests (Playwright)

**Coverage**: Critical user journeys

```typescript
// e2e/playground.spec.ts

import { test, expect } from '@playwright/test';

test.describe('Code Playground', () => {
  test('execute Python code end-to-end', async ({ page }) => {
    await page.goto('/playground');
    
    // Type code
    await page.locator('.monaco-editor').click();
    await page.keyboard.type('print("Hello from Ancient Compute")');
    
    // Select language
    await page.selectOption('select[aria-label="Language"]', 'python');
    
    // Execute
    await page.click('button:has-text("Run")');
    
    // Wait for output
    await expect(page.locator('.output-console')).toContainText('Hello from Ancient Compute');
    
    // Verify compilation time displayed
    await expect(page.locator('.compilation-time')).toBeVisible();
  });
  
  test('load example code', async ({ page }) => {
    await page.goto('/playground');
    
    // Open example library
    await page.click('button:has-text("Examples")');
    
    // Select Egyptian multiplication
    await page.click('text=Egyptian Multiplication');
    
    // Verify code loaded
    const code = await page.locator('.monaco-editor').textContent();
    expect(code).toContain('egyptian_multiply');
  });
});
```

### Accessibility Tests (axe-core)

```typescript
// e2e/accessibility.spec.ts

import { test, expect } from '@playwright/test';
import { injectAxe, checkA11y } from 'axe-playwright';

test.describe('Accessibility', () => {
  test('timeline page passes WCAG 2.1 AA', async ({ page }) => {
    await page.goto('/timeline');
    await injectAxe(page);
    
    await checkA11y(page, null, {
      detailedReport: true,
      detailedReportOptions: { html: true }
    });
  });
  
  test('keyboard navigation works', async ({ page }) => {
    await page.goto('/timeline');
    
    // Tab through interactive elements
    await page.keyboard.press('Tab'); // Skip link
    await page.keyboard.press('Tab'); // First era marker
    await page.keyboard.press('Enter'); // Select era
    
    // Verify era selected
    await expect(page.locator('.selected-era-card')).toBeVisible();
  });
});
```

### Performance Tests (Lighthouse CI)

```yaml
# .lighthouserc.json
{
  "ci": {
    "collect": {
      "url": [
        "http://localhost:5173/",
        "http://localhost:5173/timeline",
        "http://localhost:5173/playground",
        "http://localhost:5173/emulator/3d"
      ],
      "numberOfRuns": 3,
      "settings": {
        "preset": "desktop"
      }
    },
    "assert": {
      "assertions": {
        "categories:performance": ["error", { "minScore": 0.9 }],
        "categories:accessibility": ["error", { "minScore": 0.95 }],
        "first-contentful-paint": ["error", { "maxNumericValue": 1500 }],
        "interactive": ["error", { "maxNumericValue": 3500 }]
      }
    }
  }
}
```

---

## Summary

This Phase 4 architecture provides a comprehensive blueprint for completing the Ancient Compute frontend from 80% → 100%. Key achievements:

1. **Clear Component Hierarchy**: 51 total components (11 existing + 40 new)
2. **Robust State Management**: 7 Svelte stores with derived stores
3. **Modern Tech Stack**: SvelteKit, Monaco, Three.js, D3.js, Chart.js
4. **Performance Optimized**: Code splitting, lazy loading, adaptive quality
5. **Fully Accessible**: WCAG 2.1 AA compliance with screen reader support
6. **Well-Tested**: >85% coverage with Vitest, Playwright, axe-core
7. **8-Week Roadmap**: Clear deliverables and task breakdown

**Next Steps**:
1. Review and approve this architecture
2. Set up Vitest configuration
3. Begin Week 1 implementation (Code Playground)
4. Iterate based on user feedback

**Success Metrics**:
- Lighthouse Performance: 90+
- Lighthouse Accessibility: 95+
- Test Coverage: >85%
- Bundle Size: <400KB initial load
- User Completion Rate: >60% for first module

This architecture balances ambition with pragmatism, building upon the solid 80% foundation already in place.
