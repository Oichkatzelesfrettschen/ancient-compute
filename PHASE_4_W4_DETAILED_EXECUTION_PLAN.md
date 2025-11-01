# Phase 4.W4: Educational Content & Timeline
## Detailed Execution Plan (Week 17)

**Duration:** November 2-6, 2025 (5 working days)
**Target:** Interactive timeline + 7 historical modules + 50+ lessons
**LOC Target:** ~3,000 frontend + ~1,500 backend + ~400 tests
**Status:** READY TO EXECUTE

---

## DAY 1: Timeline Component Architecture

### Task 1.1: Create Timeline.svelte Component (400-500 LOC)

**File:** `frontend/src/lib/components/education/Timeline.svelte`

**Purpose:** Interactive horizontal timeline spanning 12,500 years with era markers

**Component Spec:**
```svelte
<script>
  // Props
  export let selectedEra = 'prehistory';
  export let onEraSelect = (era) => {};

  // State
  let scrollPosition = 0;
  let viewportWidth = 1200;
  let timelineData = [];

  // Constants
  const ERA_WIDTH = 300; // pixels per era
  const TOTAL_WIDTH = 8 * ERA_WIDTH; // 8 eras total

  // Data structure
  const eras = [
    { id: 'prehistory', label: 'Prehistory', start: -20000, end: -3000, color: '#8B4513' },
    { id: 'ancient', label: 'Ancient', start: -3000, end: 500, color: '#CD5C5C' },
    { id: 'medieval', label: 'Medieval', start: 500, end: 1500, color: '#DAA520' },
    { id: 'early-modern', label: 'Early Modern', start: 1500, end: 1850, color: '#4169E1' },
    { id: 'foundations', label: 'Foundations Crisis', start: 1850, end: 1940, color: '#228B22' },
    { id: 'electronic', label: 'Electronic Age', start: 1940, end: 1980, color: '#DC143C' },
    { id: 'type-theory', label: 'Type Theory Evolution', start: 1970, end: 2000, color: '#FF1493' },
    { id: 'paradigm', label: 'Paradigm Synthesis', start: 1980, end: 2025, color: '#00CED1' }
  ];

  onMount(() => {
    // Load historical data
    loadTimelineData();
  });

  function loadTimelineData() {
    // Load from API or local JSON
    fetch('/api/timeline')
      .then(r => r.json())
      .then(data => {
        timelineData = data;
      });
  }

  function handleEraClick(era) {
    selectedEra = era.id;
    onEraSelect(era);
    // Smooth scroll to era position
    const eraIndex = eras.findIndex(e => e.id === era.id);
    scrollToEra(eraIndex);
  }

  function scrollToEra(index) {
    const targetScroll = index * ERA_WIDTH - viewportWidth / 2;
    // Smooth scroll animation
    scrollPosition = targetScroll;
  }
</script>

<div class="timeline-container">
  <!-- Left arrow (scroll left) -->
  <button class="timeline-nav prev" on:click={() => scroll(-500)}>←</button>

  <!-- Timeline viewport -->
  <div class="timeline-viewport" bind:clientWidth={viewportWidth}>
    <svg class="timeline-svg" width={TOTAL_WIDTH} height={200}>
      <!-- Draw baseline -->
      <line x1="0" y1="100" x2={TOTAL_WIDTH} y2="100" stroke="#ccc" stroke-width="2" />

      <!-- Draw era markers -->
      {#each eras as era, i}
        <g
          class="era-marker"
          on:click={() => handleEraClick(era)}
          class:selected={selectedEra === era.id}
        >
          <!-- Vertical line -->
          <line x1={i * ERA_WIDTH + ERA_WIDTH/2} y1="80" x2={i * ERA_WIDTH + ERA_WIDTH/2} y2="120"
                stroke={era.color} stroke-width="2" />

          <!-- Era label -->
          <text x={i * ERA_WIDTH + ERA_WIDTH/2} y="150" text-anchor="middle" font-size="12">
            {era.label}
          </text>

          <!-- Date range -->
          <text x={i * ERA_WIDTH + ERA_WIDTH/2} y="165" text-anchor="middle" font-size="10" fill="#666">
            {era.start} – {era.end}
          </text>
        </g>
      {/each}
    </svg>
  </div>

  <!-- Right arrow (scroll right) -->
  <button class="timeline-nav next" on:click={() => scroll(500)}>→</button>
</div>

<style>
  .timeline-container {
    display: flex;
    align-items: center;
    gap: 1rem;
    background: #f5f5f5;
    padding: 1rem;
    border-radius: 8px;
  }

  .timeline-viewport {
    flex: 1;
    overflow-x: auto;
    border: 1px solid #ddd;
    background: white;
    height: 200px;
  }

  .timeline-svg {
    cursor: pointer;
  }

  .era-marker {
    cursor: pointer;
    transition: opacity 0.2s;
  }

  .era-marker:hover {
    opacity: 0.7;
  }

  .era-marker.selected {
    opacity: 1;
    font-weight: bold;
  }

  .timeline-nav {
    background: #007bff;
    color: white;
    border: none;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    cursor: pointer;
  }

  .timeline-nav:hover {
    background: #0056b3;
  }
</style>
```

**Testing:**
- [ ] Component renders without errors
- [ ] Era markers click-selectable
- [ ] Smooth scrolling working
- [ ] Selected state visually distinct
- [ ] Responsive to viewport resize

**Checklist:**
- [ ] File created
- [ ] Types imported (Svelte)
- [ ] API endpoint referenced (/api/timeline)
- [ ] Styles scoped and clean
- [ ] Accessibility attributes added

---

### Task 1.2: Create HistoricalNavigator.svelte (250-300 LOC)

**File:** `frontend/src/lib/components/education/HistoricalNavigator.svelte`

**Purpose:** Era selector and module list navigation

**Key Features:**
- Dropdown for quick era selection
- Module list for selected era (with lesson counts)
- Progress indicator per module
- Visual hierarchy (era → module → lessons)

**Implementation:**
```svelte
<!-- Skeleton structure -->
<script>
  export let selectedEra = 'prehistory';
  export let onModuleSelect = (module) => {};

  let eras = [];
  let modules = [];
  let selectedModule = null;

  // Load modules when era changes
  $: if (selectedEra) {
    loadModules(selectedEra);
  }

  function loadModules(eraId) {
    fetch(`/api/timeline/${eraId}/modules`)
      .then(r => r.json())
      .then(data => {
        modules = data;
      });
  }
</script>

<div class="navigator">
  <!-- Era selector dropdown -->
  <select value={selectedEra} on:change={(e) => selectedEra = e.target.value}>
    {#each eras as era}
      <option value={era.id}>{era.label}</option>
    {/each}
  </select>

  <!-- Modules list -->
  <div class="modules-list">
    {#each modules as module}
      <div class="module-item" on:click={() => onModuleSelect(module)}>
        <h4>{module.title}</h4>
        <p>{module.description}</p>
        <div class="progress">
          <span>{module.completedLessons}/{module.totalLessons} lessons</span>
          <div class="progress-bar">
            <div class="progress-fill" style="width: {(module.completedLessons / module.totalLessons) * 100}%"></div>
          </div>
        </div>
      </div>
    {/each}
  </div>
</div>
```

---

### Task 1.3: Create TimelineData Store (300-400 LOC)

**File:** `frontend/src/lib/stores/timelineStore.ts`

**Purpose:** Centralized data management for timeline and modules

**Structure:**
```typescript
import { writable } from 'svelte/store';

export interface Era {
  id: string;
  label: string;
  description: string;
  start: number;
  end: number;
  color: string;
  icon?: string;
}

export interface Module {
  id: string;
  eraId: string;
  title: string;
  description: string;
  lessons: Lesson[];
  exercises: Exercise[];
  completedLessons: number;
  totalLessons: number;
}

export interface Lesson {
  id: string;
  moduleId: string;
  title: string;
  content: string;
  historicalContext: string;
  codeExamples: CodeExample[];
  estimatedReadTime: number;
  completed: boolean;
}

export interface Exercise {
  id: string;
  moduleId: string;
  title: string;
  problem: string;
  languages: string[]; // C, Python, Haskell, etc.
  testCases: TestCase[];
  hints: string[];
  solution: string;
  completed: boolean;
}

export interface CodeExample {
  id: string;
  language: string;
  code: string;
  description: string;
  output?: string;
}

export interface TestCase {
  input: string;
  expectedOutput: string;
  description: string;
}

// Create stores
export const eraStore = writable<Era[]>([]);
export const moduleStore = writable<Module[]>([]);
export const selectedEraStore = writable<string>('prehistory');
export const selectedModuleStore = writable<string | null>(null);

// Load all timeline data
export async function loadTimelineData() {
  const response = await fetch('/api/timeline');
  const data = await response.json();
  eraStore.set(data.eras);
  moduleStore.set(data.modules);
}

// Load specific era's modules
export async function loadEraModules(eraId: string) {
  const response = await fetch(`/api/timeline/${eraId}/modules`);
  const modules = await response.json();
  moduleStore.set(modules);
}
```

---

### Task 1.4: Timeline Tests (200-250 LOC)

**File:** `frontend/src/lib/components/education/__tests__/Timeline.test.ts`

**Test Plan:**
```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import Timeline from '../Timeline.svelte';
import { render, screen, fireEvent } from '@testing-library/svelte';

describe('Timeline Component', () => {
  it('should render all 8 era markers', () => {
    render(Timeline);
    // Check that all era labels are present
    expect(screen.getByText('Prehistory')).toBeInTheDocument();
    expect(screen.getByText('Ancient')).toBeInTheDocument();
    expect(screen.getByText('Medieval')).toBeInTheDocument();
    // ... 5 more eras
  });

  it('should select era on click', () => {
    render(Timeline);
    const ancientMarker = screen.getByText('Ancient');
    fireEvent.click(ancientMarker);

    // Check that Ancient is now selected (class or prop)
    expect(ancientMarker.parentElement).toHaveClass('selected');
  });

  it('should emit onEraSelect callback', () => {
    const mockSelect = vitest.fn();
    render(Timeline, { props: { onEraSelect: mockSelect } });

    const ancientMarker = screen.getByText('Ancient');
    fireEvent.click(ancientMarker);

    expect(mockSelect).toHaveBeenCalledWith(expect.objectContaining({ id: 'ancient' }));
  });

  it('should scroll timeline on arrow click', () => {
    render(Timeline);
    const nextButton = screen.getByRole('button', { name: /→/ });
    fireEvent.click(nextButton);

    // Check that viewport scrolled
    const viewport = document.querySelector('.timeline-viewport');
    expect(viewport?.scrollLeft).toBeGreaterThan(0);
  });
});
```

**Subtasks:**
- [ ] Install @testing-library/svelte
- [ ] Write unit tests for Timeline
- [ ] Write unit tests for HistoricalNavigator
- [ ] Write unit tests for TimelineStore
- [ ] All tests passing

---

### Task 1.5: Commit Day 1 Work

**Command:**
```bash
git add frontend/src/lib/components/education/
git add frontend/src/lib/stores/timelineStore.ts
git add frontend/src/lib/components/education/__tests__/
git commit -m "Phase 4.W4.D1: Timeline component architecture"
```

---

## DAY 2-3: Historical Module Components

### Task 2.1: Module.svelte (Parent Container, 300-400 LOC)

**File:** `frontend/src/lib/components/education/Module.svelte`

**Responsibilities:**
- Display era header with description
- Navigation (prev/next module buttons)
- Content area for lessons/exercises
- Progress tracking
- Bookmark/favorite functionality

**Key Structure:**
```svelte
<div class="module-container">
  <!-- Header -->
  <header class="module-header">
    <h1>{module.title}</h1>
    <p class="description">{module.description}</p>
    <div class="meta">
      <span>{module.lessons.length} lessons</span>
      <span>{module.exercises.length} exercises</span>
      <span>{module.estimatedTime} hours</span>
    </div>
  </header>

  <!-- Navigation -->
  <nav class="module-nav">
    <button on:click={prevModule} disabled={!hasPrevModule}>← Previous Module</button>
    <button on:click={nextModule} disabled={!hasNextModule}>Next Module →</button>
  </nav>

  <!-- Tabs: Lessons | Exercises -->
  <div class="tabs">
    <button class:active={tab === 'lessons'}>Lessons ({module.lessons.length})</button>
    <button class:active={tab === 'exercises'}>Exercises ({module.exercises.length})</button>
  </div>

  <!-- Content area -->
  <div class="content">
    {#if tab === 'lessons'}
      <LessonList lessons={module.lessons} />
    {:else}
      <ExerciseList exercises={module.exercises} />
    {/if}
  </div>

  <!-- Progress footer -->
  <footer class="progress-footer">
    <div class="progress-bar">
      <div class="progress-fill" style="width: {progress}%"></div>
    </div>
    <p>{completedLessons}/{totalLessons} lessons completed</p>
  </footer>
</div>
```

---

### Task 2.2: Lesson.svelte (Individual Lesson, 300-400 LOC)

**File:** `frontend/src/lib/components/education/Lesson.svelte`

**Responsibilities:**
- Display lesson title, description, historical context
- Render lesson content (Markdown or HTML)
- Display code examples with syntax highlighting
- Interactive timeline (lesson progression)
- Mark as complete / bookmark

**Key Sections:**
```svelte
<!-- Historical Context Box -->
<div class="historical-context">
  <h3>Historical Context</h3>
  <p>{lesson.historicalContext}</p>
  <p class="timeline">Timeline: {lesson.era.start} – {lesson.era.end}</p>
</div>

<!-- Main Lesson Content -->
<article class="lesson-content">
  <!-- Markdown renderer or raw HTML -->
  {#each lesson.contentBlocks as block}
    {#if block.type === 'heading'}
      <h{block.level}>{block.content}</h{block.level}>
    {:else if block.type === 'paragraph'}
      <p>{block.content}</p>
    {:else if block.type === 'list'}
      <ul>
        {#each block.items as item}
          <li>{item}</li>
        {/each}
      </ul>
    {/if}
  {/each}
</article>

<!-- Code Examples -->
<div class="code-examples">
  {#each lesson.codeExamples as example}
    <div class="example">
      <h4>{example.description}</h4>
      <pre><code class="language-{example.language}">{example.code}</code></pre>
      {#if example.output}
        <p><strong>Output:</strong></p>
        <pre><output>{example.output}</output></pre>
      {/if}
    </div>
  {/each}
</div>

<!-- Key Concepts Box -->
<aside class="key-concepts">
  <h3>Key Concepts</h3>
  <ul>
    {#each lesson.keyConcepts as concept}
      <li><strong>{concept.term}</strong>: {concept.definition}</li>
    {/each}
  </ul>
</aside>
```

---

### Task 2.3: Exercise.svelte (Problem Statement + Validation, 400-500 LOC)

**File:** `frontend/src/lib/components/education/Exercise.svelte`

**Responsibilities:**
- Display problem statement and requirements
- Language selector (C, Python, Haskell, etc.)
- Code editor (Monaco integration)
- Test harness runner
- Show solution (optional)
- Track completion

**Key Structure:**
```svelte
<div class="exercise-container">
  <!-- Problem Statement -->
  <section class="problem-statement">
    <h2>{exercise.title}</h2>
    <div class="requirements">
      {@html exercise.problem}
    </div>
  </section>

  <!-- Language Selector -->
  <div class="language-selector">
    <label>Language:</label>
    <select bind:value={selectedLanguage}>
      {#each exercise.languages as lang}
        <option value={lang}>{lang}</option>
      {/each}
    </select>
  </div>

  <!-- Code Editor -->
  <div class="code-editor">
    <CodeEditor
      bind:code={userCode}
      language={selectedLanguage}
    />
  </div>

  <!-- Run Tests Button -->
  <button class="run-tests" on:click={runTests} disabled={isRunning}>
    {isRunning ? 'Running...' : 'Run Tests'}
  </button>

  <!-- Test Results -->
  {#if testResults}
    <div class="test-results">
      {#each testResults.tests as test}
        <div class="test-case" class:passed={test.passed} class:failed={!test.passed}>
          <h4>{test.name}</h4>
          {#if test.passed}
            <span class="badge success">✓ PASSED</span>
          {:else}
            <span class="badge error">✗ FAILED</span>
            <p><strong>Expected:</strong> {test.expected}</p>
            <p><strong>Got:</strong> {test.actual}</p>
          {/if}
        </div>
      {/each}

      {#if testResults.allPassed}
        <div class="congratulations">
          <p>🎉 All tests passed! Exercise complete.</p>
          <button on:click={markComplete}>Mark as Complete</button>
        </div>
      {/if}
    </div>
  {/if}

  <!-- Hints & Solution -->
  <details class="hints-section">
    <summary>Hints</summary>
    <ul>
      {#each exercise.hints as hint}
        <li>{hint}</li>
      {/each}
    </ul>
  </details>

  <details class="solution-section">
    <summary>Show Solution</summary>
    <pre><code class="language-{selectedLanguage}">{exercise.solution}</code></pre>
  </details>
</div>
```

---

### Task 2.4: ProgressTracker.svelte (200-250 LOC)

**File:** `frontend/src/lib/components/education/ProgressTracker.svelte`

**Displays:**
- Overall completion percentage (all modules)
- Per-module progress (lessons completed)
- Achievement badges
- Time spent
- Streaks (daily lesson consistency)

---

### Task 2.5: Module Tests (250-300 LOC)

Create tests for:
- Module.svelte (rendering, navigation, tab switching)
- Lesson.svelte (content display, code example rendering)
- Exercise.svelte (code running, test harness)
- ProgressTracker.svelte (percentage calculations)

**Key Test Cases:**
- [ ] All lessons render correctly
- [ ] Navigation works (prev/next module)
- [ ] Code examples display with syntax highlighting
- [ ] Exercise tests pass/fail correctly
- [ ] Progress updates on lesson completion
- [ ] 20+ tests total

---

## DAY 4-5: Backend Content Delivery

### Task 4.1: Create /api/timeline Endpoint (150-200 LOC)

**File:** `backend/src/api/timeline.py`

**Endpoints:**
```python
@router.get("/timeline")
async def get_all_timeline() -> dict:
    """
    Returns all eras and modules.

    Response:
    {
      "eras": [
        {"id": "prehistory", "label": "Prehistory", ...},
        ...
      ],
      "modules": [
        {"id": "mod-1", "eraId": "prehistory", "title": "...", ...},
        ...
      ]
    }
    """
    pass

@router.get("/timeline/{era_id}")
async def get_era_details(era_id: str) -> Era:
    """Returns era details with all modules."""
    pass

@router.get("/timeline/{era_id}/modules")
async def get_era_modules(era_id: str) -> list[Module]:
    """Returns modules for given era."""
    pass

@router.get("/timeline/{era_id}/modules/{module_id}")
async def get_module_details(era_id: str, module_id: str) -> Module:
    """Returns complete module with all lessons and exercises."""
    pass
```

---

### Task 4.2: Create /api/modules Endpoint (200-250 LOC)

```python
@router.get("/modules")
async def list_all_modules() -> list[Module]:
    """List all available modules."""
    pass

@router.get("/modules/{module_id}")
async def get_module(module_id: str) -> Module:
    """Get module details."""
    pass

@router.get("/modules/{module_id}/lessons/{lesson_id}")
async def get_lesson(module_id: str, lesson_id: str) -> Lesson:
    """Get lesson with full content."""
    pass

@router.post("/modules/{module_id}/progress")
async def update_module_progress(
    module_id: str,
    progress: dict,  # {"completedLessons": [...], "completedExercises": [...]}
    user_id: str  # from auth
) -> dict:
    """Track user progress."""
    pass
```

---

### Task 4.3: Database Schema (150-200 LOC)

**File:** `backend/src/database/models.py`

**Models:**
```python
from sqlalchemy import Column, String, Integer, Text, DateTime, Boolean, JSON
from datetime import datetime

class Era(Base):
    __tablename__ = "eras"

    id = Column(String, primary_key=True)
    label = Column(String)
    description = Column(Text)
    start_year = Column(Integer)
    end_year = Column(Integer)
    color = Column(String)
    icon = Column(String, nullable=True)
    created_at = Column(DateTime, default=datetime.utcnow)

class Module(Base):
    __tablename__ = "modules"

    id = Column(String, primary_key=True)
    era_id = Column(String, ForeignKey("eras.id"))
    title = Column(String)
    description = Column(Text)
    estimated_time = Column(Integer)  # minutes
    created_at = Column(DateTime, default=datetime.utcnow)

class Lesson(Base):
    __tablename__ = "lessons"

    id = Column(String, primary_key=True)
    module_id = Column(String, ForeignKey("modules.id"))
    title = Column(String)
    content = Column(Text)  # Markdown
    historical_context = Column(Text)
    code_examples = Column(JSON)  # Array of CodeExample
    created_at = Column(DateTime, default=datetime.utcnow)

class Exercise(Base):
    __tablename__ = "exercises"

    id = Column(String, primary_key=True)
    module_id = Column(String, ForeignKey("modules.id"))
    title = Column(String)
    problem = Column(Text)
    languages = Column(JSON)  # ["C", "Python", "Haskell", ...]
    test_cases = Column(JSON)  # Array of TestCase
    hints = Column(JSON)  # Array of hints
    solution = Column(Text)
    created_at = Column(DateTime, default=datetime.utcnow)

class UserProgress(Base):
    __tablename__ = "user_progress"

    id = Column(String, primary_key=True)
    user_id = Column(String)
    module_id = Column(String, ForeignKey("modules.id"))
    completed_lessons = Column(JSON)
    completed_exercises = Column(JSON)
    started_at = Column(DateTime, default=datetime.utcnow)
    completed_at = Column(DateTime, nullable=True)
```

---

### Task 4.4: Load Historical Content (500+ lines data)

**File:** `backend/scripts/load_content.py`

**Script to:**
- Create 7 eras
- Create 50+ lessons across all eras
- Create 20+ exercises with test cases
- Load all content into database

**Example Data Structure:**
```python
content = {
  "prehistory": {
    "label": "Prehistory of Counting",
    "modules": [
      {
        "id": "prehistory-1",
        "title": "Ishango Bone & Tally Marks",
        "lessons": [
          {
            "id": "lesson-1",
            "title": "Evidence of Early Counting",
            "content": "...",
            "historical_context": "Discovered in Ishango, Democratic Republic of Congo...",
            "code_examples": [
              {
                "language": "python",
                "code": "# Represent tallies as binary\nfor i in range(10):\n    print(bin(i))",
                "description": "Binary representation of tallies"
              }
            ]
          }
        ],
        "exercises": [
          {
            "id": "ex-1",
            "title": "Implement Tally System",
            "problem": "Write a program that converts numbers to tally marks...",
            "languages": ["C", "Python", "Haskell"],
            "test_cases": [
              {"input": "5", "expected_output": "IIII|"},
              {"input": "10", "expected_output": "IIII|IIII|"}
            ],
            "solution": "..."
          }
        ]
      }
    ]
  },
  "ancient": {
    # ... 8 more modules
  }
  # ... 6 more eras
}
```

---

## ACCEPTANCE CRITERIA FOR PHASE 4.W4

### ✓ Code Delivered
- [ ] Timeline.svelte: 450 LOC
- [ ] HistoricalNavigator.svelte: 280 LOC
- [ ] TimelineStore.ts: 350 LOC
- [ ] Module.svelte: 350 LOC
- [ ] Lesson.svelte: 350 LOC
- [ ] Exercise.svelte: 450 LOC
- [ ] ProgressTracker.svelte: 220 LOC
- [ ] Timeline.py: 180 LOC
- [ ] Modules.py: 230 LOC
- [ ] Models.py: 200 LOC
- [ ] Content loader script: 500+ LOC

**Total: ~3,600 LOC**

### ✓ Tests Passing
- [ ] Timeline component: 8+ tests
- [ ] HistoricalNavigator: 6+ tests
- [ ] Module/Lesson/Exercise: 20+ tests
- [ ] Backend endpoints: 15+ tests
- [ ] Content loading: 5+ tests

**Total: 54+ tests**

### ✓ Features Working
- [ ] All 7 eras displayed in timeline
- [ ] All 50+ lessons loadable and readable
- [ ] All 20+ exercises executable
- [ ] Progress tracking functional
- [ ] Navigation smooth and responsive
- [ ] API endpoints tested and documented

### ✓ Quality Metrics
- [ ] Zero TypeScript errors
- [ ] Zero Python type errors (mypy)
- [ ] 85%+ test coverage
- [ ] Lighthouse score 85+
- [ ] All commits well-documented

---

## GIT COMMIT PLAN

**Day 1 Commit:**
```
Phase 4.W4.D1: Timeline Component Architecture (450 LOC)

Components:
- Timeline.svelte: Interactive historical timeline (8 eras)
- HistoricalNavigator.svelte: Era/module selector
- TimelineStore.ts: Centralized data management

Tests:
- 15+ unit tests for timeline components
- All tests passing

Design follows Phase 4 architectural patterns.
```

**Day 2-3 Commit:**
```
Phase 4.W4.D2-3: Historical Module Components (1,350 LOC)

Components:
- Module.svelte: Era module container
- Lesson.svelte: Individual lesson display
- Exercise.svelte: Problem + code execution UI
- ProgressTracker.svelte: Completion tracking

Features:
- Lesson content with historical context
- Code examples with syntax highlighting
- Exercise validation with test cases
- Progress visualization

Tests:
- 25+ unit tests
- Component integration tests
```

**Day 4-5 Commit:**
```
Phase 4.W4.D4-5: Backend Content Delivery (1,100 LOC)

API Endpoints:
- /api/timeline: Era and module metadata
- /api/modules: Module and lesson data
- /api/modules/{id}/progress: Progress tracking

Database:
- Eras, Modules, Lessons, Exercises tables
- User progress tracking table
- Indexed for performance

Content:
- 7 historical eras fully populated
- 50+ lessons with content and examples
- 20+ exercises with test harnesses

Tests:
- 15+ endpoint tests
- Content loading validation
```

**Phase 4.W4 Final Commit:**
```
Phase 4.W4 COMPLETE: Educational Content & Timeline

Summary:
- 3,600+ LOC across frontend and backend
- 54+ tests (all passing)
- 7 historical modules fully functional
- 50+ lessons, 20+ exercises
- API documentation complete
- Ready for Phase 4.W5 (language integration)

Metrics:
- Test coverage: 85%+
- Build time: < 5 seconds
- Zero warnings/errors
- Lighthouse: 85+
```

---

## SUCCESS CHECKLIST

- [ ] Timeline renders all 8 eras
- [ ] Era selection updates module list
- [ ] All lessons accessible and readable
- [ ] All exercises executable (code runs)
- [ ] Test results display correctly
- [ ] Progress tracking works end-to-end
- [ ] Backend APIs documented (OpenAPI/Swagger)
- [ ] Database queries optimized (< 100ms)
- [ ] All 54 tests passing
- [ ] Git history clean and well-documented

---

**Phase 4.W4 Ready for Execution**
**Estimated Effort:** 40 hours (5 days × 8 hours)
**Target Completion:** November 6, 2025

