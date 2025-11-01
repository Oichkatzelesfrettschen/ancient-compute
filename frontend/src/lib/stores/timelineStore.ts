/**
 * TimelineStore.ts - Centralized state management for historical timeline and educational content
 *
 * Manages:
 * - 8 historical eras (Prehistory through Modern)
 * - 50+ lessons organized by module
 * - 20+ exercises with validation
 * - User progress tracking across all content
 */

import { writable, derived } from 'svelte/store';

// ============================================================================
// TYPE DEFINITIONS
// ============================================================================

export interface CodeExample {
  id: string;
  language: string;
  title: string;
  code: string;
  explanation: string;
}

export interface KeyConcept {
  term: string;
  definition: string;
  historicalContext: string;
}

export interface TestCase {
  id: string;
  name: string;
  input: string;
  expectedOutput: string;
  explanation: string;
}

export interface Lesson {
  id: string;
  moduleId: string;
  eraId: string;
  title: string;
  subtitle?: string;
  description: string;
  content: string; // Markdown content
  historicalContext: string;
  codeExamples: CodeExample[];
  keyConcepts: KeyConcept[];
  estimatedReadTime: number; // minutes
  prerequisites: string[]; // lesson IDs
  nextLessons: string[]; // lesson IDs
  completed: boolean;
  completedAt?: string; // ISO timestamp
}

export interface Exercise {
  id: string;
  moduleId: string;
  eraId: string;
  title: string;
  description: string;
  problem: string; // Problem statement (markdown)
  languages: string[]; // Supported languages: C, Python, Haskell, IDRIS2, LISP, Java, etc.
  difficulty: 'beginner' | 'intermediate' | 'advanced';
  testCases: TestCase[];
  hints: string[];
  solution: string;
  timeLimit: number; // seconds
  memoryLimit: number; // MB
  completed: boolean;
  completedAt?: string;
  bestScore?: number; // percentage
}

export interface Module {
  id: string;
  eraId: string;
  title: string;
  description: string;
  subtitle?: string;
  order: number;
  lessons: Lesson[];
  exercises: Exercise[];
  estimatedTime: number; // hours
  icon?: string;
  color?: string;
  completedLessons: number;
  completedExercises: number;
}

export interface Era {
  id: string;
  label: string;
  fullName: string;
  description: string;
  historicalContext: string;
  startYear: number;
  endYear: number;
  color: string;
  icon?: string;
  order: number;
  modules: Module[];
}

export interface TimelineState {
  eras: Era[];
  selectedEraId: string | null;
  selectedModuleId: string | null;
  selectedLessonId: string | null;
  currentView: 'timeline' | 'era' | 'module' | 'lesson' | 'exercise';
  isLoading: boolean;
  error: string | null;
}

export interface UserProgress {
  userId: string;
  eraId: string;
  moduleId: string;
  completedLessonIds: string[];
  completedExerciseIds: string[];
  lastAccessedAt: string; // ISO timestamp
  totalTimeSpent: number; // seconds
}

// ============================================================================
// STORE INITIALIZATION
// ============================================================================

const initialState: TimelineState = {
  eras: [],
  selectedEraId: null,
  selectedModuleId: null,
  selectedLessonId: null,
  currentView: 'timeline',
  isLoading: false,
  error: null,
};

export const timelineStore = writable<TimelineState>(initialState);

// ============================================================================
// DERIVED STORES
// ============================================================================

/**
 * Get currently selected era
 */
export const currentEra = derived(
  timelineStore,
  ($state) =>
    $state.selectedEraId
      ? $state.eras.find((e) => e.id === $state.selectedEraId)
      : null
);

/**
 * Get currently selected module
 */
export const currentModule = derived(
  [timelineStore, currentEra],
  ([$state, $era]) =>
    $state.selectedModuleId && $era
      ? $era.modules.find((m) => m.id === $state.selectedModuleId)
      : null
);

/**
 * Get currently selected lesson
 */
export const currentLesson = derived(
  [timelineStore, currentModule],
  ([$state, $module]) =>
    $state.selectedLessonId && $module
      ? $module.lessons.find((l) => l.id === $state.selectedLessonId)
      : null
);

/**
 * Calculate overall progress percentage
 */
export const overallProgress = derived(
  timelineStore,
  ($state) => {
    const totalLessons = $state.eras.reduce(
      (sum, era) =>
        sum +
        era.modules.reduce((modSum, mod) => modSum + mod.lessons.length, 0),
      0
    );

    const completedLessons = $state.eras.reduce(
      (sum, era) =>
        sum +
        era.modules.reduce(
          (modSum, mod) =>
            modSum + mod.lessons.filter((l) => l.completed).length,
          0
        ),
      0
    );

    return totalLessons > 0
      ? Math.round((completedLessons / totalLessons) * 100)
      : 0;
  }
);

/**
 * Get list of all available eras in order
 */
export const erasList = derived(
  timelineStore,
  ($state) => $state.eras.sort((a, b) => a.order - b.order)
);

/**
 * Get modules for currently selected era
 */
export const currentEraModules = derived(
  currentEra,
  ($era) => ($era ? $era.modules.sort((a, b) => a.order - b.order) : [])
);

// ============================================================================
// STORE ACTIONS
// ============================================================================

/**
 * Load all timeline data from backend API
 */
export async function loadTimelineData(): Promise<void> {
  timelineStore.update((state) => ({
    ...state,
    isLoading: true,
    error: null,
  }));

  try {
    const response = await fetch('/api/timeline');
    if (!response.ok) {
      throw new Error(`Failed to load timeline: ${response.statusText}`);
    }

    const data = await response.json();

    timelineStore.update((state) => ({
      ...state,
      eras: data.eras || [],
      isLoading: false,
    }));

    // Auto-select first era if available
    const firstEra = data.eras?.[0];
    if (firstEra) {
      selectEra(firstEra.id);
    }
  } catch (error) {
    const errorMessage =
      error instanceof Error ? error.message : 'Unknown error loading timeline';
    timelineStore.update((state) => ({
      ...state,
      isLoading: false,
      error: errorMessage,
    }));

    console.error('[TimelineStore] Error loading data:', error);
  }
}

/**
 * Select an era and load its modules
 */
export function selectEra(eraId: string): void {
  timelineStore.update((state) => {
    const era = state.eras.find((e) => e.id === eraId);
    return {
      ...state,
      selectedEraId: eraId,
      selectedModuleId: era?.modules?.[0]?.id || null,
      selectedLessonId: null,
      currentView: 'era',
    };
  });
}

/**
 * Select a module within current era
 */
export function selectModule(moduleId: string): void {
  timelineStore.update((state) => {
    const module = state.eras
      .find((e) => e.id === state.selectedEraId)
      ?.modules.find((m) => m.id === moduleId);

    return {
      ...state,
      selectedModuleId: moduleId,
      selectedLessonId: module?.lessons?.[0]?.id || null,
      currentView: 'module',
    };
  });
}

/**
 * Select a lesson within current module
 */
export function selectLesson(lessonId: string): void {
  timelineStore.update((state) => ({
    ...state,
    selectedLessonId: lessonId,
    currentView: 'lesson',
  }));
}

/**
 * Mark lesson as completed and record timestamp
 */
export function markLessonComplete(lessonId: string): void {
  const now = new Date().toISOString();

  timelineStore.update((state) => ({
    ...state,
    eras: state.eras.map((era) => ({
      ...era,
      modules: era.modules.map((module) => ({
        ...module,
        lessons: module.lessons.map((lesson) =>
          lesson.id === lessonId
            ? { ...lesson, completed: true, completedAt: now }
            : lesson
        ),
        completedLessons: module.lessons.filter(
          (l) => l.id === lessonId || l.completed
        ).length,
      })),
    })),
  }));
}

/**
 * Mark exercise as completed with score
 */
export function markExerciseComplete(
  exerciseId: string,
  score: number
): void {
  const now = new Date().toISOString();

  timelineStore.update((state) => ({
    ...state,
    eras: state.eras.map((era) => ({
      ...era,
      modules: era.modules.map((module) => ({
        ...module,
        exercises: module.exercises.map((exercise) =>
          exercise.id === exerciseId
            ? {
                ...exercise,
                completed: true,
                completedAt: now,
                bestScore: Math.max(exercise.bestScore || 0, score),
              }
            : exercise
        ),
        completedExercises: module.exercises.filter(
          (e) => e.id === exerciseId || e.completed
        ).length,
      })),
    })),
  }));
}

/**
 * Navigate to previous lesson
 */
export function previousLesson(): void {
  timelineStore.update((state) => {
    const module = state.eras
      .find((e) => e.id === state.selectedEraId)
      ?.modules.find((m) => m.id === state.selectedModuleId);

    if (!module) return state;

    const currentIndex = module.lessons.findIndex(
      (l) => l.id === state.selectedLessonId
    );
    if (currentIndex <= 0) return state;

    return {
      ...state,
      selectedLessonId: module.lessons[currentIndex - 1].id,
    };
  });
}

/**
 * Navigate to next lesson
 */
export function nextLesson(): void {
  timelineStore.update((state) => {
    const module = state.eras
      .find((e) => e.id === state.selectedEraId)
      ?.modules.find((m) => m.id === state.selectedModuleId);

    if (!module) return state;

    const currentIndex = module.lessons.findIndex(
      (l) => l.id === state.selectedLessonId
    );
    if (currentIndex >= module.lessons.length - 1) return state;

    return {
      ...state,
      selectedLessonId: module.lessons[currentIndex + 1].id,
    };
  });
}

/**
 * Navigate to previous module in current era
 */
export function previousModule(): void {
  timelineStore.update((state) => {
    const era = state.eras.find((e) => e.id === state.selectedEraId);
    if (!era) return state;

    const currentIndex = era.modules.findIndex(
      (m) => m.id === state.selectedModuleId
    );
    if (currentIndex <= 0) return state;

    return {
      ...state,
      selectedModuleId: era.modules[currentIndex - 1].id,
      selectedLessonId: era.modules[currentIndex - 1].lessons[0]?.id || null,
    };
  });
}

/**
 * Navigate to next module in current era
 */
export function nextModule(): void {
  timelineStore.update((state) => {
    const era = state.eras.find((e) => e.id === state.selectedEraId);
    if (!era) return state;

    const currentIndex = era.modules.findIndex(
      (m) => m.id === state.selectedModuleId
    );
    if (currentIndex >= era.modules.length - 1) return state;

    return {
      ...state,
      selectedModuleId: era.modules[currentIndex + 1].id,
      selectedLessonId: era.modules[currentIndex + 1].lessons[0]?.id || null,
    };
  });
}

/**
 * Reset timeline to initial state
 */
export function resetTimeline(): void {
  timelineStore.set(initialState);
}
