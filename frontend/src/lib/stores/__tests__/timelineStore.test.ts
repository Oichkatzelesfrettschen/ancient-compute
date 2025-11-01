/**
 * timelineStore.test.ts - Unit tests for TimelineStore
 *
 * Tests:
 * - Store initialization and state management
 * - Era selection and navigation
 * - Module and lesson selection
 * - Progress tracking
 * - Derived stores (currentEra, currentModule, overallProgress)
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  timelineStore,
  currentEra,
  currentModule,
  currentLesson,
  overallProgress,
  erasList,
  currentEraModules,
  loadTimelineData,
  selectEra,
  selectModule,
  selectLesson,
  markLessonComplete,
  markExerciseComplete,
  previousLesson,
  nextLesson,
  previousModule,
  nextModule,
  resetTimeline,
  type Era,
  type Module,
  type Lesson,
  type TimelineState,
} from '../timelineStore';

// ============================================================================
// TEST DATA
// ============================================================================

const mockLesson: Lesson = {
  id: 'lesson-1',
  moduleId: 'module-1',
  eraId: 'era-1',
  title: 'Introduction to Computation',
  description: 'Learn the basics',
  content: '# Content',
  historicalContext: 'Ancient Egypt',
  codeExamples: [],
  keyConcepts: [],
  estimatedReadTime: 15,
  prerequisites: [],
  nextLessons: [],
  completed: false,
};

const mockModule: Module = {
  id: 'module-1',
  eraId: 'era-1',
  title: 'Ancient Foundations',
  description: 'Ancient computational thinking',
  order: 1,
  lessons: [mockLesson],
  exercises: [],
  estimatedTime: 2,
  completedLessons: 0,
  completedExercises: 0,
};

const mockEra: Era = {
  id: 'era-1',
  label: 'Ancient',
  fullName: 'Ancient Period',
  description: 'Ancient computational systems',
  historicalContext: '3000 BC - 500 AD',
  startYear: -3000,
  endYear: 500,
  color: '#CD5C5C',
  order: 1,
  modules: [mockModule],
};

// ============================================================================
// TESTS
// ============================================================================

describe('TimelineStore', () => {
  beforeEach(() => {
    resetTimeline();
  });

  // ========================================================================
  // STORE INITIALIZATION
  // ========================================================================

  describe('Initialization', () => {
    it('should initialize with empty state', () => {
      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.eras).toEqual([]);
      expect(state!.selectedEraId).toBeNull();
      expect(state!.selectedModuleId).toBeNull();
      expect(state!.selectedLessonId).toBeNull();
      expect(state!.currentView).toBe('timeline');
      expect(state!.isLoading).toBe(false);
      expect(state!.error).toBeNull();
    });

    it('should have empty erasList initially', () => {
      let list: Era[] = [];
      erasList.subscribe((value) => {
        list = value;
      });

      expect(list).toEqual([]);
    });

    it('should have null currentEra initially', () => {
      let era: Era | null = null;
      currentEra.subscribe((value) => {
        era = value;
      });

      expect(era).toBeNull();
    });
  });

  // ========================================================================
  // ERA SELECTION
  // ========================================================================

  describe('Era Selection', () => {
    it('should update store when selecting era', () => {
      // Manually set eras in store for testing
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
      }));

      selectEra('era-1');

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedEraId).toBe('era-1');
      expect(state!.currentView).toBe('era');
    });

    it('should auto-select first module when selecting era', () => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
      }));

      selectEra('era-1');

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedModuleId).toBe('module-1');
    });

    it('should update currentEra derived store', () => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
      }));

      selectEra('era-1');

      let era: Era | null = null;
      currentEra.subscribe((value) => {
        era = value;
      });

      expect(era).toEqual(mockEra);
      expect(era?.label).toBe('Ancient');
    });
  });

  // ========================================================================
  // MODULE SELECTION
  // ========================================================================

  describe('Module Selection', () => {
    beforeEach(() => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
        selectedEraId: 'era-1',
      }));
    });

    it('should select module and auto-select first lesson', () => {
      selectModule('module-1');

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedModuleId).toBe('module-1');
      expect(state!.selectedLessonId).toBe('lesson-1');
      expect(state!.currentView).toBe('module');
    });

    it('should update currentModule derived store', () => {
      selectModule('module-1');

      let module: Module | null = null;
      currentModule.subscribe((value) => {
        module = value;
      });

      expect(module).toEqual(mockModule);
      expect(module?.title).toBe('Ancient Foundations');
    });
  });

  // ========================================================================
  // LESSON SELECTION
  // ========================================================================

  describe('Lesson Selection', () => {
    beforeEach(() => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
        selectedEraId: 'era-1',
        selectedModuleId: 'module-1',
      }));
    });

    it('should select lesson', () => {
      selectLesson('lesson-1');

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedLessonId).toBe('lesson-1');
      expect(state!.currentView).toBe('lesson');
    });

    it('should update currentLesson derived store', () => {
      selectLesson('lesson-1');

      let lesson: Lesson | null = null;
      currentLesson.subscribe((value) => {
        lesson = value;
      });

      expect(lesson).toEqual(mockLesson);
      expect(lesson?.title).toBe('Introduction to Computation');
    });
  });

  // ========================================================================
  // PROGRESS TRACKING
  // ========================================================================

  describe('Progress Tracking', () => {
    beforeEach(() => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
      }));
    });

    it('should mark lesson as completed with timestamp', () => {
      const beforeTime = new Date().toISOString();
      markLessonComplete('lesson-1');
      const afterTime = new Date().toISOString();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      const lesson = state!.eras[0].modules[0].lessons[0];
      expect(lesson.completed).toBe(true);
      expect(lesson.completedAt).toBeDefined();

      const completedAt = new Date(lesson.completedAt!);
      expect(completedAt.getTime()).toBeGreaterThanOrEqual(new Date(beforeTime).getTime());
      expect(completedAt.getTime()).toBeLessThanOrEqual(new Date(afterTime).getTime());
    });

    it('should update completedLessons count in module', () => {
      markLessonComplete('lesson-1');

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      const module = state!.eras[0].modules[0];
      expect(module.completedLessons).toBeGreaterThan(0);
    });

    it('should mark exercise as completed with score', () => {
      timelineStore.update((state) => ({
        ...state,
        eras: state.eras.map((era) => ({
          ...era,
          modules: era.modules.map((module) => ({
            ...module,
            exercises: [
              {
                id: 'exercise-1',
                moduleId: 'module-1',
                eraId: 'era-1',
                title: 'Exercise 1',
                description: 'Test',
                problem: 'Solve this',
                languages: ['python'],
                difficulty: 'beginner',
                testCases: [],
                hints: [],
                solution: '',
                timeLimit: 60,
                memoryLimit: 256,
                completed: false,
              },
            ],
          })),
        })),
      }));

      const beforeTime = new Date().toISOString();
      markExerciseComplete('exercise-1', 95);
      const afterTime = new Date().toISOString();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      const exercise = state!.eras[0].modules[0].exercises[0];
      expect(exercise.completed).toBe(true);
      expect(exercise.bestScore).toBe(95);
      expect(exercise.completedAt).toBeDefined();

      const completedAt = new Date(exercise.completedAt!);
      expect(completedAt.getTime()).toBeGreaterThanOrEqual(new Date(beforeTime).getTime());
      expect(completedAt.getTime()).toBeLessThanOrEqual(new Date(afterTime).getTime());
    });

    it('should track best score for exercises', () => {
      // Set up exercise
      timelineStore.update((state) => ({
        ...state,
        eras: state.eras.map((era) => ({
          ...era,
          modules: era.modules.map((module) => ({
            ...module,
            exercises: [
              {
                id: 'exercise-1',
                moduleId: 'module-1',
                eraId: 'era-1',
                title: 'Exercise 1',
                description: 'Test',
                problem: 'Solve this',
                languages: ['python'],
                difficulty: 'beginner',
                testCases: [],
                hints: [],
                solution: '',
                timeLimit: 60,
                memoryLimit: 256,
                completed: false,
              },
            ],
          })),
        })),
      }));

      markExerciseComplete('exercise-1', 75);
      markExerciseComplete('exercise-1', 85);
      markExerciseComplete('exercise-1', 95);

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      const exercise = state!.eras[0].modules[0].exercises[0];
      expect(exercise.bestScore).toBe(95); // Should keep highest score
    });
  });

  // ========================================================================
  // NAVIGATION
  // ========================================================================

  describe('Navigation', () => {
    const lesson1: Lesson = { ...mockLesson, id: 'lesson-1' };
    const lesson2: Lesson = { ...mockLesson, id: 'lesson-2' };
    const lesson3: Lesson = { ...mockLesson, id: 'lesson-3' };

    const moduleWithMultipleLessons: Module = {
      ...mockModule,
      lessons: [lesson1, lesson2, lesson3],
    };

    const eraWithMultipleModules: Era = {
      ...mockEra,
      modules: [
        moduleWithMultipleLessons,
        { ...mockModule, id: 'module-2', title: 'Module 2' },
      ],
    };

    beforeEach(() => {
      timelineStore.update((state) => ({
        ...state,
        eras: [eraWithMultipleModules],
        selectedEraId: 'era-1',
        selectedModuleId: 'module-1',
        selectedLessonId: 'lesson-1',
      }));
    });

    it('should navigate to next lesson', () => {
      nextLesson();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedLessonId).toBe('lesson-2');
    });

    it('should navigate to previous lesson', () => {
      timelineStore.update((state) => ({
        ...state,
        selectedLessonId: 'lesson-2',
      }));

      previousLesson();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedLessonId).toBe('lesson-1');
    });

    it('should not go beyond last lesson', () => {
      timelineStore.update((state) => ({
        ...state,
        selectedLessonId: 'lesson-3',
      }));

      nextLesson();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedLessonId).toBe('lesson-3');
    });

    it('should not go before first lesson', () => {
      previousLesson();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedLessonId).toBe('lesson-1');
    });

    it('should navigate to next module', () => {
      nextModule();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedModuleId).toBe('module-2');
    });

    it('should navigate to previous module', () => {
      timelineStore.update((state) => ({
        ...state,
        selectedModuleId: 'module-2',
      }));

      previousModule();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.selectedModuleId).toBe('module-1');
    });
  });

  // ========================================================================
  // DERIVED STORES
  // ========================================================================

  describe('Derived Stores', () => {
    it('should calculate overall progress correctly', () => {
      const lesson1 = { ...mockLesson, id: 'lesson-1', completed: true };
      const lesson2 = { ...mockLesson, id: 'lesson-2', completed: false };

      const era = {
        ...mockEra,
        modules: [
          {
            ...mockModule,
            lessons: [lesson1, lesson2],
            completedLessons: 1,
          },
        ],
      };

      timelineStore.update((state) => ({
        ...state,
        eras: [era],
      }));

      let progress: number = 0;
      overallProgress.subscribe((value) => {
        progress = value;
      });

      expect(progress).toBe(50); // 1 of 2 lessons completed
    });

    it('should return sorted erasList', () => {
      const era1 = { ...mockEra, id: 'era-1', order: 3 };
      const era2 = { ...mockEra, id: 'era-2', order: 1 };
      const era3 = { ...mockEra, id: 'era-3', order: 2 };

      timelineStore.update((state) => ({
        ...state,
        eras: [era1, era2, era3],
      }));

      let list: Era[] = [];
      erasList.subscribe((value) => {
        list = value;
      });

      expect(list[0].order).toBe(1);
      expect(list[1].order).toBe(2);
      expect(list[2].order).toBe(3);
    });

    it('should return current era modules sorted', () => {
      const module1 = { ...mockModule, id: 'module-1', order: 2 };
      const module2 = { ...mockModule, id: 'module-2', order: 1 };

      const era = {
        ...mockEra,
        modules: [module1, module2],
      };

      timelineStore.update((state) => ({
        ...state,
        eras: [era],
        selectedEraId: 'era-1',
      }));

      let modules: Module[] = [];
      currentEraModules.subscribe((value) => {
        modules = value;
      });

      expect(modules[0].order).toBe(1);
      expect(modules[1].order).toBe(2);
    });
  });

  // ========================================================================
  // RESET
  // ========================================================================

  describe('Reset', () => {
    it('should reset timeline to initial state', () => {
      timelineStore.update((state) => ({
        ...state,
        eras: [mockEra],
        selectedEraId: 'era-1',
        selectedModuleId: 'module-1',
      }));

      resetTimeline();

      let state: TimelineState;
      timelineStore.subscribe((value) => {
        state = value;
      });

      expect(state!.eras).toEqual([]);
      expect(state!.selectedEraId).toBeNull();
      expect(state!.selectedModuleId).toBeNull();
      expect(state!.selectedLessonId).toBeNull();
    });
  });
});
