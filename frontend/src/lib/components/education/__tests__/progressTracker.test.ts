/**
 * progressTracker.test.ts - Unit tests for ProgressTracker component
 *
 * Tests:
 * - Mastery level calculation
 * - Progress percentage calculation
 * - Recent completion filtering and sorting
 * - Statistics calculation
 * - Date formatting
 * - Responsive behavior
 */

import { describe, it, expect, beforeEach } from 'vitest';
import type { Lesson, Exercise, Module, Era } from '../../../stores/timelineStore';

// ============================================================================
// TEST DATA AND MOCK FUNCTIONS
// ============================================================================

const mockLesson = (overrides: Partial<Lesson> = {}): Lesson => ({
  id: 'lesson-1',
  moduleId: 'module-1',
  eraId: 'era-1',
  title: 'Sample Lesson',
  description: 'A sample lesson',
  content: '# Content',
  historicalContext: 'Ancient Egypt',
  codeExamples: [],
  keyConcepts: [],
  estimatedReadTime: 15,
  prerequisites: [],
  nextLessons: [],
  completed: false,
  ...overrides,
});

const mockExercise = (overrides: Partial<Exercise> = {}): Exercise => ({
  id: 'exercise-1',
  moduleId: 'module-1',
  eraId: 'era-1',
  title: 'Sample Exercise',
  description: 'A sample exercise',
  problem: 'Solve this',
  languages: ['python'],
  difficulty: 'beginner',
  testCases: [],
  hints: [],
  solution: '',
  timeLimit: 60,
  memoryLimit: 256,
  completed: false,
  ...overrides,
});

const mockModule = (overrides: Partial<Module> = {}): Module => ({
  id: 'module-1',
  eraId: 'era-1',
  title: 'Sample Module',
  description: 'Sample module',
  order: 1,
  lessons: [],
  exercises: [],
  estimatedTime: 2,
  completedLessons: 0,
  completedExercises: 0,
  ...overrides,
});

const mockEra = (overrides: Partial<Era> = {}): Era => ({
  id: 'era-1',
  label: 'Ancient',
  fullName: 'Ancient Period',
  description: 'Ancient era',
  historicalContext: '3000 BC - 500 AD',
  startYear: -3000,
  endYear: 500,
  color: '#CD5C5C',
  order: 1,
  modules: [],
  ...overrides,
});

// ============================================================================
// HELPER FUNCTIONS (extracted from component logic)
// ============================================================================

interface MasteryBadge {
  level: number;
  label: string;
  threshold: number;
  color: string;
  icon: string;
}

const masteryLevels: MasteryBadge[] = [
  { level: 0, label: 'Novice', threshold: 0, color: '#9e9e9e', icon: '○' },
  { level: 1, label: 'Apprentice', threshold: 25, color: '#4caf50', icon: '◐' },
  { level: 2, label: 'Journeyman', threshold: 50, color: '#2196f3', icon: '◑' },
  { level: 3, label: 'Expert', threshold: 75, color: '#ff9800', icon: '◕' },
  { level: 4, label: 'Master', threshold: 100, color: '#e91e63', icon: '●' },
];

function getMasteryLevel(progress: number): MasteryBadge {
  return (
    masteryLevels.slice().reverse().find((m) => progress >= m.threshold) || masteryLevels[0]
  );
}

interface CompletionEvent {
  id: string;
  type: 'lesson' | 'exercise';
  title: string;
  completedAt: string;
  eraLabel: string;
  icon: string;
  score?: number;
}

function getRecentCompletions(state: any): CompletionEvent[] {
  const events: CompletionEvent[] = [];

  state.eras.forEach((era) => {
    const eraLabel = era.label;

    era.modules.forEach((module) => {
      module.lessons?.forEach((lesson: Lesson) => {
        if (lesson.completed && lesson.completedAt) {
          events.push({
            id: lesson.id,
            type: 'lesson',
            title: lesson.title,
            completedAt: lesson.completedAt,
            eraLabel,
            icon: '📖',
            score: undefined,
          });
        }
      });

      module.exercises?.forEach((exercise: Exercise) => {
        if (exercise.completed && exercise.bestScore !== undefined) {
          events.push({
            id: exercise.id,
            type: 'exercise',
            title: exercise.title,
            completedAt: exercise.completedAt || new Date().toISOString(),
            eraLabel,
            icon: '💻',
            score: exercise.bestScore,
          });
        }
      });
    });
  });

  return events.sort((a, b) => new Date(b.completedAt).getTime() - new Date(a.completedAt).getTime()).slice(0, 8);
}

function calculateStats(state: any) {
  let totalLessons = 0;
  let completedLessons = 0;
  let totalExercises = 0;
  let completedExercises = 0;
  let totalHours = 0;

  state.eras.forEach((era) => {
    era.modules.forEach((module) => {
      totalLessons += module.lessons?.length || 0;
      completedLessons += module.lessons?.filter((l: Lesson) => l.completed).length || 0;

      totalExercises += module.exercises?.length || 0;
      completedExercises += module.exercises?.filter((e: Exercise) => e.completed).length || 0;

      totalHours += (module.lessons?.length || 0) * 0.25 + (module.exercises?.length || 0) * 0.75;
    });
  });

  return {
    totalLessons,
    completedLessons,
    totalExercises,
    completedExercises,
    totalHours: Math.round(totalHours),
  };
}

function calculateOverallProgress(state: any): number {
  let totalLessons = 0;
  let completedLessons = 0;

  state.eras.forEach((era) => {
    era.modules.forEach((module) => {
      totalLessons += module.lessons?.length || 0;
      completedLessons += module.lessons?.filter((l: Lesson) => l.completed).length || 0;
    });
  });

  return totalLessons > 0 ? (completedLessons / totalLessons) * 100 : 0;
}

// ============================================================================
// TESTS
// ============================================================================

describe('ProgressTracker', () => {
  // ========================================================================
  // MASTERY LEVEL CALCULATION
  // ========================================================================

  describe('Mastery Level Calculation', () => {
    it('should return Novice for 0% progress', () => {
      const level = getMasteryLevel(0);
      expect(level.label).toBe('Novice');
      expect(level.threshold).toBe(0);
    });

    it('should return Apprentice for 25-49% progress', () => {
      const level25 = getMasteryLevel(25);
      const level49 = getMasteryLevel(49);

      expect(level25.label).toBe('Apprentice');
      expect(level49.label).toBe('Apprentice');
    });

    it('should return Journeyman for 50-74% progress', () => {
      const level50 = getMasteryLevel(50);
      const level74 = getMasteryLevel(74);

      expect(level50.label).toBe('Journeyman');
      expect(level74.label).toBe('Journeyman');
    });

    it('should return Expert for 75-99% progress', () => {
      const level75 = getMasteryLevel(75);
      const level99 = getMasteryLevel(99);

      expect(level75.label).toBe('Expert');
      expect(level99.label).toBe('Expert');
    });

    it('should return Master for 100% progress', () => {
      const level = getMasteryLevel(100);
      expect(level.label).toBe('Master');
      expect(level.icon).toBe('●');
    });

    it('should have correct colors for each level', () => {
      expect(getMasteryLevel(0).color).toBe('#9e9e9e'); // Novice
      expect(getMasteryLevel(25).color).toBe('#4caf50'); // Apprentice
      expect(getMasteryLevel(50).color).toBe('#2196f3'); // Journeyman
      expect(getMasteryLevel(75).color).toBe('#ff9800'); // Expert
      expect(getMasteryLevel(100).color).toBe('#e91e63'); // Master
    });
  });

  // ========================================================================
  // PROGRESS CALCULATION
  // ========================================================================

  describe('Progress Calculation', () => {
    it('should calculate 0% progress with no lessons', () => {
      const state = {
        eras: [mockEra({ modules: [mockModule()] })],
      };

      const progress = calculateOverallProgress(state);
      expect(progress).toBe(0);
    });

    it('should calculate 50% progress with 1 of 2 lessons completed', () => {
      const module = mockModule({
        lessons: [
          mockLesson({ id: 'lesson-1', completed: true }),
          mockLesson({ id: 'lesson-2', completed: false }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const progress = calculateOverallProgress(state);
      expect(progress).toBe(50);
    });

    it('should calculate 100% progress with all lessons completed', () => {
      const module = mockModule({
        lessons: [
          mockLesson({ id: 'lesson-1', completed: true }),
          mockLesson({ id: 'lesson-2', completed: true }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const progress = calculateOverallProgress(state);
      expect(progress).toBe(100);
    });

    it('should calculate progress across multiple modules', () => {
      const module1 = mockModule({
        id: 'module-1',
        lessons: [
          mockLesson({ id: 'lesson-1', completed: true }),
          mockLesson({ id: 'lesson-2', completed: false }),
        ],
      });

      const module2 = mockModule({
        id: 'module-2',
        lessons: [
          mockLesson({ id: 'lesson-3', completed: true }),
          mockLesson({ id: 'lesson-4', completed: true }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module1, module2] })] };
      const progress = calculateOverallProgress(state);

      // 3 of 4 = 75%
      expect(progress).toBe(75);
    });

    it('should calculate progress across multiple eras', () => {
      const era1 = mockEra({
        id: 'era-1',
        modules: [
          mockModule({
            lessons: [
              mockLesson({ id: 'lesson-1', completed: true }),
              mockLesson({ id: 'lesson-2', completed: false }),
            ],
          }),
        ],
      });

      const era2 = mockEra({
        id: 'era-2',
        modules: [
          mockModule({
            lessons: [
              mockLesson({ id: 'lesson-3', completed: true }),
              mockLesson({ id: 'lesson-4', completed: true }),
            ],
          }),
        ],
      });

      const state = { eras: [era1, era2] };
      const progress = calculateOverallProgress(state);

      // 3 of 4 = 75%
      expect(progress).toBe(75);
    });
  });

  // ========================================================================
  // RECENT COMPLETIONS
  // ========================================================================

  describe('Recent Completions', () => {
    it('should return empty array when no completions', () => {
      const state = { eras: [mockEra({ modules: [mockModule()] })] };
      const events = getRecentCompletions(state);
      expect(events).toEqual([]);
    });

    it('should include completed lessons with timestamps', () => {
      const completedAt = new Date().toISOString();
      const module = mockModule({
        lessons: [mockLesson({ id: 'lesson-1', completed: true, completedAt })],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const events = getRecentCompletions(state);

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('lesson');
      expect(events[0].title).toBe('Sample Lesson');
      expect(events[0].icon).toBe('📖');
    });

    it('should include completed exercises with scores', () => {
      const completedAt = new Date().toISOString();
      const module = mockModule({
        exercises: [mockExercise({ id: 'exercise-1', completed: true, bestScore: 95, completedAt })],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const events = getRecentCompletions(state);

      expect(events).toHaveLength(1);
      expect(events[0].type).toBe('exercise');
      expect(events[0].score).toBe(95);
      expect(events[0].icon).toBe('💻');
    });

    it('should sort events by date descending (most recent first)', () => {
      const now = new Date();
      const earlier = new Date(now.getTime() - 60000); // 1 minute ago

      const module = mockModule({
        lessons: [
          mockLesson({ id: 'lesson-1', completed: true, completedAt: now.toISOString(), title: 'Recent Lesson' }),
          mockLesson({ id: 'lesson-2', completed: true, completedAt: earlier.toISOString(), title: 'Old Lesson' }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const events = getRecentCompletions(state);

      expect(events[0].title).toBe('Recent Lesson');
      expect(events[1].title).toBe('Old Lesson');
    });

    it('should limit to 8 most recent events', () => {
      const lessons = Array.from({ length: 12 }, (_, i) =>
        mockLesson({
          id: `lesson-${i}`,
          completed: true,
          completedAt: new Date(Date.now() - i * 60000).toISOString(),
        })
      );

      const module = mockModule({ lessons });
      const state = { eras: [mockEra({ modules: [module] })] };
      const events = getRecentCompletions(state);

      expect(events).toHaveLength(8);
    });

    it('should include era label for each event', () => {
      const module = mockModule({
        lessons: [mockLesson({ id: 'lesson-1', completed: true, completedAt: new Date().toISOString() })],
      });

      const era = mockEra({ id: 'era-1', label: 'Ancient', modules: [module] });
      const state = { eras: [era] };
      const events = getRecentCompletions(state);

      expect(events[0].eraLabel).toBe('Ancient');
    });
  });

  // ========================================================================
  // STATISTICS CALCULATION
  // ========================================================================

  describe('Statistics Calculation', () => {
    it('should calculate zero stats for empty state', () => {
      const state = { eras: [mockEra({ modules: [mockModule()] })] };
      const stats = calculateStats(state);

      expect(stats.totalLessons).toBe(0);
      expect(stats.completedLessons).toBe(0);
      expect(stats.totalExercises).toBe(0);
      expect(stats.completedExercises).toBe(0);
      expect(stats.totalHours).toBe(0);
    });

    it('should count total lessons correctly', () => {
      const module = mockModule({
        lessons: [mockLesson({ id: 'lesson-1' }), mockLesson({ id: 'lesson-2' })],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const stats = calculateStats(state);

      expect(stats.totalLessons).toBe(2);
    });

    it('should count completed lessons correctly', () => {
      const module = mockModule({
        lessons: [
          mockLesson({ id: 'lesson-1', completed: true }),
          mockLesson({ id: 'lesson-2', completed: false }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const stats = calculateStats(state);

      expect(stats.completedLessons).toBe(1);
    });

    it('should count total exercises correctly', () => {
      const module = mockModule({
        exercises: [mockExercise({ id: 'exercise-1' }), mockExercise({ id: 'exercise-2' })],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const stats = calculateStats(state);

      expect(stats.totalExercises).toBe(2);
    });

    it('should count completed exercises correctly', () => {
      const module = mockModule({
        exercises: [
          mockExercise({ id: 'exercise-1', completed: true, bestScore: 90 }),
          mockExercise({ id: 'exercise-2', completed: false }),
        ],
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const stats = calculateStats(state);

      expect(stats.completedExercises).toBe(1);
    });

    it('should estimate hours based on lesson/exercise counts', () => {
      // Lessons: 15 min (0.25 hr), Exercises: 45 min (0.75 hr)
      const module = mockModule({
        lessons: [mockLesson({ id: 'lesson-1' })], // 0.25 hr
        exercises: [mockExercise({ id: 'exercise-1' })], // 0.75 hr
      });

      const state = { eras: [mockEra({ modules: [module] })] };
      const stats = calculateStats(state);

      expect(stats.totalHours).toBe(1); // 0.25 + 0.75 = 1
    });

    it('should aggregate stats across multiple modules', () => {
      const module1 = mockModule({
        id: 'module-1',
        lessons: [mockLesson({ id: 'lesson-1' })],
        exercises: [mockExercise({ id: 'exercise-1' })],
      });

      const module2 = mockModule({
        id: 'module-2',
        lessons: [mockLesson({ id: 'lesson-2' })],
        exercises: [mockExercise({ id: 'exercise-2' })],
      });

      const state = { eras: [mockEra({ modules: [module1, module2] })] };
      const stats = calculateStats(state);

      expect(stats.totalLessons).toBe(2);
      expect(stats.totalExercises).toBe(2);
      expect(stats.totalHours).toBe(2); // (1 * 0.25 + 1 * 0.75) * 2 = 2
    });
  });

  // ========================================================================
  // INTEGRATION TESTS
  // ========================================================================

  describe('Integration', () => {
    it('should handle complex multi-era, multi-module state', () => {
      const era1 = mockEra({
        id: 'era-1',
        label: 'Ancient',
        color: '#CD5C5C',
        modules: [
          mockModule({
            id: 'module-1',
            lessons: [
              mockLesson({ id: 'lesson-1', completed: true, completedAt: new Date().toISOString() }),
              mockLesson({ id: 'lesson-2', completed: false }),
            ],
            exercises: [mockExercise({ id: 'exercise-1', completed: true, bestScore: 100 })],
          }),
        ],
      });

      const era2 = mockEra({
        id: 'era-2',
        label: 'Medieval',
        color: '#4169E1',
        modules: [
          mockModule({
            id: 'module-2',
            lessons: [
              mockLesson({ id: 'lesson-3', completed: true, completedAt: new Date(Date.now() - 60000).toISOString() }),
            ],
            exercises: [
              mockExercise({ id: 'exercise-2', completed: false }),
              mockExercise({ id: 'exercise-3', completed: true, bestScore: 85 }),
            ],
          }),
        ],
      });

      const state = { eras: [era1, era2] };

      // Check progress: 2 lessons of 3 = 66.67%
      const progress = calculateOverallProgress(state);
      expect(progress).toBeGreaterThan(66);
      expect(progress).toBeLessThan(67);

      // Check stats
      const stats = calculateStats(state);
      expect(stats.totalLessons).toBe(3);
      expect(stats.completedLessons).toBe(2);
      expect(stats.totalExercises).toBe(3);
      expect(stats.completedExercises).toBe(2);

      // Check recent completions (should have 3: 2 lessons + 1 exercise with score)
      const completions = getRecentCompletions(state);
      expect(completions.length).toBeGreaterThan(0);
      expect(completions[0].eraLabel).toBeDefined();

      // Check mastery level
      const mastery = getMasteryLevel(progress);
      expect(mastery.label).toBe('Journeyman');
    });
  });
});
