/**
 * educationStore.ts - Education-specific state management
 *
 * Manages:
 * - Module filtering and sorting
 * - User progress tracking
 * - Current lesson/exercise state
 * - Submission history
 */

import { writable, derived, get } from 'svelte/store';
import type { Module, Lesson, Exercise } from './timelineStore';

// ============================================================================
// TYPE DEFINITIONS
// ============================================================================

export interface ModuleFilters {
	era: string[];
	difficulty: string[];
	status: ('not_started' | 'in_progress' | 'completed')[];
	searchQuery: string;
}

export interface ModuleSortConfig {
	field: 'name' | 'difficulty' | 'progress' | 'date' | 'estimated_time';
	order: 'asc' | 'desc';
}

export interface ModuleProgress {
	moduleId: string;
	lessonsCompleted: number;
	lessonsTotal: number;
	exercisesCompleted: number;
	exercisesTotal: number;
	timeSpent: number; // seconds
	lastAccessedAt: string;
	status: 'not_started' | 'in_progress' | 'completed';
}

export interface ExerciseSubmission {
	id: string;
	exerciseId: string;
	code: string;
	language: string;
	submittedAt: string;
	status: 'pending' | 'running' | 'passed' | 'failed';
	score: number; // percentage
	testResults: TestResult[];
	executionTime: number; // milliseconds
	memoryUsed: number; // MB
}

export interface TestResult {
	testCaseId: string;
	name: string;
	status: 'pending' | 'running' | 'passed' | 'failed';
	input: string;
	expectedOutput: string;
	actualOutput: string;
	executionTime: number;
	error?: string;
}

export interface EducationState {
	// Modules
	modules: Module[];
	filteredModules: Module[];
	filters: ModuleFilters;
	sort: ModuleSortConfig;

	// Current items
	currentModule: Module | null;
	currentLesson: Lesson | null;
	currentExercise: Exercise | null;

	// Progress
	userProgress: Map<string, ModuleProgress>;

	// Submissions
	submissions: Map<string, ExerciseSubmission[]>;
	currentSubmission: ExerciseSubmission | null;

	// UI state
	isLoading: boolean;
	error: string | null;
}

// ============================================================================
// INITIAL STATE
// ============================================================================

const initialState: EducationState = {
	modules: [],
	filteredModules: [],
	filters: {
		era: [],
		difficulty: [],
		status: [],
		searchQuery: '',
	},
	sort: {
		field: 'name',
		order: 'asc',
	},
	currentModule: null,
	currentLesson: null,
	currentExercise: null,
	userProgress: new Map(),
	submissions: new Map(),
	currentSubmission: null,
	isLoading: false,
	error: null,
};

// ============================================================================
// STORE
// ============================================================================

export const educationStore = writable<EducationState>(initialState);

// ============================================================================
// DERIVED STORES
// ============================================================================

/**
 * Get modules with progress information
 */
export const modulesWithProgress = derived(
	educationStore,
	($state) => {
		return $state.modules.map((module) => {
			const progress = $state.userProgress.get(module.id);
			return {
				...module,
				progress: progress || {
					moduleId: module.id,
					lessonsCompleted: 0,
					lessonsTotal: module.lessons.length,
					exercisesCompleted: 0,
					exercisesTotal: module.exercises.length,
					timeSpent: 0,
					lastAccessedAt: new Date().toISOString(),
					status: 'not_started' as const,
				},
			};
		});
	}
);

/**
 * Get filtered and sorted modules
 */
export const filteredSortedModules = derived(
	modulesWithProgress,
	($modules) => {
		const state = get(educationStore);
		let filtered = [...$modules];

		// Apply era filter
		if (state.filters.era.length > 0) {
			filtered = filtered.filter((m) => state.filters.era.includes(m.eraId));
		}

		// Apply difficulty filter (based on average lesson difficulty)
		if (state.filters.difficulty.length > 0) {
			filtered = filtered.filter((m) => {
				// For now, use a simple heuristic
				const avgDifficulty = m.estimatedTime < 2 ? 'beginner' : m.estimatedTime < 5 ? 'intermediate' : 'advanced';
				return state.filters.difficulty.includes(avgDifficulty);
			});
		}

		// Apply status filter
		if (state.filters.status.length > 0) {
			filtered = filtered.filter((m) => state.filters.status.includes(m.progress.status));
		}

		// Apply search query
		if (state.filters.searchQuery) {
			const query = state.filters.searchQuery.toLowerCase();
			filtered = filtered.filter(
				(m) =>
					m.title.toLowerCase().includes(query) ||
					m.description.toLowerCase().includes(query)
			);
		}

		// Apply sorting
		filtered.sort((a, b) => {
			let comparison = 0;

			switch (state.sort.field) {
				case 'name':
					comparison = a.title.localeCompare(b.title);
					break;
				case 'difficulty':
					comparison = a.estimatedTime - b.estimatedTime;
					break;
				case 'progress':
					const progressA = (a.progress.lessonsCompleted / a.progress.lessonsTotal) || 0;
					const progressB = (b.progress.lessonsCompleted / b.progress.lessonsTotal) || 0;
					comparison = progressA - progressB;
					break;
				case 'date':
					comparison = new Date(a.progress.lastAccessedAt).getTime() - new Date(b.progress.lastAccessedAt).getTime();
					break;
				case 'estimated_time':
					comparison = a.estimatedTime - b.estimatedTime;
					break;
			}

			return state.sort.order === 'asc' ? comparison : -comparison;
		});

		return filtered;
	}
);

/**
 * Get current module progress
 */
export const currentModuleProgress = derived(
	educationStore,
	($state) => {
		if (!$state.currentModule) return null;
		return $state.userProgress.get($state.currentModule.id) || null;
	}
);

/**
 * Get submission history for current exercise
 */
export const currentExerciseSubmissions = derived(
	educationStore,
	($state) => {
		if (!$state.currentExercise) return [];
		return $state.submissions.get($state.currentExercise.id) || [];
	}
);

// ============================================================================
// ACTIONS
// ============================================================================

/**
 * Load modules from API
 */
export async function loadModules(): Promise<void> {
	educationStore.update((state) => ({ ...state, isLoading: true, error: null }));

	try {
		// TODO: Replace with actual API call
		// const response = await fetch('/api/v1/modules');
		// const data = await response.json();

		educationStore.update((state) => ({
			...state,
			modules: [], // data.modules
			isLoading: false,
		}));
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			isLoading: false,
			error: error instanceof Error ? error.message : 'Failed to load modules',
		}));
	}
}

/**
 * Load a specific module
 */
export async function loadModule(moduleId: string): Promise<void> {
	educationStore.update((state) => ({ ...state, isLoading: true, error: null }));

	try {
		// TODO: Replace with actual API call
		// const response = await fetch(`/api/v1/modules/${moduleId}`);
		// const module = await response.json();

		educationStore.update((state) => ({
			...state,
			currentModule: null, // module
			isLoading: false,
		}));
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			isLoading: false,
			error: error instanceof Error ? error.message : 'Failed to load module',
		}));
	}
}

/**
 * Load a specific lesson
 */
export async function loadLesson(lessonId: string): Promise<void> {
	educationStore.update((state) => ({ ...state, isLoading: true, error: null }));

	try {
		// TODO: Replace with actual API call
		// const response = await fetch(`/api/v1/lessons/${lessonId}`);
		// const lesson = await response.json();

		educationStore.update((state) => ({
			...state,
			currentLesson: null, // lesson
			isLoading: false,
		}));
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			isLoading: false,
			error: error instanceof Error ? error.message : 'Failed to load lesson',
		}));
	}
}

/**
 * Load a specific exercise
 */
export async function loadExercise(exerciseId: string): Promise<void> {
	educationStore.update((state) => ({ ...state, isLoading: true, error: null }));

	try {
		// TODO: Replace with actual API call
		// const response = await fetch(`/api/v1/exercises/${exerciseId}`);
		// const exercise = await response.json();

		educationStore.update((state) => ({
			...state,
			currentExercise: null, // exercise
			isLoading: false,
		}));
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			isLoading: false,
			error: error instanceof Error ? error.message : 'Failed to load exercise',
		}));
	}
}

/**
 * Submit exercise solution
 */
export async function submitExercise(exerciseId: string, code: string, language: string): Promise<void> {
	const submission: ExerciseSubmission = {
		id: `sub_${Date.now()}`,
		exerciseId,
		code,
		language,
		submittedAt: new Date().toISOString(),
		status: 'pending',
		score: 0,
		testResults: [],
		executionTime: 0,
		memoryUsed: 0,
	};

	educationStore.update((state) => ({
		...state,
		currentSubmission: submission,
	}));

	try {
		// TODO: Replace with actual API call
		// const response = await fetch(`/api/v1/exercises/${exerciseId}/submit`, {
		//   method: 'POST',
		//   headers: { 'Content-Type': 'application/json' },
		//   body: JSON.stringify({ code, language }),
		// });
		// const result = await response.json();

		// Update submission with results
		educationStore.update((state) => {
			const submissions = state.submissions.get(exerciseId) || [];
			return {
				...state,
				submissions: new Map(state.submissions).set(exerciseId, [submission, ...submissions]),
				currentSubmission: submission,
			};
		});
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			error: error instanceof Error ? error.message : 'Failed to submit exercise',
		}));
	}
}

/**
 * Mark lesson as complete
 */
export async function markLessonComplete(lessonId: string): Promise<void> {
	try {
		// TODO: Replace with actual API call
		// await fetch(`/api/v1/lessons/${lessonId}/complete`, { method: 'POST' });

		educationStore.update((state) => {
			if (state.currentLesson && state.currentLesson.id === lessonId) {
				return {
					...state,
					currentLesson: {
						...state.currentLesson,
						completed: true,
						completedAt: new Date().toISOString(),
					},
				};
			}
			return state;
		});
	} catch (error) {
		educationStore.update((state) => ({
			...state,
			error: error instanceof Error ? error.message : 'Failed to mark lesson complete',
		}));
	}
}

/**
 * Update progress for a module
 */
export function updateModuleProgress(moduleId: string, progress: Partial<ModuleProgress>): void {
	educationStore.update((state) => {
		const currentProgress = state.userProgress.get(moduleId) || {
			moduleId,
			lessonsCompleted: 0,
			lessonsTotal: 0,
			exercisesCompleted: 0,
			exercisesTotal: 0,
			timeSpent: 0,
			lastAccessedAt: new Date().toISOString(),
			status: 'not_started' as const,
		};

		const updatedProgress = { ...currentProgress, ...progress };
		const newProgressMap = new Map(state.userProgress);
		newProgressMap.set(moduleId, updatedProgress);

		return {
			...state,
			userProgress: newProgressMap,
		};
	});
}

/**
 * Apply filters
 */
export function applyFilters(filters: Partial<ModuleFilters>): void {
	educationStore.update((state) => ({
		...state,
		filters: { ...state.filters, ...filters },
	}));
}

/**
 * Set sort configuration
 */
export function setSort(field: ModuleSortConfig['field'], order?: ModuleSortConfig['order']): void {
	educationStore.update((state) => {
		const currentOrder = state.sort.field === field && state.sort.order === 'asc' ? 'desc' : 'asc';
		return {
			...state,
			sort: {
				field,
				order: order || currentOrder,
			},
		};
	});
}

/**
 * Clear filters
 */
export function clearFilters(): void {
	educationStore.update((state) => ({
		...state,
		filters: {
			era: [],
			difficulty: [],
			status: [],
			searchQuery: '',
		},
	}));
}

/**
 * Reset store to initial state
 */
export function resetEducationStore(): void {
	educationStore.set(initialState);
}
