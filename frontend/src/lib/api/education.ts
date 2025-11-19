/**
 * education.ts - Education API client
 *
 * Provides methods for:
 * - Module browsing and retrieval
 * - Lesson content loading
 * - Exercise submission and validation
 * - Progress tracking
 */

import type { Module, Lesson, Exercise } from '../stores/timelineStore';
import type { ModuleProgress, ExerciseSubmission, TestResult } from '../stores/educationStore';

// Configuration
const API_BASE_URL = import.meta.env.VITE_API_URL || '/api/v1';

/**
 * API error type
 */
export interface ApiError {
	message: string;
	status: number;
	details?: unknown;
}

/**
 * Generic fetch wrapper with error handling
 */
async function apiFetch<T>(endpoint: string, options?: RequestInit): Promise<T> {
	const url = `${API_BASE_URL}${endpoint}`;

	try {
		const response = await fetch(url, {
			...options,
			headers: {
				'Content-Type': 'application/json',
				...options?.headers,
			},
		});

		if (!response.ok) {
			const errorData = await response.json().catch(() => ({}));
			const error: ApiError = {
				message: errorData.message || `HTTP ${response.status}: ${response.statusText}`,
				status: response.status,
				details: errorData,
			};
			throw error;
		}

		return await response.json();
	} catch (error) {
		if (error && typeof error === 'object' && 'status' in error) {
			throw error;
		}
		throw {
			message: error instanceof Error ? error.message : 'Network error',
			status: 0,
		} as ApiError;
	}
}

// ============================================================================
// MODULE API
// ============================================================================

export interface ModuleListResponse {
	modules: Module[];
	total: number;
}

export interface ModuleListParams {
	era?: string;
	difficulty?: string;
	status?: string;
	search?: string;
	offset?: number;
	limit?: number;
}

export const moduleApi = {
	/**
	 * List all modules with optional filtering
	 */
	async list(params?: ModuleListParams): Promise<ModuleListResponse> {
		const queryParams = new URLSearchParams();
		if (params?.era) queryParams.append('era', params.era);
		if (params?.difficulty) queryParams.append('difficulty', params.difficulty);
		if (params?.status) queryParams.append('status', params.status);
		if (params?.search) queryParams.append('search', params.search);
		if (params?.offset !== undefined) queryParams.append('offset', params.offset.toString());
		if (params?.limit !== undefined) queryParams.append('limit', params.limit.toString());

		const query = queryParams.toString();
		return apiFetch<ModuleListResponse>(`/modules${query ? `?${query}` : ''}`);
	},

	/**
	 * Get a single module by ID
	 */
	async get(moduleId: string): Promise<Module> {
		return apiFetch<Module>(`/modules/${moduleId}`);
	},

	/**
	 * Get a module by slug
	 */
	async getBySlug(slug: string): Promise<Module> {
		return apiFetch<Module>(`/modules/slug/${slug}`);
	},
};

// ============================================================================
// LESSON API
// ============================================================================

export interface LessonResponse extends Lesson {
	prevLessonId?: string;
	nextLessonId?: string;
}

export const lessonApi = {
	/**
	 * Get a lesson by ID
	 */
	async get(lessonId: string): Promise<LessonResponse> {
		return apiFetch<LessonResponse>(`/lessons/${lessonId}`);
	},

	/**
	 * Mark a lesson as complete
	 */
	async markComplete(lessonId: string): Promise<{ success: boolean }> {
		return apiFetch<{ success: boolean }>(`/lessons/${lessonId}/complete`, {
			method: 'POST',
		});
	},

	/**
	 * Get lessons for a module
	 */
	async listByModule(moduleId: string): Promise<{ lessons: Lesson[] }> {
		return apiFetch<{ lessons: Lesson[] }>(`/modules/${moduleId}/lessons`);
	},
};

// ============================================================================
// EXERCISE API
// ============================================================================

export interface ExerciseResponse extends Exercise {
	starterCode?: Record<string, string>; // language -> code
}

export interface SubmitExerciseRequest {
	code: string;
	language: string;
}

export interface SubmitExerciseResponse {
	submission: ExerciseSubmission;
}

export interface SubmissionListResponse {
	submissions: ExerciseSubmission[];
	total: number;
}

export const exerciseApi = {
	/**
	 * Get an exercise by ID
	 */
	async get(exerciseId: string): Promise<ExerciseResponse> {
		return apiFetch<ExerciseResponse>(`/exercises/${exerciseId}`);
	},

	/**
	 * Submit an exercise solution
	 */
	async submit(exerciseId: string, request: SubmitExerciseRequest): Promise<SubmitExerciseResponse> {
		return apiFetch<SubmitExerciseResponse>(`/exercises/${exerciseId}/submit`, {
			method: 'POST',
			body: JSON.stringify(request),
		});
	},

	/**
	 * Get submission history for an exercise
	 */
	async listSubmissions(exerciseId: string, limit = 10): Promise<SubmissionListResponse> {
		return apiFetch<SubmissionListResponse>(
			`/exercises/${exerciseId}/submissions?limit=${limit}`
		);
	},

	/**
	 * Get a specific submission
	 */
	async getSubmission(submissionId: string): Promise<ExerciseSubmission> {
		return apiFetch<ExerciseSubmission>(`/submissions/${submissionId}`);
	},

	/**
	 * Get exercises for a module
	 */
	async listByModule(moduleId: string): Promise<{ exercises: Exercise[] }> {
		return apiFetch<{ exercises: Exercise[] }>(`/modules/${moduleId}/exercises`);
	},
};

// ============================================================================
// PROGRESS API
// ============================================================================

export interface UserProgressResponse {
	progress: ModuleProgress[];
}

export interface UpdateProgressRequest {
	lessonId?: string;
	exerciseId?: string;
	timeSpent?: number;
}

export const progressApi = {
	/**
	 * Get user progress for all modules
	 */
	async getAll(): Promise<UserProgressResponse> {
		return apiFetch<UserProgressResponse>('/users/me/progress');
	},

	/**
	 * Get user progress for a specific module
	 */
	async getModule(moduleId: string): Promise<ModuleProgress> {
		return apiFetch<ModuleProgress>(`/users/me/progress/${moduleId}`);
	},

	/**
	 * Update user progress
	 */
	async update(moduleId: string, request: UpdateProgressRequest): Promise<ModuleProgress> {
		return apiFetch<ModuleProgress>(`/users/me/progress/${moduleId}`, {
			method: 'POST',
			body: JSON.stringify(request),
		});
	},
};

// ============================================================================
// CODE EXECUTION API
// ============================================================================

export interface ExecuteCodeRequest {
	code: string;
	language: string;
	input?: string;
}

export interface ExecuteCodeResponse {
	stdout: string;
	stderr: string;
	exit_code: number;
	execution_time: number;
	compilation_time?: number;
	ir_text?: string;
	assembly_text?: string;
}

export const executionApi = {
	/**
	 * Execute code in a supported language
	 */
	async execute(request: ExecuteCodeRequest): Promise<ExecuteCodeResponse> {
		return apiFetch<ExecuteCodeResponse>('/execute/run', {
			method: 'POST',
			body: JSON.stringify(request),
		});
	},

	/**
	 * Get list of supported languages
	 */
	async getSupportedLanguages(): Promise<{ languages: string[] }> {
		return apiFetch<{ languages: string[] }>('/execute/languages');
	},
};

// ============================================================================
// ERA API
// ============================================================================

export interface Era {
	id: string;
	label: string;
	fullName: string;
	description: string;
	startYear: number;
	endYear: number;
	color: string;
	order: number;
}

export const eraApi = {
	/**
	 * List all eras
	 */
	async list(): Promise<{ eras: Era[] }> {
		return apiFetch<{ eras: Era[] }>('/timeline/eras');
	},

	/**
	 * Get a single era
	 */
	async get(eraId: string): Promise<Era> {
		return apiFetch<Era>(`/timeline/eras/${eraId}`);
	},
};

// ============================================================================
// EXPORT ALL APIs
// ============================================================================

export default {
	modules: moduleApi,
	lessons: lessonApi,
	exercises: exerciseApi,
	progress: progressApi,
	execution: executionApi,
	eras: eraApi,
};
