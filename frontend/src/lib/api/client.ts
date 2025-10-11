// Ancient Compute - API Client

/**
 * API client for communicating with the Ancient Compute backend.
 * Provides type-safe methods for all backend endpoints.
 */

// Types
export interface Module {
	id: string;
	slug: string;
	title: string;
	description: string;
	era?: string;
	start_year?: number;
	end_year?: number;
	sequence_order?: number;
	estimated_hours?: number;
	difficulty_level?: number;
}

export interface TimelineEvent {
	year: number;
	title: string;
	description: string;
	category?: string;
	civilization?: string;
}

export interface ApiError {
	message: string;
	status: number;
}

// Configuration
const API_BASE_URL = import.meta.env.VITE_API_URL || '/api/v1';

/**
 * Generic fetch wrapper with error handling
 */
async function apiFetch<T>(
	endpoint: string,
	options?: RequestInit
): Promise<T> {
	const url = `${API_BASE_URL}${endpoint}`;

	try {
		const response = await fetch(url, {
			...options,
			headers: {
				'Content-Type': 'application/json',
				...options?.headers
			}
		});

		if (!response.ok) {
			const error: ApiError = {
				message: `HTTP ${response.status}: ${response.statusText}`,
				status: response.status
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
			status: 0
		} as ApiError;
	}
}

/**
 * Module API methods
 */
export const moduleApi = {
	/**
	 * List all available modules
	 */
	async list(): Promise<{ modules: Module[] }> {
		return apiFetch<{ modules: Module[] }>('/modules');
	},

	/**
	 * Get a single module by slug
	 */
	async get(slug: string): Promise<Module> {
		return apiFetch<Module>(`/modules/${slug}`);
	}
};

/**
 * Timeline API methods
 */
export const timelineApi = {
	/**
	 * Get timeline events
	 */
	async list(): Promise<{ timeline: TimelineEvent[] }> {
		return apiFetch<{ timeline: TimelineEvent[] }>('/timeline');
	},

	/**
	 * Get timeline events within a year range
	 */
	async getRange(startYear: number, endYear: number): Promise<{ timeline: TimelineEvent[] }> {
		return apiFetch<{ timeline: TimelineEvent[] }>(
			`/timeline?start=${startYear}&end=${endYear}`
		);
	}
};

/**
 * Health check methods
 */
export const healthApi = {
	/**
	 * Check backend health status
	 */
	async check(): Promise<{ status: string; service: string }> {
		return apiFetch<{ status: string; service: string }>('/health');
	}
};
