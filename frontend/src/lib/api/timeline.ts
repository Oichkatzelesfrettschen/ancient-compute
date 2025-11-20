/**
 * Timeline API Client
 *
 * Provides type-safe API methods for fetching timeline data including:
 * - Historical eras (8 eras from 20,000 BC to 2025 AD)
 * - Milestones (key inventions, theories, people, events)
 * - Era details with modules and lessons
 */

// ============================================================================
// TYPE DEFINITIONS
// ============================================================================

export interface Milestone {
  id: string;
  year: number; // Negative for BCE
  title: string;
  description: string;
  category: 'invention' | 'theory' | 'person' | 'event';
  civilization: string;
  eraId?: string;
  imageUrl?: string;
  relatedModuleIds?: string[];
}

export interface TimelineEra {
  id: string;
  name: string;
  fullName?: string;
  startYear: number; // Negative for BCE
  endYear: number;
  color: string;
  description: string;
  historicalContext?: string;
  icon?: string;
  order: number;
  milestones: Milestone[];
  moduleCount?: number;
  lessonCount?: number;
}

export interface EraDetail extends TimelineEra {
  modules: Array<{
    id: string;
    title: string;
    description: string;
    lessonCount: number;
    estimatedHours: number;
  }>;
}

export interface TimelineFilter {
  categories?: string[];
  civilizations?: string[];
  startYear?: number;
  endYear?: number;
  searchQuery?: string;
}

export interface FullTimeline {
  eras: TimelineEra[];
  totalMilestones: number;
  civilizations: string[];
  categories: string[];
}

// ============================================================================
// API CONFIGURATION
// ============================================================================

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
        ...options?.headers,
      },
    });

    if (!response.ok) {
      const errorData = await response.json().catch(() => ({}));
      throw new Error(
        errorData.message || `HTTP ${response.status}: ${response.statusText}`
      );
    }

    return await response.json();
  } catch (error) {
    if (error instanceof Error) {
      throw error;
    }
    throw new Error('Network error occurred');
  }
}

// ============================================================================
// TIMELINE API METHODS
// ============================================================================

/**
 * Fetch complete timeline with all eras and milestones
 */
export async function fetchTimeline(): Promise<FullTimeline> {
  return apiFetch<FullTimeline>('/timeline/full');
}

/**
 * Fetch all eras (without milestones)
 */
export async function fetchEras(): Promise<TimelineEra[]> {
  const response = await apiFetch<{ eras: TimelineEra[] }>('/timeline/eras');
  return response.eras;
}

/**
 * Fetch detailed information for a specific era
 */
export async function fetchEraDetail(eraId: string): Promise<EraDetail> {
  return apiFetch<EraDetail>(`/timeline/eras/${eraId}`);
}

/**
 * Fetch milestones with optional filtering
 */
export async function fetchMilestones(
  filter?: TimelineFilter
): Promise<Milestone[]> {
  const params = new URLSearchParams();

  if (filter?.categories?.length) {
    params.append('categories', filter.categories.join(','));
  }
  if (filter?.civilizations?.length) {
    params.append('civilizations', filter.civilizations.join(','));
  }
  if (filter?.startYear !== undefined) {
    params.append('start_year', filter.startYear.toString());
  }
  if (filter?.endYear !== undefined) {
    params.append('end_year', filter.endYear.toString());
  }
  if (filter?.searchQuery) {
    params.append('q', filter.searchQuery);
  }

  const queryString = params.toString();
  const endpoint = queryString
    ? `/timeline/milestones?${queryString}`
    : '/timeline/milestones';

  const response = await apiFetch<{ milestones: Milestone[] }>(endpoint);
  return response.milestones;
}

/**
 * Fetch milestones for a specific era
 */
export async function fetchEraMilestones(eraId: string): Promise<Milestone[]> {
  const response = await apiFetch<{ milestones: Milestone[] }>(
    `/timeline/eras/${eraId}/milestones`
  );
  return response.milestones;
}

/**
 * Search timeline for events matching query
 */
export async function searchTimeline(query: string): Promise<{
  milestones: Milestone[];
  eras: TimelineEra[];
}> {
  return apiFetch<{ milestones: Milestone[]; eras: TimelineEra[] }>(
    `/timeline/search?q=${encodeURIComponent(query)}`
  );
}

/**
 * Get list of all civilizations in the timeline
 */
export async function fetchCivilizations(): Promise<string[]> {
  const response = await apiFetch<{ civilizations: string[] }>(
    '/timeline/civilizations'
  );
  return response.civilizations;
}

/**
 * Get timeline statistics
 */
export async function fetchTimelineStats(): Promise<{
  totalEras: number;
  totalMilestones: number;
  totalYearsSpanned: number;
  civilizationCount: number;
}> {
  return apiFetch<{
    totalEras: number;
    totalMilestones: number;
    totalYearsSpanned: number;
    civilizationCount: number;
  }>('/timeline/stats');
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Format a year for display (handles BCE/CE)
 */
export function formatYear(year: number): string {
  if (year < 0) {
    return `${Math.abs(year).toLocaleString()} BCE`;
  } else if (year === 0) {
    return '0';
  } else {
    return `${year.toLocaleString()} CE`;
  }
}

/**
 * Format a year range for display
 */
export function formatYearRange(startYear: number, endYear: number): string {
  return `${formatYear(startYear)} - ${formatYear(endYear)}`;
}

/**
 * Calculate the duration of an era in years
 */
export function getEraDuration(era: TimelineEra): number {
  return era.endYear - era.startYear;
}

/**
 * Check if a year falls within an era
 */
export function isYearInEra(year: number, era: TimelineEra): boolean {
  return year >= era.startYear && year <= era.endYear;
}

/**
 * Find the era that contains a given year
 */
export function findEraByYear(year: number, eras: TimelineEra[]): TimelineEra | null {
  return eras.find(era => isYearInEra(year, era)) || null;
}
