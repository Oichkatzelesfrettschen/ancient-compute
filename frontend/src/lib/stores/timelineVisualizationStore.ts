/**
 * Timeline Visualization Store
 *
 * Manages state for the interactive D3.js timeline visualization including:
 * - Zoom levels (overview, era, decade, year)
 * - Current view and navigation
 * - Milestone selection and tooltips
 * - Filters (categories, civilizations)
 */

import { writable, derived, get } from 'svelte/store';
import type { TimelineEra, Milestone, TimelineFilter } from '$lib/api/timeline';

// ============================================================================
// TYPE DEFINITIONS
// ============================================================================

export type ZoomLevel = 'overview' | 'era' | 'decade' | 'year';

export interface TimelineViewport {
  startYear: number;
  endYear: number;
  centerYear: number;
  scale: number; // Pixels per year
}

export interface TimelineVisualizationState {
  // Data
  eras: TimelineEra[];
  allMilestones: Milestone[];
  filteredMilestones: Milestone[];

  // Current view
  currentEraId: string | null;
  currentYear: number;
  zoomLevel: ZoomLevel;
  viewport: TimelineViewport;

  // Interaction
  selectedMilestone: Milestone | null;
  hoveredMilestone: Milestone | null;
  tooltipPosition: { x: number; y: number } | null;

  // Filters
  filters: TimelineFilter;
  availableCivilizations: string[];
  availableCategories: string[];

  // UI state
  isLoading: boolean;
  error: string | null;
  isPanning: boolean;
}

// ============================================================================
// CONSTANTS
// ============================================================================

const ZOOM_SCALES: Record<ZoomLevel, number> = {
  overview: 0.01,  // 0.01px per year (100 years per pixel)
  era: 0.1,        // 0.1px per year (10 years per pixel)
  decade: 1.0,     // 1px per year
  year: 12.0,      // 12px per year
};

const DEFAULT_VIEWPORT: TimelineViewport = {
  startYear: -20000,
  endYear: 2025,
  centerYear: 0,
  scale: ZOOM_SCALES.overview,
};

// ============================================================================
// INITIAL STATE
// ============================================================================

const initialState: TimelineVisualizationState = {
  eras: [],
  allMilestones: [],
  filteredMilestones: [],

  currentEraId: null,
  currentYear: 0,
  zoomLevel: 'overview',
  viewport: DEFAULT_VIEWPORT,

  selectedMilestone: null,
  hoveredMilestone: null,
  tooltipPosition: null,

  filters: {
    categories: [],
    civilizations: [],
  },
  availableCivilizations: [],
  availableCategories: ['invention', 'theory', 'person', 'event'],

  isLoading: false,
  error: null,
  isPanning: false,
};

// ============================================================================
// STORE CREATION
// ============================================================================

export const timelineVizStore = writable<TimelineVisualizationState>(initialState);

// ============================================================================
// DERIVED STORES
// ============================================================================

/**
 * Get currently selected era object
 */
export const currentEra = derived(
  timelineVizStore,
  ($state) =>
    $state.currentEraId
      ? $state.eras.find((e) => e.id === $state.currentEraId)
      : null
);

/**
 * Get milestones visible in current viewport
 */
export const visibleMilestones = derived(
  timelineVizStore,
  ($state) => {
    const { filteredMilestones, viewport } = $state;
    return filteredMilestones.filter(
      (m) => m.year >= viewport.startYear && m.year <= viewport.endYear
    );
  }
);

/**
 * Get eras visible in current viewport
 */
export const visibleEras = derived(
  timelineVizStore,
  ($state) => {
    const { eras, viewport } = $state;
    return eras.filter(
      (era) =>
        era.endYear >= viewport.startYear && era.startYear <= viewport.endYear
    );
  }
);

/**
 * Get zoom level multiplier (for display)
 */
export const zoomMultiplier = derived(
  timelineVizStore,
  ($state) => {
    const baseScale = ZOOM_SCALES.overview;
    return Math.round($state.viewport.scale / baseScale);
  }
);

/**
 * Check if filters are active
 */
export const hasActiveFilters = derived(
  timelineVizStore,
  ($state) =>
    $state.filters.categories && $state.filters.categories.length > 0 ||
    $state.filters.civilizations && $state.filters.civilizations.length > 0 ||
    $state.filters.searchQuery && $state.filters.searchQuery.length > 0
);

// ============================================================================
// STORE ACTIONS
// ============================================================================

/**
 * Load timeline data (eras and milestones)
 */
export function loadTimelineData(
  eras: TimelineEra[],
  milestones: Milestone[]
): void {
  timelineVizStore.update((state) => {
    const civilizations = Array.from(
      new Set(milestones.map((m) => m.civilization))
    ).sort();

    return {
      ...state,
      eras: eras.sort((a, b) => a.order - b.order),
      allMilestones: milestones,
      filteredMilestones: milestones,
      availableCivilizations: civilizations,
      isLoading: false,
      error: null,
    };
  });
}

/**
 * Set loading state
 */
export function setLoading(isLoading: boolean): void {
  timelineVizStore.update((state) => ({
    ...state,
    isLoading,
  }));
}

/**
 * Set error message
 */
export function setError(error: string | null): void {
  timelineVizStore.update((state) => ({
    ...state,
    error,
    isLoading: false,
  }));
}

/**
 * Set zoom level and adjust viewport accordingly
 */
export function setZoomLevel(level: ZoomLevel): void {
  timelineVizStore.update((state) => {
    const newScale = ZOOM_SCALES[level];
    const centerYear = state.viewport.centerYear;

    return {
      ...state,
      zoomLevel: level,
      viewport: {
        ...state.viewport,
        scale: newScale,
        ...calculateViewportFromCenter(centerYear, newScale),
      },
    };
  });
}

/**
 * Zoom in one level
 */
export function zoomIn(): void {
  const state = get(timelineVizStore);
  const levels: ZoomLevel[] = ['overview', 'era', 'decade', 'year'];
  const currentIndex = levels.indexOf(state.zoomLevel);
  if (currentIndex < levels.length - 1) {
    setZoomLevel(levels[currentIndex + 1]);
  }
}

/**
 * Zoom out one level
 */
export function zoomOut(): void {
  const state = get(timelineVizStore);
  const levels: ZoomLevel[] = ['overview', 'era', 'decade', 'year'];
  const currentIndex = levels.indexOf(state.zoomLevel);
  if (currentIndex > 0) {
    setZoomLevel(levels[currentIndex - 1]);
  }
}

/**
 * Reset zoom to overview
 */
export function resetZoom(): void {
  setZoomLevel('overview');
  centerOnYear(0);
}

/**
 * Center viewport on a specific year
 */
export function centerOnYear(year: number): void {
  timelineVizStore.update((state) => ({
    ...state,
    currentYear: year,
    viewport: {
      ...state.viewport,
      centerYear: year,
      ...calculateViewportFromCenter(year, state.viewport.scale),
    },
  }));
}

/**
 * Select and zoom to a specific era
 */
export function selectEra(eraId: string): void {
  timelineVizStore.update((state) => {
    const era = state.eras.find((e) => e.id === eraId);
    if (!era) return state;

    const centerYear = Math.floor((era.startYear + era.endYear) / 2);
    const scale = ZOOM_SCALES.era;

    return {
      ...state,
      currentEraId: eraId,
      currentYear: centerYear,
      zoomLevel: 'era',
      viewport: {
        ...state.viewport,
        centerYear,
        scale,
        ...calculateViewportFromCenter(centerYear, scale),
      },
    };
  });
}

/**
 * Pan the viewport by a delta in years
 */
export function panByYears(deltaYears: number): void {
  timelineVizStore.update((state) => {
    const newCenter = state.viewport.centerYear + deltaYears;
    return {
      ...state,
      viewport: {
        ...state.viewport,
        centerYear: newCenter,
        ...calculateViewportFromCenter(newCenter, state.viewport.scale),
      },
    };
  });
}

/**
 * Set panning state (for UI feedback)
 */
export function setPanning(isPanning: boolean): void {
  timelineVizStore.update((state) => ({
    ...state,
    isPanning,
  }));
}

/**
 * Select a milestone for detailed view
 */
export function selectMilestone(milestone: Milestone | null): void {
  timelineVizStore.update((state) => ({
    ...state,
    selectedMilestone: milestone,
  }));
}

/**
 * Set hovered milestone and tooltip position
 */
export function setHoveredMilestone(
  milestone: Milestone | null,
  position: { x: number; y: number } | null
): void {
  timelineVizStore.update((state) => ({
    ...state,
    hoveredMilestone: milestone,
    tooltipPosition: position,
  }));
}

/**
 * Apply filters to milestones
 */
export function applyFilters(filters: TimelineFilter): void {
  timelineVizStore.update((state) => {
    const filtered = filterMilestones(state.allMilestones, filters);
    return {
      ...state,
      filters,
      filteredMilestones: filtered,
    };
  });
}

/**
 * Clear all filters
 */
export function clearFilters(): void {
  timelineVizStore.update((state) => ({
    ...state,
    filters: {
      categories: [],
      civilizations: [],
    },
    filteredMilestones: state.allMilestones,
  }));
}

/**
 * Toggle a category filter
 */
export function toggleCategoryFilter(category: string): void {
  const state = get(timelineVizStore);
  const categories = state.filters.categories || [];
  const newCategories = categories.includes(category)
    ? categories.filter((c) => c !== category)
    : [...categories, category];

  applyFilters({
    ...state.filters,
    categories: newCategories,
  });
}

/**
 * Toggle a civilization filter
 */
export function toggleCivilizationFilter(civilization: string): void {
  const state = get(timelineVizStore);
  const civilizations = state.filters.civilizations || [];
  const newCivilizations = civilizations.includes(civilization)
    ? civilizations.filter((c) => c !== civilization)
    : [...civilizations, civilization];

  applyFilters({
    ...state.filters,
    civilizations: newCivilizations,
  });
}

// ============================================================================
// HELPER FUNCTIONS
// ============================================================================

/**
 * Calculate viewport bounds from center and scale
 */
function calculateViewportFromCenter(
  centerYear: number,
  scale: number
): { startYear: number; endYear: number } {
  // Assume viewport width of 1200px
  const viewportWidth = 1200;
  const halfWidth = viewportWidth / 2;
  const yearsPerPixel = 1 / scale;

  return {
    startYear: Math.floor(centerYear - halfWidth * yearsPerPixel),
    endYear: Math.floor(centerYear + halfWidth * yearsPerPixel),
  };
}

/**
 * Filter milestones based on criteria
 */
function filterMilestones(
  milestones: Milestone[],
  filters: TimelineFilter
): Milestone[] {
  return milestones.filter((milestone) => {
    // Category filter
    if (filters.categories && filters.categories.length > 0) {
      if (!filters.categories.includes(milestone.category)) {
        return false;
      }
    }

    // Civilization filter
    if (filters.civilizations && filters.civilizations.length > 0) {
      if (!filters.civilizations.includes(milestone.civilization)) {
        return false;
      }
    }

    // Year range filter
    if (filters.startYear !== undefined && milestone.year < filters.startYear) {
      return false;
    }
    if (filters.endYear !== undefined && milestone.year > filters.endYear) {
      return false;
    }

    // Search query filter
    if (filters.searchQuery) {
      const query = filters.searchQuery.toLowerCase();
      const searchableText = `${milestone.title} ${milestone.description} ${milestone.civilization}`.toLowerCase();
      if (!searchableText.includes(query)) {
        return false;
      }
    }

    return true;
  });
}
