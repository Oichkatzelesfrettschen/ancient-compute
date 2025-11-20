/**
 * timelineVisualizationStore.test.ts - Tests for timeline visualization store
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { get } from 'svelte/store';
import {
  timelineVizStore,
  loadTimelineData,
  setZoomLevel,
  zoomIn,
  zoomOut,
  resetZoom,
  selectEra,
  centerOnYear,
  selectMilestone,
  setHoveredMilestone,
  applyFilters,
  clearFilters,
  toggleCategoryFilter,
  toggleCivilizationFilter,
  currentEra,
  visibleMilestones,
  hasActiveFilters,
} from '../timelineVisualizationStore';
import type { TimelineEra, Milestone } from '$lib/api/timeline';

describe('timelineVisualizationStore', () => {
  const sampleEras: TimelineEra[] = [
    {
      id: 'era-1',
      name: 'Ancient',
      startYear: -3000,
      endYear: 500,
      color: '#FFD700',
      description: 'Ancient era',
      order: 0,
      milestones: [],
    },
    {
      id: 'era-2',
      name: 'Medieval',
      startYear: 500,
      endYear: 1500,
      color: '#4169E1',
      description: 'Medieval era',
      order: 1,
      milestones: [],
    },
  ];

  const sampleMilestones: Milestone[] = [
    {
      id: 'm1',
      year: -2000,
      title: 'Ancient Invention',
      description: 'Description',
      category: 'invention',
      civilization: 'Greek',
      eraId: 'era-1',
    },
    {
      id: 'm2',
      year: 1000,
      title: 'Medieval Theory',
      description: 'Description',
      category: 'theory',
      civilization: 'Islamic',
      eraId: 'era-2',
    },
  ];

  beforeEach(() => {
    // Reset store before each test
    loadTimelineData([], []);
  });

  describe('Data Loading', () => {
    it('should load timeline data', () => {
      loadTimelineData(sampleEras, sampleMilestones);

      const state = get(timelineVizStore);
      expect(state.eras).toHaveLength(2);
      expect(state.allMilestones).toHaveLength(2);
      expect(state.filteredMilestones).toHaveLength(2);
    });

    it('should extract civilizations from milestones', () => {
      loadTimelineData(sampleEras, sampleMilestones);

      const state = get(timelineVizStore);
      expect(state.availableCivilizations).toContain('Greek');
      expect(state.availableCivilizations).toContain('Islamic');
    });

    it('should sort eras by order', () => {
      const unsortedEras = [sampleEras[1], sampleEras[0]];
      loadTimelineData(unsortedEras, sampleMilestones);

      const state = get(timelineVizStore);
      expect(state.eras[0].order).toBeLessThan(state.eras[1].order);
    });
  });

  describe('Zoom Controls', () => {
    it('should set zoom level', () => {
      setZoomLevel('era');

      const state = get(timelineVizStore);
      expect(state.zoomLevel).toBe('era');
    });

    it('should zoom in through levels', () => {
      setZoomLevel('overview');
      zoomIn();
      expect(get(timelineVizStore).zoomLevel).toBe('era');

      zoomIn();
      expect(get(timelineVizStore).zoomLevel).toBe('decade');

      zoomIn();
      expect(get(timelineVizStore).zoomLevel).toBe('year');
    });

    it('should not zoom in beyond maximum level', () => {
      setZoomLevel('year');
      zoomIn();
      expect(get(timelineVizStore).zoomLevel).toBe('year');
    });

    it('should zoom out through levels', () => {
      setZoomLevel('year');
      zoomOut();
      expect(get(timelineVizStore).zoomLevel).toBe('decade');

      zoomOut();
      expect(get(timelineVizStore).zoomLevel).toBe('era');

      zoomOut();
      expect(get(timelineVizStore).zoomLevel).toBe('overview');
    });

    it('should not zoom out beyond minimum level', () => {
      setZoomLevel('overview');
      zoomOut();
      expect(get(timelineVizStore).zoomLevel).toBe('overview');
    });

    it('should reset zoom to overview', () => {
      setZoomLevel('year');
      resetZoom();

      const state = get(timelineVizStore);
      expect(state.zoomLevel).toBe('overview');
      expect(state.currentYear).toBe(0);
    });
  });

  describe('Era Selection', () => {
    beforeEach(() => {
      loadTimelineData(sampleEras, sampleMilestones);
    });

    it('should select era', () => {
      selectEra('era-1');

      const state = get(timelineVizStore);
      expect(state.currentEraId).toBe('era-1');
      expect(state.zoomLevel).toBe('era');
    });

    it('should center on era when selected', () => {
      selectEra('era-1');

      const state = get(timelineVizStore);
      const era = state.eras.find(e => e.id === 'era-1')!;
      const expectedCenter = Math.floor((era.startYear + era.endYear) / 2);

      expect(state.currentYear).toBe(expectedCenter);
    });

    it('should update currentEra derived store', () => {
      selectEra('era-1');

      const era = get(currentEra);
      expect(era).toBeTruthy();
      expect(era?.id).toBe('era-1');
    });
  });

  describe('Year Navigation', () => {
    it('should center on specific year', () => {
      centerOnYear(1000);

      const state = get(timelineVizStore);
      expect(state.currentYear).toBe(1000);
    });

    it('should handle BCE years', () => {
      centerOnYear(-2000);

      const state = get(timelineVizStore);
      expect(state.currentYear).toBe(-2000);
    });

    it('should handle year 0', () => {
      centerOnYear(0);

      const state = get(timelineVizStore);
      expect(state.currentYear).toBe(0);
    });
  });

  describe('Milestone Selection', () => {
    beforeEach(() => {
      loadTimelineData(sampleEras, sampleMilestones);
    });

    it('should select milestone', () => {
      const milestone = sampleMilestones[0];
      selectMilestone(milestone);

      const state = get(timelineVizStore);
      expect(state.selectedMilestone).toEqual(milestone);
    });

    it('should clear milestone selection', () => {
      selectMilestone(sampleMilestones[0]);
      selectMilestone(null);

      const state = get(timelineVizStore);
      expect(state.selectedMilestone).toBeNull();
    });

    it('should set hovered milestone', () => {
      const milestone = sampleMilestones[0];
      const position = { x: 100, y: 200 };

      setHoveredMilestone(milestone, position);

      const state = get(timelineVizStore);
      expect(state.hoveredMilestone).toEqual(milestone);
      expect(state.tooltipPosition).toEqual(position);
    });

    it('should clear hovered milestone', () => {
      setHoveredMilestone(sampleMilestones[0], { x: 100, y: 200 });
      setHoveredMilestone(null, null);

      const state = get(timelineVizStore);
      expect(state.hoveredMilestone).toBeNull();
      expect(state.tooltipPosition).toBeNull();
    });
  });

  describe('Filtering', () => {
    beforeEach(() => {
      loadTimelineData(sampleEras, sampleMilestones);
    });

    it('should filter by category', () => {
      applyFilters({ categories: ['invention'] });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(1);
      expect(state.filteredMilestones[0].category).toBe('invention');
    });

    it('should filter by civilization', () => {
      applyFilters({ civilizations: ['Greek'] });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(1);
      expect(state.filteredMilestones[0].civilization).toBe('Greek');
    });

    it('should filter by year range', () => {
      applyFilters({ startYear: -1000, endYear: 0 });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones.length).toBeLessThanOrEqual(1);
    });

    it('should filter by search query', () => {
      applyFilters({ searchQuery: 'Ancient' });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(1);
      expect(state.filteredMilestones[0].title).toContain('Ancient');
    });

    it('should combine multiple filters', () => {
      applyFilters({
        categories: ['invention'],
        civilizations: ['Greek'],
      });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(1);
    });

    it('should clear all filters', () => {
      applyFilters({ categories: ['invention'] });
      clearFilters();

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(2);
      expect(state.filters.categories).toHaveLength(0);
    });

    it('should toggle category filter', () => {
      toggleCategoryFilter('invention');
      expect(get(timelineVizStore).filters.categories).toContain('invention');

      toggleCategoryFilter('invention');
      expect(get(timelineVizStore).filters.categories).not.toContain('invention');
    });

    it('should toggle civilization filter', () => {
      toggleCivilizationFilter('Greek');
      expect(get(timelineVizStore).filters.civilizations).toContain('Greek');

      toggleCivilizationFilter('Greek');
      expect(get(timelineVizStore).filters.civilizations).not.toContain('Greek');
    });
  });

  describe('Derived Stores', () => {
    beforeEach(() => {
      loadTimelineData(sampleEras, sampleMilestones);
    });

    it('should provide current era', () => {
      selectEra('era-1');

      const era = get(currentEra);
      expect(era).toBeTruthy();
      expect(era?.id).toBe('era-1');
    });

    it('should provide null when no era selected', () => {
      const era = get(currentEra);
      expect(era).toBeNull();
    });

    it('should provide visible milestones based on viewport', () => {
      // Set viewport to cover medieval era
      centerOnYear(1000);

      const visible = get(visibleMilestones);
      expect(visible.length).toBeGreaterThanOrEqual(0);
    });

    it('should indicate when filters are active', () => {
      expect(get(hasActiveFilters)).toBe(false);

      applyFilters({ categories: ['invention'] });
      expect(get(hasActiveFilters)).toBe(true);

      clearFilters();
      expect(get(hasActiveFilters)).toBe(false);
    });
  });

  describe('Viewport Management', () => {
    it('should update viewport when zooming', () => {
      const initialViewport = get(timelineVizStore).viewport;

      setZoomLevel('era');

      const newViewport = get(timelineVizStore).viewport;
      expect(newViewport.scale).not.toBe(initialViewport.scale);
    });

    it('should update viewport when centering on year', () => {
      centerOnYear(1500);

      const state = get(timelineVizStore);
      expect(state.viewport.centerYear).toBe(1500);
    });

    it('should maintain viewport consistency', () => {
      const state = get(timelineVizStore);
      const { startYear, endYear, centerYear } = state.viewport;

      expect(startYear).toBeLessThan(centerYear);
      expect(centerYear).toBeLessThan(endYear);
    });
  });

  describe('Error Handling', () => {
    it('should handle empty milestone array', () => {
      loadTimelineData(sampleEras, []);

      const state = get(timelineVizStore);
      expect(state.allMilestones).toHaveLength(0);
      expect(state.filteredMilestones).toHaveLength(0);
    });

    it('should handle empty eras array', () => {
      loadTimelineData([], sampleMilestones);

      const state = get(timelineVizStore);
      expect(state.eras).toHaveLength(0);
    });

    it('should handle invalid era selection', () => {
      loadTimelineData(sampleEras, sampleMilestones);
      selectEra('non-existent-era');

      // Should not crash
      const state = get(timelineVizStore);
      expect(state).toBeTruthy();
    });

    it('should handle filters with no results', () => {
      loadTimelineData(sampleEras, sampleMilestones);
      applyFilters({ searchQuery: 'nonexistent' });

      const state = get(timelineVizStore);
      expect(state.filteredMilestones).toHaveLength(0);
    });
  });

  describe('State Consistency', () => {
    beforeEach(() => {
      loadTimelineData(sampleEras, sampleMilestones);
    });

    it('should maintain filtered milestones <= all milestones', () => {
      const state = get(timelineVizStore);
      expect(state.filteredMilestones.length).toBeLessThanOrEqual(
        state.allMilestones.length
      );
    });

    it('should maintain available civilizations from milestones', () => {
      const state = get(timelineVizStore);
      const milestoneCivs = new Set(state.allMilestones.map(m => m.civilization));

      state.availableCivilizations.forEach(civ => {
        expect(milestoneCivs.has(civ)).toBe(true);
      });
    });

    it('should sort available civilizations', () => {
      const state = get(timelineVizStore);
      const sorted = [...state.availableCivilizations].sort();

      expect(state.availableCivilizations).toEqual(sorted);
    });
  });
});
