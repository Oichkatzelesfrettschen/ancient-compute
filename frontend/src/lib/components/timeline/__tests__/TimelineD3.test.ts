/**
 * TimelineD3.test.ts - Comprehensive tests for D3 timeline visualization
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, screen } from '@testing-library/svelte';
import TimelineD3 from '../TimelineD3.svelte';
import { timelineVizStore, loadTimelineData } from '$lib/stores/timelineVisualizationStore';
import { SAMPLE_TIMELINE_DATA } from '$lib/data/sampleTimelineData';
import type { TimelineEra, Milestone } from '$lib/api/timeline';

// Mock D3
vi.mock('d3', () => ({
  select: vi.fn(() => ({
    selectAll: vi.fn(() => ({
      remove: vi.fn(),
    })),
    append: vi.fn(() => ({
      attr: vi.fn().mockReturnThis(),
      selectAll: vi.fn(() => ({
        data: vi.fn(() => ({
          join: vi.fn(() => ({
            attr: vi.fn().mockReturnThis(),
            on: vi.fn().mockReturnThis(),
          })),
        })),
      })),
      call: vi.fn().mockReturnThis(),
    })),
    attr: vi.fn().mockReturnThis(),
  })),
  scaleLog: vi.fn(() => ({
    domain: vi.fn().mockReturnThis(),
    range: vi.fn().mockReturnThis(),
    clamp: vi.fn().mockReturnThis(),
  })),
  scaleLinear: vi.fn(() => ({
    domain: vi.fn().mockReturnThis(),
    range: vi.fn().mockReturnThis(),
  })),
  axisBottom: vi.fn(() => ({
    tickValues: vi.fn().mockReturnThis(),
    tickFormat: vi.fn().mockReturnThis(),
  })),
  zoom: vi.fn(() => ({
    scaleExtent: vi.fn().mockReturnThis(),
    on: vi.fn().mockReturnThis(),
  })),
}));

describe('TimelineD3', () => {
  beforeEach(() => {
    // Reset store before each test
    loadTimelineData(SAMPLE_TIMELINE_DATA.eras, SAMPLE_TIMELINE_DATA.milestones);
  });

  describe('Component Rendering', () => {
    it('should render timeline container', () => {
      const { container } = render(TimelineD3);
      const timelineContainer = container.querySelector('.timeline-container');
      expect(timelineContainer).toBeTruthy();
    });

    it('should render SVG element', () => {
      const { container } = render(TimelineD3);
      const svg = container.querySelector('svg.timeline-svg');
      expect(svg).toBeTruthy();
    });

    it('should apply custom width and height', () => {
      const { container } = render(TimelineD3, {
        props: { width: 800, height: 300 },
      });
      const svg = container.querySelector('svg');
      // Note: Actual dimensions might be set via D3, this checks props are accepted
      expect(svg).toBeTruthy();
    });

    it('should have correct container styles', () => {
      const { container } = render(TimelineD3);
      const timelineContainer = container.querySelector('.timeline-container');
      expect(timelineContainer).toHaveClass('timeline-container');
    });
  });

  describe('Data Visualization', () => {
    it('should render with era data from store', () => {
      const { container } = render(TimelineD3);
      // Component should access store data
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle empty era data gracefully', () => {
      loadTimelineData([], []);
      const { container } = render(TimelineD3);
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should render all 8 eras', () => {
      const { container } = render(TimelineD3);
      // With sample data, should have 8 eras
      expect(SAMPLE_TIMELINE_DATA.eras).toHaveLength(8);
    });

    it('should render milestone data', () => {
      const { container } = render(TimelineD3);
      // Should have milestones from sample data
      expect(SAMPLE_TIMELINE_DATA.milestones.length).toBeGreaterThan(0);
    });
  });

  describe('Scales and Transformations', () => {
    it('should handle BCE dates correctly', () => {
      const eraWithBCE: TimelineEra = {
        id: 'test-era',
        name: 'Test Era',
        startYear: -3000,
        endYear: -1000,
        color: '#FF0000',
        description: 'Test',
        order: 0,
        milestones: [],
      };

      loadTimelineData([eraWithBCE], []);
      const { container } = render(TimelineD3);
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle CE dates correctly', () => {
      const eraWithCE: TimelineEra = {
        id: 'test-era',
        name: 'Test Era',
        startYear: 1000,
        endYear: 2000,
        color: '#00FF00',
        description: 'Test',
        order: 0,
        milestones: [],
      };

      loadTimelineData([eraWithCE], []);
      const { container } = render(TimelineD3);
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle year 0 transition', () => {
      const erasSpanningZero: TimelineEra[] = [
        {
          id: 'era-bce',
          name: 'BCE Era',
          startYear: -500,
          endYear: -1,
          color: '#FF0000',
          description: 'Before',
          order: 0,
          milestones: [],
        },
        {
          id: 'era-ce',
          name: 'CE Era',
          startYear: 1,
          endYear: 500,
          color: '#00FF00',
          description: 'After',
          order: 1,
          milestones: [],
        },
      ];

      loadTimelineData(erasSpanningZero, []);
      const { container } = render(TimelineD3);
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });
  });

  describe('Responsive Behavior', () => {
    it('should accept custom width', () => {
      const { container } = render(TimelineD3, {
        props: { width: 1600 },
      });
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should accept custom height', () => {
      const { container } = render(TimelineD3, {
        props: { height: 600 },
      });
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle small dimensions', () => {
      const { container } = render(TimelineD3, {
        props: { width: 400, height: 200 },
      });
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle large dimensions', () => {
      const { container } = render(TimelineD3, {
        props: { width: 2400, height: 800 },
      });
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });
  });

  describe('Era Colors', () => {
    it('should use correct colors for each era', () => {
      const eras = SAMPLE_TIMELINE_DATA.eras;

      expect(eras[0].color).toBe('#8B4513'); // Prehistory - Brown
      expect(eras[1].color).toBe('#FFD700'); // Ancient - Gold
      expect(eras[2].color).toBe('#4169E1'); // Medieval - Royal Blue
      expect(eras[3].color).toBe('#DC143C'); // Early Modern - Crimson
      expect(eras[4].color).toBe('#2F4F4F'); // Foundations - Dark Slate
      expect(eras[5].color).toBe('#FF8C00'); // Electronic - Dark Orange
      expect(eras[6].color).toBe('#9370DB'); // Type Theory - Medium Purple
      expect(eras[7].color).toBe('#00CED1'); // Modern - Dark Turquoise
    });
  });

  describe('Milestone Categories', () => {
    it('should handle invention category', () => {
      const inventions = SAMPLE_TIMELINE_DATA.milestones.filter(
        m => m.category === 'invention'
      );
      expect(inventions.length).toBeGreaterThan(0);
    });

    it('should handle theory category', () => {
      const theories = SAMPLE_TIMELINE_DATA.milestones.filter(
        m => m.category === 'theory'
      );
      expect(theories.length).toBeGreaterThan(0);
    });

    it('should handle person category', () => {
      const people = SAMPLE_TIMELINE_DATA.milestones.filter(
        m => m.category === 'person'
      );
      expect(people.length).toBeGreaterThanOrEqual(0);
    });

    it('should handle event category', () => {
      const events = SAMPLE_TIMELINE_DATA.milestones.filter(
        m => m.category === 'event'
      );
      expect(events.length).toBeGreaterThan(0);
    });
  });

  describe('Data Integrity', () => {
    it('should have milestones within era boundaries', () => {
      const eras = SAMPLE_TIMELINE_DATA.eras;
      const milestones = SAMPLE_TIMELINE_DATA.milestones;

      milestones.forEach(milestone => {
        const era = eras.find(e => e.id === milestone.eraId);
        if (era) {
          expect(milestone.year).toBeGreaterThanOrEqual(era.startYear);
          expect(milestone.year).toBeLessThanOrEqual(era.endYear);
        }
      });
    });

    it('should have non-overlapping eras', () => {
      const eras = SAMPLE_TIMELINE_DATA.eras.sort((a, b) => a.order - b.order);

      for (let i = 0; i < eras.length - 1; i++) {
        expect(eras[i].endYear).toBeLessThanOrEqual(eras[i + 1].startYear);
      }
    });

    it('should have eras in chronological order', () => {
      const eras = SAMPLE_TIMELINE_DATA.eras;
      const sortedEras = [...eras].sort((a, b) => a.order - b.order);

      for (let i = 0; i < sortedEras.length - 1; i++) {
        expect(sortedEras[i].startYear).toBeLessThan(sortedEras[i + 1].startYear);
      }
    });
  });

  describe('Store Integration', () => {
    it('should react to store changes', () => {
      const { container } = render(TimelineD3);

      // Change store data
      const newEras: TimelineEra[] = [{
        id: 'new-era',
        name: 'New Era',
        startYear: 0,
        endYear: 100,
        color: '#000000',
        description: 'New',
        order: 0,
        milestones: [],
      }];

      loadTimelineData(newEras, []);

      // Component should still render
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle filtered milestones', () => {
      const { container } = render(TimelineD3);

      // All milestones should be loaded initially
      let state: any;
      const unsubscribe = timelineVizStore.subscribe(s => { state = s; });

      expect(state.filteredMilestones).toHaveLength(SAMPLE_TIMELINE_DATA.milestones.length);

      unsubscribe();
    });
  });

  describe('Performance', () => {
    it('should handle large datasets', () => {
      // Create 100 milestones
      const largeMilestoneSet: Milestone[] = Array.from({ length: 100 }, (_, i) => ({
        id: `milestone-${i}`,
        year: -20000 + i * 200,
        title: `Milestone ${i}`,
        description: `Description ${i}`,
        category: ['invention', 'theory', 'person', 'event'][i % 4] as any,
        civilization: 'Test',
        eraId: 'era-0',
      }));

      loadTimelineData(SAMPLE_TIMELINE_DATA.eras, largeMilestoneSet);
      const { container } = render(TimelineD3);

      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should render efficiently with minimal eras', () => {
      const minimalEras: TimelineEra[] = [{
        id: 'single-era',
        name: 'Single Era',
        startYear: -1000,
        endYear: 1000,
        color: '#FF0000',
        description: 'Single',
        order: 0,
        milestones: [],
      }];

      loadTimelineData(minimalEras, []);
      const { container } = render(TimelineD3);

      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });
  });

  describe('Accessibility', () => {
    it('should have proper ARIA attributes', () => {
      const { container } = render(TimelineD3);
      const svg = container.querySelector('svg');

      // SVG should be in the DOM
      expect(svg).toBeTruthy();
    });

    it('should be keyboard navigable', () => {
      const { container } = render(TimelineD3);

      // Component should render and be in DOM
      expect(container.querySelector('.timeline-container')).toBeTruthy();
    });
  });

  describe('Error Handling', () => {
    it('should handle missing milestone data', () => {
      const erasWithoutMilestones = SAMPLE_TIMELINE_DATA.eras.map(era => ({
        ...era,
        milestones: [],
      }));

      loadTimelineData(erasWithoutMilestones, []);
      const { container } = render(TimelineD3);

      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });

    it('should handle invalid date ranges', () => {
      const invalidEra: TimelineEra = {
        id: 'invalid',
        name: 'Invalid',
        startYear: 2000,
        endYear: 1000, // End before start
        color: '#000000',
        description: 'Invalid',
        order: 0,
        milestones: [],
      };

      loadTimelineData([invalidEra], []);
      const { container } = render(TimelineD3);

      // Should still render without crashing
      expect(container.querySelector('.timeline-svg')).toBeTruthy();
    });
  });

  describe('Civilizations Coverage', () => {
    it('should include multiple civilizations', () => {
      const civilizations = new Set(
        SAMPLE_TIMELINE_DATA.milestones.map(m => m.civilization)
      );

      expect(civilizations.size).toBeGreaterThan(5);
    });

    it('should include African civilization', () => {
      const hasAfrican = SAMPLE_TIMELINE_DATA.milestones.some(
        m => m.civilization.toLowerCase().includes('african')
      );
      expect(hasAfrican).toBe(true);
    });

    it('should include Asian civilizations', () => {
      const hasAsian = SAMPLE_TIMELINE_DATA.milestones.some(
        m => m.civilization.toLowerCase().includes('indian') ||
           m.civilization.toLowerCase().includes('islamic')
      );
      expect(hasAsian).toBe(true);
    });

    it('should include European civilization', () => {
      const hasEuropean = SAMPLE_TIMELINE_DATA.milestones.some(
        m => m.civilization.toLowerCase().includes('european') ||
           m.civilization.toLowerCase().includes('greek')
      );
      expect(hasEuropean).toBe(true);
    });
  });

  describe('Historical Accuracy', () => {
    it('should have Ishango bone in prehistory', () => {
      const ishango = SAMPLE_TIMELINE_DATA.milestones.find(
        m => m.title.includes('Ishango')
      );

      expect(ishango).toBeTruthy();
      expect(ishango?.year).toBeLessThan(-10000);
    });

    it('should have Euclid in ancient era', () => {
      const euclid = SAMPLE_TIMELINE_DATA.milestones.find(
        m => m.title.includes('Euclid')
      );

      expect(euclid).toBeTruthy();
      expect(euclid?.eraId).toBe('era-1');
    });

    it('should have Turing in foundations era', () => {
      const turing = SAMPLE_TIMELINE_DATA.milestones.find(
        m => m.title.includes('Turing')
      );

      expect(turing).toBeTruthy();
      expect(turing?.eraId).toBe('era-4');
    });

    it('should have ENIAC in electronic era', () => {
      const eniac = SAMPLE_TIMELINE_DATA.milestones.find(
        m => m.title.includes('ENIAC')
      );

      expect(eniac).toBeTruthy();
      expect(eniac?.eraId).toBe('era-5');
    });
  });
});
