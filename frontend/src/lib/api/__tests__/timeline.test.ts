/**
 * timeline.test.ts - Tests for timeline API client
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  fetchTimeline,
  fetchEras,
  fetchEraDetail,
  fetchMilestones,
  fetchEraMilestones,
  searchTimeline,
  formatYear,
  formatYearRange,
  getEraDuration,
  isYearInEra,
  findEraByYear,
} from '../timeline';
import type { TimelineEra, Milestone } from '../timeline';

// Mock fetch
global.fetch = vi.fn();

describe('Timeline API', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  const mockEras: TimelineEra[] = [
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
  ];

  const mockMilestones: Milestone[] = [
    {
      id: 'm1',
      year: -2000,
      title: 'Ancient Invention',
      description: 'Description',
      category: 'invention',
      civilization: 'Greek',
    },
  ];

  describe('fetchTimeline', () => {
    it('should fetch full timeline data', async () => {
      const mockResponse = {
        eras: mockEras,
        totalMilestones: 1,
        civilizations: ['Greek'],
        categories: ['invention'],
      };

      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => mockResponse,
      });

      const result = await fetchTimeline();

      expect(result.eras).toEqual(mockEras);
      expect(result.totalMilestones).toBe(1);
    });

    it('should handle API errors', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: false,
        status: 404,
        statusText: 'Not Found',
        json: async () => ({}),
      });

      await expect(fetchTimeline()).rejects.toThrow();
    });

    it('should handle network errors', async () => {
      (global.fetch as any).mockRejectedValueOnce(new Error('Network error'));

      await expect(fetchTimeline()).rejects.toThrow('Network error');
    });
  });

  describe('fetchEras', () => {
    it('should fetch eras list', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ eras: mockEras }),
      });

      const result = await fetchEras();

      expect(result).toEqual(mockEras);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/timeline/eras'),
        expect.any(Object)
      );
    });
  });

  describe('fetchEraDetail', () => {
    it('should fetch era details', async () => {
      const mockDetail = {
        ...mockEras[0],
        modules: [
          {
            id: 'mod-1',
            title: 'Module 1',
            description: 'Description',
            lessonCount: 5,
            estimatedHours: 10,
          },
        ],
      };

      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => mockDetail,
      });

      const result = await fetchEraDetail('era-1');

      expect(result.id).toBe('era-1');
      expect(result.modules).toBeTruthy();
    });
  });

  describe('fetchMilestones', () => {
    it('should fetch milestones without filters', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      const result = await fetchMilestones();

      expect(result).toEqual(mockMilestones);
    });

    it('should fetch milestones with category filter', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      await fetchMilestones({ categories: ['invention'] });

      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('categories=invention'),
        expect.any(Object)
      );
    });

    it('should fetch milestones with civilization filter', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      await fetchMilestones({ civilizations: ['Greek'] });

      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('civilizations=Greek'),
        expect.any(Object)
      );
    });

    it('should fetch milestones with year range', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      await fetchMilestones({ startYear: -3000, endYear: 0 });

      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('start_year=-3000'),
        expect.any(Object)
      );
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('end_year=0'),
        expect.any(Object)
      );
    });

    it('should fetch milestones with search query', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      await fetchMilestones({ searchQuery: 'ancient' });

      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('q=ancient'),
        expect.any(Object)
      );
    });
  });

  describe('fetchEraMilestones', () => {
    it('should fetch milestones for specific era', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({ milestones: mockMilestones }),
      });

      const result = await fetchEraMilestones('era-1');

      expect(result).toEqual(mockMilestones);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/timeline/eras/era-1/milestones'),
        expect.any(Object)
      );
    });
  });

  describe('searchTimeline', () => {
    it('should search timeline', async () => {
      (global.fetch as any).mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          milestones: mockMilestones,
          eras: mockEras,
        }),
      });

      const result = await searchTimeline('ancient');

      expect(result.milestones).toBeTruthy();
      expect(result.eras).toBeTruthy();
    });
  });

  describe('Utility Functions', () => {
    describe('formatYear', () => {
      it('should format BCE years', () => {
        expect(formatYear(-3000)).toBe('3,000 BCE');
        expect(formatYear(-1)).toBe('1 BCE');
      });

      it('should format CE years', () => {
        expect(formatYear(1000)).toBe('1,000 CE');
        expect(formatYear(2025)).toBe('2,025 CE');
      });

      it('should handle year 0', () => {
        expect(formatYear(0)).toBe('0');
      });

      it('should handle large numbers with commas', () => {
        expect(formatYear(-20000)).toBe('20,000 BCE');
        expect(formatYear(10000)).toBe('10,000 CE');
      });
    });

    describe('formatYearRange', () => {
      it('should format BCE to BCE range', () => {
        const result = formatYearRange(-3000, -1000);
        expect(result).toBe('3,000 BCE - 1,000 BCE');
      });

      it('should format BCE to CE range', () => {
        const result = formatYearRange(-500, 500);
        expect(result).toBe('500 BCE - 500 CE');
      });

      it('should format CE to CE range', () => {
        const result = formatYearRange(1000, 2000);
        expect(result).toBe('1,000 CE - 2,000 CE');
      });
    });

    describe('getEraDuration', () => {
      it('should calculate era duration', () => {
        const era: TimelineEra = {
          id: 'test',
          name: 'Test',
          startYear: -3000,
          endYear: 500,
          color: '#000',
          description: 'Test',
          order: 0,
          milestones: [],
        };

        expect(getEraDuration(era)).toBe(3500);
      });

      it('should handle CE-only eras', () => {
        const era: TimelineEra = {
          id: 'test',
          name: 'Test',
          startYear: 1000,
          endYear: 2000,
          color: '#000',
          description: 'Test',
          order: 0,
          milestones: [],
        };

        expect(getEraDuration(era)).toBe(1000);
      });
    });

    describe('isYearInEra', () => {
      const era: TimelineEra = {
        id: 'test',
        name: 'Test',
        startYear: -3000,
        endYear: 500,
        color: '#000',
        description: 'Test',
        order: 0,
        milestones: [],
      };

      it('should return true for year within era', () => {
        expect(isYearInEra(0, era)).toBe(true);
        expect(isYearInEra(-2000, era)).toBe(true);
        expect(isYearInEra(400, era)).toBe(true);
      });

      it('should return true for boundary years', () => {
        expect(isYearInEra(-3000, era)).toBe(true);
        expect(isYearInEra(500, era)).toBe(true);
      });

      it('should return false for year outside era', () => {
        expect(isYearInEra(-4000, era)).toBe(false);
        expect(isYearInEra(1000, era)).toBe(false);
      });
    });

    describe('findEraByYear', () => {
      const eras: TimelineEra[] = [
        {
          id: 'era-1',
          name: 'Ancient',
          startYear: -3000,
          endYear: 500,
          color: '#FFD700',
          description: 'Ancient',
          order: 0,
          milestones: [],
        },
        {
          id: 'era-2',
          name: 'Medieval',
          startYear: 500,
          endYear: 1500,
          color: '#4169E1',
          description: 'Medieval',
          order: 1,
          milestones: [],
        },
      ];

      it('should find era containing year', () => {
        const result = findEraByYear(-2000, eras);
        expect(result?.id).toBe('era-1');
      });

      it('should find era for boundary year', () => {
        const result = findEraByYear(500, eras);
        expect(result).toBeTruthy();
      });

      it('should return null for year not in any era', () => {
        const result = findEraByYear(-5000, eras);
        expect(result).toBeNull();
      });

      it('should return null for empty eras array', () => {
        const result = findEraByYear(1000, []);
        expect(result).toBeNull();
      });
    });
  });
});
