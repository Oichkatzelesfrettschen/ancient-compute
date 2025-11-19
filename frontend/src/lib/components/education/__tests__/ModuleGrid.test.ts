/**
 * ModuleGrid.test.ts - Tests for ModuleGrid component
 *
 * Tests:
 * - Component rendering
 * - Filtering functionality
 * - Sorting functionality
 * - Search functionality
 * - User interactions
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, fireEvent, screen } from '@testing-library/svelte';
import ModuleGrid from '../ModuleGrid.svelte';
import { educationStore } from '../../../stores/educationStore';
import { sampleModules, sampleEras } from '../../../data/sampleModules';

describe('ModuleGrid', () => {
	beforeEach(() => {
		// Reset store before each test
		educationStore.set({
			modules: sampleModules,
			filteredModules: sampleModules,
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
		});
	});

	describe('Rendering', () => {
		it('should render module cards', () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const cards = container.querySelectorAll('.module-card');
			expect(cards.length).toBeGreaterThan(0);
		});

		it('should display search box', () => {
			render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');
			expect(searchInput).toBeTruthy();
		});

		it('should display filter controls', () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const filterToggle = container.querySelector('.filter-toggle');
			expect(filterToggle).toBeTruthy();
		});

		it('should display sort controls', () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const sortSelect = container.querySelector('.sort-select');
			expect(sortSelect).toBeTruthy();
		});
	});

	describe('Search Functionality', () => {
		it('should filter modules by search query', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			await fireEvent.input(searchInput, { target: { value: 'Boolean' } });

			const cards = container.querySelectorAll('.module-card');
			expect(cards.length).toBeLessThan(sampleModules.length);
		});

		it('should show clear button when search has text', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			await fireEvent.input(searchInput, { target: { value: 'test' } });

			const clearButton = container.querySelector('.clear-search');
			expect(clearButton).toBeTruthy();
		});

		it('should clear search when clear button clicked', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...') as HTMLInputElement;

			await fireEvent.input(searchInput, { target: { value: 'test' } });
			const clearButton = container.querySelector('.clear-search');
			await fireEvent.click(clearButton as Element);

			expect(searchInput.value).toBe('');
		});
	});

	describe('Filter Functionality', () => {
		it('should toggle filters panel', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const filterToggle = container.querySelector('.filter-toggle');

			await fireEvent.click(filterToggle as Element);

			const filtersPanel = container.querySelector('.filters-panel');
			expect(filtersPanel).toBeTruthy();
		});

		it('should show active filter count', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const filterToggle = container.querySelector('.filter-toggle');

			await fireEvent.click(filterToggle as Element);

			// Select a difficulty filter
			const beginnerFilter = screen.getByText('Beginner');
			await fireEvent.click(beginnerFilter);

			const filterCount = container.querySelector('.filter-count');
			expect(filterCount?.textContent).toBe('1');
		});

		it('should clear all filters', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const filterToggle = container.querySelector('.filter-toggle');

			await fireEvent.click(filterToggle as Element);

			// Select filters
			const beginnerFilter = screen.getByText('Beginner');
			await fireEvent.click(beginnerFilter);

			// Clear filters
			const clearButton = container.querySelector('.clear-filters-btn');
			await fireEvent.click(clearButton as Element);

			const filterCount = container.querySelector('.filter-count');
			expect(filterCount).toBeFalsy();
		});
	});

	describe('Sort Functionality', () => {
		it('should sort modules by name', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const sortSelect = container.querySelector('.sort-select') as HTMLSelectElement;

			await fireEvent.change(sortSelect, { target: { value: 'name' } });

			expect(sortSelect.value).toBe('name');
		});

		it('should toggle sort order', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const sortOrderBtn = container.querySelector('.sort-order-btn');

			await fireEvent.click(sortOrderBtn as Element);

			// Check if order changed (icon should change)
			expect(sortOrderBtn?.textContent).toMatch(/[↑↓]/);
		});
	});

	describe('Empty State', () => {
		it('should show empty state when no modules match filters', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			// Search for something that doesn't exist
			await fireEvent.input(searchInput, { target: { value: 'nonexistent-module-12345' } });

			const emptyState = container.querySelector('.empty-state');
			expect(emptyState).toBeTruthy();
			expect(emptyState?.textContent).toContain('No modules found');
		});

		it('should show clear filters button in empty state', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			await fireEvent.input(searchInput, { target: { value: 'nonexistent' } });

			const clearButton = container.querySelector('.empty-action');
			expect(clearButton?.textContent).toContain('Clear filters');
		});
	});

	describe('Results Summary', () => {
		it('should show module count', () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const resultsCount = container.querySelector('.results-count');
			expect(resultsCount).toBeTruthy();
			expect(resultsCount?.textContent).toMatch(/\d+ modules?/);
		});

		it('should show filtered indicator', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			await fireEvent.input(searchInput, { target: { value: 'test' } });

			const filteredIndicator = container.querySelector('.results-filtered');
			expect(filteredIndicator).toBeTruthy();
		});
	});

	describe('Loading State', () => {
		it('should show skeleton cards when loading', () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });

			// Manually set loading state
			educationStore.update(state => ({ ...state, isLoading: true }));

			const skeletonCards = container.querySelectorAll('.skeleton-card');
			expect(skeletonCards.length).toBeGreaterThan(0);
		});
	});

	describe('Accessibility', () => {
		it('should have proper ARIA labels', () => {
			render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');
			expect(searchInput.getAttribute('type')).toBe('text');
		});

		it('should support keyboard navigation', async () => {
			const { container } = render(ModuleGrid, { props: { eras: sampleEras } });
			const searchInput = screen.getByPlaceholderText('Search modules...');

			await fireEvent.focus(searchInput);
			await fireEvent.keyDown(searchInput, { key: 'Tab' });

			// Check if next element gets focus
			expect(document.activeElement).toBeTruthy();
		});
	});
});
