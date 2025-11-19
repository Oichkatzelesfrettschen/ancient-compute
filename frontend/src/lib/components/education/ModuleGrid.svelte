<script lang="ts">
/**
 * ModuleGrid.svelte - Module browsing grid with filtering and sorting
 *
 * Features:
 * - Responsive grid layout
 * - Filter by era, difficulty, status
 * - Sort by name, difficulty, progress, date
 * - Search functionality
 * - Loading skeleton states
 * - Infinite scroll/pagination
 */

import { onMount } from 'svelte';
import ModuleCard from './ModuleCard.svelte';
import {
	educationStore,
	filteredSortedModules,
	applyFilters,
	setSort,
	clearFilters,
	type ModuleFilters,
	type ModuleSortConfig,
} from '../../stores/educationStore';
import type { Module } from '../../stores/timelineStore';

export let eras: Array<{ id: string; label: string }> = [];

let searchQuery = '';
let selectedEras: string[] = [];
let selectedDifficulties: string[] = [];
let selectedStatuses: string[] = [];
let sortField: ModuleSortConfig['field'] = 'name';
let sortOrder: ModuleSortConfig['order'] = 'asc';
let isLoading = false;
let showFilters = false;

// Subscribe to filtered modules
let modules: any[] = [];
filteredSortedModules.subscribe((m) => {
	modules = m;
});

// Difficulty options
const difficultyOptions = [
	{ value: 'beginner', label: 'Beginner', color: '#7cb342' },
	{ value: 'intermediate', label: 'Intermediate', color: '#ff9800' },
	{ value: 'advanced', label: 'Advanced', color: '#e53935' },
];

// Status options
const statusOptions = [
	{ value: 'not_started', label: 'Not Started', color: '#ff9800' },
	{ value: 'in_progress', label: 'In Progress', color: '#2196f3' },
	{ value: 'completed', label: 'Completed', color: '#7cb342' },
];

// Sort options
const sortOptions = [
	{ value: 'name', label: 'Name' },
	{ value: 'difficulty', label: 'Difficulty' },
	{ value: 'progress', label: 'Progress' },
	{ value: 'date', label: 'Recently Accessed' },
	{ value: 'estimated_time', label: 'Duration' },
];

// Reactive statement to apply filters
$: {
	applyFilters({
		era: selectedEras,
		difficulty: selectedDifficulties,
		status: selectedStatuses,
		searchQuery,
	});
}

// Reactive statement to apply sorting
$: {
	setSort(sortField, sortOrder);
}

function handleClearFilters(): void {
	searchQuery = '';
	selectedEras = [];
	selectedDifficulties = [];
	selectedStatuses = [];
	clearFilters();
}

function toggleEraFilter(eraId: string): void {
	if (selectedEras.includes(eraId)) {
		selectedEras = selectedEras.filter((id) => id !== eraId);
	} else {
		selectedEras = [...selectedEras, eraId];
	}
}

function toggleDifficultyFilter(difficulty: string): void {
	if (selectedDifficulties.includes(difficulty)) {
		selectedDifficulties = selectedDifficulties.filter((d) => d !== difficulty);
	} else {
		selectedDifficulties = [...selectedDifficulties, difficulty];
	}
}

function toggleStatusFilter(status: string): void {
	if (selectedStatuses.includes(status)) {
		selectedStatuses = selectedStatuses.filter((s) => s !== status);
	} else {
		selectedStatuses = [...selectedStatuses, status];
	}
}

function toggleSortOrder(): void {
	sortOrder = sortOrder === 'asc' ? 'desc' : 'asc';
}

const activeFilterCount = $educationStore.filters.era.length +
	$educationStore.filters.difficulty.length +
	$educationStore.filters.status.length +
	($educationStore.filters.searchQuery ? 1 : 0);
</script>

<div class="module-grid-container">
	<!-- Header with search and filters -->
	<div class="grid-header">
		<div class="search-section">
			<div class="search-box">
				<span class="search-icon">🔍</span>
				<input
					type="text"
					placeholder="Search modules..."
					bind:value={searchQuery}
					class="search-input"
				/>
				{#if searchQuery}
					<button class="clear-search" on:click={() => (searchQuery = '')}>✕</button>
				{/if}
			</div>
		</div>

		<div class="controls-section">
			<!-- Filter toggle -->
			<button class="filter-toggle" on:click={() => (showFilters = !showFilters)}>
				<span class="icon">🔽</span>
				<span>Filters</span>
				{#if activeFilterCount > 0}
					<span class="filter-count">{activeFilterCount}</span>
				{/if}
			</button>

			<!-- Sort controls -->
			<div class="sort-controls">
				<select bind:value={sortField} class="sort-select">
					{#each sortOptions as option}
						<option value={option.value}>{option.label}</option>
					{/each}
				</select>
				<button class="sort-order-btn" on:click={toggleSortOrder} title="Toggle sort order">
					{sortOrder === 'asc' ? '↑' : '↓'}
				</button>
			</div>
		</div>
	</div>

	<!-- Filters panel -->
	{#if showFilters}
		<div class="filters-panel">
			<!-- Era filters -->
			{#if eras.length > 0}
				<div class="filter-group">
					<h3 class="filter-title">Era</h3>
					<div class="filter-options">
						{#each eras as era}
							<button
								class="filter-option"
								class:active={selectedEras.includes(era.id)}
								on:click={() => toggleEraFilter(era.id)}
							>
								{era.label}
							</button>
						{/each}
					</div>
				</div>
			{/if}

			<!-- Difficulty filters -->
			<div class="filter-group">
				<h3 class="filter-title">Difficulty</h3>
				<div class="filter-options">
					{#each difficultyOptions as option}
						<button
							class="filter-option"
							class:active={selectedDifficulties.includes(option.value)}
							on:click={() => toggleDifficultyFilter(option.value)}
						>
							<span class="filter-color" style="background-color: {option.color}"></span>
							{option.label}
						</button>
					{/each}
				</div>
			</div>

			<!-- Status filters -->
			<div class="filter-group">
				<h3 class="filter-title">Status</h3>
				<div class="filter-options">
					{#each statusOptions as option}
						<button
							class="filter-option"
							class:active={selectedStatuses.includes(option.value)}
							on:click={() => toggleStatusFilter(option.value)}
						>
							<span class="filter-color" style="background-color: {option.color}"></span>
							{option.label}
						</button>
					{/each}
				</div>
			</div>

			<!-- Clear filters -->
			{#if activeFilterCount > 0}
				<div class="filter-actions">
					<button class="clear-filters-btn" on:click={handleClearFilters}>
						Clear all filters
					</button>
				</div>
			{/if}
		</div>
	{/if}

	<!-- Results summary -->
	<div class="results-summary">
		<span class="results-count">
			{modules.length} {modules.length === 1 ? 'module' : 'modules'}
		</span>
		{#if activeFilterCount > 0}
			<span class="results-filtered">(filtered)</span>
		{/if}
	</div>

	<!-- Module grid -->
	{#if isLoading}
		<div class="module-grid">
			{#each Array(6) as _, i}
				<div class="skeleton-card"></div>
			{/each}
		</div>
	{:else if modules.length > 0}
		<div class="module-grid">
			{#each modules as module (module.id)}
				<ModuleCard {module} progress={module.progress} />
			{/each}
		</div>
	{:else}
		<div class="empty-state">
			<div class="empty-icon">📚</div>
			<h3 class="empty-title">No modules found</h3>
			<p class="empty-text">
				{#if activeFilterCount > 0}
					Try adjusting your filters or search query.
				{:else}
					No modules are currently available.
				{/if}
			</p>
			{#if activeFilterCount > 0}
				<button class="empty-action" on:click={handleClearFilters}>Clear filters</button>
			{/if}
		</div>
	{/if}
</div>

<style>
	.module-grid-container {
		display: flex;
		flex-direction: column;
		gap: 24px;
		width: 100%;
	}

	.grid-header {
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.search-section {
		display: flex;
		gap: 12px;
	}

	.search-box {
		position: relative;
		flex: 1;
		max-width: 600px;
	}

	.search-icon {
		position: absolute;
		left: 14px;
		top: 50%;
		transform: translateY(-50%);
		font-size: 16px;
		pointer-events: none;
	}

	.search-input {
		width: 100%;
		padding: 12px 40px 12px 44px;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 14px;
		transition: all 0.2s ease;
	}

	.search-input:focus {
		outline: none;
		border-color: #4a90e2;
		box-shadow: 0 0 0 3px rgba(74, 144, 226, 0.1);
	}

	.clear-search {
		position: absolute;
		right: 12px;
		top: 50%;
		transform: translateY(-50%);
		background: transparent;
		border: none;
		cursor: pointer;
		font-size: 16px;
		color: #999;
		padding: 4px;
		transition: color 0.2s ease;
	}

	.clear-search:hover {
		color: #666;
	}

	.controls-section {
		display: flex;
		gap: 12px;
		align-items: center;
	}

	.filter-toggle {
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 10px 16px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
		transition: all 0.2s ease;
	}

	.filter-toggle:hover {
		border-color: #4a90e2;
		color: #4a90e2;
	}

	.filter-toggle .icon {
		font-size: 12px;
	}

	.filter-count {
		display: flex;
		align-items: center;
		justify-content: center;
		min-width: 20px;
		height: 20px;
		padding: 0 6px;
		background: #4a90e2;
		border-radius: 10px;
		font-size: 11px;
		font-weight: 700;
		color: white;
	}

	.sort-controls {
		display: flex;
		gap: 8px;
	}

	.sort-select {
		padding: 10px 12px;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
		background: white;
		cursor: pointer;
		transition: all 0.2s ease;
	}

	.sort-select:focus {
		outline: none;
		border-color: #4a90e2;
	}

	.sort-order-btn {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 40px;
		padding: 10px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		cursor: pointer;
		font-size: 18px;
		color: #2c3e50;
		transition: all 0.2s ease;
	}

	.sort-order-btn:hover {
		border-color: #4a90e2;
		color: #4a90e2;
	}

	.filters-panel {
		display: flex;
		flex-direction: column;
		gap: 20px;
		padding: 20px;
		background: #f8f8f8;
		border-radius: 8px;
	}

	.filter-group {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.filter-title {
		margin: 0;
		font-size: 13px;
		font-weight: 700;
		color: #2c3e50;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.filter-options {
		display: flex;
		flex-wrap: wrap;
		gap: 8px;
	}

	.filter-option {
		display: flex;
		align-items: center;
		gap: 6px;
		padding: 8px 14px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		cursor: pointer;
		font-size: 13px;
		font-weight: 600;
		color: #666;
		transition: all 0.2s ease;
	}

	.filter-option:hover {
		border-color: #4a90e2;
		color: #2c3e50;
	}

	.filter-option.active {
		background: #4a90e2;
		border-color: #4a90e2;
		color: white;
	}

	.filter-color {
		display: inline-block;
		width: 12px;
		height: 12px;
		border-radius: 50%;
	}

	.filter-actions {
		display: flex;
		justify-content: flex-end;
		padding-top: 12px;
		border-top: 1px solid #e0e0e0;
	}

	.clear-filters-btn {
		padding: 8px 16px;
		background: transparent;
		border: none;
		cursor: pointer;
		font-size: 13px;
		font-weight: 600;
		color: #e53935;
		transition: color 0.2s ease;
	}

	.clear-filters-btn:hover {
		color: #c62828;
	}

	.results-summary {
		display: flex;
		align-items: center;
		gap: 8px;
		font-size: 14px;
	}

	.results-count {
		font-weight: 600;
		color: #2c3e50;
	}

	.results-filtered {
		color: #999;
		font-style: italic;
	}

	.module-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
		gap: 24px;
	}

	.skeleton-card {
		height: 380px;
		background: linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%);
		background-size: 200% 100%;
		animation: loading 1.5s infinite;
		border-radius: 12px;
	}

	@keyframes loading {
		0% {
			background-position: 200% 0;
		}
		100% {
			background-position: -200% 0;
		}
	}

	.empty-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		gap: 16px;
		min-height: 400px;
		padding: 40px;
		text-align: center;
	}

	.empty-icon {
		font-size: 64px;
		opacity: 0.3;
	}

	.empty-title {
		margin: 0;
		font-size: 24px;
		font-weight: 700;
		color: #2c3e50;
	}

	.empty-text {
		margin: 0;
		font-size: 14px;
		color: #666;
		max-width: 400px;
	}

	.empty-action {
		padding: 10px 20px;
		background: #4a90e2;
		border: none;
		border-radius: 6px;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: white;
		transition: background 0.2s ease;
	}

	.empty-action:hover {
		background: #357abd;
	}

	@media (max-width: 768px) {
		.grid-header {
			gap: 12px;
		}

		.controls-section {
			flex-wrap: wrap;
		}

		.module-grid {
			grid-template-columns: 1fr;
		}

		.filters-panel {
			padding: 16px;
		}

		.filter-options {
			gap: 6px;
		}

		.filter-option {
			padding: 6px 10px;
			font-size: 12px;
		}
	}
</style>
