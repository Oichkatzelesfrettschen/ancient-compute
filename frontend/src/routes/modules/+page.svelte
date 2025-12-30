<script lang="ts">
	// Ancient Compute - Modules Page
	import { onMount } from 'svelte';
	import { moduleApi } from '$lib/api';
	import type { Module } from '$lib/api';

	let modules: Module[] = [];
	let loading = true;
	let error = '';
	let selectedEra: string = 'all';

	const eras = [
		{ value: 'all', label: 'All Eras' },
		{ value: 'prehistory', label: 'Prehistory (20,000 BC - 3000 BC)' },
		{ value: 'ancient', label: 'Ancient (3000 BC - 500 AD)' },
		{ value: 'medieval', label: 'Medieval (500 AD - 1500 AD)' },
		{ value: 'early_modern', label: 'Early Modern (1500 AD - 1800 AD)' },
		{ value: 'modern', label: 'Modern (1800 AD - 1950 AD)' },
		{ value: 'contemporary', label: 'Contemporary (1950 AD - Present)' }
	];

	onMount(async () => {
		try {
			const data = await moduleApi.list();
			modules = data.modules || [];
		} catch (e: any) {
			error = e.message || 'Failed to load modules';
			console.error('Error loading modules:', e);
		} finally {
			loading = false;
		}
	});

	$: filteredModules = selectedEra === 'all'
		? modules
		: modules.filter(m => m.era === selectedEra);
</script>

<svelte:head>
	<title>Learning Modules - Ancient Compute</title>
</svelte:head>

<div class="modules-page">
	<header class="page-header">
		<h1>Learning Modules</h1>
		<p class="page-description">
			Explore 12,500 years of computational history through structured learning modules.
			Each module covers a specific period, civilization, or concept in the evolution
			of computation and logic.
		</p>
	</header>

	<div class="filters">
		<label for="era-filter">Filter by Era:</label>
		<select id="era-filter" bind:value={selectedEra}>
			{#each eras as era}
				<option value={era.value}>{era.label}</option>
			{/each}
		</select>
	</div>

	<div class="modules-content">
		{#if loading}
			<div class="loading">
				<p>Loading modules...</p>
			</div>
		{:else if error}
			<div class="error">
				<p>{error}</p>
			</div>
		{:else if filteredModules.length > 0}
			<div class="modules-grid">
				{#each filteredModules as module}
					<article class="module-card">
						<div class="module-header">
							<h2>{module.title}</h2>
							{#if module.era}
								<span class="era-badge">{module.era}</span>
							{/if}
						</div>
						<p class="module-description">{module.description}</p>
						{#if module.start_year && module.end_year}
							<div class="module-meta">
								<span class="years">{module.start_year} to {module.end_year}</span>
								{#if module.estimated_hours}
									<span class="duration">{module.estimated_hours} hours</span>
								{/if}
							</div>
						{/if}
						<a href="/modules/{module.slug}" class="module-link">
							Start Module →
						</a>
					</article>
				{/each}
			</div>
		{:else}
			<div class="empty">
				<p>No modules found for the selected era.</p>
				<button on:click={() => selectedEra = 'all'}>Show all modules</button>
			</div>
		{/if}
	</div>
</div>

<style>
	.modules-page {
		max-width: 1000px;
		margin: 0 auto;
	}

	.page-header {
		text-align: center;
		margin-bottom: 3rem;
	}

	.page-header h1 {
		font-size: 2.5rem;
		color: #1a1a2e;
		margin-bottom: 1rem;
	}

	.page-description {
		font-size: 1.1rem;
		color: #555;
		line-height: 1.6;
		max-width: 800px;
		margin: 0 auto;
	}

	.filters {
		display: flex;
		align-items: center;
		gap: 1rem;
		margin-bottom: 2rem;
		padding: 1rem;
		background: white;
		border-radius: 8px;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}

	.filters label {
		font-weight: 600;
		color: #1a1a2e;
	}

	.filters select {
		flex: 1;
		padding: 0.5rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 1rem;
	}

	.modules-grid {
		display: grid;
		gap: 1.5rem;
		grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
	}

	.module-card {
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		padding: 1.5rem;
		transition: all 0.2s;
	}

	.module-card:hover {
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
		transform: translateY(-2px);
	}

	.module-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		margin-bottom: 1rem;
	}

	.module-card h2 {
		font-size: 1.25rem;
		color: #1a1a2e;
		margin: 0;
		flex: 1;
	}

	.era-badge {
		background: #667eea;
		color: white;
		padding: 0.25rem 0.5rem;
		border-radius: 4px;
		font-size: 0.75rem;
		text-transform: uppercase;
		margin-left: 0.5rem;
	}

	.module-description {
		color: #555;
		line-height: 1.5;
		margin-bottom: 1rem;
	}

	.module-meta {
		display: flex;
		gap: 1rem;
		margin-bottom: 1rem;
		font-size: 0.875rem;
		color: #666;
	}

	.module-link {
		display: inline-block;
		color: #667eea;
		text-decoration: none;
		font-weight: 600;
		transition: color 0.2s;
	}

	.module-link:hover {
		color: #4c5ed0;
	}

	.loading, .error, .empty {
		text-align: center;
		padding: 3rem;
		background: white;
		border-radius: 8px;
	}

	.error {
		color: #d32f2f;
		background: #ffebee;
	}

	.empty button {
		margin-top: 1rem;
		padding: 0.5rem 1rem;
		background: #667eea;
		color: white;
		border: none;
		border-radius: 4px;
		cursor: pointer;
		font-size: 1rem;
	}

	.empty button:hover {
		background: #4c5ed0;
	}
</style>
