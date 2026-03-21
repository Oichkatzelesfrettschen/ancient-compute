<script lang="ts">
	import { onMount } from 'svelte';
	import { machinesApi, CATEGORY_LABELS, CATEGORY_COLORS, type MachineListItem } from '$lib/api/machines';

	let machines: MachineListItem[] = [];
	let loading = true;
	let error = '';
	let filterCategory = '';

	const categories = Object.entries(CATEGORY_LABELS);

	onMount(async () => {
		try {
			machines = await machinesApi.list();
		} catch (e) {
			error = e instanceof Error ? e.message : String(e);
		} finally {
			loading = false;
		}
	});

	$: filtered = filterCategory
		? machines.filter((m) => m.category === filterCategory)
		: machines;

	function yearLabel(year: number): string {
		return year < 0 ? `${Math.abs(year)} BCE` : String(year);
	}
</script>

<svelte:head>
	<title>Machine Gallery -- Ancient Compute</title>
	<meta name="description" content="Interactive gallery of 24 historical computing machines from ancient Greece to the dawn of the computer age." />
</svelte:head>

<div class="gallery-page">
	<header class="page-header">
		<h1>Historical Machine Gallery</h1>
		<p class="subtitle">
			24 emulated machines spanning from ancient Greece (~100 BCE) to 1949.
			Each machine is fully programmable via a live step-by-step interface.
		</p>
	</header>

	<div class="filter-bar">
		<button
			class="filter-btn"
			class:active={filterCategory === ''}
			on:click={() => (filterCategory = '')}
		>All</button>
		{#each categories as [key, label]}
			<button
				class="filter-btn"
				class:active={filterCategory === key}
				style="--cat-color: {CATEGORY_COLORS[key]}"
				on:click={() => (filterCategory = filterCategory === key ? '' : key)}
			>{label}</button>
		{/each}
	</div>

	{#if loading}
		<div class="loading">Loading machines...</div>
	{:else if error}
		<div class="error">Failed to load machines: {error}</div>
	{:else}
		<div class="machine-grid">
			{#each filtered as machine (machine.id)}
				<a href="/machines/{machine.id}" class="machine-card">
					<div class="card-header">
						<span
							class="category-badge"
							style="background: {CATEGORY_COLORS[machine.category] ?? '#374151'}"
						>
							{CATEGORY_LABELS[machine.category] ?? machine.category}
						</span>
						<span class="year">{yearLabel(machine.year)}</span>
					</div>

					<h2 class="machine-name">{machine.name}</h2>
					<p class="inventor">{machine.inventor} &mdash; {machine.country}</p>
					<p class="brief">{machine.brief}</p>

					<div class="tags">
						{#each machine.tags.slice(0, 3) as tag}
							<span class="tag">{tag}</span>
						{/each}
					</div>

					<div class="card-footer">
						<span class="input-type">Input: {machine.program_input_type}</span>
						<span class="explore-link">Open emulator &rarr;</span>
					</div>
				</a>
			{/each}
		</div>
	{/if}
</div>

<style>
	.gallery-page {
		max-width: 1200px;
		margin: 0 auto;
		padding: 2rem;
	}

	.page-header {
		margin-bottom: 2rem;
		text-align: center;
	}

	.page-header h1 {
		font-size: 2rem;
		color: #1a1a2e;
		margin-bottom: 0.5rem;
	}

	.subtitle {
		color: #555;
		max-width: 640px;
		margin: 0 auto;
	}

	.filter-bar {
		display: flex;
		flex-wrap: wrap;
		gap: 0.5rem;
		margin-bottom: 1.5rem;
		justify-content: center;
	}

	.filter-btn {
		padding: 0.35rem 0.9rem;
		border: 1px solid #ddd;
		border-radius: 20px;
		background: white;
		cursor: pointer;
		font-size: 0.85rem;
		transition: all 0.15s;
		color: #333;
	}

	.filter-btn:hover {
		border-color: var(--cat-color, #667eea);
		color: var(--cat-color, #667eea);
	}

	.filter-btn.active {
		background: var(--cat-color, #667eea);
		border-color: var(--cat-color, #667eea);
		color: white;
	}

	.machine-grid {
		display: grid;
		grid-template-columns: repeat(auto-fill, minmax(320px, 1fr));
		gap: 1.25rem;
	}

	.machine-card {
		display: flex;
		flex-direction: column;
		background: white;
		border: 1px solid #e8e8e8;
		border-radius: 8px;
		padding: 1.25rem;
		text-decoration: none;
		color: inherit;
		transition: box-shadow 0.2s, transform 0.15s;
	}

	.machine-card:hover {
		box-shadow: 0 4px 16px rgba(0, 0, 0, 0.1);
		transform: translateY(-2px);
	}

	.card-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.75rem;
	}

	.category-badge {
		display: inline-block;
		padding: 0.2rem 0.6rem;
		border-radius: 12px;
		color: white;
		font-size: 0.75rem;
		font-weight: 600;
		letter-spacing: 0.02em;
	}

	.year {
		font-size: 0.85rem;
		color: #888;
		font-variant-numeric: tabular-nums;
	}

	.machine-name {
		font-size: 1.1rem;
		font-weight: 700;
		color: #1a1a2e;
		margin: 0 0 0.25rem;
	}

	.inventor {
		font-size: 0.85rem;
		color: #666;
		margin: 0 0 0.6rem;
	}

	.brief {
		font-size: 0.9rem;
		color: #444;
		margin: 0 0 0.75rem;
		flex: 1;
		line-height: 1.45;
	}

	.tags {
		display: flex;
		flex-wrap: wrap;
		gap: 0.3rem;
		margin-bottom: 0.75rem;
	}

	.tag {
		padding: 0.15rem 0.5rem;
		background: #f0f0f0;
		border-radius: 4px;
		font-size: 0.75rem;
		color: #555;
	}

	.card-footer {
		display: flex;
		justify-content: space-between;
		align-items: center;
		border-top: 1px solid #f0f0f0;
		padding-top: 0.6rem;
		margin-top: auto;
		font-size: 0.8rem;
	}

	.input-type {
		color: #888;
	}

	.explore-link {
		color: #667eea;
		font-weight: 600;
	}

	.loading,
	.error {
		text-align: center;
		padding: 3rem;
		color: #666;
	}

	.error {
		color: #c00;
	}

	@media (max-width: 600px) {
		.machine-grid {
			grid-template-columns: 1fr;
		}
	}
</style>
