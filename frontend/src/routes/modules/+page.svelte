<script lang="ts">
	// Ancient Compute - Modules Page
	import { onMount } from 'svelte';
	import ModuleGrid from '$lib/components/education/ModuleGrid.svelte';
	import { educationStore, loadModules } from '$lib/stores/educationStore';
	import { sampleEras, sampleModules } from '$lib/data/sampleModules';

	let loading = true;

	onMount(async () => {
		try {
			// Load sample data (replace with API call in production)
			educationStore.update(state => ({
				...state,
				modules: sampleModules,
			}));
			await loadModules();
		} catch (e: any) {
			console.error('Error loading modules:', e);
		} finally {
			loading = false;
		}
	});
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

	{#if loading}
		<div class="loading">
			<p>Loading modules...</p>
		</div>
	{:else}
		<ModuleGrid eras={sampleEras} />
	{/if}
</div>

<style>
	.modules-page {
		max-width: 1400px;
		margin: 0 auto;
		padding: 32px 24px;
	}

	.page-header {
		text-align: center;
		margin-bottom: 48px;
	}

	.page-header h1 {
		font-size: 40px;
		font-weight: 700;
		color: #2c3e50;
		margin-bottom: 16px;
	}

	.page-description {
		font-size: 16px;
		color: #666;
		line-height: 1.6;
		max-width: 800px;
		margin: 0 auto;
	}

	.loading {
		text-align: center;
		padding: 60px 20px;
		background: white;
		border-radius: 8px;
		font-size: 16px;
		color: #999;
	}
</style>
