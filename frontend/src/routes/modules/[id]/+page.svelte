<script lang="ts">
	/**
	 * Module Detail Page
	 * Displays module overview with lessons and exercises
	 */
	import { page } from '$app/stores';
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import Module from '$lib/components/education/Module.svelte';
	import { selectModule } from '$lib/stores/timelineStore';
	import { sampleModules } from '$lib/data/sampleModules';

	let moduleId: string;
	let loading = true;

	$: moduleId = $page.params.id;

	onMount(() => {
		// Load module data
		const module = sampleModules.find(m => m.id === moduleId);
		if (module) {
			selectModule(moduleId);
		}
		loading = false;
	});
</script>

<svelte:head>
	<title>Module - Ancient Compute</title>
</svelte:head>

<div class="module-page">
	{#if loading}
		<div class="loading">Loading module...</div>
	{:else}
		<Module />
	{/if}
</div>

<style>
	.module-page {
		max-width: 1200px;
		margin: 0 auto;
		padding: 24px;
	}

	.loading {
		text-align: center;
		padding: 60px 20px;
		font-size: 16px;
		color: #999;
	}
</style>
