<script lang="ts">
	// Ancient Compute - Timeline Page
	import { onMount } from 'svelte';
	import { timelineApi } from '$lib/api';
	import type { TimelineEvent } from '$lib/api';

	let events: TimelineEvent[] = [];
	let loading = true;
	let error = '';

	onMount(async () => {
		try {
			const data = await timelineApi.list();
			events = data.timeline || [];
		} catch (e: any) {
			error = e.message || 'Failed to load timeline';
			console.error('Error loading timeline:', e);
		} finally {
			loading = false;
		}
	});
</script>

<svelte:head>
	<title>Historical Timeline - Ancient Compute</title>
</svelte:head>

<div class="timeline-page">
	<header class="page-header">
		<h1>Computational History Timeline</h1>
		<p class="page-description">
			An interactive journey through 12,500 years of human innovation in
			computation, logic, and symbolic reasoning. From prehistoric counting
			to modern dependent type theory.
		</p>
	</header>

	<div class="timeline-visualization">
		<div class="coming-soon">
			<h2>Interactive Timeline Coming Soon</h2>
			<p>
				This page will feature an interactive D3.js visualization showing
				the chronological progression of computational concepts across
				civilizations.
			</p>
		</div>
	</div>

	<div class="timeline-content">
		{#if loading}
			<div class="loading">
				<p>Loading timeline...</p>
			</div>
		{:else if error}
			<div class="error">
				<p>{error}</p>
			</div>
		{:else if events.length > 0}
			<div class="events-list">
				<h2>Historical Events</h2>
				{#each events as event}
					<article class="event-card">
						<div class="event-year">
							{event.year < 0 ? `${Math.abs(event.year)} BC` : `${event.year} AD`}
						</div>
						<div class="event-content">
							<h3>{event.title}</h3>
							<p>{event.description}</p>
							{#if event.civilization}
								<span class="civilization-badge">{event.civilization}</span>
							{/if}
						</div>
					</article>
				{/each}
			</div>
		{:else}
			<div class="empty">
				<p>No timeline events available yet.</p>
			</div>
		{/if}
	</div>
</div>

<style>
	.timeline-page {
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

	.timeline-visualization {
		background: white;
		border-radius: 8px;
		padding: 3rem;
		margin-bottom: 3rem;
		box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
	}

	.coming-soon {
		text-align: center;
		color: #666;
	}

	.coming-soon h2 {
		color: #667eea;
		margin-bottom: 1rem;
	}

	.events-list h2 {
		font-size: 2rem;
		color: #1a1a2e;
		margin-bottom: 2rem;
	}

	.event-card {
		display: flex;
		gap: 2rem;
		padding: 1.5rem;
		background: white;
		border-left: 4px solid #667eea;
		border-radius: 4px;
		margin-bottom: 1.5rem;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
	}

	.event-year {
		flex-shrink: 0;
		width: 120px;
		font-weight: 700;
		font-size: 1.1rem;
		color: #667eea;
	}

	.event-content {
		flex: 1;
	}

	.event-content h3 {
		margin: 0 0 0.5rem 0;
		color: #1a1a2e;
		font-size: 1.25rem;
	}

	.event-content p {
		margin: 0 0 0.75rem 0;
		color: #555;
		line-height: 1.5;
	}

	.civilization-badge {
		display: inline-block;
		background: #f0f0f0;
		color: #666;
		padding: 0.25rem 0.75rem;
		border-radius: 12px;
		font-size: 0.875rem;
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

	@media (max-width: 768px) {
		.event-card {
			flex-direction: column;
			gap: 0.5rem;
		}

		.event-year {
			width: auto;
		}
	}
</style>
