<script lang="ts">
	// Ancient Compute - Landing Page
	import { onMount } from 'svelte';
	import { moduleApi } from '$lib/api';
	import type { Module } from '$lib/api';

	let modules: Module[] = [];
	let loading = true;
	let error = '';

	onMount(async () => {
		try {
			const data = await moduleApi.list();
			modules = data.modules || [];
		} catch (e: any) {
			error = e.message || 'Error connecting to backend';
			console.error('Failed to load modules:', e);
		} finally {
			loading = false;
		}
	});
</script>

<svelte:head>
	<title>Ancient Compute - 12,500 Years of Computational History</title>
</svelte:head>

<div class="landing">
	<section class="hero">
		<h1>Welcome to Ancient Compute</h1>
		<p class="subtitle">
			An educational journey through 12,500 years of computational history
		</p>
		<p class="description">
			From prehistoric tally marks and Babylonian algorithms to Greek logic,
			lambda calculus, and modern dependent type theory - discover how humanity's
			quest to formalize thought has evolved across civilizations and millennia.
		</p>
	</section>

	<section class="features">
		<h2>What You'll Learn</h2>
		<div class="feature-grid">
			<div class="feature">
				<h3>Historical Foundations</h3>
				<p>Trace computation from Ishango bone (20,000 BC) through modern type systems</p>
			</div>
			<div class="feature">
				<h3>Cross-Cultural Perspectives</h3>
				<p>Babylonian, Greek, Indian, Islamic, and Chinese contributions</p>
			</div>
			<div class="feature">
				<h3>Type Theory Evolution</h3>
				<p>From untyped to dependent types across 8 programming languages</p>
			</div>
			<div class="feature">
				<h3>Interactive Learning</h3>
				<p>Execute code in C, Python, Haskell, IDRIS2, LISP, Assembly, Java, System F</p>
			</div>
		</div>
	</section>

	<section class="modules">
		<h2>Learning Modules</h2>
		{#if loading}
			<p>Loading modules...</p>
		{:else if error}
			<p class="error">{error}</p>
		{:else if modules.length > 0}
			<div class="module-list">
				{#each modules as module}
					<div class="module-card">
						<h3>{module.title}</h3>
						<p>{module.description}</p>
					</div>
				{/each}
			</div>
		{:else}
			<p>No modules available yet. Stay tuned!</p>
		{/if}
	</section>
</div>

<style>
	.landing {
		max-width: 1000px;
	}

	.hero {
		text-align: center;
		margin-bottom: 3rem;
		padding: 2rem;
		background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
		color: white;
		border-radius: 8px;
	}

	.hero h1 {
		font-size: 2.5rem;
		margin-bottom: 1rem;
	}

	.subtitle {
		font-size: 1.25rem;
		margin-bottom: 1.5rem;
		opacity: 0.95;
	}

	.description {
		font-size: 1rem;
		line-height: 1.6;
		max-width: 800px;
		margin: 0 auto;
	}

	.features,
	.modules {
		margin: 3rem 0;
	}

	h2 {
		font-size: 2rem;
		margin-bottom: 1.5rem;
		color: #1a1a2e;
	}

	.feature-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
		gap: 1.5rem;
	}

	.feature {
		padding: 1.5rem;
		background: #f8f9fa;
		border-radius: 8px;
		border-left: 4px solid #667eea;
	}

	.feature h3 {
		margin-top: 0;
		color: #667eea;
	}

	.module-list {
		display: grid;
		gap: 1rem;
	}

	.module-card {
		padding: 1.5rem;
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 8px;
		transition: box-shadow 0.2s;
	}

	.module-card:hover {
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.1);
	}

	.module-card h3 {
		margin-top: 0;
		color: #1a1a2e;
	}

	.error {
		color: #d32f2f;
		padding: 1rem;
		background: #ffebee;
		border-radius: 4px;
	}
</style>
