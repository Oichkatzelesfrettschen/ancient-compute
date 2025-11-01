<script lang="ts">
	import EmulatorView from '$lib/components/visualization/EmulatorView.svelte';
</script>

<svelte:head>
	<title>3D Mechanical Visualization - Difference Engine Emulator</title>
	<meta
		name="description"
		content="Interactive 3D visualization of Babbage's Difference Engine No. 2 with real-time mechanical animation"
	/>
</svelte:head>

<div class="visualization-page">
	<header class="page-header">
		<div class="header-content">
			<h1>Difference Engine No. 2 - 3D Visualization</h1>
			<p class="subtitle">Real-time mechanical simulation</p>
		</div>
		<nav class="breadcrumb">
			<a href="/emulator">Procedural Emulator</a>
			<span class="separator">→</span>
			<span class="current">3D Visualization</span>
		</nav>
	</header>

	<main class="visualization-container">
		<EmulatorView />
	</main>

	<footer class="page-footer">
		<p>
			Phase 4.W3: Interactive 3D visualization with state synchronization | Powered by Three.js WebGL
			rendering
		</p>
	</footer>
</div>

<style>
	.visualization-page {
		display: flex;
		flex-direction: column;
		height: 100vh;
		width: 100%;
		background: #0f1419;
		color: #b0b8d4;
	}

	.page-header {
		background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%);
		border-bottom: 3px solid #667eea;
		padding: 1.5rem 2rem;
		box-shadow: 0 4px 12px rgba(0, 0, 0, 0.5);
	}

	.header-content {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.page-header h1 {
		color: #667eea;
		margin: 0;
		font-size: 1.8rem;
		font-weight: 700;
		letter-spacing: -0.5px;
	}

	.subtitle {
		color: #b0b8d4;
		margin: 0;
		font-size: 0.95rem;
		font-style: italic;
		opacity: 0.8;
	}

	.breadcrumb {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		margin-top: 0.75rem;
		font-size: 0.85rem;
	}

	.breadcrumb a {
		color: #667eea;
		text-decoration: none;
		transition: color 0.2s ease;
	}

	.breadcrumb a:hover {
		color: #7c8ef5;
		text-decoration: underline;
	}

	.separator {
		color: #533483;
		opacity: 0.5;
	}

	.current {
		color: #b0b8d4;
		opacity: 0.8;
	}

	.visualization-container {
		flex: 1;
		display: flex;
		overflow: hidden;
		padding: 1rem;
		gap: 1rem;
	}

	.page-footer {
		background: #16213e;
		border-top: 1px solid #0f3460;
		padding: 1rem 2rem;
		text-align: center;
		font-size: 0.8rem;
		color: #b0b8d4;
	}

	.page-footer p {
		margin: 0;
		opacity: 0.7;
	}

	/* Responsive design */
	@media (max-width: 768px) {
		.page-header {
			padding: 1rem 1.5rem;
		}

		.page-header h1 {
			font-size: 1.4rem;
		}

		.breadcrumb {
			display: none;
		}

		.visualization-container {
			padding: 0.75rem;
		}

		.page-footer {
			padding: 0.75rem 1.5rem;
			font-size: 0.75rem;
		}
	}
</style>
