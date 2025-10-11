<script lang="ts">
	// Ancient Compute - Root Layout
	import Header from '$lib/components/common/Header.svelte';
	import Navigation from '$lib/components/common/Navigation.svelte';
</script>

<div class="app">
	<Header />
	<Navigation />

	<main>
		<slot />
	</main>

	<footer>
		<p>
			From Prehistoric Tally Marks to Modern Dependent Type Theory
		</p>
		<p class="footer-links">
			<a href="https://github.com/ancient-compute/ancient-compute" target="_blank" rel="noopener">GitHub</a>
			<span class="separator">|</span>
			<a href="/docs" target="_blank">Documentation</a>
		</p>
	</footer>
</div>

<style>
	:global(body) {
		margin: 0;
		padding: 0;
		font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
		background: #f5f5f5;
		color: #1a1a2e;
	}

	.app {
		display: flex;
		flex-direction: column;
		min-height: 100vh;
	}

	main {
		flex: 1;
		padding: 2rem;
		max-width: 1200px;
		margin: 0 auto;
		width: 100%;
	}

	footer {
		background: #16213e;
		color: #eee;
		padding: 1.5rem 2rem;
		text-align: center;
		font-size: 0.875rem;
		margin-top: 3rem;
	}

	footer p {
		margin: 0.5rem 0;
	}

	.footer-links {
		margin-top: 0.75rem;
	}

	.footer-links a {
		color: #667eea;
		text-decoration: none;
		transition: color 0.2s;
	}

	.footer-links a:hover {
		color: #8a9ff5;
	}

	.separator {
		margin: 0 0.75rem;
		opacity: 0.5;
	}
</style>
