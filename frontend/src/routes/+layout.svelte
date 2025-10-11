<script lang="ts">
	// Ancient Compute - Root Layout
</script>

<div class="app">
	<header>
		<nav>
			<h1>Ancient Compute</h1>
			<p>12,500 Years of Computational History</p>
		</nav>
	</header>

	<main>
		<slot />
	</main>

	<footer>
		<p>
			From Prehistoric Tally Marks to Modern Dependent Type Theory
		</p>
	</footer>
</div>

<style>
	.app {
		display: flex;
		flex-direction: column;
		min-height: 100vh;
	}

	header {
		background: #1a1a2e;
		color: #eee;
		padding: 1rem 2rem;
		border-bottom: 2px solid #16213e;
	}

	nav h1 {
		margin: 0;
		font-size: 1.5rem;
	}

	nav p {
		margin: 0.25rem 0 0 0;
		font-size: 0.875rem;
		opacity: 0.8;
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
		padding: 1rem 2rem;
		text-align: center;
		font-size: 0.875rem;
	}
</style>
