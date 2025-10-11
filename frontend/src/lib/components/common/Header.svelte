<script lang="ts">
	// Ancient Compute - Header Component
	export let title: string = 'Ancient Compute';
	export let subtitle: string = '12,500 Years of Computational History';
</script>

<header class="site-header">
	<div class="header-content">
		<div class="branding">
			<h1><a href="/">{title}</a></h1>
			<p class="subtitle">{subtitle}</p>
		</div>
		<nav class="header-nav">
			<a href="/" class="nav-link">Home</a>
			<a href="/modules" class="nav-link">Modules</a>
			<a href="/timeline" class="nav-link">Timeline</a>
			<a href="/about" class="nav-link">About</a>
		</nav>
	</div>
</header>

<style>
	.site-header {
		background: #1a1a2e;
		color: #eee;
		border-bottom: 2px solid #16213e;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
	}

	.header-content {
		max-width: 1200px;
		margin: 0 auto;
		padding: 1rem 2rem;
		display: flex;
		justify-content: space-between;
		align-items: center;
		flex-wrap: wrap;
		gap: 1rem;
	}

	.branding h1 {
		margin: 0;
		font-size: 1.5rem;
		font-weight: 600;
	}

	.branding h1 a {
		color: #eee;
		text-decoration: none;
		transition: color 0.2s;
	}

	.branding h1 a:hover {
		color: #667eea;
	}

	.subtitle {
		margin: 0.25rem 0 0 0;
		font-size: 0.875rem;
		opacity: 0.8;
	}

	.header-nav {
		display: flex;
		gap: 1.5rem;
		align-items: center;
	}

	.nav-link {
		color: #eee;
		text-decoration: none;
		font-size: 0.95rem;
		padding: 0.5rem 0.75rem;
		border-radius: 4px;
		transition: all 0.2s;
	}

	.nav-link:hover {
		background: rgba(102, 126, 234, 0.2);
		color: #667eea;
	}

	@media (max-width: 768px) {
		.header-content {
			flex-direction: column;
			text-align: center;
		}

		.header-nav {
			width: 100%;
			justify-content: center;
		}
	}
</style>
