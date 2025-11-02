<script lang="ts">
	// Ancient Compute - Navigation Component
	import { page } from '$app/stores';

	interface NavItem {
		label: string;
		path: string;
		description?: string;
	}

	const navItems: NavItem[] = [
		{ label: 'Home', path: '/', description: 'Welcome page' },
		{ label: 'Modules', path: '/modules', description: 'Browse learning modules' },
		{ label: 'Timeline', path: '/timeline', description: 'Interactive historical timeline' },
		{ label: 'MINIX Metrics', path: '/infra/minix', description: 'MINIX in QEMU metrics' },
		{ label: 'About', path: '/about', description: 'About this project' }
	];

	function isActive(path: string): boolean {
		return $page.url.pathname === path;
	}
</script>

<nav class="main-navigation">
	<ul class="nav-list">
		{#each navItems as item}
			<li class="nav-item">
				<a
					href={item.path}
					class="nav-link"
					class:active={isActive(item.path)}
					title={item.description}
				>
					{item.label}
				</a>
			</li>
		{/each}
	</ul>
</nav>

<style>
	.main-navigation {
		background: #f8f9fa;
		border-bottom: 1px solid #e0e0e0;
		padding: 0.75rem 0;
	}

	.nav-list {
		list-style: none;
		margin: 0;
		padding: 0;
		display: flex;
		justify-content: center;
		gap: 0.5rem;
		max-width: 1200px;
		margin: 0 auto;
		padding: 0 2rem;
	}

	.nav-item {
		margin: 0;
	}

	.nav-link {
		display: block;
		padding: 0.5rem 1rem;
		color: #1a1a2e;
		text-decoration: none;
		border-radius: 4px;
		transition: all 0.2s;
		font-weight: 500;
	}

	.nav-link:hover {
		background: #667eea;
		color: white;
	}

	.nav-link.active {
		background: #667eea;
		color: white;
		box-shadow: 0 2px 4px rgba(102, 126, 234, 0.3);
	}

	@media (max-width: 768px) {
		.nav-list {
			flex-wrap: wrap;
			padding: 0 1rem;
		}

		.nav-link {
			padding: 0.4rem 0.75rem;
			font-size: 0.9rem;
		}
	}
</style>
