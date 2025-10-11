<script lang="ts">
	// Ancient Compute - About Page
</script>

<svelte:head>
	<title>About - Ancient Compute</title>
</svelte:head>

<div class="about-page">
	<header class="page-header">
		<h1>About Ancient Compute</h1>
		<p class="subtitle">Teaching 12,500 Years of Computational History</p>
	</header>

	<section class="content-section">
		<h2>Mission</h2>
		<p>
			Ancient Compute is an educational platform designed to teach the complete
			history of computation and logic, from prehistoric counting systems to
			modern dependent type theory. We believe understanding this history is
			essential for anyone working with computation today.
		</p>
	</section>

	<section class="content-section">
		<h2>What You'll Learn</h2>
		<ul>
			<li>
				<strong>Historical Foundations:</strong> Trace computational thought
				from the Ishango bone (20,000 BC) through Babylonian algorithms,
				Greek logic, and beyond.
			</li>
			<li>
				<strong>Cross-Cultural Perspectives:</strong> Explore contributions
				from Babylonian, Greek, Indian, Islamic, and Chinese civilizations.
			</li>
			<li>
				<strong>Type Theory Evolution:</strong> Follow the progression from
				untyped systems to dependent types across multiple programming languages.
			</li>
			<li>
				<strong>Interactive Learning:</strong> Execute code in 8 languages
				including C, Python, Haskell, IDRIS2, LISP, Assembly, Java, and System F.
			</li>
		</ul>
	</section>

	<section class="content-section">
		<h2>Technology Stack</h2>
		<div class="tech-grid">
			<div class="tech-item">
				<h3>Frontend</h3>
				<p>SvelteKit with TypeScript, D3.js for visualizations, Monaco Editor</p>
			</div>
			<div class="tech-item">
				<h3>Backend</h3>
				<p>FastAPI with Python, PostgreSQL, Redis, SQLAlchemy</p>
			</div>
			<div class="tech-item">
				<h3>Language Services</h3>
				<p>Docker-isolated execution environments for 8 programming languages</p>
			</div>
			<div class="tech-item">
				<h3>Build System</h3>
				<p>Bazel for hermetic polyglot builds across all languages</p>
			</div>
		</div>
	</section>

	<section class="content-section">
		<h2>Security</h2>
		<p>
			All code execution happens in Docker containers with 5-layer security:
		</p>
		<ol>
			<li>Docker container isolation</li>
			<li>gVisor runtime for kernel-level security</li>
			<li>Seccomp-bpf syscall filtering</li>
			<li>Cgroups resource limits</li>
			<li>Read-only filesystems</li>
		</ol>
	</section>

	<section class="content-section">
		<h2>Open Source</h2>
		<p>
			Ancient Compute is an open-source project. Contributions are welcome!
			Visit our GitHub repository to learn more about contributing.
		</p>
		<a href="https://github.com/ancient-compute/ancient-compute" target="_blank" rel="noopener" class="github-link">
			View on GitHub →
		</a>
	</section>
</div>

<style>
	.about-page {
		max-width: 800px;
		margin: 0 auto;
	}

	.page-header {
		text-align: center;
		margin-bottom: 3rem;
	}

	.page-header h1 {
		font-size: 2.5rem;
		color: #1a1a2e;
		margin-bottom: 0.5rem;
	}

	.subtitle {
		font-size: 1.25rem;
		color: #667eea;
		font-weight: 600;
	}

	.content-section {
		background: white;
		padding: 2rem;
		border-radius: 8px;
		margin-bottom: 2rem;
		box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
	}

	.content-section h2 {
		color: #1a1a2e;
		margin-top: 0;
		margin-bottom: 1rem;
		font-size: 1.75rem;
	}

	.content-section p,
	.content-section li {
		color: #555;
		line-height: 1.7;
		font-size: 1.05rem;
	}

	.content-section ul,
	.content-section ol {
		margin: 1rem 0;
		padding-left: 1.5rem;
	}

	.content-section li {
		margin-bottom: 0.75rem;
	}

	.tech-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1.5rem;
		margin-top: 1.5rem;
	}

	.tech-item {
		padding: 1rem;
		background: #f8f9fa;
		border-radius: 8px;
		border-left: 4px solid #667eea;
	}

	.tech-item h3 {
		margin: 0 0 0.5rem 0;
		color: #667eea;
		font-size: 1.1rem;
	}

	.tech-item p {
		margin: 0;
		font-size: 0.95rem;
	}

	.github-link {
		display: inline-block;
		margin-top: 1rem;
		padding: 0.75rem 1.5rem;
		background: #667eea;
		color: white;
		text-decoration: none;
		border-radius: 4px;
		font-weight: 600;
		transition: background 0.2s;
	}

	.github-link:hover {
		background: #4c5ed0;
	}
</style>
