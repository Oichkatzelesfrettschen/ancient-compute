<script lang="ts">
	import EmulatorControl from '$lib/components/emulator/EmulatorControl.svelte';
	import EmulatorState from '$lib/components/emulator/EmulatorState.svelte';
	import ResultsDisplay from '$lib/components/emulator/ResultsDisplay.svelte';
	import DebuggerPanel from '$lib/components/emulator/DebuggerPanel.svelte';
	import type { MachineState, ExecutionResult, DebuggerVariable } from '$lib/api/types';

	// Page state
	let machineState: MachineState | null = null;
	let executionResults: ExecutionResult[] = [];
	let debuggerVariables: Record<string, DebuggerVariable> = {};
	let isPaused: boolean = false;
	let moldProgress: number = 0;

	/**
	 * Update machine state when results are returned
	 */
	function handleExecutionResults(results: ExecutionResult[]) {
		executionResults = results;
		if (results.length > 0) {
			// Get the last state as current
			const lastResult = results[results.length - 1];
			if (lastResult.state) {
				machineState = lastResult.state;
			}

			// Update mold progress (approximation)
			moldProgress = Math.min(50, results.length);
		}
	}
</script>

<svelte:head>
	<title>Difference Engine No. 2 Emulator - Ancient Compute</title>
	<meta
		name="description"
		content="Interactive emulator for Babbage's Difference Engine No. 2 with debugger and mechanical visualization"
	/>
</svelte:head>

<div class="emulator-page">
	<header class="page-header">
		<h1>Difference Engine No. 2 Emulator</h1>
		<p class="subtitle">
			Designed and implemented by Charles Babbage (1847) | Ada Lovelace Notes (1843)
		</p>
	</header>

	<main class="emulator-main">
		<!-- Left Column: Controls and Input -->
		<aside class="control-sidebar">
			<EmulatorControl
				on:results={(e) => handleExecutionResults(e.detail)}
				on:stateUpdate={(e) => {
					machineState = e.detail;
					isPaused = e.detail.paused;
				}}
			/>
		</aside>

		<!-- Center: Main Content Area -->
		<article class="main-content">
			<!-- Tabs for switching between different views -->
			<div class="view-container">
				<!-- State Display -->
				<section class="view-section">
					<EmulatorState state={machineState} />
				</section>

				<!-- Results Table -->
				<section class="view-section">
					<ResultsDisplay results={executionResults} {moldProgress} />
				</section>
			</div>
		</article>

		<!-- Right Column: Debugger -->
		<aside class="debugger-sidebar">
			<DebuggerPanel
				{isPaused}
				currentState={machineState}
				variables={debuggerVariables}
			/>
		</aside>
	</main>

	<!-- Documentation Section -->
	<section class="documentation">
		<h2>About This Emulator</h2>

		<div class="docs-grid">
			<div class="docs-card">
				<h3>The Difference Engine</h3>
				<p>
					Charles Babbage's Difference Engine No. 2 (1847) was the first general-purpose computing
					machine. It could evaluate polynomial functions using the method of finite differences,
					originally conceived by Johann Müller in the 1700s.
				</p>
				<p>
					This emulator faithfully reproduces the mechanical operation of Babbage's design, simulating
					:
				</p>
				<ul>
					<li>Eight column of 31-digit decimal numbers</li>
					<li>Mechanical addition and carry operations</li>
					<li>Anticipating carriage for efficient propagation</li>
					<li>Shaft-driven timing and coordination</li>
				</ul>
			</div>

			<div class="docs-card">
				<h3>How to Use</h3>
				<ol>
					<li><strong>Define Polynomial</strong>: Enter coefficients [a₀, a₁, a₂, ...] for f(x) = a₀ + a₁x + a₂x² + ...</li>
					<li><strong>Set X Range</strong>: Specify start and end values for x evaluation</li>
					<li><strong>Execute</strong>: Click "Execute Polynomial" to run the computation</li>
					<li><strong>Observe Results</strong>: See both numerical results and mechanical state</li>
					<li><strong>Debug</strong>: Set breakpoints and step through cycles to understand the computation</li>
				</ol>
			</div>

			<div class="docs-card">
				<h3>Mechanical Principles</h3>
				<p>The Difference Engine operates through four main phases:</p>
				<ul>
					<li><strong>Addition</strong>: Columns add with mechanical wheels</li>
					<li><strong>Carry</strong>: Anticipating carriage propagates overflows</li>
					<li><strong>Table</strong>: Result transferred to output register</li>
					<li><strong>Output</strong>: Values printed or stereotyped</li>
				</ul>
				<p>
					Each phase takes one complete rotation of the main shaft (with 65,536 positions per full
					rotation).
				</p>
			</div>

			<div class="docs-card">
				<h3>Ada Lovelace's Contribution</h3>
				<p>
					Ada Lovelace's 1843 "Notes" described how the Difference Engine could be programmed through
					punch cards - creating the first algorithm for machine execution (for Bernoulli numbers).
				</p>
				<p>
					Her insights included concepts that wouldn't be formalized until the 20th century:
					recursion, conditional logic, and the universal programmability of the engine.
				</p>
			</div>

			<div class="docs-card">
				<h3>The Method of Differences</h3>
				<p>
					The Difference Engine exploits the mathematical property that polynomial functions have
					constant differences at some order:
				</p>
				<ul>
					<li>f(x) = 5x² has first differences: 15, 25, 35, 45, ...</li>
					<li>Second differences are constant: 10, 10, 10, ...</li>
				</ul>
				<p>By storing these differences, the engine can compute successive values by simple addition.</p>
			</div>

			<div class="docs-card">
				<h3>Debugging Features</h3>
				<p>The integrated debugger allows you to:</p>
				<ul>
					<li>Set breakpoints at specific cycles or mechanical phases</li>
					<li>Define named variables to track through computation</li>
					<li>Step through execution cycle by cycle</li>
					<li>Inspect column values and carry signals</li>
					<li>View complete execution history</li>
				</ul>
			</div>
		</div>
	</section>

	<!-- Example Polynomials -->
	<section class="examples">
		<h2>Example Polynomials</h2>
		<div class="examples-grid">
			<div class="example">
				<h4>Linear: f(x) = 2x + 1</h4>
				<p><strong>Coefficients:</strong> [1, 2]</p>
				<p><strong>x ∈ [1, 5]:</strong> 3, 5, 7, 9, 11</p>
			</div>
			<div class="example">
				<h4>Quadratic: f(x) = x² + 1</h4>
				<p><strong>Coefficients:</strong> [1, 0, 1]</p>
				<p><strong>x ∈ [1, 5]:</strong> 2, 5, 10, 17, 26</p>
			</div>
			<div class="example">
				<h4>Cubic: f(x) = x³</h4>
				<p><strong>Coefficients:</strong> [0, 0, 0, 1]</p>
				<p><strong>x ∈ [1, 5]:</strong> 1, 8, 27, 64, 125</p>
			</div>
		</div>
	</section>

	<!-- Historical Context -->
	<section class="history">
		<h2>Historical Timeline</h2>
		<div class="timeline">
			<div class="timeline-item">
				<span class="date">1822</span>
				<p>Babbage presents concept of Difference Engine to Royal Society</p>
			</div>
			<div class="timeline-item">
				<span class="date">1823-1834</span>
				<p>First Difference Engine constructed (partial), £17,000 spent</p>
			</div>
			<div class="timeline-item">
				<span class="date">1838-1849</span>
				<p>Design of Difference Engine No. 2 begins (improved, smaller design)</p>
			</div>
			<div class="timeline-item">
				<span class="date">1843</span>
				<p>Ada Lovelace publishes Notes on the Analytical Engine</p>
			</div>
			<div class="timeline-item">
				<span class="date">1877-1910</span>
				<p>Henry Babbage (Charles's son) completes partial Engine</p>
			</div>
			<div class="timeline-item">
				<span class="date">1985-1991</span>
				<p>Doron Swade constructs working DE2 at Science Museum London</p>
			</div>
		</div>
	</section>
</div>

<style>
	.emulator-page {
		max-width: 1600px;
		margin: 0 auto;
		padding: 2rem;
	}

	.page-header {
		text-align: center;
		margin-bottom: 3rem;
		border-bottom: 3px solid #667eea;
		padding-bottom: 2rem;
	}

	.page-header h1 {
		color: #667eea;
		margin: 0 0 0.5rem 0;
		font-size: 2.5rem;
	}

	.subtitle {
		color: #b0b8d4;
		margin: 0;
		font-size: 1rem;
		font-style: italic;
	}

	.emulator-main {
		display: grid;
		grid-template-columns: 1fr 2fr 1fr;
		gap: 2rem;
		margin-bottom: 3rem;
	}

	.control-sidebar,
	.debugger-sidebar {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}

	.main-content {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}

	.view-container {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}

	.view-section {
		width: 100%;
	}

	/* Documentation */
	.documentation {
		margin-bottom: 3rem;
		padding: 2rem;
		background: #1a1a2e;
		border: 1px solid #0f3460;
		border-radius: 8px;
	}

	.documentation h2 {
		color: #667eea;
		text-align: center;
		margin: 0 0 2rem 0;
		font-size: 1.8rem;
	}

	.docs-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
		gap: 1.5rem;
	}

	.docs-card {
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 1.5rem;
		transition: all 0.3s ease;
	}

	.docs-card:hover {
		border-color: #667eea;
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.2);
	}

	.docs-card h3 {
		color: #667eea;
		margin: 0 0 1rem 0;
		font-size: 1.2rem;
	}

	.docs-card p {
		color: #b0b8d4;
		margin: 0.5rem 0;
		font-size: 0.9rem;
		line-height: 1.6;
	}

	.docs-card ul,
	.docs-card ol {
		color: #b0b8d4;
		margin: 0.75rem 0;
		padding-left: 1.5rem;
		font-size: 0.9rem;
		line-height: 1.6;
	}

	.docs-card li {
		margin: 0.5rem 0;
	}

	/* Examples */
	.examples {
		margin-bottom: 3rem;
		padding: 2rem;
		background: #1a1a2e;
		border: 1px solid #0f3460;
		border-radius: 8px;
	}

	.examples h2 {
		color: #667eea;
		text-align: center;
		margin: 0 0 2rem 0;
		font-size: 1.8rem;
	}

	.examples-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
		gap: 1.5rem;
	}

	.example {
		background: #16213e;
		border: 1px solid #533483;
		border-radius: 8px;
		padding: 1.5rem;
		text-align: center;
	}

	.example h4 {
		color: #667eea;
		margin: 0 0 1rem 0;
		font-size: 1.1rem;
		font-family: 'Courier New', monospace;
	}

	.example p {
		color: #b0b8d4;
		margin: 0.5rem 0;
		font-size: 0.9rem;
	}

	/* Timeline */
	.history {
		padding: 2rem;
		background: #1a1a2e;
		border: 1px solid #0f3460;
		border-radius: 8px;
	}

	.history h2 {
		color: #667eea;
		text-align: center;
		margin: 0 0 2rem 0;
		font-size: 1.8rem;
	}

	.timeline {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1.5rem;
	}

	.timeline-item {
		background: #16213e;
		border-left: 3px solid #667eea;
		border-radius: 6px;
		padding: 1.5rem;
	}

	.timeline-item .date {
		display: block;
		color: #667eea;
		font-size: 1.3rem;
		font-weight: 700;
		margin-bottom: 0.5rem;
	}

	.timeline-item p {
		color: #b0b8d4;
		margin: 0;
		font-size: 0.9rem;
		line-height: 1.5;
	}

	/* Responsive */
	@media (max-width: 1200px) {
		.emulator-main {
			grid-template-columns: 1fr;
		}

		.control-sidebar,
		.debugger-sidebar {
			grid-column: auto;
		}
	}

	@media (max-width: 768px) {
		.emulator-page {
			padding: 1rem;
		}

		.page-header h1 {
			font-size: 1.8rem;
		}

		.docs-grid,
		.examples-grid,
		.timeline {
			grid-template-columns: 1fr;
		}

		.subtitle {
			font-size: 0.9rem;
		}
	}
</style>
