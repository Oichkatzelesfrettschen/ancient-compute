<script lang="ts">
	import type { ExecutionResult } from '$lib/api/types';

	export let results: ExecutionResult[] = [];
	export let moldProgress: number = 0;

	let exportFormat: 'csv' | 'json' = 'csv';

	/**
	 * Format a number as 8-digit result (Babbage-style output)
	 */
	function formatResult(value: number): string {
		return Math.abs(value).toString().padStart(8, '0');
	}

	/**
	 * Get CSS class for result value (for coloring)
	 */
	function getResultClass(value: number): string {
		if (value < 0) return 'negative';
		if (value === 0) return 'zero';
		return 'positive';
	}

	/**
	 * Export results as CSV
	 */
	function exportAsCSV() {
		const headers = ['x', 'f(x)', 'cycle', 'phase'];
		const rows = results.map((r) => [r.x, r.result, r.cycle, r.phase]);

		const csv = [
			headers.join(','),
			...rows.map((row) => row.join(','))
		].join('\n');

		downloadFile(csv, 'results.csv', 'text/csv');
	}

	/**
	 * Export results as JSON
	 */
	function exportAsJSON() {
		const json = JSON.stringify(results, null, 2);
		downloadFile(json, 'results.json', 'application/json');
	}

	/**
	 * Trigger file download
	 */
	function downloadFile(content: string, filename: string, mimeType: string) {
		const blob = new Blob([content], { type: mimeType });
		const url = URL.createObjectURL(blob);
		const link = document.createElement('a');
		link.href = url;
		link.download = filename;
		document.body.appendChild(link);
		link.click();
		document.body.removeChild(link);
		URL.revokeObjectURL(url);
	}

	/**
	 * Handle export button click
	 */
	function handleExport() {
		if (exportFormat === 'csv') {
			exportAsCSV();
		} else {
			exportAsJSON();
		}
	}

	/**
	 * Get polynomial degree from results
	 */
	function estimatePolynomialDegree(): number {
		if (results.length < 3) return 0;

		// Analyze pattern of differences to estimate degree
		const differences: number[] = [];
		for (let i = 1; i < results.length; i++) {
			differences.push(results[i].result - results[i - 1].result);
		}

		let degree = 1;
		let current = differences;
		while (degree < 5 && current.length > 1) {
			const next: number[] = [];
			let isConstant = true;
			for (let i = 1; i < current.length; i++) {
				const diff = current[i] - current[i - 1];
				next.push(diff);
				if (diff !== next[0]) isConstant = false;
			}
			if (isConstant && next.length > 0) {
				return degree + 1;
			}
			current = next;
			degree++;
		}

		return degree;
	}
</script>

<div class="results-display">
	{#if results.length === 0}
		<div class="empty-state">
			<p>No results yet. Execute a polynomial to see results.</p>
		</div>
	{:else}
		<div class="results-header">
			<h3>Polynomial Evaluation Results</h3>
			<div class="result-stats">
				<span>Values: {results.length}</span>
				<span>Degree: ≈{estimatePolynomialDegree()}</span>
			</div>
		</div>

		<!-- Results Table -->
		<div class="results-table-container">
			<table class="results-table">
				<thead>
					<tr>
						<th>x</th>
						<th>f(x)</th>
						<th>Cycle</th>
						<th>Phase</th>
					</tr>
				</thead>
				<tbody>
					{#each results as result (result.x)}
						<tr>
							<td class="x-value">{result.x}</td>
							<td class={`result-value ${getResultClass(result.result)}`}>
								{formatResult(result.result)}
							</td>
							<td class="cycle-value">{result.cycle}</td>
							<td class="phase-value">{result.phase}</td>
						</tr>
					{/each}
				</tbody>
			</table>
		</div>

		<!-- Print Preview -->
		<div class="print-preview-section">
			<h4>Print Preview (Babbage-Style Output)</h4>
			<div class="print-preview">
				{#each results as result, index}
					<div class="print-line">
						<span class="print-number">{formatResult(result.result)}</span>
						{#if (index + 1) % 5 === 0}
							<span class="print-separator">|</span>
						{/if}
					</div>
				{/each}
			</div>
		</div>

		<!-- Stereotype Mold Progress -->
		{#if moldProgress > 0}
			<div class="mold-section">
				<h4>Stereotype Mold Progress</h4>
				<div class="mold-info">
					<p>Mold filling: {moldProgress}/50 rows</p>
				</div>
				<div class="mold-progress-bar">
					<div class="progress" style="width: {(moldProgress / 50) * 100}%"></div>
				</div>
				{#if moldProgress === 50}
					<p class="mold-full">Mold is full and ready for extraction</p>
				{/if}
			</div>
		{/if}

		<!-- Export Section -->
		<div class="export-section">
			<h4>Export Results</h4>
			<div class="export-controls">
				<select bind:value={exportFormat}>
					<option value="csv">CSV Format</option>
					<option value="json">JSON Format</option>
				</select>
				<button on:click={handleExport} class="export-btn">
					Download {exportFormat.toUpperCase()}
				</button>
			</div>
		</div>

		<!-- Summary Statistics -->
		<div class="statistics-section">
			<h4>Statistics</h4>
			<div class="stats-grid">
				<div class="stat-item">
					<span class="stat-label">Minimum</span>
					<span class="stat-value">{Math.min(...results.map((r) => r.result))}</span>
				</div>
				<div class="stat-item">
					<span class="stat-label">Maximum</span>
					<span class="stat-value">{Math.max(...results.map((r) => r.result))}</span>
				</div>
				<div class="stat-item">
					<span class="stat-label">Average</span>
					<span class="stat-value">
						{Math.round(
							(results.reduce((sum, r) => sum + r.result, 0) / results.length) * 100
						) / 100}
					</span>
				</div>
				<div class="stat-item">
					<span class="stat-label">Total Cycles</span>
					<span class="stat-value">{Math.max(...results.map((r) => r.cycle))}</span>
				</div>
			</div>
		</div>
	{/if}
</div>

<style>
	.results-display {
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
	}

	.empty-state {
		text-align: center;
		padding: 3rem 1rem;
		color: #b0b8d4;
	}

	.empty-state p {
		margin: 0;
		font-size: 0.95rem;
		opacity: 0.8;
	}

	.results-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 1.5rem;
		flex-wrap: wrap;
		gap: 1rem;
	}

	.results-header h3 {
		color: #667eea;
		margin: 0;
		font-size: 1.2rem;
	}

	.result-stats {
		display: flex;
		gap: 1.5rem;
		font-size: 0.9rem;
		color: #b0b8d4;
	}

	.result-stats span {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.results-table-container {
		overflow-x: auto;
		margin-bottom: 2rem;
		border: 1px solid #0f3460;
		border-radius: 6px;
	}

	.results-table {
		width: 100%;
		border-collapse: collapse;
		background: rgba(15, 52, 96, 0.5);
	}

	.results-table thead {
		background: rgba(102, 126, 234, 0.1);
		border-bottom: 2px solid #667eea;
	}

	.results-table th {
		padding: 0.75rem;
		text-align: left;
		color: #667eea;
		font-weight: 600;
		font-size: 0.9rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.results-table td {
		padding: 0.75rem;
		border-bottom: 1px solid #0f3460;
		color: #e0e0e0;
		font-size: 0.9rem;
	}

	.results-table tbody tr:hover {
		background: rgba(102, 126, 234, 0.1);
	}

	.x-value {
		font-weight: 600;
		color: #667eea;
	}

	.result-value {
		font-family: 'Courier New', monospace;
		font-weight: 600;
		letter-spacing: 0.05em;
	}

	.result-value.positive {
		color: #b0e0c0;
	}

	.result-value.negative {
		color: #f0b0c0;
	}

	.result-value.zero {
		color: #b0b8d4;
	}

	.cycle-value {
		color: #b0b8d4;
		text-align: center;
	}

	.phase-value {
		color: #f0d080;
		text-align: center;
		font-size: 0.85rem;
	}

	.print-preview-section {
		margin-bottom: 2rem;
		padding-top: 1.5rem;
		border-top: 1px solid #0f3460;
	}

	.print-preview-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.print-preview {
		background: rgba(15, 52, 96, 0.3);
		border: 2px solid #533483;
		border-radius: 6px;
		padding: 1.5rem;
		font-family: 'Courier New', monospace;
		line-height: 1.8;
		color: #b0b8d4;
		overflow-x: auto;
	}

	.print-line {
		display: inline-block;
		margin-right: 1rem;
		margin-bottom: 0.5rem;
	}

	.print-number {
		color: #667eea;
		font-weight: 600;
		font-size: 0.95rem;
		letter-spacing: 0.1em;
	}

	.print-separator {
		margin: 0 0.75rem;
		color: #533483;
	}

	.mold-section {
		margin-bottom: 2rem;
		padding-top: 1.5rem;
		border-top: 1px solid #0f3460;
	}

	.mold-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.mold-info {
		margin-bottom: 0.75rem;
		color: #b0b8d4;
	}

	.mold-info p {
		margin: 0;
		font-size: 0.9rem;
	}

	.mold-progress-bar {
		width: 100%;
		height: 24px;
		background: rgba(15, 52, 96, 0.5);
		border: 1px solid #533483;
		border-radius: 4px;
		overflow: hidden;
	}

	.progress {
		height: 100%;
		background: linear-gradient(90deg, #667eea, #7a8fee);
		transition: width 0.3s ease;
	}

	.mold-full {
		margin: 1rem 0 0 0;
		color: #b0e0c0;
		font-weight: 600;
		padding: 0.75rem;
		background: rgba(45, 90, 61, 0.3);
		border-left: 3px solid #4a8a5e;
		border-radius: 4px;
	}

	.export-section {
		margin-bottom: 2rem;
		padding-top: 1.5rem;
		border-top: 1px solid #0f3460;
	}

	.export-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.export-controls {
		display: flex;
		gap: 1rem;
		flex-wrap: wrap;
	}

	.export-controls select {
		padding: 0.75rem;
		background: #0f3460;
		border: 1px solid #533483;
		border-radius: 4px;
		color: #e0e0e0;
		cursor: pointer;
		font-size: 0.9rem;
	}

	.export-controls select:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 4px rgba(102, 126, 234, 0.3);
	}

	.export-btn {
		padding: 0.75rem 1.5rem;
		background: #667eea;
		color: white;
		border: none;
		border-radius: 4px;
		cursor: pointer;
		font-weight: 600;
		font-size: 0.9rem;
		transition: all 0.2s;
	}

	.export-btn:hover {
		background: #7a8fee;
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}

	.statistics-section {
		padding-top: 1.5rem;
		border-top: 1px solid #0f3460;
	}

	.statistics-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.stats-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
		gap: 1rem;
	}

	.stat-item {
		background: rgba(15, 52, 96, 0.5);
		border: 1px solid #533483;
		border-radius: 6px;
		padding: 1rem;
		text-align: center;
	}

	.stat-label {
		display: block;
		color: #b0b8d4;
		font-size: 0.85rem;
		font-weight: 500;
		margin-bottom: 0.5rem;
		text-transform: uppercase;
		letter-spacing: 0.3px;
	}

	.stat-value {
		display: block;
		color: #667eea;
		font-size: 1.3rem;
		font-weight: 700;
		font-family: 'Courier New', monospace;
	}

	@media (max-width: 768px) {
		.results-header {
			flex-direction: column;
			align-items: flex-start;
		}

		.result-stats {
			flex-direction: column;
			gap: 0.5rem;
		}

		.export-controls {
			flex-direction: column;
		}

		.export-controls select,
		.export-btn {
			width: 100%;
		}

		.stats-grid {
			grid-template-columns: repeat(2, 1fr);
		}
	}

	@media (max-width: 600px) {
		.results-display {
			padding: 1rem;
		}

		.print-preview {
			font-size: 0.85rem;
			padding: 1rem;
		}

		.stats-grid {
			grid-template-columns: 1fr;
		}
	}
</style>
