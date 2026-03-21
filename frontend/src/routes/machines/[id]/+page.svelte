<script lang="ts">
	import { page } from '$app/stores';
	import { onMount } from 'svelte';
	import {
		machinesApi,
		CATEGORY_LABELS,
		CATEGORY_COLORS,
		type MachineDetail,
		type MachineState
	} from '$lib/api/machines';

	const machineId = $page.params.id;

	let detail: MachineDetail | null = null;
	let state: MachineState | null = null;
	let loadError = '';
	let stepError = '';
	let busy = false;
	let programText = '';
	let manualOpen = false;
	let runMaxSteps = 200;

	onMount(async () => {
		try {
			detail = await machinesApi.get(machineId);
			programText = JSON.stringify(detail.example_payload, null, 2);
			state = await machinesApi.state(machineId);
		} catch (e) {
			loadError = e instanceof Error ? e.message : String(e);
		}
	});

	async function doReset() {
		busy = true;
		stepError = '';
		try {
			state = await machinesApi.reset(machineId);
		} catch (e) {
			stepError = e instanceof Error ? e.message : String(e);
		} finally {
			busy = false;
		}
	}

	async function doLoad() {
		busy = true;
		stepError = '';
		try {
			let payload: Record<string, unknown>;
			try {
				payload = JSON.parse(programText);
			} catch {
				stepError = 'Invalid JSON in program editor';
				return;
			}
			await machinesApi.load(machineId, payload);
			state = await machinesApi.state(machineId);
		} catch (e) {
			stepError = e instanceof Error ? e.message : String(e);
		} finally {
			busy = false;
		}
	}

	async function doStep() {
		busy = true;
		stepError = '';
		try {
			state = await machinesApi.step(machineId);
		} catch (e) {
			stepError = e instanceof Error ? e.message : String(e);
		} finally {
			busy = false;
		}
	}

	async function doRun() {
		busy = true;
		stepError = '';
		try {
			const result = await machinesApi.run(machineId, runMaxSteps);
			state = {
				cycle_count: 0,
				registers: result.registers,
				columns: [],
				snapshot: result.snapshot,
				halted: result.halted
			};
		} catch (e) {
			stepError = e instanceof Error ? e.message : String(e);
		} finally {
			busy = false;
		}
	}

	function formatValue(v: unknown): string {
		if (v === null || v === undefined) return 'null';
		if (typeof v === 'number') {
			return Number.isInteger(v) ? String(v) : v.toFixed(6).replace(/\.?0+$/, '');
		}
		if (typeof v === 'boolean') return v ? 'true' : 'false';
		if (typeof v === 'object') return JSON.stringify(v);
		return String(v);
	}

	function snapshotEntries(snap: unknown): [string, string][] {
		if (!snap || typeof snap !== 'object') return [];
		return Object.entries(snap as Record<string, unknown>).map(([k, v]) => [
			k,
			formatValue(v)
		]);
	}

	$: catColor = detail ? (CATEGORY_COLORS[detail.category] ?? '#374151') : '#667eea';
	$: catLabel = detail ? (CATEGORY_LABELS[detail.category] ?? detail.category) : '';
</script>

<svelte:head>
	<title>{detail?.name ?? machineId} -- Ancient Compute</title>
</svelte:head>

<div class="machine-page">
	{#if loadError}
		<div class="error-banner">Failed to load machine: {loadError}</div>
	{:else if !detail}
		<div class="loading">Loading...</div>
	{:else}
		<!-- ---- Header ---- -->
		<header class="machine-header" style="border-top: 4px solid {catColor}">
			<div class="header-meta">
				<a href="/machines" class="back-link">&larr; Gallery</a>
				<span class="category-badge" style="background: {catColor}">{catLabel}</span>
				<span class="year-chip">{detail.year < 0 ? Math.abs(detail.year) + ' BCE' : detail.year}</span>
			</div>
			<h1 class="machine-title">{detail.name}</h1>
			<p class="inventor-line">{detail.inventor} &mdash; {detail.country}</p>
			<p class="brief">{detail.brief}</p>
		</header>

		<!-- ---- Main layout ---- -->
		<div class="workspace">
			<!-- Left: program editor + controls -->
			<section class="left-panel">
				<div class="panel-section">
					<div class="section-title">
						Program
						<span class="input-type-hint">{detail.program_input_type}</span>
					</div>
					<textarea
						class="program-editor"
						bind:value={programText}
						spellcheck="false"
						rows={12}
						placeholder="Enter program here..."
					></textarea>
					{#if stepError}
						<div class="inline-error">{stepError}</div>
					{/if}
				</div>

				<div class="controls">
					<button class="btn btn-secondary" disabled={busy} on:click={doReset}>Reset</button>
					<button class="btn btn-primary" disabled={busy} on:click={doLoad}>Load</button>
					<button class="btn btn-accent" disabled={busy} on:click={doStep}>Step</button>
					<div class="run-row">
						<button class="btn btn-run" disabled={busy} on:click={doRun}>Run</button>
						<label class="max-steps-label">
							Max steps:
							<input
								type="number"
								class="max-steps-input"
								bind:value={runMaxSteps}
								min={1}
								max={100000}
							/>
						</label>
					</div>
				</div>

				<!-- Manual toggle -->
				<div class="manual-section">
					<button
						class="manual-toggle"
						on:click={() => (manualOpen = !manualOpen)}
					>
						{manualOpen ? 'Hide' : 'Show'} Programming Manual
					</button>
					{#if manualOpen}
						<pre class="manual-text">{detail.manual}</pre>
					{/if}
				</div>
			</section>

			<!-- Right: registers + columns + snapshot -->
			<section class="right-panel">
				{#if state}
					<!-- Status bar -->
					<div class="status-bar">
						<span>Cycles: <strong>{state.cycle_count}</strong></span>
						{#if state.halted}
							<span class="halted-badge">HALTED</span>
						{/if}
					</div>

					<!-- Registers -->
					<div class="panel-section">
						<div class="section-title">Registers</div>
						<table class="register-table">
							<tbody>
								{#each Object.entries(state.registers) as [name, value]}
									<tr>
										<td class="reg-name">{name}</td>
										<td class="reg-value">{formatValue(value)}</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>

					<!-- Column display -->
					{#if state.columns.length > 0}
						<div class="panel-section">
							<div class="section-title">Columns / Dials</div>
							<div class="columns-strip">
								{#each state.columns as val, i}
									<div class="column-cell" title="col {i}">
										<span class="col-val">{val}</span>
										<span class="col-idx">{i}</span>
									</div>
								{/each}
							</div>
						</div>
					{/if}

					<!-- Snapshot -->
					<div class="panel-section">
						<div class="section-title">Snapshot</div>
						<table class="register-table">
							<tbody>
								{#each snapshotEntries(state.snapshot) as [k, v]}
									<tr>
										<td class="reg-name">{k}</td>
										<td class="reg-value snap-val">{v}</td>
									</tr>
								{/each}
							</tbody>
						</table>
					</div>
				{:else}
					<div class="no-state">Load a program and click Step or Run.</div>
				{/if}
			</section>
		</div>
	{/if}
</div>

<style>
	.machine-page {
		max-width: 1200px;
		margin: 0 auto;
		padding: 1.5rem 2rem;
	}

	.loading,
	.no-state {
		text-align: center;
		padding: 3rem;
		color: #888;
	}

	.error-banner {
		padding: 1rem;
		background: #fff0f0;
		border: 1px solid #ffaaaa;
		border-radius: 6px;
		color: #c00;
	}

	/* Header */
	.machine-header {
		background: #fafafa;
		border-radius: 8px;
		padding: 1.25rem 1.5rem;
		margin-bottom: 1.5rem;
		border: 1px solid #e8e8e8;
	}

	.header-meta {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		margin-bottom: 0.6rem;
	}

	.back-link {
		color: #667eea;
		font-size: 0.85rem;
		text-decoration: none;
		font-weight: 600;
	}

	.back-link:hover {
		text-decoration: underline;
	}

	.category-badge {
		display: inline-block;
		padding: 0.2rem 0.6rem;
		border-radius: 12px;
		color: white;
		font-size: 0.75rem;
		font-weight: 600;
	}

	.year-chip {
		font-size: 0.85rem;
		color: #888;
	}

	.machine-title {
		font-size: 1.6rem;
		color: #1a1a2e;
		margin: 0 0 0.2rem;
	}

	.inventor-line {
		color: #666;
		font-size: 0.9rem;
		margin: 0 0 0.4rem;
	}

	.brief {
		color: #444;
		margin: 0;
	}

	/* Workspace layout */
	.workspace {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 1.25rem;
		align-items: start;
	}

	@media (max-width: 768px) {
		.workspace {
			grid-template-columns: 1fr;
		}
	}

	/* Panels */
	.left-panel,
	.right-panel {
		display: flex;
		flex-direction: column;
		gap: 1rem;
	}

	.panel-section {
		background: white;
		border: 1px solid #e8e8e8;
		border-radius: 8px;
		padding: 1rem;
	}

	.section-title {
		font-size: 0.8rem;
		font-weight: 700;
		letter-spacing: 0.05em;
		text-transform: uppercase;
		color: #888;
		margin-bottom: 0.6rem;
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.input-type-hint {
		font-size: 0.7rem;
		font-weight: 400;
		background: #f0f0f0;
		padding: 0.1rem 0.4rem;
		border-radius: 4px;
		color: #555;
		letter-spacing: 0;
		text-transform: none;
	}

	/* Program editor */
	.program-editor {
		width: 100%;
		font-family: 'Courier New', monospace;
		font-size: 0.82rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		padding: 0.6rem;
		resize: vertical;
		box-sizing: border-box;
		background: #fdfdfd;
		color: #1a1a2e;
		line-height: 1.5;
	}

	.program-editor:focus {
		outline: none;
		border-color: #667eea;
	}

	.inline-error {
		margin-top: 0.4rem;
		color: #c00;
		font-size: 0.82rem;
	}

	/* Controls */
	.controls {
		display: flex;
		flex-wrap: wrap;
		gap: 0.5rem;
		align-items: center;
	}

	.run-row {
		display: flex;
		align-items: center;
		gap: 0.4rem;
		flex: 1;
	}

	.btn {
		padding: 0.45rem 1rem;
		border: none;
		border-radius: 5px;
		cursor: pointer;
		font-size: 0.85rem;
		font-weight: 600;
		transition: opacity 0.15s;
	}

	.btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.btn-primary {
		background: #667eea;
		color: white;
	}

	.btn-secondary {
		background: #f0f0f0;
		color: #333;
	}

	.btn-accent {
		background: #1d4ed8;
		color: white;
	}

	.btn-run {
		background: #047857;
		color: white;
	}

	.max-steps-label {
		font-size: 0.8rem;
		color: #666;
		display: flex;
		align-items: center;
		gap: 0.3rem;
	}

	.max-steps-input {
		width: 70px;
		padding: 0.2rem 0.4rem;
		border: 1px solid #ddd;
		border-radius: 4px;
		font-size: 0.8rem;
	}

	/* Manual */
	.manual-section {
		background: white;
		border: 1px solid #e8e8e8;
		border-radius: 8px;
		padding: 1rem;
	}

	.manual-toggle {
		background: none;
		border: 1px solid #ddd;
		border-radius: 5px;
		padding: 0.4rem 0.8rem;
		cursor: pointer;
		font-size: 0.82rem;
		color: #555;
		width: 100%;
		text-align: left;
	}

	.manual-toggle:hover {
		background: #f5f5f5;
	}

	.manual-text {
		margin-top: 0.75rem;
		font-family: 'Courier New', monospace;
		font-size: 0.75rem;
		line-height: 1.55;
		color: #333;
		white-space: pre-wrap;
		overflow-x: auto;
		max-height: 500px;
		overflow-y: auto;
		border-top: 1px solid #eee;
		padding-top: 0.75rem;
	}

	/* Status bar */
	.status-bar {
		display: flex;
		align-items: center;
		gap: 1rem;
		padding: 0.5rem 0.75rem;
		background: #f8f8f8;
		border: 1px solid #e8e8e8;
		border-radius: 6px;
		font-size: 0.85rem;
		color: #444;
	}

	.halted-badge {
		background: #b91c1c;
		color: white;
		padding: 0.15rem 0.5rem;
		border-radius: 4px;
		font-size: 0.75rem;
		font-weight: 700;
	}

	/* Register table */
	.register-table {
		width: 100%;
		border-collapse: collapse;
		font-size: 0.82rem;
	}

	.register-table tr:not(:last-child) {
		border-bottom: 1px solid #f0f0f0;
	}

	.reg-name {
		padding: 0.3rem 0.5rem 0.3rem 0;
		color: #888;
		font-family: 'Courier New', monospace;
		width: 40%;
		vertical-align: top;
	}

	.reg-value {
		padding: 0.3rem 0;
		font-family: 'Courier New', monospace;
		color: #1a1a2e;
		word-break: break-all;
	}

	.snap-val {
		color: #047857;
	}

	/* Columns strip */
	.columns-strip {
		display: flex;
		flex-wrap: wrap;
		gap: 4px;
		max-height: 120px;
		overflow-y: auto;
	}

	.column-cell {
		display: flex;
		flex-direction: column;
		align-items: center;
		min-width: 36px;
		background: #f5f7ff;
		border: 1px solid #dde3f5;
		border-radius: 4px;
		padding: 4px 2px;
	}

	.col-val {
		font-family: 'Courier New', monospace;
		font-size: 0.78rem;
		font-weight: 700;
		color: #1d4ed8;
	}

	.col-idx {
		font-size: 0.65rem;
		color: #aaa;
	}
</style>
