<script lang="ts">
	/**
	 * ControlPanel: Debug info display and control buttons
	 *
	 * Responsibilities:
	 * 1. Display performance metrics and debug information
	 * 2. Provide reset and debug toggle buttons
	 * 3. Show animation state and engine status
	 * 4. Responsive design for various screen sizes
	 */

	export let isAnimating = false;
	export let debugInfo = '';
	export let metrics = {
		fps: 0,
		deltaTime: 0,
		renderTime: 0,
		animationTime: 0
	};
	export let onReset: () => void = () => {};
	export let onToggleDebug: () => void = () => {};

	let showFullInfo = false;

	// Parse debug info to extract key details
	function parseDebugInfo() {
		const lines = debugInfo.split('\n');
		return {
			title: lines[0] || '',
			fps: lines[1] || '',
			geometry: lines[2] || '',
			materials: lines[3] || ''
		};
	}

	$: debugParsed = parseDebugInfo();
</script>

<div class="control-panel">
	<div class="panel-header">
		<h3>Engine Status</h3>
		<div class="status-indicator" class:animating={isAnimating} title="Animation running">
			{isAnimating ? '◉ Running' : '◯ Ready'}
		</div>
	</div>

	<div class="metrics-section">
		<div class="metric-row">
			<span class="metric-name">FPS:</span>
			<span class="metric-display">{metrics.fps.toFixed(1)}</span>
		</div>
		<div class="metric-row">
			<span class="metric-name">Frame Time:</span>
			<span class="metric-display">{metrics.deltaTime.toFixed(2)}ms</span>
		</div>
		<div class="metric-row">
			<span class="metric-name">Render:</span>
			<span class="metric-display">{metrics.renderTime.toFixed(2)}ms</span>
		</div>
		<div class="metric-row">
			<span class="metric-name">Animation:</span>
			<span class="metric-display">{metrics.animationTime.toFixed(2)}ms</span>
		</div>
	</div>

	<div class="debug-section">
		<button
			class="toggle-button"
			on:click={() => (showFullInfo = !showFullInfo)}
			aria-label="Toggle debug information"
		>
			{showFullInfo ? '▼' : '▶'} Debug Info
		</button>

		{#if showFullInfo && debugInfo}
			<div class="debug-content">
				<pre>{debugInfo}</pre>
			</div>
		{/if}
	</div>

	<div class="button-group">
		<button class="control-button reset-button" on:click={onReset} aria-label="Reset visualization">
			Reset View
		</button>
		<button class="control-button debug-button" on:click={onToggleDebug} aria-label="Toggle debug overlay">
			Debug Overlay
		</button>
	</div>

	<div class="help-section">
		<h4>Controls</h4>
		<ul class="help-list">
			<li><strong>Click + Drag:</strong> Rotate</li>
			<li><strong>Right Click + Drag:</strong> Pan</li>
			<li><strong>Scroll:</strong> Zoom</li>
			<li><strong>R:</strong> Reset View</li>
			<li><strong>H:</strong> Help</li>
		</ul>
	</div>

	<div class="stats-footer">
		<small>Difference Engine No. 2</small>
		<small>Phase 4.W3 Visualization</small>
	</div>
</div>

<style>
	.control-panel {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 1.5rem;
		max-height: 100%;
		overflow-y: auto;
		font-family: 'Courier New', monospace;
	}

	.panel-header {
		display: flex;
		align-items: center;
		justify-content: space-between;
		border-bottom: 2px solid #667eea;
		padding-bottom: 0.75rem;
		margin-bottom: 0.5rem;
	}

	.panel-header h3 {
		color: #667eea;
		margin: 0;
		font-size: 1rem;
		font-weight: 700;
	}

	.status-indicator {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 4px 8px;
		background: rgba(102, 126, 234, 0.1);
		border-radius: 4px;
		color: #b0b8d4;
		font-size: 0.8rem;
		font-weight: 600;
		letter-spacing: 0.5px;
	}

	.status-indicator.animating {
		color: #48bb78;
		background: rgba(72, 187, 120, 0.1);
	}

	/* Metrics Section */
	.metrics-section {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		padding: 0.75rem;
		background: rgba(102, 126, 234, 0.05);
		border-left: 3px solid #667eea;
		border-radius: 4px;
	}

	.metric-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 0.75rem;
	}

	.metric-name {
		color: #667eea;
		font-size: 0.85rem;
		font-weight: 600;
		flex: 0 0 auto;
	}

	.metric-display {
		color: #48bb78;
		font-size: 0.85rem;
		text-align: right;
		flex: 1;
		font-weight: 700;
	}

	/* Debug Section */
	.debug-section {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.toggle-button {
		background: rgba(102, 126, 234, 0.1);
		border: 1px solid #667eea;
		color: #667eea;
		padding: 0.5rem 0.75rem;
		border-radius: 4px;
		font-family: 'Courier New', monospace;
		font-size: 0.8rem;
		cursor: pointer;
		transition: all 0.2s ease;
		text-align: left;
		font-weight: 600;
	}

	.toggle-button:hover {
		background: rgba(102, 126, 234, 0.2);
		border-color: #7c8ef5;
	}

	.debug-content {
		background: rgba(0, 0, 0, 0.3);
		border: 1px solid rgba(102, 126, 234, 0.2);
		border-radius: 4px;
		padding: 0.5rem;
		max-height: 200px;
		overflow-y: auto;
	}

	.debug-content pre {
		margin: 0;
		color: #b0b8d4;
		font-size: 0.7rem;
		line-height: 1.3;
		white-space: pre-wrap;
		word-wrap: break-word;
	}

	/* Button Group */
	.button-group {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 0.75rem;
	}

	.control-button {
		padding: 0.75rem;
		border-radius: 6px;
		border: none;
		font-family: 'Courier New', monospace;
		font-size: 0.85rem;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.3s ease;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		white-space: nowrap;
	}

	.reset-button {
		background: #667eea;
		color: #1a1a2e;
	}

	.reset-button:hover {
		background: #7c8ef5;
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}

	.reset-button:active {
		transform: scale(0.98);
	}

	.debug-button {
		background: #533483;
		color: #e0d5ff;
	}

	.debug-button:hover {
		background: #6b4b9e;
		box-shadow: 0 4px 12px rgba(83, 52, 131, 0.3);
	}

	.debug-button:active {
		transform: scale(0.98);
	}

	/* Help Section */
	.help-section {
		padding: 0.75rem;
		background: rgba(83, 52, 131, 0.1);
		border-left: 3px solid #533483;
		border-radius: 4px;
	}

	.help-section h4 {
		color: #b0b8d4;
		margin: 0 0 0.5rem 0;
		font-size: 0.85rem;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.help-list {
		list-style: none;
		margin: 0;
		padding: 0;
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}

	.help-list li {
		color: #b0b8d4;
		font-size: 0.75rem;
		line-height: 1.4;
	}

	.help-list strong {
		color: #667eea;
		font-weight: 700;
	}

	/* Stats Footer */
	.stats-footer {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		padding-top: 0.75rem;
		border-top: 1px solid rgba(102, 126, 234, 0.2);
		text-align: center;
	}

	.stats-footer small {
		color: #667eea;
		font-size: 0.7rem;
		opacity: 0.8;
	}

	/* Scrollbar Styling */
	.control-panel::-webkit-scrollbar {
		width: 6px;
	}

	.control-panel::-webkit-scrollbar-track {
		background: rgba(102, 126, 234, 0.1);
		border-radius: 3px;
	}

	.control-panel::-webkit-scrollbar-thumb {
		background: #667eea;
		border-radius: 3px;
	}

	.control-panel::-webkit-scrollbar-thumb:hover {
		background: #7c8ef5;
	}
</style>
