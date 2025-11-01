<script lang="ts">
	/**
	 * CanvasContainer: Canvas DOM binding and initialization
	 *
	 * Responsibilities:
	 * 1. Bind canvas element to JavaScript VisualizationManager
	 * 2. Display performance metrics overlay (FPS, render time)
	 * 3. Provide responsive canvas sizing
	 * 4. Show loading state during initialization
	 */

	let canvasElement: HTMLCanvasElement | null = null;
	let isLoading = true;
	let containerWidth = 1024;
	let containerHeight = 768;

	export let canvas: HTMLCanvasElement | null = null;
	export let performanceMetrics = {
		fps: 0,
		deltaTime: 0,
		renderTime: 0,
		animationTime: 0
	};

	// Update exported canvas reference when element mounts
	$: if (canvasElement) {
		canvas = canvasElement;
		isLoading = false;
	}

	// Calculate container dimensions
	function updateDimensions() {
		const container = document.querySelector('.canvas-container');
		if (container instanceof HTMLElement) {
			containerWidth = container.clientWidth;
			containerHeight = container.clientHeight;
		}
	}

	// Update dimensions on resize
	function handleResize() {
		updateDimensions();
	}

	// Initialize dimensions on mount
	import { onMount } from 'svelte';
	onMount(() => {
		updateDimensions();
		window.addEventListener('resize', handleResize);

		return () => {
			window.removeEventListener('resize', handleResize);
		};
	});
</script>

<div class="canvas-container">
	<canvas
		bind:this={canvasElement}
		width={containerWidth}
		height={containerHeight}
		class="render-canvas"
		tabindex="0"
		aria-label="Difference Engine 3D Visualization Canvas"
	/>

	{#if isLoading}
		<div class="loading-overlay">
			<div class="spinner"></div>
			<p>Initializing 3D Engine...</p>
		</div>
	{/if}

	<!-- Performance metrics overlay -->
	<div class="metrics-overlay">
		<div class="metric-item">
			<span class="metric-label">FPS:</span>
			<span class="metric-value">{performanceMetrics.fps.toFixed(1)}</span>
		</div>
		<div class="metric-item">
			<span class="metric-label">Frame:</span>
			<span class="metric-value">{performanceMetrics.deltaTime.toFixed(2)}ms</span>
		</div>
		<div class="metric-item">
			<span class="metric-label">Render:</span>
			<span class="metric-value">{performanceMetrics.renderTime.toFixed(2)}ms</span>
		</div>
		<div class="metric-item">
			<span class="metric-label">Anim:</span>
			<span class="metric-value">{performanceMetrics.animationTime.toFixed(2)}ms</span>
		</div>
	</div>

	<!-- Interaction hints overlay -->
	<div class="controls-hint">
		<div class="hint-item">
			<kbd>Left Click</kbd> + Drag: Rotate
		</div>
		<div class="hint-item">
			<kbd>Right Click</kbd> + Drag: Pan
		</div>
		<div class="hint-item">
			<kbd>Scroll</kbd>: Zoom
		</div>
		<div class="hint-item">
			<kbd>R</kbd>: Reset View
		</div>
	</div>
</div>

<style>
	.canvas-container {
		position: relative;
		width: 100%;
		height: 100%;
		display: flex;
		align-items: center;
		justify-content: center;
		background: linear-gradient(135deg, #1a1a2e 0%, #0f3460 100%);
	}

	.render-canvas {
		display: block;
		width: 100%;
		height: 100%;
		border: none;
		outline: none;
	}

	.render-canvas:focus {
		outline: 2px solid #667eea;
		outline-offset: -2px;
	}

	/* Loading overlay */
	.loading-overlay {
		position: absolute;
		inset: 0;
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		background: rgba(26, 26, 46, 0.95);
		backdrop-filter: blur(4px);
		z-index: 10;
	}

	.spinner {
		width: 48px;
		height: 48px;
		border: 3px solid rgba(102, 126, 234, 0.2);
		border-top-color: #667eea;
		border-radius: 50%;
		animation: spin 1s linear infinite;
		margin-bottom: 1rem;
	}

	@keyframes spin {
		to {
			transform: rotate(360deg);
		}
	}

	.loading-overlay p {
		color: #b0b8d4;
		font-size: 0.9rem;
		font-family: 'Courier New', monospace;
	}

	/* Performance metrics overlay */
	.metrics-overlay {
		position: absolute;
		top: 12px;
		left: 12px;
		display: flex;
		flex-direction: column;
		gap: 6px;
		background: rgba(26, 26, 46, 0.85);
		border: 1px solid rgba(102, 126, 234, 0.3);
		border-radius: 6px;
		padding: 10px 14px;
		backdrop-filter: blur(8px);
		font-family: 'Courier New', monospace;
		font-size: 0.8rem;
		z-index: 5;
	}

	.metric-item {
		display: flex;
		gap: 0.5rem;
		align-items: center;
		white-space: nowrap;
	}

	.metric-label {
		color: #667eea;
		font-weight: 600;
		min-width: 45px;
	}

	.metric-value {
		color: #48bb78;
		text-align: right;
		min-width: 50px;
	}

	/* Control hints */
	.controls-hint {
		position: absolute;
		bottom: 12px;
		right: 12px;
		display: flex;
		flex-direction: column;
		gap: 6px;
		background: rgba(26, 26, 46, 0.85);
		border: 1px solid rgba(102, 126, 234, 0.3);
		border-radius: 6px;
		padding: 10px 12px;
		backdrop-filter: blur(8px);
		font-size: 0.75rem;
		color: #b0b8d4;
		z-index: 5;
		max-width: 180px;
	}

	.hint-item {
		display: flex;
		align-items: center;
		gap: 8px;
		white-space: nowrap;
		line-height: 1.4;
	}

	kbd {
		background: rgba(102, 126, 234, 0.15);
		border: 1px solid rgba(102, 126, 234, 0.5);
		border-radius: 3px;
		padding: 2px 4px;
		font-family: 'Courier New', monospace;
		font-size: 0.7rem;
		color: #667eea;
		font-weight: 600;
		min-width: 24px;
		text-align: center;
	}

	/* Responsive adjustments */
	@media (max-width: 768px) {
		.metrics-overlay {
			font-size: 0.7rem;
			top: 8px;
			left: 8px;
			padding: 8px 10px;
		}

		.controls-hint {
			font-size: 0.65rem;
			bottom: 8px;
			right: 8px;
			padding: 8px 10px;
		}

		.hint-item {
			gap: 4px;
		}
	}

	@media (max-width: 480px) {
		.controls-hint {
			display: none;
		}
	}
</style>
