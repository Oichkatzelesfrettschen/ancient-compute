<script lang="ts">
	import { onMount, onDestroy } from 'svelte';
	import type { MachineState, StateDiff } from '$lib/visualization/state/types';
	import { machineStateStore } from '$lib/visualization/state';
	import { VisualizationManager } from '$lib/visualization/3d/VisualizationManager';
	import { StateReconciler } from '$lib/visualization/state';
	import CanvasContainer from './CanvasContainer.svelte';
	import ControlPanel from './ControlPanel.svelte';

	/**
	 * EmulatorView: Master orchestrator for 3D mechanical visualization
	 *
	 * Responsibilities:
	 * 1. Manage VisualizationManager lifecycle (mount, destroy)
	 * 2. Subscribe to machine state changes via Svelte store
	 * 3. Compute state diffs and trigger animations
	 * 4. Route performance metrics to child components
	 * 5. Handle responsive canvas resizing
	 */

	// Reactive store subscription
	let currentState: MachineState | null = null;
	let previousState: MachineState | null = null;

	// Component state
	let manager: VisualizationManager | null = null;
	let canvas: HTMLCanvasElement | null = null;
	let isAnimating = false;
	let debugInfo = '';
	let performanceMetrics = {
		fps: 0,
		deltaTime: 0,
		renderTime: 0,
		animationTime: 0
	};

	// Configuration
	const vizConfig = {
		width: 1024,
		height: 768,
		enablePerformanceMonitoring: true,
		enableDebugOverlay: false
	};

	/**
	 * Initialize VisualizationManager on component mount
	 * Sets up canvas, scene, renderer, and animation loop
	 */
	onMount(() => {
		if (!canvas) return;

		try {
			manager = new VisualizationManager({
				canvas,
				...vizConfig
			});

			// Start animation loop
			manager.start();

			// Handle window resize
			const handleResize = () => {
				if (!manager) return;
				const rect = canvas?.getBoundingClientRect();
				if (rect) {
					manager.scene.onWindowResize(rect.width, rect.height);
				}
			};

			window.addEventListener('resize', handleResize);

			// Subscribe to store updates
			const unsubscribe = machineStateStore.subscribe((newState) => {
				currentState = newState;

				// Only update visualization if manager is ready and state changed
				if (manager && previousState && previousState !== newState) {
					handleStateUpdate(newState, previousState);
				}

				// Set previous state for next comparison
				if (!previousState) {
					previousState = newState;
				}
			});

			return () => {
				window.removeEventListener('resize', handleResize);
				unsubscribe();
			};
		} catch (error) {
			console.error('Failed to initialize VisualizationManager:', error);
		}
	});

	/**
	 * Cleanup on component destroy
	 * Stops animation loop and releases GPU resources
	 */
	onDestroy(() => {
		if (manager) {
			manager.stop();
			manager.dispose();
			manager = null;
		}
	});

	/**
	 * Handle state update: compute diff and trigger animation
	 *
	 * @param newState Current machine state
	 * @param oldState Previous machine state
	 */
	function handleStateUpdate(newState: MachineState, oldState: MachineState) {
		if (!manager) return;

		try {
			// Compute diff between states
			const diff = StateReconciler.computeDiff(oldState, newState);

			// Update visualization (triggers animation sequence)
			manager.updateState(newState, diff);

			// Update debug info
			debugInfo = manager.getDebugInfo();

			// Track previous state for next diff
			previousState = newState;

			// Update performance metrics
			const stats = manager.scene.getStats();
			performanceMetrics = {
				fps: stats.fps,
				deltaTime: stats.deltaTime,
				renderTime: stats.renderTime,
				animationTime: stats.animationTime
			};
		} catch (error) {
			console.error('Error updating visualization:', error);
		}
	}

	/**
	 * Reset visualization to initial state
	 */
	function resetVisualization() {
		if (!manager) return;
		manager.reset();
		debugInfo = manager.getDebugInfo();
	}

	/**
	 * Toggle debug overlay display
	 */
	function toggleDebug() {
		if (!manager) return;
		vizConfig.enableDebugOverlay = !vizConfig.enableDebugOverlay;
	}
</script>

<div class="emulator-view">
	<div class="canvas-wrapper">
		<CanvasContainer bind:canvas {performanceMetrics} />
	</div>

	<ControlPanel
		isAnimating={isAnimating || manager?.timeline.isRunning() || false}
		debugInfo={debugInfo}
		metrics={performanceMetrics}
		onReset={resetVisualization}
		onToggleDebug={toggleDebug}
	/>
</div>

<style>
	.emulator-view {
		display: grid;
		grid-template-columns: 1fr 300px;
		gap: 1rem;
		height: 100%;
		width: 100%;
	}

	.canvas-wrapper {
		background: #1a1a2e;
		border-radius: 8px;
		overflow: hidden;
		box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
	}

	@media (max-width: 1200px) {
		.emulator-view {
			grid-template-columns: 1fr;
		}
	}
</style>
