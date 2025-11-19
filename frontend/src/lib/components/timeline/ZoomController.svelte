<script lang="ts">
  /**
   * ZoomController.svelte - Zoom controls for timeline
   *
   * Features:
   * - Zoom in/out buttons
   * - Zoom level indicator
   * - Reset to overview button
   * - Keyboard shortcuts (Ctrl+/-, Ctrl+0)
   */

  import { onMount, onDestroy } from 'svelte';
  import { timelineVizStore, zoomIn, zoomOut, resetZoom, setZoomLevel, zoomMultiplier } from '$lib/stores/timelineVisualizationStore';
  import type { ZoomLevel } from '$lib/stores/timelineVisualizationStore';

  // Subscribe to store
  $: state = $timelineVizStore;
  $: zoomLevel = state.zoomLevel;
  $: multiplier = $zoomMultiplier;

  // Zoom level info
  const ZOOM_LEVELS: { level: ZoomLevel; label: string; description: string }[] = [
    { level: 'overview', label: 'Overview', description: '12,500 years' },
    { level: 'era', label: 'Era', description: '1,000-5,000 years' },
    { level: 'decade', label: 'Decade', description: '10-100 years' },
    { level: 'year', label: 'Year', description: '1-10 years' },
  ];

  /**
   * Handle keyboard shortcuts
   */
  function handleKeyDown(event: KeyboardEvent) {
    if (event.ctrlKey || event.metaKey) {
      if (event.key === '=' || event.key === '+') {
        event.preventDefault();
        zoomIn();
      } else if (event.key === '-' || event.key === '_') {
        event.preventDefault();
        zoomOut();
      } else if (event.key === '0') {
        event.preventDefault();
        resetZoom();
      }
    }
  }

  /**
   * Get zoom level index
   */
  function getZoomIndex(level: ZoomLevel): number {
    return ZOOM_LEVELS.findIndex(z => z.level === level);
  }

  /**
   * Check if can zoom in
   */
  $: canZoomIn = getZoomIndex(zoomLevel) < ZOOM_LEVELS.length - 1;

  /**
   * Check if can zoom out
   */
  $: canZoomOut = getZoomIndex(zoomLevel) > 0;

  // Lifecycle
  onMount(() => {
    window.addEventListener('keydown', handleKeyDown);
  });

  onDestroy(() => {
    window.removeEventListener('keydown', handleKeyDown);
  });
</script>

<div class="zoom-controller">
  <div class="zoom-header">
    <h4>Zoom Level</h4>
    <span class="zoom-multiplier">{multiplier}×</span>
  </div>

  <!-- Zoom buttons -->
  <div class="zoom-buttons">
    <button
      class="zoom-btn zoom-out"
      disabled={!canZoomOut}
      on:click={zoomOut}
      aria-label="Zoom out"
      title="Zoom out (Ctrl + -)"
    >
      <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M5 10H15" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <circle cx="10" cy="10" r="8" stroke="currentColor" stroke-width="2"/>
      </svg>
    </button>

    <button
      class="zoom-btn zoom-reset"
      on:click={resetZoom}
      aria-label="Reset zoom"
      title="Reset to overview (Ctrl + 0)"
    >
      <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M10 4V10L14 14" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <circle cx="10" cy="10" r="8" stroke="currentColor" stroke-width="2"/>
      </svg>
    </button>

    <button
      class="zoom-btn zoom-in"
      disabled={!canZoomIn}
      on:click={zoomIn}
      aria-label="Zoom in"
      title="Zoom in (Ctrl + +)"
    >
      <svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M10 5V15M5 10H15" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
        <circle cx="10" cy="10" r="8" stroke="currentColor" stroke-width="2"/>
      </svg>
    </button>
  </div>

  <!-- Zoom level selector -->
  <div class="zoom-levels">
    {#each ZOOM_LEVELS as { level, label, description }, index}
      <button
        class="zoom-level"
        class:active={zoomLevel === level}
        on:click={() => setZoomLevel(level)}
        aria-label={`${label}: ${description}`}
        title={description}
      >
        <div class="level-indicator" class:filled={getZoomIndex(zoomLevel) >= index}></div>
        <div class="level-label">{label}</div>
      </button>
    {/each}
  </div>

  <!-- Current level info -->
  <div class="current-level-info">
    <div class="level-name">
      {ZOOM_LEVELS.find(z => z.level === zoomLevel)?.label}
    </div>
    <div class="level-description">
      {ZOOM_LEVELS.find(z => z.level === zoomLevel)?.description}
    </div>
  </div>

  <!-- Keyboard shortcuts help -->
  <div class="keyboard-shortcuts">
    <div class="shortcut-title">Keyboard Shortcuts</div>
    <div class="shortcuts-list">
      <div class="shortcut">
        <kbd>Ctrl</kbd> + <kbd>+</kbd>
        <span>Zoom in</span>
      </div>
      <div class="shortcut">
        <kbd>Ctrl</kbd> + <kbd>-</kbd>
        <span>Zoom out</span>
      </div>
      <div class="shortcut">
        <kbd>Ctrl</kbd> + <kbd>0</kbd>
        <span>Reset</span>
      </div>
    </div>
  </div>
</div>

<style>
  .zoom-controller {
    background: white;
    border-radius: 8px;
    padding: 1.5rem;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    min-width: 200px;
  }

  .zoom-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .zoom-header h4 {
    margin: 0;
    font-size: 1rem;
    color: #333;
  }

  .zoom-multiplier {
    font-weight: 700;
    font-size: 1.1rem;
    color: #667eea;
  }

  /* Zoom buttons */
  .zoom-buttons {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }

  .zoom-btn {
    flex: 1;
    padding: 0.75rem;
    background: #f5f5f5;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.2s;
    display: flex;
    align-items: center;
    justify-content: center;
    color: #333;
  }

  .zoom-btn:hover:not(:disabled) {
    background: #667eea;
    border-color: #667eea;
    color: white;
  }

  .zoom-btn:disabled {
    opacity: 0.4;
    cursor: not-allowed;
  }

  .zoom-btn:focus {
    outline: 2px solid #667eea;
    outline-offset: 2px;
  }

  /* Zoom levels */
  .zoom-levels {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
    margin-bottom: 1.5rem;
  }

  .zoom-level {
    display: flex;
    align-items: center;
    gap: 0.75rem;
    padding: 0.5rem;
    background: none;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    transition: background 0.2s;
  }

  .zoom-level:hover {
    background: #f5f5f5;
  }

  .zoom-level.active {
    background: #e8ecff;
  }

  .level-indicator {
    width: 12px;
    height: 12px;
    border: 2px solid #ccc;
    border-radius: 50%;
    transition: all 0.2s;
  }

  .level-indicator.filled {
    background: #667eea;
    border-color: #667eea;
  }

  .level-label {
    font-size: 0.9rem;
    color: #333;
  }

  /* Current level info */
  .current-level-info {
    padding: 1rem;
    background: #f9fafb;
    border-radius: 6px;
    margin-bottom: 1.5rem;
  }

  .level-name {
    font-weight: 600;
    color: #667eea;
    margin-bottom: 0.25rem;
  }

  .level-description {
    font-size: 0.85rem;
    color: #666;
  }

  /* Keyboard shortcuts */
  .keyboard-shortcuts {
    padding-top: 1rem;
    border-top: 1px solid #e0e0e0;
  }

  .shortcut-title {
    font-size: 0.85rem;
    font-weight: 600;
    color: #666;
    margin-bottom: 0.75rem;
  }

  .shortcuts-list {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .shortcut {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.8rem;
    color: #666;
  }

  .shortcut kbd {
    display: inline-block;
    padding: 0.2rem 0.4rem;
    background: #f5f5f5;
    border: 1px solid #ccc;
    border-radius: 3px;
    font-family: monospace;
    font-size: 0.75rem;
    box-shadow: 0 1px 2px rgba(0, 0, 0, 0.1);
  }

  .shortcut span {
    margin-left: auto;
  }

  /* Responsive */
  @media (max-width: 768px) {
    .zoom-controller {
      min-width: auto;
    }

    .keyboard-shortcuts {
      display: none;
    }
  }
</style>
