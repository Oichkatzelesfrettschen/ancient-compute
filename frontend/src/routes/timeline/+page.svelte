<script lang="ts">
  /**
   * Timeline Page - Interactive 12,500-year computational history
   *
   * Features:
   * - D3.js timeline visualization
   * - Era navigation
   * - Zoom controls
   * - Milestone tooltips
   * - Responsive layout
   */

  import { onMount } from 'svelte';
  import { timelineVizStore, loadTimelineData, setLoading, setError } from '$lib/stores/timelineVisualizationStore';
  import { fetchTimeline } from '$lib/api/timeline';
  import { SAMPLE_TIMELINE_DATA } from '$lib/data/sampleTimelineData';

  // Components
  import TimelineD3 from '$lib/components/timeline/TimelineD3.svelte';
  import EraNavigator from '$lib/components/timeline/EraNavigator.svelte';
  import ZoomController from '$lib/components/timeline/ZoomController.svelte';
  import TimelineTooltip from '$lib/components/timeline/TimelineTooltip.svelte';

  // State
  $: state = $timelineVizStore;
  $: isLoading = state.isLoading;
  $: error = state.error;

  /**
   * Load timeline data from API (with fallback to sample data)
   */
  async function loadData() {
    setLoading(true);

    try {
      // Try to fetch from API
      const data = await fetchTimeline();
      loadTimelineData(data.eras, data.eras.flatMap(e => e.milestones));
    } catch (err) {
      console.warn('Failed to load timeline from API, using sample data:', err);
      // Fallback to sample data
      loadTimelineData(SAMPLE_TIMELINE_DATA.eras, SAMPLE_TIMELINE_DATA.milestones);
    }
  }

  // Load data on mount
  onMount(() => {
    loadData();
  });
</script>

<svelte:head>
  <title>Interactive Timeline - Ancient Compute</title>
  <meta
    name="description"
    content="Explore 12,500 years of computational history from prehistoric counting to modern type theory through an interactive timeline visualization."
  />
</svelte:head>

<div class="timeline-page">
  <!-- Page header -->
  <header class="page-header">
    <div class="header-content">
      <h1>Computational History Timeline</h1>
      <p class="page-description">
        An interactive journey through 12,500 years of human innovation in
        computation, logic, and symbolic reasoning. From prehistoric counting
        to modern dependent type theory.
      </p>
    </div>
  </header>

  {#if isLoading}
    <!-- Loading state -->
    <div class="loading-container">
      <div class="spinner"></div>
      <p>Loading timeline data...</p>
    </div>
  {:else if error}
    <!-- Error state -->
    <div class="error-container">
      <h2>Failed to Load Timeline</h2>
      <p>{error}</p>
      <button class="retry-btn" on:click={loadData}>
        Retry
      </button>
    </div>
  {:else}
    <!-- Main content -->
    <div class="timeline-content">
      <!-- Era navigator -->
      <div class="section era-navigator-section">
        <EraNavigator />
      </div>

      <!-- Visualization and controls -->
      <div class="section visualization-section">
        <div class="visualization-grid">
          <!-- Main timeline -->
          <div class="timeline-main">
            <TimelineD3 height={500} />
          </div>

          <!-- Zoom controller -->
          <aside class="timeline-sidebar">
            <ZoomController />
          </aside>
        </div>
      </div>

      <!-- Statistics -->
      <div class="section stats-section">
        <div class="stats-grid">
          <div class="stat-card">
            <div class="stat-value">{state.eras.length}</div>
            <div class="stat-label">Historical Eras</div>
          </div>
          <div class="stat-card">
            <div class="stat-value">{state.allMilestones.length}</div>
            <div class="stat-label">Key Milestones</div>
          </div>
          <div class="stat-card">
            <div class="stat-value">12,500+</div>
            <div class="stat-label">Years of History</div>
          </div>
          <div class="stat-card">
            <div class="stat-value">{state.availableCivilizations.length}</div>
            <div class="stat-label">Civilizations</div>
          </div>
        </div>
      </div>

      <!-- Instructions -->
      <div class="section instructions-section">
        <h2>How to Use the Timeline</h2>
        <div class="instructions-grid">
          <div class="instruction-card">
            <div class="instruction-icon">🖱️</div>
            <h3>Navigate</h3>
            <p>Click on era tabs to zoom to specific periods. Scroll or pan to explore the timeline.</p>
          </div>
          <div class="instruction-card">
            <div class="instruction-icon">🔍</div>
            <h3>Zoom</h3>
            <p>Use zoom controls or keyboard shortcuts (Ctrl +/-) to adjust detail level.</p>
          </div>
          <div class="instruction-card">
            <div class="instruction-icon">📍</div>
            <h3>Explore</h3>
            <p>Hover over milestone markers to see details. Click to learn more.</p>
          </div>
          <div class="instruction-card">
            <div class="instruction-icon">⌨️</div>
            <h3>Shortcuts</h3>
            <p>Arrow keys navigate between eras. Ctrl+0 resets to overview.</p>
          </div>
        </div>
      </div>
    </div>
  {/if}

  <!-- Tooltip (always rendered, visibility controlled by component) -->
  <TimelineTooltip />
</div>

<style>
  .timeline-page {
    min-height: 100vh;
    background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
  }

  /* Header */
  .page-header {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 3rem 2rem;
    text-align: center;
  }

  .header-content {
    max-width: 800px;
    margin: 0 auto;
  }

  .page-header h1 {
    font-size: 2.5rem;
    margin: 0 0 1rem 0;
    font-weight: 700;
  }

  .page-description {
    font-size: 1.1rem;
    line-height: 1.6;
    opacity: 0.95;
    margin: 0;
  }

  /* Loading state */
  .loading-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    min-height: 400px;
    gap: 1rem;
  }

  .spinner {
    width: 50px;
    height: 50px;
    border: 4px solid #e0e0e0;
    border-top-color: #667eea;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    to { transform: rotate(360deg); }
  }

  .loading-container p {
    color: #666;
    font-size: 1.1rem;
  }

  /* Error state */
  .error-container {
    max-width: 600px;
    margin: 2rem auto;
    padding: 2rem;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    text-align: center;
  }

  .error-container h2 {
    color: #d32f2f;
    margin: 0 0 1rem 0;
  }

  .error-container p {
    color: #666;
    margin: 0 0 1.5rem 0;
  }

  .retry-btn {
    padding: 0.75rem 1.5rem;
    background: #667eea;
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 1rem;
    cursor: pointer;
    transition: background 0.2s;
  }

  .retry-btn:hover {
    background: #5568d3;
  }

  /* Main content */
  .timeline-content {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
  }

  .section {
    margin-bottom: 2rem;
  }

  /* Visualization section */
  .visualization-grid {
    display: grid;
    grid-template-columns: 1fr 240px;
    gap: 1.5rem;
  }

  .timeline-main {
    min-height: 500px;
  }

  /* Stats section */
  .stats-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
  }

  .stat-card {
    background: white;
    border-radius: 8px;
    padding: 2rem;
    text-align: center;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    transition: transform 0.2s, box-shadow 0.2s;
  }

  .stat-card:hover {
    transform: translateY(-4px);
    box-shadow: 0 4px 16px rgba(0, 0, 0, 0.15);
  }

  .stat-value {
    font-size: 2.5rem;
    font-weight: 700;
    color: #667eea;
    margin-bottom: 0.5rem;
  }

  .stat-label {
    font-size: 1rem;
    color: #666;
    font-weight: 500;
  }

  /* Instructions section */
  .instructions-section h2 {
    font-size: 2rem;
    color: #1a1a2e;
    margin: 0 0 1.5rem 0;
    text-align: center;
  }

  .instructions-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 1.5rem;
  }

  .instruction-card {
    background: white;
    border-radius: 8px;
    padding: 2rem;
    text-align: center;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }

  .instruction-icon {
    font-size: 3rem;
    margin-bottom: 1rem;
  }

  .instruction-card h3 {
    font-size: 1.25rem;
    color: #1a1a2e;
    margin: 0 0 0.75rem 0;
  }

  .instruction-card p {
    color: #666;
    line-height: 1.6;
    margin: 0;
  }

  /* Responsive */
  @media (max-width: 1024px) {
    .visualization-grid {
      grid-template-columns: 1fr;
    }

    .timeline-sidebar {
      order: -1;
    }
  }

  @media (max-width: 768px) {
    .page-header {
      padding: 2rem 1rem;
    }

    .page-header h1 {
      font-size: 2rem;
    }

    .page-description {
      font-size: 1rem;
    }

    .timeline-content {
      padding: 1rem;
    }

    .stats-grid,
    .instructions-grid {
      grid-template-columns: 1fr;
    }

    .stat-value {
      font-size: 2rem;
    }
  }
</style>
