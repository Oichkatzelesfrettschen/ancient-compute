<script lang="ts">
  /**
   * EraNavigator.svelte - Era selection and breadcrumb navigation
   *
   * Features:
   * - Horizontal era selector tabs
   * - Click to zoom to specific era
   * - Visual indicator for current era
   * - Breadcrumb navigation
   * - Keyboard navigation support
   */

  import { timelineVizStore, selectEra, currentEra } from '$lib/stores/timelineVisualizationStore';
  import { formatYearRange } from '$lib/api/timeline';
  import type { TimelineEra } from '$lib/api/timeline';

  // Subscribe to store
  $: state = $timelineVizStore;
  $: eras = state.eras;
  $: current = $currentEra;

  /**
   * Handle era selection
   */
  function handleEraClick(era: TimelineEra) {
    selectEra(era.id);
  }

  /**
   * Handle keyboard navigation
   */
  function handleKeyDown(event: KeyboardEvent, index: number) {
    if (event.key === 'ArrowLeft' && index > 0) {
      event.preventDefault();
      selectEra(eras[index - 1].id);
      (event.target as HTMLElement).previousElementSibling?.querySelector('button')?.focus();
    } else if (event.key === 'ArrowRight' && index < eras.length - 1) {
      event.preventDefault();
      selectEra(eras[index + 1].id);
      (event.target as HTMLElement).nextElementSibling?.querySelector('button')?.focus();
    } else if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault();
      selectEra(eras[index].id);
    }
  }
</script>

<nav class="era-navigator" aria-label="Historical era navigation">
  <!-- Breadcrumb -->
  <div class="breadcrumb">
    <button
      class="breadcrumb-item"
      on:click={() => selectEra(eras[0]?.id || '')}
    >
      Timeline
    </button>
    {#if current}
      <span class="breadcrumb-separator">/</span>
      <span class="breadcrumb-item current">{current.name}</span>
    {/if}
  </div>

  <!-- Era tabs -->
  <div class="era-tabs" role="tablist">
    {#each eras as era, index (era.id)}
      <div class="era-tab-wrapper">
        <button
          role="tab"
          aria-selected={current?.id === era.id}
          aria-label={`${era.name} (${formatYearRange(era.startYear, era.endYear)})`}
          class="era-tab"
          class:active={current?.id === era.id}
          style="--era-color: {era.color}"
          on:click={() => handleEraClick(era)}
          on:keydown={(e) => handleKeyDown(e, index)}
          tabindex={current?.id === era.id ? 0 : -1}
        >
          <div class="era-icon">
            {#if era.icon}
              <span class="icon">{era.icon}</span>
            {:else}
              <span class="order">{era.order + 1}</span>
            {/if}
          </div>
          <div class="era-info">
            <div class="era-name">{era.name}</div>
            <div class="era-years">{formatYearRange(era.startYear, era.endYear)}</div>
          </div>
          <div class="era-indicator" style="background-color: {era.color}"></div>
        </button>
      </div>
    {/each}
  </div>

  <!-- Era description -->
  {#if current}
    <div class="era-description">
      <h3>{current.fullName || current.name}</h3>
      <p>{current.description}</p>
      {#if current.moduleCount !== undefined}
        <div class="era-stats">
          <span class="stat">
            <strong>{current.moduleCount}</strong> modules
          </span>
          {#if current.lessonCount !== undefined}
            <span class="stat">
              <strong>{current.lessonCount}</strong> lessons
            </span>
          {/if}
        </div>
      {/if}
    </div>
  {/if}
</nav>

<style>
  .era-navigator {
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
    overflow: hidden;
  }

  /* Breadcrumb */
  .breadcrumb {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    padding: 1rem 1.5rem;
    background: #f5f5f5;
    border-bottom: 1px solid #e0e0e0;
    font-size: 0.9rem;
  }

  .breadcrumb-item {
    background: none;
    border: none;
    padding: 0;
    color: #667eea;
    cursor: pointer;
    font-size: inherit;
    transition: color 0.2s;
  }

  .breadcrumb-item:hover {
    color: #5568d3;
    text-decoration: underline;
  }

  .breadcrumb-item.current {
    color: #333;
    font-weight: 600;
    cursor: default;
  }

  .breadcrumb-item.current:hover {
    text-decoration: none;
  }

  .breadcrumb-separator {
    color: #999;
  }

  /* Era tabs */
  .era-tabs {
    display: flex;
    overflow-x: auto;
    scrollbar-width: thin;
    scrollbar-color: #ccc #f5f5f5;
  }

  .era-tabs::-webkit-scrollbar {
    height: 6px;
  }

  .era-tabs::-webkit-scrollbar-track {
    background: #f5f5f5;
  }

  .era-tabs::-webkit-scrollbar-thumb {
    background: #ccc;
    border-radius: 3px;
  }

  .era-tab-wrapper {
    flex: 1 0 auto;
    min-width: 140px;
  }

  .era-tab {
    width: 100%;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.5rem;
    padding: 1rem 0.75rem;
    background: white;
    border: none;
    border-bottom: 3px solid transparent;
    cursor: pointer;
    transition: all 0.2s;
    position: relative;
  }

  .era-tab:hover {
    background: #fafafa;
  }

  .era-tab:focus {
    outline: 2px solid #667eea;
    outline-offset: -2px;
  }

  .era-tab.active {
    background: #f0f4ff;
    border-bottom-color: var(--era-color);
  }

  .era-icon {
    width: 40px;
    height: 40px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 50%;
    background: var(--era-color);
    color: white;
    font-weight: bold;
    font-size: 1.2rem;
  }

  .era-tab.active .era-icon {
    box-shadow: 0 0 0 4px rgba(102, 126, 234, 0.2);
  }

  .era-info {
    text-align: center;
  }

  .era-name {
    font-weight: 600;
    font-size: 0.9rem;
    color: #333;
    margin-bottom: 0.25rem;
  }

  .era-years {
    font-size: 0.75rem;
    color: #666;
  }

  .era-indicator {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    height: 3px;
    opacity: 0;
    transition: opacity 0.2s;
  }

  .era-tab.active .era-indicator {
    opacity: 1;
  }

  /* Era description */
  .era-description {
    padding: 1.5rem;
    background: #f9fafb;
    border-top: 1px solid #e0e0e0;
  }

  .era-description h3 {
    margin: 0 0 0.75rem 0;
    font-size: 1.25rem;
    color: #1a1a2e;
  }

  .era-description p {
    margin: 0 0 1rem 0;
    color: #555;
    line-height: 1.6;
  }

  .era-stats {
    display: flex;
    gap: 1.5rem;
    font-size: 0.9rem;
    color: #666;
  }

  .stat strong {
    color: #667eea;
    font-weight: 700;
  }

  /* Responsive */
  @media (max-width: 768px) {
    .era-tab-wrapper {
      min-width: 120px;
    }

    .era-icon {
      width: 32px;
      height: 32px;
      font-size: 1rem;
    }

    .era-name {
      font-size: 0.8rem;
    }

    .era-years {
      font-size: 0.7rem;
    }
  }
</style>
