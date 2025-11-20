<script lang="ts">
  /**
   * TimelineTooltip.svelte - Rich tooltip for milestone hover
   *
   * Features:
   * - Rich HTML content
   * - Images for key inventions
   * - Links to related modules/lessons
   * - Smart positioning (avoid screen edges)
   * - Smooth fade in/out
   */

  import { timelineVizStore } from '$lib/stores/timelineVisualizationStore';
  import { formatYear } from '$lib/api/timeline';
  import type { Milestone } from '$lib/api/timeline';

  // Subscribe to store
  $: state = $timelineVizStore;
  $: milestone = state.hoveredMilestone;
  $: position = state.tooltipPosition;
  $: isVisible = milestone !== null && position !== null;

  // Tooltip dimensions (approximate)
  const TOOLTIP_WIDTH = 300;
  const TOOLTIP_HEIGHT = 200;
  const TOOLTIP_OFFSET = 10;

  /**
   * Calculate smart tooltip position
   */
  function getTooltipStyle(pos: { x: number; y: number } | null): string {
    if (!pos) return 'display: none;';

    let left = pos.x;
    let top = pos.y - TOOLTIP_HEIGHT - TOOLTIP_OFFSET;

    // Adjust horizontal position to avoid screen edges
    if (left + TOOLTIP_WIDTH > window.innerWidth) {
      left = window.innerWidth - TOOLTIP_WIDTH - 20;
    }
    if (left < 20) {
      left = 20;
    }

    // Adjust vertical position if too close to top
    if (top < 20) {
      top = pos.y + TOOLTIP_OFFSET;
    }

    return `left: ${left}px; top: ${top}px;`;
  }

  /**
   * Get category badge color
   */
  function getCategoryColor(category: string): string {
    const colors: Record<string, string> = {
      invention: '#FF6B6B',
      theory: '#4ECDC4',
      person: '#FFD93D',
      event: '#95E1D3',
    };
    return colors[category] || '#999';
  }
</script>

{#if isVisible && milestone}
  <div
    class="tooltip"
    class:visible={isVisible}
    style={getTooltipStyle(position)}
    role="tooltip"
    aria-live="polite"
  >
    <!-- Header -->
    <div class="tooltip-header">
      <div class="category-badge" style="background-color: {getCategoryColor(milestone.category)}">
        {milestone.category}
      </div>
      <div class="year">{formatYear(milestone.year)}</div>
    </div>

    <!-- Content -->
    <div class="tooltip-content">
      <h4 class="title">{milestone.title}</h4>
      <p class="description">{milestone.description}</p>

      <!-- Image if available -->
      {#if milestone.imageUrl}
        <div class="image-container">
          <img src={milestone.imageUrl} alt={milestone.title} loading="lazy" />
        </div>
      {/if}

      <!-- Civilization -->
      <div class="civilization">
        <strong>Civilization:</strong> {milestone.civilization}
      </div>

      <!-- Related modules -->
      {#if milestone.relatedModuleIds && milestone.relatedModuleIds.length > 0}
        <div class="related-modules">
          <strong>Related content:</strong>
          <div class="module-links">
            {#each milestone.relatedModuleIds.slice(0, 3) as moduleId}
              <a href="/modules/{moduleId}" class="module-link">
                View module →
              </a>
            {/each}
          </div>
        </div>
      {/if}
    </div>

    <!-- Arrow -->
    <div class="tooltip-arrow"></div>
  </div>
{/if}

<style>
  .tooltip {
    position: fixed;
    width: 300px;
    background: white;
    border-radius: 8px;
    box-shadow: 0 4px 16px rgba(0, 0, 0, 0.15);
    opacity: 0;
    transform: translateY(-10px);
    transition: opacity 0.2s ease, transform 0.2s ease;
    pointer-events: none;
    z-index: 1000;
    overflow: hidden;
  }

  .tooltip.visible {
    opacity: 1;
    transform: translateY(0);
  }

  /* Header */
  .tooltip-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.75rem 1rem;
    background: #f9fafb;
    border-bottom: 1px solid #e0e0e0;
  }

  .category-badge {
    display: inline-block;
    padding: 0.25rem 0.75rem;
    border-radius: 12px;
    color: white;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .year {
    font-weight: 600;
    color: #667eea;
    font-size: 0.9rem;
  }

  /* Content */
  .tooltip-content {
    padding: 1rem;
    max-height: 300px;
    overflow-y: auto;
  }

  .title {
    margin: 0 0 0.5rem 0;
    font-size: 1.1rem;
    color: #1a1a2e;
    line-height: 1.3;
  }

  .description {
    margin: 0 0 0.75rem 0;
    color: #555;
    font-size: 0.9rem;
    line-height: 1.5;
  }

  /* Image */
  .image-container {
    margin: 0.75rem 0;
    border-radius: 6px;
    overflow: hidden;
  }

  .image-container img {
    width: 100%;
    height: auto;
    display: block;
  }

  /* Civilization */
  .civilization {
    margin: 0.75rem 0;
    padding: 0.5rem;
    background: #f0f4ff;
    border-left: 3px solid #667eea;
    border-radius: 4px;
    font-size: 0.85rem;
    color: #333;
  }

  .civilization strong {
    color: #667eea;
  }

  /* Related modules */
  .related-modules {
    margin-top: 0.75rem;
    padding-top: 0.75rem;
    border-top: 1px solid #e0e0e0;
    font-size: 0.85rem;
  }

  .related-modules strong {
    display: block;
    margin-bottom: 0.5rem;
    color: #333;
  }

  .module-links {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .module-link {
    color: #667eea;
    text-decoration: none;
    transition: color 0.2s;
    font-weight: 500;
  }

  .module-link:hover {
    color: #5568d3;
    text-decoration: underline;
  }

  /* Arrow */
  .tooltip-arrow {
    position: absolute;
    bottom: -8px;
    left: 50%;
    transform: translateX(-50%);
    width: 0;
    height: 0;
    border-left: 8px solid transparent;
    border-right: 8px solid transparent;
    border-top: 8px solid white;
    filter: drop-shadow(0 2px 4px rgba(0, 0, 0, 0.1));
  }

  /* Scrollbar styling */
  .tooltip-content::-webkit-scrollbar {
    width: 6px;
  }

  .tooltip-content::-webkit-scrollbar-track {
    background: #f5f5f5;
  }

  .tooltip-content::-webkit-scrollbar-thumb {
    background: #ccc;
    border-radius: 3px;
  }
</style>
