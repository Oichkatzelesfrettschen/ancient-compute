<script lang="ts">
  /**
   * MilestoneMarker.svelte - Individual milestone marker component
   *
   * Features:
   * - SVG marker with category icon
   * - Hover effects
   * - Click to view details
   * - Pulsing animation for important milestones
   */

  import { selectMilestone, setHoveredMilestone } from '$lib/stores/timelineVisualizationStore';
  import { formatYear } from '$lib/api/timeline';
  import type { Milestone } from '$lib/api/timeline';

  // Props
  export let milestone: Milestone;
  export let x: number;
  export let y: number;
  export let color: string;
  export let size: number = 12;
  export let important: boolean = false;

  // State
  let isHovered = false;

  // Category icons
  const categoryIcons: Record<string, string> = {
    invention: '⚙️',
    theory: '📐',
    person: '👤',
    event: '📅',
  };

  /**
   * Handle marker click
   */
  function handleClick() {
    selectMilestone(milestone);
  }

  /**
   * Handle mouse enter
   */
  function handleMouseEnter(event: MouseEvent) {
    isHovered = true;
    const target = event.currentTarget as Element;
    const rect = target.getBoundingClientRect();
    setHoveredMilestone(milestone, {
      x: rect.left + rect.width / 2,
      y: rect.top,
    });
  }

  /**
   * Handle mouse leave
   */
  function handleMouseLeave() {
    isHovered = false;
    setHoveredMilestone(null, null);
  }
</script>

<g
  class="milestone-marker"
  class:important
  class:hovered={isHovered}
  transform="translate({x},{y})"
  on:click={handleClick}
  on:mouseenter={handleMouseEnter}
  on:mouseleave={handleMouseLeave}
  role="button"
  tabindex="0"
  aria-label="{milestone.title} ({formatYear(milestone.year)})"
>
  <!-- Pulsing circle for important milestones -->
  {#if important}
    <circle
      class="pulse-circle"
      r={size * 1.5}
      fill={color}
      opacity="0"
    >
      <animate
        attributeName="r"
        from={size}
        to={size * 2}
        dur="2s"
        repeatCount="indefinite"
      />
      <animate
        attributeName="opacity"
        from="0.5"
        to="0"
        dur="2s"
        repeatCount="indefinite"
      />
    </circle>
  {/if}

  <!-- Main marker circle -->
  <circle
    class="marker-circle"
    r={isHovered ? size * 1.3 : size}
    fill={color}
    stroke="white"
    stroke-width="2"
  />

  <!-- Category icon (text) -->
  <text
    class="category-icon"
    x="0"
    y="0"
    text-anchor="middle"
    dominant-baseline="central"
    font-size={size}
    pointer-events="none"
  >
    {categoryIcons[milestone.category] || ''}
  </text>

  <!-- Vertical line to timeline -->
  <line
    class="marker-line"
    x1="0"
    y1={size + 2}
    x2="0"
    y2="30"
    stroke={color}
    stroke-width="1"
    stroke-dasharray="2,2"
    opacity="0.5"
  />
</g>

<style>
  .milestone-marker {
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .milestone-marker:hover .marker-circle {
    filter: drop-shadow(0 0 6px rgba(0, 0, 0, 0.3));
  }

  .milestone-marker:focus {
    outline: none;
  }

  .milestone-marker:focus .marker-circle {
    stroke: #667eea;
    stroke-width: 3;
  }

  .marker-circle {
    transition: all 0.2s ease;
  }

  .category-icon {
    user-select: none;
  }

  .marker-line {
    transition: opacity 0.2s ease;
  }

  .milestone-marker.hovered .marker-line {
    opacity: 1;
  }

  .milestone-marker.important .marker-circle {
    stroke-width: 3;
  }

  .pulse-circle {
    pointer-events: none;
  }
</style>
