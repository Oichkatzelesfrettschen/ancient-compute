<script lang="ts">
/**
 * Timeline.svelte - Interactive historical timeline component
 *
 * Features:
 * - 8 historical eras spanning 12,500 years
 * - Smooth horizontal scrolling with era markers
 * - Visual progress indicators for each era
 * - Era selection with color-coded styling
 * - Responsive viewport with navigation arrows
 */

import { timelineStore, selectEra, erasList, currentEra } from '../../stores/timelineStore';
import type { Era } from '../../stores/timelineStore';

let scrollContainer: HTMLDivElement;
let viewportWidth = 1200;
const ERA_PIXEL_WIDTH = 280; // Width per era in pixels
const ERA_MARKER_RADIUS = 8; // SVG circle radius

// Era definitions (will be loaded from store)
let eras: Era[] = [];
let selectedEraId: string | null = null;

// Subscribe to stores
erasList.subscribe((value) => {
  eras = value;
  if (eras.length > 0 && !selectedEraId) {
    selectedEraId = eras[0].id;
  }
});

currentEra.subscribe((era) => {
  if (era) {
    selectedEraId = era.id;
  }
});

// Timeline dimensions
const TIMELINE_HEIGHT = 200;
const SVG_WIDTH = eras.length * ERA_PIXEL_WIDTH;
const SVG_HEIGHT = TIMELINE_HEIGHT;
const TIMELINE_LINE_Y = SVG_HEIGHT / 2;

// ============================================================================
// SCROLL HANDLING
// ============================================================================

function scrollTimeline(direction: 'left' | 'right'): void {
  if (!scrollContainer) return;

  const scrollAmount = 300;
  const targetScroll =
    scrollContainer.scrollLeft +
    (direction === 'right' ? scrollAmount : -scrollAmount);

  scrollContainer.scrollTo({
    left: targetScroll,
    behavior: 'smooth',
  });
}

function scrollToEra(eraId: string): void {
  const eraIndex = eras.findIndex((e) => e.id === eraId);
  if (eraIndex === -1 || !scrollContainer) return;

  const targetScroll = eraIndex * ERA_PIXEL_WIDTH - viewportWidth / 2 + ERA_PIXEL_WIDTH / 2;

  scrollContainer.scrollTo({
    left: Math.max(0, targetScroll),
    behavior: 'smooth',
  });
}

// ============================================================================
// ERA SELECTION
// ============================================================================

function handleEraClick(era: Era): void {
  selectEra(era.id);
  scrollToEra(era.id);
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Format year for display (handles BC/AD)
 */
function formatYear(year: number): string {
  if (year < 0) {
    return `${Math.abs(year)} BC`;
  } else if (year === 0) {
    return 'AD 1';
  } else {
    return `AD ${year}`;
  }
}

/**
 * Calculate completion percentage for era
 */
function getEraProgress(era: Era): number {
  const totalLessons = era.modules.reduce(
    (sum, mod) => sum + mod.lessons.length,
    0
  );
  const completedLessons = era.modules.reduce(
    (sum, mod) =>
      sum + mod.lessons.filter((lesson) => lesson.completed).length,
    0
  );

  return totalLessons > 0 ? Math.round((completedLessons / totalLessons) * 100) : 0;
}

/**
 * Get X coordinate for era marker on SVG
 */
function getEraX(index: number): number {
  return index * ERA_PIXEL_WIDTH + ERA_PIXEL_WIDTH / 2;
}
</script>

<div class="timeline-wrapper">
  <!-- Header with title and progress -->
  <div class="timeline-header">
    <div class="header-left">
      <h2 class="timeline-title">Historical Timeline</h2>
      <p class="timeline-subtitle">12,500 years of computational thinking</p>
    </div>
    <div class="header-right">
      <div class="progress-indicator">
        <span class="progress-label">Overall Progress</span>
        <div class="progress-bar">
          <div class="progress-fill" style="width: {calculateOverallProgress()}%"></div>
        </div>
        <span class="progress-percent">{calculateOverallProgress()}%</span>
      </div>
    </div>
  </div>

  <!-- Timeline scrollable area -->
  <div class="timeline-container">
    <!-- Left navigation arrow -->
    <button
      class="timeline-nav timeline-nav-left"
      aria-label="Scroll timeline left"
      on:click={() => scrollTimeline('left')}
    >
      <svg width="24" height="24" viewBox="0 0 24 24" fill="none">
        <path d="M15 18l-6-6 6-6" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
      </svg>
    </button>

    <!-- Timeline SVG and markers -->
    <div class="timeline-viewport" bind:clientWidth={viewportWidth} bind:this={scrollContainer}>
      <svg
        class="timeline-svg"
        width={SVG_WIDTH}
        height={SVG_HEIGHT}
        viewBox="0 0 {SVG_WIDTH} {SVG_HEIGHT}"
      >
        <!-- Main timeline line -->
        <line
          x1="0"
          y1={TIMELINE_LINE_Y}
          x2={SVG_WIDTH}
          y2={TIMELINE_LINE_Y}
          stroke="#e0e0e0"
          stroke-width="2"
        />

        <!-- Era markers and labels -->
        {#each eras as era, index (era.id)}
          {@const x = getEraX(index)}
          {@const isSelected = selectedEraId === era.id}
          {@const progress = getEraProgress(era)}

          <!-- Vertical line to timeline -->
          <line
            x1={x}
            y1={TIMELINE_LINE_Y - 12}
            x2={x}
            y2={TIMELINE_LINE_Y + 12}
            stroke={era.color}
            stroke-width={isSelected ? 3 : 2}
            class="era-line"
          />

          <!-- Era circle marker -->
          <circle
            cx={x}
            cy={TIMELINE_LINE_Y}
            r={ERA_MARKER_RADIUS}
            fill={era.color}
            stroke={isSelected ? '#333' : 'transparent'}
            stroke-width={isSelected ? 3 : 0}
            class="era-marker"
            class:selected={isSelected}
            on:click={() => handleEraClick(era)}
            role="button"
            tabindex="0"
          />

          <!-- Progress ring (outer circle for completion) -->
          {#if progress > 0}
            <circle
              cx={x}
              cy={TIMELINE_LINE_Y}
              r={ERA_MARKER_RADIUS + 4}
              fill="none"
              stroke={era.color}
              stroke-width="2"
              opacity="0.3"
              class="progress-ring"
            />
          {/if}

          <!-- Era label below timeline -->
          <text
            x={x}
            y={TIMELINE_LINE_Y + 40}
            text-anchor="middle"
            class="era-label"
            class:selected={isSelected}
            font-size="13"
            font-weight={isSelected ? '600' : '400'}
          >
            {era.label}
          </text>

          <!-- Era abbreviation (years) -->
          <text
            x={x}
            y={TIMELINE_LINE_Y + 58}
            text-anchor="middle"
            class="era-years"
            font-size="11"
            opacity="0.7"
          >
            {formatYear(era.startYear)} – {formatYear(era.endYear)}
          </text>
        {/each}
      </svg>
    </div>

    <!-- Right navigation arrow -->
    <button
      class="timeline-nav timeline-nav-right"
      aria-label="Scroll timeline right"
      on:click={() => scrollTimeline('right')}
    >
      <svg width="24" height="24" viewBox="0 0 24 24" fill="none">
        <path d="M9 18l6-6-6-6" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
      </svg>
    </button>
  </div>

  <!-- Era details card below timeline -->
  {#if selectedEraId && eras.find((e) => e.id === selectedEraId) as selectedEra}
    <div class="selected-era-card" style="border-left: 4px solid {selectedEra.color}">
      <div class="era-card-header">
        <h3 class="era-name">{selectedEra.fullName}</h3>
        <div class="era-stats">
          <span class="stat">
            <span class="stat-number">{selectedEra.modules.length}</span>
            <span class="stat-label">Modules</span>
          </span>
          <span class="stat">
            <span class="stat-number">{selectedEra.modules.reduce((sum, m) => sum + m.lessons.length, 0)}</span>
            <span class="stat-label">Lessons</span>
          </span>
          <span class="stat">
            <span class="stat-number">{getEraProgress(selectedEra)}%</span>
            <span class="stat-label">Complete</span>
          </span>
        </div>
      </div>
      <p class="era-description">{selectedEra.description}</p>
    </div>
  {/if}
</div>

<script lang="ts">
  import { timelineStore as store } from '../../stores/timelineStore';

  function calculateOverallProgress(): number {
    let total = 0;
    let completed = 0;

    $store.eras.forEach((era) => {
      era.modules.forEach((mod) => {
        mod.lessons.forEach((lesson) => {
          total++;
          if (lesson.completed) completed++;
        });
      });
    });

    return total > 0 ? Math.round((completed / total) * 100) : 0;
  }
</script>

<style>
  .timeline-wrapper {
    display: flex;
    flex-direction: column;
    gap: 24px;
    padding: 24px;
    background: linear-gradient(135deg, #f5f5f5 0%, #fafafa 100%);
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }

  .timeline-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 32px;
  }

  .header-left {
    flex: 1;
  }

  .timeline-title {
    margin: 0;
    font-size: 24px;
    font-weight: 700;
    color: #2c3e50;
    line-height: 1.2;
  }

  .timeline-subtitle {
    margin: 8px 0 0 0;
    font-size: 13px;
    color: #666;
    font-style: italic;
  }

  .header-right {
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 8px;
  }

  .progress-indicator {
    display: flex;
    align-items: center;
    gap: 12px;
    background: white;
    padding: 12px 16px;
    border-radius: 6px;
    border: 1px solid #e0e0e0;
  }

  .progress-label {
    font-size: 12px;
    font-weight: 600;
    color: #666;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .progress-bar {
    width: 120px;
    height: 8px;
    background: #e0e0e0;
    border-radius: 4px;
    overflow: hidden;
  }

  .progress-fill {
    height: 100%;
    background: linear-gradient(90deg, #4a90e2 0%, #7b68ee 100%);
    transition: width 0.3s ease;
  }

  .progress-percent {
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
    min-width: 35px;
  }

  .timeline-container {
    display: flex;
    align-items: center;
    gap: 12px;
    background: white;
    border-radius: 6px;
    border: 1px solid #e0e0e0;
    padding: 8px;
  }

  .timeline-nav {
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 40px;
    height: 40px;
    background: #f5f5f5;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    cursor: pointer;
    transition: all 0.2s ease;
    color: #666;
  }

  .timeline-nav:hover {
    background: #e8e8e8;
    border-color: #d0d0d0;
    color: #333;
  }

  .timeline-nav:active {
    transform: scale(0.95);
  }

  .timeline-viewport {
    flex: 1;
    overflow-x: auto;
    overflow-y: hidden;
    height: 180px;
    scroll-behavior: smooth;
  }

  .timeline-viewport::-webkit-scrollbar {
    height: 6px;
  }

  .timeline-viewport::-webkit-scrollbar-track {
    background: #f0f0f0;
  }

  .timeline-viewport::-webkit-scrollbar-thumb {
    background: #ccc;
    border-radius: 3px;
  }

  .timeline-viewport::-webkit-scrollbar-thumb:hover {
    background: #999;
  }

  .timeline-svg {
    display: block;
    min-width: 100%;
    height: 100%;
  }

  .era-line {
    transition: stroke-width 0.2s ease;
  }

  .era-marker {
    cursor: pointer;
    transition: all 0.2s ease;
  }

  .era-marker:hover {
    filter: brightness(1.1);
    r: 10;
  }

  .era-marker.selected {
    filter: brightness(0.95);
  }

  .progress-ring {
    transition: opacity 0.2s ease;
  }

  .era-label {
    cursor: pointer;
    transition: all 0.2s ease;
    fill: #666;
  }

  .era-label.selected {
    fill: #2c3e50;
    font-weight: 600;
  }

  .era-label:hover {
    fill: #2c3e50;
  }

  .era-years {
    fill: #999;
    pointer-events: none;
  }

  .selected-era-card {
    background: white;
    padding: 20px;
    border-radius: 6px;
    border: 1px solid #e0e0e0;
    animation: slideDown 0.3s ease;
  }

  @keyframes slideDown {
    from {
      opacity: 0;
      transform: translateY(-10px);
    }
    to {
      opacity: 1;
      transform: translateY(0);
    }
  }

  .era-card-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 12px;
  }

  .era-name {
    margin: 0;
    font-size: 18px;
    font-weight: 700;
    color: #2c3e50;
  }

  .era-stats {
    display: flex;
    gap: 24px;
  }

  .stat {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 4px;
  }

  .stat-number {
    font-size: 16px;
    font-weight: 700;
    color: #2c3e50;
  }

  .stat-label {
    font-size: 11px;
    color: #999;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .era-description {
    margin: 0;
    font-size: 13px;
    line-height: 1.6;
    color: #666;
  }

  @media (max-width: 768px) {
    .timeline-header {
      flex-direction: column;
      align-items: flex-start;
    }

    .header-right {
      align-items: flex-start;
      width: 100%;
    }

    .progress-indicator {
      width: 100%;
      justify-content: space-between;
    }

    .era-card-header {
      flex-direction: column;
      gap: 12px;
    }

    .era-stats {
      width: 100%;
      justify-content: space-around;
    }
  }
</style>
