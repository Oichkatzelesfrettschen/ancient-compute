<script lang="ts">
/**
 * HistoricalNavigator.svelte - Era and module selection component
 *
 * Features:
 * - Vertical list of all 8 eras
 * - Expandable module list per era
 * - Progress indicators for each module
 * - Quick navigation between historical periods
 * - Responsive sidebar with collapsible sections
 */

import { erasList, currentEra, selectEra, selectModule } from '../../stores/timelineStore';
import type { Era, Module } from '../../stores/timelineStore';

let expandedEras: Set<string> = new Set();
let selectedModuleId: string | null = null;

// Subscribe to current era to highlight it
currentEra.subscribe((era) => {
  if (era && !expandedEras.has(era.id)) {
    // Auto-expand current era if not already expanded
    expandedEras = new Set([...expandedEras, era.id]);
  }
});

function toggleEra(eraId: string): void {
  const newExpanded = new Set(expandedEras);
  if (newExpanded.has(eraId)) {
    newExpanded.delete(eraId);
  } else {
    newExpanded.add(eraId);
  }
  expandedEras = newExpanded;
}

function handleEraClick(era: Era): void {
  selectEra(era.id);
  // Auto-expand the selected era
  expandedEras = new Set([...expandedEras, era.id]);
}

function handleModuleClick(module: Module): void {
  selectedModuleId = module.id;
  selectModule(module.id);
}

function calculateModuleProgress(module: Module): number {
  const total = module.lessons.length;
  const completed = module.lessons.filter((l) => l.completed).length;
  return total > 0 ? Math.round((completed / total) * 100) : 0;
}

function getCompletionStatus(module: Module): string {
  const progress = calculateModuleProgress(module);
  if (progress === 0) return 'not-started';
  if (progress === 100) return 'completed';
  return 'in-progress';
}
</script>

<nav class="historical-navigator">
  <!-- Navigator header -->
  <div class="nav-header">
    <h2 class="nav-title">Historical Periods</h2>
    <p class="nav-subtitle">Explore computational thinking across millennia</p>
  </div>

  <!-- Eras list -->
  <div class="eras-container">
    {#each $erasList as era (era.id)}
      {@const isExpanded = expandedEras.has(era.id)}

      <!-- Era item -->
      <div class="era-item" class:expanded={isExpanded}>
        <!-- Era header/trigger -->
        <button
          class="era-header"
          class:active={$currentEra?.id === era.id}
          style="border-left-color: {era.color}"
          on:click={() => handleEraClick(era)}
        >
          <div class="era-header-content">
            <div class="era-icon" style="background-color: {era.color}">
              {era.icon || '◆'}
            </div>
            <div class="era-info">
              <h3 class="era-name">{era.label}</h3>
              <p class="era-dates">{era.startYear} – {era.endYear}</p>
            </div>
          </div>

          <!-- Expand/collapse indicator -->
          <div class="era-toggle" class:rotated={isExpanded}>
            <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
              <path
                d="M6 12L12 8 6 4"
                stroke="currentColor"
                stroke-width="2"
                stroke-linecap="round"
                stroke-linejoin="round"
              />
            </svg>
          </div>
        </button>

        <!-- Modules list (expandable) -->
        {#if isExpanded && era.modules.length > 0}
          <div class="modules-list" transition:slide>
            {#each era.modules as module (module.id)}
              {@const progress = calculateModuleProgress(module)}
              {@const status = getCompletionStatus(module)}

              <button
                class="module-item"
                class:selected={selectedModuleId === module.id}
                class:completed={status === 'completed'}
                class:in-progress={status === 'in-progress'}
                on:click={() => handleModuleClick(module)}
              >
                <!-- Module status indicator -->
                <div class="module-status" class:completed class:in-progress>
                  {#if status === 'completed'}
                    <svg width="14" height="14" viewBox="0 0 14 14" fill="none">
                      <path
                        d="M11.5 3.5L5.5 11.5L2 8"
                        stroke="currentColor"
                        stroke-width="1.5"
                        stroke-linecap="round"
                        stroke-linejoin="round"
                      />
                    </svg>
                  {:else if status === 'in-progress'}
                    <div class="progress-dot"></div>
                  {:else}
                    <div class="empty-dot"></div>
                  {/if}
                </div>

                <!-- Module info -->
                <div class="module-info">
                  <div class="module-name">{module.title}</div>
                  <div class="module-lessons">
                    {module.completedLessons}/{module.lessons.length} lessons
                  </div>
                </div>

                <!-- Progress indicator -->
                <div class="module-progress">
                  <div class="progress-percentage">{progress}%</div>
                </div>
              </button>
            {/each}
          </div>
        {/if}
      </div>
    {/each}
  </div>

  <!-- Navigator footer with legend -->
  <div class="nav-footer">
    <div class="legend">
      <div class="legend-item">
        <div class="legend-icon empty-dot"></div>
        <span>Not started</span>
      </div>
      <div class="legend-item">
        <div class="legend-icon progress-dot"></div>
        <span>In progress</span>
      </div>
      <div class="legend-item">
        <div class="legend-icon completed-dot"></div>
        <span>Completed</span>
      </div>
    </div>
  </div>
</nav>

<style>
  .historical-navigator {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: #fafafa;
    border-right: 1px solid #e0e0e0;
    overflow: hidden;
  }

  .nav-header {
    padding: 20px 16px;
    border-bottom: 1px solid #e0e0e0;
    background: white;
  }

  .nav-title {
    margin: 0;
    font-size: 16px;
    font-weight: 700;
    color: #2c3e50;
  }

  .nav-subtitle {
    margin: 4px 0 0 0;
    font-size: 12px;
    color: #999;
  }

  .eras-container {
    flex: 1;
    overflow-y: auto;
    padding: 8px 0;
  }

  .eras-container::-webkit-scrollbar {
    width: 6px;
  }

  .eras-container::-webkit-scrollbar-track {
    background: transparent;
  }

  .eras-container::-webkit-scrollbar-thumb {
    background: #ccc;
    border-radius: 3px;
  }

  .eras-container::-webkit-scrollbar-thumb:hover {
    background: #999;
  }

  .era-item {
    border-bottom: 1px solid #efefef;
  }

  .era-item.expanded {
    background: #f5f5f5;
  }

  .era-header {
    width: 100%;
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 12px;
    background: transparent;
    border: none;
    border-left: 3px solid transparent;
    cursor: pointer;
    transition: all 0.2s ease;
    text-align: left;
  }

  .era-header:hover {
    background: #efefef;
  }

  .era-header.active {
    background: #e8f4fd;
    border-left-color: inherit;
    font-weight: 600;
  }

  .era-header-content {
    display: flex;
    align-items: center;
    gap: 12px;
    flex: 1;
    min-width: 0;
  }

  .era-icon {
    flex-shrink: 0;
    width: 32px;
    height: 32px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 4px;
    font-size: 16px;
    color: white;
    font-weight: 700;
  }

  .era-info {
    flex: 1;
    min-width: 0;
  }

  .era-name {
    margin: 0;
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .era-dates {
    margin: 2px 0 0 0;
    font-size: 11px;
    color: #999;
  }

  .era-toggle {
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    color: #999;
    transition: transform 0.2s ease;
  }

  .era-toggle.rotated {
    transform: rotate(90deg);
  }

  :global(.modules-list) {
    background: white;
    border-top: 1px solid #efefef;
  }

  .module-item {
    display: flex;
    align-items: center;
    gap: 12px;
    width: 100%;
    padding: 8px 12px 8px 36px;
    background: transparent;
    border: none;
    cursor: pointer;
    transition: all 0.2s ease;
    text-align: left;
    font-size: 13px;
  }

  .module-item:hover {
    background: #f5f5f5;
  }

  .module-item.selected {
    background: #e8f4fd;
    color: #4a90e2;
  }

  .module-item.completed {
    color: #7cb342;
  }

  .module-item.in-progress {
    color: #ff9800;
  }

  .module-status {
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 16px;
    height: 16px;
    color: inherit;
  }

  .empty-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    border: 2px solid #ddd;
    background: transparent;
  }

  .progress-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: currentColor;
    animation: pulse 2s infinite;
  }

  @keyframes pulse {
    0%, 100% {
      opacity: 1;
    }
    50% {
      opacity: 0.5;
    }
  }

  .completed-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: currentColor;
  }

  .module-info {
    flex: 1;
    min-width: 0;
  }

  .module-name {
    font-weight: 500;
    color: #2c3e50;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .module-lessons {
    font-size: 11px;
    color: #999;
    margin-top: 2px;
  }

  .module-progress {
    flex-shrink: 0;
  }

  .progress-percentage {
    font-size: 11px;
    font-weight: 600;
    color: #999;
  }

  .nav-footer {
    padding: 12px 16px;
    border-top: 1px solid #e0e0e0;
    background: white;
  }

  .legend {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 11px;
    color: #666;
  }

  .legend-icon {
    flex-shrink: 0;
  }

  @media (max-width: 768px) {
    .historical-navigator {
      border-right: none;
      border-bottom: 1px solid #e0e0e0;
    }

    .eras-container {
      max-height: 200px;
    }

    .nav-footer {
      display: none;
    }
  }
</style>

<script context="module">
  // Import slide transition
  import { slide } from 'svelte/transition';
</script>
