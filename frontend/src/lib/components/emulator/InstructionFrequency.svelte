<!--
  InstructionFrequency.svelte
  Operation frequency distribution chart

  Features:
  - Bar chart showing operation type breakdown
  - Percentage of total operations
  - Sortable by frequency or name
  - Export data as CSV/JSON
  - Filter by operation type
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { machineStateStore } from '$lib/visualization/state/MachineStateStore';

  // Props
  export let height = 300;
  export let showPercentages = true;

  // State
  let canvas: HTMLCanvasElement;
  let chart: any = null;
  let operationStats: OperationStat[] = [];
  let sortBy: 'frequency' | 'name' = 'frequency';
  let sortOrder: 'asc' | 'desc' = 'desc';
  let filterType: string = 'all';

  // Types
  interface OperationStat {
    operation: string;
    count: number;
    percentage: number;
    phase: string;
    color: string;
  }

  // Operation colors (consistent theme)
  const operationColors: Record<string, string> = {
    'add': '#2ecc71',
    'sub': '#3498db',
    'mul': '#e74c3c',
    'div': '#f39c12',
    'load': '#9b59b6',
    'store': '#1abc9c',
    'jump': '#e67e22',
    'compare': '#95a5a6',
    'idle': '#7f8c8d',
    'other': '#34495e'
  };

  // Reactive data
  $: history = machineStateStore.getHistory();
  $: if (history.length > 0) {
    analyzeOperations(history);
  }

  $: sortedStats = getSortedStats(operationStats, sortBy, sortOrder);
  $: filteredStats = getFilteredStats(sortedStats, filterType);

  onMount(async () => {
    // Dynamic import Chart.js to reduce bundle size
    try {
      const ChartJS = await import('chart.js/auto');
      const Chart = ChartJS.default;

      if (canvas) {
        initializeChart(Chart);
      }
    } catch (error) {
      console.error('Failed to load Chart.js:', error);
    }
  });

  onDestroy(() => {
    if (chart) {
      chart.destroy();
      chart = null;
    }
  });

  /**
   * Analyze operations from execution history
   */
  function analyzeOperations(history: any[]) {
    const operationMap = new Map<string, { count: number; phase: string }>();
    let totalOperations = 0;

    history.forEach(entry => {
      const operation = entry.state.mill?.operation || 'idle';
      const phase = entry.state.phase || 'idle';

      const existing = operationMap.get(operation) || { count: 0, phase };
      operationMap.set(operation, {
        count: existing.count + 1,
        phase: phase
      });

      totalOperations++;
    });

    operationStats = Array.from(operationMap.entries()).map(([operation, stats]) => ({
      operation,
      count: stats.count,
      percentage: (stats.count / totalOperations) * 100,
      phase: stats.phase,
      color: operationColors[operation] || operationColors['other']
    }));

    if (chart) {
      updateChart();
    }
  }

  /**
   * Initialize Chart.js bar chart
   */
  function initializeChart(Chart: any) {
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    if (!ctx) return;

    chart = new Chart(ctx, {
      type: 'bar',
      data: {
        labels: [],
        datasets: [{
          label: 'Operation Count',
          data: [],
          backgroundColor: [],
          borderColor: [],
          borderWidth: 1
        }]
      },
      options: {
        responsive: true,
        maintainAspectRatio: false,
        plugins: {
          legend: {
            display: false
          },
          title: {
            display: true,
            text: 'Operation Frequency Distribution',
            color: '#e0e0e0',
            font: {
              family: 'Courier New, monospace',
              size: 14,
              weight: 'bold'
            }
          },
          tooltip: {
            backgroundColor: '#1a1a1a',
            titleColor: '#e0e0e0',
            bodyColor: '#999',
            borderColor: '#333',
            borderWidth: 1,
            titleFont: {
              family: 'Courier New, monospace'
            },
            bodyFont: {
              family: 'Courier New, monospace'
            },
            callbacks: {
              label: function(context: any) {
                const value = context.parsed.y;
                const stat = filteredStats[context.dataIndex];
                return [
                  `Count: ${value}`,
                  `Percentage: ${stat.percentage.toFixed(2)}%`,
                  `Phase: ${stat.phase}`
                ];
              }
            }
          }
        },
        scales: {
          x: {
            ticks: {
              color: '#999',
              font: {
                family: 'Courier New, monospace',
                size: 11
              }
            },
            grid: {
              color: '#333'
            }
          },
          y: {
            beginAtZero: true,
            ticks: {
              color: '#999',
              font: {
                family: 'Courier New, monospace',
                size: 11
              }
            },
            grid: {
              color: '#333'
            }
          }
        }
      }
    });

    updateChart();
  }

  /**
   * Update chart with current data
   */
  function updateChart() {
    if (!chart) return;

    chart.data.labels = filteredStats.map(s => s.operation);
    chart.data.datasets[0].data = filteredStats.map(s => s.count);
    chart.data.datasets[0].backgroundColor = filteredStats.map(s => s.color + 'CC');
    chart.data.datasets[0].borderColor = filteredStats.map(s => s.color);

    chart.update();
  }

  /**
   * Get sorted statistics
   */
  function getSortedStats(
    stats: OperationStat[],
    sortBy: 'frequency' | 'name',
    sortOrder: 'asc' | 'desc'
  ): OperationStat[] {
    const sorted = [...stats].sort((a, b) => {
      if (sortBy === 'frequency') {
        return sortOrder === 'desc' ? b.count - a.count : a.count - b.count;
      } else {
        return sortOrder === 'desc'
          ? b.operation.localeCompare(a.operation)
          : a.operation.localeCompare(b.operation);
      }
    });

    return sorted;
  }

  /**
   * Get filtered statistics
   */
  function getFilteredStats(
    stats: OperationStat[],
    filterType: string
  ): OperationStat[] {
    if (filterType === 'all') {
      return stats;
    }

    return stats.filter(s => s.phase === filterType);
  }

  /**
   * Toggle sort
   */
  function toggleSort(newSortBy: 'frequency' | 'name') {
    if (sortBy === newSortBy) {
      sortOrder = sortOrder === 'asc' ? 'desc' : 'asc';
    } else {
      sortBy = newSortBy;
      sortOrder = 'desc';
    }

    updateChart();
  }

  /**
   * Export data as CSV
   */
  function exportAsCSV() {
    const headers = ['Operation', 'Count', 'Percentage', 'Phase'];
    const rows = filteredStats.map(s => [
      s.operation,
      s.count,
      s.percentage.toFixed(2),
      s.phase
    ]);

    const csv = [
      headers.join(','),
      ...rows.map(row => row.join(','))
    ].join('\n');

    const blob = new Blob([csv], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);

    const link = document.createElement('a');
    link.href = url;
    link.download = 'operation-frequency.csv';
    link.click();

    URL.revokeObjectURL(url);
  }

  /**
   * Export data as JSON
   */
  function exportAsJSON() {
    const json = JSON.stringify(filteredStats, null, 2);

    const blob = new Blob([json], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    const link = document.createElement('a');
    link.href = url;
    link.download = 'operation-frequency.json';
    link.click();

    URL.revokeObjectURL(url);
  }

  /**
   * Get unique phases for filter
   */
  function getUniquePhases(): string[] {
    const phases = new Set(operationStats.map(s => s.phase));
    return Array.from(phases).sort();
  }

  $: uniquePhases = getUniquePhases();
</script>

<style>
  .instruction-frequency {
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    padding: 1rem;
    font-family: 'Courier New', monospace;
  }

  .frequency-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-color, #333);
  }

  .header-title {
    font-size: 1rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
  }

  .header-controls {
    display: flex;
    gap: 0.5rem;
    align-items: center;
  }

  .control-button {
    padding: 0.3rem 0.6rem;
    background: var(--button-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    font-family: inherit;
    font-size: 0.75rem;
    transition: all 0.2s;
  }

  .control-button:hover {
    background: var(--button-hover-bg, #3a3a3a);
  }

  .control-button.active {
    background: #4caf50;
    border-color: #4caf50;
    color: white;
  }

  .filter-select {
    padding: 0.3rem 0.6rem;
    background: var(--select-bg, #0a0a0a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    font-family: inherit;
    font-size: 0.75rem;
    cursor: pointer;
  }

  .chart-container {
    background: #1a1a1a;
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 1rem;
    margin-bottom: 1rem;
  }

  .stats-table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.85rem;
  }

  .stats-table th {
    background: var(--table-header-bg, #0a0a0a);
    padding: 0.75rem 0.5rem;
    text-align: left;
    color: var(--text-secondary, #999);
    font-weight: bold;
    border-bottom: 2px solid var(--border-color, #333);
    cursor: pointer;
    user-select: none;
  }

  .stats-table th:hover {
    background: var(--table-header-hover-bg, #1a1a1a);
  }

  .stats-table td {
    padding: 0.5rem;
    border-bottom: 1px solid var(--border-color, #333);
  }

  .stats-table tr:hover {
    background: var(--table-row-hover-bg, #1a1a1a);
  }

  .operation-cell {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .operation-color {
    width: 12px;
    height: 12px;
    border-radius: 3px;
  }

  .count-cell {
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .percentage-cell {
    color: var(--text-primary, #ff9800);
  }

  .phase-cell {
    color: var(--text-secondary, #999);
  }

  .sort-indicator {
    margin-left: 0.3rem;
    font-size: 0.7rem;
  }

  .empty-message {
    padding: 3rem;
    text-align: center;
    color: var(--text-secondary, #666);
    font-size: 0.9rem;
  }
</style>

<div class="instruction-frequency">
  <!-- Header -->
  <div class="frequency-header">
    <div class="header-title">📊 Instruction Frequency</div>
    <div class="header-controls">
      <select
        class="filter-select"
        bind:value={filterType}
        on:change={() => updateChart()}
      >
        <option value="all">All Phases</option>
        {#each uniquePhases as phase}
          <option value={phase}>{phase}</option>
        {/each}
      </select>

      <button
        class="control-button"
        class:active={showPercentages}
        on:click={() => showPercentages = !showPercentages}
      >
        %
      </button>

      <button class="control-button" on:click={exportAsCSV}>
        CSV
      </button>

      <button class="control-button" on:click={exportAsJSON}>
        JSON
      </button>
    </div>
  </div>

  {#if operationStats.length === 0}
    <div class="empty-message">
      No operation data yet.<br/>
      Run a program to see frequency distribution.
    </div>
  {:else}
    <!-- Chart -->
    <div class="chart-container" style="height: {height}px">
      <canvas bind:this={canvas}></canvas>
    </div>

    <!-- Stats Table -->
    <table class="stats-table">
      <thead>
        <tr>
          <th on:click={() => toggleSort('name')}>
            Operation
            {#if sortBy === 'name'}
              <span class="sort-indicator">{sortOrder === 'desc' ? '▼' : '▲'}</span>
            {/if}
          </th>
          <th on:click={() => toggleSort('frequency')}>
            Count
            {#if sortBy === 'frequency'}
              <span class="sort-indicator">{sortOrder === 'desc' ? '▼' : '▲'}</span>
            {/if}
          </th>
          {#if showPercentages}
            <th>Percentage</th>
          {/if}
          <th>Phase</th>
        </tr>
      </thead>
      <tbody>
        {#each filteredStats as stat}
          <tr>
            <td>
              <div class="operation-cell">
                <div class="operation-color" style="background: {stat.color}"></div>
                <span>{stat.operation}</span>
              </div>
            </td>
            <td class="count-cell">{stat.count}</td>
            {#if showPercentages}
              <td class="percentage-cell">{stat.percentage.toFixed(2)}%</td>
            {/if}
            <td class="phase-cell">{stat.phase}</td>
          </tr>
        {/each}
      </tbody>
    </table>
  {/if}
</div>
