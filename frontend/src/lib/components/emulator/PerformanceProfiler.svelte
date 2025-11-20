<!--
  PerformanceProfiler.svelte
  Performance analysis and profiling for emulator execution

  Features:
  - Real-time FPS and update rate monitoring
  - Cycle timing histogram
  - Bottleneck detection
  - Memory usage tracking
  - Performance recommendations
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import type { EmulatorStateBridge } from '$lib/visualization/integration/EmulatorStateBridge';
  import { executionStateStore } from '$lib/visualization/state/MachineStateStore';

  // Props
  export let bridge: EmulatorStateBridge | null = null;
  export let autoRefresh = true;
  export let refreshInterval = 1000; // 1 second

  // State
  let metrics = {
    fps: 0,
    updateRate: 0,
    avgCycleTime: 0,
    minCycleTime: Infinity,
    maxCycleTime: 0,
    totalCycles: 0,
    droppedFrames: 0,
    memoryUsage: 0
  };

  let cycleTimes: number[] = [];
  let lastCycleTime = 0;
  let lastFrameTime = performance.now();
  let frameCount = 0;
  let updateCount = 0;
  let refreshTimer: number | null = null;
  let cycleTimeHistory: number[] = [];
  let bottlenecks: string[] = [];

  // Reactive
  $: cycles = $executionStateStore.cycles;
  $: isRunning = $executionStateStore.runningFlag;

  // Watch for cycle changes
  $: {
    if (cycles > 0) {
      recordCycleTime(cycles);
    }
  }

  onMount(() => {
    if (autoRefresh) {
      startAutoRefresh();
    }
  });

  onDestroy(() => {
    stopAutoRefresh();
  });

  /**
   * Start automatic metric refresh
   */
  function startAutoRefresh() {
    if (refreshTimer !== null) return;

    refreshTimer = window.setInterval(() => {
      updateMetrics();
    }, refreshInterval);
  }

  /**
   * Stop automatic metric refresh
   */
  function stopAutoRefresh() {
    if (refreshTimer !== null) {
      clearInterval(refreshTimer);
      refreshTimer = null;
    }
  }

  /**
   * Record cycle execution time
   */
  function recordCycleTime(currentCycle: number) {
    const now = performance.now();

    if (lastCycleTime > 0) {
      const cycleTime = now - lastCycleTime;
      cycleTimes.push(cycleTime);

      // Keep only last 100 cycle times
      if (cycleTimes.length > 100) {
        cycleTimes.shift();
      }

      cycleTimeHistory.push(cycleTime);

      // Update min/max
      if (cycleTime < metrics.minCycleTime) {
        metrics.minCycleTime = cycleTime;
      }
      if (cycleTime > metrics.maxCycleTime) {
        metrics.maxCycleTime = cycleTime;
      }
    }

    lastCycleTime = now;
  }

  /**
   * Update all performance metrics
   */
  function updateMetrics() {
    const now = performance.now();
    const deltaTime = now - lastFrameTime;

    // FPS calculation
    frameCount++;
    if (deltaTime >= 1000) {
      metrics.fps = Math.round((frameCount * 1000) / deltaTime);
      frameCount = 0;
      lastFrameTime = now;
    }

    // Update rate (from bridge if available)
    if (bridge) {
      const bridgeMetrics = bridge.getPerformanceMetrics();
      metrics.updateRate = Math.round((bridgeMetrics.updatesProcessed * 1000) / deltaTime);
      metrics.droppedFrames = bridgeMetrics.droppedUpdates;
    }

    // Average cycle time
    if (cycleTimes.length > 0) {
      metrics.avgCycleTime =
        cycleTimes.reduce((sum, t) => sum + t, 0) / cycleTimes.length;
    }

    // Total cycles
    metrics.totalCycles = cycles;

    // Memory usage (rough estimate)
    if (performance.memory) {
      metrics.memoryUsage = performance.memory.usedJSHeapSize / (1024 * 1024); // MB
    }

    // Detect bottlenecks
    detectBottlenecks();
  }

  /**
   * Detect performance bottlenecks
   */
  function detectBottlenecks() {
    bottlenecks = [];

    // Low FPS
    if (metrics.fps > 0 && metrics.fps < 30) {
      bottlenecks.push(`Low FPS: ${metrics.fps} (target: 60)`);
    }

    // High cycle time variance
    if (cycleTimes.length > 10) {
      const variance =
        cycleTimes.reduce((sum, t) => sum + Math.pow(t - metrics.avgCycleTime, 2), 0) /
        cycleTimes.length;
      const stdDev = Math.sqrt(variance);

      if (stdDev > metrics.avgCycleTime * 0.5) {
        bottlenecks.push(`High cycle time variance: ${stdDev.toFixed(2)}ms`);
      }
    }

    // Dropped frames
    if (metrics.droppedFrames > 0) {
      bottlenecks.push(`Dropped ${metrics.droppedFrames} updates`);
    }

    // High memory usage
    if (metrics.memoryUsage > 500) {
      bottlenecks.push(`High memory usage: ${metrics.memoryUsage.toFixed(0)}MB`);
    }

    // Slow average cycle time
    if (metrics.avgCycleTime > 100) {
      bottlenecks.push(`Slow avg cycle time: ${metrics.avgCycleTime.toFixed(2)}ms`);
    }
  }

  /**
   * Get performance recommendations
   */
  function getRecommendations(): string[] {
    const recommendations = [];

    if (metrics.fps < 30) {
      recommendations.push('Consider reducing visualization quality');
      recommendations.push('Enable Canvas2D fallback for better performance');
    }

    if (metrics.avgCycleTime > 100) {
      recommendations.push('Emulator execution may be slow');
      recommendations.push('Check backend performance and network latency');
    }

    if (metrics.droppedFrames > 10) {
      recommendations.push('Increase update throttle interval');
      recommendations.push('Reduce WebSocket message frequency');
    }

    if (metrics.memoryUsage > 500) {
      recommendations.push('Clear execution history periodically');
      recommendations.push('Limit history buffer size');
    }

    if (recommendations.length === 0) {
      recommendations.push('Performance is optimal ✓');
    }

    return recommendations;
  }

  /**
   * Reset all metrics
   */
  function resetMetrics() {
    metrics = {
      fps: 0,
      updateRate: 0,
      avgCycleTime: 0,
      minCycleTime: Infinity,
      maxCycleTime: 0,
      totalCycles: 0,
      droppedFrames: 0,
      memoryUsage: 0
    };

    cycleTimes = [];
    cycleTimeHistory = [];
    bottlenecks = [];
    frameCount = 0;
    lastFrameTime = performance.now();
    lastCycleTime = 0;
  }

  /**
   * Get histogram data for cycle times
   */
  function getHistogramData(): { min: number; max: number; count: number }[] {
    if (cycleTimes.length === 0) return [];

    const bins = 10;
    const min = metrics.minCycleTime;
    const max = metrics.maxCycleTime;
    const range = max - min;
    const binSize = range / bins;

    const histogram = Array(bins).fill(0).map((_, i) => ({
      min: min + i * binSize,
      max: min + (i + 1) * binSize,
      count: 0
    }));

    cycleTimes.forEach(time => {
      const binIndex = Math.min(Math.floor((time - min) / binSize), bins - 1);
      histogram[binIndex].count++;
    });

    return histogram;
  }

  $: histogramData = getHistogramData();
  $: recommendations = getRecommendations();
  $: maxHistogramCount = Math.max(...histogramData.map(h => h.count), 1);
</script>

<style>
  .profiler-panel {
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    font-family: 'Courier New', monospace;
    overflow: hidden;
  }

  .profiler-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.75rem 1rem;
    background: var(--header-bg, #0a0a0a);
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

  .profiler-content {
    padding: 1rem;
  }

  .metrics-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 0.75rem;
    margin-bottom: 1.5rem;
  }

  .metric-card {
    background: var(--metric-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
  }

  .metric-label {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
    margin-bottom: 0.4rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .metric-value {
    font-size: 1.5rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .metric-unit {
    font-size: 0.9rem;
    color: var(--text-secondary, #999);
    margin-left: 0.3rem;
  }

  .section-title {
    font-size: 0.9rem;
    font-weight: bold;
    color: var(--text-secondary, #999);
    margin-bottom: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .histogram {
    background: var(--histogram-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 1rem;
    margin-bottom: 1.5rem;
  }

  .histogram-bars {
    display: flex;
    align-items: flex-end;
    gap: 4px;
    height: 120px;
    margin-bottom: 0.5rem;
  }

  .histogram-bar {
    flex: 1;
    background: linear-gradient(to top, #4caf50, #8bc34a);
    border-radius: 2px 2px 0 0;
    position: relative;
    transition: all 0.3s;
  }

  .histogram-bar:hover {
    background: linear-gradient(to top, #45a049, #7cb342);
  }

  .histogram-label {
    font-size: 0.7rem;
    color: var(--text-secondary, #999);
    text-align: center;
  }

  .bottlenecks {
    background: var(--bottleneck-bg, #3a1e1e);
    border: 1px solid var(--bottleneck-border, #f44336);
    border-radius: 4px;
    padding: 0.75rem;
    margin-bottom: 1.5rem;
  }

  .bottleneck-item {
    padding: 0.4rem;
    font-size: 0.85rem;
    color: var(--bottleneck-text, #ff9800);
    margin-bottom: 0.3rem;
  }

  .bottleneck-item::before {
    content: '⚠️ ';
  }

  .recommendations {
    background: var(--recommend-bg, #1e3a1e);
    border: 1px solid var(--recommend-border, #4caf50);
    border-radius: 4px;
    padding: 0.75rem;
  }

  .recommendation-item {
    padding: 0.4rem;
    font-size: 0.85rem;
    color: var(--recommend-text, #8bc34a);
    margin-bottom: 0.3rem;
  }

  .recommendation-item::before {
    content: '💡 ';
  }

  .no-bottlenecks {
    padding: 1rem;
    text-align: center;
    color: var(--text-secondary, #666);
    font-size: 0.9rem;
  }
</style>

<div class="profiler-panel">
  <!-- Header -->
  <div class="profiler-header">
    <div class="header-title">📈 Performance Profiler</div>
    <div class="header-controls">
      <button
        class="control-button"
        class:active={autoRefresh}
        on:click={() => {
          autoRefresh = !autoRefresh;
          if (autoRefresh) startAutoRefresh();
          else stopAutoRefresh();
        }}
      >
        {autoRefresh ? '⏸' : '▶'} Auto
      </button>
      <button class="control-button" on:click={updateMetrics}>
        🔄 Refresh
      </button>
      <button class="control-button" on:click={resetMetrics}>
        ♻️ Reset
      </button>
    </div>
  </div>

  <!-- Content -->
  <div class="profiler-content">
    <!-- Metrics Grid -->
    <div class="metrics-grid">
      <div class="metric-card">
        <div class="metric-label">FPS</div>
        <div class="metric-value">
          {metrics.fps}
          <span class="metric-unit">fps</span>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Update Rate</div>
        <div class="metric-value">
          {metrics.updateRate}
          <span class="metric-unit">/s</span>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Avg Cycle Time</div>
        <div class="metric-value">
          {metrics.avgCycleTime.toFixed(2)}
          <span class="metric-unit">ms</span>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Total Cycles</div>
        <div class="metric-value">
          {metrics.totalCycles}
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Min Cycle Time</div>
        <div class="metric-value">
          {metrics.minCycleTime === Infinity ? '—' : metrics.minCycleTime.toFixed(2)}
          <span class="metric-unit">ms</span>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Max Cycle Time</div>
        <div class="metric-value">
          {metrics.maxCycleTime.toFixed(2)}
          <span class="metric-unit">ms</span>
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Dropped Updates</div>
        <div class="metric-value">
          {metrics.droppedFrames}
        </div>
      </div>

      <div class="metric-card">
        <div class="metric-label">Memory Usage</div>
        <div class="metric-value">
          {metrics.memoryUsage.toFixed(0)}
          <span class="metric-unit">MB</span>
        </div>
      </div>
    </div>

    <!-- Cycle Time Histogram -->
    {#if histogramData.length > 0}
      <div class="section-title">Cycle Time Distribution</div>
      <div class="histogram">
        <div class="histogram-bars">
          {#each histogramData as bin}
            <div
              class="histogram-bar"
              style="height: {(bin.count / maxHistogramCount) * 100}%"
              title="{bin.min.toFixed(1)}ms - {bin.max.toFixed(1)}ms: {bin.count} samples"
            ></div>
          {/each}
        </div>
        <div class="histogram-label">
          {metrics.minCycleTime === Infinity ? '—' : metrics.minCycleTime.toFixed(1)}ms → {metrics.maxCycleTime.toFixed(1)}ms
        </div>
      </div>
    {/if}

    <!-- Bottlenecks -->
    {#if bottlenecks.length > 0}
      <div class="section-title">Detected Bottlenecks</div>
      <div class="bottlenecks">
        {#each bottlenecks as bottleneck}
          <div class="bottleneck-item">{bottleneck}</div>
        {/each}
      </div>
    {:else}
      <div class="no-bottlenecks">
        ✓ No performance bottlenecks detected
      </div>
    {/if}

    <!-- Recommendations -->
    <div class="section-title">Recommendations</div>
    <div class="recommendations">
      {#each recommendations as recommendation}
        <div class="recommendation-item">{recommendation}</div>
      {/each}
    </div>
  </div>
</div>
