<!--
  HotSpotAnalyzer.svelte
  Identifies computational hot spots and bottlenecks in emulator execution

  Features:
  - Top 10 slowest cycles
  - Phase time distribution
  - Bottleneck operations
  - Optimization suggestions
  - Drill-down analysis
-->
<script lang="ts">
  import { machineStateStore } from '$lib/visualization/state/MachineStateStore';
  import type { EmulatorStateBridge } from '$lib/visualization/integration/EmulatorStateBridge';

  // Props
  export let bridge: EmulatorStateBridge | null = null;
  export let topN = 10;

  // State
  let hotSpots: HotSpot[] = [];
  let phaseDistribution: PhaseStats[] = [];
  let bottlenecks: Bottleneck[] = [];
  let optimizationSuggestions: string[] = [];
  let selectedHotSpot: HotSpot | null = null;

  // Types
  interface HotSpot {
    cycle: number;
    duration: number;
    phase: string;
    operation: string;
    percentage: number;
  }

  interface PhaseStats {
    phase: string;
    totalTime: number;
    count: number;
    avgTime: number;
    percentage: number;
    color: string;
  }

  interface Bottleneck {
    type: string;
    description: string;
    severity: 'high' | 'medium' | 'low';
    cycles: number[];
    suggestion: string;
  }

  // Phase colors (consistent with ExecutionTimeline)
  const phaseColors: Record<string, string> = {
    'fetch': '#3498db',
    'decode': '#9b59b6',
    'execute': '#e74c3c',
    'writeback': '#2ecc71',
    'idle': '#95a5a6',
    'memory': '#f39c12',
    'arithmetic': '#e67e22',
    'control': '#1abc9c'
  };

  // Reactive analysis
  $: history = machineStateStore.getHistory();
  $: if (history.length > 0) {
    analyzeHotSpots(history);
  }

  /**
   * Analyze execution history for hot spots
   */
  function analyzeHotSpots(history: any[]) {
    // Calculate cycle durations
    const cycleTimes = history.map((entry, index) => {
      if (index === 0) {
        return {
          cycle: entry.state.cycles,
          duration: 0,
          phase: entry.state.phase || 'idle',
          operation: 'start',
          timestamp: entry.timestamp
        };
      }

      return {
        cycle: entry.state.cycles,
        duration: entry.timestamp - history[index - 1].timestamp,
        phase: entry.state.phase || 'idle',
        operation: entry.state.mill?.operation || 'idle',
        timestamp: entry.timestamp
      };
    });

    // Find total execution time
    const totalTime = cycleTimes.reduce((sum, ct) => sum + ct.duration, 0);

    // Identify top N slowest cycles
    hotSpots = cycleTimes
      .filter(ct => ct.duration > 0)
      .sort((a, b) => b.duration - a.duration)
      .slice(0, topN)
      .map(ct => ({
        cycle: ct.cycle,
        duration: ct.duration,
        phase: ct.phase,
        operation: ct.operation,
        percentage: (ct.duration / totalTime) * 100
      }));

    // Calculate phase distribution
    const phaseMap = new Map<string, { time: number; count: number }>();

    cycleTimes.forEach(ct => {
      const existing = phaseMap.get(ct.phase) || { time: 0, count: 0 };
      phaseMap.set(ct.phase, {
        time: existing.time + ct.duration,
        count: existing.count + 1
      });
    });

    phaseDistribution = Array.from(phaseMap.entries())
      .map(([phase, stats]) => ({
        phase,
        totalTime: stats.time,
        count: stats.count,
        avgTime: stats.time / stats.count,
        percentage: (stats.time / totalTime) * 100,
        color: phaseColors[phase] || phaseColors['idle']
      }))
      .sort((a, b) => b.totalTime - a.totalTime);

    // Detect bottlenecks
    detectBottlenecks(cycleTimes, phaseDistribution);

    // Generate optimization suggestions
    generateSuggestions(hotSpots, phaseDistribution, bottlenecks);
  }

  /**
   * Detect bottlenecks in execution
   */
  function detectBottlenecks(
    cycleTimes: any[],
    phaseStats: PhaseStats[]
  ) {
    bottlenecks = [];

    // Check for slow phases
    phaseStats.forEach(stats => {
      if (stats.avgTime > 50) { // >50ms average
        bottlenecks.push({
          type: `Slow ${stats.phase} phase`,
          description: `Average ${stats.phase} time is ${stats.avgTime.toFixed(2)}ms`,
          severity: stats.avgTime > 100 ? 'high' : 'medium',
          cycles: cycleTimes
            .filter(ct => ct.phase === stats.phase && ct.duration > stats.avgTime)
            .map(ct => ct.cycle),
          suggestion: `Optimize ${stats.phase} operations or reduce frequency`
        });
      }
    });

    // Check for memory-heavy operations
    const memoryPhase = phaseStats.find(p => p.phase === 'memory');
    if (memoryPhase && memoryPhase.percentage > 40) {
      bottlenecks.push({
        type: 'Memory bottleneck',
        description: `${memoryPhase.percentage.toFixed(1)}% of time spent in memory operations`,
        severity: 'high',
        cycles: cycleTimes
          .filter(ct => ct.phase === 'memory')
          .map(ct => ct.cycle),
        suggestion: 'Consider caching frequently accessed memory or reducing memory operations'
      });
    }

    // Check for arithmetic-heavy operations
    const arithmeticPhase = phaseStats.find(p => p.phase === 'arithmetic');
    if (arithmeticPhase && arithmeticPhase.percentage > 50) {
      bottlenecks.push({
        type: 'Arithmetic bottleneck',
        description: `${arithmeticPhase.percentage.toFixed(1)}% of time in arithmetic`,
        severity: 'medium',
        cycles: cycleTimes
          .filter(ct => ct.phase === 'arithmetic')
          .map(ct => ct.cycle),
        suggestion: 'Verify arithmetic operations are necessary; consider look-up tables'
      });
    }

    // Check for execution variance
    if (hotSpots.length > 0) {
      const maxTime = hotSpots[0].duration;
      const avgTime = cycleTimes.reduce((sum, ct) => sum + ct.duration, 0) / cycleTimes.length;

      if (maxTime > avgTime * 10) {
        bottlenecks.push({
          type: 'High execution variance',
          description: `Some cycles take ${(maxTime / avgTime).toFixed(1)}x longer than average`,
          severity: 'medium',
          cycles: hotSpots.map(hs => hs.cycle),
          suggestion: 'Investigate outlier cycles for optimization opportunities'
        });
      }
    }

    // Sort by severity
    bottlenecks.sort((a, b) => {
      const severityOrder = { high: 0, medium: 1, low: 2 };
      return severityOrder[a.severity] - severityOrder[b.severity];
    });
  }

  /**
   * Generate optimization suggestions
   */
  function generateSuggestions(
    hotSpots: HotSpot[],
    phaseStats: PhaseStats[],
    bottlenecks: Bottleneck[]
  ) {
    optimizationSuggestions = [];

    if (bottlenecks.length === 0) {
      optimizationSuggestions.push('✓ No major bottlenecks detected');
      optimizationSuggestions.push('✓ Execution profile looks good');
      return;
    }

    // Add suggestions from bottlenecks
    bottlenecks.forEach(b => {
      optimizationSuggestions.push(`• ${b.suggestion}`);
    });

    // Add general suggestions
    if (hotSpots.length > 0 && hotSpots[0].percentage > 20) {
      optimizationSuggestions.push(
        `• Focus optimization on cycle ${hotSpots[0].cycle} (${hotSpots[0].percentage.toFixed(1)}% of time)`
      );
    }

    const topPhase = phaseStats[0];
    if (topPhase && topPhase.percentage > 60) {
      optimizationSuggestions.push(
        `• ${topPhase.phase} dominates (${topPhase.percentage.toFixed(1)}%) - prioritize optimization here`
      );
    }
  }

  /**
   * Select hot spot for detailed view
   */
  function selectHotSpot(hotSpot: HotSpot) {
    selectedHotSpot = hotSpot;

    // Rewind to this cycle
    const history = machineStateStore.getHistory();
    const entry = history.find(h => h.state.cycles === hotSpot.cycle);
    if (entry) {
      const index = history.indexOf(entry);
      machineStateStore.rewindToHistoryEntry(index);
    }
  }

  /**
   * Get severity color
   */
  function getSeverityColor(severity: string): string {
    switch (severity) {
      case 'high': return '#f44336';
      case 'medium': return '#ff9800';
      case 'low': return '#ffc107';
      default: return '#9e9e9e';
    }
  }
</script>

<style>
  .hotspot-analyzer {
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    padding: 1rem;
    font-family: 'Courier New', monospace;
  }

  .analyzer-header {
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-color, #333);
  }

  .header-title {
    font-size: 1rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
  }

  .section {
    margin-bottom: 1.5rem;
  }

  .section-title {
    font-size: 0.9rem;
    font-weight: bold;
    color: var(--text-secondary, #999);
    margin-bottom: 0.75rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .hotspot-list {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .hotspot-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    background: var(--item-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
    cursor: pointer;
    transition: all 0.2s;
  }

  .hotspot-item:hover {
    background: var(--item-hover-bg, #1a1a1a);
    border-color: var(--border-hover-color, #666);
  }

  .hotspot-item.selected {
    background: var(--item-selected-bg, #1e3a5f);
    border-color: var(--item-selected-border, #3a7bc8);
  }

  .hotspot-info {
    flex: 1;
  }

  .hotspot-cycle {
    font-size: 0.9rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .hotspot-details {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
    margin-top: 0.2rem;
  }

  .hotspot-metrics {
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    gap: 0.2rem;
  }

  .hotspot-duration {
    font-size: 0.9rem;
    color: var(--text-primary, #ff9800);
    font-weight: bold;
  }

  .hotspot-percentage {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
  }

  .phase-stats {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    gap: 0.75rem;
  }

  .phase-stat {
    background: var(--stat-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
  }

  .phase-header {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
  }

  .phase-color {
    width: 12px;
    height: 12px;
    border-radius: 3px;
  }

  .phase-name {
    font-size: 0.85rem;
    color: var(--text-primary, #e0e0e0);
    font-weight: bold;
  }

  .phase-metrics {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
  }

  .phase-metric-row {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.2rem;
  }

  .bottleneck-list {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .bottleneck-item {
    background: var(--bottleneck-bg, #0a0a0a);
    border-left: 4px solid;
    border-radius: 4px;
    padding: 0.75rem;
  }

  .bottleneck-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.5rem;
  }

  .bottleneck-type {
    font-size: 0.9rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
  }

  .bottleneck-severity {
    padding: 0.2rem 0.5rem;
    border-radius: 3px;
    font-size: 0.7rem;
    font-weight: bold;
    text-transform: uppercase;
  }

  .bottleneck-description {
    font-size: 0.85rem;
    color: var(--text-secondary, #999);
    margin-bottom: 0.5rem;
  }

  .bottleneck-suggestion {
    font-size: 0.8rem;
    color: var(--text-primary, #4caf50);
    font-style: italic;
  }

  .bottleneck-suggestion::before {
    content: '💡 ';
  }

  .suggestions-list {
    background: var(--suggestions-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
  }

  .suggestion-item {
    padding: 0.4rem 0;
    font-size: 0.85rem;
    color: var(--text-primary, #8bc34a);
  }

  .empty-message {
    padding: 2rem;
    text-align: center;
    color: var(--text-secondary, #666);
    font-size: 0.9rem;
  }
</style>

<div class="hotspot-analyzer">
  <!-- Header -->
  <div class="analyzer-header">
    <div class="header-title">🔥 Hot Spot Analyzer</div>
  </div>

  {#if hotSpots.length === 0}
    <div class="empty-message">
      No execution data to analyze.<br/>
      Run a program to identify hot spots.
    </div>
  {:else}
    <!-- Top Slowest Cycles -->
    <div class="section">
      <div class="section-title">Top {topN} Slowest Cycles</div>
      <div class="hotspot-list">
        {#each hotSpots as hotSpot}
          <div
            class="hotspot-item"
            class:selected={selectedHotSpot?.cycle === hotSpot.cycle}
            on:click={() => selectHotSpot(hotSpot)}
          >
            <div class="hotspot-info">
              <div class="hotspot-cycle">Cycle {hotSpot.cycle}</div>
              <div class="hotspot-details">
                {hotSpot.phase} • {hotSpot.operation}
              </div>
            </div>
            <div class="hotspot-metrics">
              <div class="hotspot-duration">{hotSpot.duration.toFixed(2)}ms</div>
              <div class="hotspot-percentage">{hotSpot.percentage.toFixed(1)}%</div>
            </div>
          </div>
        {/each}
      </div>
    </div>

    <!-- Phase Distribution -->
    <div class="section">
      <div class="section-title">Phase Time Distribution</div>
      <div class="phase-stats">
        {#each phaseDistribution as phaseStat}
          <div class="phase-stat">
            <div class="phase-header">
              <div class="phase-color" style="background: {phaseStat.color}"></div>
              <div class="phase-name">{phaseStat.phase}</div>
            </div>
            <div class="phase-metrics">
              <div class="phase-metric-row">
                <span>Total:</span>
                <span>{phaseStat.totalTime.toFixed(2)}ms</span>
              </div>
              <div class="phase-metric-row">
                <span>Average:</span>
                <span>{phaseStat.avgTime.toFixed(2)}ms</span>
              </div>
              <div class="phase-metric-row">
                <span>Count:</span>
                <span>{phaseStat.count}</span>
              </div>
              <div class="phase-metric-row">
                <span>Percentage:</span>
                <span>{phaseStat.percentage.toFixed(1)}%</span>
              </div>
            </div>
          </div>
        {/each}
      </div>
    </div>

    <!-- Bottlenecks -->
    {#if bottlenecks.length > 0}
      <div class="section">
        <div class="section-title">Detected Bottlenecks</div>
        <div class="bottleneck-list">
          {#each bottlenecks as bottleneck}
            <div
              class="bottleneck-item"
              style="border-left-color: {getSeverityColor(bottleneck.severity)}"
            >
              <div class="bottleneck-header">
                <div class="bottleneck-type">{bottleneck.type}</div>
                <div
                  class="bottleneck-severity"
                  style="background: {getSeverityColor(bottleneck.severity)}; color: white"
                >
                  {bottleneck.severity}
                </div>
              </div>
              <div class="bottleneck-description">{bottleneck.description}</div>
              <div class="bottleneck-suggestion">{bottleneck.suggestion}</div>
            </div>
          {/each}
        </div>
      </div>
    {/if}

    <!-- Optimization Suggestions -->
    <div class="section">
      <div class="section-title">Optimization Suggestions</div>
      <div class="suggestions-list">
        {#each optimizationSuggestions as suggestion}
          <div class="suggestion-item">{suggestion}</div>
        {/each}
      </div>
    </div>
  {/if}
</div>
