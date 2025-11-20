<!--
  EmulatorControls.svelte
  Control panel for Babbage Analytical Engine emulator

  Features:
  - Play/Pause/Step/Reset controls
  - Speed adjustment
  - Breakpoint management
  - Connection status
  - Performance metrics display
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import type { EmulatorStateBridge } from '$lib/visualization/integration/EmulatorStateBridge';
  import { executionStateStore } from '$lib/visualization/state/MachineStateStore';
  import { connectionStatusStore } from '$lib/visualization/websocket';

  // Props
  export let bridge: EmulatorStateBridge | null = null;
  export let showAdvanced = false;

  // State
  let isRunning = false;
  let isPaused = false;
  let executionSpeed = 1.0;
  let polynomial: number[] = [1, 0, 0]; // x^2
  let xStart = 0;
  let xEnd = 10;
  let breakpoints: Set<number> = new Set();
  let newBreakpoint = '';
  let performanceMetrics: any = null;
  let metricsInterval: number | null = null;

  // Subscribe to execution state
  $: programCounter = $executionStateStore.programCounter;
  $: accumulator = $executionStateStore.accumulator;
  $: carryFlag = $executionStateStore.carryFlag;
  $: runningFlag = $executionStateStore.runningFlag;
  $: cycles = $executionStateStore.cycles;
  $: connectionStatus = $connectionStatusStore;

  onMount(() => {
    // Update performance metrics periodically
    metricsInterval = window.setInterval(() => {
      if (bridge) {
        performanceMetrics = bridge.getPerformanceMetrics();
      }
    }, 1000);
  });

  onDestroy(() => {
    if (metricsInterval !== null) {
      clearInterval(metricsInterval);
    }
  });

  /**
   * Execute program
   */
  function handleExecute() {
    if (!bridge) return;

    bridge.execute(polynomial, [xStart, xEnd]);
    isRunning = true;
    isPaused = false;
  }

  /**
   * Pause execution
   */
  function handlePause() {
    if (!bridge) return;

    bridge.pause();
    isPaused = true;
  }

  /**
   * Resume execution
   */
  function handleResume() {
    if (!bridge) return;

    bridge.resume();
    isPaused = false;
  }

  /**
   * Step one cycle
   */
  function handleStep() {
    if (!bridge) return;

    bridge.stepCycle();
  }

  /**
   * Reset emulator
   */
  function handleReset() {
    if (!bridge) return;

    bridge.reset();
    isRunning = false;
    isPaused = false;
  }

  /**
   * Add breakpoint
   */
  function handleAddBreakpoint() {
    if (!bridge || !newBreakpoint) return;

    const cycle = parseInt(newBreakpoint, 10);
    if (isNaN(cycle) || cycle < 0) return;

    bridge.setBreakpoint(cycle);
    breakpoints.add(cycle);
    breakpoints = breakpoints; // Trigger reactivity
    newBreakpoint = '';
  }

  /**
   * Remove breakpoint
   */
  function handleRemoveBreakpoint(cycle: number) {
    if (!bridge) return;

    bridge.removeBreakpoint(cycle);
    breakpoints.delete(cycle);
    breakpoints = breakpoints; // Trigger reactivity
  }

  /**
   * Request snapshot
   */
  function handleRequestSnapshot() {
    if (!bridge) return;
    bridge.requestSnapshot();
  }

  /**
   * Format polynomial for display
   */
  function formatPolynomial(coeffs: number[]): string {
    return coeffs
      .map((coeff, i) => {
        const power = coeffs.length - 1 - i;
        if (coeff === 0) return '';
        const sign = coeff > 0 && i > 0 ? '+' : '';
        const coeffStr = Math.abs(coeff) === 1 && power > 0 ? '' : Math.abs(coeff).toString();
        const varStr = power === 0 ? '' : power === 1 ? 'x' : `x^${power}`;
        return `${sign}${coeff < 0 ? '-' : ''}${coeffStr}${varStr}`;
      })
      .filter(s => s !== '')
      .join(' ') || '0';
  }
</script>

<style>
  .emulator-controls {
    background: var(--control-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    padding: 1rem;
    font-family: 'Courier New', monospace;
  }

  .controls-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
    padding-bottom: 0.5rem;
    border-bottom: 1px solid var(--border-color, #333);
  }

  .controls-title {
    font-size: 1.1rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
  }

  .connection-status {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.9rem;
  }

  .status-indicator {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    animation: pulse 2s infinite;
  }

  .status-connected {
    background-color: #4caf50;
  }

  .status-connecting {
    background-color: #ff9800;
  }

  .status-disconnected {
    background-color: #f44336;
  }

  .status-error {
    background-color: #f44336;
    animation: pulse 0.5s infinite;
  }

  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.5; }
  }

  .control-section {
    margin-bottom: 1rem;
  }

  .section-title {
    font-size: 0.9rem;
    font-weight: bold;
    color: var(--text-secondary, #999);
    margin-bottom: 0.5rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .button-group {
    display: flex;
    gap: 0.5rem;
    flex-wrap: wrap;
  }

  .control-button {
    padding: 0.5rem 1rem;
    background: var(--button-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    font-family: inherit;
    font-size: 0.9rem;
    transition: all 0.2s;
  }

  .control-button:hover:not(:disabled) {
    background: var(--button-hover-bg, #3a3a3a);
    border-color: var(--border-hover-color, #666);
  }

  .control-button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .control-button.primary {
    background: #4caf50;
    border-color: #4caf50;
    color: white;
  }

  .control-button.primary:hover:not(:disabled) {
    background: #45a049;
  }

  .control-button.danger {
    background: #f44336;
    border-color: #f44336;
    color: white;
  }

  .control-button.danger:hover:not(:disabled) {
    background: #da190b;
  }

  .input-group {
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }

  .input-row {
    display: flex;
    gap: 0.5rem;
    align-items: center;
  }

  .input-label {
    min-width: 100px;
    font-size: 0.9rem;
    color: var(--text-secondary, #999);
  }

  .input-field {
    flex: 1;
    padding: 0.4rem;
    background: var(--input-bg, #222);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    font-family: inherit;
    font-size: 0.9rem;
  }

  .input-field:focus {
    outline: none;
    border-color: var(--border-focus-color, #666);
  }

  .state-display {
    background: var(--display-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
    font-family: 'Courier New', monospace;
  }

  .state-row {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.4rem;
    font-size: 0.9rem;
  }

  .state-label {
    color: var(--text-secondary, #999);
  }

  .state-value {
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .breakpoint-list {
    max-height: 120px;
    overflow-y: auto;
    background: var(--display-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.5rem;
  }

  .breakpoint-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.4rem;
    margin-bottom: 0.3rem;
    background: var(--item-bg, #1a1a1a);
    border-radius: 3px;
  }

  .breakpoint-remove {
    background: none;
    border: none;
    color: #f44336;
    cursor: pointer;
    font-size: 1.2rem;
    padding: 0 0.5rem;
  }

  .breakpoint-remove:hover {
    color: #da190b;
  }

  .polynomial-display {
    background: var(--display-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.75rem;
    font-family: 'Courier New', monospace;
    font-size: 1.1rem;
    text-align: center;
    color: var(--text-primary, #0f0);
    margin-top: 0.5rem;
  }

  .metrics-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 0.5rem;
    font-size: 0.85rem;
  }

  .metric {
    background: var(--display-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.5rem;
  }

  .metric-label {
    color: var(--text-secondary, #999);
    font-size: 0.75rem;
    margin-bottom: 0.2rem;
  }

  .metric-value {
    color: var(--text-primary, #0f0);
    font-weight: bold;
    font-size: 1.1rem;
  }
</style>

<div class="emulator-controls">
  <!-- Header -->
  <div class="controls-header">
    <div class="controls-title">⚙️ Analytical Engine Controls</div>
    <div class="connection-status">
      <div
        class="status-indicator"
        class:status-connected={connectionStatus === 'connected'}
        class:status-connecting={connectionStatus === 'connecting'}
        class:status-disconnected={connectionStatus === 'disconnected'}
        class:status-error={connectionStatus === 'error'}
      ></div>
      <span>{connectionStatus}</span>
    </div>
  </div>

  <!-- Execution Controls -->
  <div class="control-section">
    <div class="section-title">Execution</div>
    <div class="button-group">
      <button
        class="control-button primary"
        on:click={handleExecute}
        disabled={!bridge || isRunning || connectionStatus !== 'connected'}
      >
        ▶ Execute
      </button>
      <button
        class="control-button"
        on:click={handlePause}
        disabled={!bridge || !isRunning || isPaused}
      >
        ⏸ Pause
      </button>
      <button
        class="control-button"
        on:click={handleResume}
        disabled={!bridge || !isPaused}
      >
        ▶ Resume
      </button>
      <button
        class="control-button"
        on:click={handleStep}
        disabled={!bridge || connectionStatus !== 'connected'}
      >
        ⏭ Step
      </button>
      <button
        class="control-button danger"
        on:click={handleReset}
        disabled={!bridge}
      >
        ⏹ Reset
      </button>
    </div>
  </div>

  <!-- Program Input -->
  <div class="control-section">
    <div class="section-title">Program</div>
    <div class="input-group">
      <div class="input-row">
        <span class="input-label">Polynomial:</span>
        <input
          class="input-field"
          type="text"
          bind:value={polynomial}
          placeholder="[1, 0, 0] for x^2"
          disabled={isRunning}
        />
      </div>
      <div class="input-row">
        <span class="input-label">X Start:</span>
        <input
          class="input-field"
          type="number"
          bind:value={xStart}
          disabled={isRunning}
        />
      </div>
      <div class="input-row">
        <span class="input-label">X End:</span>
        <input
          class="input-field"
          type="number"
          bind:value={xEnd}
          disabled={isRunning}
        />
      </div>
    </div>
    <div class="polynomial-display">
      f(x) = {formatPolynomial(polynomial)}
    </div>
  </div>

  <!-- Machine State Display -->
  <div class="control-section">
    <div class="section-title">Machine State</div>
    <div class="state-display">
      <div class="state-row">
        <span class="state-label">Program Counter:</span>
        <span class="state-value">{programCounter}</span>
      </div>
      <div class="state-row">
        <span class="state-label">Accumulator:</span>
        <span class="state-value">{accumulator}</span>
      </div>
      <div class="state-row">
        <span class="state-label">Carry Flag:</span>
        <span class="state-value">{carryFlag ? '1' : '0'}</span>
      </div>
      <div class="state-row">
        <span class="state-label">Running:</span>
        <span class="state-value">{runningFlag ? 'YES' : 'NO'}</span>
      </div>
      <div class="state-row">
        <span class="state-label">Cycles:</span>
        <span class="state-value">{cycles}</span>
      </div>
    </div>
  </div>

  {#if showAdvanced}
    <!-- Breakpoints -->
    <div class="control-section">
      <div class="section-title">Breakpoints</div>
      <div class="input-row">
        <input
          class="input-field"
          type="number"
          bind:value={newBreakpoint}
          placeholder="Cycle number"
        />
        <button class="control-button" on:click={handleAddBreakpoint}>
          + Add
        </button>
      </div>
      {#if breakpoints.size > 0}
        <div class="breakpoint-list">
          {#each Array.from(breakpoints).sort((a, b) => a - b) as bp}
            <div class="breakpoint-item">
              <span>Cycle {bp}</span>
              <button
                class="breakpoint-remove"
                on:click={() => handleRemoveBreakpoint(bp)}
              >
                ×
              </button>
            </div>
          {/each}
        </div>
      {/if}
    </div>

    <!-- Performance Metrics -->
    {#if performanceMetrics}
      <div class="control-section">
        <div class="section-title">Performance</div>
        <div class="metrics-grid">
          <div class="metric">
            <div class="metric-label">Updates Received</div>
            <div class="metric-value">{performanceMetrics.updatesReceived}</div>
          </div>
          <div class="metric">
            <div class="metric-label">Updates Processed</div>
            <div class="metric-value">{performanceMetrics.updatesProcessed}</div>
          </div>
          <div class="metric">
            <div class="metric-label">Avg Update Time</div>
            <div class="metric-value">
              {performanceMetrics.averageUpdateTime.toFixed(2)}ms
            </div>
          </div>
          <div class="metric">
            <div class="metric-label">Dropped Updates</div>
            <div class="metric-value">{performanceMetrics.droppedUpdates}</div>
          </div>
        </div>
      </div>
    {/if}

    <!-- Advanced Actions -->
    <div class="control-section">
      <div class="section-title">Advanced</div>
      <div class="button-group">
        <button
          class="control-button"
          on:click={handleRequestSnapshot}
          disabled={!bridge || connectionStatus !== 'connected'}
        >
          📸 Request Snapshot
        </button>
      </div>
    </div>
  {/if}
</div>
