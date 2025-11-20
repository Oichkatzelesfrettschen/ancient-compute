<!--
  DebuggerPanel.svelte
  Interactive debugger for Babbage Analytical Engine emulator

  Features:
  - Execution history timeline
  - Breakpoint visualization
  - Step-by-step execution
  - State diff highlighting
  - Call stack (program barrels)
  - Watch expressions
-->
<script lang="ts">
  import { onMount } from 'svelte';
  import type { EmulatorStateBridge } from '$lib/visualization/integration/EmulatorStateBridge';
  import { machineStateStore, type StateHistoryEntry } from '$lib/visualization/state/MachineStateStore';

  // Props
  export let bridge: EmulatorStateBridge | null = null;
  export let height = 400;

  // State
  let history: StateHistoryEntry[] = [];
  let selectedHistoryIndex = -1;
  let breakpoints: Map<number, boolean> = new Map(); // cycle -> enabled
  let watchExpressions: string[] = [];
  let newWatch = '';
  let isStepMode = false;
  let showDiff = true;

  // Reactive
  $: currentState = $machineStateStore;
  $: if (currentState) {
    updateHistory();
  }

  onMount(() => {
    loadBreakpoints();
  });

  /**
   * Update execution history from store
   */
  function updateHistory() {
    history = machineStateStore.getHistory();

    // Auto-select latest if nothing selected
    if (selectedHistoryIndex === -1 && history.length > 0) {
      selectedHistoryIndex = history.length - 1;
    }
  }

  /**
   * Select history entry for inspection
   */
  function selectHistoryEntry(index: number) {
    selectedHistoryIndex = index;

    // Optionally rewind visualization
    if (index >= 0 && index < history.length) {
      machineStateStore.rewindToHistoryEntry(index);
    }
  }

  /**
   * Step forward one cycle
   */
  function stepForward() {
    if (!bridge) return;

    bridge.stepCycle();
    isStepMode = true;
  }

  /**
   * Continue execution (disable step mode)
   */
  function continueExecution() {
    if (!bridge) return;

    bridge.resume();
    isStepMode = false;
  }

  /**
   * Toggle breakpoint at cycle
   */
  function toggleBreakpoint(cycle: number) {
    if (!bridge) return;

    const isEnabled = breakpoints.get(cycle) ?? false;

    if (isEnabled) {
      bridge.removeBreakpoint(cycle);
      breakpoints.delete(cycle);
    } else {
      bridge.setBreakpoint(cycle);
      breakpoints.set(cycle, true);
    }

    breakpoints = breakpoints; // Trigger reactivity
    saveBreakpoints();
  }

  /**
   * Add watch expression
   */
  function addWatchExpression() {
    if (!newWatch.trim()) return;

    watchExpressions = [...watchExpressions, newWatch.trim()];
    newWatch = '';
    saveWatchExpressions();
  }

  /**
   * Remove watch expression
   */
  function removeWatchExpression(index: number) {
    watchExpressions = watchExpressions.filter((_, i) => i !== index);
    saveWatchExpressions();
  }

  /**
   * Evaluate watch expression against current state
   */
  function evaluateWatch(expr: string): string {
    if (!currentState) return 'N/A';

    try {
      // Simple expression evaluation (could be enhanced with proper parser)
      if (expr.startsWith('register[')) {
        const regName = expr.match(/register\[['"](.+?)['"]\]/)?.[1];
        if (regName) {
          return currentState.registers.get(regName)?.toString() ?? 'undefined';
        }
      } else if (expr.startsWith('column[')) {
        const colIndex = parseInt(expr.match(/column\[(\d+)\]/)?.[1] ?? '-1', 10);
        if (colIndex >= 0 && colIndex < currentState.columns.length) {
          return currentState.columns[colIndex].value.toString();
        }
      } else if (expr.startsWith('store[')) {
        const storeIndex = parseInt(expr.match(/store\[(\d+)\]/)?.[1] ?? '-1', 10);
        if (storeIndex >= 0 && storeIndex < currentState.store.positions.length) {
          return currentState.store.positions[storeIndex].toString();
        }
      } else if (expr === 'pc' || expr === 'programCounter') {
        return currentState.programCounter.toString();
      } else if (expr === 'acc' || expr === 'accumulator') {
        return currentState.accumulator.toString();
      } else if (expr === 'cycles') {
        return currentState.cycles.toString();
      }

      return 'Invalid expression';
    } catch (error) {
      return 'Error';
    }
  }

  /**
   * Get diff highlighting class for history entry
   */
  function getDiffClass(index: number): string {
    if (!showDiff || index === 0) return '';

    const current = history[index];
    const previous = history[index - 1];

    if (!current || !previous) return '';

    // Check if significant state changed
    const cyclesChanged = current.state.cycles !== previous.state.cycles;
    const pcChanged = current.state.programCounter !== previous.state.programCounter;
    const accChanged = current.state.accumulator !== previous.state.accumulator;

    if (cyclesChanged || pcChanged || accChanged) {
      return 'state-changed';
    }

    return '';
  }

  /**
   * Load breakpoints from localStorage
   */
  function loadBreakpoints() {
    try {
      const saved = localStorage.getItem('debugger-breakpoints');
      if (saved) {
        const parsed = JSON.parse(saved);
        breakpoints = new Map(Object.entries(parsed).map(([k, v]) => [parseInt(k, 10), v as boolean]));
      }
    } catch (error) {
      console.warn('Failed to load breakpoints:', error);
    }
  }

  /**
   * Save breakpoints to localStorage
   */
  function saveBreakpoints() {
    try {
      const obj = Object.fromEntries(breakpoints.entries());
      localStorage.setItem('debugger-breakpoints', JSON.stringify(obj));
    } catch (error) {
      console.warn('Failed to save breakpoints:', error);
    }
  }

  /**
   * Load watch expressions from localStorage
   */
  function loadWatchExpressions() {
    try {
      const saved = localStorage.getItem('debugger-watch');
      if (saved) {
        watchExpressions = JSON.parse(saved);
      }
    } catch (error) {
      console.warn('Failed to load watch expressions:', error);
    }
  }

  /**
   * Save watch expressions to localStorage
   */
  function saveWatchExpressions() {
    try {
      localStorage.setItem('debugger-watch', JSON.stringify(watchExpressions));
    } catch (error) {
      console.warn('Failed to save watch expressions:', error);
    }
  }

  /**
   * Clear execution history
   */
  function clearHistory() {
    machineStateStore.clearHistory();
    history = [];
    selectedHistoryIndex = -1;
  }

  /**
   * Format timestamp for display
   */
  function formatTimestamp(timestamp: number): string {
    const date = new Date(timestamp);
    return date.toLocaleTimeString('en-US', {
      hour12: false,
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      fractionalSecondDigits: 3
    });
  }
</script>

<style>
  .debugger-panel {
    display: flex;
    flex-direction: column;
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    font-family: 'Courier New', monospace;
    overflow: hidden;
  }

  .debugger-header {
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

  .debug-button {
    padding: 0.4rem 0.8rem;
    background: var(--button-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    font-family: inherit;
    font-size: 0.85rem;
    transition: all 0.2s;
  }

  .debug-button:hover:not(:disabled) {
    background: var(--button-hover-bg, #3a3a3a);
  }

  .debug-button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .debug-button.active {
    background: #4caf50;
    border-color: #4caf50;
  }

  .debugger-content {
    display: flex;
    flex: 1;
    overflow: hidden;
  }

  .history-panel {
    flex: 2;
    display: flex;
    flex-direction: column;
    border-right: 1px solid var(--border-color, #333);
  }

  .history-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem 1rem;
    background: var(--subheader-bg, #151515);
    border-bottom: 1px solid var(--border-color, #333);
    font-size: 0.85rem;
    color: var(--text-secondary, #999);
  }

  .history-list {
    flex: 1;
    overflow-y: auto;
    padding: 0.5rem;
  }

  .history-entry {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem;
    margin-bottom: 0.3rem;
    background: var(--entry-bg, #222);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s;
  }

  .history-entry:hover {
    background: var(--entry-hover-bg, #2a2a2a);
  }

  .history-entry.selected {
    background: var(--entry-selected-bg, #1e3a5f);
    border-color: var(--entry-selected-border, #3a7bc8);
  }

  .history-entry.state-changed {
    border-left: 3px solid #ff9800;
  }

  .history-entry-info {
    flex: 1;
  }

  .history-entry-cycle {
    font-size: 0.9rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .history-entry-time {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
  }

  .history-entry-breakpoint {
    width: 20px;
    height: 20px;
    border-radius: 50%;
    background: var(--breakpoint-bg, transparent);
    border: 2px solid var(--border-color, #666);
    cursor: pointer;
    transition: all 0.2s;
  }

  .history-entry-breakpoint.active {
    background: #f44336;
    border-color: #f44336;
  }

  .history-entry-breakpoint:hover {
    border-color: #f44336;
  }

  .watch-panel {
    flex: 1;
    display: flex;
    flex-direction: column;
  }

  .watch-header {
    padding: 0.5rem 1rem;
    background: var(--subheader-bg, #151515);
    border-bottom: 1px solid var(--border-color, #333);
    font-size: 0.85rem;
    color: var(--text-secondary, #999);
  }

  .watch-list {
    flex: 1;
    overflow-y: auto;
    padding: 0.5rem;
  }

  .watch-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.5rem;
    margin-bottom: 0.3rem;
    background: var(--entry-bg, #222);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
  }

  .watch-expression {
    flex: 1;
    font-size: 0.85rem;
    color: var(--text-secondary, #999);
  }

  .watch-value {
    font-size: 0.9rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
    margin: 0 0.5rem;
  }

  .watch-remove {
    background: none;
    border: none;
    color: #f44336;
    cursor: pointer;
    font-size: 1.1rem;
    padding: 0 0.5rem;
  }

  .watch-remove:hover {
    color: #da190b;
  }

  .watch-input {
    display: flex;
    gap: 0.5rem;
    padding: 0.5rem;
    border-top: 1px solid var(--border-color, #333);
  }

  .watch-input input {
    flex: 1;
    padding: 0.4rem;
    background: var(--input-bg, #0a0a0a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    font-family: inherit;
    font-size: 0.85rem;
  }

  .watch-input input:focus {
    outline: none;
    border-color: var(--border-focus-color, #666);
  }

  .empty-message {
    padding: 2rem;
    text-align: center;
    color: var(--text-secondary, #666);
    font-size: 0.9rem;
  }
</style>

<div class="debugger-panel" style="height: {height}px">
  <!-- Header -->
  <div class="debugger-header">
    <div class="header-title">🐛 Debugger</div>
    <div class="header-controls">
      <button
        class="debug-button"
        class:active={isStepMode}
        on:click={stepForward}
        disabled={!bridge}
      >
        ⏭ Step
      </button>
      <button
        class="debug-button"
        on:click={continueExecution}
        disabled={!bridge || !isStepMode}
      >
        ▶ Continue
      </button>
      <button
        class="debug-button"
        on:click={clearHistory}
        disabled={history.length === 0}
      >
        🗑 Clear
      </button>
      <button
        class="debug-button"
        class:active={showDiff}
        on:click={() => showDiff = !showDiff}
      >
        {showDiff ? '🔍' : '👁'} Diff
      </button>
    </div>
  </div>

  <!-- Content -->
  <div class="debugger-content">
    <!-- Execution History -->
    <div class="history-panel">
      <div class="history-header">
        <span>Execution History ({history.length})</span>
      </div>
      <div class="history-list">
        {#if history.length === 0}
          <div class="empty-message">
            No execution history yet.<br/>
            Run a program to begin debugging.
          </div>
        {:else}
          {#each history as entry, index}
            <div
              class="history-entry"
              class:selected={index === selectedHistoryIndex}
              class:state-changed={getDiffClass(index) === 'state-changed'}
              on:click={() => selectHistoryEntry(index)}
            >
              <div class="history-entry-info">
                <div class="history-entry-cycle">
                  Cycle {entry.state.cycles}
                </div>
                <div class="history-entry-time">
                  {formatTimestamp(entry.timestamp)}
                </div>
              </div>
              <div
                class="history-entry-breakpoint"
                class:active={breakpoints.get(entry.state.cycles) ?? false}
                on:click|stopPropagation={() => toggleBreakpoint(entry.state.cycles)}
              ></div>
            </div>
          {/each}
        {/if}
      </div>
    </div>

    <!-- Watch Expressions -->
    <div class="watch-panel">
      <div class="watch-header">
        Watch Expressions
      </div>
      <div class="watch-list">
        {#if watchExpressions.length === 0}
          <div class="empty-message">
            No watch expressions.<br/>
            Add expressions to monitor state.
          </div>
        {:else}
          {#each watchExpressions as expr, index}
            <div class="watch-item">
              <div class="watch-expression">{expr}</div>
              <div class="watch-value">{evaluateWatch(expr)}</div>
              <button
                class="watch-remove"
                on:click={() => removeWatchExpression(index)}
              >
                ×
              </button>
            </div>
          {/each}
        {/if}
      </div>
      <div class="watch-input">
        <input
          type="text"
          bind:value={newWatch}
          placeholder="e.g., register['R0'], column[0], pc"
          on:keypress={(e) => e.key === 'Enter' && addWatchExpression()}
        />
        <button class="debug-button" on:click={addWatchExpression}>
          + Add
        </button>
      </div>
    </div>
  </div>
</div>
