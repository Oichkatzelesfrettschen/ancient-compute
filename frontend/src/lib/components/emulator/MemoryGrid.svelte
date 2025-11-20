<!--
  MemoryGrid.svelte
  Visual memory browser for Babbage emulator store

  Features:
  - Grid visualization of memory positions
  - Change highlighting with animation
  - Address search and navigation
  - Hex/decimal/binary display modes
  - Active position highlighting
-->
<script lang="ts">
  import { storeStore } from '$lib/visualization/state/MachineStateStore';
  import { onMount } from 'svelte';

  // Props
  export let columns = 16;
  export let showAddress = true;
  export let hexMode = false;

  // State
  let previousValues: number[] = [];
  let changedIndices: Set<number> = new Set();
  let selectedIndex: number | null = null;
  let searchAddress = '';
  let changeTimeout: number | null = null;

  // Reactive
  $: store = $storeStore;
  $: positions = store?.positions ?? [];
  $: activeIndices = store?.activeIndices ?? [];
  $: capacity = store?.capacity ?? 0;

  // Watch for memory changes
  $: {
    if (positions.length > 0) {
      detectChanges(positions);
    }
  }

  // Calculate rows
  $: rows = Math.ceil(capacity / columns);

  onMount(() => {
    previousValues = [...positions];
  });

  /**
   * Detect changed memory positions
   */
  function detectChanges(currentPositions: number[]) {
    const changed = new Set<number>();

    currentPositions.forEach((value, index) => {
      const prevValue = previousValues[index];
      if (prevValue !== undefined && prevValue !== value) {
        changed.add(index);
      }
    });

    if (changed.size > 0) {
      changedIndices = changed;

      // Clear highlight after 500ms
      if (changeTimeout !== null) {
        clearTimeout(changeTimeout);
      }

      changeTimeout = window.setTimeout(() => {
        changedIndices = new Set();
        changeTimeout = null;
      }, 500);
    }

    previousValues = [...currentPositions];
  }

  /**
   * Format memory value
   */
  function formatValue(value: number): string {
    if (hexMode) {
      return value.toString(16).toUpperCase().padStart(2, '0');
    } else {
      return value.toString().padStart(3, ' ');
    }
  }

  /**
   * Format address
   */
  function formatAddress(index: number): string {
    if (hexMode) {
      return `0x${index.toString(16).toUpperCase().padStart(4, '0')}`;
    } else {
      return index.toString().padStart(4, '0');
    }
  }

  /**
   * Handle cell click
   */
  function handleCellClick(index: number) {
    selectedIndex = index;
  }

  /**
   * Search and navigate to address
   */
  function navigateToAddress() {
    const addr = parseInt(searchAddress, hexMode ? 16 : 10);
    if (!isNaN(addr) && addr >= 0 && addr < capacity) {
      selectedIndex = addr;

      // Scroll to cell
      const cell = document.querySelector(`[data-index="${addr}"]`);
      if (cell) {
        cell.scrollIntoView({ behavior: 'smooth', block: 'center' });
      }
    }
  }

  /**
   * Get cell class
   */
  function getCellClass(index: number): string {
    const classes = ['memory-cell'];

    if (index === selectedIndex) {
      classes.push('selected');
    }
    if (changedIndices.has(index)) {
      classes.push('changed');
    }
    if (activeIndices.includes(index)) {
      classes.push('active');
    }

    return classes.join(' ');
  }
</script>

<style>
  .memory-grid-container {
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    font-family: 'Courier New', monospace;
    overflow: hidden;
    display: flex;
    flex-direction: column;
  }

  .grid-header {
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
    align-items: center;
  }

  .search-box {
    display: flex;
    gap: 0.3rem;
  }

  .search-input {
    width: 100px;
    padding: 0.3rem 0.5rem;
    background: var(--input-bg, #0a0a0a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    font-family: inherit;
    font-size: 0.85rem;
  }

  .search-input:focus {
    outline: none;
    border-color: var(--border-focus-color, #666);
  }

  .search-button {
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

  .search-button:hover {
    background: var(--button-hover-bg, #3a3a3a);
  }

  .toggle-button {
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

  .toggle-button.active {
    background: #4caf50;
    border-color: #4caf50;
    color: white;
  }

  .toggle-button:hover:not(.active) {
    background: var(--button-hover-bg, #3a3a3a);
  }

  .grid-content {
    flex: 1;
    overflow: auto;
    padding: 1rem;
  }

  .memory-grid {
    display: grid;
    gap: 2px;
    font-size: 0.85rem;
  }

  .grid-row {
    display: contents;
  }

  .row-header {
    background: var(--row-header-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    padding: 0.4rem 0.6rem;
    text-align: right;
    color: var(--text-secondary, #999);
    font-size: 0.75rem;
    display: flex;
    align-items: center;
    justify-content: flex-end;
  }

  .memory-cell {
    background: var(--cell-bg, #222);
    border: 1px solid var(--border-color, #333);
    padding: 0.4rem;
    text-align: center;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    transition: all 0.2s;
    position: relative;
  }

  .memory-cell:hover {
    background: var(--cell-hover-bg, #2a2a2a);
    border-color: var(--border-hover-color, #666);
  }

  .memory-cell.selected {
    background: var(--cell-selected-bg, #1e3a5f);
    border-color: var(--cell-selected-border, #3a7bc8);
    box-shadow: 0 0 8px rgba(58, 123, 200, 0.5);
    z-index: 1;
  }

  .memory-cell.changed {
    background: var(--cell-changed-bg, #3a2a1e);
    border-color: var(--cell-changed-border, #ff9800);
    animation: flash 0.5s ease-in-out;
  }

  .memory-cell.active {
    border-color: var(--cell-active-border, #4caf50);
    box-shadow: 0 0 4px rgba(76, 175, 80, 0.3);
  }

  @keyframes flash {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.6; }
  }

  .cell-info {
    margin-top: 1rem;
    padding: 1rem;
    background: var(--info-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
  }

  .info-row {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.5rem;
    font-size: 0.9rem;
  }

  .info-label {
    color: var(--text-secondary, #999);
  }

  .info-value {
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .legend {
    display: flex;
    gap: 1rem;
    margin-top: 1rem;
    padding: 0.75rem;
    background: var(--legend-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    font-size: 0.8rem;
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .legend-color {
    width: 16px;
    height: 16px;
    border: 1px solid var(--border-color, #333);
    border-radius: 3px;
  }

  .legend-color.selected {
    background: var(--cell-selected-bg, #1e3a5f);
    border-color: var(--cell-selected-border, #3a7bc8);
  }

  .legend-color.changed {
    background: var(--cell-changed-bg, #3a2a1e);
    border-color: var(--cell-changed-border, #ff9800);
  }

  .legend-color.active {
    border-color: var(--cell-active-border, #4caf50);
    border-width: 2px;
  }
</style>

<div class="memory-grid-container">
  <!-- Header -->
  <div class="grid-header">
    <div class="header-title">💾 Memory Store ({capacity} positions)</div>
    <div class="header-controls">
      <div class="search-box">
        <input
          class="search-input"
          type="text"
          bind:value={searchAddress}
          placeholder={hexMode ? "0x0000" : "0000"}
          on:keypress={(e) => e.key === 'Enter' && navigateToAddress()}
        />
        <button class="search-button" on:click={navigateToAddress}>
          🔍 Go
        </button>
      </div>
      <button
        class="toggle-button"
        class:active={hexMode}
        on:click={() => hexMode = !hexMode}
      >
        {hexMode ? 'HEX' : 'DEC'}
      </button>
      <button
        class="toggle-button"
        class:active={showAddress}
        on:click={() => showAddress = !showAddress}
      >
        {showAddress ? '📍' : '🔲'}
      </button>
    </div>
  </div>

  <!-- Content -->
  <div class="grid-content">
    <!-- Memory Grid -->
    <div
      class="memory-grid"
      style="grid-template-columns: {showAddress ? 'auto' : ''} repeat({columns}, minmax(40px, 1fr))"
    >
      {#each Array(rows) as _, rowIndex}
        <div class="grid-row">
          {#if showAddress}
            <div class="row-header">
              {formatAddress(rowIndex * columns)}
            </div>
          {/if}
          {#each Array(columns) as _, colIndex}
            {@const index = rowIndex * columns + colIndex}
            {#if index < capacity}
              <div
                class={getCellClass(index)}
                data-index={index}
                on:click={() => handleCellClick(index)}
              >
                {formatValue(positions[index] ?? 0)}
              </div>
            {:else}
              <div class="memory-cell" style="opacity: 0.2">—</div>
            {/if}
          {/each}
        </div>
      {/each}
    </div>

    <!-- Selected Cell Info -->
    {#if selectedIndex !== null && selectedIndex < capacity}
      <div class="cell-info">
        <div class="info-row">
          <span class="info-label">Address:</span>
          <span class="info-value">{formatAddress(selectedIndex)}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Value (Dec):</span>
          <span class="info-value">{positions[selectedIndex]}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Value (Hex):</span>
          <span class="info-value">0x{(positions[selectedIndex] ?? 0).toString(16).toUpperCase().padStart(2, '0')}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Value (Bin):</span>
          <span class="info-value">0b{(positions[selectedIndex] ?? 0).toString(2).padStart(8, '0')}</span>
        </div>
        <div class="info-row">
          <span class="info-label">Status:</span>
          <span class="info-value">
            {activeIndices.includes(selectedIndex) ? 'Active' : 'Inactive'}
          </span>
        </div>
      </div>
    {/if}

    <!-- Legend -->
    <div class="legend">
      <div class="legend-item">
        <div class="legend-color selected"></div>
        <span>Selected</span>
      </div>
      <div class="legend-item">
        <div class="legend-color changed"></div>
        <span>Changed</span>
      </div>
      <div class="legend-item">
        <div class="legend-color active"></div>
        <span>Active</span>
      </div>
    </div>
  </div>
</div>
