<!--
  RegisterInspector.svelte
  Live display of Babbage emulator register values

  Features:
  - All register values with change highlighting
  - Binary/decimal/hex format toggle
  - Compact and detailed view modes
  - Value change animation
-->
<script lang="ts">
  import { registersStore, executionStateStore } from '$lib/visualization/state/MachineStateStore';
  import { onMount } from 'svelte';

  // Props
  export let compact = false;
  export let showBinary = false;
  export let showHex = false;

  // State
  let previousValues: Map<string, number> = new Map();
  let changedRegisters: Set<string> = new Set();
  let changeTimeout: number | null = null;

  // Reactive
  $: registers = $registersStore;
  $: pc = $executionStateStore.programCounter;
  $: accumulator = $executionStateStore.accumulator;
  $: carryFlag = $executionStateStore.carryFlag;
  $: runningFlag = $executionStateStore.runningFlag;

  // Watch for register changes
  $: {
    if (registers) {
      detectChanges(registers);
    }
  }

  /**
   * Detect changed registers and highlight
   */
  function detectChanges(currentRegisters: Map<string, number>) {
    const changed = new Set<string>();

    currentRegisters.forEach((value, name) => {
      const prevValue = previousValues.get(name);
      if (prevValue !== undefined && prevValue !== value) {
        changed.add(name);
      }
    });

    // Check special registers
    if (previousValues.get('PC') !== pc) {
      changed.add('PC');
    }
    if (previousValues.get('ACC') !== accumulator) {
      changed.add('ACC');
    }

    if (changed.size > 0) {
      changedRegisters = changed;

      // Clear highlight after 500ms
      if (changeTimeout !== null) {
        clearTimeout(changeTimeout);
      }

      changeTimeout = window.setTimeout(() => {
        changedRegisters = new Set();
        changeTimeout = null;
      }, 500);
    }

    // Update previous values
    previousValues = new Map(currentRegisters);
    previousValues.set('PC', pc);
    previousValues.set('ACC', accumulator);
  }

  /**
   * Format value based on display mode
   */
  function formatValue(value: number): string {
    if (showBinary) {
      return `0b${value.toString(2).padStart(16, '0')}`;
    } else if (showHex) {
      return `0x${value.toString(16).toUpperCase().padStart(4, '0')}`;
    } else {
      return value.toString().padStart(5, ' ');
    }
  }

  /**
   * Format flag value
   */
  function formatFlag(flag: boolean): string {
    return flag ? '1' : '0';
  }
</script>

<style>
  .register-inspector {
    background: var(--panel-bg, #1a1a1a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    font-family: 'Courier New', monospace;
    overflow: hidden;
  }

  .inspector-header {
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

  .inspector-content {
    padding: 1rem;
  }

  .register-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(150px, 1fr));
    gap: 0.75rem;
  }

  .register-grid.compact {
    grid-template-columns: repeat(auto-fill, minmax(120px, 1fr));
    gap: 0.5rem;
  }

  .register-item {
    background: var(--register-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.5rem;
    transition: all 0.3s;
  }

  .register-item.changed {
    background: var(--register-changed-bg, #1e3a1e);
    border-color: var(--register-changed-border, #4caf50);
    box-shadow: 0 0 8px rgba(76, 175, 80, 0.3);
  }

  .register-name {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
    margin-bottom: 0.3rem;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .register-value {
    font-size: 1.1rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
    font-family: 'Courier New', monospace;
  }

  .register-item.compact .register-name {
    font-size: 0.65rem;
  }

  .register-item.compact .register-value {
    font-size: 0.95rem;
  }

  .special-registers {
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border-color, #333);
  }

  .special-title {
    font-size: 0.85rem;
    color: var(--text-secondary, #999);
    margin-bottom: 0.75rem;
    font-weight: bold;
  }

  .flags-row {
    display: flex;
    gap: 1rem;
  }

  .flag-item {
    flex: 1;
    background: var(--flag-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    padding: 0.5rem;
  }

  .flag-item.active {
    background: var(--flag-active-bg, #1e3a5f);
    border-color: var(--flag-active-border, #3a7bc8);
  }

  .flag-name {
    font-size: 0.75rem;
    color: var(--text-secondary, #999);
    margin-bottom: 0.3rem;
  }

  .flag-value {
    font-size: 1.3rem;
    color: var(--text-primary, #0f0);
    font-weight: bold;
  }

  .flag-item.active .flag-value {
    color: #3a7bc8;
  }
</style>

<div class="register-inspector">
  <!-- Header -->
  <div class="inspector-header">
    <div class="header-title">📊 Registers</div>
    <div class="header-controls">
      <button
        class="toggle-button"
        class:active={showBinary}
        on:click={() => { showBinary = !showBinary; showHex = false; }}
      >
        BIN
      </button>
      <button
        class="toggle-button"
        class:active={showHex}
        on:click={() => { showHex = !showHex; showBinary = false; }}
      >
        HEX
      </button>
      <button
        class="toggle-button"
        class:active={!showBinary && !showHex}
        on:click={() => { showBinary = false; showHex = false; }}
      >
        DEC
      </button>
      <button
        class="toggle-button"
        class:active={compact}
        on:click={() => compact = !compact}
      >
        {compact ? '📋' : '📑'}
      </button>
    </div>
  </div>

  <!-- Content -->
  <div class="inspector-content">
    <!-- General Purpose Registers -->
    <div class="register-grid" class:compact>
      {#each Array.from(registers.entries()).sort((a, b) => a[0].localeCompare(b[0])) as [name, value]}
        <div
          class="register-item"
          class:compact
          class:changed={changedRegisters.has(name)}
        >
          <div class="register-name">{name}</div>
          <div class="register-value">{formatValue(value)}</div>
        </div>
      {/each}
    </div>

    <!-- Special Registers -->
    <div class="special-registers">
      <div class="special-title">Special Registers</div>
      <div class="register-grid" class:compact>
        <div
          class="register-item"
          class:compact
          class:changed={changedRegisters.has('PC')}
        >
          <div class="register-name">PC (Program Counter)</div>
          <div class="register-value">{formatValue(pc)}</div>
        </div>
        <div
          class="register-item"
          class:compact
          class:changed={changedRegisters.has('ACC')}
        >
          <div class="register-name">ACC (Accumulator)</div>
          <div class="register-value">{formatValue(accumulator)}</div>
        </div>
      </div>
    </div>

    <!-- Flags -->
    <div class="special-registers">
      <div class="special-title">Flags</div>
      <div class="flags-row">
        <div class="flag-item" class:active={carryFlag}>
          <div class="flag-name">Carry Flag</div>
          <div class="flag-value">{formatFlag(carryFlag)}</div>
        </div>
        <div class="flag-item" class:active={runningFlag}>
          <div class="flag-name">Running</div>
          <div class="flag-value">{formatFlag(runningFlag)}</div>
        </div>
      </div>
    </div>
  </div>
</div>
