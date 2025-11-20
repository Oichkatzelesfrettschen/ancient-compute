/**
 * Svelte Store for Machine State Management
 * 
 * Provides reactive store for Babbage emulator state with:
 * - State snapshots and updates
 * - History tracking for debugging
 * - State diff calculation
 * - Subscription management
 */

import { writable, derived, get } from 'svelte/store';
import type { MachineState, StateDiff } from './types';

/**
 * State history entry
 */
export interface StateHistoryEntry {
  state: MachineState;
  timestamp: number;
  sequenceNumber?: number;
}

/**
 * Machine state store with history
 */
class MachineStateStoreClass {
  private state = writable<MachineState | null>(null);
  private history: StateHistoryEntry[] = [];
  private maxHistorySize = 100;
  private previousState: MachineState | null = null;

  /**
   * Set complete machine state
   */
  setState(newState: MachineState): void {
    const timestamp = Date.now();

    // Store previous state
    this.previousState = get(this.state);

    // Update current state
    this.state.set(newState);

    // Add to history
    this.addToHistory({
      state: newState,
      timestamp,
      sequenceNumber: newState.timestamp
    });
  }

  /**
   * Update partial state (merge with current)
   */
  updateState(partialState: Partial<MachineState>): void {
    const current = get(this.state);
    if (!current) {
      console.warn('[MachineStateStore] Cannot update: no current state');
      return;
    }

    const newState: MachineState = {
      ...current,
      ...partialState,
      timestamp: Date.now()
    };

    this.setState(newState);
  }

  /**
   * Get current state
   */
  getState(): MachineState | null {
    return get(this.state);
  }

  /**
   * Calculate diff between previous and current state
   */
  getStateDiff(): StateDiff | null {
    const current = get(this.state);
    if (!this.previousState || !current) {
      return null;
    }

    return this.calculateDiff(this.previousState, current);
  }

  /**
   * Subscribe to state changes
   */
  subscribe(handler: (state: MachineState | null) => void): () => void {
    return this.state.subscribe(handler);
  }

  /**
   * Reset state
   */
  reset(): void {
    this.state.set(null);
    this.history = [];
    this.previousState = null;
  }

  /**
   * Get state history
   */
  getHistory(): StateHistoryEntry[] {
    return [...this.history];
  }

  /**
   * Get history entry at index
   */
  getHistoryEntry(index: number): StateHistoryEntry | null {
    return this.history[index] ?? null;
  }

  /**
   * Rewind to previous state in history
   */
  rewindToHistoryEntry(index: number): void {
    const entry = this.history[index];
    if (!entry) {
      console.warn('[MachineStateStore] Invalid history index:', index);
      return;
    }

    this.state.set(entry.state);
  }

  /**
   * Clear history but keep current state
   */
  clearHistory(): void {
    this.history = [];
  }

  /**
   * Add state to history
   */
  private addToHistory(entry: StateHistoryEntry): void {
    this.history.push(entry);

    // Trim history if too large
    if (this.history.length > this.maxHistorySize) {
      this.history.shift();
    }
  }

  /**
   * Calculate difference between two states
   */
  private calculateDiff(oldState: MachineState, newState: MachineState): StateDiff {
    const diff: StateDiff = {
      changedColumns: [],
      changedCarryFlags: [],
      changedStoreIndices: [],
      barrelsChanged: []
    };

    // Check column changes
    newState.columns.forEach((column, index) => {
      const oldColumn = oldState.columns[index];
      if (!oldColumn || column.value !== oldColumn.value || column.isActive !== oldColumn.isActive) {
        diff.changedColumns.push(index);
      }
    });

    // Check carry flag (simplified - checking accumulator carryFlag)
    if (newState.carryFlag !== oldState.carryFlag) {
      diff.changedCarryFlags.push(0);
    }

    // Check store changes
    newState.store.positions.forEach((value, index) => {
      if (value !== oldState.store.positions[index]) {
        diff.changedStoreIndices!.push(index);
      }
    });

    // Check mill state
    diff.millStateChanged =
      newState.mill.operation !== oldState.mill.operation ||
      newState.mill.operandA !== oldState.mill.operandA ||
      newState.mill.operandB !== oldState.mill.operandB ||
      newState.mill.result !== oldState.mill.result;

    // Check barrel changes
    newState.barrels.forEach((barrel, index) => {
      const oldBarrel = oldState.barrels[index];
      if (!oldBarrel || barrel.isActive !== oldBarrel.isActive || barrel.programCounter !== oldBarrel.programCounter) {
        diff.barrelsChanged!.push(index);
      }
    });

    // Check phase/angle changes
    diff.phaseChanged = newState.phase !== oldState.phase;
    diff.angleChanged = newState.timingAngle !== oldState.timingAngle;

    return diff;
  }
}

/**
 * Singleton machine state store
 */
export const machineStateStore = new MachineStateStoreClass();

/**
 * Derived stores for specific state aspects
 */
export const columnsStore = derived(
  machineStateStore,
  ($state) => $state?.columns ?? []
);

export const millStore = derived(
  machineStateStore,
  ($state) => $state?.mill ?? null
);

export const storeStore = derived(
  machineStateStore,
  ($state) => $state?.store ?? null
);

export const barrelsStore = derived(
  machineStateStore,
  ($state) => $state?.barrels ?? []
);

export const registersStore = derived(
  machineStateStore,
  ($state) => $state?.registers ?? new Map()
);

export const executionStateStore = derived(
  machineStateStore,
  ($state) => ({
    programCounter: $state?.programCounter ?? 0,
    accumulator: $state?.accumulator ?? 0,
    carryFlag: $state?.carryFlag ?? false,
    runningFlag: $state?.runningFlag ?? false,
    cycles: $state?.cycles ?? 0
  })
);

// Re-export types
export type { MachineState, StateDiff };
