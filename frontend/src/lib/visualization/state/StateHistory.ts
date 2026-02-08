/**
 * State History: State Tracking, Replay, and Debugging
 *
 * Provides:
 * - Circular buffer storage of state snapshots (50 max)
 * - Revert to previous states
 * - Replay animations from history
 * - State comparison and statistics
 * - Debugging utilities
 */

import type {
  MachineState,
  StateHistoryEntry,
  StateDiff,
  ColumnSnapshot,
  MachineStateSnapshot
} from './types';

/**
 * State history tracker
 */
export class StateHistory {
  private entries: StateHistoryEntry[] = [];
  private readonly maxSize: number = 50;
  private currentIndex: number = -1;
  private isReplaying: boolean = false;

  /**
   * Add state to history
   */
  addState(state: MachineState, diff?: StateDiff): void {
    // If we're not at the end, truncate history beyond current index
    if (this.currentIndex < this.entries.length - 1) {
      this.entries = this.entries.slice(0, this.currentIndex + 1);
    }

    const entry: StateHistoryEntry = {
      state: this.deepCloneState(state),
      timestamp: state.timestamp,
      diff
    };

    if (this.entries.length < this.maxSize) {
      this.entries.push(entry);
      this.currentIndex = this.entries.length - 1;
    } else {
      // Circular buffer: remove oldest and add newest
      this.entries.shift();
      this.entries.push(entry);
      this.currentIndex = this.maxSize - 1;
    }
  }

  /**
   * Get current state in history
   */
  getCurrentState(): MachineState | null {
    if (this.currentIndex >= 0 && this.currentIndex < this.entries.length) {
      return this.entries[this.currentIndex].state;
    }
    return null;
  }

  /**
   * Move to previous state
   */
  movePrevious(): MachineState | null {
    if (this.currentIndex > 0) {
      this.currentIndex -= 1;
      return this.entries[this.currentIndex].state;
    }
    return null;
  }

  /**
   * Move to next state
   */
  moveNext(): MachineState | null {
    if (this.currentIndex < this.entries.length - 1) {
      this.currentIndex += 1;
      return this.entries[this.currentIndex].state;
    }
    return null;
  }

  /**
   * Move to specific index
   */
  moveToIndex(index: number): MachineState | null {
    if (index >= 0 && index < this.entries.length) {
      this.currentIndex = index;
      return this.entries[index].state;
    }
    return null;
  }

  /**
   * Jump to most recent state
   */
  moveToLatest(): MachineState | null {
    if (this.entries.length > 0) {
      this.currentIndex = this.entries.length - 1;
      return this.entries[this.currentIndex].state;
    }
    return null;
  }

  /**
   * Get state at version
   */
  getStateAtVersion(version: number): MachineState | null {
    for (const entry of this.entries) {
      if (entry.state.stateVersion === version) {
        return entry.state;
      }
    }
    return null;
  }

  /**
   * Get state at timestamp
   */
  getStateAtTimestamp(timestamp: number): MachineState | null {
    // Find closest state to timestamp
    let closest: MachineState | null = null;
    let minDelta = Infinity;

    for (const entry of this.entries) {
      const delta = Math.abs(entry.timestamp - timestamp);
      if (delta < minDelta) {
        minDelta = delta;
        closest = entry.state;
      }
    }

    return closest;
  }

  /**
   * Get state at offset from current
   */
  getAtOffset(offset: number): MachineState | null {
    const index = this.currentIndex + offset;
    return this.moveToIndex(index);
  }

  /**
   * Get all states
   */
  getAllStates(): MachineState[] {
    return this.entries.map((entry) => entry.state);
  }

  /**
   * Get all entries with diffs
   */
  getAllEntries(): StateHistoryEntry[] {
    return [...this.entries];
  }

  /**
   * Get last N states
   */
  getLastStates(n: number): MachineState[] {
    const start = Math.max(0, this.entries.length - n);
    return this.entries.slice(start).map((entry) => entry.state);
  }

  /**
   * Get history length
   */
  length(): number {
    return this.entries.length;
  }

  /**
   * Get current index
   */
  getCurrentIndex(): number {
    return this.currentIndex;
  }

  /**
   * Check if at latest
   */
  isAtLatest(): boolean {
    return this.currentIndex === this.entries.length - 1;
  }

  /**
   * Check if at oldest
   */
  isAtOldest(): boolean {
    return this.currentIndex === 0;
  }

  /**
   * Clear history
   */
  clear(): void {
    this.entries = [];
    this.currentIndex = -1;
  }

  /**
   * Get statistics
   */
  getStatistics(): {
    totalStates: number;
    totalTime: number; // milliseconds
    averageTimeBetweenUpdates: number;
    totalColumnChanges: number;
    totalCarryChanges: number;
    totalShaftChanges: number;
  } {
    if (this.entries.length === 0) {
      return {
        totalStates: 0,
        totalTime: 0,
        averageTimeBetweenUpdates: 0,
        totalColumnChanges: 0,
        totalCarryChanges: 0,
        totalShaftChanges: 0
      };
    }

    const firstTimestamp = this.entries[0].timestamp;
    const lastTimestamp = this.entries[this.entries.length - 1].timestamp;
    const totalTime = lastTimestamp - firstTimestamp;

    let columnChanges = 0;
    let carryChanges = 0;
    let shaftChanges = 0;

    for (const entry of this.entries) {
      if (entry.diff) {
        columnChanges += entry.diff.columnChanges.length;
        carryChanges += entry.diff.carryChanges.length;
        shaftChanges += entry.diff.shaftChanges.length;
      }
    }

    return {
      totalStates: this.entries.length,
      totalTime,
      averageTimeBetweenUpdates:
        this.entries.length > 1 ? totalTime / (this.entries.length - 1) : 0,
      totalColumnChanges: columnChanges,
      totalCarryChanges: carryChanges,
      totalShaftChanges: shaftChanges
    };
  }

  /**
   * Compare two states
   */
  compareStates(state1: MachineState, state2: MachineState): {
    phaseChanged: boolean;
    columnChanges: number;
    carryChanges: number;
    shaftChanges: number;
  } {
    let columnChanges = 0;
    let carryChanges = 0;
    let shaftChanges = 0;

    // Count column differences
    for (let i = 0; i < 8; i++) {
      if (state1.columnStates[i].value !== state2.columnStates[i].value) {
        columnChanges += 1;
      }
    }

    // Count carry differences
    for (let i = 0; i < 8; i++) {
      if (
        state1.carryLevers[i].isEngaged !==
        state2.carryLevers[i].isEngaged
      ) {
        carryChanges += 1;
      }
    }

    // Count shaft differences
    for (let i = 0; i < 8; i++) {
      if (state1.shafts[i].rotation !== state2.shafts[i].rotation) {
        shaftChanges += 1;
      }
    }

    return {
      phaseChanged: state1.phase !== state2.phase,
      columnChanges,
      carryChanges,
      shaftChanges
    };
  }

  /**
   * Find state transition by condition
   */
  findTransition(
    predicate: (
      previous: MachineState,
      current: MachineState
    ) => boolean
  ): { index: number; fromState: MachineState; toState: MachineState } | null {
    for (let i = 1; i < this.entries.length; i++) {
      const prev = this.entries[i - 1].state;
      const curr = this.entries[i].state;
      if (predicate(prev, curr)) {
        return {
          index: i,
          fromState: prev,
          toState: curr
        };
      }
    }
    return null;
  }

  /**
   * Find all state transitions by condition
   */
  findAllTransitions(
    predicate: (
      previous: MachineState,
      current: MachineState
    ) => boolean
  ): Array<{ index: number; fromState: MachineState; toState: MachineState }> {
    const transitions = [];
    for (let i = 1; i < this.entries.length; i++) {
      const prev = this.entries[i - 1].state;
      const curr = this.entries[i].state;
      if (predicate(prev, curr)) {
        transitions.push({
          index: i,
          fromState: prev,
          toState: curr
        });
      }
    }
    return transitions;
  }

  /**
   * Get snapshots for debugging
   */
  getSnapshots(): MachineStateSnapshot[] {
    return this.entries.map((entry, index) => ({
      phase: entry.state.phase,
      columns: entry.state.columnStates.map((col) => ({
        columnIndex: col.columnIndex,
        value: col.value,
        wheelRotation: col.wheelRotation
      })),
      timestamp: entry.timestamp,
      version: entry.state.stateVersion
    }));
  }

  /**
   * Export history as JSON for debugging
   */
  exportAsJSON(): string {
    return JSON.stringify(
      {
        metadata: {
          totalStates: this.entries.length,
          maxSize: this.maxSize,
          currentIndex: this.currentIndex,
          isReplaying: this.isReplaying
        },
        entries: this.entries.map((entry) => ({
          state: entry.state,
          timestamp: entry.timestamp,
          diff: entry.diff
        }))
      },
      null,
      2
    );
  }

  /**
   * Get console-friendly summary
   */
  getSummary(): string {
    const stats = this.getStatistics();
    return `
State History Summary:
- Total States: ${stats.totalStates}
- Time Span: ${(stats.totalTime / 1000).toFixed(2)}s
- Avg Update Rate: ${stats.averageTimeBetweenUpdates.toFixed(1)}ms
- Column Changes: ${stats.totalColumnChanges}
- Carry Changes: ${stats.totalCarryChanges}
- Shaft Changes: ${stats.totalShaftChanges}
- Current Position: ${this.currentIndex + 1} / ${this.entries.length}
    `.trim();
  }

  /**
   * Deep clone state
   */
  private deepCloneState(state: MachineState): MachineState {
    return JSON.parse(JSON.stringify(state));
  }
}

/**
 * Global state history instance
 */
export const globalStateHistory = new StateHistory();
