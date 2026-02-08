/**
 * Machine State Store: Svelte Writable Store
 *
 * Manages the complete state of the Difference Engine with:
 * - Reactive state updates via Svelte writable store
 * - Circular buffer history (50 snapshots)
 * - State version tracking for conflict detection
 * - Subscription system for state changes
 */

import { writable, derived, type Writable } from 'svelte/store';
import type {
  MachineState,
  MachinePhase,
  StateDiff,
  ColumnState,
  CarryLeverState,
  ShaftState,
  ShaftType,
  StateMetrics,
  StateHistoryEntry,
  DebugStats
} from './types';

/**
 * Create initial machine state
 */
function createInitialState(): MachineState {
  return {
    phase: 'IDLE',
    elapsedTime: 0,
    isPaused: false,
    stepNumber: 0,
    columnStates: Array.from({ length: 8 }, (_, i) => ({
      columnIndex: i,
      value: 0,
      previousValue: 0,
      targetValue: 0,
      wheelRotation: 0,
      carryActive: false,
      hasCarry: false,
      transitionProgress: 0
    })),
    carryLevers: Array.from({ length: 8 }, (_, i) => ({
      columnIndex: i,
      isEngaged: false,
      rotationAngle: 0,
      hasCarryToPropagate: false,
      propagationProgress: 0
    })),
    shafts: Array.from({ length: 8 }, (_, i) => {
      const shaftTypes: ShaftType[] = [
        'INPUT',
        'ADDEND1',
        'ADDEND2',
        'ADDEND3',
        'ADDEND4',
        'ADDEND5',
        'ADDEND6',
        'OUTPUT'
      ];
      return {
        shaftIndex: i,
        shaftType: shaftTypes[i],
        rotation: 0,
        rotationVelocity: 0,
        isRotating: false
      };
    }),
    phaseProgress: 0,
    timestamp: Date.now(),
    stateVersion: 0
  };
}

/**
 * Circular buffer for state history
 */
class CircularStateBuffer {
  private buffer: StateHistoryEntry[] = [];
  private readonly maxSize: number = 50;
  private writeIndex: number = 0;
  private isFull: boolean = false;

  /**
   * Add state to buffer
   */
  add(state: MachineState, diff?: StateDiff): void {
    const entry: StateHistoryEntry = {
      state: JSON.parse(JSON.stringify(state)), // Deep copy
      timestamp: state.timestamp,
      diff
    };

    if (this.buffer.length < this.maxSize) {
      this.buffer.push(entry);
    } else {
      this.buffer[this.writeIndex] = entry;
      this.isFull = true;
    }

    this.writeIndex = (this.writeIndex + 1) % this.maxSize;
  }

  /**
   * Get all entries in chronological order
   */
  getAll(): StateHistoryEntry[] {
    if (!this.isFull) {
      return this.buffer;
    }
    return [...this.buffer.slice(this.writeIndex), ...this.buffer.slice(0, this.writeIndex)];
  }

  /**
   * Get entry at offset from head (0 = most recent)
   */
  getAt(offset: number): StateHistoryEntry | null {
    if (offset >= this.buffer.length) return null;
    const actualIndex = (this.writeIndex - 1 - offset + this.buffer.length) % this.buffer.length;
    return this.buffer[actualIndex];
  }

  /**
   * Get last N entries
   */
  getLast(n: number): StateHistoryEntry[] {
    const all = this.getAll();
    return all.slice(Math.max(0, all.length - n));
  }

  /**
   * Clear history
   */
  clear(): void {
    this.buffer = [];
    this.writeIndex = 0;
    this.isFull = false;
  }

  /**
   * Get size
   */
  size(): number {
    return this.buffer.length;
  }
}

/**
 * Create MachineStateStore
 */
export function createMachineStateStore() {
  const initialState = createInitialState();
  const stateStore = writable<MachineState>(initialState);
  const historyBuffer = new CircularStateBuffer();
  const diffHistory: StateDiff[] = [];
  let metricsData: StateMetrics = {
    averageMessageLatency: 0,
    messageQueueLength: 0,
    lastUpdateTimestamp: Date.now(),
    stateVersionCount: 0,
    reconciledConflictCount: 0,
    droppedMessageCount: 0,
    interpolationPathCount: 0
  };

  let debugStats: DebugStats = {
    messagesReceived: 0,
    messagesSent: 0,
    reconnectCount: 0,
    conflictCount: 0,
    interpolationCount: 0,
    averageStateDiffSize: 0,
    averageMessageRoundtrip: 0
  };

  // Add initial state to history
  historyBuffer.add(initialState);

  return {
    // Store subscriptions
    subscribe: stateStore.subscribe,

    /**
     * Update complete state
     */
    setState(newState: MachineState): void {
      stateStore.set(newState);
      historyBuffer.add(newState);
      metricsData.lastUpdateTimestamp = Date.now();
      metricsData.stateVersionCount = newState.stateVersion;
    },

    /**
     * Update state with diff
     */
    updateState(diff: Partial<MachineState> & { stateDiff?: StateDiff }): void {
      stateStore.update((current) => {
        const updated = { ...current, ...diff };
        updated.stateVersion += 1;
        updated.timestamp = Date.now();
        historyBuffer.add(updated, diff.stateDiff);
        if (diff.stateDiff) {
          diffHistory.push(diff.stateDiff);
        }
        metricsData.lastUpdateTimestamp = Date.now();
        return updated;
      });
    },

    /**
     * Update specific column
     */
    updateColumn(
      columnIndex: number,
      update: Partial<ColumnState>
    ): void {
      stateStore.update((current) => {
        const columns = [...current.columnStates];
        columns[columnIndex] = { ...columns[columnIndex], ...update };
        return {
          ...current,
          columnStates: columns,
          stateVersion: current.stateVersion + 1,
          timestamp: Date.now()
        };
      });
    },

    /**
     * Update multiple columns
     */
    updateColumns(updates: Map<number, Partial<ColumnState>>): void {
      stateStore.update((current) => {
        const columns = [...current.columnStates];
        updates.forEach((update, index) => {
          columns[index] = { ...columns[index], ...update };
        });
        return {
          ...current,
          columnStates: columns,
          stateVersion: current.stateVersion + 1,
          timestamp: Date.now()
        };
      });
    },

    /**
     * Update carry lever
     */
    updateCarryLever(
      columnIndex: number,
      update: Partial<CarryLeverState>
    ): void {
      stateStore.update((current) => {
        const levers = [...current.carryLevers];
        levers[columnIndex] = { ...levers[columnIndex], ...update };
        return {
          ...current,
          carryLevers: levers,
          stateVersion: current.stateVersion + 1,
          timestamp: Date.now()
        };
      });
    },

    /**
     * Update shaft
     */
    updateShaft(
      shaftIndex: number,
      update: Partial<ShaftState>
    ): void {
      stateStore.update((current) => {
        const shafts = [...current.shafts];
        shafts[shaftIndex] = { ...shafts[shaftIndex], ...update };
        return {
          ...current,
          shafts: shafts,
          stateVersion: current.stateVersion + 1,
          timestamp: Date.now()
        };
      });
    },

    /**
     * Update machine phase
     */
    setPhase(phase: MachinePhase, progress: number = 0): void {
      stateStore.update((current) => ({
        ...current,
        phase,
        phaseProgress: progress,
        stateVersion: current.stateVersion + 1,
        timestamp: Date.now()
      }));
    },

    /**
     * Set pause state
     */
    setPaused(isPaused: boolean): void {
      stateStore.update((current) => ({
        ...current,
        isPaused,
        stateVersion: current.stateVersion + 1,
        timestamp: Date.now()
      }));
    },

    /**
     * Increment step number
     */
    nextStep(): void {
      stateStore.update((current) => ({
        ...current,
        stepNumber: current.stepNumber + 1,
        stateVersion: current.stateVersion + 1,
        timestamp: Date.now()
      }));
    },

    /**
     * Reset to initial state
     */
    reset(): void {
      const initial = createInitialState();
      stateStore.set(initial);
      historyBuffer.clear();
      historyBuffer.add(initial);
      diffHistory.length = 0;
    },

    /**
     * Get current state (synchronous)
     */
    getState(): MachineState {
      let state: MachineState = initialState;
      const unsubscribe = stateStore.subscribe((s) => {
        state = s;
      });
      unsubscribe();
      return state;
    },

    /**
     * Revert to previous state (offset from head)
     */
    revertToPrevious(offset: number = 1): void {
      const entry = historyBuffer.getAt(offset);
      if (entry) {
        stateStore.set(entry.state);
        metricsData.lastUpdateTimestamp = Date.now();
      }
    },

    /**
     * Get state at specific version
     */
    getStateAtVersion(version: number): MachineState | null {
      const current = this.getState();
      if (version === current.stateVersion) {
        return current;
      }
      // Linear search through history (could be optimized with indexing)
      const all = historyBuffer.getAll();
      for (const entry of all) {
        if (entry.state.stateVersion === version) {
          return entry.state;
        }
      }
      return null;
    },

    /**
     * Get history entries
     */
    getHistory(limit: number = 10): StateHistoryEntry[] {
      return historyBuffer.getLast(limit);
    },

    /**
     * Get full history
     */
    getAllHistory(): StateHistoryEntry[] {
      return historyBuffer.getAll();
    },

    /**
     * Get diff history
     */
    getDiffHistory(): StateDiff[] {
      return [...diffHistory];
    },

    /**
     * Get metrics
     */
    getMetrics(): StateMetrics {
      return { ...metricsData };
    },

    /**
     * Update metrics
     */
    updateMetrics(update: Partial<StateMetrics>): void {
      metricsData = { ...metricsData, ...update };
    },

    /**
     * Get debug stats
     */
    getDebugStats(): DebugStats {
      return { ...debugStats };
    },

    /**
     * Increment debug counters
     */
    recordMessageReceived(latency: number, diffSize: number = 0): void {
      debugStats.messagesReceived += 1;
      const avgRoundtrip =
        (debugStats.averageMessageRoundtrip * (debugStats.messagesReceived - 1) + latency) /
        debugStats.messagesReceived;
      debugStats.averageMessageRoundtrip = avgRoundtrip;
      metricsData.averageMessageLatency = avgRoundtrip;
    },

    recordMessageSent(): void {
      debugStats.messagesSent += 1;
    },

    recordReconnect(): void {
      debugStats.reconnectCount += 1;
      metricsData.averageMessageLatency = 0;
    },

    recordConflict(): void {
      debugStats.conflictCount += 1;
      metricsData.reconciledConflictCount += 1;
    },

    recordInterpolation(): void {
      debugStats.interpolationCount += 1;
    },

    recordDroppedMessage(): void {
      debugStats.messagesSent += 1;
      metricsData.droppedMessageCount += 1;
    },

    /**
     * Clear debug stats
     */
    clearDebugStats(): void {
      debugStats = {
        messagesReceived: 0,
        messagesSent: 0,
        reconnectCount: 0,
        conflictCount: 0,
        interpolationCount: 0,
        averageStateDiffSize: 0,
        averageMessageRoundtrip: 0
      };
    }
  };
}

/**
 * Global store instance
 */
export const machineStateStore = createMachineStateStore();

/**
 * Derived store: Current phase
 */
export const currentPhase = derived(
  machineStateStore as any,
  ($state) => $state.phase
);

/**
 * Derived store: Current step
 */
export const currentStep = derived(
  machineStateStore as any,
  ($state) => $state.stepNumber
);

/**
 * Derived store: Is paused
 */
export const isPausedStore = derived(
  machineStateStore as any,
  ($state) => $state.isPaused
);

/**
 * Derived store: Column values (for easy access)
 */
export const columnValues = derived(
  machineStateStore as any,
  ($state) => $state.columnStates.map((col) => col.value)
);
