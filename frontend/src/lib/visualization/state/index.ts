/**
 * State Management Module Exports
 *
 * Re-exports all state management components:
 * - Type definitions
 * - MachineStateStore (Svelte writable store)
 * - WebSocketClient (network communication)
 * - StateReconciler (diffing and conflict resolution)
 * - StateHistory (tracking and replay)
 */

// Type exports
export type {
  MachineState,
  MachinePhase,
  ColumnState,
  CarryLeverState,
  ShaftState,
  ShaftType,
  StateDiff,
  ColumnChange,
  CarryChange,
  ShaftChange,
  InterpolationPath,
  EasingFunction,
  WebSocketMessageType,
  WebSocketMessage,
  StateUpdatePayload,
  ConnectionStatus,
  WebSocketConfig,
  ReconciliationResult,
  StateConflict,
  AnimationStep,
  StateHistoryEntry,
  StateMetrics,
  DebugStats,
  ColumnSnapshot,
  MachineStateSnapshot
} from './types';

// MachineStateStore exports
export {
  createMachineStateStore,
  machineStateStore,
  currentPhase,
  currentStep,
  isPausedStore,
  columnValues
} from './MachineStateStore';

// WebSocketClient exports
export {
  WebSocketClient,
  getWebSocketClient,
  createWebSocketClient
} from './WebSocketClient';

// StateReconciler exports
export {
  StateReconciler,
  globalReconciler
} from './StateReconciler';

// StateHistory exports
export {
  StateHistory,
  globalStateHistory
} from './StateHistory';
