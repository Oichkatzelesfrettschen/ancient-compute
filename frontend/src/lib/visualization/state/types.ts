/**
 * Type Definitions: Machine State Management
 *
 * Comprehensive TypeScript interfaces for real-time state synchronization,
 * WebSocket communication, state reconciliation, and animation orchestration.
 */

/**
 * Core Machine State: Complete state of the Difference Engine at any moment
 */
export interface MachineState {
  // Execution context
  phase: MachinePhase;
  elapsedTime: number; // milliseconds since start
  isPaused: boolean;
  stepNumber: number; // Which step in the algorithm

  // Digit wheels state (248 wheels: 8x31)
  columnStates: ColumnState[];

  // Carry mechanism state
  carryLevers: CarryLeverState[];

  // Shaft rotations (8 shafts: input, add1-6, output)
  shafts: ShaftState[];

  // Mechanical phase progress (0-1, position within phase)
  phaseProgress: number;

  // Timestamp for synchronization
  timestamp: number;

  // Version for conflict detection
  stateVersion: number;
}

/**
 * Eight mechanical phases of the Difference Engine operation
 */
export type MachinePhase =
  | 'IDLE'        // 0°: No operation, waiting for input
  | 'INPUT'       // 45°: Input value being set on input shaft
  | 'ADDITION'    // 90°: Addition step (main operation)
  | 'CARRY'       // 135°: Carry propagation across columns
  | 'OUTPUT'      // 180°: Output value available
  | 'ADVANCE'     // 225°: Advancing to next step (difference calculation)
  | 'RESET'       // 270°: Resetting mechanism for next iteration
  | 'PAUSE';      // 315°: Paused state

/**
 * Single column state (one digit wheel and associated mechanisms)
 */
export interface ColumnState {
  columnIndex: number; // 0-7
  value: number; // 0-9
  previousValue: number;
  targetValue: number; // Animation target
  wheelRotation: number; // Radians (0 to 2π)
  carryActive: boolean;
  hasCarry: boolean; // Carry was generated this step
  transitionProgress: number; // 0-1 for animation
}

/**
 * Carry lever state (one per column)
 */
export interface CarryLeverState {
  columnIndex: number; // 0-7
  isEngaged: boolean; // Lever is engaged with carry wheel
  rotationAngle: number; // Degrees (0-90)
  hasCarryToPropagate: boolean;
  propagationProgress: number; // 0-1 for animation
}

/**
 * Shaft state (8 shafts in total)
 */
export interface ShaftState {
  shaftIndex: number; // 0-7
  shaftType: ShaftType;
  rotation: number; // Radians (0 to 2π)
  rotationVelocity: number; // Radians per millisecond
  isRotating: boolean;
}

/**
 * Types of shafts
 */
export type ShaftType =
  | 'INPUT'
  | 'ADDEND1'
  | 'ADDEND2'
  | 'ADDEND3'
  | 'ADDEND4'
  | 'ADDEND5'
  | 'ADDEND6'
  | 'OUTPUT';

/**
 * State Diff: Represents changes between old and new state
 */
export interface StateDiff {
  phase?: MachinePhase;
  phaseChanged: boolean;
  columnChanges: ColumnChange[];
  carryChanges: CarryChange[];
  shaftChanges: ShaftChange[];
  timestamp: number;
  stateVersionDelta: number; // How many versions changed
}

/**
 * Individual column change
 */
export interface ColumnChange {
  columnIndex: number;
  oldValue: number;
  newValue: number;
  oldRotation: number;
  newRotation: number;
  magnitude: number; // |newValue - oldValue|
}

/**
 * Individual carry change
 */
export interface CarryChange {
  columnIndex: number;
  wasEngaged: boolean;
  isEngaged: boolean;
  hadCarry: boolean;
  hasCarry: boolean;
}

/**
 * Individual shaft change
 */
export interface ShaftChange {
  shaftIndex: number;
  oldRotation: number;
  newRotation: number;
  rotationDelta: number;
}

/**
 * Interpolation path for smooth animation
 */
export interface InterpolationPath {
  startValue: number;
  endValue: number;
  startTime: number;
  duration: number; // milliseconds
  easingFunction: EasingFunction;
}

/**
 * Easing function type for animation timing
 */
export type EasingFunction =
  | 'linear'
  | 'easeInOutQuad'
  | 'easeInOutCubic'
  | 'easeInOutExpo'
  | 'easeOutBounce';

/**
 * WebSocket message types
 */
export type WebSocketMessageType =
  | 'STATE_UPDATE'         // Backend -> Frontend: Full or partial state update
  | 'STATE_SNAPSHOT'       // Frontend -> Backend: Request current snapshot
  | 'ANIMATION_COMPLETE'   // Frontend -> Backend: Animation finished, ready for next
  | 'ACKNOWLEDGE'          // Bidirectional: Message received and processed
  | 'ERROR'                // Bidirectional: Error occurred
  | 'HEARTBEAT'            // Bidirectional: Connection alive check
  | 'RECONNECT_COMPLETE';  // Frontend -> Backend: Reconnected, resync required

/**
 * WebSocket message structure
 */
export interface WebSocketMessage<T = unknown> {
  type: WebSocketMessageType;
  payload: T;
  messageId: string;
  timestamp: number;
  sequenceNumber: number; // For ordering
}

/**
 * State update payload in WebSocket message
 */
export interface StateUpdatePayload {
  fullState?: MachineState; // Full state (on reconnect or initial)
  stateDiff?: StateDiff; // Partial update
  version: number;
  clientVersion: number; // Version client had when sending
}

/**
 * Connection status
 */
export type ConnectionStatus =
  | 'DISCONNECTED'
  | 'CONNECTING'
  | 'CONNECTED'
  | 'RECONNECTING'
  | 'ERROR';

/**
 * WebSocket client configuration
 */
export interface WebSocketConfig {
  url: string;
  reconnectAttempts: number; // Default: 5
  initialBackoffMs: number; // Default: 1000ms
  maxBackoffMs: number; // Default: 16000ms
  heartbeatIntervalMs: number; // Default: 30000ms
  messageQueueSize: number; // Default: 100
}

/**
 * State reconciliation result
 */
export interface ReconciliationResult {
  resolvedState: MachineState;
  conflicts: StateConflict[];
  interpolationPaths: Map<string, InterpolationPath>;
  animationSequence: AnimationStep[];
  timeToAnimate: number; // milliseconds
}

/**
 * State conflict during reconciliation
 */
export interface StateConflict {
  property: string;
  clientValue: unknown;
  serverValue: unknown;
  resolution: 'ACCEPT_SERVER' | 'ACCEPT_CLIENT' | 'MERGE';
}

/**
 * Single animation step in sequence
 */
export interface AnimationStep {
  targetObject: string; // 'column-0', 'shaft-1', 'carry-2', etc.
  propertyName: string; // 'rotation', 'value', etc.
  startValue: number;
  endValue: number;
  startTime: number; // milliseconds from animation start
  duration: number; // milliseconds
  easing: EasingFunction;
  onComplete?: () => void;
}

/**
 * State history entry (circular buffer)
 */
export interface StateHistoryEntry {
  state: MachineState;
  timestamp: number;
  diff?: StateDiff; // Diff from previous state
}

/**
 * Performance metrics for state management
 */
export interface StateMetrics {
  averageMessageLatency: number; // ms
  messageQueueLength: number;
  lastUpdateTimestamp: number;
  stateVersionCount: number;
  reconciledConflictCount: number;
  droppedMessageCount: number;
  interpolationPathCount: number;
}

/**
 * Statistics for debugging
 */
export interface DebugStats {
  messagesReceived: number;
  messagesSent: number;
  reconnectCount: number;
  conflictCount: number;
  interpolationCount: number;
  averageStateDiffSize: number; // bytes
  averageMessageRoundtrip: number; // ms
}

/**
 * Column state snapshot for comparison
 */
export interface ColumnSnapshot {
  columnIndex: number;
  value: number;
  wheelRotation: number;
}

/**
 * Complete state snapshot
 */
export interface MachineStateSnapshot {
  phase: MachinePhase;
  columns: ColumnSnapshot[];
  timestamp: number;
  version: number;
}
