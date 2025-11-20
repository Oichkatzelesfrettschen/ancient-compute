/**
 * Type definitions for emulator state synchronization
 */

/**
 * Machine state from Babbage emulator backend
 */
export interface MachineState {
  // Registers
  registers: Map<string, number>;
  programCounter: number;
  accumulator: number;
  carryFlag: boolean;
  runningFlag: boolean;
  
  // Columns (digit wheels)
  columns: ColumnState[];
  
  // Store (memory)
  store: StoreState;
  
  // Mill (arithmetic unit)
  mill: MillState;
  
  // Program barrels
  barrels: BarrelState[];
  
  // Execution state
  cycles: number;
  timestamp: number;
  phase?: string;
  timingAngle?: number;
}

export interface ColumnState {
  id: string;
  value: number;         // 0-9
  isActive: boolean;
  rotation: number;      // 0-360 degrees
}

export interface StoreState {
  positions: number[];   // Array of values stored
  capacity: number;
  activeIndices: number[];
}

export interface MillState {
  operation: 'idle' | 'add' | 'sub' | 'mul' | 'div';
  operandA: number;
  operandB: number;
  result: number;
  progress: number;      // 0-1 for animation
}

export interface BarrelState {
  id: string;
  programCounter: number;
  instructions: any[];
  isActive: boolean;
}

/**
 * WebSocket message types
 */
export type MessageType =
  | 'STATE_UPDATE'
  | 'STATE_SNAPSHOT'
  | 'ANIMATION_COMPLETE'
  | 'REQUEST_SNAPSHOT'
  | 'EXECUTE'
  | 'PAUSE'
  | 'RESUME'
  | 'STEP'
  | 'RESET'
  | 'BREAKPOINT_SET'
  | 'BREAKPOINT_REMOVE'
  | 'ERROR';

/**
 * WebSocket message payload for state updates
 */
export interface StateUpdatePayload {
  state: MachineState;
  sequenceNumber?: number;
  delta?: boolean;  // True if this is a delta update
}

/**
 * WebSocket message structure
 */
export interface WebSocketMessage {
  type: MessageType;
  payload: any;
  timestamp: number;
  sequenceNumber?: number;
}

/**
 * State diff for efficient visual updates
 */
export interface StateDiff {
  changedColumns: number[];
  changedCarryFlags: number[];
  changedStoreIndices?: number[];
  millStateChanged?: boolean;
  barrelsChanged?: number[];
  phaseChanged?: boolean;
  angleChanged?: boolean;
}

/**
 * Connection status
 */
export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

/**
 * WebSocket error
 */
export interface WebSocketError {
  code: number;
  message: string;
  timestamp: number;
}
