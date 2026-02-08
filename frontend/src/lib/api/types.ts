/**
 * API Type Definitions for Ancient Compute Emulator
 *
 * Defines TypeScript interfaces for communication between frontend and backend.
 */

/**
 * Result of a polynomial evaluation for a single x value
 */
export interface ExecutionResult {
	x: number;
	result: number;
	cycle: number;
	phase: string;
	state?: MachineState;
}

/**
 * Current state of the Difference Engine
 */
export interface MachineState {
	cycle: number;
	phase: MechanicalPhase;
	angle: number;
	columns: number[];
	carrySignals: boolean[];
	accumulator: number;
	totalOperations: number;
}

/**
 * Mechanical phases of the Difference Engine
 */
export enum MechanicalPhase {
	IDLE = 'IDLE',
	ADDITION = 'ADDITION',
	CARRY = 'CARRY',
	TABLE = 'TABLE',
	OUTPUT = 'OUTPUT'
}

/**
 * Response format for execute endpoint
 */
export interface ExecuteResponse {
	success: boolean;
	results?: ExecutionResult[];
	error?: string;
	totalCycles?: number;
	executionTime?: number;
}

/**
 * Response format for reset endpoint
 */
export interface ResetResponse {
	success: boolean;
	error?: string;
}

/**
 * Response format for step endpoint
 */
export interface StepResponse {
	success: boolean;
	results?: ExecutionResult[];
	error?: string;
	state?: MachineState;
}

/**
 * Debugger breakpoint configuration
 */
export interface Breakpoint {
	id: number;
	type: 'CYCLE' | 'PHASE' | 'VALUE_CHANGE' | 'CONDITION';
	enabled: boolean;
	hitCount: number;
	cycleTarget?: number;
	phaseTarget?: MechanicalPhase;
	variableName?: string;
}

/**
 * Debugger variable entry
 */
export interface DebuggerVariable {
	name: string;
	initialValue: number;
	currentValue: number;
	readCount: number;
	writeCount: number;
	firstAccessCycle?: number;
	lastAccessCycle?: number;
	accessHistory: AccessHistoryEntry[];
}

/**
 * Access history entry for a variable
 */
export interface AccessHistoryEntry {
	cycle: number;
	operation: 'READ' | 'WRITE';
	value: number;
}

/**
 * Response format for debug/breakpoint endpoint
 */
export interface BreakpointResponse {
	success: boolean;
	breakpointId?: number;
	error?: string;
}

/**
 * Response format for debug/step endpoint
 */
export interface DebugStepResponse {
	success: boolean;
	state?: MachineState;
	breakpointsHit?: number[];
	error?: string;
}

/**
 * Response format for debug/continue endpoint
 */
export interface DebugContinueResponse {
	success: boolean;
	cyclesRun?: number;
	breakpointHit?: number[];
	state?: MachineState;
	error?: string;
}

/**
 * Response format for state endpoint
 */
export interface StateResponse {
	success: boolean;
	state?: MachineState;
	variables?: Record<string, DebuggerVariable>;
	error?: string;
}

/**
 * Response format for results endpoint
 */
export interface ResultsResponse {
	success: boolean;
	results?: ExecutionResult[];
	error?: string;
}
