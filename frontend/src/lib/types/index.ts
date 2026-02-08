import type { AnalyticalEngineSnapshot } from './analytical_engine';
import type { MechanicalPhase } from './emulator_types'; // Assuming this might be needed later

export interface DebugState {
    cycle: number;
    phase: string | null;
    registers: Record<string, any>;
    snapshot: AnalyticalEngineSnapshot; // Changed from any to AnalyticalEngineSnapshot
    variables: Record<string, any>;
}

export interface PerformanceReport {
    total_cycles: number;
    instruction_frequency: Record<string, number>;
    hot_spots: {
        opcode: string | null;
        memory: Array<[number, number]>;
    };
    suggestions: string[];
}

export interface DebugResponse {
    triggered_breakpoints: number[] | null;
    state: DebugState;
}