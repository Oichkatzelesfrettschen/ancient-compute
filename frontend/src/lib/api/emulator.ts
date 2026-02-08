/**
 * Emulator API Client
 *
 * Provides frontend interface to backend emulator endpoints
 */

import type {
	ExecuteResponse,
	ResetResponse,
	StepResponse,
	BreakpointResponse,
	DebugStepResponse,
	DebugContinueResponse,
	StateResponse,
	ResultsResponse,
	Breakpoint
} from './types';

const API_BASE = '/api';

/**
 * Execute polynomial evaluation
 *
 * @param coefficients Polynomial coefficients [a0, a1, a2, ...]
 * @param xRange [start, end] values for x
 * @param speed Execution speed multiplier (default 1.0)
 * @returns Execution results
 */
export async function executePolynomial(
	coefficients: number[],
	xRange: [number, number],
	speed: number = 1.0
): Promise<ExecuteResponse> {
	try {
		const response = await fetch(`${API_BASE}/execute`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				coefficients,
				x_range: xRange,
				execution_speed: speed
			})
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Reset the emulator to initial state
 *
 * @returns Reset response
 */
export async function resetEmulator(): Promise<ResetResponse> {
	try {
		const response = await fetch(`${API_BASE}/reset`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Execute a single mechanical cycle
 *
 * @returns Step execution result
 */
export async function stepCycle(): Promise<StepResponse> {
	try {
		const response = await fetch(`${API_BASE}/step`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Set a breakpoint for debugging
 *
 * @param type Breakpoint type (CYCLE, PHASE, VALUE_CHANGE, CONDITION)
 * @param options Type-specific options
 * @returns Breakpoint creation response
 */
export async function setBreakpoint(
	type: string,
	options: Record<string, unknown>
): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/breakpoint`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				type,
				...options
			})
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Enable a breakpoint
 *
 * @param breakpointId ID of breakpoint to enable
 * @returns Response
 */
export async function enableBreakpoint(breakpointId: number): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/breakpoint/${breakpointId}/enable`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Disable a breakpoint
 *
 * @param breakpointId ID of breakpoint to disable
 * @returns Response
 */
export async function disableBreakpoint(breakpointId: number): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/breakpoint/${breakpointId}/disable`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Remove a breakpoint
 *
 * @param breakpointId ID of breakpoint to remove
 * @returns Response
 */
export async function removeBreakpoint(breakpointId: number): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/breakpoint/${breakpointId}`, {
			method: 'DELETE',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Step through execution with debugger
 *
 * @returns Debug step response
 */
export async function debugStep(): Promise<DebugStepResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/step`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Continue execution until breakpoint or limit
 *
 * @param maxCycles Maximum cycles to run (optional)
 * @returns Debug continue response
 */
export async function debugContinue(maxCycles?: number): Promise<DebugContinueResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/continue`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				max_cycles: maxCycles
			})
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Get current emulator state
 *
 * @returns Current state response
 */
export async function getState(): Promise<StateResponse> {
	try {
		const response = await fetch(`${API_BASE}/state`, {
			method: 'GET',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Get evaluation results
 *
 * @returns Results response
 */
export async function getResults(): Promise<ResultsResponse> {
	try {
		const response = await fetch(`${API_BASE}/results`, {
			method: 'GET',
			headers: {
				'Content-Type': 'application/json'
			}
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Define a debugger variable
 *
 * @param name Variable name
 * @param value Initial value
 * @returns Response
 */
export async function defineVariable(name: string, value: number): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/variable`, {
			method: 'POST',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				name,
				value
			})
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}

/**
 * Set a debugger variable value
 *
 * @param name Variable name
 * @param value New value
 * @returns Response
 */
export async function setVariable(name: string, value: number): Promise<BreakpointResponse> {
	try {
		const response = await fetch(`${API_BASE}/debug/variable/${name}`, {
			method: 'PUT',
			headers: {
				'Content-Type': 'application/json'
			},
			body: JSON.stringify({
				value
			})
		});

		if (!response.ok) {
			const error = await response.json();
			return {
				success: false,
				error: error.detail || `HTTP ${response.status}`
			};
		}

		return await response.json();
	} catch (error) {
		return {
			success: false,
			error: error instanceof Error ? error.message : 'Network request failed'
		};
	}
}
