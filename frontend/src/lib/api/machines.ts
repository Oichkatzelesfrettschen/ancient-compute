/**
 * Machines API client -- historical emulator gallery endpoints.
 */

const API_BASE = import.meta.env.VITE_API_URL || '/api/v1';

export interface MachineListItem {
	id: string;
	name: string;
	year: number;
	country: string;
	inventor: string;
	category: string;
	brief: string;
	program_input_type: string;
	tags: string[];
}

export interface MachineDetail extends MachineListItem {
	manual: string;
	example_payload: Record<string, unknown>;
}

export interface MachineState {
	cycle_count: number;
	registers: Record<string, unknown>;
	columns: number[];
	snapshot: unknown;
	halted: boolean;
}

export interface RunResult {
	steps_taken: number;
	registers: Record<string, unknown>;
	snapshot: unknown;
	halted: boolean;
}

async function apiFetch<T>(path: string, init?: RequestInit): Promise<T> {
	const res = await fetch(`${API_BASE}${path}`, {
		headers: { 'Content-Type': 'application/json' },
		...init
	});
	if (!res.ok) throw new Error(`HTTP ${res.status}: ${res.statusText}`);
	return res.json();
}

export const machinesApi = {
	list(category?: string): Promise<MachineListItem[]> {
		const qs = category ? `?category=${encodeURIComponent(category)}` : '';
		return apiFetch<MachineListItem[]>(`/machines${qs}`);
	},

	get(id: string): Promise<MachineDetail> {
		return apiFetch<MachineDetail>(`/machines/${id}`);
	},

	reset(id: string): Promise<MachineState> {
		return apiFetch<MachineState>(`/machines/${id}/reset`, { method: 'POST' });
	},

	load(id: string, payload: Record<string, unknown>): Promise<{ ok: boolean }> {
		return apiFetch<{ ok: boolean }>(`/machines/${id}/load`, {
			method: 'POST',
			body: JSON.stringify({ payload })
		});
	},

	step(id: string): Promise<MachineState> {
		return apiFetch<MachineState>(`/machines/${id}/step`, { method: 'POST' });
	},

	run(id: string, maxSteps = 1000): Promise<RunResult> {
		return apiFetch<RunResult>(`/machines/${id}/run?max_steps=${maxSteps}`, { method: 'POST' });
	},

	state(id: string): Promise<MachineState> {
		return apiFetch<MachineState>(`/machines/${id}/state`);
	}
};

export const CATEGORY_LABELS: Record<string, string> = {
	astronomical: 'Astronomical',
	calculator: 'Calculator',
	difference_engine: 'Difference Engine',
	analytical_engine: 'Analytical Engine',
	cipher: 'Cipher / Crypto',
	stored_program: 'Stored Program',
	tabulator: 'Tabulator'
};

export const CATEGORY_COLORS: Record<string, string> = {
	astronomical: '#6b21a8',
	calculator: '#1d4ed8',
	difference_engine: '#0f766e',
	analytical_engine: '#047857',
	cipher: '#b91c1c',
	stored_program: '#b45309',
	tabulator: '#374151'
};
