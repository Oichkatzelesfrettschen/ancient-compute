import type { AnalyticalEngineSnapshot } from '../types/analytical_engine';
import type { DebugState, PerformanceReport, DebugResponse } from '$lib/types'; // Import from central types file

const BASE_URL = 'http://localhost:8000/api/v1/tools';

export const toolsApi = {
    async step(): Promise<DebugResponse> {
        const res = await fetch(`${BASE_URL}/debug/step`, { method: 'POST' });
        return res.json();
    },

    async command(action: 'reset' | 'continue' | 'pause'): Promise<{ status: string }> {
        const res = await fetch(`${BASE_URL}/debug/command`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ action })
        });
        return res.json();
    },

    async getPerformance(): Promise<PerformanceReport> {
        const res = await fetch(`${BASE_URL}/performance`);
        return res.json();
    }
};