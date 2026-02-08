/**
 * API Client Unit Tests for Difference Engine Emulator
 *
 * Tests the frontend API client functions that communicate with the backend
 * Covers:
 * - Polynomial execution requests
 * - State inspection
 * - Debugger operations (breakpoints, variables, step/continue)
 * - Error handling and network failures
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import {
  executePolynomial,
  resetEmulator,
  stepCycle,
  setBreakpoint,
  enableBreakpoint,
  disableBreakpoint,
  removeBreakpoint,
  debugStep,
  debugContinue,
  defineVariable,
  setVariable,
  getState,
  getResults
} from './emulator';

// Mock fetch globally
global.fetch = vi.fn();

describe('Emulator API Client', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  // ============================================================================
  // Polynomial Execution Tests
  // ============================================================================

  describe('executePolynomial', () => {
    it('should execute linear polynomial and return results', async () => {
      const mockResponse = {
        success: true,
        results: [
          { x: 1, result: 3, cycle: 10, phase: 'OUTPUT' },
          { x: 2, result: 5, cycle: 20, phase: 'OUTPUT' },
          { x: 3, result: 7, cycle: 30, phase: 'OUTPUT' }
        ],
        totalCycles: 30
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockResponse
      });

      const result = await executePolynomial([1, 2], [1, 3], 1.0);

      expect(result.success).toBe(true);
      expect(result.results).toHaveLength(3);
      expect(result.results?.[0].result).toBe(3);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/execute'),
        expect.objectContaining({
          method: 'POST',
          body: expect.stringContaining('"coefficients":[1,2]')
        })
      );
    });

    it('should handle network errors gracefully', async () => {
      global.fetch = vi.fn().mockRejectedValueOnce(new Error('Network timeout'));

      const result = await executePolynomial([1, 2], [1, 5], 1.0);

      expect(result.success).toBe(false);
      expect(result.error).toContain('Network timeout');
    });

    it('should handle HTTP errors with error detail', async () => {
      const mockErrorResponse = {
        success: false,
        error: 'Invalid coefficients'
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: false,
        status: 400,
        json: async () => ({ detail: 'Invalid request' })
      });

      const result = await executePolynomial([], [1, 5], 1.0);

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should respect execution speed parameter', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true, results: [] })
      });

      await executePolynomial([1, 2], [1, 5], 2.5);

      const callBody = JSON.parse(
        (global.fetch as any).mock.calls[0][1].body
      );
      expect(callBody.execution_speed).toBe(2.5);
    });

    it('should handle quadratic polynomial evaluation', async () => {
      const mockResponse = {
        success: true,
        results: [
          { x: 1, result: 2, cycle: 10, phase: 'OUTPUT' },
          { x: 2, result: 5, cycle: 20, phase: 'OUTPUT' },
          { x: 3, result: 10, cycle: 30, phase: 'OUTPUT' }
        ],
        totalCycles: 30
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockResponse
      });

      const result = await executePolynomial([1, 0, 1], [1, 3], 1.0);

      expect(result.success).toBe(true);
      expect(result.results?.[2].result).toBe(10); // x=3: 3^2 + 1 = 10
    });
  });

  // ============================================================================
  // State Inspection Tests
  // ============================================================================

  describe('getState', () => {
    it('should retrieve current machine state', async () => {
      const mockState = {
        success: true,
        state: {
          cycle: 42,
          phase: 'CARRY',
          angle: 12345,
          columns: [0, 1, 2, 3, 4, 5, 6, 7],
          carrySignals: [false, true, false, false, false, false, false, false],
          accumulator: 100,
          totalOperations: 420
        }
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockState
      });

      const result = await getState();

      expect(result.success).toBe(true);
      expect(result.state?.cycle).toBe(42);
      expect(result.state?.phase).toBe('CARRY');
      expect(result.state?.columns).toHaveLength(8);
    });

    it('should handle state retrieval errors', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: false,
        status: 400,
        json: async () => ({ detail: 'Emulator not initialized' })
      });

      const result = await getState();

      expect(result.success).toBe(false);
    });
  });

  describe('getResults', () => {
    it('should retrieve previous execution results', async () => {
      const mockResults = {
        success: true,
        results: [
          { x: 1, result: 3 },
          { x: 2, result: 5 }
        ]
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockResults
      });

      const result = await getResults();

      expect(result.success).toBe(true);
      expect(result.results).toBeDefined();
    });
  });

  describe('resetEmulator', () => {
    it('should send reset request', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await resetEmulator();

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/reset'),
        expect.objectContaining({ method: 'POST' })
      );
    });
  });

  // ============================================================================
  // Breakpoint Tests
  // ============================================================================

  describe('setBreakpoint', () => {
    it('should set cycle breakpoint', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true, breakpointId: 1 })
      });

      const result = await setBreakpoint('CYCLE', { cycle_target: 100 });

      expect(result.success).toBe(true);
      expect(result.breakpointId).toBe(1);
      const body = JSON.parse((global.fetch as any).mock.calls[0][1].body);
      expect(body.type).toBe('CYCLE');
      expect(body.cycle_target).toBe(100);
    });

    it('should set phase breakpoint', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true, breakpointId: 2 })
      });

      const result = await setBreakpoint('PHASE', { phase_target: 'CARRY' });

      expect(result.success).toBe(true);
      const body = JSON.parse((global.fetch as any).mock.calls[0][1].body);
      expect(body.phase_target).toBe('CARRY');
    });

    it('should set value change breakpoint', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true, breakpointId: 3 })
      });

      const result = await setBreakpoint('VALUE_CHANGE', { variable_name: 'accumulator' });

      expect(result.success).toBe(true);
      const body = JSON.parse((global.fetch as any).mock.calls[0][1].body);
      expect(body.variable_name).toBe('accumulator');
    });

    it('should handle breakpoint creation errors', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: false,
        status: 400,
        json: async () => ({ detail: 'Invalid breakpoint type' })
      });

      const result = await setBreakpoint('INVALID', {});

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });
  });

  describe('enableBreakpoint', () => {
    it('should enable a breakpoint by ID', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await enableBreakpoint(1);

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/debug/breakpoint/1/enable'),
        expect.objectContaining({ method: 'POST' })
      );
    });
  });

  describe('disableBreakpoint', () => {
    it('should disable a breakpoint by ID', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await disableBreakpoint(1);

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/debug/breakpoint/1/disable'),
        expect.objectContaining({ method: 'POST' })
      );
    });
  });

  describe('removeBreakpoint', () => {
    it('should delete a breakpoint by ID', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await removeBreakpoint(1);

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/debug/breakpoint/1'),
        expect.objectContaining({ method: 'DELETE' })
      );
    });
  });

  // ============================================================================
  // Variable Management Tests
  // ============================================================================

  describe('defineVariable', () => {
    it('should define a new debugger variable', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await defineVariable('myvar', 42);

      expect(result.success).toBe(true);
      const body = JSON.parse((global.fetch as any).mock.calls[0][1].body);
      expect(body.name).toBe('myvar');
      expect(body.value).toBe(42);
    });

    it('should handle variable definition errors', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: false,
        status: 400,
        json: async () => ({ detail: 'Variable already exists' })
      });

      const result = await defineVariable('dup', 10);

      expect(result.success).toBe(false);
    });
  });

  describe('setVariable', () => {
    it('should update variable value', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await setVariable('myvar', 100);

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/debug/variable/myvar'),
        expect.objectContaining({ method: 'PUT' })
      );
    });
  });

  // ============================================================================
  // Step/Continue Tests
  // ============================================================================

  describe('debugStep', () => {
    it('should step one mechanical cycle', async () => {
      const mockResponse = {
        success: true,
        state: {
          cycle: 1,
          phase: 'ADDITION',
          angle: 1000,
          columns: [0, 0, 0, 0, 0, 0, 0, 0],
          carrySignals: [false, false, false, false, false, false, false, false],
          accumulator: 0,
          totalOperations: 0
        },
        breakpointsHit: []
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockResponse
      });

      const result = await debugStep();

      expect(result.success).toBe(true);
      expect(result.state?.cycle).toBe(1);
      expect(result.breakpointsHit).toBeDefined();
    });
  });

  describe('stepCycle', () => {
    it('should step single cycle', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true })
      });

      const result = await stepCycle();

      expect(result.success).toBe(true);
      expect(global.fetch).toHaveBeenCalledWith(
        expect.stringContaining('/api/step'),
        expect.objectContaining({ method: 'POST' })
      );
    });
  });

  describe('debugContinue', () => {
    it('should continue execution with optional max cycles', async () => {
      const mockResponse = {
        success: true,
        cyclesRun: 50,
        state: {
          cycle: 50,
          phase: 'TABLE',
          angle: 50000,
          columns: [1, 2, 3, 4, 5, 6, 7, 8],
          carrySignals: [false, false, false, false, false, false, false, false],
          accumulator: 100,
          totalOperations: 200
        },
        breakpointHit: true
      };

      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => mockResponse
      });

      const result = await debugContinue(100);

      expect(result.success).toBe(true);
      expect(result.cyclesRun).toBe(50);
      expect(result.breakpointHit).toBe(true);
    });

    it('should handle continue without max cycles', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => ({ success: true, cyclesRun: 1000 })
      });

      const result = await debugContinue();

      expect(result.success).toBe(true);
    });
  });

  // ============================================================================
  // Integration-like Tests (multiple operations)
  // ============================================================================

  describe('Integration: Execute then Step', () => {
    it('should execute polynomial and then step through results', async () => {
      // Execute response
      global.fetch = vi.fn()
        .mockResolvedValueOnce({
          ok: true,
          json: async () => ({
            success: true,
            results: [{ x: 1, result: 3, cycle: 10, phase: 'OUTPUT' }],
            totalCycles: 10
          })
        })
        .mockResolvedValueOnce({
          ok: true,
          json: async () => ({
            success: true,
            state: {
              cycle: 11,
              phase: 'ADDITION',
              angle: 1000,
              columns: [0, 0, 0, 0, 0, 0, 0, 0],
              carrySignals: [false, false, false, false, false, false, false, false],
              accumulator: 0,
              totalOperations: 0
            },
            breakpointsHit: []
          })
        });

      const execResult = await executePolynomial([1, 2], [1, 1], 1.0);
      expect(execResult.success).toBe(true);

      const stepResult = await debugStep();
      expect(stepResult.success).toBe(true);
      expect(stepResult.state?.cycle).toBe(11);
    });
  });

  describe('Integration: Set Breakpoint then Execute', () => {
    it('should set breakpoint and then execute', async () => {
      // Set breakpoint response
      global.fetch = vi.fn()
        .mockResolvedValueOnce({
          ok: true,
          json: async () => ({ success: true, breakpointId: 5 })
        })
        .mockResolvedValueOnce({
          ok: true,
          json: async () => ({
            success: true,
            results: [{ x: 1, result: 3, cycle: 10, phase: 'OUTPUT' }],
            totalCycles: 10
          })
        });

      const bpResult = await setBreakpoint('CYCLE', { cycle_target: 50 });
      expect(bpResult.success).toBe(true);
      expect(bpResult.breakpointId).toBe(5);

      const execResult = await executePolynomial([1, 2], [1, 1], 1.0);
      expect(execResult.success).toBe(true);
    });
  });

  // ============================================================================
  // Error Edge Cases
  // ============================================================================

  describe('Error handling', () => {
    it('should handle malformed JSON response', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: true,
        json: async () => {
          throw new Error('Invalid JSON');
        }
      });

      const result = await executePolynomial([1, 2], [1, 5], 1.0);

      expect(result.success).toBe(false);
      expect(result.error).toBeDefined();
    });

    it('should handle HTTP 500 server error', async () => {
      global.fetch = vi.fn().mockResolvedValueOnce({
        ok: false,
        status: 500,
        json: async () => ({ detail: 'Internal server error' })
      });

      const result = await executePolynomial([1, 2], [1, 5], 1.0);

      expect(result.success).toBe(false);
      expect(result.error).toContain('500');
    });

    it('should handle timeout as network error', async () => {
      global.fetch = vi.fn().mockRejectedValueOnce(
        new Error('Request timeout')
      );

      const result = await executePolynomial([1, 2], [1, 5], 1.0);

      expect(result.success).toBe(false);
      expect(result.error).toContain('timeout');
    });
  });
});
