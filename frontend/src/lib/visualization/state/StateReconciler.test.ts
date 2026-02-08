/**
 * Unit Tests: StateReconciler
 *
 * Tests:
 * - State diffing
 * - Conflict detection and resolution
 * - Interpolation path generation
 * - Animation sequence generation
 * - Easing functions
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { StateReconciler } from './StateReconciler';
import type { MachineState } from './types';

function createTestState(overrides?: Partial<MachineState>): MachineState {
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
    shafts: Array.from({ length: 8 }, (_, i) => ({
      shaftIndex: i,
      shaftType:
        ['INPUT', 'ADDEND1', 'ADDEND2', 'ADDEND3', 'ADDEND4', 'ADDEND5', 'ADDEND6', 'OUTPUT'] as const[
          i
        ],
      rotation: 0,
      rotationVelocity: 0,
      isRotating: false
    })),
    phaseProgress: 0,
    timestamp: Date.now(),
    stateVersion: 0,
    ...overrides
  };
}

describe('StateReconciler', () => {
  let reconciler: StateReconciler;

  beforeEach(() => {
    reconciler = new StateReconciler();
  });

  describe('Diff Computation', () => {
    it('should detect phase changes', () => {
      const old = createTestState();
      const updated = createTestState({ phase: 'ADDITION' });

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.phaseChanged).toBe(true);
      expect(diff.phase).toBe('ADDITION');
    });

    it('should detect column changes', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.columnStates[0].value = 5;
      updated.columnStates[0].wheelRotation = Math.PI / 2;

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.columnChanges.length).toBe(1);
      expect(diff.columnChanges[0].oldValue).toBe(0);
      expect(diff.columnChanges[0].newValue).toBe(5);
      expect(diff.columnChanges[0].magnitude).toBe(5);
    });

    it('should detect multiple column changes', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.columnStates[0].value = 3;
      updated.columnStates[2].value = 7;
      updated.columnStates[5].value = 2;

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.columnChanges.length).toBe(3);
    });

    it('should detect carry changes', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.carryLevers[0].isEngaged = true;
      updated.carryLevers[2].hasCarryToPropagate = true;

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.carryChanges.length).toBe(2);
    });

    it('should detect shaft changes', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.shafts[0].rotation = Math.PI;
      updated.shafts[1].rotation = Math.PI / 2;

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.shaftChanges.length).toBe(2);
    });

    it('should detect version delta', () => {
      const old = createTestState({ stateVersion: 5 });
      const updated = createTestState({ stateVersion: 10 });

      const diff = reconciler.computeDiff(old, updated);

      expect(diff.stateVersionDelta).toBe(5);
    });
  });

  describe('Conflict Reconciliation', () => {
    it('should accept server state by default', () => {
      const client = createTestState({ columnStates: [
        { ...createTestState().columnStates[0], value: 3 }
      ] } as any);
      const server = createTestState({ columnStates: [
        { ...createTestState().columnStates[0], value: 7 }
      ] } as any);

      const result = reconciler.reconcile(client, server);

      expect(result.resolvedState.columnStates[0].value).toBe(7);
    });

    it('should detect conflicts', () => {
      const client = createTestState({
        stateVersion: 5,
        columnStates: Array.from({ length: 8 }, (_, i) => ({
          ...createTestState().columnStates[i],
          value: i + 1
        }))
      } as any);

      const server = createTestState({
        stateVersion: 10,
        columnStates: Array.from({ length: 8 }, (_, i) => ({
          ...createTestState().columnStates[i],
          value: i + 2
        }))
      } as any);

      const result = reconciler.reconcile(client, server);

      expect(result.conflicts.length).toBeGreaterThan(0);
    });

    it('should generate interpolation paths', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.columnStates[0].value = 5;

      const result = reconciler.reconcile(old, updated);

      expect(result.interpolationPaths.size).toBeGreaterThan(0);
    });

    it('should generate animation sequence', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.columnStates[0].value = 5;
      updated.columnStates[1].value = 3;

      const result = reconciler.reconcile(old, updated);

      expect(result.animationSequence.length).toBeGreaterThan(0);
    });

    it('should calculate animation time', () => {
      const old = createTestState();
      const updated = createTestState();
      updated.columnStates[0].value = 5;

      const result = reconciler.reconcile(old, updated);

      expect(result.timeToAnimate).toBeGreaterThan(0);
    });
  });

  describe('Easing Functions', () => {
    it('should evaluate linear easing', () => {
      const fn = StateReconciler.getEasingFunction('linear');
      expect(fn(0)).toBe(0);
      expect(fn(0.5)).toBe(0.5);
      expect(fn(1)).toBe(1);
    });

    it('should evaluate easeInOutQuad', () => {
      const fn = StateReconciler.getEasingFunction('easeInOutQuad');
      expect(fn(0)).toBe(0);
      expect(fn(1)).toBe(1);
      const mid = fn(0.5);
      expect(mid).toBeGreaterThan(0.25);
      expect(mid).toBeLessThan(0.75);
    });

    it('should evaluate easeInOutCubic', () => {
      const fn = StateReconciler.getEasingFunction('easeInOutCubic');
      expect(fn(0)).toBe(0);
      expect(fn(1)).toBe(1);
    });

    it('should evaluate easeInOutExpo', () => {
      const fn = StateReconciler.getEasingFunction('easeInOutExpo');
      expect(fn(0)).toBe(0);
      expect(fn(1)).toBe(1);
    });

    it('should evaluate easeOutBounce', () => {
      const fn = StateReconciler.getEasingFunction('easeOutBounce');
      expect(fn(0)).toBe(0);
      expect(fn(1)).toBe(1);
    });
  });

  describe('Interpolation', () => {
    it('should interpolate values with linear easing', () => {
      const interpolated = StateReconciler.interpolate(0, 10, 0.5, 'linear');
      expect(interpolated).toBe(5);
    });

    it('should interpolate with easeInOutQuad', () => {
      const interpolated = StateReconciler.interpolate(0, 10, 0.5, 'easeInOutQuad');
      expect(interpolated).toBeGreaterThan(4);
      expect(interpolated).toBeLessThan(6);
    });

    it('should clamp interpolation parameter', () => {
      const below = StateReconciler.interpolate(0, 10, -0.5, 'linear');
      expect(below).toBe(0);

      const above = StateReconciler.interpolate(0, 10, 1.5, 'linear');
      expect(above).toBe(10);
    });
  });

  describe('Validation', () => {
    it('should validate state values', () => {
      // Should not throw
      const valid = createTestState();
      const result = reconciler.reconcile(valid, valid);
      expect(result).toBeDefined();
    });
  });
});
