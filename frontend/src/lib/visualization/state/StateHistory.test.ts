/**
 * Unit Tests: StateHistory
 *
 * Tests:
 * - Circular buffer storage
 * - Navigation (previous, next, index)
 * - State retrieval
 * - Statistics calculation
 * - State comparison
 * - Transition finding
 * - Export functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { StateHistory } from './StateHistory';
import type { MachineState } from './types';

function createTestState(value: number): MachineState {
  return {
    phase: 'IDLE',
    elapsedTime: 0,
    isPaused: false,
    stepNumber: value,
    columnStates: Array.from({ length: 8 }, (_, i) => ({
      columnIndex: i,
      value: i === 0 ? value : 0,
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
    stateVersion: value
  };
}

describe('StateHistory', () => {
  let history: StateHistory;

  beforeEach(() => {
    history = new StateHistory();
  });

  describe('Adding States', () => {
    it('should add state to history', () => {
      const state = createTestState(1);
      history.addState(state);

      expect(history.length()).toBe(1);
    });

    it('should add multiple states', () => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }

      expect(history.length()).toBe(5);
    });

    it('should not exceed max size', () => {
      for (let i = 1; i <= 100; i++) {
        history.addState(createTestState(i));
      }

      expect(history.length()).toBeLessThanOrEqual(50);
    });
  });

  describe('Navigation', () => {
    beforeEach(() => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }
    });

    it('should get current state', () => {
      const current = history.getCurrentState();
      expect(current).toBeDefined();
      expect(current?.stepNumber).toBe(5);
    });

    it('should move to previous state', () => {
      const prev = history.movePrevious();
      expect(prev).toBeDefined();
      expect(prev?.stepNumber).toBe(4);
    });

    it('should move to next state', () => {
      history.movePrevious();
      const next = history.moveNext();
      expect(next).toBeDefined();
      expect(next?.stepNumber).toBe(5);
    });

    it('should move to specific index', () => {
      const state = history.moveToIndex(2);
      expect(state).toBeDefined();
      expect(state?.stepNumber).toBe(3);
    });

    it('should move to latest', () => {
      history.movePrevious();
      history.movePrevious();
      const latest = history.moveToLatest();
      expect(latest).toBeDefined();
      expect(latest?.stepNumber).toBe(5);
    });

    it('should not move beyond boundaries', () => {
      const first = history.moveToIndex(0);
      const prev = history.movePrevious();
      expect(prev).toBeNull();

      history.moveToIndex(4);
      const last = history.moveNext();
      expect(last).toBeNull();
    });
  });

  describe('State Retrieval', () => {
    beforeEach(() => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }
    });

    it('should get all states', () => {
      const all = history.getAllStates();
      expect(all.length).toBe(5);
    });

    it('should get last N states', () => {
      const last3 = history.getLastStates(3);
      expect(last3.length).toBe(3);
      expect(last3[last3.length - 1].stepNumber).toBe(5);
    });

    it('should get state at version', () => {
      const state = history.getStateAtVersion(3);
      expect(state).toBeDefined();
      expect(state?.stepNumber).toBe(3);
    });

    it('should return null for unknown version', () => {
      const state = history.getStateAtVersion(999);
      expect(state).toBeNull();
    });

    it('should get state at offset', () => {
      history.moveToIndex(2);
      const offset = history.getAtOffset(1);
      expect(offset).toBeDefined();
      expect(offset?.stepNumber).toBe(4);
    });
  });

  describe('Index Tracking', () => {
    beforeEach(() => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }
    });

    it('should track current index', () => {
      expect(history.getCurrentIndex()).toBe(4);
      history.movePrevious();
      expect(history.getCurrentIndex()).toBe(3);
    });

    it('should detect if at latest', () => {
      expect(history.isAtLatest()).toBe(true);
      history.movePrevious();
      expect(history.isAtLatest()).toBe(false);
    });

    it('should detect if at oldest', () => {
      expect(history.isAtOldest()).toBe(false);
      history.moveToIndex(0);
      expect(history.isAtOldest()).toBe(true);
    });
  });

  describe('Statistics', () => {
    it('should calculate statistics', () => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }

      const stats = history.getStatistics();
      expect(stats.totalStates).toBe(5);
      expect(stats.totalTime).toBeGreaterThanOrEqual(0);
      expect(stats.averageTimeBetweenUpdates).toBeGreaterThanOrEqual(0);
    });

    it('should handle empty history stats', () => {
      const stats = history.getStatistics();
      expect(stats.totalStates).toBe(0);
      expect(stats.totalTime).toBe(0);
    });
  });

  describe('State Comparison', () => {
    it('should compare states', () => {
      const state1 = createTestState(1);
      const state2 = createTestState(2);
      state2.phase = 'ADDITION';
      state2.columnStates[1].value = 5;

      const comparison = history.compareStates(state1, state2);

      expect(comparison.phaseChanged).toBe(true);
      expect(comparison.columnChanges).toBeGreaterThan(0);
    });
  });

  describe('Transition Finding', () => {
    beforeEach(() => {
      for (let i = 1; i <= 5; i++) {
        const state = createTestState(i);
        if (i >= 3) state.phase = 'ADDITION';
        history.addState(state);
      }
    });

    it('should find phase transition', () => {
      const transition = history.findTransition(
        (prev, curr) => prev.phase !== curr.phase
      );

      expect(transition).toBeDefined();
      expect(transition?.fromState.phase).toBe('IDLE');
      expect(transition?.toState.phase).toBe('ADDITION');
    });

    it('should find all transitions', () => {
      const transitions = history.findAllTransitions(
        (prev, curr) => prev.phase !== curr.phase
      );

      expect(transitions.length).toBeGreaterThan(0);
    });

    it('should return null for non-existent transition', () => {
      const transition = history.findTransition(
        (prev, curr) => prev.stepNumber > 1000
      );

      expect(transition).toBeNull();
    });
  });

  describe('Export and Debug', () => {
    beforeEach(() => {
      for (let i = 1; i <= 3; i++) {
        history.addState(createTestState(i));
      }
    });

    it('should export as JSON', () => {
      const json = history.exportAsJSON();
      expect(typeof json).toBe('string');

      const parsed = JSON.parse(json);
      expect(parsed.metadata).toBeDefined();
      expect(parsed.entries).toBeDefined();
    });

    it('should get snapshots', () => {
      const snapshots = history.getSnapshots();
      expect(snapshots.length).toBe(3);
      expect(snapshots[0].phase).toBe('IDLE');
    });

    it('should generate summary', () => {
      const summary = history.getSummary();
      expect(summary).toContain('State History Summary');
      expect(summary).toContain('Total States: 3');
    });
  });

  describe('Clear', () => {
    it('should clear history', () => {
      for (let i = 1; i <= 5; i++) {
        history.addState(createTestState(i));
      }

      expect(history.length()).toBe(5);

      history.clear();

      expect(history.length()).toBe(0);
      expect(history.getCurrentState()).toBeNull();
    });
  });

  describe('Circular Buffer Behavior', () => {
    it('should wrap around when full', () => {
      for (let i = 1; i <= 60; i++) {
        history.addState(createTestState(i));
      }

      expect(history.length()).toBe(50);

      const latest = history.moveToLatest();
      expect(latest?.stepNumber).toBe(60);

      // Oldest should be from iteration 11 (1-10 were overwritten)
      history.moveToIndex(0);
      const oldest = history.getCurrentState();
      expect(oldest?.stepNumber).toBeGreaterThanOrEqual(11);
    });
  });

  describe('Integration', () => {
    it('should handle complex navigation', () => {
      for (let i = 1; i <= 10; i++) {
        history.addState(createTestState(i));
      }

      // Navigate backwards
      for (let i = 0; i < 5; i++) {
        history.movePrevious();
      }
      expect(history.getCurrentIndex()).toBe(4);

      // Jump to index
      history.moveToIndex(8);
      expect(history.getCurrentIndex()).toBe(8);

      // Move forward
      history.moveNext();
      expect(history.getCurrentIndex()).toBe(9);

      // Jump to latest
      history.movePrevious();
      history.moveToLatest();
      expect(history.isAtLatest()).toBe(true);
    });
  });
});
