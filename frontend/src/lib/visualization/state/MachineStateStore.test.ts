/**
 * Unit Tests: MachineStateStore (Svelte Writable Store)
 *
 * Tests:
 * - Store initialization
 * - State updates and subscriptions
 * - Column/carry/shaft updates
 * - Phase management
 * - Circular buffer history
 * - Metrics tracking
 * - Reset functionality
 */

import { describe, it, expect, beforeEach } from 'vitest';
import {
  createMachineStateStore,
  currentPhase,
  currentStep,
  isPausedStore,
  columnValues
} from './MachineStateStore';
import type { MachineState } from './types';

describe('MachineStateStore', () => {
  let store: ReturnType<typeof createMachineStateStore>;

  beforeEach(() => {
    store = createMachineStateStore();
  });

  describe('Initialization', () => {
    it('should create store with initial state', () => {
      const state = store.getState();
      expect(state).toBeDefined();
      expect(state.phase).toBe('IDLE');
      expect(state.stepNumber).toBe(0);
      expect(state.isPaused).toBe(false);
    });

    it('should have 8 columns initialized', () => {
      const state = store.getState();
      expect(state.columnStates).toHaveLength(8);
      for (let i = 0; i < 8; i++) {
        expect(state.columnStates[i].columnIndex).toBe(i);
        expect(state.columnStates[i].value).toBe(0);
      }
    });

    it('should have 8 carry levers initialized', () => {
      const state = store.getState();
      expect(state.carryLevers).toHaveLength(8);
      for (let i = 0; i < 8; i++) {
        expect(state.carryLevers[i].columnIndex).toBe(i);
        expect(state.carryLevers[i].isEngaged).toBe(false);
      }
    });

    it('should have 8 shafts initialized', () => {
      const state = store.getState();
      expect(state.shafts).toHaveLength(8);
      expect(state.shafts[0].shaftType).toBe('INPUT');
      expect(state.shafts[7].shaftType).toBe('OUTPUT');
    });
  });

  describe('State Updates', () => {
    it('should update complete state', () => {
      const newState = store.getState();
      newState.stepNumber = 5;
      newState.phase = 'ADDITION';
      store.setState(newState);

      const state = store.getState();
      expect(state.stepNumber).toBe(5);
      expect(state.phase).toBe('ADDITION');
    });

    it('should update single column', () => {
      store.updateColumn(0, { value: 5, carryActive: true });
      const state = store.getState();
      expect(state.columnStates[0].value).toBe(5);
      expect(state.columnStates[0].carryActive).toBe(true);
    });

    it('should update multiple columns', () => {
      const updates = new Map([
        [0, { value: 3 }],
        [1, { value: 7 }],
        [2, { value: 2 }]
      ]);
      store.updateColumns(updates);

      const state = store.getState();
      expect(state.columnStates[0].value).toBe(3);
      expect(state.columnStates[1].value).toBe(7);
      expect(state.columnStates[2].value).toBe(2);
    });

    it('should update carry lever', () => {
      store.updateCarryLever(0, { isEngaged: true, rotationAngle: 90 });
      const state = store.getState();
      expect(state.carryLevers[0].isEngaged).toBe(true);
      expect(state.carryLevers[0].rotationAngle).toBe(90);
    });

    it('should update shaft', () => {
      const angle = Math.PI / 2;
      store.updateShaft(0, { rotation: angle, isRotating: true });
      const state = store.getState();
      expect(state.shafts[0].rotation).toBe(angle);
      expect(state.shafts[0].isRotating).toBe(true);
    });

    it('should increment state version on update', () => {
      const initial = store.getState().stateVersion;
      store.updateColumn(0, { value: 1 });
      const updated = store.getState().stateVersion;
      expect(updated).toBeGreaterThan(initial);
    });

    it('should update timestamp on change', () => {
      const beforeTime = Date.now();
      store.updateColumn(0, { value: 1 });
      const state = store.getState();
      expect(state.timestamp).toBeGreaterThanOrEqual(beforeTime);
    });
  });

  describe('Phase Management', () => {
    it('should set phase', () => {
      store.setPhase('ADDITION', 0.5);
      const state = store.getState();
      expect(state.phase).toBe('ADDITION');
      expect(state.phaseProgress).toBe(0.5);
    });

    it('should change through all phases', () => {
      const phases = [
        'IDLE',
        'INPUT',
        'ADDITION',
        'CARRY',
        'OUTPUT',
        'ADVANCE',
        'RESET',
        'PAUSE'
      ] as const;

      for (const phase of phases) {
        store.setPhase(phase);
        const state = store.getState();
        expect(state.phase).toBe(phase);
      }
    });
  });

  describe('Pause/Resume', () => {
    it('should set pause state', () => {
      store.setPaused(true);
      const state = store.getState();
      expect(state.isPaused).toBe(true);

      store.setPaused(false);
      const state2 = store.getState();
      expect(state2.isPaused).toBe(false);
    });
  });

  describe('Step Management', () => {
    it('should increment step number', () => {
      const initial = store.getState().stepNumber;
      store.nextStep();
      const updated = store.getState().stepNumber;
      expect(updated).toBe(initial + 1);
    });

    it('should increment step multiple times', () => {
      const initial = store.getState().stepNumber;
      store.nextStep();
      store.nextStep();
      store.nextStep();
      const updated = store.getState().stepNumber;
      expect(updated).toBe(initial + 3);
    });
  });

  describe('History', () => {
    it('should track state history', () => {
      store.updateColumn(0, { value: 1 });
      store.updateColumn(0, { value: 2 });
      store.updateColumn(0, { value: 3 });

      const history = store.getHistory(5);
      expect(history.length).toBeGreaterThan(0);
    });

    it('should revert to previous state', () => {
      store.updateColumn(0, { value: 5 });
      const beforeRevert = store.getState();

      store.updateColumn(0, { value: 10 });
      expect(store.getState().columnStates[0].value).toBe(10);

      store.revertToPrevious(1);
      const afterRevert = store.getState();
      expect(afterRevert.columnStates[0].value).toBe(5);
    });

    it('should retrieve state at version', () => {
      const initial = store.getState().stateVersion;
      store.updateColumn(0, { value: 5 });

      const versionState = store.getStateAtVersion(initial);
      expect(versionState).toBeDefined();
    });

    it('should get all history', () => {
      store.updateColumn(0, { value: 1 });
      store.updateColumn(1, { value: 2 });
      store.updateColumn(2, { value: 3 });

      const allHistory = store.getAllHistory();
      expect(allHistory.length).toBeGreaterThan(0);
    });
  });

  describe('Reset', () => {
    it('should reset to initial state', () => {
      store.updateColumn(0, { value: 5 });
      store.setPhase('ADDITION');
      store.nextStep();

      store.reset();

      const state = store.getState();
      expect(state.phase).toBe('IDLE');
      expect(state.stepNumber).toBe(0);
      expect(state.columnStates[0].value).toBe(0);
    });

    it('should clear history on reset', () => {
      store.updateColumn(0, { value: 1 });
      store.updateColumn(0, { value: 2 });

      store.reset();

      // History should only have initial state
      const history = store.getHistory(100);
      expect(history.length).toBeLessThanOrEqual(2);
    });
  });

  describe('Metrics', () => {
    it('should track metrics', () => {
      const metrics = store.getMetrics();
      expect(metrics).toBeDefined();
      expect(metrics.lastUpdateTimestamp).toBeLessThanOrEqual(Date.now());
      expect(metrics.stateVersionCount).toBeGreaterThanOrEqual(0);
    });

    it('should update metrics', () => {
      store.updateMetrics({ droppedMessageCount: 5 });
      const metrics = store.getMetrics();
      expect(metrics.droppedMessageCount).toBe(5);
    });

    it('should record message operations', () => {
      store.recordMessageReceived(10, 256);
      store.recordMessageSent();
      store.recordReconnect();
      store.recordConflict();
      store.recordInterpolation();

      const stats = store.getDebugStats();
      expect(stats.messagesReceived).toBe(1);
      expect(stats.messagesSent).toBe(1);
      expect(stats.reconnectCount).toBe(1);
      expect(stats.conflictCount).toBe(1);
      expect(stats.interpolationCount).toBe(1);
    });

    it('should clear debug stats', () => {
      store.recordMessageReceived(10);
      store.recordMessageSent();

      store.clearDebugStats();

      const stats = store.getDebugStats();
      expect(stats.messagesReceived).toBe(0);
      expect(stats.messagesSent).toBe(0);
    });
  });

  describe('Subscriptions', () => {
    it('should notify subscribers on state change', () => {
      let notified = false;
      const unsubscribe = store.subscribe((state) => {
        if (state.columnStates[0].value === 5) {
          notified = true;
        }
      });

      store.updateColumn(0, { value: 5 });
      expect(notified).toBe(true);

      unsubscribe();
    });
  });

  describe('Derived Stores', () => {
    it('should derive current phase', (done) => {
      store.setPhase('ADDITION');

      const unsubscribe = currentPhase.subscribe((phase) => {
        if (phase === 'ADDITION') {
          expect(phase).toBe('ADDITION');
          unsubscribe();
          done();
        }
      });
    });

    it('should derive current step', (done) => {
      store.nextStep();
      store.nextStep();

      const unsubscribe = currentStep.subscribe((step) => {
        expect(step).toBeGreaterThanOrEqual(2);
        unsubscribe();
        done();
      });
    });

    it('should derive paused state', (done) => {
      store.setPaused(true);

      const unsubscribe = isPausedStore.subscribe((paused) => {
        if (paused === true) {
          expect(paused).toBe(true);
          unsubscribe();
          done();
        }
      });
    });

    it('should derive column values', (done) => {
      store.updateColumns(
        new Map([
          [0, { value: 3 }],
          [1, { value: 7 }],
          [2, { value: 2 }]
        ])
      );

      const unsubscribe = columnValues.subscribe((values) => {
        if (values[0] === 3 && values[1] === 7) {
          expect(values[0]).toBe(3);
          expect(values[1]).toBe(7);
          expect(values[2]).toBe(2);
          unsubscribe();
          done();
        }
      });
    });
  });

  describe('Edge Cases', () => {
    it('should handle column value boundaries', () => {
      store.updateColumn(0, { value: 0 });
      expect(store.getState().columnStates[0].value).toBe(0);

      store.updateColumn(0, { value: 9 });
      expect(store.getState().columnStates[0].value).toBe(9);
    });

    it('should handle rapid updates', () => {
      for (let i = 0; i < 100; i++) {
        store.updateColumn(i % 8, { value: i % 10 });
      }

      const state = store.getState();
      expect(state.stateVersion).toBeGreaterThanOrEqual(100);
    });

    it('should maintain data integrity', () => {
      store.updateColumns(
        new Map(Array.from({ length: 8 }, (_, i) => [i, { value: (i + 1) % 10 }]))
      );

      const state = store.getState();
      for (let i = 0; i < 8; i++) {
        expect(state.columnStates[i].value).toBe((i + 1) % 10);
      }
    });
  });
});
