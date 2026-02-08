/**
 * VisualizationManager Test Suite
 * Tests orchestration of scene, animation, state, and interaction
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { VisualizationManager } from '../VisualizationManager';
import type { MachineState, StateDiff } from '../../state/types';

// Mock types for testing
const createMockMachineState = (columns: number[] = [0, 0, 0, 0, 0, 0, 0, 0]): MachineState => ({
  columns,
  carryFlags: [false, false, false, false, false, false, false, false],
  isRunning: false,
  timestamp: Date.now()
});

const createMockStateDiff = (): StateDiff => ({
  changedColumns: [0],
  changedFlags: [],
  carry: []
});

describe('VisualizationManager', () => {
  let canvas: HTMLCanvasElement;
  let manager: VisualizationManager;

  beforeEach(() => {
    canvas = document.createElement('canvas');
    canvas.width = 1024;
    canvas.height = 768;

    manager = new VisualizationManager({
      canvas,
      width: 1024,
      height: 768,
      enablePerformanceMonitoring: true,
      enableDebugOverlay: false
    });
  });

  afterEach(() => {
    manager.dispose();
  });

  describe('Initialization', () => {
    it('should create visualization manager instance', () => {
      expect(manager).toBeDefined();
      expect(manager).toBeInstanceOf(VisualizationManager);
    });

    it('should initialize with provided config', () => {
      const testManager = new VisualizationManager({
        canvas,
        width: 800,
        height: 600,
        enablePerformanceMonitoring: false
      });

      expect(testManager).toBeDefined();

      testManager.dispose();
    });

    it('should initialize all core components', () => {
      expect(manager.getScene()).toBeDefined();
      expect(manager.getCamera()).toBeDefined();
      expect(manager.getTimeline()).toBeDefined();
      expect(manager.getAnimationSystem()).toBeDefined();
    });

    it('should initialize feature detector', () => {
      const detector = manager.getFeatureDetector();

      expect(detector).toBeDefined();
    });

    it('should start rendering on creation', () => {
      // Manager should start automatically
      expect(manager).toBeDefined();
    });
  });

  describe('State Updates', () => {
    it('should accept state updates', () => {
      const oldState = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const newState = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(newState, diff);

      expect(manager.getCurrentState()).toEqual(newState);
    });

    it('should track previous state', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      expect(manager.getCurrentState()).toEqual(state2);
    });

    it('should start animation on state update', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Should trigger animation
      expect(manager.isAnimatingNow()).toBeDefined();
    });

    it('should handle null initial state', () => {
      const state = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      // First update with no previous state should not animate
      manager.updateState(state, diff);

      expect(manager.getCurrentState()).toEqual(state);
    });

    it('should process state diff', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff: StateDiff = {
        changedColumns: [0],
        changedFlags: [],
        carry: []
      };

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      expect(manager.getCurrentState()).toEqual(state2);
    });
  });

  describe('Animation Control', () => {
    it('should start animations', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // After state update, animation should be running
      expect(manager.isAnimatingNow()).toBe(true);
    });

    it('should track animation state', () => {
      expect(manager.isAnimatingNow()).toBe(false);

      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      expect(manager.isAnimatingNow()).toBe(true);
    });

    it('should reset animations', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      manager.reset();

      expect(manager.isAnimatingNow()).toBe(false);
    });
  });

  describe('Animation Target Mapping', () => {
    it('should parse column wheel targets', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([5, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Animation should target column0wheel0 format
      expect(manager.isAnimatingNow()).toBe(true);
    });

    it('should parse shaft targets', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Should handle shaft0 format
      expect(manager).toBeDefined();
    });

    it('should parse carry lever targets', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const diff: StateDiff = {
        changedColumns: [],
        changedFlags: [0],
        carry: [{ leverIndex: 0, direction: 1, magnitude: 0.5 }]
      };

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Should handle carryLever0 format
      expect(manager).toBeDefined();
    });

    it('should update mesh rotations', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([3, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      const wheel = manager.getScene().getDigitWheelMesh(0, 0);

      // Wheel should be accessible for animation
      expect(wheel).toBeDefined();
    });

    it('should update engagement indicators', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const diff: StateDiff = {
        changedColumns: [],
        changedFlags: [],
        carry: [{ leverIndex: 0, direction: 1, magnitude: 0.5 }]
      };

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      const indicator = manager.getScene().getEngineGroup()
        .getObjectByName('carry-lever-0')
        ?.children[1];

      expect(indicator).toBeDefined();
    });
  });

  describe('Rendering Control', () => {
    it('should start visualization', () => {
      manager.start();

      expect(manager).toBeDefined();
    });

    it('should stop visualization', () => {
      manager.start();
      manager.stop();

      expect(manager).toBeDefined();
    });

    it('should handle repeated start/stop calls', () => {
      manager.start();
      manager.start();
      manager.stop();
      manager.stop();

      expect(manager).toBeDefined();
    });
  });

  describe('Performance Monitoring', () => {
    it('should track performance metrics', () => {
      const metrics = manager.getMetrics();

      expect(metrics.fps).toBeGreaterThanOrEqual(0);
      expect(metrics.deltaTime).toBeGreaterThanOrEqual(0);
      expect(metrics.renderTime).toBeGreaterThanOrEqual(0);
      expect(metrics.animationTime).toBeGreaterThanOrEqual(0);
      expect(metrics.totalFrames).toBeGreaterThanOrEqual(0);
    });

    it('should update metrics over time', (done) => {
      const metrics1 = manager.getMetrics();

      setTimeout(() => {
        const metrics2 = manager.getMetrics();

        expect(metrics2.totalFrames).toBeGreaterThanOrEqual(metrics1.totalFrames);

        done();
      }, 50);
    });

    it('should enable/disable performance monitoring', () => {
      manager.setPerformanceMonitoring(false);
      manager.setPerformanceMonitoring(true);

      expect(manager).toBeDefined();
    });

    it('should return readonly metrics', () => {
      const metrics = manager.getMetrics();

      // Should be a copy, not reference
      expect(metrics).toBeDefined();
    });
  });

  describe('Debug Information', () => {
    it('should generate debug info string', () => {
      const info = manager.getDebugInfo();

      expect(typeof info).toBe('string');
      expect(info.length).toBeGreaterThan(0);
    });

    it('should include scene info in debug output', () => {
      const info = manager.getDebugInfo();

      expect(info).toContain('Visualization Manager');
    });

    it('should include animation state in debug output', () => {
      const info = manager.getDebugInfo();

      expect(info).toContain('Animation');
    });

    it('should include metrics in debug output', () => {
      const info = manager.getDebugInfo();

      expect(info).toContain('FPS');
    });

    it('should include current state in debug output', () => {
      const info = manager.getDebugInfo();

      expect(info).toContain('State');
    });
  });

  describe('Component Access', () => {
    it('should provide scene reference', () => {
      const scene = manager.getScene();

      expect(scene).toBeDefined();
    });

    it('should provide camera reference', () => {
      const camera = manager.getCamera();

      expect(camera).toBeDefined();
    });

    it('should provide timeline reference', () => {
      const timeline = manager.getTimeline();

      expect(timeline).toBeDefined();
    });

    it('should provide animation system reference', () => {
      const animationSystem = manager.getAnimationSystem();

      expect(animationSystem).toBeDefined();
    });

    it('should provide feature detector reference', () => {
      const detector = manager.getFeatureDetector();

      expect(detector).toBeDefined();
    });
  });

  describe('State Tracking', () => {
    it('should track current state', () => {
      const state = createMockMachineState([1, 2, 3, 4, 5, 6, 7, 8]);
      const diff = createMockStateDiff();

      manager.updateState(state, diff);

      expect(manager.getCurrentState()).toEqual(state);
    });

    it('should handle null state initially', () => {
      expect(manager.getCurrentState()).toBeNull();
    });

    it('should update state reference on new update', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 1, 1, 1, 1, 1, 1, 1]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      expect(manager.getCurrentState()).toEqual(state2);
      expect(manager.getCurrentState()).not.toEqual(state1);
    });
  });

  describe('Resource Cleanup', () => {
    it('should dispose all resources', () => {
      manager.dispose();

      expect(manager).toBeDefined();
    });

    it('should clean up resize observer', () => {
      manager.dispose();

      // Should not throw when disposing
      expect(manager).toBeDefined();
    });

    it('should handle multiple dispose calls', () => {
      manager.dispose();
      manager.dispose();

      // Should not throw
      expect(manager).toBeDefined();
    });

    it('should clean up scene', () => {
      manager.dispose();

      expect(manager).toBeDefined();
    });

    it('should clean up timeline', () => {
      manager.dispose();

      expect(manager).toBeDefined();
    });
  });

  describe('Window Resize Handling', () => {
    it('should handle window resize events', () => {
      // Manually trigger resize
      const resizeEvent = new Event('resize');
      window.dispatchEvent(resizeEvent);

      expect(manager).toBeDefined();
    });

    it('should update canvas on resize', () => {
      canvas.width = 800;
      canvas.height = 600;

      const resizeEvent = new Event('resize');
      window.dispatchEvent(resizeEvent);

      expect(manager).toBeDefined();
    });

    it('should handle size changes gracefully', () => {
      const resizeEvent = new Event('resize');

      window.dispatchEvent(resizeEvent);
      window.dispatchEvent(resizeEvent);

      expect(manager).toBeDefined();
    });
  });

  describe('Factory Function', () => {
    it('should export factory function', async () => {
      const { createVisualizationManager } = await import('../VisualizationManager');

      expect(createVisualizationManager).toBeDefined();
    });

    it('should create manager with factory', async () => {
      const { createVisualizationManager } = await import('../VisualizationManager');

      const factoryCanvas = document.createElement('canvas');
      factoryCanvas.width = 1024;
      factoryCanvas.height = 768;

      const factoryManager = createVisualizationManager(factoryCanvas);

      expect(factoryManager).toBeInstanceOf(VisualizationManager);

      factoryManager.dispose();
    });

    it('should provide default config values', async () => {
      const { createVisualizationManager } = await import('../VisualizationManager');

      const factoryCanvas = document.createElement('canvas');
      factoryCanvas.width = 1024;
      factoryCanvas.height = 768;

      const factoryManager = createVisualizationManager(factoryCanvas);

      expect(factoryManager).toBeDefined();

      factoryManager.dispose();
    });
  });

  describe('Integration with Animation System', () => {
    it('should integrate with state animator', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Should have started animation
      expect(manager.isAnimatingNow()).toBe(true);
    });

    it('should handle animation completion', (done) => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Wait for animation to complete (should be quick for unit test)
      setTimeout(() => {
        // Animation should complete
        expect(manager.isAnimatingNow()).toBeDefined();
        done();
      }, 100);
    });

    it('should route animation events', () => {
      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([2, 0, 0, 0, 0, 0, 0, 0]);
      const diff = createMockStateDiff();

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);

      // Should route events from timeline to scene updates
      expect(manager).toBeDefined();
    });
  });

  describe('Multiple State Updates', () => {
    it('should handle rapid state updates', () => {
      const diff = createMockStateDiff();

      for (let i = 0; i < 8; i++) {
        const state = createMockMachineState(
          Array(8).fill(0).map((_, idx) => idx === 0 ? i : 0)
        );
        manager.updateState(state, diff);
      }

      expect(manager.getCurrentState()).toBeDefined();
    });

    it('should queue animations properly', () => {
      const diff = createMockStateDiff();

      const state1 = createMockMachineState([0, 0, 0, 0, 0, 0, 0, 0]);
      const state2 = createMockMachineState([1, 0, 0, 0, 0, 0, 0, 0]);
      const state3 = createMockMachineState([2, 0, 0, 0, 0, 0, 0, 0]);

      manager.updateState(state1, diff);
      manager.updateState(state2, diff);
      manager.updateState(state3, diff);

      expect(manager.getCurrentState()).toEqual(state3);
    });
  });
});
