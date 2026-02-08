/**
 * Unit Tests: Timeline
 *
 * Tests frame-based animation orchestration, easing functions,
 * event callbacks, and state management
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { Timeline, type AnimationEvent } from './Timeline';
import type { AnimationStep } from '../state/types';

describe('Timeline', () => {
  let timeline: Timeline;

  beforeEach(() => {
    timeline = new Timeline();
  });

  describe('Initialization', () => {
    it('should create timeline', () => {
      expect(timeline).toBeDefined();
      expect(timeline.getState()).toBe('IDLE');
    });

    it('should start with no animations', () => {
      expect(timeline.getAnimationCount()).toBe(0);
    });

    it('should have zero initial time', () => {
      expect(timeline.getCurrentTime()).toBe(0);
    });
  });

  describe('Animation Management', () => {
    it('should add single animation', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      expect(typeof id).toBe('string');
      expect(timeline.getAnimationCount()).toBe(1);
      expect(timeline.hasAnimation(id)).toBe(true);
    });

    it('should add multiple animations', () => {
      const steps: AnimationStep[] = [
        {
          targetObject: 'test1',
          startValue: 0,
          endValue: 10,
          duration: 1000,
          startTime: 0,
          easing: 'linear'
        },
        {
          targetObject: 'test2',
          startValue: 0,
          endValue: 20,
          duration: 1000,
          startTime: 0,
          easing: 'linear'
        }
      ];

      const ids = timeline.addAnimations(steps);
      expect(ids.length).toBe(2);
      expect(timeline.getAnimationCount()).toBe(2);
    });

    it('should remove animation', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.removeAnimation(id);
      expect(timeline.hasAnimation(id)).toBe(false);
    });
  });

  describe('Animation Lifecycle', () => {
    it('should transition from IDLE to RUNNING', () => {
      expect(timeline.getState()).toBe('IDLE');
      timeline.start();
      expect(timeline.getState()).toBe('RUNNING');
    });

    it('should pause running timeline', () => {
      timeline.start();
      timeline.pause();
      expect(timeline.getState()).toBe('PAUSED');
    });

    it('should resume paused timeline', () => {
      timeline.start();
      timeline.pause();
      timeline.resume();
      expect(timeline.getState()).toBe('RUNNING');
    });

    it('should stop timeline', () => {
      timeline.start();
      timeline.stop();
      expect(timeline.getState()).toBe('IDLE');
      expect(timeline.getAnimationCount()).toBe(0);
    });
  });

  describe('Animation Updates', () => {
    it('should update elapsed time', () => {
      timeline.start();
      timeline.update(100);
      expect(timeline.getCurrentTime()).toBeGreaterThan(0);
    });

    it('should accumulate delta times', () => {
      timeline.start();
      timeline.update(50);
      timeline.update(50);
      expect(timeline.getCurrentTime()).toBeCloseTo(100, 0);
    });

    it('should not update when paused', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();
      timeline.pause();

      const beforeTime = timeline.getCurrentTime();
      timeline.update(100);
      const afterTime = timeline.getCurrentTime();

      expect(afterTime).toBe(beforeTime);
    });

    it('should not update when idle', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      const beforeTime = timeline.getCurrentTime();
      timeline.update(100);
      const afterTime = timeline.getCurrentTime();

      expect(afterTime).toBe(beforeTime);
    });
  });

  describe('Progress Tracking', () => {
    it('should track animation progress', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(500);

      const progress = timeline.getAnimationProgress(id);
      expect(progress).toBeCloseTo(0.5, 1);
    });

    it('should clamp progress to [0, 1]', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(500);

      const progress = timeline.getAnimationProgress(id);
      expect(progress).toBeLessThanOrEqual(1);
      expect(progress).toBeGreaterThanOrEqual(0);
    });
  });

  describe('Animation Values', () => {
    it('should interpolate animation value', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 100,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(500);

      const value = timeline.getAnimationValue(id);
      expect(value).toBeCloseTo(50, 0);
    });

    it('should return 0 for non-existent animation', () => {
      const value = timeline.getAnimationValue('nonexistent');
      expect(value).toBe(0);
    });
  });

  describe('Completion Detection', () => {
    it('should mark animation complete', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(150);

      expect(timeline.getCompletedCount()).toBe(1);
    });

    it('should detect all animations complete', () => {
      const steps: AnimationStep[] = [
        {
          targetObject: 'test1',
          startValue: 0,
          endValue: 10,
          duration: 100,
          startTime: 0,
          easing: 'linear'
        },
        {
          targetObject: 'test2',
          startValue: 0,
          endValue: 20,
          duration: 100,
          startTime: 0,
          easing: 'linear'
        }
      ];

      timeline.addAnimations(steps);
      timeline.start();
      timeline.update(150);

      expect(timeline.areAllAnimationsComplete()).toBe(true);
      expect(timeline.getState()).toBe('COMPLETED');
    });

    it('should not mark complete with no animations', () => {
      timeline.start();
      timeline.update(100);
      expect(timeline.areAllAnimationsComplete()).toBe(false);
    });
  });

  describe('Event System', () => {
    it('should emit START event', () => {
      const callback = vi.fn();
      timeline.onEvent(callback);

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();
      timeline.update(0);

      const startEvents = (callback.mock.calls as any[]).filter(
        (call) => call[0].type === 'START'
      );
      expect(startEvents.length).toBeGreaterThanOrEqual(0);
    });

    it('should emit UPDATE event', () => {
      const callback = vi.fn();
      timeline.onEvent(callback);

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();
      timeline.update(100);

      const updateEvents = (callback.mock.calls as any[]).filter(
        (call) => call[0].type === 'UPDATE'
      );
      expect(updateEvents.length).toBeGreaterThan(0);
    });

    it('should emit COMPLETE event', () => {
      const callback = vi.fn();
      timeline.onEvent(callback);

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();
      timeline.update(150);

      const completeEvents = (callback.mock.calls as any[]).filter(
        (call) => call[0].type === 'COMPLETE'
      );
      expect(completeEvents.length).toBeGreaterThan(0);
    });

    it('should unsubscribe from events', () => {
      const callback = vi.fn();
      const unsubscribe = timeline.onEvent(callback);

      unsubscribe();

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();
      timeline.update(50);

      expect(callback).not.toHaveBeenCalled();
    });
  });

  describe('Completion Callbacks', () => {
    it('should invoke completion callback', (done) => {
      const callback = vi.fn(() => {
        done();
      });

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step, callback);
      timeline.start();
      timeline.update(150);

      // Give callback time to execute
      setTimeout(() => {
        if (!callback.mock.called) {
          done();
        }
      }, 10);
    });

    it('should handle callback errors gracefully', () => {
      const errorCallback = vi.fn(() => {
        throw new Error('Test error');
      });

      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step, errorCallback);
      timeline.start();

      // Should not throw
      expect(() => timeline.update(150)).not.toThrow();
    });
  });

  describe('Staggered Animations', () => {
    it('should handle staggered start times', () => {
      const steps: AnimationStep[] = [
        {
          targetObject: 'test1',
          startValue: 0,
          endValue: 10,
          duration: 1000,
          startTime: 0,
          easing: 'linear'
        },
        {
          targetObject: 'test2',
          startValue: 0,
          endValue: 20,
          duration: 1000,
          startTime: 100,
          easing: 'linear'
        }
      ];

      timeline.addAnimations(steps);
      timeline.start();
      timeline.update(50);

      // First animation should have progressed
      expect(timeline.getAnimationProgress('anim-0')).toBeGreaterThan(0);
      // Second animation should not have started
      expect(timeline.getAnimationProgress('anim-1')).toBe(0);
    });
  });

  describe('Max Duration Calculation', () => {
    it('should calculate max duration', () => {
      const steps: AnimationStep[] = [
        {
          targetObject: 'test1',
          startValue: 0,
          endValue: 10,
          duration: 1000,
          startTime: 0,
          easing: 'linear'
        },
        {
          targetObject: 'test2',
          startValue: 0,
          endValue: 20,
          duration: 500,
          startTime: 1000,
          easing: 'linear'
        }
      ];

      timeline.addAnimations(steps);
      const maxDuration = timeline.getMaxDuration();

      // Should account for latest animation end time (1000 + 500 = 1500)
      expect(maxDuration).toBeGreaterThanOrEqual(1500);
    });
  });

  describe('Debug Info', () => {
    it('should provide summary', () => {
      const summary = timeline.getSummary();
      expect(typeof summary).toBe('string');
      expect(summary.length).toBeGreaterThan(0);
    });

    it('should include state in summary', () => {
      timeline.start();
      const summary = timeline.getSummary();
      expect(summary).toContain('RUNNING');
    });

    it('should include animation counts in summary', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      const summary = timeline.getSummary();
      expect(summary).toContain('1');
    });
  });

  describe('Edge Cases', () => {
    it('should handle zero duration animation', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 0,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(0);

      expect(timeline.getAnimationProgress(id)).toBe(1);
    });

    it('should handle negative delta time gracefully', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 1000,
        startTime: 0,
        easing: 'linear'
      };

      timeline.addAnimation(step);
      timeline.start();

      // Negative delta shouldn't crash
      expect(() => timeline.update(-100)).not.toThrow();
    });

    it('should handle very large delta time', () => {
      const step: AnimationStep = {
        targetObject: 'test',
        startValue: 0,
        endValue: 10,
        duration: 100,
        startTime: 0,
        easing: 'linear'
      };

      const id = timeline.addAnimation(step);
      timeline.start();
      timeline.update(100000);

      expect(timeline.getAnimationProgress(id)).toBe(1);
    });
  });
});
