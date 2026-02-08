/**
 * Timeline: Global Animation Orchestrator
 *
 * Manages:
 * - Animation scheduling and execution
 * - Frame-based animation updates
 * - Easing function evaluation
 * - Callback invocation on completion
 * - Animation queue management
 * - State tracking and pause/resume
 */

import { StateReconciler } from '../state/StateReconciler';
import type { AnimationStep, EasingFunction } from '../state/types';

/**
 * Animation state
 */
export type AnimationState = 'IDLE' | 'RUNNING' | 'PAUSED' | 'COMPLETED';

/**
 * Animation event
 */
export interface AnimationEvent {
  type: 'START' | 'UPDATE' | 'COMPLETE' | 'PAUSE' | 'RESUME';
  animationId: string;
  target: string;
  progress: number; // 0-1
  value: number; // interpolated value
}

/**
 * Active animation tracking
 */
interface ActiveAnimation {
  step: AnimationStep;
  startTime: number;
  animationId: string;
  isCompleted: boolean;
  lastValue: number;
}

/**
 * Timeline implementation
 */
export class Timeline {
  private animations: Map<string, ActiveAnimation> = new Map();
  private timelineStart: number = 0;
  private currentTime: number = 0;
  private state: AnimationState = 'IDLE';
  private eventCallbacks: ((event: AnimationEvent) => void)[] = [];
  private completionCallbacks: Map<string, () => void> = new Map();

  /**
   * Add animation step to timeline
   */
  addAnimation(step: AnimationStep, onComplete?: () => void): string {
    const animationId = `anim-${Math.random().toString(36).substr(2, 9)}`;

    const active: ActiveAnimation = {
      step,
      startTime: this.currentTime + step.startTime,
      animationId,
      isCompleted: false,
      lastValue: step.startValue
    };

    this.animations.set(animationId, active);

    if (onComplete) {
      this.completionCallbacks.set(animationId, onComplete);
    }

    return animationId;
  }

  /**
   * Add multiple animations
   */
  addAnimations(steps: AnimationStep[]): string[] {
    return steps.map((step) => this.addAnimation(step));
  }

  /**
   * Start timeline
   */
  start(): void {
    if (this.state === 'IDLE') {
      this.timelineStart = Date.now();
      this.currentTime = 0;
      this.state = 'RUNNING';
    }
  }

  /**
   * Update timeline (call once per frame)
   */
  update(deltaTime: number = 16.67): void {
    if (this.state !== 'RUNNING') {
      return;
    }

    this.currentTime += deltaTime;

    const completedAnimations: string[] = [];

    for (const [animationId, active] of this.animations.entries()) {
      if (active.isCompleted) {
        continue;
      }

      const step = active.step;
      const elapsedSinceStart = this.currentTime - active.startTime;

      // Check if animation should have started
      if (elapsedSinceStart < 0) {
        continue;
      }

      // Calculate progress (0-1)
      const progress = Math.min(1, elapsedSinceStart / step.duration);

      // Get easing function and evaluate
      const easingFn = StateReconciler.getEasingFunction(step.easing);
      const easedProgress = easingFn(progress);

      // Interpolate value
      const value = StateReconciler.interpolate(
        step.startValue,
        step.endValue,
        easedProgress,
        step.easing
      );

      // Emit update event
      this.emitEvent({
        type: 'UPDATE',
        animationId,
        target: step.targetObject,
        progress,
        value
      });

      active.lastValue = value;

      // Check completion
      if (progress >= 1) {
        active.isCompleted = true;
        completedAnimations.push(animationId);

        // Emit completion event
        this.emitEvent({
          type: 'COMPLETE',
          animationId,
          target: step.targetObject,
          progress: 1,
          value: step.endValue
        });

        // Invoke callback
        const callback = this.completionCallbacks.get(animationId);
        if (callback) {
          try {
            callback();
          } catch (error) {
            console.error('[Timeline] Completion callback error:', error);
          }
        }
      }
    }

    // Check if all animations completed
    if (this.areAllAnimationsComplete()) {
      this.state = 'COMPLETED';
    }
  }

  /**
   * Pause timeline
   */
  pause(): void {
    if (this.state === 'RUNNING') {
      this.state = 'PAUSED';

      for (const [animationId] of this.animations) {
        this.emitEvent({
          type: 'PAUSE',
          animationId,
          target: this.animations.get(animationId)!.step.targetObject,
          progress: this.getAnimationProgress(animationId),
          value: this.animations.get(animationId)!.lastValue
        });
      }
    }
  }

  /**
   * Resume timeline
   */
  resume(): void {
    if (this.state === 'PAUSED') {
      this.state = 'RUNNING';

      for (const [animationId] of this.animations) {
        this.emitEvent({
          type: 'RESUME',
          animationId,
          target: this.animations.get(animationId)!.step.targetObject,
          progress: this.getAnimationProgress(animationId),
          value: this.animations.get(animationId)!.lastValue
        });
      }
    }
  }

  /**
   * Stop timeline and clear animations
   */
  stop(): void {
    this.animations.clear();
    this.completionCallbacks.clear();
    this.state = 'IDLE';
    this.currentTime = 0;
  }

  /**
   * Check if animation exists and is running
   */
  hasAnimation(animationId: string): boolean {
    const anim = this.animations.get(animationId);
    return !!anim && !anim.isCompleted;
  }

  /**
   * Get animation progress (0-1)
   */
  getAnimationProgress(animationId: string): number {
    const anim = this.animations.get(animationId);
    if (!anim) return 0;

    const elapsedSinceStart = this.currentTime - anim.startTime;
    return Math.max(0, Math.min(1, elapsedSinceStart / anim.step.duration));
  }

  /**
   * Get current value of animation
   */
  getAnimationValue(animationId: string): number {
    const anim = this.animations.get(animationId);
    return anim ? anim.lastValue : 0;
  }

  /**
   * Remove animation
   */
  removeAnimation(animationId: string): void {
    this.animations.delete(animationId);
    this.completionCallbacks.delete(animationId);
  }

  /**
   * Get timeline state
   */
  getState(): AnimationState {
    return this.state;
  }

  /**
   * Get total animation count
   */
  getAnimationCount(): number {
    return this.animations.size;
  }

  /**
   * Get completed animation count
   */
  getCompletedCount(): number {
    let completed = 0;
    for (const anim of this.animations.values()) {
      if (anim.isCompleted) {
        completed += 1;
      }
    }
    return completed;
  }

  /**
   * Get current time
   */
  getCurrentTime(): number {
    return this.currentTime;
  }

  /**
   * Check if all animations are complete
   */
  areAllAnimationsComplete(): boolean {
    if (this.animations.size === 0) {
      return false;
    }

    for (const anim of this.animations.values()) {
      if (!anim.isCompleted) {
        return false;
      }
    }
    return true;
  }

  /**
   * Register event callback
   */
  onEvent(callback: (event: AnimationEvent) => void): () => void {
    this.eventCallbacks.push(callback);

    // Return unsubscribe function
    return () => {
      const index = this.eventCallbacks.indexOf(callback);
      if (index >= 0) {
        this.eventCallbacks.splice(index, 1);
      }
    };
  }

  /**
   * Get max animation end time
   */
  getMaxDuration(): number {
    let maxEnd = 0;

    for (const anim of this.animations.values()) {
      const end = anim.startTime + anim.step.duration;
      if (end > maxEnd) {
        maxEnd = end;
      }
    }

    return Math.max(0, maxEnd - this.timelineStart);
  }

  /**
   * Emit event to all listeners
   */
  private emitEvent(event: AnimationEvent): void {
    for (const callback of this.eventCallbacks) {
      try {
        callback(event);
      } catch (error) {
        console.error('[Timeline] Event callback error:', error);
      }
    }
  }

  /**
   * Get summary for debugging
   */
  getSummary(): string {
    return `
Timeline Summary:
- State: ${this.state}
- Current Time: ${this.currentTime.toFixed(2)}ms
- Total Animations: ${this.getAnimationCount()}
- Completed: ${this.getCompletedCount()}
- Max Duration: ${this.getMaxDuration().toFixed(2)}ms
    `.trim();
  }
}

/**
 * Global timeline instance
 */
export const globalTimeline = new Timeline();
