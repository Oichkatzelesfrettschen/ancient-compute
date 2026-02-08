/**
 * State Animator: Diff-to-Animation Mapping
 *
 * Orchestrates animation of state changes:
 * - Receives StateDiff from StateReconciler
 * - Creates animation sequence for Timeline
 * - Coordinates ShaftRotation, CarryPropagation, ColumnValueAnimation
 * - Manages staggering and timing across all 264 mechanical elements
 *
 * Animation layers (in order):
 * 1. Column value changes (digit wheels) - 50ms stagger between columns
 * 2. Carry propagation - 30ms stagger between levers
 * 3. Shaft rotations - 20ms stagger between shafts
 */

import type { StateDiff, MachineState } from '../state/types';
import { Timeline, type AnimationEvent } from './Timeline';
import { ShaftRotation } from './ShaftRotation';
import { CarryPropagation } from './CarryPropagation';
import { ColumnValueAnimation } from './ColumnValueAnimation';

/**
 * Animation sequence metadata
 */
export interface AnimationSequence {
  animationId: string;
  startTime: number; // ms
  duration: number; // ms
  layers: {
    columns: AnimationLayerInfo;
    carries: AnimationLayerInfo;
    shafts: AnimationLayerInfo;
  };
  eventCallbacks: {
    onStart?: () => void;
    onProgress?: (event: AnimationEvent) => void;
    onComplete?: () => void;
  };
}

/**
 * Layer animation info
 */
export interface AnimationLayerInfo {
  count: number; // Number of animations in layer
  startTime: number; // First animation start (ms)
  endTime: number; // Last animation end (ms)
  staggerDelayMs: number; // Delay between sequential animations
}

/**
 * State animator orchestrator
 */
export class StateAnimator {
  private timeline: Timeline;
  private shaftRotations: ShaftRotation[] = [];
  private columnAnimations: ColumnValueAnimation[] = [];
  private carryPropagation: CarryPropagation | null = null;
  private currentAnimationSequence: AnimationSequence | null = null;
  private animationIdCounter: number = 0;

  // Configuration
  private readonly columnStaggerMs: number = 50; // Delay between column animations
  private readonly carryStaggerMs: number = 30; // Delay between carry lever animations
  private readonly shaftStaggerMs: number = 20; // Delay between shaft animations
  private readonly columnDurationBaseMs: number = 200; // Base duration for column animations
  private readonly shaftDurationBaseMs: number = 200; // Base duration for shaft animations

  constructor(timeline: Timeline) {
    this.timeline = timeline;
    this.initializeAnimators();
  }

  /**
   * Initialize all sub-animators
   */
  private initializeAnimators(): void {
    // Create shaft rotations for 8 shafts
    this.shaftRotations = [];
    for (let i = 0; i < 8; i++) {
      this.shaftRotations.push(new ShaftRotation(i, 0, 0, this.shaftDurationBaseMs));
    }

    // Create column value animations for 8 columns
    this.columnAnimations = [];
    for (let i = 0; i < 8; i++) {
      this.columnAnimations.push(new ColumnValueAnimation(i));
    }

    // Create carry propagation orchestrator
    this.carryPropagation = new CarryPropagation();
  }

  /**
   * Animate state change based on diff
   */
  animateStateDiff(
    oldState: MachineState,
    newState: MachineState,
    diff: StateDiff,
    onComplete?: () => void
  ): string {
    const animationId = `seq-${++this.animationIdCounter}`;
    const baseStartTime = 0; // Relative to timeline

    // Layer 1: Column value animations (50ms stagger)
    const columnStartTime = baseStartTime;
    const columnAnimationIds = this.createColumnAnimations(
      newState,
      columnStartTime,
      this.columnStaggerMs
    );

    // Layer 2: Carry propagation animations (30ms stagger, offset from columns)
    const carryStartTime = columnStartTime + this.columnStaggerMs * 8; // After all columns start
    const carryAnimationIds = this.createCarryAnimations(newState, carryStartTime);

    // Layer 3: Shaft rotations (20ms stagger, offset from carries)
    const shaftStartTime = carryStartTime + 360; // After carry propagation completes
    const shaftAnimationIds = this.createShaftAnimations(
      newState,
      shaftStartTime,
      this.shaftStaggerMs
    );

    // Calculate total duration
    const maxColumnEnd = columnStartTime + this.columnStaggerMs * 8 + this.columnDurationBaseMs;
    const maxCarryEnd = carryStartTime + 360; // CarryPropagation total duration
    const maxShaftEnd = shaftStartTime + this.shaftStaggerMs * 8 + this.shaftDurationBaseMs;
    const totalDuration = Math.max(maxColumnEnd, maxCarryEnd, maxShaftEnd);

    // Create sequence metadata
    this.currentAnimationSequence = {
      animationId,
      startTime: baseStartTime,
      duration: totalDuration,
      layers: {
        columns: {
          count: columnAnimationIds.length,
          startTime: columnStartTime,
          endTime: maxColumnEnd,
          staggerDelayMs: this.columnStaggerMs
        },
        carries: {
          count: carryAnimationIds.length,
          startTime: carryStartTime,
          endTime: maxCarryEnd,
          staggerDelayMs: this.carryStaggerMs
        },
        shafts: {
          count: shaftAnimationIds.length,
          startTime: shaftStartTime,
          endTime: maxShaftEnd,
          staggerDelayMs: this.shaftStaggerMs
        }
      },
      eventCallbacks: {
        onComplete
      }
    };

    // Register completion callback if all animations complete
    if (onComplete) {
      // Add callback to last animation
      const allAnimationIds = [...columnAnimationIds, ...carryAnimationIds, ...shaftAnimationIds];
      if (allAnimationIds.length > 0) {
        const lastAnimationId = allAnimationIds[allAnimationIds.length - 1];
        // Note: Timeline doesn't expose a way to add completion callback to existing animation
        // This would need to be handled via onEvent listening or callbacks
      }
    }

    return animationId;
  }

  /**
   * Create column value animations
   */
  private createColumnAnimations(
    newState: MachineState,
    startTime: number,
    staggerMs: number
  ): string[] {
    const animationIds = [];

    for (let columnIndex = 0; columnIndex < 8; columnIndex++) {
      const columnStartTime = startTime + columnIndex * staggerMs;
      const columnState = newState.columns[columnIndex];
      const values = this.extractColumnValues(columnState);

      // Create animation steps for column
      const columnAnimationSteps = this.createColumnValueAnimationSteps(
        columnIndex,
        values,
        columnStartTime
      );

      // Add each step to timeline
      for (const step of columnAnimationSteps) {
        const stepId = this.timeline.addAnimation(step);
        animationIds.push(stepId);
      }
    }

    return animationIds;
  }

  /**
   * Create carry propagation animations
   */
  private createCarryAnimations(newState: MachineState, startTime: number): string[] {
    // Create carry propagation sequence
    const carrySequenceSteps = this.createCarryPropagationSteps(newState, startTime);

    const animationIds = [];
    for (const step of carrySequenceSteps) {
      const stepId = this.timeline.addAnimation(step);
      animationIds.push(stepId);
    }

    return animationIds;
  }

  /**
   * Create shaft rotation animations
   */
  private createShaftAnimations(
    newState: MachineState,
    startTime: number,
    staggerMs: number
  ): string[] {
    const animationIds = [];

    for (let shaftIndex = 0; shaftIndex < 8; shaftIndex++) {
      const shaftStartTime = startTime + shaftIndex * staggerMs;
      const shaftState = newState.shafts[shaftIndex];
      const targetAngle = shaftState.rotationRadians;

      // Create animation step for shaft
      const shaftAnimationStep = this.createShaftRotationStep(
        shaftIndex,
        targetAngle,
        shaftStartTime
      );

      const stepId = this.timeline.addAnimation(shaftAnimationStep);
      animationIds.push(stepId);
    }

    return animationIds;
  }

  /**
   * Extract column values (0-9 for each wheel)
   */
  private extractColumnValues(columnState: any): number[] {
    const values = [];
    // Assuming columnState has digitalValue or wheels property
    if (columnState.digitalValue !== undefined) {
      // Convert decimal to individual digits
      const valueStr = String(columnState.digitalValue).padStart(31, '0');
      for (let i = 0; i < 31; i++) {
        values.push(parseInt(valueStr[i], 10));
      }
    } else {
      // Default: all zeros
      for (let i = 0; i < 31; i++) {
        values.push(0);
      }
    }
    return values;
  }

  /**
   * Create animation steps for column value change
   */
  private createColumnValueAnimationSteps(
    columnIndex: number,
    values: number[],
    startTime: number
  ): Array<{
    targetObject: string;
    startValue: number;
    endValue: number;
    duration: number;
    startTime: number;
    easing: 'linear' | 'easeInOutQuad' | 'easeInOutCubic' | 'easeInOutExpo' | 'easeOutBounce';
  }> {
    const steps = [];
    const wheelStaggerMs = 5; // 5ms between wheels in same column

    for (let wheelIndex = 0; wheelIndex < values.length; wheelIndex++) {
      const wheelValue = values[wheelIndex];
      const wheelStartTime = startTime + wheelIndex * wheelStaggerMs;
      const degreesPerValue = 36; // 360° / 10 values
      const endRotationRadians = (wheelValue * degreesPerValue * Math.PI) / 180;

      steps.push({
        targetObject: `column${columnIndex}wheel${wheelIndex}`,
        startValue: 0,
        endValue: endRotationRadians,
        duration: this.columnDurationBaseMs,
        startTime: wheelStartTime,
        easing: 'easeInOutCubic'
      });
    }

    return steps;
  }

  /**
   * Create animation steps for carry propagation
   */
  private createCarryPropagationSteps(
    newState: MachineState,
    startTime: number
  ): Array<{
    targetObject: string;
    startValue: number;
    endValue: number;
    duration: number;
    startTime: number;
    easing: 'linear' | 'easeInOutQuad' | 'easeInOutCubic' | 'easeInOutExpo' | 'easeOutBounce';
  }> {
    const steps = [];

    // Create staggered animations for 8 carry levers
    for (let leverIndex = 0; leverIndex < 8; leverIndex++) {
      const leverStartTime = startTime + leverIndex * this.carryStaggerMs;
      const leverState = newState.carryLevers[leverIndex];
      const isEngaged = leverState.isEngaged;

      steps.push({
        targetObject: `carryLever${leverIndex}`,
        startValue: 0,
        endValue: isEngaged ? Math.PI / 2 : 0, // 0° to 90°
        duration: 160, // 160ms engagement
        startTime: leverStartTime,
        easing: 'easeInOutCubic'
      });
    }

    return steps;
  }

  /**
   * Create animation step for shaft rotation
   */
  private createShaftRotationStep(
    shaftIndex: number,
    targetAngle: number,
    startTime: number
  ): {
    targetObject: string;
    startValue: number;
    endValue: number;
    duration: number;
    startTime: number;
    easing: 'linear' | 'easeInOutQuad' | 'easeInOutCubic' | 'easeInOutExpo' | 'easeOutBounce';
  } {
    return {
      targetObject: `shaft${shaftIndex}`,
      startValue: 0,
      endValue: targetAngle,
      duration: this.shaftDurationBaseMs,
      startTime,
      easing: 'easeInOutCubic'
    };
  }

  /**
   * Get current animation sequence
   */
  getCurrentSequence(): AnimationSequence | null {
    return this.currentAnimationSequence ? { ...this.currentAnimationSequence } : null;
  }

  /**
   * Get shaft rotation animator
   */
  getShaftRotation(shaftIndex: number): ShaftRotation | null {
    if (shaftIndex < 0 || shaftIndex >= this.shaftRotations.length) {
      return null;
    }
    return this.shaftRotations[shaftIndex];
  }

  /**
   * Get column animation animator
   */
  getColumnAnimation(columnIndex: number): ColumnValueAnimation | null {
    if (columnIndex < 0 || columnIndex >= this.columnAnimations.length) {
      return null;
    }
    return this.columnAnimations[columnIndex];
  }

  /**
   * Get carry propagation animator
   */
  getCarryPropagation(): CarryPropagation | null {
    return this.carryPropagation;
  }

  /**
   * Get timeline
   */
  getTimeline(): Timeline {
    return this.timeline;
  }

  /**
   * Get debug info
   */
  getDebugInfo(): string {
    const sequence = this.currentAnimationSequence;

    if (!sequence) {
      return 'State Animator: No active animation sequence';
    }

    return `
State Animator:
- Animation ID: ${sequence.animationId}
- Total Duration: ${sequence.duration.toFixed(0)}ms
- Columns: ${sequence.layers.columns.count} animations (${sequence.layers.columns.startTime}-${sequence.layers.columns.endTime.toFixed(0)}ms)
- Carries: ${sequence.layers.carries.count} animations (${sequence.layers.carries.startTime}-${sequence.layers.carries.endTime.toFixed(0)}ms)
- Shafts: ${sequence.layers.shafts.count} animations (${sequence.layers.shafts.startTime}-${sequence.layers.shafts.endTime.toFixed(0)}ms)
- Timeline State: ${this.timeline.getState()}
    `.trim();
  }
}

/**
 * Factory function to create StateAnimator with shared Timeline
 */
export function createStateAnimator(): { animator: StateAnimator; timeline: Timeline } {
  const timeline = new Timeline();
  const animator = new StateAnimator(timeline);
  return { animator, timeline };
}
