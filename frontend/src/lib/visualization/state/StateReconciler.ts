/**
 * State Reconciler: Intelligent State Diffing and Conflict Resolution
 *
 * Handles:
 * - Computing diffs between states
 * - Conflict detection and resolution
 * - Interpolation path generation for smooth animation
 * - State validation and consistency checking
 */

import type {
  MachineState,
  StateDiff,
  ColumnChange,
  CarryChange,
  ShaftChange,
  StateConflict,
  InterpolationPath,
  EasingFunction,
  ReconciliationResult,
  AnimationStep,
  ColumnState,
  CarryLeverState,
  ShaftState
} from './types';

/**
 * State reconciliation configuration
 */
export interface ReconciliationConfig {
  conflictStrategy: 'ACCEPT_SERVER' | 'ACCEPT_CLIENT' | 'MERGE';
  interpolationDuration: number; // milliseconds
  enableValidation: boolean;
}

/**
 * Default reconciliation config
 */
const DEFAULT_CONFIG: ReconciliationConfig = {
  conflictStrategy: 'ACCEPT_SERVER',
  interpolationDuration: 200,
  enableValidation: true
};

/**
 * State reconciler
 */
export class StateReconciler {
  private config: ReconciliationConfig;

  constructor(config?: Partial<ReconciliationConfig>) {
    this.config = { ...DEFAULT_CONFIG, ...config };
  }

  /**
   * Compute diff between two states
   */
  computeDiff(oldState: MachineState, newState: MachineState): StateDiff {
    const diff: StateDiff = {
      phase: undefined,
      phaseChanged: oldState.phase !== newState.phase,
      columnChanges: [],
      carryChanges: [],
      shaftChanges: [],
      timestamp: newState.timestamp,
      stateVersionDelta: newState.stateVersion - oldState.stateVersion
    };

    // Track phase change
    if (diff.phaseChanged) {
      diff.phase = newState.phase;
    }

    // Compute column changes
    for (let i = 0; i < 8; i++) {
      const oldCol = oldState.columnStates[i];
      const newCol = newState.columnStates[i];

      if (
        oldCol.value !== newCol.value ||
        oldCol.wheelRotation !== newCol.wheelRotation
      ) {
        diff.columnChanges.push({
          columnIndex: i,
          oldValue: oldCol.value,
          newValue: newCol.value,
          oldRotation: oldCol.wheelRotation,
          newRotation: newCol.wheelRotation,
          magnitude: Math.abs(newCol.value - oldCol.value)
        });
      }
    }

    // Compute carry changes
    for (let i = 0; i < 8; i++) {
      const oldCarry = oldState.carryLevers[i];
      const newCarry = newState.carryLevers[i];

      if (
        oldCarry.isEngaged !== newCarry.isEngaged ||
        oldCarry.hasCarryToPropagate !== newCarry.hasCarryToPropagate
      ) {
        diff.carryChanges.push({
          columnIndex: i,
          wasEngaged: oldCarry.isEngaged,
          isEngaged: newCarry.isEngaged,
          hadCarry: oldCarry.hasCarryToPropagate,
          hasCarry: newCarry.hasCarryToPropagate
        });
      }
    }

    // Compute shaft changes
    for (let i = 0; i < 8; i++) {
      const oldShaft = oldState.shafts[i];
      const newShaft = newState.shafts[i];

      if (oldShaft.rotation !== newShaft.rotation) {
        diff.shaftChanges.push({
          shaftIndex: i,
          oldRotation: oldShaft.rotation,
          newRotation: newShaft.rotation,
          rotationDelta: this.normalizeRotationDelta(
            newShaft.rotation - oldShaft.rotation
          )
        });
      }
    }

    return diff;
  }

  /**
   * Reconcile conflicting states
   */
  reconcile(
    clientState: MachineState,
    serverState: MachineState
  ): ReconciliationResult {
    const conflicts: StateConflict[] = [];
    let resolvedState: MachineState = { ...serverState };

    // Validate states if enabled
    if (this.config.enableValidation) {
      this.validateState(serverState);
    }

    // Detect and resolve conflicts
    if (clientState.stateVersion !== serverState.stateVersion) {
      // Version mismatch - potential conflict
      for (let i = 0; i < 8; i++) {
        const clientCol = clientState.columnStates[i];
        const serverCol = serverState.columnStates[i];

        if (clientCol.value !== serverCol.value) {
          conflicts.push({
            property: `column-${i}.value`,
            clientValue: clientCol.value,
            serverValue: serverCol.value,
            resolution: this.config.conflictStrategy
          });

          // Apply resolution
          if (this.config.conflictStrategy === 'ACCEPT_CLIENT') {
            resolvedState.columnStates[i].value = clientCol.value;
            resolvedState.columnStates[i].wheelRotation = clientCol.wheelRotation;
          }
          // else ACCEPT_SERVER is already applied (serverState is base)
        }
      }
    }

    // Generate interpolation paths and animation sequence
    const oldDiff = this.computeDiff(clientState, serverState);
    const interpolationPaths = this.generateInterpolationPaths(oldDiff);
    const animationSequence = this.generateAnimationSequence(
      oldDiff,
      interpolationPaths
    );

    // Calculate time needed for all animations
    const timeToAnimate = Math.max(
      ...animationSequence.map((step) => step.startTime + step.duration),
      0
    );

    return {
      resolvedState,
      conflicts,
      interpolationPaths,
      animationSequence,
      timeToAnimate
    };
  }

  /**
   * Generate interpolation paths from diff
   */
  private generateInterpolationPaths(diff: StateDiff): Map<string, InterpolationPath> {
    const paths = new Map<string, InterpolationPath>();
    const baseDuration = this.config.interpolationDuration;

    // Column animations
    for (const change of diff.columnChanges) {
      const key = `column-${change.columnIndex}.value`;
      const magnitude = change.magnitude;

      // Duration scales with magnitude: 1 digit = base duration, 9 digits = 2x duration
      const scaledDuration = baseDuration * (0.5 + magnitude / 9);

      paths.set(key, {
        startValue: change.oldValue,
        endValue: change.newValue,
        startTime: 0,
        duration: scaledDuration,
        easingFunction: 'easeInOutQuad'
      });

      // Wheel rotation animation
      const rotationKey = `column-${change.columnIndex}.rotation`;
      paths.set(rotationKey, {
        startValue: change.oldRotation,
        endValue: change.newRotation,
        startTime: 0,
        duration: scaledDuration,
        easingFunction: 'easeInOutQuad'
      });
    }

    // Carry lever animations
    for (let i = 0; i < diff.carryChanges.length; i++) {
      const change = diff.carryChanges[i];
      if (change.wasEngaged !== change.isEngaged) {
        const key = `carry-${change.columnIndex}.engaged`;
        paths.set(key, {
          startValue: change.wasEngaged ? 1 : 0,
          endValue: change.isEngaged ? 1 : 0,
          startTime: 0,
          duration: 150,
          easingFunction: 'easeInOutCubic'
        });
      }
    }

    // Shaft animations
    for (const change of diff.shaftChanges) {
      const key = `shaft-${change.shaftIndex}.rotation`;
      paths.set(key, {
        startValue: change.oldRotation,
        endValue: change.newRotation,
        startTime: 0,
        duration: baseDuration * 1.5,
        easingFunction: 'easeInOutExpo'
      });
    }

    return paths;
  }

  /**
   * Generate animation sequence from diff and paths
   */
  private generateAnimationSequence(
    diff: StateDiff,
    paths: Map<string, InterpolationPath>
  ): AnimationStep[] {
    const sequence: AnimationStep[] = [];
    let currentStartTime = 0;

    // Stagger column animations
    for (const change of diff.columnChanges) {
      const key = `column-${change.columnIndex}.value`;
      const path = paths.get(key);
      if (path) {
        sequence.push({
          targetObject: `column-${change.columnIndex}`,
          propertyName: 'value',
          startValue: path.startValue,
          endValue: path.endValue,
          startTime: currentStartTime,
          duration: path.duration,
          easing: path.easingFunction
        });

        // Stagger: delay by 50ms per column
        currentStartTime += 50;
      }
    }

    // Add carry animations (parallel to column animations)
    let carryStartTime = 0;
    for (const change of diff.carryChanges) {
      if (change.wasEngaged !== change.isEngaged) {
        const key = `carry-${change.columnIndex}.engaged`;
        const path = paths.get(key);
        if (path) {
          sequence.push({
            targetObject: `carry-${change.columnIndex}`,
            propertyName: 'engaged',
            startValue: path.startValue,
            endValue: path.endValue,
            startTime: carryStartTime,
            duration: path.duration,
            easing: path.easingFunction
          });
          carryStartTime += 30;
        }
      }
    }

    // Add shaft animations (parallel)
    let shaftStartTime = 0;
    for (const change of diff.shaftChanges) {
      const key = `shaft-${change.shaftIndex}.rotation`;
      const path = paths.get(key);
      if (path) {
        sequence.push({
          targetObject: `shaft-${change.shaftIndex}`,
          propertyName: 'rotation',
          startValue: path.startValue,
          endValue: path.endValue,
          startTime: shaftStartTime,
          duration: path.duration,
          easing: path.easingFunction
        });
        shaftStartTime += 20;
      }
    }

    return sequence;
  }

  /**
   * Validate state consistency
   */
  private validateState(state: MachineState): void {
    // Column values should be 0-9
    for (const col of state.columnStates) {
      if (col.value < 0 || col.value > 9) {
        console.warn(`Invalid column value: ${col.value}`);
      }
    }

    // Rotations should be 0-2π
    for (const col of state.columnStates) {
      if (col.wheelRotation < 0 || col.wheelRotation > Math.PI * 2) {
        console.warn(`Invalid wheel rotation: ${col.wheelRotation}`);
      }
    }

    // Phase should be valid
    const validPhases = [
      'IDLE',
      'INPUT',
      'ADDITION',
      'CARRY',
      'OUTPUT',
      'ADVANCE',
      'RESET',
      'PAUSE'
    ];
    if (!validPhases.includes(state.phase)) {
      console.warn(`Invalid phase: ${state.phase}`);
    }
  }

  /**
   * Normalize rotation delta to shortest path
   */
  private normalizeRotationDelta(delta: number): number {
    // Normalize to [-π, π]
    let normalized = delta % (Math.PI * 2);
    if (normalized > Math.PI) {
      normalized -= Math.PI * 2;
    } else if (normalized < -Math.PI) {
      normalized += Math.PI * 2;
    }
    return normalized;
  }

  /**
   * Get easing function implementation
   */
  static getEasingFunction(easing: EasingFunction): (t: number) => number {
    switch (easing) {
      case 'linear':
        return (t) => t;
      case 'easeInOutQuad':
        return (t) => (t < 0.5 ? 2 * t * t : 1 - Math.pow(-2 * t + 2, 2) / 2);
      case 'easeInOutCubic':
        return (t) =>
          t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
      case 'easeInOutExpo':
        return (t) =>
          t === 0
            ? 0
            : t === 1
              ? 1
              : t < 0.5
                ? Math.pow(2, 20 * t - 10) / 2
                : (2 - Math.pow(2, -20 * t + 10)) / 2;
      case 'easeOutBounce':
        return (t) => {
          const n1 = 7.5625;
          const d1 = 2.75;
          if (t < 1 / d1) {
            return n1 * t * t;
          } else if (t < 2 / d1) {
            return n1 * (t -= 1.5 / d1) * t + 0.75;
          } else if (t < 2.5 / d1) {
            return n1 * (t -= 2.25 / d1) * t + 0.9375;
          } else {
            return n1 * (t -= 2.625 / d1) * t + 0.984375;
          }
        };
      default:
        return (t) => t;
    }
  }

  /**
   * Interpolate value at time t (0-1)
   */
  static interpolate(
    startValue: number,
    endValue: number,
    t: number,
    easing: EasingFunction
  ): number {
    const easingFn = this.getEasingFunction(easing);
    const easedT = easingFn(Math.max(0, Math.min(1, t)));
    return startValue + (endValue - startValue) * easedT;
  }
}

/**
 * Global reconciler instance
 */
export const globalReconciler = new StateReconciler();
