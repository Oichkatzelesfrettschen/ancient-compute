/**
 * Column Value Animation: Digit Wheel Rotations
 *
 * Manages animations for 248 digit wheels (8 columns × 31 wheels per column)
 *
 * Value mapping:
 * - Display value: 0-9
 * - Rotation per value: 36° (360° / 10 values)
 * - Smooth animation with duration scaling
 *
 * Duration scaling by change magnitude:
 * - 1 digit change: 150ms base
 * - 9 digit change: ~300ms scaled
 * - Formula: 150ms + (magnitude - 1) * 20ms
 */

/**
 * Single digit wheel animation
 */
interface DigitWheelAnimationState {
  columnIndex: number;
  wheelIndex: number; // Position in column (0-30)
  startValue: number; // 0-9
  endValue: number;   // 0-9
  currentValue: number; // Interpolated
  startRotation: number; // Radians
  endRotation: number;   // Radians
  currentRotation: number; // Radians (interpolated)
  duration: number; // ms
  elapsedTime: number; // ms
  isAnimating: boolean;
  isComplete: boolean;
}

/**
 * Column animation manager
 */
export class ColumnValueAnimation {
  private columnIndex: number;
  private wheelAnimations: DigitWheelAnimationState[] = [];
  private readonly wheelsPerColumn: number = 31; // Historical: 31 decimal places per column
  private readonly degreesPerValue: number = 36; // 360° / 10 values
  private readonly minDuration: number = 150; // ms for 1-step change
  private readonly durationPerStep: number = 20; // ms per additional step

  constructor(columnIndex: number) {
    this.columnIndex = columnIndex;
    this.initializeWheels();
  }

  /**
   * Initialize all wheels in column with default state
   */
  private initializeWheels(): void {
    this.wheelAnimations = [];

    for (let i = 0; i < this.wheelsPerColumn; i++) {
      this.wheelAnimations.push({
        columnIndex: this.columnIndex,
        wheelIndex: i,
        startValue: 0,
        endValue: 0,
        currentValue: 0,
        startRotation: 0,
        endRotation: 0,
        currentRotation: 0,
        duration: 0,
        elapsedTime: 0,
        isAnimating: false,
        isComplete: true
      });
    }
  }

  /**
   * Animate wheel to new value
   */
  animateWheelToValue(wheelIndex: number, newValue: number): void {
    if (wheelIndex < 0 || wheelIndex >= this.wheelsPerColumn) {
      return;
    }

    const wheel = this.wheelAnimations[wheelIndex];
    const oldValue = wheel.currentValue;

    // Calculate shortest rotation path
    let valueDelta = newValue - oldValue;

    // Handle wrapping (0-9 circular)
    if (Math.abs(valueDelta) > 5) {
      if (valueDelta > 0) {
        valueDelta -= 10;
      } else {
        valueDelta += 10;
      }
    }

    const magnitude = Math.abs(valueDelta);

    wheel.startValue = oldValue;
    wheel.endValue = newValue;
    wheel.startRotation = wheel.currentRotation;
    wheel.endRotation = wheel.currentRotation + (valueDelta * this.degreesPerValue * Math.PI) / 180;
    wheel.duration = this.calculateDuration(magnitude);
    wheel.elapsedTime = 0;
    wheel.isAnimating = true;
    wheel.isComplete = false;
  }

  /**
   * Animate entire column to new values
   */
  animateColumnToValues(newValues: number[]): void {
    if (newValues.length !== this.wheelsPerColumn) {
      console.warn('[ColumnValueAnimation] Invalid values array length');
      return;
    }

    for (let i = 0; i < this.wheelsPerColumn; i++) {
      this.animateWheelToValue(i, newValues[i]);
    }
  }

  /**
   * Update all wheel animations (call once per frame)
   */
  update(deltaTime: number = 16.67): void {
    for (const wheel of this.wheelAnimations) {
      if (!wheel.isAnimating) {
        continue;
      }

      wheel.elapsedTime += deltaTime;

      if (wheel.elapsedTime >= wheel.duration) {
        // Animation complete
        wheel.currentValue = wheel.endValue;
        wheel.currentRotation = wheel.endRotation;
        wheel.isAnimating = false;
        wheel.isComplete = true;
      } else {
        // Interpolate with ease-in-out-cubic
        const progress = wheel.elapsedTime / wheel.duration;
        const eased = this.easeInOutCubic(progress);

        // Interpolate value (for display purposes, not used in 3D)
        wheel.currentValue = wheel.startValue + (wheel.endValue - wheel.startValue) * eased;

        // Interpolate rotation
        wheel.currentRotation =
          wheel.startRotation + (wheel.endRotation - wheel.startRotation) * eased;
      }
    }
  }

  /**
   * Get current rotation for wheel (radians)
   */
  getWheelRotation(wheelIndex: number): number {
    if (wheelIndex < 0 || wheelIndex >= this.wheelsPerColumn) {
      return 0;
    }
    return this.wheelAnimations[wheelIndex].currentRotation;
  }

  /**
   * Get current rotation in degrees
   */
  getWheelRotationDegrees(wheelIndex: number): number {
    return (this.getWheelRotation(wheelIndex) * 180) / Math.PI;
  }

  /**
   * Get current displayed value for wheel
   */
  getWheelValue(wheelIndex: number): number {
    if (wheelIndex < 0 || wheelIndex >= this.wheelsPerColumn) {
      return 0;
    }
    return Math.round(this.wheelAnimations[wheelIndex].currentValue);
  }

  /**
   * Get all wheel rotations
   */
  getAllWheelRotations(): number[] {
    return this.wheelAnimations.map((wheel) => wheel.currentRotation);
  }

  /**
   * Get all wheel values (0-9)
   */
  getAllWheelValues(): number[] {
    return this.wheelAnimations.map((wheel) => Math.round(wheel.currentValue));
  }

  /**
   * Check if wheel is animating
   */
  isWheelAnimating(wheelIndex: number): boolean {
    if (wheelIndex < 0 || wheelIndex >= this.wheelsPerColumn) {
      return false;
    }
    return this.wheelAnimations[wheelIndex].isAnimating;
  }

  /**
   * Get number of animating wheels
   */
  getAnimatingCount(): number {
    return this.wheelAnimations.filter((wheel) => wheel.isAnimating).length;
  }

  /**
   * Check if entire column is done animating
   */
  isColumnAnimating(): boolean {
    return this.getAnimatingCount() > 0;
  }

  /**
   * Get column animation progress (0-1)
   */
  getColumnProgress(): number {
    const animatingWheels = this.wheelAnimations.filter((wheel) => wheel.isAnimating);

    if (animatingWheels.length === 0) {
      return 1; // All complete
    }

    // Average progress of animating wheels
    const totalProgress = animatingWheels.reduce((sum, wheel) => {
      return sum + wheel.elapsedTime / wheel.duration;
    }, 0);

    return totalProgress / animatingWheels.length;
  }

  /**
   * Reset all wheels to value 0
   */
  reset(): void {
    for (const wheel of this.wheelAnimations) {
      wheel.currentValue = 0;
      wheel.currentRotation = 0;
      wheel.startValue = 0;
      wheel.endValue = 0;
      wheel.isAnimating = false;
      wheel.isComplete = true;
      wheel.elapsedTime = 0;
    }
  }

  /**
   * Get wheel state for debugging
   */
  getWheelState(wheelIndex: number): DigitWheelAnimationState | null {
    if (wheelIndex < 0 || wheelIndex >= this.wheelsPerColumn) {
      return null;
    }
    return { ...this.wheelAnimations[wheelIndex] };
  }

  /**
   * Get all wheel states for debugging
   */
  getAllWheelStates(): DigitWheelAnimationState[] {
    return this.wheelAnimations.map((wheel) => ({ ...wheel }));
  }

  /**
   * Get debug info
   */
  getDebugInfo(): string {
    const animating = this.getAnimatingCount();
    const progress = this.getColumnProgress();

    return `
Column ${this.columnIndex} Value Animation:
- Wheels Animating: ${animating}/${this.wheelsPerColumn}
- Column Progress: ${(progress * 100).toFixed(1)}%
- Values: [${this.getAllWheelValues().join(',')}]
- Status: ${animating > 0 ? 'ANIMATING' : 'IDLE'}
    `.trim();
  }

  /**
   * Calculate animation duration based on change magnitude
   */
  private calculateDuration(magnitude: number): number {
    // 1 step: 150ms
    // 2 steps: 170ms
    // 9 steps: 310ms
    return this.minDuration + (magnitude - 1) * this.durationPerStep;
  }

  /**
   * Easing function: ease-in-out-cubic
   */
  private easeInOutCubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }
}

/**
 * Create column value animation for entire Difference Engine
 */
export function createAllColumnAnimations(): ColumnValueAnimation[] {
  const columns = [];
  for (let i = 0; i < 8; i++) {
    columns.push(new ColumnValueAnimation(i));
  }
  return columns;
}

/**
 * Create animation sequence for column value change
 */
export function createColumnValueAnimationSequence(
  columnIndex: number,
  newValues: number[],
  startTime: number = 0,
  staggerDelayMs: number = 50
): Array<{
  targetObject: string;
  startValue: number;
  endValue: number;
  duration: number;
  startTime: number;
  easing: 'linear' | 'easeInOutQuad' | 'easeInOutCubic' | 'easeInOutExpo' | 'easeOutBounce';
}> {
  const sequences = [];

  // Create staggered animations for each wheel in column
  for (let wheelIndex = 0; wheelIndex < newValues.length; wheelIndex++) {
    const wheelStartTime = startTime + wheelIndex * staggerDelayMs;
    const magnitude = Math.abs(newValues[wheelIndex]); // Simplified; assumes starting from 0
    const duration = 150 + (magnitude - 1) * 20;

    sequences.push({
      targetObject: `column${columnIndex}wheel${wheelIndex}`,
      startValue: 0,
      endValue: (newValues[wheelIndex] * 36 * Math.PI) / 180, // Convert to radians
      duration,
      startTime: wheelStartTime,
      easing: 'easeInOutCubic'
    });
  }

  return sequences;
}
