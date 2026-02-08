/**
 * Carry Propagation: Sequential Lever Engagement Animations
 *
 * Manages 8 carry levers with:
 * - Sequential engagement (staggered 30ms between each)
 * - Smooth rotation from 0° to 90° during engagement
 * - Propagation progress tracking (0-1)
 * - Individual lever state tracking
 *
 * Timeline:
 * - Lever 0: 0ms - 150ms (160ms total)
 * - Lever 1: 30ms - 180ms
 * - Lever 2: 60ms - 210ms
 * - Lever 3: 90ms - 240ms
 * - Lever 4: 120ms - 270ms
 * - Lever 5: 150ms - 300ms
 * - Lever 6: 180ms - 330ms
 * - Lever 7: 210ms - 360ms
 *
 * Total propagation: 360ms
 */

/**
 * Individual lever animation state
 */
interface LeverAnimationState {
  leverIndex: number;
  startTime: number;      // Absolute time when lever engagement starts (ms)
  duration: number;       // 160ms standard engagement duration
  engagementRadians: number; // 0 to π/2 radians (0° to 90°)
  isEngaged: boolean;
  isComplete: boolean;
  elapsedTime: number;
}

/**
 * Carry propagation orchestrator
 */
export class CarryPropagation {
  private leverAnimations: LeverAnimationState[] = [];
  private elapsedTime: number = 0;
  private isActive: boolean = false;
  private readonly totalDuration: number = 360; // Total time for all 8 levers (ms)
  private readonly leverCount: number = 8;
  private readonly staggerDelayMs: number = 30; // Delay between lever starts
  private readonly leverDurationMs: number = 160; // Individual lever engagement time

  constructor() {
    this.initializeLeverAnimations();
  }

  /**
   * Initialize all lever animation states
   */
  private initializeLeverAnimations(): void {
    this.leverAnimations = [];

    for (let i = 0; i < this.leverCount; i++) {
      const startTime = i * this.staggerDelayMs;
      this.leverAnimations.push({
        leverIndex: i,
        startTime,
        duration: this.leverDurationMs,
        engagementRadians: 0,
        isEngaged: false,
        isComplete: false,
        elapsedTime: 0
      });
    }
  }

  /**
   * Start carry propagation sequence
   */
  start(): void {
    this.isActive = true;
    this.elapsedTime = 0;
    this.initializeLeverAnimations();
  }

  /**
   * Update propagation animation (call once per frame)
   */
  update(deltaTime: number = 16.67): void {
    if (!this.isActive) {
      return;
    }

    this.elapsedTime += deltaTime;

    // Update each lever's animation state
    for (const lever of this.leverAnimations) {
      const elapsedSinceStart = this.elapsedTime - lever.startTime;

      // Lever hasn't started yet
      if (elapsedSinceStart < 0) {
        lever.engagementRadians = 0;
        lever.isEngaged = false;
        lever.isComplete = false;
        continue;
      }

      // Lever is animating
      if (elapsedSinceStart < lever.duration) {
        lever.isEngaged = true;
        lever.isComplete = false;

        // Smooth easing: ease-in-out-cubic for natural mechanical feel
        const progress = elapsedSinceStart / lever.duration;
        const eased = this.easeInOutCubic(progress);

        // Rotate from 0 to π/2 radians (0° to 90°)
        lever.engagementRadians = eased * (Math.PI / 2);
        lever.elapsedTime = elapsedSinceStart;
      } else {
        // Lever animation complete
        lever.isEngaged = true;
        lever.isComplete = true;
        lever.engagementRadians = Math.PI / 2; // 90°
        lever.elapsedTime = lever.duration;
      }
    }

    // Check if entire propagation is complete
    if (this.elapsedTime >= this.totalDuration) {
      this.isActive = false;
    }
  }

  /**
   * Get engagement angle for specific lever (radians)
   */
  getLeverEngagement(leverIndex: number): number {
    if (leverIndex < 0 || leverIndex >= this.leverCount) {
      return 0;
    }
    return this.leverAnimations[leverIndex].engagementRadians;
  }

  /**
   * Get engagement angle in degrees
   */
  getLeverEngagementDegrees(leverIndex: number): number {
    return (this.getLeverEngagement(leverIndex) * 180) / Math.PI;
  }

  /**
   * Check if specific lever is engaged
   */
  isLeverEngaged(leverIndex: number): boolean {
    if (leverIndex < 0 || leverIndex >= this.leverCount) {
      return false;
    }
    return this.leverAnimations[leverIndex].isEngaged;
  }

  /**
   * Check if specific lever animation is complete
   */
  isLeverComplete(leverIndex: number): boolean {
    if (leverIndex < 0 || leverIndex >= this.leverCount) {
      return false;
    }
    return this.leverAnimations[leverIndex].isComplete;
  }

  /**
   * Get all lever engagement angles as array
   */
  getAllLeverEngagements(): number[] {
    return this.leverAnimations.map((lever) => lever.engagementRadians);
  }

  /**
   * Get propagation progress (0-1)
   */
  getProgress(): number {
    return Math.min(1, this.elapsedTime / this.totalDuration);
  }

  /**
   * Get current elapsed time
   */
  getElapsedTime(): number {
    return this.elapsedTime;
  }

  /**
   * Check if propagation is active
   */
  isActive_(): boolean {
    return this.isActive;
  }

  /**
   * Check if propagation is complete
   */
  isComplete(): boolean {
    return !this.isActive && this.elapsedTime >= this.totalDuration;
  }

  /**
   * Get number of engaged levers
   */
  getEngagedCount(): number {
    return this.leverAnimations.filter((lever) => lever.isEngaged).length;
  }

  /**
   * Get number of complete levers
   */
  getCompleteCount(): number {
    return this.leverAnimations.filter((lever) => lever.isComplete).length;
  }

  /**
   * Reset propagation state
   */
  reset(): void {
    this.elapsedTime = 0;
    this.isActive = false;
    this.initializeLeverAnimations();
  }

  /**
   * Stop propagation immediately
   */
  stop(): void {
    this.isActive = false;
  }

  /**
   * Pause propagation (maintain state)
   */
  pause(): void {
    this.isActive = false;
  }

  /**
   * Resume propagation from pause
   */
  resume(): void {
    if (!this.isComplete()) {
      this.isActive = true;
    }
  }

  /**
   * Get lever state for debugging
   */
  getLeverState(leverIndex: number): LeverAnimationState | null {
    if (leverIndex < 0 || leverIndex >= this.leverCount) {
      return null;
    }
    return { ...this.leverAnimations[leverIndex] };
  }

  /**
   * Get all lever states for debugging
   */
  getAllLeverStates(): LeverAnimationState[] {
    return this.leverAnimations.map((lever) => ({ ...lever }));
  }

  /**
   * Get debug summary
   */
  getDebugInfo(): string {
    return `
Carry Propagation:
- Elapsed: ${this.elapsedTime.toFixed(1)}ms / ${this.totalDuration}ms
- Progress: ${(this.getProgress() * 100).toFixed(1)}%
- Active: ${this.isActive}
- Engaged: ${this.getEngagedCount()}/${this.leverCount}
- Complete: ${this.getCompleteCount()}/${this.leverCount}
- Status: ${this.isComplete() ? 'COMPLETE' : this.isActive ? 'ANIMATING' : 'IDLE'}
    `.trim();
  }

  /**
   * Easing function: ease-in-out-cubic
   * Smooth acceleration and deceleration
   */
  private easeInOutCubic(t: number): number {
    return t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2;
  }
}

/**
 * Create carry propagation animation sequence
 * Returns array of AnimationStep objects for use with Timeline
 */
export function createCarryPropagationSequence(
  startTime: number = 0
): Array<{
  targetObject: string;
  startValue: number;
  endValue: number;
  duration: number;
  startTime: number;
  easing: 'linear' | 'easeInOutQuad' | 'easeInOutCubic' | 'easeInOutExpo' | 'easeOutBounce';
}> {
  const sequences = [];

  // Create staggered animations for each of 8 carry levers
  for (let i = 0; i < 8; i++) {
    const leverStartTime = startTime + i * 30; // 30ms stagger
    sequences.push({
      targetObject: `carryLever${i}`,
      startValue: 0, // 0 radians
      endValue: Math.PI / 2, // π/2 radians (90°)
      duration: 160, // 160ms engagement
      startTime: leverStartTime,
      easing: 'easeInOutCubic'
    });
  }

  return sequences;
}
