/**
 * Shaft Rotation: Quaternion-Based Smooth Rotation
 *
 * Uses quaternion SLERP (Spherical Linear Interpolation) for:
 * - Smooth interpolation between rotation states
 * - Avoiding gimbal lock
 * - Optimal rotation paths
 *
 * Implemented for 8 shafts in the Difference Engine:
 * - INPUT shaft
 * - 6 ADDEND shafts
 * - OUTPUT shaft
 */

/**
 * Simple quaternion class
 */
export class Quaternion {
  x: number;
  y: number;
  z: number;
  w: number;

  constructor(x: number = 0, y: number = 0, z: number = 0, w: number = 1) {
    this.x = x;
    this.y = y;
    this.z = z;
    this.w = w;
  }

  /**
   * Create from axis-angle rotation
   */
  static fromAxisAngle(axis: { x: number; y: number; z: number }, angle: number): Quaternion {
    const halfAngle = angle / 2;
    const sin = Math.sin(halfAngle);
    return new Quaternion(
      axis.x * sin,
      axis.y * sin,
      axis.z * sin,
      Math.cos(halfAngle)
    );
  }

  /**
   * Create quaternion from Euler angles (Z rotation only for shaft)
   */
  static fromEulerZ(angleZ: number): Quaternion {
    const halfAngle = angleZ / 2;
    return new Quaternion(
      0,
      0,
      Math.sin(halfAngle),
      Math.cos(halfAngle)
    );
  }

  /**
   * Conjugate
   */
  conjugate(): Quaternion {
    return new Quaternion(-this.x, -this.y, -this.z, this.w);
  }

  /**
   * Magnitude
   */
  magnitude(): number {
    return Math.sqrt(this.x * this.x + this.y * this.y + this.z * this.z + this.w * this.w);
  }

  /**
   * Normalize
   */
  normalize(): Quaternion {
    const mag = this.magnitude();
    if (mag === 0) return new Quaternion(0, 0, 0, 1);
    return new Quaternion(
      this.x / mag,
      this.y / mag,
      this.z / mag,
      this.w / mag
    );
  }

  /**
   * Dot product
   */
  dot(other: Quaternion): number {
    return this.x * other.x + this.y * other.y + this.z * other.z + this.w * other.w;
  }

  /**
   * SLERP: Spherical Linear Interpolation
   * Smoothly interpolates between two rotations
   */
  static slerp(q1: Quaternion, q2: Quaternion, t: number): Quaternion {
    // Clamp t to [0, 1]
    t = Math.max(0, Math.min(1, t));

    // Make copies to avoid mutation
    let qa = new Quaternion(q1.x, q1.y, q1.z, q1.w);
    let qb = new Quaternion(q2.x, q2.y, q2.z, q2.w);

    // Calculate dot product
    let dot = qa.dot(qb);

    // If dot < 0, negate one quaternion to take the shorter path
    if (dot < 0) {
      qb.x = -qb.x;
      qb.y = -qb.y;
      qb.z = -qb.z;
      qb.w = -qb.w;
      dot = -dot;
    }

    // Clamp dot to avoid numerical issues with acos
    dot = Math.max(-1, Math.min(1, dot));

    // Get the angle between the quaternions
    const theta = Math.acos(dot);
    const sinTheta = Math.sin(theta);

    // If theta is very small, use linear interpolation
    if (Math.abs(sinTheta) < 0.001) {
      return new Quaternion(
        qa.x + (qb.x - qa.x) * t,
        qa.y + (qb.y - qa.y) * t,
        qa.z + (qb.z - qa.z) * t,
        qa.w + (qb.w - qa.w) * t
      ).normalize();
    }

    const w1 = Math.sin((1 - t) * theta) / sinTheta;
    const w2 = Math.sin(t * theta) / sinTheta;

    return new Quaternion(
      qa.x * w1 + qb.x * w2,
      qa.y * w1 + qb.y * w2,
      qa.z * w1 + qb.z * w2,
      qa.w * w1 + qb.w * w2
    );
  }

  /**
   * Convert to Euler angles (Z rotation only)
   */
  toEulerZ(): number {
    // Extract Z rotation from quaternion
    const sinHalf = 2 * (this.w * this.z + this.x * this.y);
    const cosHalf = 1 - 2 * (this.z * this.z + this.y * this.y);
    return Math.atan2(sinHalf, cosHalf);
  }

  /**
   * Convert to axis-angle representation
   */
  toAxisAngle(): { axis: { x: number; y: number; z: number }; angle: number } {
    const n = this.normalize();
    const angle = 2 * Math.acos(Math.max(-1, Math.min(1, n.w)));
    const sinAngle = Math.sin(angle / 2);

    if (sinAngle < 0.001) {
      return {
        axis: { x: 0, y: 0, z: 1 },
        angle: 0
      };
    }

    return {
      axis: {
        x: n.x / sinAngle,
        y: n.y / sinAngle,
        z: n.z / sinAngle
      },
      angle
    };
  }
}

/**
 * Shaft rotation animation manager
 */
export class ShaftRotation {
  private shaftId: number;
  private startRotation: number; // Radians
  private endRotation: number; // Radians
  private startQuaternion: Quaternion;
  private endQuaternion: Quaternion;
  private duration: number; // milliseconds
  private elapsedTime: number = 0;

  constructor(shaftId: number, startAngle: number, endAngle: number, durationMs: number = 200) {
    this.shaftId = shaftId;
    this.startRotation = startAngle;
    this.endRotation = endAngle;
    this.duration = durationMs;

    // Create quaternions from rotations
    this.startQuaternion = Quaternion.fromEulerZ(startAngle);
    this.endQuaternion = Quaternion.fromEulerZ(endAngle);
  }

  /**
   * Update rotation based on elapsed time
   */
  update(deltaTime: number = 16.67): number {
    this.elapsedTime = Math.min(this.elapsedTime + deltaTime, this.duration);
    return this.getRotation();
  }

  /**
   * Get interpolated rotation at current time
   */
  getRotation(): number {
    if (this.duration <= 0) {
      return this.endRotation;
    }

    const progress = this.elapsedTime / this.duration;
    const interpolated = Quaternion.slerp(
      this.startQuaternion,
      this.endQuaternion,
      progress
    );

    return interpolated.toEulerZ();
  }

  /**
   * Get rotation at specific progress (0-1)
   */
  getRotationAtProgress(progress: number): number {
    const clampedProgress = Math.max(0, Math.min(1, progress));
    const interpolated = Quaternion.slerp(
      this.startQuaternion,
      this.endQuaternion,
      clampedProgress
    );

    return interpolated.toEulerZ();
  }

  /**
   * Get current progress (0-1)
   */
  getProgress(): number {
    if (this.duration <= 0) {
      return 1;
    }
    return Math.min(1, this.elapsedTime / this.duration);
  }

  /**
   * Check if animation is complete
   */
  isComplete(): boolean {
    return this.elapsedTime >= this.duration;
  }

  /**
   * Get current elapsed time
   */
  getElapsedTime(): number {
    return this.elapsedTime;
  }

  /**
   * Reset animation
   */
  reset(): void {
    this.elapsedTime = 0;
  }

  /**
   * Set new target rotation
   */
  setTarget(newEndAngle: number, newDurationMs: number = 200): void {
    this.startRotation = this.getRotation();
    this.endRotation = newEndAngle;
    this.duration = newDurationMs;
    this.elapsedTime = 0;

    this.startQuaternion = Quaternion.fromEulerZ(this.startRotation);
    this.endQuaternion = Quaternion.fromEulerZ(newEndAngle);
  }

  /**
   * Get shaft ID
   */
  getShaftId(): number {
    return this.shaftId;
  }

  /**
   * Get debug info
   */
  getDebugInfo(): string {
    return `
Shaft ${this.shaftId} Rotation:
- Start: ${this.startRotation.toFixed(4)} rad (${(this.startRotation * 180 / Math.PI).toFixed(1)}°)
- End: ${this.endRotation.toFixed(4)} rad (${(this.endRotation * 180 / Math.PI).toFixed(1)}°)
- Current: ${this.getRotation().toFixed(4)} rad (${(this.getRotation() * 180 / Math.PI).toFixed(1)}°)
- Progress: ${(this.getProgress() * 100).toFixed(1)}%
- Duration: ${this.duration}ms
    `.trim();
  }
}

/**
 * Utility function: Normalize rotation angle to [0, 2π)
 */
export function normalizeAngle(angle: number): number {
  const twoPi = Math.PI * 2;
  let normalized = angle % twoPi;
  if (normalized < 0) {
    normalized += twoPi;
  }
  return normalized;
}

/**
 * Utility function: Get shortest rotation path between two angles
 */
export function getShortestRotationDelta(from: number, to: number): number {
  let delta = to - from;
  while (delta > Math.PI) {
    delta -= Math.PI * 2;
  }
  while (delta < -Math.PI) {
    delta += Math.PI * 2;
  }
  return delta;
}

/**
 * Create all 8 shaft rotations
 */
export function createAllShaftRotations(
  shaftRotations: number[],
  newRotations: number[],
  durationMs: number = 200
): ShaftRotation[] {
  const rotations: ShaftRotation[] = [];
  for (let i = 0; i < 8; i++) {
    rotations.push(new ShaftRotation(i, shaftRotations[i], newRotations[i], durationMs));
  }
  return rotations;
}
