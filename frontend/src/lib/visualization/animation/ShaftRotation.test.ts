/**
 * Unit Tests: ShaftRotation and Quaternion
 *
 * Tests quaternion operations, SLERP interpolation,
 * gimbal lock prevention, and shaft rotation animations
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { Quaternion, ShaftRotation, normalizeAngle, getShortestRotationDelta, createAllShaftRotations } from './ShaftRotation';

describe('Quaternion', () => {
  describe('Initialization', () => {
    it('should create identity quaternion', () => {
      const q = new Quaternion(0, 0, 0, 1);
      expect(q.w).toBe(1);
      expect(q.x).toBe(0);
      expect(q.y).toBe(0);
      expect(q.z).toBe(0);
    });

    it('should create custom quaternion', () => {
      const q = new Quaternion(1, 2, 3, 4);
      expect(q.x).toBe(1);
      expect(q.y).toBe(2);
      expect(q.z).toBe(3);
      expect(q.w).toBe(4);
    });

    it('should use default parameters', () => {
      const q = new Quaternion();
      expect(q.x).toBe(0);
      expect(q.y).toBe(0);
      expect(q.z).toBe(0);
      expect(q.w).toBe(1);
    });
  });

  describe('Factory Methods', () => {
    it('should create from axis-angle', () => {
      const axis = { x: 0, y: 0, z: 1 };
      const angle = Math.PI / 2; // 90 degrees around Z axis

      const q = Quaternion.fromAxisAngle(axis, angle);
      expect(q).toBeDefined();
      expect(Math.abs(q.magnitude() - 1)).toBeLessThan(0.0001);
    });

    it('should create from Euler Z', () => {
      const angle = Math.PI / 4; // 45 degrees
      const q = Quaternion.fromEulerZ(angle);

      expect(q.x).toBe(0);
      expect(q.y).toBe(0);
      expect(Math.abs(q.z - Math.sin(angle / 2))).toBeLessThan(0.0001);
    });

    it('should create zero rotation from Euler Z (0 angle)', () => {
      const q = Quaternion.fromEulerZ(0);
      expect(q.x).toBe(0);
      expect(q.y).toBe(0);
      expect(q.z).toBe(0);
      expect(q.w).toBe(1);
    });
  });

  describe('Magnitude and Normalization', () => {
    it('should calculate magnitude', () => {
      const q = new Quaternion(1, 0, 0, 0);
      expect(q.magnitude()).toBeCloseTo(1, 5);
    });

    it('should normalize quaternion', () => {
      const q = new Quaternion(2, 2, 2, 2);
      const normalized = q.normalize();

      expect(normalized.magnitude()).toBeCloseTo(1, 5);
    });

    it('should handle zero magnitude gracefully', () => {
      const q = new Quaternion(0, 0, 0, 0);
      const normalized = q.normalize();

      // Should return identity when magnitude is zero
      expect(normalized.w).toBe(1);
      expect(normalized.x).toBe(0);
      expect(normalized.y).toBe(0);
      expect(normalized.z).toBe(0);
    });
  });

  describe('Conjugate', () => {
    it('should compute conjugate', () => {
      const q = new Quaternion(1, 2, 3, 4);
      const conj = q.conjugate();

      expect(conj.x).toBe(-1);
      expect(conj.y).toBe(-2);
      expect(conj.z).toBe(-3);
      expect(conj.w).toBe(4);
    });

    it('should verify conjugate property', () => {
      const q = new Quaternion(1, 2, 3, 4);
      expect(q.conjugate().conjugate().x).toBe(q.x);
    });
  });

  describe('Dot Product', () => {
    it('should compute dot product', () => {
      const q1 = new Quaternion(1, 0, 0, 0);
      const q2 = new Quaternion(0, 1, 0, 0);

      const dot = q1.dot(q2);
      expect(dot).toBe(0);
    });

    it('should give 1 for parallel quaternions', () => {
      const q1 = new Quaternion(1, 0, 0, 0);
      const dot = q1.dot(q1);

      expect(dot).toBeCloseTo(1, 5);
    });

    it('should give negative dot for opposite quaternions', () => {
      const q1 = new Quaternion(1, 0, 0, 0);
      const q2 = new Quaternion(-1, 0, 0, 0);

      const dot = q1.dot(q2);
      expect(dot).toBeLessThan(0);
    });
  });

  describe('SLERP Interpolation', () => {
    it('should interpolate between quaternions', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(Math.PI / 2);

      const qHalf = Quaternion.slerp(q1, q2, 0.5);
      expect(qHalf).toBeDefined();
      expect(qHalf.magnitude()).toBeCloseTo(1, 5);
    });

    it('should return start quaternion at t=0', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(Math.PI / 2);

      const q = Quaternion.slerp(q1, q2, 0);
      expect(q.x).toBeCloseTo(q1.x, 5);
      expect(q.y).toBeCloseTo(q1.y, 5);
      expect(q.z).toBeCloseTo(q1.z, 5);
      expect(q.w).toBeCloseTo(q1.w, 5);
    });

    it('should return end quaternion at t=1', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(Math.PI / 2);

      const q = Quaternion.slerp(q1, q2, 1);
      expect(Math.abs(q.x - q2.x)).toBeLessThan(0.001);
      expect(Math.abs(q.y - q2.y)).toBeLessThan(0.001);
      expect(Math.abs(q.z - q2.z)).toBeLessThan(0.001);
      expect(Math.abs(q.w - q2.w)).toBeLessThan(0.001);
    });

    it('should clamp t to [0, 1]', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(Math.PI / 2);

      const qOver = Quaternion.slerp(q1, q2, 2);
      const qMax = Quaternion.slerp(q1, q2, 1);

      expect(qOver.x).toBeCloseTo(qMax.x, 5);
    });

    it('should handle very small angles (linear fallback)', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(0.0001);

      const q = Quaternion.slerp(q1, q2, 0.5);
      expect(q.magnitude()).toBeCloseTo(1, 5);
    });

    it('should take shortest path', () => {
      const q1 = Quaternion.fromEulerZ(0);
      const q2 = Quaternion.fromEulerZ(Math.PI * 1.5); // Large angle

      const q = Quaternion.slerp(q1, q2, 0.5);
      expect(q.magnitude()).toBeCloseTo(1, 5);
    });
  });

  describe('Euler Angle Conversion', () => {
    it('should convert to Euler Z', () => {
      const angle = Math.PI / 4;
      const q = Quaternion.fromEulerZ(angle);
      const convertedAngle = q.toEulerZ();

      expect(Math.abs(convertedAngle - angle)).toBeLessThan(0.001);
    });

    it('should handle zero rotation', () => {
      const q = new Quaternion(0, 0, 0, 1);
      const angle = q.toEulerZ();

      expect(Math.abs(angle)).toBeLessThan(0.001);
    });
  });

  describe('Axis-Angle Conversion', () => {
    it('should convert to axis-angle', () => {
      const axis = { x: 0, y: 0, z: 1 };
      const angle = Math.PI / 2;

      const q = Quaternion.fromAxisAngle(axis, angle);
      const result = q.toAxisAngle();

      expect(Math.abs(result.angle - angle)).toBeLessThan(0.001);
      expect(Math.abs(result.axis.z - 1)).toBeLessThan(0.001);
    });

    it('should handle zero angle', () => {
      const q = new Quaternion(0, 0, 0, 1);
      const result = q.toAxisAngle();

      expect(Math.abs(result.angle)).toBeLessThan(0.001);
      expect(result.axis.z).toBe(1);
    });
  });
});

describe('ShaftRotation', () => {
  let shaft: ShaftRotation;

  beforeEach(() => {
    shaft = new ShaftRotation(0, 0, Math.PI / 2, 200); // Rotate from 0 to 90° in 200ms
  });

  describe('Initialization', () => {
    it('should create shaft rotation', () => {
      expect(shaft).toBeDefined();
      expect(shaft.getShaftId()).toBe(0);
    });

    it('should start with start angle', () => {
      expect(shaft.getRotation()).toBeCloseTo(0, 5);
    });

    it('should track progress at 0', () => {
      expect(shaft.getProgress()).toBeCloseTo(0, 5);
    });

    it('should not be complete initially', () => {
      expect(shaft.isComplete()).toBe(false);
    });
  });

  describe('Animation Update', () => {
    it('should update rotation over time', () => {
      const initial = shaft.getRotation();
      shaft.update(100);
      const updated = shaft.getRotation();

      expect(updated).toBeGreaterThan(initial);
    });

    it('should reach end angle when complete', () => {
      shaft.update(300); // 200ms duration + 100ms extra
      expect(shaft.getRotation()).toBeCloseTo(Math.PI / 2, 2);
    });

    it('should accumulate delta time', () => {
      shaft.update(100);
      const progress1 = shaft.getProgress();

      shaft.update(100);
      const progress2 = shaft.getProgress();

      expect(progress2).toBeGreaterThan(progress1);
    });

    it('should mark complete when duration exceeded', () => {
      shaft.update(300);
      expect(shaft.isComplete()).toBe(true);
    });
  });

  describe('Progress Tracking', () => {
    it('should return 0 progress at start', () => {
      expect(shaft.getProgress()).toBeCloseTo(0, 5);
    });

    it('should return 0.5 progress at half duration', () => {
      shaft.update(100);
      expect(shaft.getProgress()).toBeCloseTo(0.5, 1);
    });

    it('should return 1 progress when complete', () => {
      shaft.update(300);
      expect(shaft.getProgress()).toBeCloseTo(1, 2);
    });
  });

  describe('Rotation at Progress', () => {
    it('should get rotation at specific progress', () => {
      const rotation = shaft.getRotationAtProgress(0.5);
      expect(rotation).toBeDefined();
      expect(rotation).toBeGreaterThan(0);
      expect(rotation).toBeLessThan(Math.PI / 2);
    });

    it('should clamp progress to [0, 1]', () => {
      const rot1 = shaft.getRotationAtProgress(-1);
      const rot2 = shaft.getRotationAtProgress(0);

      expect(rot1).toBeCloseTo(rot2, 5);
    });
  });

  describe('Target Change', () => {
    it('should update target angle', () => {
      shaft.update(300); // Reach end

      shaft.setTarget(Math.PI, 200);
      expect(shaft.getProgress()).toBeCloseTo(0, 2);
    });

    it('should reset elapsed time when target changes', () => {
      shaft.update(100);
      shaft.setTarget(Math.PI, 200);

      expect(shaft.getElapsedTime()).toBe(0);
    });
  });

  describe('Debug Info', () => {
    it('should provide debug info', () => {
      const info = shaft.getDebugInfo();
      expect(typeof info).toBe('string');
      expect(info.length).toBeGreaterThan(0);
    });

    it('should include angle info in debug', () => {
      const info = shaft.getDebugInfo();
      expect(info).toContain('Shaft');
      expect(info).toContain('Progress');
    });
  });

  describe('Gimbal Lock Prevention', () => {
    it('should use quaternion SLERP to prevent gimbal lock', () => {
      // Multiple rotations should be smooth
      const shaft1 = new ShaftRotation(0, 0, Math.PI / 2, 200);
      const shaft2 = new ShaftRotation(1, Math.PI / 2, Math.PI, 200);

      shaft1.update(100);
      shaft2.update(100);

      const rot1 = shaft1.getRotation();
      const rot2 = shaft2.getRotation();

      // Both should have progressed
      expect(rot1).toBeGreaterThan(0);
      expect(rot2).toBeGreaterThan(Math.PI / 2);
    });
  });
});

describe('Utility Functions', () => {
  describe('normalizeAngle', () => {
    it('should normalize to [0, 2π)', () => {
      const angle = 3 * Math.PI;
      const normalized = normalizeAngle(angle);

      expect(normalized).toBeGreaterThanOrEqual(0);
      expect(normalized).toBeLessThan(2 * Math.PI);
    });

    it('should handle negative angles', () => {
      const angle = -Math.PI / 2;
      const normalized = normalizeAngle(angle);

      expect(normalized).toBeGreaterThanOrEqual(0);
      expect(normalized).toBeLessThan(2 * Math.PI);
    });

    it('should preserve angles already in range', () => {
      const angle = Math.PI / 4;
      expect(normalizeAngle(angle)).toBeCloseTo(angle, 5);
    });
  });

  describe('getShortestRotationDelta', () => {
    it('should calculate short path', () => {
      const delta = getShortestRotationDelta(0, Math.PI / 2);
      expect(Math.abs(delta - Math.PI / 2)).toBeLessThan(0.001);
    });

    it('should prefer short path over long path', () => {
      const delta = getShortestRotationDelta(0, 3 * Math.PI);
      expect(Math.abs(delta)).toBeLessThan(Math.PI);
    });

    it('should handle negative deltas', () => {
      const delta = getShortestRotationDelta(Math.PI, Math.PI / 4);
      expect(delta).toBeLessThan(0);
    });
  });

  describe('createAllShaftRotations', () => {
    it('should create 8 shafts', () => {
      const rotations = [0, 0, 0, 0, 0, 0, 0, 0];
      const newRotations = [Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2];

      const shafts = createAllShaftRotations(rotations, newRotations, 200);

      expect(shafts.length).toBe(8);
    });

    it('should create shafts with correct indices', () => {
      const rotations = [0, 0, 0, 0, 0, 0, 0, 0];
      const newRotations = [Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2];

      const shafts = createAllShaftRotations(rotations, newRotations, 200);

      for (let i = 0; i < 8; i++) {
        expect(shafts[i].getShaftId()).toBe(i);
      }
    });

    it('should respect custom duration', () => {
      const rotations = [0, 0, 0, 0, 0, 0, 0, 0];
      const newRotations = [Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2, Math.PI / 2];

      const shafts = createAllShaftRotations(rotations, newRotations, 500);

      shafts[0].update(250);
      expect(shafts[0].getProgress()).toBeCloseTo(0.5, 1);
    });
  });
});
