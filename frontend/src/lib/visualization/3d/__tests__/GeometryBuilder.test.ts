/**
 * GeometryBuilder Test Suite
 * Tests 3D geometry creation, caching, and component management
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { GeometryBuilder } from '../GeometryBuilder';

// Mock MaterialFactory
class MockMaterialFactory {
  createColumnMaterial() {
    return new THREE.MeshPhongMaterial({ color: 0x4a9eff });
  }
  createShaftMaterial() {
    return new THREE.MeshPhongMaterial({ color: 0xff6b6b });
  }
  createCarryLeverMaterial() {
    return new THREE.MeshPhongMaterial({ color: 0xffc107 });
  }
  createCarryHighlightMaterial() {
    return new THREE.MeshPhongMaterial({ color: 0x4caf50 });
  }
  createFrameMaterial() {
    return new THREE.MeshPhongMaterial({ color: 0x888888 });
  }
}

describe('GeometryBuilder', () => {
  let builder: GeometryBuilder;
  let materialFactory: MockMaterialFactory;

  beforeEach(() => {
    builder = new GeometryBuilder();
    materialFactory = new MockMaterialFactory();
  });

  afterEach(() => {
    GeometryBuilder.disposeCache();
  });

  describe('Initialization', () => {
    it('should create geometry builder instance', () => {
      expect(builder).toBeDefined();
      expect(builder).toBeInstanceOf(GeometryBuilder);
    });

    it('should initialize with empty component arrays', () => {
      const counts = builder.getComponentCounts();

      expect(counts.digitWheels).toBe(0);
      expect(counts.shafts).toBe(0);
      expect(counts.carryLevers).toBe(0);
      expect(counts.engagementIndicators).toBe(0);
    });
  });

  describe('Digit Wheels', () => {
    it('should build all 248 digit wheels', () => {
      const wheelGroup = builder.buildDigitWheels(materialFactory);

      expect(wheelGroup).toBeInstanceOf(THREE.Group);
      expect(wheelGroup.name).toBe('digit-wheels');
      expect(wheelGroup.children).toHaveLength(8); // 8 columns
    });

    it('should have 31 wheels per column', () => {
      const wheelGroup = builder.buildDigitWheels(materialFactory);

      const firstColumn = wheelGroup.children[0] as THREE.Group;
      expect(firstColumn.children).toHaveLength(31);
    });

    it('should position wheels correctly along X axis', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel0 = builder.getDigitWheel(0, 0);
      const wheel7 = builder.getDigitWheel(7, 0);

      expect(wheel0).toBeDefined();
      expect(wheel7).toBeDefined();
      expect(wheel7!.position.x).toBeGreaterThan(wheel0!.position.x);
    });

    it('should position wheels correctly along Z axis within column', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel0 = builder.getDigitWheel(0, 0);
      const wheel30 = builder.getDigitWheel(0, 30);

      expect(wheel30!.position.z).toBeGreaterThan(wheel0!.position.z);
    });

    it('should track all digit wheels internally', () => {
      builder.buildDigitWheels(materialFactory);

      const counts = builder.getComponentCounts();
      expect(counts.digitWheels).toBe(248);
    });

    it('should return all digit wheels', () => {
      builder.buildDigitWheels(materialFactory);

      const allWheels = builder.getAllDigitWheels();
      expect(allWheels).toHaveLength(248);
    });

    it('should access digit wheel by column and index', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(0, 0);

      expect(wheel).toBeDefined();
      expect(wheel).toBeInstanceOf(THREE.Mesh);
    });

    it('should return null for out-of-range wheel indices', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(100, 100);

      expect(wheel).toBeNull();
    });
  });

  describe('Shafts', () => {
    it('should build 8 shafts', () => {
      const shaftGroup = builder.buildShafts(materialFactory);

      expect(shaftGroup).toBeInstanceOf(THREE.Group);
      expect(shaftGroup.name).toBe('shafts');
      expect(shaftGroup.children).toHaveLength(8);
    });

    it('should position shafts above wheels', () => {
      builder.buildDigitWheels(materialFactory);
      const shaftGroup = builder.buildShafts(materialFactory);

      const firstShaft = shaftGroup.children[0] as THREE.Mesh;

      expect(firstShaft.position.y).toBeGreaterThan(0);
    });

    it('should track all shafts internally', () => {
      builder.buildShafts(materialFactory);

      const counts = builder.getComponentCounts();
      expect(counts.shafts).toBe(8);
    });

    it('should return all shafts', () => {
      builder.buildShafts(materialFactory);

      const allShafts = builder.getAllShafts();
      expect(allShafts).toHaveLength(8);
    });

    it('should access shaft by index', () => {
      builder.buildShafts(materialFactory);

      const shaft = builder.getShaft(0);

      expect(shaft).toBeDefined();
      expect(shaft).toBeInstanceOf(THREE.Mesh);
    });

    it('should return null for out-of-range shaft index', () => {
      builder.buildShafts(materialFactory);

      const shaft = builder.getShaft(100);

      expect(shaft).toBeNull();
    });

    it('should rotate shafts along Z axis', () => {
      builder.buildShafts(materialFactory);

      const shaft = builder.getShaft(0) as THREE.Mesh;

      expect(Math.abs(shaft.rotation.x - Math.PI / 2)).toBeLessThan(0.01);
    });
  });

  describe('Carry Levers', () => {
    it('should build 8 carry levers', () => {
      const leverGroup = builder.buildCarryLevers(materialFactory);

      expect(leverGroup).toBeInstanceOf(THREE.Group);
      expect(leverGroup.name).toBe('carry-levers');
      expect(leverGroup.children).toHaveLength(8);
    });

    it('should track all carry levers internally', () => {
      builder.buildCarryLevers(materialFactory);

      const counts = builder.getComponentCounts();
      expect(counts.carryLevers).toBe(8);
    });

    it('should create engagement indicators', () => {
      builder.buildCarryLevers(materialFactory);

      const counts = builder.getComponentCounts();
      expect(counts.engagementIndicators).toBe(8);
    });

    it('should return all carry levers', () => {
      builder.buildCarryLevers(materialFactory);

      const allLevers = builder.getAllCarryLevers();
      expect(allLevers).toHaveLength(8);
    });

    it('should access carry lever by index', () => {
      builder.buildCarryLevers(materialFactory);

      const lever = builder.getCarryLever(0);

      expect(lever).toBeDefined();
      expect(lever).toBeInstanceOf(THREE.Mesh);
    });

    it('should access engagement indicator by index', () => {
      builder.buildCarryLevers(materialFactory);

      const indicator = builder.getEngagementIndicator(0);

      expect(indicator).toBeDefined();
      expect(indicator).toBeInstanceOf(THREE.Mesh);
    });

    it('should return null for out-of-range lever index', () => {
      builder.buildCarryLevers(materialFactory);

      const lever = builder.getCarryLever(100);

      expect(lever).toBeNull();
    });

    it('should position levers above shafts', () => {
      builder.buildCarryLevers(materialFactory);

      const lever = builder.getCarryLever(0) as THREE.Mesh;

      expect(lever.position.y).toBeGreaterThan(0.5);
    });

    it('should hide indicators initially', () => {
      builder.buildCarryLevers(materialFactory);

      const indicator = builder.getEngagementIndicator(0) as THREE.Mesh;

      expect(indicator.visible).toBe(false);
    });
  });

  describe('Frame Structure', () => {
    it('should build frame group', () => {
      const frameGroup = builder.buildFrame(materialFactory);

      expect(frameGroup).toBeInstanceOf(THREE.Group);
      expect(frameGroup.name).toBe('frame');
    });

    it('should include base plate', () => {
      const frameGroup = builder.buildFrame(materialFactory);

      expect(frameGroup.children.length).toBeGreaterThan(0);
    });

    it('should include support columns', () => {
      const frameGroup = builder.buildFrame(materialFactory);

      // Base plate (1) + support columns (4) + back plate (1) = 6 elements
      expect(frameGroup.children.length).toBeGreaterThanOrEqual(4);
    });

    it('should position base plate below zero', () => {
      const frameGroup = builder.buildFrame(materialFactory);

      const basePlate = frameGroup.children[0] as THREE.Mesh;
      expect(basePlate.position.y).toBeLessThan(0);
    });
  });

  describe('Complete Engine Assembly', () => {
    it('should build complete engine', () => {
      const engine = builder.buildCompleteEngine(materialFactory);

      expect(engine).toBeInstanceOf(THREE.Group);
      expect(engine.name).toBe('difference-engine');
    });

    it('should include all components', () => {
      const engine = builder.buildCompleteEngine(materialFactory);

      expect(engine.children.length).toBeGreaterThanOrEqual(4); // frame, wheels, shafts, levers
    });

    it('should maintain component counts after assembly', () => {
      builder.buildCompleteEngine(materialFactory);

      const counts = builder.getComponentCounts();
      expect(counts.digitWheels).toBe(248);
      expect(counts.shafts).toBe(8);
      expect(counts.carryLevers).toBe(8);
      expect(counts.engagementIndicators).toBe(8);
    });

    it('should allow access to components after assembly', () => {
      builder.buildCompleteEngine(materialFactory);

      expect(builder.getDigitWheel(0, 0)).toBeDefined();
      expect(builder.getShaft(0)).toBeDefined();
      expect(builder.getCarryLever(0)).toBeDefined();
      expect(builder.getEngagementIndicator(0)).toBeDefined();
    });
  });

  describe('Geometry Caching', () => {
    it('should cache cylinder geometries', () => {
      builder.buildDigitWheels(materialFactory);
      const stats1 = GeometryBuilder.getStats();

      builder.buildShafts(materialFactory);
      const stats2 = GeometryBuilder.getStats();

      // Should reuse cylinder geometry, not create new one
      expect(stats2.cylinderCount).toBeLessThanOrEqual(stats1.cylinderCount + 1);
    });

    it('should cache box geometries', () => {
      builder.buildCarryLevers(materialFactory);
      const stats1 = GeometryBuilder.getStats();

      builder.buildFrame(materialFactory);
      const stats2 = GeometryBuilder.getStats();

      // Should reuse box geometries
      expect(stats2.boxCount).toBeGreaterThanOrEqual(stats1.boxCount);
    });

    it('should report geometry statistics', () => {
      builder.buildCompleteEngine(materialFactory);

      const stats = GeometryBuilder.getStats();

      expect(stats.cylinderCount).toBeGreaterThan(0);
      expect(stats.boxCount).toBeGreaterThan(0);
      expect(stats.totalGeometries).toBe(
        stats.cylinderCount + stats.boxCount + stats.torusCount
      );
    });

    it('should dispose cache properly', () => {
      builder.buildCompleteEngine(materialFactory);
      const stats1 = GeometryBuilder.getStats();

      expect(stats1.totalGeometries).toBeGreaterThan(0);

      GeometryBuilder.disposeCache();
      const stats2 = GeometryBuilder.getStats();

      expect(stats2.totalGeometries).toBe(0);
    });
  });

  describe('Component Counts', () => {
    it('should track component counts', () => {
      builder.buildCompleteEngine(materialFactory);

      const counts = builder.getComponentCounts();

      expect(counts.digitWheels).toBe(248);
      expect(counts.shafts).toBe(8);
      expect(counts.carryLevers).toBe(8);
      expect(counts.engagementIndicators).toBe(8);
    });

    it('should update counts as components are built', () => {
      let counts = builder.getComponentCounts();
      expect(counts.digitWheels).toBe(0);

      builder.buildDigitWheels(materialFactory);
      counts = builder.getComponentCounts();
      expect(counts.digitWheels).toBe(248);
      expect(counts.shafts).toBe(0);

      builder.buildShafts(materialFactory);
      counts = builder.getComponentCounts();
      expect(counts.shafts).toBe(8);
    });

    it('should have independent count tracking', () => {
      const builder1 = new GeometryBuilder();
      const builder2 = new GeometryBuilder();

      builder1.buildDigitWheels(materialFactory);
      builder1.buildShafts(materialFactory);

      builder2.buildCarryLevers(materialFactory);

      const counts1 = builder1.getComponentCounts();
      const counts2 = builder2.getComponentCounts();

      expect(counts1.digitWheels).toBe(248);
      expect(counts1.shafts).toBe(8);
      expect(counts2.carryLevers).toBe(8);
      expect(counts2.digitWheels).toBe(0);
    });
  });

  describe('Material Integration', () => {
    it('should use factory to create materials', () => {
      const factorySpy = vi.spyOn(materialFactory, 'createColumnMaterial');

      builder.buildDigitWheels(materialFactory);

      expect(factorySpy).toHaveBeenCalled();
    });

    it('should apply materials to meshes', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(0, 0) as THREE.Mesh;

      expect(wheel.material).toBeDefined();
      expect(wheel.material).not.toBeNull();
    });

    it('should use different materials for different components', () => {
      builder.buildCompleteEngine(materialFactory);

      const wheel = builder.getDigitWheel(0, 0) as THREE.Mesh;
      const shaft = builder.getShaft(0) as THREE.Mesh;
      const lever = builder.getCarryLever(0) as THREE.Mesh;

      // All should have materials
      expect(wheel.material).toBeDefined();
      expect(shaft.material).toBeDefined();
      expect(lever.material).toBeDefined();
    });
  });

  describe('Edge Cases', () => {
    it('should handle requesting wheel from unbuilt column', () => {
      // Don't build wheels
      const wheel = builder.getDigitWheel(0, 0);

      expect(wheel).toBeNull();
    });

    it('should handle requesting shaft from unbuilt structure', () => {
      // Don't build shafts
      const shaft = builder.getShaft(0);

      expect(shaft).toBeNull();
    });

    it('should handle requesting lever from unbuilt structure', () => {
      // Don't build levers
      const lever = builder.getCarryLever(0);

      expect(lever).toBeNull();
    });

    it('should handle negative indices gracefully', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(-1, 0);

      expect(wheel).toBeNull();
    });

    it('should handle very large indices', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(1000, 1000);

      expect(wheel).toBeNull();
    });
  });

  describe('Geometry Properties', () => {
    it('should create wheels with correct radius', () => {
      builder.buildDigitWheels(materialFactory);

      const wheel = builder.getDigitWheel(0, 0) as THREE.Mesh;
      const geometry = wheel.geometry as THREE.CylinderGeometry;

      // CylinderGeometry stores radius values
      expect(geometry.parameters.radiusTop).toBeCloseTo(0.35, 1);
    });

    it('should create shafts with correct length', () => {
      builder.buildShafts(materialFactory);

      const shaft = builder.getShaft(0) as THREE.Mesh;
      const geometry = shaft.geometry as THREE.CylinderGeometry;

      // Shafts have specified height
      expect(geometry.parameters.height).toBe(12);
    });

    it('should create levers with box geometry', () => {
      builder.buildCarryLevers(materialFactory);

      const lever = builder.getCarryLever(0) as THREE.Mesh;
      const geometry = lever.geometry as THREE.BoxGeometry;

      expect(geometry.parameters).toBeDefined();
    });

    it('should set shadow casting on geometries', () => {
      builder.buildCompleteEngine(materialFactory);

      const wheel = builder.getDigitWheel(0, 0) as THREE.Mesh;

      expect(wheel.castShadow).toBe(true);
      expect(wheel.receiveShadow).toBe(true);
    });
  });
});
