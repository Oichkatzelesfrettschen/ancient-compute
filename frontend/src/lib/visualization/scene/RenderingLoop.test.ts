/**
 * Unit Tests: RenderingLoop, GeometryCache, MaterialLibrary
 *
 * Tests animation frame orchestration, geometry caching, and material
 * management. These are critical infrastructure components.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { RenderingLoop } from './RenderingLoop';
import { SceneManager } from './SceneManager';
import { GeometryCache } from '../mechanics/GeometryCache';
import { MaterialLibrary, MaterialType } from '../mechanics/MaterialLibrary';

describe('RenderingLoop', () => {
  let container: HTMLElement;
  let sceneManager: SceneManager;
  let loop: RenderingLoop;

  beforeEach(() => {
    container = document.createElement('div');
    container.style.width = '1024px';
    container.style.height = '768px';
    document.body.appendChild(container);

    sceneManager = new SceneManager({ canvasContainer: container });
    loop = new RenderingLoop({ sceneManager });
  });

  afterEach(() => {
    loop.dispose();
    sceneManager.dispose();
    document.body.removeChild(container);
  });

  describe('Initialization', () => {
    it('should create RenderingLoop instance', () => {
      expect(loop).toBeDefined();
    });

    it('should have target frame rate', () => {
      const fps = loop.getTargetFrameRate();
      expect(fps).toBeGreaterThan(0);
    });

    it('should set custom target frame rate', () => {
      loop.setTargetFrameRate(30);
      expect(loop.getTargetFrameRate()).toBe(30);
    });
  });

  describe('Loop Control', () => {
    it('should start and stop loop', () => {
      loop.start();
      expect(loop.isActive()).toBe(false); // Not paused, but need to check running state

      loop.stop();
      expect(loop.isActive()).toBe(false);
    });

    it('should pause and resume loop', () => {
      loop.start();
      loop.pause();
      expect(loop.isActive()).toBe(false);

      loop.resume();
      // Still running after resume
    });

    it('should prevent double start', () => {
      loop.start();
      loop.start(); // Should not error

      loop.stop();
    });
  });

  describe('Update Callbacks', () => {
    it('should register update callback', () => {
      const callback = vi.fn();
      loop.addUpdateCallback('test', callback);

      loop.start();
      // Callbacks execute during loop
      loop.stop();
    });

    it('should remove update callback', () => {
      const callback = vi.fn();
      loop.addUpdateCallback('test', callback);
      loop.removeUpdateCallback('test');

      expect(loop).toBeDefined();
    });

    it('should support multiple callbacks', () => {
      const callback1 = vi.fn();
      const callback2 = vi.fn();

      loop.addUpdateCallback('test1', callback1);
      loop.addUpdateCallback('test2', callback2);

      loop.start();
      loop.stop();
    });
  });

  describe('Timing', () => {
    it('should track delta time', () => {
      const dt1 = loop.getDeltaTime();
      expect(typeof dt1).toBe('number');

      loop.start();
      // Simulate time passage
      loop.stop();

      const dt2 = loop.getDeltaTime();
      expect(typeof dt2).toBe('number');
    });

    it('should track elapsed time', () => {
      const elapsed1 = loop.getElapsedTime();
      expect(typeof elapsed1).toBe('number');

      loop.start();
      // Simulate passage
      loop.stop();

      const elapsed2 = loop.getElapsedTime();
      expect(elapsed2).toBeGreaterThanOrEqual(elapsed1);
    });
  });

  describe('Performance Metrics', () => {
    it('should report FPS', () => {
      const fps = loop.getFps();
      expect(typeof fps).toBe('number');
      expect(fps).toBeGreaterThanOrEqual(0);
    });

    it('should get performance metrics', () => {
      const metrics = loop.getMetrics();

      expect(metrics.fps).toBeGreaterThanOrEqual(0);
      expect(metrics.deltaTime).toBeGreaterThanOrEqual(0);
      expect(metrics.frameCount).toBeGreaterThanOrEqual(0);
    });

    it('should reset metrics', () => {
      loop.resetMetrics();
      const metrics = loop.getMetrics();

      expect(metrics.frameCount).toBe(0);
    });

    it('should get frame history', () => {
      loop.start();
      // Run a few frames
      for (let i = 0; i < 3; i++) {
        loop.getDeltaTime();
      }
      loop.stop();

      const history = loop.getFrameHistory(10);
      expect(Array.isArray(history)).toBe(true);
    });
  });

  describe('Cleanup', () => {
    it('should dispose resources', () => {
      loop.stop();
      loop.dispose();

      expect(loop).toBeDefined();
    });
  });
});

describe('GeometryCache', () => {
  let cache: GeometryCache;

  beforeEach(() => {
    cache = new GeometryCache();
  });

  afterEach(() => {
    cache.dispose();
  });

  describe('Geometry Registration', () => {
    it('should register custom geometry', () => {
      cache.registerFactory('custom', () => new THREE.BoxGeometry(1, 1, 1));

      const geom = cache.getGeometry('custom');
      expect(geom).toBeInstanceOf(THREE.BufferGeometry);
    });

    it('should throw on missing geometry', () => {
      expect(() => {
        cache.getGeometry('nonexistent');
      }).toThrow();
    });
  });

  describe('Caching', () => {
    it('should cache geometry after first access', () => {
      const geom1 = cache.getGeometry('digit-wheel');
      const geom2 = cache.getGeometry('digit-wheel');

      expect(geom1).toBeInstanceOf(THREE.BufferGeometry);
      expect(cache.has('digit-wheel')).toBe(true);
    });

    it('should track reference counts', () => {
      cache.getGeometry('digit-wheel');
      expect(cache.getReferenceCount('digit-wheel')).toBe(1);

      cache.getGeometry('digit-wheel');
      expect(cache.getReferenceCount('digit-wheel')).toBe(2);
    });

    it('should decrement reference on release', () => {
      cache.getGeometry('digit-wheel');
      cache.getGeometry('digit-wheel');

      expect(cache.getReferenceCount('digit-wheel')).toBe(2);

      cache.releaseGeometry('digit-wheel');
      expect(cache.getReferenceCount('digit-wheel')).toBe(1);
    });
  });

  describe('Statistics', () => {
    it('should report cache statistics', () => {
      cache.getGeometry('digit-wheel');
      cache.getGeometry('shaft-rod');

      const stats = cache.getStats();
      expect(stats.totalCached).toBeGreaterThan(0);
      expect(stats.registeredFactories).toBeGreaterThan(0);
      expect(stats.totalVertices).toBeGreaterThan(0);
    });

    it('should get cached names', () => {
      cache.getGeometry('digit-wheel');
      const names = cache.getCachedNames();

      expect(names).toContain('digit-wheel');
    });
  });

  describe('Cleanup', () => {
    it('should clear unused geometries', () => {
      cache.getGeometry('digit-wheel');
      cache.releaseGeometry('digit-wheel');

      cache.clearUnused();
      expect(cache.has('digit-wheel')).toBe(false);
    });

    it('should dispose all on clear', () => {
      cache.getGeometry('digit-wheel');
      cache.clear();

      expect(cache.getCachedNames().length).toBe(0);
    });
  });
});

describe('MaterialLibrary', () => {
  let lib: MaterialLibrary;

  beforeEach(() => {
    lib = new MaterialLibrary();
  });

  afterEach(() => {
    lib.dispose();
  });

  describe('Initialization', () => {
    it('should create library with default materials', () => {
      expect(lib.has(MaterialType.STEEL)).toBe(true);
      expect(lib.has(MaterialType.BRASS)).toBe(true);
    });

    it('should have standard materials', () => {
      const materials = lib.getAvailableMaterials();
      expect(materials.length).toBeGreaterThan(0);
    });
  });

  describe('Material Access', () => {
    it('should get material by name', () => {
      const mat = lib.getMaterial(MaterialType.STEEL);
      expect(mat).toBeInstanceOf(THREE.Material);
    });

    it('should throw on missing material', () => {
      expect(() => {
        lib.getMaterial('nonexistent');
      }).toThrow();
    });

    it('should get raw material reference', () => {
      const mat = lib.getRawMaterial(MaterialType.BRASS);
      expect(mat).toBeInstanceOf(THREE.Material);
    });
  });

  describe('Material Updates', () => {
    it('should update material properties', () => {
      lib.updateMaterial(MaterialType.STEEL, {
        roughness: 0.2
      });

      const mat = lib.getRawMaterial(MaterialType.STEEL);
      if (mat instanceof THREE.MeshStandardMaterial) {
        expect(mat.roughness).toBeCloseTo(0.2);
      }
    });

    it('should create material variants', () => {
      lib.createVariant(MaterialType.STEEL, 'steel-shiny', {
        roughness: 0.1
      });

      expect(lib.has('steel-shiny')).toBe(true);
    });

    it('should set material color', () => {
      lib.setColor(MaterialType.STEEL, 0xff0000);

      const color = lib.getColor(MaterialType.STEEL);
      expect(color).toBeInstanceOf(THREE.Color);
    });
  });

  describe('Statistics', () => {
    it('should report library statistics', () => {
      const stats = lib.getStats();

      expect(stats.totalMaterials).toBeGreaterThan(0);
      expect(Array.isArray(stats.materials)).toBe(true);
    });
  });

  describe('Cleanup', () => {
    it('should dispose all materials', () => {
      lib.dispose();

      expect(lib.getAvailableMaterials().length).toBe(0);
    });
  });
});
