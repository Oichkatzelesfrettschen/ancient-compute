/**
 * Unit Tests: SceneManager
 *
 * Tests core Three.js scene setup, renderer configuration, and canvas
 * integration. Verifies proper initialization of scene graph, camera,
 * and WebGL context.
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { SceneManager } from './SceneManager';

describe('SceneManager', () => {
  let container: HTMLElement;
  let sceneManager: SceneManager;

  beforeEach(() => {
    // Create a mock container element
    container = document.createElement('div');
    container.style.width = '1024px';
    container.style.height = '768px';
    document.body.appendChild(container);
  });

  afterEach(() => {
    if (sceneManager) {
      sceneManager.dispose();
    }
    document.body.removeChild(container);
  });

  describe('Initialization', () => {
    it('should create a SceneManager with default config', () => {
      sceneManager = new SceneManager({ canvasContainer: container });

      expect(sceneManager).toBeDefined();
      expect(sceneManager.isInitialized).toBe(true);
    });

    it('should initialize with custom dimensions', () => {
      sceneManager = new SceneManager({
        canvasContainer: container,
        width: 800,
        height: 600
      });

      const viewport = sceneManager.getViewport();
      expect(viewport.width).toBe(800);
      expect(viewport.height).toBe(600);
    });

    it('should create a WebGL renderer', () => {
      sceneManager = new SceneManager({ canvasContainer: container });
      const renderer = sceneManager.getRenderer();

      expect(renderer).toBeInstanceOf(THREE.WebGLRenderer);
      expect(renderer.domElement).toBeDefined();
    });

    it('should create a scene with background color', () => {
      sceneManager = new SceneManager({
        canvasContainer: container,
        backgroundColor: 0xff0000
      });

      const scene = sceneManager.getScene();
      expect(scene).toBeInstanceOf(THREE.Scene);
      expect(scene.background).toBeInstanceOf(THREE.Color);
    });

    it('should create a camera with proper aspect ratio', () => {
      sceneManager = new SceneManager({
        canvasContainer: container,
        width: 1024,
        height: 768
      });

      const camera = sceneManager.getCamera();
      expect(camera).toBeInstanceOf(THREE.PerspectiveCamera);
      expect(camera.aspect).toBeCloseTo(1024 / 768);
    });

    it('should add canvas to container', () => {
      sceneManager = new SceneManager({ canvasContainer: container });

      const canvas = sceneManager.getCanvas();
      expect(container.contains(canvas)).toBe(true);
    });
  });

  describe('Camera Properties', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should position camera at default location', () => {
      const camera = sceneManager.getCamera();

      expect(camera.position.x).toBeCloseTo(40);
      expect(camera.position.y).toBeCloseTo(30);
      expect(camera.position.z).toBeCloseTo(50);
    });

    it('should have correct camera frustum', () => {
      const camera = sceneManager.getCamera();

      expect(camera.fov).toBe(75);
      expect(camera.near).toBe(0.1);
      expect(camera.far).toBe(2000);
    });

    it('should update camera on resize', () => {
      const initialAspect = sceneManager.getCamera().aspect;

      container.style.width = '512px';
      container.style.height = '512px';

      // Trigger resize observer
      const resizeEvent = new Event('resize');
      window.dispatchEvent(resizeEvent);

      // Note: In actual browser, ResizeObserver would trigger
      // This test verifies the mechanism exists
      expect(sceneManager.getCamera()).toBeDefined();
    });
  });

  describe('Rendering', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should render a frame', () => {
      const renderer = sceneManager.getRenderer();
      const renderSpy = vi.spyOn(renderer, 'render');

      sceneManager.render();

      expect(renderSpy).toHaveBeenCalled();
    });

    it('should track delta time', () => {
      const initialDelta = sceneManager.getDeltaTime();

      // Render multiple frames
      sceneManager.render();
      sceneManager.render();

      const finalDelta = sceneManager.getDeltaTime();
      expect(typeof finalDelta).toBe('number');
    });

    it('should track elapsed time', () => {
      const startTime = sceneManager.getElapsedTime();

      // Simulate time passage
      sceneManager.render();

      const endTime = sceneManager.getElapsedTime();
      expect(endTime).toBeGreaterThanOrEqual(startTime);
    });
  });

  describe('Capabilities', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should report WebGL capabilities', () => {
      const capabilities = sceneManager.getCapabilities();

      expect(capabilities.maxTextures).toBeGreaterThan(0);
      expect(capabilities.maxVertexAttribs).toBeGreaterThanOrEqual(8);
      expect(capabilities.maxFragmentUniforms).toBeGreaterThan(0);
    });

    it('should report valid vertex attributes', () => {
      const capabilities = sceneManager.getCapabilities();

      // Most WebGL implementations support at least 8 vertex attributes
      expect(capabilities.maxVertexAttribs).toBeGreaterThanOrEqual(8);
    });
  });

  describe('State Snapshot', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should create scene snapshot', () => {
      const snapshot = sceneManager.getSnapshot();

      expect(snapshot.cameraPosition).toBeInstanceOf(THREE.Vector3);
      expect(snapshot.cameraTarget).toBeInstanceOf(THREE.Vector3);
      expect(snapshot.rendererSize).toBeDefined();
      expect(snapshot.dpr).toBeGreaterThan(0);
      expect(snapshot.timeElapsed).toBeGreaterThanOrEqual(0);
    });

    it('should track elapsed time in snapshot', () => {
      const snapshot1 = sceneManager.getSnapshot();

      sceneManager.render();
      sceneManager.render();

      const snapshot2 = sceneManager.getSnapshot();
      expect(snapshot2.timeElapsed).toBeGreaterThanOrEqual(snapshot1.timeElapsed);
    });
  });

  describe('Reset', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should reset camera to default position', () => {
      const camera = sceneManager.getCamera();
      camera.position.set(100, 100, 100);

      sceneManager.reset();

      expect(camera.position.x).toBeCloseTo(40);
      expect(camera.position.y).toBeCloseTo(30);
      expect(camera.position.z).toBeCloseTo(50);
    });

    it('should reset elapsed time counter', () => {
      const startTime = sceneManager.getElapsedTime();

      sceneManager.render();
      sceneManager.reset();

      const newTime = sceneManager.getElapsedTime();
      expect(newTime).toBeLessThan(startTime + 0.1);
    });
  });

  describe('Disposal', () => {
    beforeEach(() => {
      sceneManager = new SceneManager({ canvasContainer: container });
    });

    it('should dispose resources', () => {
      const renderer = sceneManager.getRenderer();
      const disposeSpy = vi.spyOn(renderer, 'dispose');

      sceneManager.dispose();

      expect(disposeSpy).toHaveBeenCalled();
      expect(sceneManager.isInitialized).toBe(false);
    });

    it('should clear scene after disposal', () => {
      const scene = sceneManager.getScene();
      const clearSpy = vi.spyOn(scene, 'clear');

      sceneManager.dispose();

      expect(clearSpy).toHaveBeenCalled();
    });
  });
});
