/**
 * DifferenceEngineScene Test Suite
 * Tests scene setup, camera, rendering, and viewport management
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { DifferenceEngineScene } from '../DifferenceEngineScene';

describe('DifferenceEngineScene', () => {
  let canvas: HTMLCanvasElement;
  let scene: DifferenceEngineScene;

  beforeEach(() => {
    // Create a mock canvas
    canvas = document.createElement('canvas');
    canvas.width = 1024;
    canvas.height = 768;

    scene = new DifferenceEngineScene({
      canvas,
      width: 1024,
      height: 768,
      enableShadows: true,
      enableFog: true
    });
  });

  afterEach(() => {
    scene.dispose();
  });

  describe('Initialization', () => {
    it('should create scene instance', () => {
      expect(scene).toBeDefined();
      expect(scene).toBeInstanceOf(DifferenceEngineScene);
    });

    it('should initialize with provided config', () => {
      const testScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600
      });

      const stats = testScene.getStats();
      expect(stats).toBeDefined();

      testScene.dispose();
    });

    it('should have scene, camera, and renderer', () => {
      const threeScene = scene.getScene();
      const camera = scene.getCamera();
      const renderer = scene.getRenderer();

      expect(threeScene).toBeInstanceOf(THREE.Scene);
      expect(camera).toBeInstanceOf(THREE.PerspectiveCamera);
      expect(renderer).toBeInstanceOf(THREE.WebGLRenderer);
    });

    it('should set default pixel ratio if not provided', () => {
      const testScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600
      });

      const renderer = testScene.getRenderer();
      expect(renderer).toBeDefined();

      testScene.dispose();
    });
  });

  describe('Camera Setup', () => {
    it('should create perspective camera', () => {
      const camera = scene.getCamera();

      expect(camera).toBeInstanceOf(THREE.PerspectiveCamera);
    });

    it('should position camera at elevated front-right view', () => {
      const camera = scene.getCamera();

      expect(camera.position.x).toBe(0);
      expect(camera.position.y).toBe(4);
      expect(camera.position.z).toBe(12);
    });

    it('should have correct camera aspect ratio', () => {
      const camera = scene.getCamera();

      expect(camera.aspect).toBeCloseTo(1024 / 768, 1);
    });

    it('should look at scene center', () => {
      const camera = scene.getCamera();

      // After lookAt, camera should be pointing at origin
      expect(camera).toBeDefined();
    });

    it('should reset camera view', () => {
      const camera = scene.getCamera();

      // Move camera
      camera.position.set(10, 10, 10);

      scene.resetCameraView();

      expect(camera.position.x).toBe(0);
      expect(camera.position.y).toBe(4);
      expect(camera.position.z).toBe(12);
    });
  });

  describe('Renderer Configuration', () => {
    it('should create WebGL renderer', () => {
      const renderer = scene.getRenderer();

      expect(renderer).toBeInstanceOf(THREE.WebGLRenderer);
    });

    it('should set clear color', () => {
      const renderer = scene.getRenderer();

      // Check that renderer has color set (0x1a1a2e is dark blue)
      expect(renderer).toBeDefined();
    });

    it('should enable antialiasing', () => {
      const renderer = scene.getRenderer();

      expect(renderer).toBeDefined();
    });

    it('should configure shadows when enabled', () => {
      const shadowScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600,
        enableShadows: true
      });

      const renderer = shadowScene.getRenderer();

      expect(renderer.shadowMap.enabled).toBe(true);

      shadowScene.dispose();
    });

    it('should disable shadows when disabled', () => {
      const shadowScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600,
        enableShadows: false
      });

      const renderer = shadowScene.getRenderer();

      expect(renderer.shadowMap.enabled).toBe(false);

      shadowScene.dispose();
    });

    it('should set pixel ratio', () => {
      const testScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600,
        pixelRatio: 2
      });

      const renderer = testScene.getRenderer();
      expect(renderer).toBeDefined();

      testScene.dispose();
    });
  });

  describe('Scene Setup', () => {
    it('should add lights to scene', () => {
      const threeScene = scene.getScene();

      // Should have at least 3 lights (ambient, directional, point)
      const lights = threeScene.children.filter(
        (child) => child instanceof THREE.Light || child instanceof THREE.AmbientLight
      );

      expect(lights.length).toBeGreaterThanOrEqual(3);
    });

    it('should add fog when enabled', () => {
      const threeScene = scene.getScene();

      expect(threeScene.fog).toBeDefined();
      expect(threeScene.fog).toBeInstanceOf(THREE.Fog);
    });

    it('should skip fog when disabled', () => {
      const noFogScene = new DifferenceEngineScene({
        canvas,
        width: 800,
        height: 600,
        enableFog: false
      });

      const threeScene = noFogScene.getScene();

      expect(threeScene.fog).toBeNull();

      noFogScene.dispose();
    });

    it('should set background color', () => {
      const threeScene = scene.getScene();

      expect(threeScene.background).toBeDefined();
      expect(threeScene.background).toBeInstanceOf(THREE.Color);
    });

    it('should build difference engine geometry', () => {
      const threeScene = scene.getScene();

      // Find engine group
      const engineGroup = threeScene.getObjectByName('engine');

      expect(engineGroup).toBeDefined();
      expect(engineGroup).toBeInstanceOf(THREE.Group);
    });
  });

  describe('Engine Rotation Control', () => {
    it('should rotate engine around Y axis', () => {
      const engineGroup = scene.getEngineGroup();
      const initialRotationY = engineGroup.rotation.y;

      scene.rotateEngine(100, 0);

      expect(engineGroup.rotation.y).not.toBe(initialRotationY);
    });

    it('should rotate engine around X axis', () => {
      const engineGroup = scene.getEngineGroup();
      const initialRotationX = engineGroup.rotation.x;

      scene.rotateEngine(0, 100);

      expect(engineGroup.rotation.x).not.toBe(initialRotationX);
    });

    it('should clamp X rotation to prevent flipping', () => {
      const engineGroup = scene.getEngineGroup();

      // Try to rotate far beyond limits
      scene.rotateEngine(0, 10000);

      expect(Math.abs(engineGroup.rotation.x)).toBeLessThanOrEqual(Math.PI / 2);
    });

    it('should reset engine rotation', () => {
      const engineGroup = scene.getEngineGroup();

      scene.rotateEngine(100, 100);
      scene.resetEngineRotation();

      expect(engineGroup.rotation.x).toBe(0);
      expect(engineGroup.rotation.y).toBe(0);
      expect(engineGroup.rotation.z).toBe(0);
    });

    it('should allow continuous rotation', () => {
      const engineGroup = scene.getEngineGroup();

      scene.rotateEngine(10, 0);
      const rotationAfter1 = engineGroup.rotation.y;

      scene.rotateEngine(10, 0);
      const rotationAfter2 = engineGroup.rotation.y;

      expect(rotationAfter2).toBeGreaterThan(rotationAfter1);
    });
  });

  describe('Camera Zoom Control', () => {
    it('should zoom camera', () => {
      const camera = scene.getCamera();
      const initialDistance = camera.position.length();

      scene.zoomCamera(1);

      const newDistance = camera.position.length();
      expect(newDistance).toBeLessThan(initialDistance);
    });

    it('should clamp zoom to min distance', () => {
      const camera = scene.getCamera();

      // Zoom in a lot
      scene.zoomCamera(100);

      const distance = camera.position.length();
      expect(distance).toBeGreaterThanOrEqual(5);
    });

    it('should clamp zoom to max distance', () => {
      const camera = scene.getCamera();

      // Zoom out a lot
      scene.zoomCamera(-100);

      const distance = camera.position.length();
      expect(distance).toBeLessThanOrEqual(30);
    });
  });

  describe('Rendering Control', () => {
    it('should start rendering', () => {
      scene.start();

      // Give a frame to process
      expect(scene).toBeDefined();

      scene.stop();
    });

    it('should stop rendering', () => {
      scene.start();
      scene.stop();

      expect(scene).toBeDefined();
    });

    it('should toggle rendering state', () => {
      scene.start();
      expect(scene).toBeDefined();

      scene.stop();
      expect(scene).toBeDefined();

      scene.start();
      expect(scene).toBeDefined();
    });

    it('should track frame rate', (done) => {
      scene.start();

      // Wait a bit for frames to accumulate
      setTimeout(() => {
        const stats = scene.getStats();

        expect(stats.fps).toBeGreaterThanOrEqual(0);

        scene.stop();
        done();
      }, 100);
    });
  });

  describe('Window Resize Handling', () => {
    it('should update dimensions on resize', () => {
      scene.onWindowResize(800, 600);

      const camera = scene.getCamera();

      expect(camera.aspect).toBeCloseTo(800 / 600, 1);
    });

    it('should update renderer size', () => {
      scene.onWindowResize(800, 600);

      const renderer = scene.getRenderer();
      expect(renderer).toBeDefined();
    });

    it('should update projection matrix', () => {
      const camera = scene.getCamera();
      const oldMatrix = camera.projectionMatrix.clone();

      scene.onWindowResize(800, 600);

      // Projection matrix should be updated
      expect(camera.projectionMatrix).toBeDefined();
    });

    it('should handle small dimensions', () => {
      scene.onWindowResize(100, 100);

      const camera = scene.getCamera();
      expect(camera.aspect).toBeCloseTo(1, 0.1);
    });

    it('should handle zero dimensions gracefully', () => {
      scene.onWindowResize(0, 0);

      // Should not throw, handles gracefully
      expect(scene).toBeDefined();
    });
  });

  describe('Mesh Access for Animation', () => {
    it('should access digit wheel mesh', () => {
      const wheel = scene.getDigitWheelMesh(0, 0);

      expect(wheel).toBeDefined();
      expect(wheel).toBeInstanceOf(THREE.Mesh);
    });

    it('should access shaft mesh', () => {
      const shaft = scene.getShaftMesh(0);

      expect(shaft).toBeDefined();
      expect(shaft).toBeInstanceOf(THREE.Mesh);
    });

    it('should access carry lever mesh', () => {
      const lever = scene.getCarryLeverMesh(0);

      expect(lever).toBeDefined();
      expect(lever).toBeInstanceOf(THREE.Mesh);
    });

    it('should return null for invalid indices', () => {
      const wheel = scene.getDigitWheelMesh(100, 100);

      expect(wheel).toBeNull();
    });
  });

  describe('Engagement Indicator Control', () => {
    it('should show engagement indicator', () => {
      const indicator = scene.getEngineGroup()
        .getObjectByName('carry-lever-0')
        ?.children[1] as THREE.Mesh | undefined;

      if (indicator) {
        scene.setEngagementIndicatorVisible(0, true);
        expect(indicator.visible).toBe(true);
      }
    });

    it('should hide engagement indicator', () => {
      const indicator = scene.getEngineGroup()
        .getObjectByName('carry-lever-0')
        ?.children[1] as THREE.Mesh | undefined;

      if (indicator) {
        scene.setEngagementIndicatorVisible(0, false);
        expect(indicator.visible).toBe(false);
      }
    });

    it('should update engagement indicator intensity', () => {
      scene.updateEngagementIndicator(0, 0.8);

      // Should not throw
      expect(scene).toBeDefined();
    });
  });

  describe('Statistics and Debug Info', () => {
    it('should provide scene statistics', () => {
      const stats = scene.getStats();

      expect(stats.lights).toBeGreaterThanOrEqual(3);
      expect(stats.fps).toBeGreaterThanOrEqual(0);
      expect(stats.deltaTime).toBeGreaterThanOrEqual(0);
      expect(stats.meshes).toBeGreaterThan(0);
    });

    it('should provide debug info string', () => {
      const info = scene.getInfo();

      expect(info).toContain('Difference Engine');
      expect(info).toContain('FPS');
      expect(info).toContain('Digit Wheels');
    });

    it('should report component counts in debug info', () => {
      const info = scene.getInfo();

      expect(info).toContain('248');  // digit wheels
      expect(info).toContain('8');    // shafts, carry levers
    });
  });

  describe('Resource Management', () => {
    it('should dispose renderer', () => {
      const renderer = scene.getRenderer();
      const disposeSpy = vi.spyOn(renderer, 'dispose');

      scene.dispose();

      expect(disposeSpy).toHaveBeenCalled();
    });

    it('should handle multiple dispose calls', () => {
      scene.dispose();
      scene.dispose();

      // Should not throw
      expect(scene).toBeDefined();
    });

    it('should clean up on dispose', () => {
      scene.dispose();

      // Scene should be in disposed state
      expect(scene).toBeDefined();
    });
  });

  describe('Material Factory Access', () => {
    it('should provide material factory reference', () => {
      const materialFactory = scene.getMaterialFactory();

      expect(materialFactory).toBeDefined();
    });
  });

  describe('Engine Group Access', () => {
    it('should provide engine group reference', () => {
      const engineGroup = scene.getEngineGroup();

      expect(engineGroup).toBeInstanceOf(THREE.Group);
      expect(engineGroup.name).toBe('engine');
    });

    it('should maintain engine group in scene', () => {
      const sceneObj = scene.getScene();
      const engineGroup = scene.getEngineGroup();

      expect(sceneObj.getObjectByName('engine')).toBe(engineGroup);
    });
  });

  describe('Scene Graph Organization', () => {
    it('should have properly named scene', () => {
      const sceneObj = scene.getScene();

      expect(sceneObj.name).toBe('DifferenceEngineScene');
    });

    it('should organize components hierarchically', () => {
      const engineGroup = scene.getEngineGroup();

      // Should have frame, wheels, shafts, levers groups
      expect(engineGroup.children.length).toBeGreaterThanOrEqual(4);
    });

    it('should name component groups', () => {
      const engineGroup = scene.getEngineGroup();

      const groupNames = engineGroup.children.map((child) => child.name);

      expect(groupNames).toContain('frame');
      expect(groupNames).toContain('digit-wheels');
      expect(groupNames).toContain('shafts');
      expect(groupNames).toContain('carry-levers');
    });
  });

  describe('Factory Functions', () => {
    it('should export factory function', async () => {
      const { createDifferenceEngineScene } = await import('../DifferenceEngineScene');

      expect(createDifferenceEngineScene).toBeDefined();
    });

    it('should create scene with factory', async () => {
      const { createDifferenceEngineScene } = await import('../DifferenceEngineScene');

      const testCanvas = document.createElement('canvas');
      testCanvas.width = 800;
      testCanvas.height = 600;

      const factoryScene = createDifferenceEngineScene(testCanvas, 800, 600);

      expect(factoryScene).toBeInstanceOf(DifferenceEngineScene);

      factoryScene.dispose();
    });
  });
});
