/**
 * Unit Tests: LightingSetup
 *
 * Tests scene lighting configuration including ambient, directional,
 * and point lights. Verifies shadow setup and light intensity control.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import * as THREE from 'three';
import { LightingSetup } from './LightingSetup';

describe('LightingSetup', () => {
  let scene: THREE.Scene;
  let lighting: LightingSetup;

  beforeEach(() => {
    scene = new THREE.Scene();
    lighting = new LightingSetup(scene);
  });

  afterEach(() => {
    lighting.dispose();
  });

  describe('Initialization', () => {
    it('should create LightingSetup instance', () => {
      expect(lighting).toBeDefined();
    });

    it('should add ambient light to scene', () => {
      const light = lighting.getLight('ambient');
      expect(light).toBeInstanceOf(THREE.AmbientLight);
      expect(scene.children).toContain(light);
    });

    it('should add main directional light to scene', () => {
      const light = lighting.getLight('mainDirectional');
      expect(light).toBeInstanceOf(THREE.DirectionalLight);
      expect(scene.children).toContain(light);
    });

    it('should add fill directional light to scene', () => {
      const light = lighting.getLight('fillDirectional');
      expect(light).toBeInstanceOf(THREE.DirectionalLight);
      expect(scene.children).toContain(light);
    });
  });

  describe('Default Configuration', () => {
    it('should have default ambient intensity', () => {
      const intensity = lighting.getAmbientIntensity();
      expect(intensity).toBeGreaterThan(0);
      expect(intensity).toBeLessThan(1);
    });

    it('should have default main directional intensity', () => {
      const intensity = lighting.getMainDirectionalIntensity();
      expect(intensity).toBeGreaterThan(0);
      expect(intensity).toBeLessThan(2);
    });

    it('should have default fill directional intensity', () => {
      const intensity = lighting.getFillDirectionalIntensity();
      expect(intensity).toBeGreaterThan(0);
    });
  });

  describe('Light Control', () => {
    it('should update ambient light intensity', () => {
      const newIntensity = 0.6;
      lighting.setAmbientIntensity(newIntensity);

      expect(lighting.getAmbientIntensity()).toBeCloseTo(newIntensity);
    });

    it('should update main directional intensity', () => {
      const newIntensity = 0.7;
      lighting.setMainDirectionalIntensity(newIntensity);

      expect(lighting.getMainDirectionalIntensity()).toBeCloseTo(newIntensity);
    });

    it('should update fill directional intensity', () => {
      const newIntensity = 0.5;
      lighting.setFillDirectionalIntensity(newIntensity);

      expect(lighting.getFillDirectionalIntensity()).toBeCloseTo(newIntensity);
    });
  });

  describe('Light Position', () => {
    it('should update main directional light position', () => {
      lighting.setMainDirectionalPosition(60, 50, 40);

      const light = lighting.getLight('mainDirectional') as THREE.DirectionalLight;
      expect(light.position.x).toBeCloseTo(60);
      expect(light.position.y).toBeCloseTo(50);
      expect(light.position.z).toBeCloseTo(40);
    });

    it('should update fill directional light position', () => {
      lighting.setFillDirectionalPosition(-40, 30, -60);

      const light = lighting.getLight('fillDirectional') as THREE.DirectionalLight;
      expect(light.position.x).toBeCloseTo(-40);
      expect(light.position.y).toBeCloseTo(30);
      expect(light.position.z).toBeCloseTo(-60);
    });
  });

  describe('Point Lights', () => {
    it('should add a point light', () => {
      const position = new THREE.Vector3(10, 20, 30);
      const light = lighting.addPointLight('test-light', position, 0xffffff, 1.0);

      expect(light).toBeInstanceOf(THREE.PointLight);
      expect(scene.children).toContain(light);
    });

    it('should add multiple point lights', () => {
      lighting.addPointLight('light1', new THREE.Vector3(10, 10, 10), 0xff0000, 1.0);
      lighting.addPointLight('light2', new THREE.Vector3(20, 20, 20), 0x00ff00, 1.0);

      const light1 = lighting.getLight('light1');
      const light2 = lighting.getLight('light2');

      expect(light1).toBeDefined();
      expect(light2).toBeDefined();
    });

    it('should retrieve added point light', () => {
      const position = new THREE.Vector3(5, 10, 15);
      lighting.addPointLight('accent', position, 0xffffff, 0.8);

      const light = lighting.getLight('accent');
      expect(light).toBeInstanceOf(THREE.PointLight);
    });
  });

  describe('Phase-based Lighting', () => {
    it('should adjust lighting for IDLE phase', () => {
      lighting.setMainDirectionalIntensity(0.5);
      lighting.updateForPhase('IDLE');

      expect(lighting.getMainDirectionalIntensity()).toBeCloseTo(0.8);
    });

    it('should adjust lighting for ADDITION phase', () => {
      lighting.updateForPhase('ADDITION');

      const mainIntensity = lighting.getMainDirectionalIntensity();
      expect(mainIntensity).toBeGreaterThanOrEqual(0.8);
    });

    it('should adjust lighting for OUTPUT phase', () => {
      lighting.updateForPhase('OUTPUT');

      const mainIntensity = lighting.getMainDirectionalIntensity();
      expect(mainIntensity).toBeGreaterThanOrEqual(0.9);
    });
  });

  describe('Shadows', () => {
    it('should support shadow configuration', () => {
      const lightingWithShadows = new LightingSetup(scene, {
        enableShadows: true
      });

      const mainLight = lightingWithShadows.getLight('mainDirectional') as THREE.DirectionalLight;
      expect(mainLight.castShadow).toBe(true);

      lightingWithShadows.dispose();
    });

    it('should disable shadows when configured', () => {
      const lightingNoShadows = new LightingSetup(scene, {
        enableShadows: false
      });

      const mainLight = lightingNoShadows.getLight('mainDirectional') as THREE.DirectionalLight;
      expect(mainLight.castShadow).toBe(false);

      lightingNoShadows.dispose();
    });

    it('should toggle shadows on/off', () => {
      lighting.setShadowsEnabled(false);

      const mainLight = lighting.getLight('mainDirectional') as THREE.DirectionalLight;
      expect(mainLight.castShadow).toBe(false);

      lighting.setShadowsEnabled(true);
      expect(mainLight.castShadow).toBe(true);
    });
  });

  describe('State Snapshot', () => {
    it('should create lighting snapshot', () => {
      const snapshot = lighting.getSnapshot();

      expect(snapshot.ambientIntensity).toBeGreaterThan(0);
      expect(snapshot.mainLightIntensity).toBeGreaterThan(0);
      expect(snapshot.fillLightIntensity).toBeGreaterThan(0);
      expect(typeof snapshot.shadowsEnabled).toBe('boolean');
    });

    it('should reflect intensity changes in snapshot', () => {
      lighting.setAmbientIntensity(0.5);

      const snapshot = lighting.getSnapshot();
      expect(snapshot.ambientIntensity).toBeCloseTo(0.5);
    });
  });

  describe('Cleanup', () => {
    it('should dispose all lights', () => {
      const lightCount = scene.children.filter((c) => c instanceof THREE.Light).length;
      expect(lightCount).toBeGreaterThan(0);

      lighting.dispose();

      const lightCountAfter = scene.children.filter((c) => c instanceof THREE.Light).length;
      expect(lightCountAfter).toBeLessThan(lightCount);
    });
  });
});
