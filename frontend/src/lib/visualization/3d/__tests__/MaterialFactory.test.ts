/**
 * MaterialFactory Test Suite
 * Tests material creation, caching, property updates, and disposal
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { MaterialFactory, MaterialPalette } from '../MaterialFactory';

describe('MaterialFactory', () => {
  let factory: MaterialFactory;

  beforeEach(() => {
    factory = new MaterialFactory();
  });

  afterEach(() => {
    factory.disposeAll();
  });

  describe('Color Palette', () => {
    it('should define correct column blue color', () => {
      expect(MaterialPalette.columnBlue).toEqual(new THREE.Color(0x4a9eff));
    });

    it('should define correct shaft red color', () => {
      expect(MaterialPalette.shaftRed).toEqual(new THREE.Color(0xff6b6b));
    });

    it('should define correct carry gold color', () => {
      expect(MaterialPalette.carryGold).toEqual(new THREE.Color(0xffc107));
    });

    it('should define correct frame gray color', () => {
      expect(MaterialPalette.frameGray).toEqual(new THREE.Color(0x888888));
    });

    it('should define correct highlight green color', () => {
      expect(MaterialPalette.highlightGreen).toEqual(new THREE.Color(0x4caf50));
    });

    it('should define lighting colors', () => {
      expect(MaterialPalette.lightAmbient).toBe(0x404040);
      expect(MaterialPalette.shadowDarkness).toBe(0.5);
    });

    it('should define emission colors', () => {
      expect(MaterialPalette.carryEmission).toBe(0x1a1a1a);
      expect(MaterialPalette.highlightEmission).toBe(0x004d00);
    });
  });

  describe('Column Material Creation', () => {
    it('should create column material with correct properties', () => {
      const material = factory.createColumnMaterial(0);

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).color).toEqual(MaterialPalette.columnBlue);
      expect((material as THREE.MeshPhongMaterial).shininess).toBe(100);
      expect((material as THREE.MeshPhongMaterial).transparent).toBe(false);
      expect((material as THREE.MeshPhongMaterial).wireframe).toBe(false);
    });

    it('should cache column materials', () => {
      const material1 = factory.createColumnMaterial(0);
      const material2 = factory.createColumnMaterial(0);

      expect(material1).toBe(material2);
    });

    it('should create different materials for different columns', () => {
      const material0 = factory.createColumnMaterial(0);
      const material1 = factory.createColumnMaterial(1);

      // Different instances but same color (shared palette)
      expect(material0.color).toEqual(material1.color);
    });

    it('should handle 8 columns', () => {
      const materials: THREE.Material[] = [];
      for (let i = 0; i < 8; i++) {
        materials.push(factory.createColumnMaterial(i));
      }

      expect(materials).toHaveLength(8);
      materials.forEach((m) => {
        expect(m).toBeInstanceOf(THREE.MeshPhongMaterial);
      });
    });
  });

  describe('Shaft Material Creation', () => {
    it('should create shaft material with correct properties', () => {
      const material = factory.createShaftMaterial(0);

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).color).toEqual(MaterialPalette.shaftRed);
      expect((material as THREE.MeshPhongMaterial).shininess).toBe(150);
    });

    it('should cache shaft materials', () => {
      const material1 = factory.createShaftMaterial(0);
      const material2 = factory.createShaftMaterial(0);

      expect(material1).toBe(material2);
    });

    it('should have higher shininess than columns', () => {
      const columnMat = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;
      const shaftMat = factory.createShaftMaterial(0) as THREE.MeshPhongMaterial;

      expect(shaftMat.shininess).toBeGreaterThan(columnMat.shininess);
    });
  });

  describe('Carry Lever Material Creation', () => {
    it('should create carry lever material', () => {
      const material = factory.createCarryLeverMaterial(0);

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).color).toEqual(MaterialPalette.carryGold);
      expect((material as THREE.MeshPhongMaterial).shininess).toBe(80);
    });

    it('should cache carry lever materials', () => {
      const material1 = factory.createCarryLeverMaterial(0);
      const material2 = factory.createCarryLeverMaterial(0);

      expect(material1).toBe(material2);
    });

    it('should handle 8 carry levers', () => {
      const materials: THREE.Material[] = [];
      for (let i = 0; i < 8; i++) {
        materials.push(factory.createCarryLeverMaterial(i));
      }

      expect(materials).toHaveLength(8);
    });
  });

  describe('Carry Highlight Material Creation', () => {
    it('should create highlight material with transparency', () => {
      const material = factory.createCarryHighlightMaterial(0);

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).transparent).toBe(true);
      expect((material as THREE.MeshPhongMaterial).opacity).toBe(0.6);
    });

    it('should have back-side rendering', () => {
      const material = factory.createCarryHighlightMaterial(0);

      expect((material as THREE.MeshPhongMaterial).side).toBe(THREE.BackSide);
    });

    it('should have high emissive intensity for glow', () => {
      const material = factory.createCarryHighlightMaterial(0) as THREE.MeshPhongMaterial;

      expect(material.emissiveIntensity).toBe(0.8);
    });

    it('should cache highlight materials', () => {
      const material1 = factory.createCarryHighlightMaterial(0);
      const material2 = factory.createCarryHighlightMaterial(0);

      expect(material1).toBe(material2);
    });
  });

  describe('Frame Material Creation', () => {
    it('should create frame material', () => {
      const material = factory.createFrameMaterial();

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).color).toEqual(MaterialPalette.frameGray);
    });

    it('should have lower shininess than mechanical parts', () => {
      const frameMat = factory.createFrameMaterial() as THREE.MeshPhongMaterial;
      const leverMat = factory.createCarryLeverMaterial(0) as THREE.MeshPhongMaterial;

      expect(frameMat.shininess).toBeLessThan(leverMat.shininess);
    });

    it('should cache frame material', () => {
      const material1 = factory.createFrameMaterial();
      const material2 = factory.createFrameMaterial();

      expect(material1).toBe(material2);
    });
  });

  describe('Indicator Material Creation', () => {
    it('should create indicator material', () => {
      const material = factory.createIndicatorMaterial();

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).color).toEqual(new THREE.Color(0xffffff));
    });

    it('should be white with good visibility', () => {
      const material = factory.createIndicatorMaterial() as THREE.MeshPhongMaterial;

      expect(material.color.getHex()).toBe(0xffffff);
      expect(material.emissiveIntensity).toBeGreaterThan(0);
    });
  });

  describe('Debug Material Creation', () => {
    it('should create wireframe debug material', () => {
      const material = factory.createDebugMaterial();

      expect(material).toBeInstanceOf(THREE.MeshPhongMaterial);
      expect((material as THREE.MeshPhongMaterial).wireframe).toBe(true);
      expect((material as THREE.MeshPhongMaterial).transparent).toBe(true);
      expect((material as THREE.MeshPhongMaterial).opacity).toBe(0.3);
    });

    it('should accept custom color for debug material', () => {
      const color = new THREE.Color(0xff0000);
      const material = factory.createDebugMaterial(color);

      expect((material as THREE.MeshPhongMaterial).color).toEqual(color);
    });

    it('should not cache debug materials', () => {
      const material1 = factory.createDebugMaterial();
      const material2 = factory.createDebugMaterial();

      // Different instances (not cached)
      expect(material1).not.toBe(material2);
    });
  });

  describe('Material Property Updates', () => {
    it('should update emissive intensity', () => {
      const material = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;
      const initialIntensity = material.emissiveIntensity;

      factory.updateMaterialProperty(material, 'emissiveIntensity', 0.8);

      expect(material.emissiveIntensity).toBe(0.8);
      expect(material.emissiveIntensity).not.toBe(initialIntensity);
    });

    it('should update opacity', () => {
      const material = factory.createCarryHighlightMaterial(0) as THREE.MeshPhongMaterial;

      factory.updateMaterialProperty(material, 'opacity', 0.2);

      expect(material.opacity).toBe(0.2);
    });

    it('should update emissive color', () => {
      const material = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;
      const newColor = new THREE.Color(0xff0000);

      factory.updateMaterialProperty(material, 'emissive', newColor);

      expect(material.emissive).toEqual(newColor);
    });

    it('should set needsUpdate flag on property change', () => {
      const material = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;
      material.needsUpdate = false;

      factory.updateMaterialProperty(material, 'emissiveIntensity', 0.5);

      expect(material.needsUpdate).toBe(true);
    });

    it('should ignore unknown properties', () => {
      const material = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;

      factory.updateMaterialProperty(material, 'unknownProperty' as any, 0.5);

      // Should not throw, silently ignore
      expect(material).toBeDefined();
    });
  });

  describe('Material Retrieval', () => {
    it('should retrieve material by name', () => {
      const created = factory.createColumnMaterial(0);
      const retrieved = factory.getMaterial('column-0');

      expect(retrieved).toBe(created);
    });

    it('should return undefined for unknown material', () => {
      const material = factory.getMaterial('unknown-material');

      expect(material).toBeUndefined();
    });
  });

  describe('Cache Management', () => {
    it('should disable caching when disabled', () => {
      factory.setCacheEnabled(false);

      const material1 = factory.createColumnMaterial(0);
      const material2 = factory.createColumnMaterial(0);

      expect(material1).not.toBe(material2);
    });

    it('should return cached materials when cache enabled', () => {
      factory.setCacheEnabled(true);

      const material1 = factory.createColumnMaterial(0);
      const material2 = factory.createColumnMaterial(0);

      expect(material1).toBe(material2);
    });

    it('should clear cache without disposing', () => {
      const material = factory.createColumnMaterial(0);

      factory.clearCache();

      const retrieved = factory.getMaterial('column-0');
      expect(retrieved).toBeUndefined();
    });

    it('should return cached materials map', () => {
      factory.createColumnMaterial(0);
      factory.createShaftMaterial(0);

      const cached = factory.getCachedMaterials();

      expect(cached.size).toBeGreaterThan(0);
      expect(cached.has('column-0')).toBe(true);
      expect(cached.has('shaft-0')).toBe(true);
    });
  });

  describe('Disposal', () => {
    it('should dispose all materials', () => {
      const material = factory.createColumnMaterial(0) as THREE.MeshPhongMaterial;
      const disposeSpy = vi.spyOn(material, 'dispose');

      factory.disposeAll();

      expect(disposeSpy).toHaveBeenCalled();
    });

    it('should clear cache after disposal', () => {
      factory.createColumnMaterial(0);
      factory.createShaftMaterial(0);

      factory.disposeAll();

      const cached = factory.getCachedMaterials();
      expect(cached.size).toBe(0);
    });

    it('should handle disposal of multiple materials', () => {
      const materials: THREE.Material[] = [];
      for (let i = 0; i < 8; i++) {
        materials.push(factory.createColumnMaterial(i));
      }

      factory.disposeAll();

      expect(factory.getCachedMaterials().size).toBe(0);
    });
  });

  describe('Statistics', () => {
    it('should report material statistics', () => {
      factory.createColumnMaterial(0);
      factory.createShaftMaterial(0);

      const stats = factory.getStats();

      expect(stats.totalMaterials).toBeGreaterThan(0);
      expect(stats.cacheEnabled).toBe(true);
      expect(Array.isArray(stats.materialNames)).toBe(true);
    });

    it('should list material names in statistics', () => {
      factory.createColumnMaterial(0);
      factory.createShaftMaterial(0);

      const stats = factory.getStats();

      expect(stats.materialNames).toContain('column-0');
      expect(stats.materialNames).toContain('shaft-0');
    });

    it('should update statistics after creating new materials', () => {
      let stats = factory.getStats();
      const initialCount = stats.totalMaterials;

      factory.createFrameMaterial();

      stats = factory.getStats();
      expect(stats.totalMaterials).toBeGreaterThan(initialCount);
    });
  });

  describe('Global Factory Instance', () => {
    it('should export global material factory instance', () => {
      const { globalMaterialFactory } = await import('../MaterialFactory');

      expect(globalMaterialFactory).toBeInstanceOf(MaterialFactory);
    });
  });

  describe('Scene Lights Creation', () => {
    it('should create three-light setup', async () => {
      const { createSceneLights } = await import('../MaterialFactory');
      const lights = createSceneLights();

      expect(lights).toHaveLength(3);
    });

    it('should include ambient light', async () => {
      const { createSceneLights } = await import('../MaterialFactory');
      const lights = createSceneLights();

      const ambientLight = lights.find((light) => light instanceof THREE.AmbientLight);
      expect(ambientLight).toBeDefined();
    });

    it('should include directional light with shadows', async () => {
      const { createSceneLights } = await import('../MaterialFactory');
      const lights = createSceneLights();

      const dirLight = lights.find((light) => light instanceof THREE.DirectionalLight);
      expect(dirLight).toBeDefined();
      expect((dirLight as THREE.DirectionalLight).castShadow).toBe(true);
    });

    it('should include point light for accent', async () => {
      const { createSceneLights } = await import('../MaterialFactory');
      const lights = createSceneLights();

      const pointLight = lights.find((light) => light instanceof THREE.PointLight);
      expect(pointLight).toBeDefined();
    });

    it('should configure directional light shadow map', async () => {
      const { createSceneLights } = await import('../MaterialFactory');
      const lights = createSceneLights();

      const dirLight = lights.find((light) => light instanceof THREE.DirectionalLight) as THREE.DirectionalLight;
      expect(dirLight.shadow.mapSize.width).toBe(2048);
      expect(dirLight.shadow.mapSize.height).toBe(2048);
    });
  });
});
