/**
 * Material Factory: Optimized Material Creation
 *
 * Creates reusable materials for different Difference Engine components:
 * - Column digit wheels (semi-transparent with sheen)
 * - Shafts (metallic with reflections)
 * - Carry levers (mechanical look)
 * - Frame/structure (brushed metal)
 * - Carry highlights (emission glow during propagation)
 *
 * Optimized for performance with minimal shader complexity
 */

import * as THREE from 'three';

/**
 * Material color palette
 */
export const MaterialPalette = {
  // Primary colors
  columnBlue: new THREE.Color(0x4a9eff),
  shaftRed: new THREE.Color(0xff6b6b),
  carryGold: new THREE.Color(0xffc107),
  frameGray: new THREE.Color(0x888888),
  highlightGreen: new THREE.Color(0x4caf50),

  // Lighting
  lightAmbient: 0x404040, // 25% ambient brightness
  shadowDarkness: 0.5,

  // Emissive ranges
  carryEmission: 0x1a1a1a, // Dark amber emission
  highlightEmission: 0x004d00 // Dark green emission
};

/**
 * Material configuration interface
 */
export interface MaterialConfig {
  name: string;
  type: 'standard' | 'phong' | 'basic' | 'physical';
  color: THREE.Color;
  emissive?: THREE.Color;
  emissiveIntensity?: number;
  metalness?: number;
  roughness?: number;
  transparent?: boolean;
  opacity?: number;
  wireframe?: boolean;
}

/**
 * Material factory
 */
export class MaterialFactory {
  private materials: Map<string, THREE.Material> = new Map();
  private textureLoader: THREE.TextureLoader;
  private cacheEnabled: boolean = true;

  constructor() {
    this.textureLoader = new THREE.TextureLoader();
  }

  /**
   * Create column digit wheel material
   */
  createColumnMaterial(columnIndex: number): THREE.Material {
    const name = `column-${columnIndex}`;

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: MaterialPalette.columnBlue,
      emissive: new THREE.Color(0x000000),
      emissiveIntensity: 0.3,
      shininess: 100,
      side: THREE.FrontSide,
      transparent: false,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create shaft material with metallic appearance
   */
  createShaftMaterial(shaftIndex: number): THREE.Material {
    const name = `shaft-${shaftIndex}`;

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: MaterialPalette.shaftRed,
      emissive: new THREE.Color(0x330000),
      emissiveIntensity: 0.2,
      shininess: 150,
      side: THREE.FrontSide,
      transparent: false,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create carry lever material
   */
  createCarryLeverMaterial(leverIndex: number): THREE.Material {
    const name = `carry-lever-${leverIndex}`;

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: MaterialPalette.carryGold,
      emissive: MaterialPalette.carryEmission,
      emissiveIntensity: 0.1,
      shininess: 80,
      side: THREE.FrontSide,
      transparent: false,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create carry highlight material (glowing during propagation)
   */
  createCarryHighlightMaterial(leverIndex: number): THREE.Material {
    const name = `carry-highlight-${leverIndex}`;

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: MaterialPalette.highlightGreen,
      emissive: MaterialPalette.highlightEmission,
      emissiveIntensity: 0.8,
      shininess: 60,
      side: THREE.BackSide,
      transparent: true,
      opacity: 0.6,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create frame/structure material
   */
  createFrameMaterial(): THREE.Material {
    const name = 'frame-material';

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: MaterialPalette.frameGray,
      emissive: new THREE.Color(0x1a1a1a),
      emissiveIntensity: 0.2,
      shininess: 30,
      side: THREE.FrontSide,
      transparent: false,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create indicator/label material
   */
  createIndicatorMaterial(): THREE.Material {
    const name = 'indicator-material';

    if (this.cacheEnabled && this.materials.has(name)) {
      return this.materials.get(name)!;
    }

    const material = new THREE.MeshPhongMaterial({
      color: new THREE.Color(0xffffff),
      emissive: new THREE.Color(0x333333),
      emissiveIntensity: 0.5,
      shininess: 20,
      side: THREE.FrontSide,
      transparent: false,
      wireframe: false
    });

    if (this.cacheEnabled) {
      this.materials.set(name, material);
    }

    return material;
  }

  /**
   * Create debug/wireframe material
   */
  createDebugMaterial(color: THREE.Color = new THREE.Color(0x00ff00)): THREE.Material {
    return new THREE.MeshPhongMaterial({
      color,
      emissive: new THREE.Color(0x000000),
      emissiveIntensity: 0,
      shininess: 10,
      side: THREE.FrontSide,
      transparent: true,
      opacity: 0.3,
      wireframe: true
    });
  }

  /**
   * Update material property (for animations)
   */
  updateMaterialProperty(
    material: THREE.Material,
    property: string,
    value: number | THREE.Color
  ): void {
    if (material instanceof THREE.MeshPhongMaterial) {
      if (property === 'emissiveIntensity') {
        material.emissiveIntensity = value as number;
        material.needsUpdate = true;
      } else if (property === 'opacity') {
        material.opacity = value as number;
        material.needsUpdate = true;
      } else if (property === 'emissive') {
        material.emissive.copy(value as THREE.Color);
        material.needsUpdate = true;
      }
    }
  }

  /**
   * Get material by name
   */
  getMaterial(name: string): THREE.Material | undefined {
    return this.materials.get(name);
  }

  /**
   * Dispose all materials
   */
  disposeAll(): void {
    for (const material of this.materials.values()) {
      material.dispose();
    }
    this.materials.clear();
  }

  /**
   * Clear cache without disposing (for material reuse)
   */
  clearCache(): void {
    this.materials.clear();
  }

  /**
   * Toggle caching
   */
  setCacheEnabled(enabled: boolean): void {
    this.cacheEnabled = enabled;
  }

  /**
   * Get all cached materials
   */
  getCachedMaterials(): Map<string, THREE.Material> {
    return new Map(this.materials);
  }

  /**
   * Get material statistics
   */
  getStats(): {
    totalMaterials: number;
    cacheEnabled: boolean;
    materialNames: string[];
  } {
    return {
      totalMaterials: this.materials.size,
      cacheEnabled: this.cacheEnabled,
      materialNames: Array.from(this.materials.keys())
    };
  }
}

/**
 * Global material factory instance
 */
export const globalMaterialFactory = new MaterialFactory();

/**
 * Create lights for the scene
 */
export function createSceneLights(): THREE.Light[] {
  const lights: THREE.Light[] = [];

  // Ambient light (overall scene illumination)
  const ambientLight = new THREE.AmbientLight(MaterialPalette.lightAmbient, 2);
  lights.push(ambientLight);

  // Directional light (sun-like from upper right)
  const directionalLight = new THREE.DirectionalLight(0xffffff, 1.5);
  directionalLight.position.set(5, 10, 7);
  directionalLight.castShadow = true;
  directionalLight.shadow.mapSize.width = 2048;
  directionalLight.shadow.mapSize.height = 2048;
  directionalLight.shadow.camera.far = 50;
  directionalLight.shadow.camera.left = -15;
  directionalLight.shadow.camera.right = 15;
  directionalLight.shadow.camera.top = 15;
  directionalLight.shadow.camera.bottom = -15;
  directionalLight.shadow.bias = -0.0005;
  lights.push(directionalLight);

  // Point light (accent from lower left)
  const pointLight = new THREE.PointLight(0x4a9eff, 0.8);
  pointLight.position.set(-5, 3, 5);
  pointLight.decay = 2;
  lights.push(pointLight);

  return lights;
}
