/**
 * MaterialLibrary - Material Definition and Management
 *
 * Centralizes all material definitions for the Difference Engine visualization.
 * Ensures visual consistency and enables rapid material updates across
 * all mechanical components.
 *
 * Material types:
 * - Metallic (steel, brass, bronze)
 * - Mechanical (gears, wheels, columns)
 * - Highlight (for interactive elements)
 * - Transparent (for non-critical support structures)
 */

import * as THREE from 'three';

export enum MaterialType {
  STEEL = 'steel',
  BRASS = 'brass',
  BRONZE = 'bronze',
  IRON = 'iron',
  COPPER = 'copper',
  BRASS_POLISHED = 'brass-polished',
  STEEL_DARK = 'steel-dark',
  HIGHLIGHT = 'highlight',
  TRANSPARENT = 'transparent'
}

export interface MaterialConfig {
  color?: number | string;
  metalness?: number;
  roughness?: number;
  emissive?: number | string;
  emissiveIntensity?: number;
  transparent?: boolean;
  opacity?: number;
  side?: THREE.Side;
  wireframe?: boolean;
}

/**
 * Manages material instances and definitions
 */
export class MaterialLibrary {
  private materials: Map<string, THREE.Material> = new Map();
  private configs: Map<string, MaterialConfig> = new Map();

  constructor() {
    this.initializeDefaultMaterials();
  }

  /**
   * Initialize default material definitions
   */
  private initializeDefaultMaterials(): void {
    // Steel - matte, dark metal
    this.registerMaterial(MaterialType.STEEL, {
      color: 0x666666,
      metalness: 0.9,
      roughness: 0.4,
      emissive: 0x000000,
      emissiveIntensity: 0.0
    });

    // Steel dark - darker variant
    this.registerMaterial(MaterialType.STEEL_DARK, {
      color: 0x333333,
      metalness: 0.95,
      roughness: 0.5,
      emissive: 0x000000,
      emissiveIntensity: 0.0
    });

    // Brass - warm metallic gold
    this.registerMaterial(MaterialType.BRASS, {
      color: 0xc9a862,
      metalness: 0.85,
      roughness: 0.35,
      emissive: 0x332200,
      emissiveIntensity: 0.1
    });

    // Brass polished - shiny brass
    this.registerMaterial(MaterialType.BRASS_POLISHED, {
      color: 0xd4af37,
      metalness: 0.9,
      roughness: 0.1,
      emissive: 0x664400,
      emissiveIntensity: 0.15
    });

    // Bronze - darker warm metal
    this.registerMaterial(MaterialType.BRONZE, {
      color: 0x8b6f47,
      metalness: 0.8,
      roughness: 0.4,
      emissive: 0x2a1810,
      emissiveIntensity: 0.05
    });

    // Iron - dull dark metal
    this.registerMaterial(MaterialType.IRON, {
      color: 0x4a4a4a,
      metalness: 0.8,
      roughness: 0.6,
      emissive: 0x000000,
      emissiveIntensity: 0.0
    });

    // Copper - reddish metal
    this.registerMaterial(MaterialType.COPPER, {
      color: 0xb87333,
      metalness: 0.85,
      roughness: 0.3,
      emissive: 0x4a2a0a,
      emissiveIntensity: 0.08
    });

    // Highlight - bright for interactive elements
    this.registerMaterial(MaterialType.HIGHLIGHT, {
      color: 0xffaa00,
      metalness: 1.0,
      roughness: 0.0,
      emissive: 0xffaa00,
      emissiveIntensity: 0.4
    });

    // Transparent - for visualization aid structures
    this.registerMaterial(MaterialType.TRANSPARENT, {
      color: 0x888888,
      metalness: 0.5,
      roughness: 0.5,
      emissive: 0x000000,
      emissiveIntensity: 0.0,
      transparent: true,
      opacity: 0.3
    });
  }

  /**
   * Register a material configuration
   */
  public registerMaterial(name: string, config: MaterialConfig): void {
    this.configs.set(name, config);

    // Create material instance
    const material = this.createMaterial(config);
    this.materials.set(name, material);
  }

  /**
   * Create material from config
   */
  private createMaterial(config: MaterialConfig): THREE.Material {
    const materialConfig: any = {
      color: config.color ?? 0x888888,
      side: config.side ?? THREE.FrontSide,
      wireframe: config.wireframe ?? false
    };

    // Add metalness/roughness for MeshStandardMaterial
    if (config.metalness !== undefined) {
      materialConfig.metalness = config.metalness;
    }
    if (config.roughness !== undefined) {
      materialConfig.roughness = config.roughness;
    }

    // Add emissive properties
    if (config.emissive !== undefined) {
      materialConfig.emissive = config.emissive;
      materialConfig.emissiveIntensity = config.emissiveIntensity ?? 0;
    }

    // Add transparency
    if (config.transparent !== undefined) {
      materialConfig.transparent = config.transparent;
      materialConfig.opacity = config.opacity ?? 1.0;
      materialConfig.depthWrite = !config.transparent;
    }

    return new THREE.MeshStandardMaterial(materialConfig);
  }

  /**
   * Get a material by name
   */
  public getMaterial(name: string | MaterialType): THREE.Material {
    const material = this.materials.get(name as string);
    if (!material) {
      throw new Error(`MaterialLibrary: Material "${name}" not found`);
    }
    return material.clone();
  }

  /**
   * Get raw material (shared instance, do not modify)
   */
  public getRawMaterial(name: string | MaterialType): THREE.Material {
    const material = this.materials.get(name as string);
    if (!material) {
      throw new Error(`MaterialLibrary: Material "${name}" not found`);
    }
    return material;
  }

  /**
   * Update material properties
   */
  public updateMaterial(name: string, config: Partial<MaterialConfig>): void {
    const existingConfig = this.configs.get(name) || {};
    const newConfig = { ...existingConfig, ...config };

    this.configs.set(name, newConfig);

    // Update material instance
    const material = this.materials.get(name);
    if (material && material instanceof THREE.MeshStandardMaterial) {
      if (config.color !== undefined) {
        material.color.setStyle(config.color as any);
      }
      if (config.metalness !== undefined) {
        material.metalness = config.metalness;
      }
      if (config.roughness !== undefined) {
        material.roughness = config.roughness;
      }
      if (config.emissive !== undefined) {
        material.emissive.setStyle(config.emissive as any);
      }
      if (config.emissiveIntensity !== undefined) {
        material.emissiveIntensity = config.emissiveIntensity;
      }
      if (config.opacity !== undefined) {
        material.opacity = config.opacity;
      }

      material.needsUpdate = true;
    }
  }

  /**
   * Get material color
   */
  public getColor(name: string | MaterialType): THREE.Color {
    const material = this.getRawMaterial(name as string);
    if (material instanceof THREE.MeshStandardMaterial) {
      return material.color.clone();
    }
    return new THREE.Color(0xffffff);
  }

  /**
   * Set material color
   */
  public setColor(name: string, color: number | string): void {
    const material = this.materials.get(name);
    if (material instanceof THREE.MeshStandardMaterial) {
      material.color.setStyle(color as any);
      material.needsUpdate = true;
    }
  }

  /**
   * Create a variant of an existing material
   */
  public createVariant(baseName: string, variantName: string, overrides: Partial<MaterialConfig>): void {
    const baseConfig = this.configs.get(baseName);
    if (!baseConfig) {
      throw new Error(`MaterialLibrary: Base material "${baseName}" not found`);
    }

    const variantConfig = { ...baseConfig, ...overrides };
    this.registerMaterial(variantName, variantConfig);
  }

  /**
   * Check if material exists
   */
  public has(name: string | MaterialType): boolean {
    return this.materials.has(name as string);
  }

  /**
   * Get list of available materials
   */
  public getAvailableMaterials(): string[] {
    return Array.from(this.materials.keys());
  }

  /**
   * Get material statistics
   */
  public getStats(): {
    totalMaterials: number;
    materials: Array<{ name: string; config: MaterialConfig }>;
  } {
    const materials: Array<{ name: string; config: MaterialConfig }> = [];

    this.materials.forEach((material, name) => {
      const config = this.configs.get(name);
      if (config) {
        materials.push({ name, config });
      }
    });

    return {
      totalMaterials: this.materials.size,
      materials
    };
  }

  /**
   * Dispose all materials
   */
  public dispose(): void {
    this.materials.forEach((material) => {
      material.dispose();
    });
    this.materials.clear();
    this.configs.clear();
  }
}

/**
 * Global singleton instance
 */
let globalMaterialLibrary: MaterialLibrary | null = null;

/**
 * Get or create global material library
 */
export function getGlobalMaterialLibrary(): MaterialLibrary {
  if (!globalMaterialLibrary) {
    globalMaterialLibrary = new MaterialLibrary();
  }
  return globalMaterialLibrary;
}

/**
 * Dispose global material library
 */
export function disposeGlobalMaterialLibrary(): void {
  if (globalMaterialLibrary) {
    globalMaterialLibrary.dispose();
    globalMaterialLibrary = null;
  }
}
