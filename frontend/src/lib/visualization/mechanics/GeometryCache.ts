/**
 * GeometryCache - Reusable Geometry Management
 *
 * Caches and manages Three.js geometries to avoid duplicate definitions.
 * Critical for performance when creating 248 identical digit wheels
 * (8 columns × 31 digits).
 *
 * Strategy:
 * - Store geometry definitions by name
 * - Share single geometry instance across multiple materials/objects
 * - Lazy initialization on first request
 * - Proper disposal when clearing cache
 */

import * as THREE from 'three';

export interface CachedGeometry {
  geometry: THREE.BufferGeometry;
  referenceCount: number;
  createdAt: number;
}

export type GeometryFactory = () => THREE.BufferGeometry;

/**
 * Manages a cache of Three.js geometries with reference counting
 */
export class GeometryCache {
  private cache: Map<string, CachedGeometry> = new Map();
  private factories: Map<string, GeometryFactory> = new Map();

  constructor() {
    // Register common geometries
    this.registerStandardGeometries();
  }

  /**
   * Register standard geometries used by Difference Engine
   */
  private registerStandardGeometries(): void {
    // Wheel/digit cylinder
    this.registerFactory('digit-wheel', () => {
      return new THREE.CylinderGeometry(8, 8, 3, 32);
    });

    // Column shaft
    this.registerFactory('shaft-rod', () => {
      return new THREE.CylinderGeometry(2, 2, 40, 16);
    });

    // Carry lever arm
    this.registerFactory('carry-arm', () => {
      const geometry = new THREE.BoxGeometry(2, 1, 20);
      geometry.translate(0, 0, 10);
      return geometry;
    });

    // Carriage frame
    this.registerFactory('carriage-frame', () => {
      return new THREE.BoxGeometry(100, 15, 40);
    });

    // Base platform
    this.registerFactory('base-platform', () => {
      return new THREE.BoxGeometry(120, 5, 50);
    });

    // Gear
    this.registerFactory('gear', () => {
      // Simplified gear as a cylinder with ring geometry
      return new THREE.CylinderGeometry(6, 6, 2, 20);
    });

    // Escapement wheel
    this.registerFactory('escapement-wheel', () => {
      return new THREE.CylinderGeometry(10, 10, 2, 40);
    });

    // Small connector rod
    this.registerFactory('connector-rod', () => {
      return new THREE.CylinderGeometry(0.8, 0.8, 15, 8);
    });
  }

  /**
   * Register a custom geometry factory
   */
  public registerFactory(name: string, factory: GeometryFactory): void {
    if (this.cache.has(name)) {
      console.warn(`GeometryCache: Overwriting factory for "${name}"`);
    }
    this.factories.set(name, factory);
  }

  /**
   * Get or create a geometry
   */
  public getGeometry(name: string): THREE.BufferGeometry {
    // Return cached geometry if available
    if (this.cache.has(name)) {
      const cached = this.cache.get(name)!;
      cached.referenceCount++;
      return cached.geometry;
    }

    // Create new geometry from factory
    const factory = this.factories.get(name);
    if (!factory) {
      throw new Error(`GeometryCache: No factory registered for geometry "${name}"`);
    }

    const geometry = factory();

    // Optimize geometry
    geometry.computeVertexNormals();
    geometry.computeBoundingBox();
    geometry.computeBoundingSphere();

    // Cache the geometry
    this.cache.set(name, {
      geometry,
      referenceCount: 1,
      createdAt: Date.now()
    });

    return geometry;
  }

  /**
   * Release a geometry reference
   */
  public releaseGeometry(name: string): void {
    const cached = this.cache.get(name);
    if (!cached) {
      return;
    }

    cached.referenceCount--;

    if (cached.referenceCount <= 0) {
      cached.geometry.dispose();
      this.cache.delete(name);
    }
  }

  /**
   * Get reference count for a geometry
   */
  public getReferenceCount(name: string): number {
    return this.cache.get(name)?.referenceCount ?? 0;
  }

  /**
   * Check if geometry is cached
   */
  public has(name: string): boolean {
    return this.cache.has(name);
  }

  /**
   * Get list of cached geometries
   */
  public getCachedNames(): string[] {
    return Array.from(this.cache.keys());
  }

  /**
   * Get cache statistics
   */
  public getStats(): {
    totalCached: number;
    registeredFactories: number;
    totalVertices: number;
    totalMemoryEstimate: number;
  } {
    let totalVertices = 0;
    let totalMemoryEstimate = 0;

    this.cache.forEach((cached) => {
      const position = cached.geometry.getAttribute('position');
      if (position) {
        totalVertices += position.count;
        // Rough estimate: 12 bytes per vertex (3 floats for position)
        totalMemoryEstimate += position.count * 12;
      }
    });

    return {
      totalCached: this.cache.size,
      registeredFactories: this.factories.size,
      totalVertices,
      totalMemoryEstimate
    };
  }

  /**
   * Clear all cached geometries
   */
  public clear(): void {
    this.cache.forEach((cached) => {
      cached.geometry.dispose();
    });
    this.cache.clear();
  }

  /**
   * Clear geometries with zero references
   */
  public clearUnused(): void {
    const unused: string[] = [];

    this.cache.forEach((cached, name) => {
      if (cached.referenceCount <= 0) {
        unused.push(name);
      }
    });

    unused.forEach((name) => {
      const cached = this.cache.get(name);
      if (cached) {
        cached.geometry.dispose();
        this.cache.delete(name);
      }
    });
  }

  /**
   * Dispose resources
   */
  public dispose(): void {
    this.clear();
    this.factories.clear();
  }
}

/**
 * Global singleton instance
 */
let globalGeometryCache: GeometryCache | null = null;

/**
 * Get or create global geometry cache
 */
export function getGlobalGeometryCache(): GeometryCache {
  if (!globalGeometryCache) {
    globalGeometryCache = new GeometryCache();
  }
  return globalGeometryCache;
}

/**
 * Dispose global cache
 */
export function disposeGlobalGeometryCache(): void {
  if (globalGeometryCache) {
    globalGeometryCache.dispose();
    globalGeometryCache = null;
  }
}
