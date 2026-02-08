/**
 * LightingSetup - Scene Illumination Configuration
 *
 * Manages all light sources for proper visualization of the mechanical
 * Difference Engine. Includes ambient, directional, and point lights
 * with careful tuning for mechanical detail visibility.
 *
 * Lighting strategy:
 * - Ambient light: Base illumination across all surfaces
 * - Main directional light: Key light from upper front-right
 * - Fill directional light: Reduced intensity from opposite side
 * - Point lights: Optional accent lighting for specific mechanical parts
 */

import * as THREE from 'three';

export interface LightingConfig {
  ambientIntensity?: number;
  ambientColor?: number | string;
  mainDirectionalColor?: number | string;
  mainDirectionalIntensity?: number;
  mainDirectionalPosition?: [number, number, number];
  fillDirectionalColor?: number | string;
  fillDirectionalIntensity?: number;
  fillDirectionalPosition?: [number, number, number];
  enableShadows?: boolean;
  shadowMapSize?: number;
  shadowBias?: number;
  shadowNormalBias?: number;
  shadowRadius?: number;
}

export interface LightSnapshot {
  ambientIntensity: number;
  mainLightIntensity: number;
  fillLightIntensity: number;
  shadowsEnabled: boolean;
}

export class LightingSetup {
  private scene: THREE.Scene;
  private lights: Map<string, THREE.Light> = new Map();

  // Main lights
  private ambientLight: THREE.AmbientLight;
  private mainDirectional: THREE.DirectionalLight;
  private fillDirectional: THREE.DirectionalLight;

  // Configuration
  private config: Required<LightingConfig>;
  private shadowsEnabled: boolean;

  constructor(scene: THREE.Scene, config?: LightingConfig) {
    this.scene = scene;
    this.config = this.mergeConfig(config);
    this.shadowsEnabled = this.config.enableShadows;

    // Create and add lights
    this.ambientLight = this.createAmbientLight();
    this.mainDirectional = this.createMainDirectionalLight();
    this.fillDirectional = this.createFillDirectionalLight();

    // Register lights
    this.lights.set('ambient', this.ambientLight);
    this.lights.set('mainDirectional', this.mainDirectional);
    this.lights.set('fillDirectional', this.fillDirectional);

    // Add to scene
    this.scene.add(this.ambientLight);
    this.scene.add(this.mainDirectional);
    this.scene.add(this.fillDirectional);
  }

  /**
   * Merge config with defaults
   */
  private mergeConfig(config?: LightingConfig): Required<LightingConfig> {
    return {
      ambientIntensity: config?.ambientIntensity ?? 0.4,
      ambientColor: config?.ambientColor ?? 0xffffff,
      mainDirectionalColor: config?.mainDirectionalColor ?? 0xffffff,
      mainDirectionalIntensity: config?.mainDirectionalIntensity ?? 0.8,
      mainDirectionalPosition: config?.mainDirectionalPosition ?? [50, 40, 30],
      fillDirectionalColor: config?.fillDirectionalColor ?? 0xa0a0ff,
      fillDirectionalIntensity: config?.fillDirectionalIntensity ?? 0.3,
      fillDirectionalPosition: config?.fillDirectionalPosition ?? [-30, 20, -50],
      enableShadows: config?.enableShadows ?? true,
      shadowMapSize: config?.shadowMapSize ?? 2048,
      shadowBias: config?.shadowBias ?? -0.0001,
      shadowNormalBias: config?.shadowNormalBias ?? 0.02,
      shadowRadius: config?.shadowRadius ?? 3
    };
  }

  /**
   * Create ambient light
   * Provides base illumination across entire scene
   */
  private createAmbientLight(): THREE.AmbientLight {
    const light = new THREE.AmbientLight(
      this.config.ambientColor as any,
      this.config.ambientIntensity
    );
    return light;
  }

  /**
   * Create main directional light (key light)
   * Positioned to highlight mechanical details
   */
  private createMainDirectionalLight(): THREE.DirectionalLight {
    const light = new THREE.DirectionalLight(
      this.config.mainDirectionalColor as any,
      this.config.mainDirectionalIntensity
    );

    const [x, y, z] = this.config.mainDirectionalPosition;
    light.position.set(x, y, z);
    light.target.position.set(0, 0, 0);

    // Configure shadow map
    if (this.shadowsEnabled) {
      light.castShadow = true;
      light.shadow.mapSize.width = this.config.shadowMapSize;
      light.shadow.mapSize.height = this.config.shadowMapSize;
      light.shadow.camera.left = -60;
      light.shadow.camera.right = 60;
      light.shadow.camera.top = 60;
      light.shadow.camera.bottom = -20;
      light.shadow.camera.near = 0.5;
      light.shadow.camera.far = 200;
      light.shadow.bias = this.config.shadowBias;
      light.shadow.normalBias = this.config.shadowNormalBias;
      light.shadow.radius = this.config.shadowRadius;
    }

    return light;
  }

  /**
   * Create fill directional light
   * Positioned opposite to main light to fill shadows
   */
  private createFillDirectionalLight(): THREE.DirectionalLight {
    const light = new THREE.DirectionalLight(
      this.config.fillDirectionalColor as any,
      this.config.fillDirectionalIntensity
    );

    const [x, y, z] = this.config.fillDirectionalPosition;
    light.position.set(x, y, z);
    light.target.position.set(0, 0, 0);

    // Fill light doesn't need to cast shadows
    light.castShadow = false;

    return light;
  }

  /**
   * Add a point light for accent lighting
   * Useful for highlighting specific mechanical components
   */
  public addPointLight(
    name: string,
    position: THREE.Vector3,
    color: number | string,
    intensity: number,
    distance: number = 100,
    decay: number = 2
  ): THREE.PointLight {
    const light = new THREE.PointLight(color as any, intensity, distance, decay);
    light.position.copy(position);

    if (this.shadowsEnabled) {
      light.castShadow = true;
      light.shadow.mapSize.width = 512;
      light.shadow.mapSize.height = 512;
    }

    this.scene.add(light);
    this.lights.set(name, light);

    return light;
  }

  /**
   * Set ambient light intensity
   */
  public setAmbientIntensity(intensity: number): void {
    this.ambientLight.intensity = intensity;
  }

  /**
   * Get ambient light intensity
   */
  public getAmbientIntensity(): number {
    return this.ambientLight.intensity;
  }

  /**
   * Set main directional light intensity
   */
  public setMainDirectionalIntensity(intensity: number): void {
    this.mainDirectional.intensity = intensity;
  }

  /**
   * Get main directional light intensity
   */
  public getMainDirectionalIntensity(): number {
    return this.mainDirectional.intensity;
  }

  /**
   * Set fill directional light intensity
   */
  public setFillDirectionalIntensity(intensity: number): void {
    this.fillDirectional.intensity = intensity;
  }

  /**
   * Get fill directional light intensity
   */
  public getFillDirectionalIntensity(): number {
    return this.fillDirectional.intensity;
  }

  /**
   * Update main directional light position
   */
  public setMainDirectionalPosition(x: number, y: number, z: number): void {
    this.mainDirectional.position.set(x, y, z);
  }

  /**
   * Update fill directional light position
   */
  public setFillDirectionalPosition(x: number, y: number, z: number): void {
    this.fillDirectional.position.set(x, y, z);
  }

  /**
   * Get a light by name
   */
  public getLight(name: string): THREE.Light | undefined {
    return this.lights.get(name);
  }

  /**
   * Get all lights
   */
  public getLights(): Map<string, THREE.Light> {
    return this.lights;
  }

  /**
   * Get current lighting snapshot for debugging/state tracking
   */
  public getSnapshot(): LightSnapshot {
    return {
      ambientIntensity: this.ambientLight.intensity,
      mainLightIntensity: this.mainDirectional.intensity,
      fillLightIntensity: this.fillDirectional.intensity,
      shadowsEnabled: this.shadowsEnabled
    };
  }

  /**
   * Update lighting for dramatic effect
   * Useful for scene transitions or specific mechanical phases
   */
  public updateForPhase(phase: string): void {
    // Adjust lighting based on machine phase
    // This can create visual feedback for different mechanical states

    switch (phase) {
      case 'IDLE':
        // Neutral lighting
        this.setMainDirectionalIntensity(0.8);
        this.setFillDirectionalIntensity(0.3);
        break;

      case 'ADDITION':
      case 'CARRY':
        // Slightly brighter to show activity
        this.setMainDirectionalIntensity(0.9);
        this.setFillDirectionalIntensity(0.4);
        break;

      case 'OUTPUT':
        // Bright to highlight result
        this.setMainDirectionalIntensity(1.0);
        this.setFillDirectionalIntensity(0.5);
        break;

      default:
        // Reset to default
        this.setMainDirectionalIntensity(0.8);
        this.setFillDirectionalIntensity(0.3);
    }
  }

  /**
   * Enable/disable shadows
   */
  public setShadowsEnabled(enabled: boolean): void {
    this.shadowsEnabled = enabled;
    this.mainDirectional.castShadow = enabled;
  }

  /**
   * Dispose resources
   */
  public dispose(): void {
    this.lights.forEach((light) => {
      this.scene.remove(light);
      if (light.shadow) {
        light.shadow.map?.dispose();
      }
    });
    this.lights.clear();
  }
}

/**
 * Factory function for creating lighting setup
 */
export function createLightingSetup(
  scene: THREE.Scene,
  config?: LightingConfig
): LightingSetup {
  return new LightingSetup(scene, config);
}
