/**
 * Difference Engine Scene: Main 3D Scene Orchestration
 *
 * Manages:
 * - Three.js scene, camera, and renderer setup
 * - Lighting configuration
 * - Geometry building and material management
 * - Scene graph organization
 * - Rendering pipeline
 * - Frame rate and delta time tracking
 */

import * as THREE from 'three';
import { MaterialFactory, createSceneLights } from './MaterialFactory';
import { GeometryBuilder } from './GeometryBuilder';

/**
 * Scene configuration
 */
export interface SceneConfig {
  canvas: HTMLCanvasElement;
  width: number;
  height: number;
  pixelRatio?: number;
  clearColor?: THREE.Color;
  clearAlpha?: number;
  enableShadows?: boolean;
  enableFog?: boolean;
}

/**
 * Difference Engine 3D Scene
 */
export class DifferenceEngineScene {
  // Core Three.js components
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private renderer: THREE.WebGLRenderer;

  // Scene components
  private materialFactory: MaterialFactory;
  private geometryBuilder: GeometryBuilder;
  private engineGroup: THREE.Group;

  // Lighting
  private lights: THREE.Light[] = [];
  private shadowMap: THREE.WebGLShadowMap | null = null;

  // Rendering
  private clock: THREE.Clock;
  private deltaTime: number = 0;
  private frameCount: number = 0;
  private fps: number = 60;
  private lastFrameTime: number = 0;

  // Configuration
  private config: SceneConfig;
  private isRunning: boolean = false;

  constructor(config: SceneConfig) {
    this.config = {
      pixelRatio: window.devicePixelRatio || 1,
      clearColor: new THREE.Color(0x1a1a2e),
      clearAlpha: 1.0,
      enableShadows: true,
      enableFog: true,
      ...config
    };

    // Initialize core components
    this.scene = new THREE.Scene();
    this.scene.name = 'DifferenceEngineScene';

    this.camera = this.createCamera();
    this.renderer = this.createRenderer();

    this.materialFactory = new MaterialFactory();
    this.geometryBuilder = new GeometryBuilder();

    this.engineGroup = new THREE.Group();
    this.engineGroup.name = 'engine';

    this.clock = new THREE.Clock();

    // Setup scene
    this.setupScene();
  }

  /**
   * Create perspective camera
   */
  private createCamera(): THREE.PerspectiveCamera {
    const camera = new THREE.PerspectiveCamera(
      75,
      this.config.width / this.config.height,
      0.1,
      1000
    );

    // Position camera to view entire engine
    camera.position.set(0, 4, 12);
    camera.lookAt(0, 0, 0);

    return camera;
  }

  /**
   * Create WebGL renderer
   */
  private createRenderer(): THREE.WebGLRenderer {
    const renderer = new THREE.WebGLRenderer({
      canvas: this.config.canvas,
      antialias: true,
      alpha: false,
      precision: 'highp',
      powerPreference: 'high-performance'
    });

    renderer.setClearColor(
      this.config.clearColor || new THREE.Color(0x1a1a2e),
      this.config.clearAlpha ?? 1.0
    );

    renderer.setPixelRatio(this.config.pixelRatio || 1);
    renderer.setSize(this.config.width, this.config.height, false);

    // Configure shadows
    if (this.config.enableShadows) {
      renderer.shadowMap.enabled = true;
      renderer.shadowMap.type = THREE.PCFSoftShadowMap;
      this.shadowMap = renderer.shadowMap;
    }

    // Enable rendering options
    renderer.outputEncoding = THREE.sRGBEncoding;
    renderer.gammaFactor = 2.2;

    return renderer;
  }

  /**
   * Setup scene (lights, fog, geometry)
   */
  private setupScene(): void {
    // Add lights
    this.lights = createSceneLights();
    for (const light of this.lights) {
      this.scene.add(light);
    }

    // Add fog (optional)
    if (this.config.enableFog) {
      this.scene.fog = new THREE.Fog(0x1a1a2e, 50, 100);
    }

    // Build Difference Engine geometry
    const engineGeometry = this.geometryBuilder.buildCompleteEngine(this.materialFactory);
    this.engineGroup.add(engineGeometry);
    this.scene.add(this.engineGroup);

    // Add background
    this.scene.background = this.config.clearColor;
  }

  /**
   * Start rendering loop
   */
  start(): void {
    if (this.isRunning) {
      return;
    }

    this.isRunning = true;
    this.clock.start();
    this.animate();
  }

  /**
   * Stop rendering loop
   */
  stop(): void {
    this.isRunning = false;
  }

  /**
   * Animation loop
   */
  private animate = (): void => {
    if (!this.isRunning) {
      return;
    }

    requestAnimationFrame(this.animate);

    // Calculate delta time
    this.deltaTime = this.clock.getDelta();

    // Update FPS counter
    this.frameCount++;
    const currentTime = performance.now();
    if (currentTime - this.lastFrameTime >= 1000) {
      this.fps = this.frameCount;
      this.frameCount = 0;
      this.lastFrameTime = currentTime;
    }

    // Render frame
    this.render();
  };

  /**
   * Render single frame
   */
  private render(): void {
    this.renderer.render(this.scene, this.camera);
  }

  /**
   * Update scene (called from animation system)
   */
  updateScene(callback: (deltaTime: number) => void): void {
    callback(this.deltaTime);
  }

  /**
   * Rotate entire engine (for interactive viewing)
   */
  rotateEngine(deltaX: number, deltaY: number): void {
    // Rotate around Y axis (left/right mouse)
    this.engineGroup.rotation.y += deltaX * 0.005;

    // Rotate around X axis (up/down mouse)
    this.engineGroup.rotation.x += deltaY * 0.005;

    // Clamp X rotation to prevent flipping
    this.engineGroup.rotation.x = Math.max(
      -Math.PI / 2,
      Math.min(Math.PI / 2, this.engineGroup.rotation.x)
    );
  }

  /**
   * Reset engine rotation to default view
   */
  resetEngineRotation(): void {
    this.engineGroup.rotation.set(0, 0, 0);
  }

  /**
   * Zoom camera
   */
  zoomCamera(delta: number): void {
    const direction = this.camera.position.clone().normalize();
    const currentDistance = this.camera.position.length();
    const newDistance = Math.max(5, Math.min(30, currentDistance + delta * 0.1));

    this.camera.position.copy(direction.multiplyScalar(newDistance));
  }

  /**
   * Set camera to default view
   */
  resetCameraView(): void {
    this.camera.position.set(0, 4, 12);
    this.camera.lookAt(0, 0, 0);
  }

  /**
   * Get digit wheel mesh for animation
   */
  getDigitWheelMesh(columnIndex: number, wheelIndex: number): THREE.Mesh | null {
    return this.geometryBuilder.getDigitWheel(columnIndex, wheelIndex);
  }

  /**
   * Get shaft mesh for animation
   */
  getShaftMesh(shaftIndex: number): THREE.Mesh | null {
    return this.geometryBuilder.getShaft(shaftIndex);
  }

  /**
   * Get carry lever mesh for animation
   */
  getCarryLeverMesh(leverIndex: number): THREE.Mesh | null {
    return this.geometryBuilder.getCarryLever(leverIndex);
  }

  /**
   * Show/hide engagement indicator
   */
  setEngagementIndicatorVisible(leverIndex: number, visible: boolean): void {
    const indicator = this.geometryBuilder.getEngagementIndicator(leverIndex);
    if (indicator) {
      indicator.visible = visible;
    }
  }

  /**
   * Update engagement indicator intensity
   */
  updateEngagementIndicator(leverIndex: number, intensity: number): void {
    const indicator = this.geometryBuilder.getEngagementIndicator(leverIndex);
    if (indicator && indicator.material instanceof THREE.MeshPhongMaterial) {
      indicator.material.emissiveIntensity = intensity * 0.8;
    }
  }

  /**
   * Handle window resize
   */
  onWindowResize(width: number, height: number): void {
    this.config.width = width;
    this.config.height = height;

    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();

    this.renderer.setSize(width, height, false);
  }

  /**
   * Get scene statistics
   */
  getStats(): {
    triangles: number;
    vertices: number;
    meshes: number;
    lights: number;
    fps: number;
    deltaTime: number;
  } {
    return {
      triangles: 0, // Would require traversing scene graph
      vertices: 0,
      meshes: this.scene.children.length,
      lights: this.lights.length,
      fps: this.fps,
      deltaTime: this.deltaTime
    };
  }

  /**
   * Get scene info
   */
  getInfo(): string {
    const wheelCounts = this.geometryBuilder.getComponentCounts();
    const geomStats = GeometryBuilder.getStats();

    return `
Difference Engine Scene:
- Engine: ${this.engineGroup.children.length} groups
- Digit Wheels: ${wheelCounts.digitWheels}
- Shafts: ${wheelCounts.shafts}
- Carry Levers: ${wheelCounts.carryLevers}
- Lights: ${this.lights.length}
- Camera: (${this.camera.position.x.toFixed(1)}, ${this.camera.position.y.toFixed(1)}, ${this.camera.position.z.toFixed(1)})
- FPS: ${this.fps}
- Geometries Cached: ${geomStats.totalGeometries}
    `.trim();
  }

  /**
   * Dispose resources
   */
  dispose(): void {
    this.stop();

    // Dispose geometry
    GeometryBuilder.disposeCache();

    // Dispose materials
    this.materialFactory.disposeAll();

    // Dispose scene
    this.scene.traverse((object) => {
      if (object instanceof THREE.Mesh) {
        object.geometry.dispose();
        if (object.material instanceof THREE.Material) {
          object.material.dispose();
        }
      }
    });

    // Dispose renderer
    this.renderer.dispose();
  }

  /**
   * Get scene reference
   */
  getScene(): THREE.Scene {
    return this.scene;
  }

  /**
   * Get camera reference
   */
  getCamera(): THREE.PerspectiveCamera {
    return this.camera;
  }

  /**
   * Get renderer reference
   */
  getRenderer(): THREE.WebGLRenderer {
    return this.renderer;
  }

  /**
   * Get engine group
   */
  getEngineGroup(): THREE.Group {
    return this.engineGroup;
  }

  /**
   * Get material factory
   */
  getMaterialFactory(): MaterialFactory {
    return this.materialFactory;
  }
}

/**
 * Factory function to create scene
 */
export function createDifferenceEngineScene(
  canvas: HTMLCanvasElement,
  width: number,
  height: number
): DifferenceEngineScene {
  return new DifferenceEngineScene({
    canvas,
    width,
    height,
    pixelRatio: window.devicePixelRatio || 1,
    clearColor: new THREE.Color(0x1a1a2e),
    enableShadows: true,
    enableFog: true
  });
}
