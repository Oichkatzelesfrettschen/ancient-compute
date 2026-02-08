/**
 * SceneManager - Core Three.js Scene Orchestration
 *
 * Manages the WebGL renderer, scene graph, camera setup, and canvas
 * integration. Provides the foundation for all mechanical visualization.
 *
 * Responsibilities:
 * - Create and configure WebGL renderer
 * - Initialize scene with proper color and fog
 * - Set up camera frustum and aspect ratio
 * - Handle canvas resizing and DPI scaling
 * - Manage renderer pixel ratio for sharp rendering
 */

import * as THREE from 'three';

export interface SceneConfig {
  canvasContainer: HTMLElement;
  width?: number;
  height?: number;
  pixelRatio?: number;
  backgroundColor?: number;
  fogColor?: number;
  fogNear?: number;
  fogFar?: number;
  antialias?: boolean;
  alpha?: boolean;
  precision?: 'lowp' | 'mediump' | 'highp';
}

export interface SceneSnapshot {
  cameraPosition: THREE.Vector3;
  cameraTarget: THREE.Vector3;
  rendererSize: { width: number; height: number };
  dpr: number;
  timeElapsed: number;
}

export class SceneManager {
  // Three.js core objects
  private renderer: THREE.WebGLRenderer;
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private canvas: HTMLCanvasElement;
  private container: HTMLElement;

  // Configuration
  private config: Required<SceneConfig>;
  private width: number;
  private height: number;
  private dpr: number;

  // Timing
  private startTime: number = Date.now();
  private lastFrameTime: number = 0;
  private deltaTime: number = 0;

  // State tracking
  private isInitialized: boolean = false;
  private resizeObserver: ResizeObserver | null = null;
  private renderTarget: THREE.WebGLRenderTarget | null = null;

  /**
   * Create SceneManager instance
   * @param config Configuration object for scene setup
   */
  constructor(config: SceneConfig) {
    this.container = config.canvasContainer;
    this.config = this.mergeConfig(config);

    // Get initial dimensions
    const rect = this.container.getBoundingClientRect();
    this.width = config.width || rect.width;
    this.height = config.height || rect.height;
    this.dpr = config.pixelRatio || Math.min(window.devicePixelRatio, 2);

    // Create renderer
    this.renderer = this.createRenderer();
    this.canvas = this.renderer.domElement;

    // Create scene
    this.scene = this.createScene();

    // Create camera
    this.camera = this.createCamera();

    // Set up resize observer
    this.setupResizeObserver();

    this.isInitialized = true;
  }

  /**
   * Merge provided config with defaults
   */
  private mergeConfig(config: SceneConfig): Required<SceneConfig> {
    return {
      canvasContainer: config.canvasContainer,
      width: config.width || 1024,
      height: config.height || 768,
      pixelRatio: config.pixelRatio || Math.min(window.devicePixelRatio, 2),
      backgroundColor: config.backgroundColor ?? 0x0a0a0a,
      fogColor: config.fogColor ?? 0x0a0a0a,
      fogNear: config.fogNear ?? 10,
      fogFar: config.fogFar ?? 500,
      antialias: config.antialias ?? true,
      alpha: config.alpha ?? false,
      precision: config.precision ?? 'highp'
    };
  }

  /**
   * Create and configure WebGL renderer
   */
  private createRenderer(): THREE.WebGLRenderer {
    const renderer = new THREE.WebGLRenderer({
      antialias: this.config.antialias,
      alpha: this.config.alpha,
      precision: this.config.precision,
      preserveDrawingBuffer: false,
      powerPreference: 'high-performance'
    });

    renderer.setSize(this.width, this.height);
    renderer.setPixelRatio(this.dpr);
    renderer.setClearColor(this.config.backgroundColor);
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = THREE.PCFShadowShadowMap;
    renderer.outputColorSpace = THREE.SRGBColorSpace;
    renderer.gammaFactor = 2.2;

    // Enable extensions for better performance
    const context = renderer.getContext();
    if (context) {
      context.getExtension('EXT_color_buffer_float');
      context.getExtension('OES_texture_float_linear');
    }

    this.container.appendChild(renderer.domElement);
    return renderer;
  }

  /**
   * Create and configure scene
   */
  private createScene(): THREE.Scene {
    const scene = new THREE.Scene();
    scene.background = new THREE.Color(this.config.backgroundColor);
    scene.fog = new THREE.Fog(
      this.config.fogColor,
      this.config.fogNear,
      this.config.fogFar
    );

    return scene;
  }

  /**
   * Create and configure camera
   */
  private createCamera(): THREE.PerspectiveCamera {
    const aspect = this.width / this.height;
    const camera = new THREE.PerspectiveCamera(
      75,      // Field of view
      aspect,  // Aspect ratio
      0.1,     // Near plane
      2000     // Far plane
    );

    // Position camera to view Difference Engine from 3/4 angle
    camera.position.set(40, 30, 50);
    camera.lookAt(0, 0, 0);

    return camera;
  }

  /**
   * Set up resize observer for responsive canvas
   */
  private setupResizeObserver(): void {
    this.resizeObserver = new ResizeObserver(() => {
      this.handleResize();
    });

    this.resizeObserver.observe(this.container);
  }

  /**
   * Handle container resize
   */
  private handleResize(): void {
    const rect = this.container.getBoundingClientRect();
    const newWidth = rect.width;
    const newHeight = rect.height;

    if (newWidth === this.width && newHeight === this.height) {
      return;
    }

    this.width = newWidth;
    this.height = newHeight;

    // Update renderer
    this.renderer.setSize(this.width, this.height);

    // Update camera
    this.camera.aspect = this.width / this.height;
    this.camera.updateProjectionMatrix();
  }

  /**
   * Render a single frame
   * Called during animation loop
   */
  public render(): void {
    if (!this.isInitialized) {
      return;
    }

    const now = Date.now();
    this.deltaTime = (now - this.lastFrameTime) / 1000;
    this.lastFrameTime = now;

    this.renderer.render(this.scene, this.camera);
  }

  /**
   * Get the Three.js scene
   */
  public getScene(): THREE.Scene {
    return this.scene;
  }

  /**
   * Get the Three.js camera
   */
  public getCamera(): THREE.PerspectiveCamera {
    return this.camera;
  }

  /**
   * Get the WebGL renderer
   */
  public getRenderer(): THREE.WebGLRenderer {
    return this.renderer;
  }

  /**
   * Get the canvas element
   */
  public getCanvas(): HTMLCanvasElement {
    return this.canvas;
  }

  /**
   * Get current frame delta time (seconds)
   */
  public getDeltaTime(): number {
    return this.deltaTime;
  }

  /**
   * Get elapsed time since manager creation (seconds)
   */
  public getElapsedTime(): number {
    return (Date.now() - this.startTime) / 1000;
  }

  /**
   * Get viewport dimensions
   */
  public getViewport(): { width: number; height: number } {
    return { width: this.width, height: this.height };
  }

  /**
   * Get device pixel ratio
   */
  public getPixelRatio(): number {
    return this.dpr;
  }

  /**
   * Get WebGL capabilities and limits
   */
  public getCapabilities(): {
    maxTextures: number;
    maxVertexUniforms: number;
    maxFragmentUniforms: number;
    maxVaryings: number;
    maxVertexAttribs: number;
    maxVertexTextureImageUnits: number;
  } {
    const gl = this.renderer.getContext();
    return {
      maxTextures: gl.getParameter(gl.MAX_TEXTURE_IMAGE_UNITS),
      maxVertexUniforms: gl.getParameter(gl.MAX_VERTEX_UNIFORM_VECTORS),
      maxFragmentUniforms: gl.getParameter(gl.MAX_FRAGMENT_UNIFORM_VECTORS),
      maxVaryings: gl.getParameter(gl.MAX_VARYING_VECTORS),
      maxVertexAttribs: gl.getParameter(gl.MAX_VERTEX_ATTRIBS),
      maxVertexTextureImageUnits: gl.getParameter(gl.MAX_VERTEX_TEXTURE_IMAGE_UNITS)
    };
  }

  /**
   * Get current scene snapshot
   */
  public getSnapshot(): SceneSnapshot {
    return {
      cameraPosition: this.camera.position.clone(),
      cameraTarget: new THREE.Vector3(0, 0, 0),
      rendererSize: { width: this.width, height: this.height },
      dpr: this.dpr,
      timeElapsed: this.getElapsedTime()
    };
  }

  /**
   * Reset scene to initial state
   */
  public reset(): void {
    this.startTime = Date.now();
    this.lastFrameTime = 0;
    this.deltaTime = 0;

    // Reset camera
    this.camera.position.set(40, 30, 50);
    this.camera.lookAt(0, 0, 0);
  }

  /**
   * Dispose of resources
   * Call when cleaning up scene
   */
  public dispose(): void {
    if (this.resizeObserver) {
      this.resizeObserver.disconnect();
    }

    this.renderer.dispose();
    this.renderer.forceContextLoss();

    if (this.renderTarget) {
      this.renderTarget.dispose();
    }

    this.scene.clear();
    this.isInitialized = false;
  }
}

/**
 * Factory function for creating scene manager
 * Exported for convenience
 */
export function createSceneManager(
  container: HTMLElement,
  config?: Partial<SceneConfig>
): SceneManager {
  return new SceneManager({
    canvasContainer: container,
    ...config
  });
}
