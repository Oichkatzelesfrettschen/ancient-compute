/**
 * Base Renderer: WebGL Shader Compilation and Rendering
 *
 * Manages:
 * - Shader compilation with error handling
 * - Material binding to Three.js geometries
 * - Render pass organization: opaque → transparent
 * - WebGL state management
 * - Camera setup and viewport
 * - Cross-platform WebGL 2.0 baseline support
 *
 * Design assumes Three.js 0.158.0+ for modern WebGL 2.0 usage
 */

import * as THREE from 'three';

/**
 * Shader source pair
 */
export interface ShaderSource {
  vertexShader: string;
  fragmentShader: string;
}

/**
 * Material configuration
 */
export interface MaterialConfig {
  name: string;
  vertexShader: string;
  fragmentShader: string;
  uniforms?: Record<string, THREE.IUniform>;
  side?: THREE.Side; // FrontSide, BackSide, DoubleSide
  transparent?: boolean;
  depthWrite?: boolean;
  depthTest?: boolean;
}

/**
 * Renderable object in scene
 */
export interface RenderableObject {
  mesh: THREE.Mesh;
  material: THREE.Material;
  renderPass: 'opaque' | 'transparent';
  visible: boolean;
  sortOrder: number; // For transparent pass depth sorting
}

/**
 * Base renderer with WebGL management
 */
export class BaseRenderer {
  private canvas: HTMLCanvasElement | null = null;
  private renderer: THREE.WebGLRenderer | null = null;
  private scene: THREE.Scene;
  private camera: THREE.PerspectiveCamera;
  private materials: Map<string, THREE.Material> = new Map();
  private renderables: RenderableObject[] = [];
  private shaderCache: Map<string, ShaderSource> = new Map();

  // Render passes
  private opaquePass: RenderableObject[] = [];
  private transparentPass: RenderableObject[] = [];

  // Configuration
  private readonly clearColor: THREE.Color = new THREE.Color(0x1a1a2e); // Dark background
  private readonly clearAlpha: number = 1.0;
  private readonly pixelRatio: number = window.devicePixelRatio || 1;

  // Viewport dimensions
  private width: number = 1024;
  private height: number = 768;

  constructor(canvasElement?: HTMLCanvasElement) {
    this.scene = new THREE.Scene();
    this.camera = new THREE.PerspectiveCamera(
      75,
      this.width / this.height,
      0.1,
      1000
    );
    this.camera.position.z = 5;

    if (canvasElement) {
      this.initializeRenderer(canvasElement);
    }
  }

  /**
   * Initialize WebGL renderer
   */
  private initializeRenderer(canvasElement: HTMLCanvasElement): void {
    this.canvas = canvasElement;

    this.renderer = new THREE.WebGLRenderer({
      canvas: canvasElement,
      antialias: true,
      alpha: false,
      precision: 'highp',
      powerPreference: 'high-performance'
    });

    this.renderer.setClearColor(this.clearColor, this.clearAlpha);
    this.renderer.setPixelRatio(this.pixelRatio);
    this.renderer.setSize(this.width, this.height, false);
    this.renderer.shadowMap.enabled = true;
    this.renderer.shadowMap.type = THREE.PCFShadowShadowMap;

    // Enable scissor test for viewport management (if needed)
    const gl = this.renderer.getContext();
    if (gl) {
      gl.enable(gl.SCISSOR_TEST);
    }
  }

  /**
   * Compile and cache shader
   */
  compileShader(
    shaderName: string,
    vertexSource: string,
    fragmentSource: string
  ): { success: boolean; error?: string } {
    try {
      // Cache shader source
      this.shaderCache.set(shaderName, {
        vertexShader: vertexSource,
        fragmentShader: fragmentSource
      });

      // Validate shader syntax by creating material
      const testMaterial = new THREE.ShaderMaterial({
        vertexShader: vertexSource,
        fragmentShader: fragmentSource,
        uniforms: {}
      });

      // Force compilation
      if (this.renderer) {
        this.renderer.compile(testMaterial, this.scene);
      }

      return { success: true };
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      console.error(`[BaseRenderer] Shader compilation failed for ${shaderName}:`, errorMsg);
      return { success: false, error: errorMsg };
    }
  }

  /**
   * Create material from shader
   */
  createMaterial(config: MaterialConfig): THREE.Material | null {
    // Check if already compiled
    if (this.materials.has(config.name)) {
      return this.materials.get(config.name) || null;
    }

    try {
      const material = new THREE.ShaderMaterial({
        name: config.name,
        vertexShader: config.vertexShader,
        fragmentShader: config.fragmentShader,
        uniforms: config.uniforms || {},
        side: config.side || THREE.FrontSide,
        transparent: config.transparent || false,
        depthWrite: config.depthWrite !== false,
        depthTest: config.depthTest !== false
      });

      this.materials.set(config.name, material);

      // Force compilation
      if (this.renderer) {
        this.renderer.compile(material, this.scene);
      }

      return material;
    } catch (error) {
      const errorMsg = error instanceof Error ? error.message : String(error);
      console.error(`[BaseRenderer] Material creation failed for ${config.name}:`, errorMsg);
      return null;
    }
  }

  /**
   * Add renderable object
   */
  addRenderable(
    mesh: THREE.Mesh,
    material: THREE.Material,
    renderPass: 'opaque' | 'transparent' = 'opaque',
    sortOrder: number = 0
  ): void {
    const renderable: RenderableObject = {
      mesh,
      material,
      renderPass,
      visible: true,
      sortOrder
    };

    this.renderables.push(renderable);
    this.scene.add(mesh);
    this.updateRenderPasses();
  }

  /**
   * Remove renderable object
   */
  removeRenderable(mesh: THREE.Mesh): void {
    this.renderables = this.renderables.filter((r) => r.mesh !== mesh);
    this.scene.remove(mesh);
    this.updateRenderPasses();
  }

  /**
   * Update render passes (opaque vs transparent)
   */
  private updateRenderPasses(): void {
    this.opaquePass = this.renderables.filter(
      (r) => r.renderPass === 'opaque' && r.visible
    );

    // Sort transparent objects by depth (back to front)
    this.transparentPass = this.renderables
      .filter((r) => r.renderPass === 'transparent' && r.visible)
      .sort((a, b) => b.sortOrder - a.sortOrder);
  }

  /**
   * Set viewport dimensions and update camera
   */
  setViewport(width: number, height: number): void {
    this.width = width;
    this.height = height;

    if (this.renderer) {
      this.renderer.setSize(width, height, false);
    }

    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
  }

  /**
   * Update camera position and rotation
   */
  setCameraTransform(
    position: THREE.Vector3,
    lookAt: THREE.Vector3
  ): void {
    this.camera.position.copy(position);
    this.camera.lookAt(lookAt);
  }

  /**
   * Get camera
   */
  getCamera(): THREE.PerspectiveCamera {
    return this.camera;
  }

  /**
   * Get scene
   */
  getScene(): THREE.Scene {
    return this.scene;
  }

  /**
   * Get WebGL renderer
   */
  getRenderer(): THREE.WebGLRenderer | null {
    return this.renderer;
  }

  /**
   * Render single frame
   */
  render(): void {
    if (!this.renderer) {
      return;
    }

    // Clear
    this.renderer.clear();

    // Opaque pass
    for (const renderable of this.opaquePass) {
      this.renderer.render(renderable.mesh, this.camera);
    }

    // Transparent pass (with blending)
    this.renderer.state.setBlending(THREE.NormalBlending);
    for (const renderable of this.transparentPass) {
      this.renderer.render(renderable.mesh, this.camera);
    }

    // Reset blending
    this.renderer.state.setBlending(THREE.NoBlending);
  }

  /**
   * Render to target (if using render targets)
   */
  renderToTarget(target: THREE.WebGLRenderTarget): void {
    if (!this.renderer) {
      return;
    }

    this.renderer.setRenderTarget(target);
    this.render();
    this.renderer.setRenderTarget(null);
  }

  /**
   * Animate frame
   */
  animate(callback: (time: number) => void): void {
    const animate = (time: number) => {
      requestAnimationFrame(animate);
      callback(time);
      this.render();
    };

    requestAnimationFrame(animate);
  }

  /**
   * Get shader from cache
   */
  getShader(shaderName: string): ShaderSource | null {
    return this.shaderCache.get(shaderName) || null;
  }

  /**
   * Get material
   */
  getMaterial(materialName: string): THREE.Material | null {
    return this.materials.get(materialName) || null;
  }

  /**
   * Clear all materials
   */
  clearMaterials(): void {
    for (const material of this.materials.values()) {
      if (material instanceof THREE.ShaderMaterial) {
        material.dispose();
      }
    }
    this.materials.clear();
  }

  /**
   * Clear scene and materials
   */
  dispose(): void {
    // Dispose renderables
    for (const renderable of this.renderables) {
      renderable.mesh.geometry.dispose();
      if (renderable.material instanceof THREE.ShaderMaterial) {
        renderable.material.dispose();
      }
      this.scene.remove(renderable.mesh);
    }

    this.renderables = [];
    this.opaquePass = [];
    this.transparentPass = [];

    // Dispose materials
    this.clearMaterials();

    // Dispose renderer
    if (this.renderer) {
      this.renderer.dispose();
      this.renderer = null;
    }
  }

  /**
   * Get debug info
   */
  getDebugInfo(): string {
    return `
Base Renderer:
- Canvas: ${this.canvas ? 'initialized' : 'not initialized'}
- Renderer: ${this.renderer ? 'ready' : 'not ready'}
- Viewport: ${this.width}x${this.height}
- Pixel Ratio: ${this.pixelRatio}
- Renderables: ${this.renderables.length}
- Opaque Pass: ${this.opaquePass.length}
- Transparent Pass: ${this.transparentPass.length}
- Materials: ${this.materials.size}
- Shaders: ${this.shaderCache.size}
- Clear Color: #${this.clearColor.getHexString().toUpperCase()}
    `.trim();
  }
}

/**
 * Create default column shader material
 */
export function createColumnMaterial(baseRenderer: BaseRenderer): THREE.Material | null {
  const vertexShader = `
    varying vec3 vPosition;
    varying vec3 vNormal;
    varying vec2 vUv;

    void main() {
      vPosition = position;
      vNormal = normalize(normalMatrix * normal);
      vUv = uv;

      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `;

  const fragmentShader = `
    varying vec3 vPosition;
    varying vec3 vNormal;
    varying vec2 vUv;

    uniform vec3 uColor;
    uniform float uAlpha;

    void main() {
      vec3 light = normalize(vec3(1.0, 1.0, 1.0));
      float diffuse = max(dot(vNormal, light), 0.3);
      vec3 color = uColor * diffuse;

      gl_FragColor = vec4(color, uAlpha);
    }
  `;

  return baseRenderer.createMaterial({
    name: 'columnMaterial',
    vertexShader,
    fragmentShader,
    uniforms: {
      uColor: { value: new THREE.Color(0x4a9eff) },
      uAlpha: { value: 1.0 }
    },
    side: THREE.FrontSide,
    transparent: false
  });
}

/**
 * Create default shaft shader material
 */
export function createShaftMaterial(baseRenderer: BaseRenderer): THREE.Material | null {
  const vertexShader = `
    varying vec3 vPosition;
    varying vec3 vNormal;

    void main() {
      vPosition = position;
      vNormal = normalize(normalMatrix * normal);

      gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
    }
  `;

  const fragmentShader = `
    varying vec3 vPosition;
    varying vec3 vNormal;

    uniform vec3 uColor;

    void main() {
      vec3 light = normalize(vec3(1.0, 1.0, 1.0));
      float diffuse = max(dot(vNormal, light), 0.3);
      vec3 color = uColor * diffuse;

      gl_FragColor = vec4(color, 1.0);
    }
  `;

  return baseRenderer.createMaterial({
    name: 'shaftMaterial',
    vertexShader,
    fragmentShader,
    uniforms: {
      uColor: { value: new THREE.Color(0xff6b6b) }
    },
    side: THREE.FrontSide,
    transparent: false
  });
}
