/**
 * Feature Detector: WebGL Capabilities and Device Tiering
 *
 * Detects:
 * - WebGL 2.0 support (baseline for all platforms)
 * - Optional compute shader support (Windows/Linux)
 * - Shader storage buffer objects (SSBO)
 * - Draw indirect support
 * - Query objects for timing
 * - Device performance tier classification
 *
 * Cross-Platform Strategy:
 * - macOS: OpenGL 4.1 maximum (WebGL 2.0)
 * - Windows/Linux: OpenGL 4.3+ (WebGL 2.0 + compute shaders)
 * - Graceful degradation on low-end hardware
 */

/**
 * Device performance tier
 */
export type DeviceTier = 'LOW' | 'MID' | 'HIGH';

/**
 * WebGL capabilities object
 */
export interface WebGLCapabilities {
  // Core support
  supportsWebGL2: boolean;
  supportsWebGL1: boolean;

  // Optional extensions
  supportsComputeShaders: boolean;
  supportsShaderStorageBufferObject: boolean;
  supportsDrawIndirect: boolean;
  supportsQueryObjects: boolean;
  supportsOcclusionQuery: boolean;
  supportsTimerQuery: boolean;

  // Texture support
  maxTextureSize: number;
  maxCubeMapTextureSize: number;
  maxTextureUnits: number;
  maxVertexTextureImageUnits: number;
  maxFragmentTextureImageUnits: number;

  // Buffer support
  maxVertexUniforms: number;
  maxFragmentUniforms: number;
  maxVaryingVectors: number;
  maxElementsIndices: number;
  maxElementsVertices: number;

  // Render target support
  maxRenderbufferSize: number;
  maxColorAttachments: number;
  maxDrawBuffers: number;

  // Platform detection
  platform: 'MACOS' | 'WINDOWS' | 'LINUX' | 'UNKNOWN';
  gpuVendor: string;

  // Device tier based on capabilities
  deviceTier: DeviceTier;
}

/**
 * Feature detector implementation
 */
export class FeatureDetector {
  private capabilities: WebGLCapabilities | null = null;
  private canvas: HTMLCanvasElement | null = null;
  private gl: WebGLRenderingContext | WebGL2RenderingContext | null = null;

  /**
   * Detect all features
   */
  detectFeatures(): WebGLCapabilities {
    if (this.capabilities) {
      return this.capabilities;
    }

    const caps: WebGLCapabilities = {
      supportsWebGL2: false,
      supportsWebGL1: false,
      supportsComputeShaders: false,
      supportsShaderStorageBufferObject: false,
      supportsDrawIndirect: false,
      supportsQueryObjects: false,
      supportsOcclusionQuery: false,
      supportsTimerQuery: false,
      maxTextureSize: 512,
      maxCubeMapTextureSize: 512,
      maxTextureUnits: 8,
      maxVertexTextureImageUnits: 0,
      maxFragmentTextureImageUnits: 8,
      maxVertexUniforms: 128,
      maxFragmentUniforms: 128,
      maxVaryingVectors: 8,
      maxElementsIndices: 65536,
      maxElementsVertices: 65536,
      maxRenderbufferSize: 1024,
      maxColorAttachments: 1,
      maxDrawBuffers: 1,
      platform: this.detectPlatform(),
      gpuVendor: this.detectGPUVendor(),
      deviceTier: 'LOW'
    };

    try {
      // Try WebGL 2.0 first
      this.canvas = this.createOffscreenCanvas();
      this.gl = this.canvas.getContext('webgl2');

      if (this.gl) {
        caps.supportsWebGL2 = true;
        this.queryCapabilities(this.gl as WebGL2RenderingContext, caps);
      } else {
        // Fall back to WebGL 1.0
        this.gl = this.canvas.getContext('webgl') || this.canvas.getContext('experimental-webgl');
        if (this.gl) {
          caps.supportsWebGL1 = true;
          this.queryCapabilities(this.gl, caps);
        }
      }
    } catch (error) {
      console.error('[FeatureDetector] WebGL initialization failed:', error);
    } finally {
      this.cleanup();
    }

    // Classify device tier
    caps.deviceTier = this.classifyDeviceTier(caps);

    this.capabilities = caps;
    return caps;
  }

  /**
   * Query WebGL capabilities
   */
  private queryCapabilities(
    gl: WebGL2RenderingContext | WebGLRenderingContext,
    caps: WebGLCapabilities
  ): void {
    // Texture capabilities
    caps.maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE) as number;
    caps.maxCubeMapTextureSize = gl.getParameter(gl.MAX_CUBE_MAP_TEXTURE_SIZE) as number;
    caps.maxTextureUnits = gl.getParameter(gl.MAX_TEXTURE_IMAGE_UNITS) as number;
    caps.maxVertexTextureImageUnits = gl.getParameter(gl.MAX_VERTEX_TEXTURE_IMAGE_UNITS) as number;
    caps.maxFragmentTextureImageUnits = gl.getParameter(gl.MAX_TEXTURE_IMAGE_UNITS) as number;

    // Uniform capabilities
    caps.maxVertexUniforms = gl.getParameter(gl.MAX_VERTEX_UNIFORM_VECTORS) as number;
    caps.maxFragmentUniforms = gl.getParameter(gl.MAX_FRAGMENT_UNIFORM_VECTORS) as number;
    caps.maxVaryingVectors = gl.getParameter(gl.MAX_VARYING_VECTORS) as number;

    // Element capabilities
    if ((gl as any).MAX_ELEMENT_INDEX) {
      caps.maxElementsIndices = gl.getParameter((gl as any).MAX_ELEMENT_INDEX) as number;
    }

    // Renderbuffer capabilities
    caps.maxRenderbufferSize = gl.getParameter(gl.MAX_RENDERBUFFER_SIZE) as number;

    // WebGL 2.0 specific
    if ('MAX_COLOR_ATTACHMENTS' in gl) {
      caps.maxColorAttachments = gl.getParameter((gl as any).MAX_COLOR_ATTACHMENTS) as number;
      caps.maxDrawBuffers = gl.getParameter((gl as any).MAX_DRAW_BUFFERS) as number;
    }

    // Check extensions for WebGL 2.0
    const gl2 = gl as WebGL2RenderingContext;
    if (gl2.getExtension) {
      // Compute shader support (WebGL 2.0 extension)
      const computeExt = gl2.getExtension('WEBGL_compute_shader');
      caps.supportsComputeShaders = !!computeExt;

      // Shader storage buffer object
      const ssboExt = gl2.getExtension('WEBGL_shader_storage_buffer_object');
      caps.supportsShaderStorageBufferObject = !!ssboExt;

      // Draw indirect
      const drawIndirectExt = gl2.getExtension('WEBGL_draw_buffers');
      caps.supportsDrawIndirect = !!drawIndirectExt;

      // Query objects
      const queryExt = gl2.getExtension('EXT_disjoint_timer_query_webgl2');
      const timerExt = gl2.getExtension('EXT_disjoint_timer_query');
      if (queryExt || timerExt) {
        caps.supportsQueryObjects = true;
        caps.supportsTimerQuery = true;
      }

      // Occlusion query
      const occlusionExt = gl2.getExtension('EXT_occlusion_query_webgl2');
      caps.supportsOcclusionQuery = !!occlusionExt;
    }
  }

  /**
   * Detect platform
   */
  private detectPlatform(): 'MACOS' | 'WINDOWS' | 'LINUX' | 'UNKNOWN' {
    if (typeof navigator === 'undefined') return 'UNKNOWN';

    const ua = navigator.userAgent.toLowerCase();
    if (ua.includes('mac') || ua.includes('darwin')) {
      return 'MACOS';
    } else if (ua.includes('win')) {
      return 'WINDOWS';
    } else if (ua.includes('linux')) {
      return 'LINUX';
    }
    return 'UNKNOWN';
  }

  /**
   * Detect GPU vendor
   */
  private detectGPUVendor(): string {
    try {
      const canvas = this.createOffscreenCanvas();
      const gl =
        canvas.getContext('webgl2') ||
        canvas.getContext('webgl') ||
        (canvas.getContext('experimental-webgl') as any);

      if (!gl) return 'Unknown';

      const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
      if (!debugInfo) return 'Unknown';

      const vendor = gl.getParameter((debugInfo as any).UNMASKED_VENDOR_WEBGL);
      return vendor || 'Unknown';
    } catch (error) {
      return 'Unknown';
    }
  }

  /**
   * Classify device tier
   */
  private classifyDeviceTier(caps: WebGLCapabilities): DeviceTier {
    // HIGH tier: All advanced features
    if (
      caps.supportsWebGL2 &&
      caps.supportsComputeShaders &&
      caps.maxTextureSize >= 4096 &&
      caps.maxColorAttachments >= 4
    ) {
      return 'HIGH';
    }

    // MID tier: WebGL 2.0 with good texture support
    if (
      caps.supportsWebGL2 &&
      caps.maxTextureSize >= 2048 &&
      caps.maxColorAttachments >= 2
    ) {
      return 'MID';
    }

    // LOW tier: Basic WebGL support
    return 'LOW';
  }

  /**
   * Create offscreen canvas
   */
  private createOffscreenCanvas(): HTMLCanvasElement {
    if (typeof document !== 'undefined') {
      const canvas = document.createElement('canvas');
      canvas.width = 1;
      canvas.height = 1;
      return canvas;
    }
    throw new Error('Canvas not supported');
  }

  /**
   * Cleanup resources
   */
  private cleanup(): void {
    if (this.gl) {
      const ext = this.gl.getExtension('WEBGL_lose_context');
      if (ext) {
        (ext as any).loseContext();
      }
    }
    this.gl = null;
    this.canvas = null;
  }

  /**
   * Get cached capabilities
   */
  getCapabilities(): WebGLCapabilities {
    if (!this.capabilities) {
      return this.detectFeatures();
    }
    return this.capabilities;
  }

  /**
   * Check if feature is supported
   */
  supportsFeature(feature: keyof Omit<WebGLCapabilities, 'platform' | 'gpuVendor' | 'deviceTier'>): boolean {
    const caps = this.getCapabilities();
    return caps[feature as any] === true;
  }

  /**
   * Check if platform is macOS
   */
  isMacOS(): boolean {
    return this.getCapabilities().platform === 'MACOS';
  }

  /**
   * Check if platform is Windows
   */
  isWindows(): boolean {
    return this.getCapabilities().platform === 'WINDOWS';
  }

  /**
   * Check if platform is Linux
   */
  isLinux(): boolean {
    return this.getCapabilities().platform === 'LINUX';
  }

  /**
   * Get device tier
   */
  getDeviceTier(): DeviceTier {
    return this.getCapabilities().deviceTier;
  }

  /**
   * Can use compute shaders (Windows/Linux only)
   */
  canUseComputeShaders(): boolean {
    const caps = this.getCapabilities();
    // Only enable compute shaders on non-macOS platforms
    return (
      caps.supportsComputeShaders &&
      caps.supportsShaderStorageBufferObject &&
      !this.isMacOS()
    );
  }

  /**
   * Should use HTTP polling fallback
   */
  shouldUseFallback(): boolean {
    // Use fallback on very low-end devices
    return this.getDeviceTier() === 'LOW';
  }

  /**
   * Get optimal texture size
   */
  getOptimalTextureSize(): number {
    const maxSize = this.getCapabilities().maxTextureSize;
    // Use half the max texture size for safety
    return Math.min(maxSize / 2, 2048);
  }

  /**
   * Get summary string for debugging
   */
  getSummary(): string {
    const caps = this.getCapabilities();
    return `
WebGL Feature Detection Summary:
- WebGL 2.0: ${caps.supportsWebGL2 ? 'YES' : 'NO'}
- WebGL 1.0: ${caps.supportsWebGL1 ? 'YES' : 'NO'}
- Compute Shaders: ${caps.supportsComputeShaders ? 'YES' : 'NO'}
- SSBO: ${caps.supportsShaderStorageBufferObject ? 'YES' : 'NO'}
- Device Tier: ${caps.deviceTier}
- Platform: ${caps.platform}
- GPU Vendor: ${caps.gpuVendor}
- Max Texture Size: ${caps.maxTextureSize}
- Max Color Attachments: ${caps.maxColorAttachments}
    `.trim();
  }
}

/**
 * Global feature detector instance
 */
export const globalFeatureDetector = new FeatureDetector();

/**
 * Convenience function
 */
export function detectWebGLCapabilities(): WebGLCapabilities {
  return globalFeatureDetector.detectFeatures();
}
