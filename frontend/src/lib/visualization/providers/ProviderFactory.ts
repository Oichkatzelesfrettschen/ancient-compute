/**
 * ProviderFactory.ts
 * Factory for creating and managing visualization providers with auto-detection
 */

import { VisualizationProvider, VisualizationOptions } from './VisualizationProvider';
import { ThreeJSProvider } from './ThreeJSProvider';
import { Canvas2DProvider } from './Canvas2DProvider';

export type ProviderType = 'threejs' | 'canvas2d' | 'auto';

export interface DeviceTier {
  tier: number;
  gpu: string;
  isMobile: boolean;
  fps: number;
}

export interface ProviderFactoryOptions {
  preferredProvider?: ProviderType;
  forceProvider?: boolean;
  detectGPU?: boolean;
  minTier?: number;
  testCanvas?: HTMLCanvasElement;
}

export class ProviderFactory {
  private static instance: ProviderFactory;
  private cachedDeviceTier: DeviceTier | null = null;
  private providerCache: Map<ProviderType, VisualizationProvider> = new Map();

  private constructor() {}

  static getInstance(): ProviderFactory {
    if (!ProviderFactory.instance) {
      ProviderFactory.instance = new ProviderFactory();
    }
    return ProviderFactory.instance;
  }

  /**
   * Create a visualization provider with auto-detection
   */
  async createProvider(
    canvas: HTMLCanvasElement,
    options: VisualizationOptions,
    factoryOptions: ProviderFactoryOptions = {}
  ): Promise<VisualizationProvider> {
    const providerType = await this.determineProviderType(canvas, factoryOptions);

    // Check cache first
    if (this.providerCache.has(providerType)) {
      const cached = this.providerCache.get(providerType)!;
      await cached.initialize(canvas, options);
      return cached;
    }

    // Create new provider
    const provider = this.instantiateProvider(providerType);

    try {
      await provider.initialize(canvas, options);
      this.providerCache.set(providerType, provider);
      return provider;
    } catch (error) {
      console.error(`Failed to initialize ${providerType} provider:`, error);

      // Fallback to Canvas2D if Three.js fails
      if (providerType === 'threejs' && !factoryOptions.forceProvider) {
        console.log('Falling back to Canvas2D provider');
        return this.createCanvas2DFallback(canvas, options);
      }

      throw error;
    }
  }

  /**
   * Determine which provider to use based on device capabilities
   */
  private async determineProviderType(
    canvas: HTMLCanvasElement,
    options: ProviderFactoryOptions
  ): Promise<ProviderType> {
    // If force provider is set, use it
    if (options.forceProvider && options.preferredProvider && options.preferredProvider !== 'auto') {
      return options.preferredProvider;
    }

    // If preferred provider is set and not auto, try it first
    if (options.preferredProvider && options.preferredProvider !== 'auto') {
      const canUse = await this.canUseProvider(options.preferredProvider, canvas);
      if (canUse) {
        return options.preferredProvider;
      }
    }

    // Auto-detect best provider
    return this.detectBestProvider(canvas, options);
  }

  /**
   * Auto-detect the best provider based on device capabilities
   */
  private async detectBestProvider(
    canvas: HTMLCanvasElement,
    options: ProviderFactoryOptions
  ): Promise<ProviderType> {
    // Check WebGL support
    if (!this.hasWebGLSupport(canvas)) {
      console.log('No WebGL support detected, using Canvas2D');
      return 'canvas2d';
    }

    // Get device tier
    const deviceTier = await this.getDeviceTier(canvas, options.detectGPU !== false);

    // Check against minimum tier requirement
    const minTier = options.minTier ?? 2;
    if (deviceTier.tier < minTier) {
      console.log(`Device tier ${deviceTier.tier} below minimum ${minTier}, using Canvas2D`);
      return 'canvas2d';
    }

    // Mobile devices default to Canvas2D unless high-end
    if (deviceTier.isMobile && deviceTier.tier < 3) {
      console.log('Mobile device detected, using Canvas2D for better performance');
      return 'canvas2d';
    }

    // Check for software rendering
    if (this.isSoftwareRendering(canvas)) {
      console.log('Software rendering detected, using Canvas2D');
      return 'canvas2d';
    }

    // Default to Three.js for capable devices
    console.log(`Using Three.js provider (tier: ${deviceTier.tier}, GPU: ${deviceTier.gpu})`);
    return 'threejs';
  }

  /**
   * Check if a specific provider can be used
   */
  private async canUseProvider(type: ProviderType, canvas: HTMLCanvasElement): Promise<boolean> {
    switch (type) {
      case 'threejs':
        return this.hasWebGLSupport(canvas) && !this.isSoftwareRendering(canvas);
      case 'canvas2d':
        return this.hasCanvas2DSupport(canvas);
      default:
        return false;
    }
  }

  /**
   * Check for WebGL support
   */
  private hasWebGLSupport(canvas: HTMLCanvasElement): boolean {
    try {
      const testCanvas = canvas || document.createElement('canvas');
      const gl = testCanvas.getContext('webgl2') || testCanvas.getContext('webgl');
      return !!gl;
    } catch (e) {
      return false;
    }
  }

  /**
   * Check for Canvas2D support
   */
  private hasCanvas2DSupport(canvas: HTMLCanvasElement): boolean {
    try {
      const testCanvas = canvas || document.createElement('canvas');
      const ctx = testCanvas.getContext('2d');
      return !!ctx;
    } catch (e) {
      return false;
    }
  }

  /**
   * Check if WebGL is using software rendering
   */
  private isSoftwareRendering(canvas: HTMLCanvasElement): boolean {
    try {
      const testCanvas = canvas || document.createElement('canvas');
      const gl = testCanvas.getContext('webgl2') || testCanvas.getContext('webgl');

      if (!gl) return true;

      const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
      if (!debugInfo) return false;

      const renderer = gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL);
      const vendor = gl.getParameter(debugInfo.UNMASKED_VENDOR_WEBGL);

      // Check for known software renderers
      const softwareRenderers = [
        'SwiftShader',
        'Software Rasterizer',
        'Microsoft Basic Render Driver',
        'llvmpipe',
        'softpipe',
        'Mesa'
      ];

      const rendererString = String(renderer).toLowerCase();
      const vendorString = String(vendor).toLowerCase();

      return softwareRenderers.some(sr =>
        rendererString.includes(sr.toLowerCase()) ||
        vendorString.includes(sr.toLowerCase())
      );
    } catch (e) {
      return false;
    }
  }

  /**
   * Get device tier based on GPU and performance
   */
  private async getDeviceTier(canvas: HTMLCanvasElement, detectGPU: boolean = true): Promise<DeviceTier> {
    // Return cached result if available
    if (this.cachedDeviceTier) {
      return this.cachedDeviceTier;
    }

    let tier = 2; // Default to medium
    let gpu = 'Unknown';
    const isMobile = this.isMobileDevice();

    if (detectGPU) {
      try {
        const testCanvas = canvas || document.createElement('canvas');
        const gl = testCanvas.getContext('webgl2') || testCanvas.getContext('webgl');

        if (gl) {
          const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
          if (debugInfo) {
            gpu = gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL);
            tier = this.getGPUTier(gpu);
          }
        }
      } catch (e) {
        console.warn('Failed to detect GPU:', e);
      }
    }

    // Test actual rendering performance
    const fps = await this.testRenderingPerformance(canvas);

    // Adjust tier based on FPS
    if (fps < 30) {
      tier = Math.max(1, tier - 1);
    } else if (fps >= 60) {
      tier = Math.min(3, tier + 1);
    }

    // Mobile penalty
    if (isMobile) {
      tier = Math.max(1, tier - 1);
    }

    this.cachedDeviceTier = { tier, gpu, isMobile, fps };
    return this.cachedDeviceTier;
  }

  /**
   * Determine GPU tier based on name
   */
  private getGPUTier(gpu: string): number {
    const gpuString = gpu.toLowerCase();

    // High-end GPUs (Tier 3)
    const highEndPatterns = [
      /rtx\s*(30[789]0|40[789]0)/,
      /radeon\s*rx\s*(6[89]00|7900)/,
      /m[123]\s*(pro|max|ultra)/,
      /a1[567]/
    ];

    // Low-end GPUs (Tier 1)
    const lowEndPatterns = [
      /intel\s*(hd|uhd)\s*graphics/,
      /geforce\s*gt\s/,
      /radeon\s*r[57]/,
      /mali/,
      /adreno\s*[234]/
    ];

    if (highEndPatterns.some(pattern => pattern.test(gpuString))) {
      return 3;
    }

    if (lowEndPatterns.some(pattern => pattern.test(gpuString))) {
      return 1;
    }

    return 2; // Default to medium
  }

  /**
   * Test actual rendering performance
   */
  private async testRenderingPerformance(canvas: HTMLCanvasElement): Promise<number> {
    return new Promise((resolve) => {
      const testCanvas = canvas || document.createElement('canvas');
      testCanvas.width = 800;
      testCanvas.height = 600;

      const ctx = testCanvas.getContext('2d');
      if (!ctx) {
        resolve(30);
        return;
      }

      let frames = 0;
      const startTime = performance.now();
      const duration = 1000; // Test for 1 second

      const renderFrame = () => {
        // Simple rendering test
        ctx.fillStyle = `hsl(${frames % 360}, 50%, 50%)`;
        ctx.fillRect(0, 0, testCanvas.width, testCanvas.height);

        for (let i = 0; i < 100; i++) {
          ctx.beginPath();
          ctx.arc(
            Math.random() * testCanvas.width,
            Math.random() * testCanvas.height,
            Math.random() * 20,
            0,
            Math.PI * 2
          );
          ctx.fill();
        }

        frames++;

        if (performance.now() - startTime < duration) {
          requestAnimationFrame(renderFrame);
        } else {
          const fps = Math.round(frames / (duration / 1000));
          resolve(fps);
        }
      };

      requestAnimationFrame(renderFrame);
    });
  }

  /**
   * Check if running on mobile device
   */
  private isMobileDevice(): boolean {
    // Check via user agent
    const userAgent = navigator.userAgent.toLowerCase();
    const mobilePatterns = /android|webos|iphone|ipad|ipod|blackberry|iemobile|opera mini/;

    if (mobilePatterns.test(userAgent)) {
      return true;
    }

    // Check via touch support and screen size
    const hasTouch = 'ontouchstart' in window || navigator.maxTouchPoints > 0;
    const smallScreen = window.innerWidth <= 768;

    return hasTouch && smallScreen;
  }

  /**
   * Instantiate a provider of the given type
   */
  private instantiateProvider(type: ProviderType): VisualizationProvider {
    switch (type) {
      case 'threejs':
        return new ThreeJSProvider();
      case 'canvas2d':
        return new Canvas2DProvider();
      default:
        throw new Error(`Unknown provider type: ${type}`);
    }
  }

  /**
   * Create Canvas2D fallback provider
   */
  private async createCanvas2DFallback(
    canvas: HTMLCanvasElement,
    options: VisualizationOptions
  ): Promise<VisualizationProvider> {
    const provider = new Canvas2DProvider();
    await provider.initialize(canvas, options);
    this.providerCache.set('canvas2d', provider);
    return provider;
  }

  /**
   * Clear provider cache
   */
  clearCache(): void {
    this.providerCache.forEach(provider => provider.dispose());
    this.providerCache.clear();
    this.cachedDeviceTier = null;
  }

  /**
   * Get device capabilities report
   */
  async getDeviceCapabilities(canvas?: HTMLCanvasElement): Promise<{
    webgl: boolean;
    webgl2: boolean;
    canvas2d: boolean;
    gpu: string;
    tier: number;
    isMobile: boolean;
    fps: number;
    softwareRendering: boolean;
  }> {
    const testCanvas = canvas || document.createElement('canvas');
    const deviceTier = await this.getDeviceTier(testCanvas);

    return {
      webgl: this.hasWebGLSupport(testCanvas),
      webgl2: !!testCanvas.getContext('webgl2'),
      canvas2d: this.hasCanvas2DSupport(testCanvas),
      gpu: deviceTier.gpu,
      tier: deviceTier.tier,
      isMobile: deviceTier.isMobile,
      fps: deviceTier.fps,
      softwareRendering: this.isSoftwareRendering(testCanvas)
    };
  }
}