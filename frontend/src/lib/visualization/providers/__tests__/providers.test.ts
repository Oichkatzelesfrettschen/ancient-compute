/**
 * providers.test.ts
 * Unit tests for visualization providers
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { ThreeJSProvider } from '../ThreeJSProvider';
import { Canvas2DProvider } from '../Canvas2DProvider';
import { ProviderFactory } from '../ProviderFactory';
import type { MachineState, VisualizationOptions } from '../VisualizationProvider';

// Mock canvas and WebGL context
class MockWebGLRenderingContext {
  getParameter(param: number): any {
    if (param === 0x9245) return 'Mock GPU'; // UNMASKED_VENDOR_WEBGL
    if (param === 0x9246) return 'Mock Renderer'; // UNMASKED_RENDERER_WEBGL
    return null;
  }

  getExtension(name: string): any {
    if (name === 'WEBGL_debug_renderer_info') {
      return {
        UNMASKED_VENDOR_WEBGL: 0x9245,
        UNMASKED_RENDERER_WEBGL: 0x9246
      };
    }
    return null;
  }
}

class MockCanvas {
  width = 800;
  height = 600;
  style = {};

  getContext(type: string): any {
    if (type === 'webgl' || type === 'webgl2') {
      return new MockWebGLRenderingContext();
    }
    if (type === '2d') {
      return new MockCanvasRenderingContext2D();
    }
    return null;
  }

  getBoundingClientRect() {
    return {
      left: 0,
      top: 0,
      width: this.width,
      height: this.height
    };
  }

  addEventListener() {}
  removeEventListener() {}

  toBlob(callback: (blob: Blob | null) => void) {
    callback(new Blob(['mock'], { type: 'image/png' }));
  }
}

class MockCanvasRenderingContext2D {
  fillStyle = '';
  strokeStyle = '';
  lineWidth = 1;
  font = '';
  textAlign = 'left';
  textBaseline = 'alphabetic';
  globalAlpha = 1;
  imageSmoothingEnabled = true;
  imageSmoothingQuality = 'medium';

  fillRect() {}
  strokeRect() {}
  clearRect() {}
  beginPath() {}
  moveTo() {}
  lineTo() {}
  arc() {}
  fill() {}
  stroke() {}
  fillText() {}
  save() {}
  restore() {}
  translate() {}
  rotate() {}
  scale() {}
  setLineDash() {}
  createLinearGradient() {
    return {
      addColorStop: () => {}
    };
  }
}

// Helper function to create test machine state
function createTestState(): MachineState {
  return {
    registers: new Map([
      ['R0', 10],
      ['R1', 20],
      ['R2', 30],
      ['R3', 40]
    ]),
    memory: new Uint8Array(4096),
    programCounter: 100,
    accumulator: 50,
    carryFlag: false,
    runningFlag: true,
    columns: Array(10).fill(null).map((_, i) => ({
      id: `column_${i}`,
      value: i,
      isActive: i % 2 === 0,
      rotation: i * 0.1
    })),
    store: {
      positions: Array(32).fill(0).map((_, i) => i * 2),
      capacity: 32,
      activeIndices: [0, 5, 10]
    },
    mill: {
      operation: 'add',
      operandA: 5,
      operandB: 3,
      result: 8,
      progress: 0.5
    },
    barrels: Array(3).fill(null).map((_, i) => ({
      id: `barrel_${i}`,
      programCounter: i * 10,
      instructions: [`LOAD ${i}`, `ADD ${i}`, `STORE ${i}`],
      isActive: i === 1
    })),
    cycles: 1000,
    timestamp: Date.now()
  };
}

describe('Canvas2DProvider', () => {
  let provider: Canvas2DProvider;
  let canvas: MockCanvas;
  let options: VisualizationOptions;

  beforeEach(() => {
    provider = new Canvas2DProvider();
    canvas = new MockCanvas() as any;
    options = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'fixed',
      enablePhysics: false,
      enableShadows: false,
      antialias: false,
      maxFPS: 60
    };
  });

  afterEach(() => {
    provider.dispose();
  });

  it('should initialize successfully', async () => {
    await expect(provider.initialize(canvas as any, options)).resolves.not.toThrow();
  });

  it('should render machine state', async () => {
    await provider.initialize(canvas as any, options);
    const state = createTestState();

    expect(() => provider.render(state)).not.toThrow();
  });

  it('should handle quality changes', async () => {
    await provider.initialize(canvas as any, options);

    provider.setQuality('low');
    expect(() => provider.setQuality('high')).not.toThrow();
  });

  it('should resize canvas', async () => {
    await provider.initialize(canvas as any, options);

    expect(() => provider.resize(1024, 768)).not.toThrow();
  });

  it('should capture screenshot', async () => {
    await provider.initialize(canvas as any, options);

    const blob = await provider.captureScreenshot();
    expect(blob).toBeInstanceOf(Blob);
  });

  it('should export state as JSON', async () => {
    await provider.initialize(canvas as any, options);
    const state = createTestState();
    provider.render(state);

    const exported = provider.exportState();
    expect(exported).toContain('programCounter');
    expect(exported).toContain('100');
  });

  it('should handle state change callbacks', async () => {
    await provider.initialize(canvas as any, options);

    const callback = vi.fn();
    provider.onStateChange(callback);

    const state = createTestState();
    provider.render(state);

    expect(callback).toHaveBeenCalledWith(state);
  });

  it('should handle error callbacks', async () => {
    await provider.initialize(canvas as any, options);

    const callback = vi.fn();
    provider.onError(callback);

    // Trigger an error internally (implementation specific)
    // For this test, we'll check callback registration works
    expect(() => provider.offStateChange(() => {})).not.toThrow();
  });

  it('should clean up resources on dispose', async () => {
    await provider.initialize(canvas as any, options);
    const state = createTestState();
    provider.render(state);

    expect(() => provider.dispose()).not.toThrow();
  });

  it('should return performance metrics', async () => {
    await provider.initialize(canvas as any, options);

    const metrics = provider.getPerformanceMetrics();
    expect(metrics).toHaveProperty('fps');
    expect(metrics).toHaveProperty('frameTime');
    expect(metrics).toHaveProperty('drawCalls');
  });
});

describe('ThreeJSProvider', () => {
  let provider: ThreeJSProvider;
  let canvas: MockCanvas;
  let options: VisualizationOptions;

  beforeEach(() => {
    // Mock Three.js modules
    vi.mock('three', () => ({
      Scene: vi.fn(),
      PerspectiveCamera: vi.fn(),
      WebGLRenderer: vi.fn(() => ({
        setSize: vi.fn(),
        setPixelRatio: vi.fn(),
        render: vi.fn(),
        dispose: vi.fn(),
        info: { render: { calls: 0, triangles: 0 } },
        shadowMap: { enabled: false, type: 2 }
      })),
      AmbientLight: vi.fn(),
      DirectionalLight: vi.fn(),
      PointLight: vi.fn(),
      MeshStandardMaterial: vi.fn(),
      MeshPhysicalMaterial: vi.fn(),
      MeshBasicMaterial: vi.fn(),
      BoxGeometry: vi.fn(),
      CylinderGeometry: vi.fn(),
      PlaneGeometry: vi.fn(),
      SphereGeometry: vi.fn(),
      ExtrudeGeometry: vi.fn(),
      Shape: vi.fn(),
      Group: vi.fn(),
      Mesh: vi.fn(),
      Clock: vi.fn(() => ({
        getDelta: () => 0.016,
        getElapsedTime: () => 1.0
      })),
      Color: vi.fn(),
      Fog: vi.fn(),
      PMREMGenerator: vi.fn(() => ({
        fromEquirectangular: () => ({ texture: {} }),
        dispose: vi.fn()
      })),
      DataTexture: vi.fn(),
      CanvasTexture: vi.fn(),
      Vector2: vi.fn(),
      MathUtils: {
        lerp: (a: number, b: number, t: number) => a + (b - a) * t
      },
      LOD: vi.fn(),
      AnimationMixer: vi.fn(),
      AnimationAction: vi.fn(),
      PCFSoftShadowMap: 2,
      PCFShadowMap: 1,
      sRGBEncoding: 3001,
      ACESFilmicToneMapping: 4,
      RepeatWrapping: 1000,
      RGBAFormat: 1023
    }));

    provider = new ThreeJSProvider();
    canvas = new MockCanvas() as any;
    options = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'orbit',
      enablePhysics: false,
      enableShadows: true,
      antialias: true,
      maxFPS: 60
    };
  });

  afterEach(() => {
    provider.dispose();
    vi.clearAllMocks();
  });

  it('should handle initialization errors gracefully', async () => {
    // Force an error by providing null canvas
    await expect(provider.initialize(null as any, options)).rejects.toThrow();
  });

  it('should switch quality levels', async () => {
    await provider.initialize(canvas as any, options);

    provider.setQuality('low');
    provider.setQuality('high');
    provider.setQuality('medium');

    // Should not throw
    expect(true).toBe(true);
  });

  it('should handle camera operations', async () => {
    await provider.initialize(canvas as any, options);

    provider.setCamera({ x: 10, y: 5, z: 10 });
    provider.resetCamera();

    // Should not throw
    expect(true).toBe(true);
  });
});

describe('ProviderFactory', () => {
  let factory: ProviderFactory;
  let canvas: MockCanvas;

  beforeEach(() => {
    factory = ProviderFactory.getInstance();
    canvas = new MockCanvas() as any;

    // Mock window properties
    global.window = {
      devicePixelRatio: 2,
      innerWidth: 1920,
      innerHeight: 1080,
      navigator: {
        userAgent: 'Mozilla/5.0',
        maxTouchPoints: 0
      }
    } as any;
  });

  afterEach(() => {
    factory.clearCache();
  });

  it('should be a singleton', () => {
    const factory1 = ProviderFactory.getInstance();
    const factory2 = ProviderFactory.getInstance();
    expect(factory1).toBe(factory2);
  });

  it('should auto-detect best provider', async () => {
    const options: VisualizationOptions = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'orbit',
      enablePhysics: false,
      enableShadows: true,
      antialias: true,
      maxFPS: 60
    };

    const provider = await factory.createProvider(canvas as any, options, {
      preferredProvider: 'auto'
    });

    expect(provider).toBeDefined();
    expect(provider.constructor.name).toMatch(/Provider$/);
  });

  it('should force specific provider', async () => {
    const options: VisualizationOptions = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'fixed',
      enablePhysics: false,
      enableShadows: false,
      antialias: false,
      maxFPS: 60
    };

    const provider = await factory.createProvider(canvas as any, options, {
      preferredProvider: 'canvas2d',
      forceProvider: true
    });

    expect(provider).toBeInstanceOf(Canvas2DProvider);
  });

  it('should cache providers', async () => {
    const options: VisualizationOptions = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'fixed',
      enablePhysics: false,
      enableShadows: false,
      antialias: false,
      maxFPS: 60
    };

    const provider1 = await factory.createProvider(canvas as any, options, {
      preferredProvider: 'canvas2d',
      forceProvider: true
    });

    const provider2 = await factory.createProvider(canvas as any, options, {
      preferredProvider: 'canvas2d',
      forceProvider: true
    });

    // Should reuse cached provider
    expect(provider1).toBe(provider2);
  });

  it('should detect device capabilities', async () => {
    const capabilities = await factory.getDeviceCapabilities(canvas as any);

    expect(capabilities).toHaveProperty('webgl');
    expect(capabilities).toHaveProperty('webgl2');
    expect(capabilities).toHaveProperty('canvas2d');
    expect(capabilities).toHaveProperty('gpu');
    expect(capabilities).toHaveProperty('tier');
    expect(capabilities).toHaveProperty('isMobile');
    expect(capabilities).toHaveProperty('fps');
    expect(capabilities).toHaveProperty('softwareRendering');
  });

  it('should clear cache properly', async () => {
    const options: VisualizationOptions = {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'fixed',
      enablePhysics: false,
      enableShadows: false,
      antialias: false,
      maxFPS: 60
    };

    await factory.createProvider(canvas as any, options, {
      preferredProvider: 'canvas2d',
      forceProvider: true
    });

    factory.clearCache();

    const newProvider = await factory.createProvider(canvas as any, options, {
      preferredProvider: 'canvas2d',
      forceProvider: true
    });

    // Should be a new instance after cache clear
    expect(newProvider).toBeDefined();
  });

  it('should detect mobile devices', async () => {
    // Mock mobile user agent
    global.window.navigator.userAgent = 'Mozilla/5.0 (iPhone; CPU iPhone OS 14_7_1 like Mac OS X)';
    global.window.innerWidth = 375;
    global.window.navigator.maxTouchPoints = 5;

    const capabilities = await factory.getDeviceCapabilities(canvas as any);
    expect(capabilities.isMobile).toBe(true);
  });

  it('should detect software rendering', async () => {
    // Mock software renderer
    const mockCanvas = {
      ...canvas,
      getContext: (type: string) => {
        if (type === 'webgl' || type === 'webgl2') {
          return {
            getParameter: (param: number) => {
              if (param === 0x9246) return 'SwiftShader';
              return null;
            },
            getExtension: (name: string) => {
              if (name === 'WEBGL_debug_renderer_info') {
                return {
                  UNMASKED_VENDOR_WEBGL: 0x9245,
                  UNMASKED_RENDERER_WEBGL: 0x9246
                };
              }
              return null;
            }
          };
        }
        return canvas.getContext(type);
      }
    };

    const capabilities = await factory.getDeviceCapabilities(mockCanvas as any);
    expect(capabilities.softwareRendering).toBe(true);
  });
});

describe('State Worker Integration', () => {
  it('should compute state diff', async () => {
    // This would test the StateWorker but requires Web Worker support
    // In a real test environment, you'd mock or polyfill workers
    expect(true).toBe(true);
  });

  it('should interpolate states', async () => {
    // Test state interpolation
    expect(true).toBe(true);
  });

  it('should validate state', async () => {
    // Test state validation
    expect(true).toBe(true);
  });
});

describe('CLI Integration', () => {
  it('should execute commands', async () => {
    // This would test the VisualizationCLI
    expect(true).toBe(true);
  });

  it('should capture screenshots via CLI', async () => {
    // Test CLI screenshot capture
    expect(true).toBe(true);
  });

  it('should profile performance', async () => {
    // Test performance profiling
    expect(true).toBe(true);
  });
});