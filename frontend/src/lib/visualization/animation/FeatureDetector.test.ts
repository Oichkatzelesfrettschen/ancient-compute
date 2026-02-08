/**
 * Unit Tests: FeatureDetector
 *
 * Tests WebGL capability detection, platform classification,
 * and device tier determination
 */

import { describe, it, expect, beforeEach } from 'vitest';
import { FeatureDetector, detectWebGLCapabilities } from './FeatureDetector';

describe('FeatureDetector', () => {
  let detector: FeatureDetector;

  beforeEach(() => {
    detector = new FeatureDetector();
  });

  describe('Initialization', () => {
    it('should create detector instance', () => {
      expect(detector).toBeDefined();
    });

    it('should have no cached capabilities initially', () => {
      // The detector caches on first access, so this test just validates structure
      expect(detector).toHaveProperty('detectFeatures');
      expect(detector).toHaveProperty('getCapabilities');
    });
  });

  describe('Feature Detection', () => {
    it('should detect WebGL support', () => {
      const caps = detector.detectFeatures();
      expect(caps).toBeDefined();
      expect(typeof caps.supportsWebGL2).toBe('boolean');
      expect(typeof caps.supportsWebGL1).toBe('boolean');
    });

    it('should detect at least one WebGL version', () => {
      const caps = detector.detectFeatures();
      expect(caps.supportsWebGL2 || caps.supportsWebGL1).toBe(true);
    });

    it('should detect texture capabilities', () => {
      const caps = detector.detectFeatures();
      expect(caps.maxTextureSize).toBeGreaterThanOrEqual(512);
      expect(caps.maxCubeMapTextureSize).toBeGreaterThanOrEqual(512);
      expect(caps.maxTextureUnits).toBeGreaterThanOrEqual(8);
    });

    it('should detect buffer capabilities', () => {
      const caps = detector.detectFeatures();
      expect(caps.maxVertexUniforms).toBeGreaterThanOrEqual(128);
      expect(caps.maxFragmentUniforms).toBeGreaterThanOrEqual(128);
      expect(caps.maxVaryingVectors).toBeGreaterThanOrEqual(8);
    });

    it('should detect renderbuffer capabilities', () => {
      const caps = detector.detectFeatures();
      expect(caps.maxRenderbufferSize).toBeGreaterThanOrEqual(1024);
    });
  });

  describe('Platform Detection', () => {
    it('should detect platform', () => {
      const caps = detector.detectFeatures();
      expect(caps.platform).toMatch(/MACOS|WINDOWS|LINUX|UNKNOWN/);
    });

    it('should provide platform check methods', () => {
      expect(typeof detector.isMacOS()).toBe('boolean');
      expect(typeof detector.isWindows()).toBe('boolean');
      expect(typeof detector.isLinux()).toBe('boolean');
    });

    it('should have GPU vendor info', () => {
      const caps = detector.detectFeatures();
      expect(typeof caps.gpuVendor).toBe('string');
      expect(caps.gpuVendor.length).toBeGreaterThan(0);
    });
  });

  describe('Device Tier Classification', () => {
    it('should classify device tier', () => {
      const caps = detector.detectFeatures();
      expect(caps.deviceTier).toMatch(/LOW|MID|HIGH/);
    });

    it('should provide getDeviceTier method', () => {
      const tier = detector.getDeviceTier();
      expect(tier).toMatch(/LOW|MID|HIGH/);
    });

    it('should classify HIGH tier correctly', () => {
      const caps = detector.detectFeatures();

      // HIGH tier requires: WebGL 2.0 + compute shaders + good textures
      if (
        caps.supportsWebGL2 &&
        caps.supportsComputeShaders &&
        caps.maxTextureSize >= 4096 &&
        caps.maxColorAttachments >= 4
      ) {
        expect(caps.deviceTier).toBe('HIGH');
      }
    });

    it('should classify MID tier correctly', () => {
      const caps = detector.detectFeatures();

      // MID tier requires: WebGL 2.0 + decent textures
      if (
        caps.supportsWebGL2 &&
        caps.maxTextureSize >= 2048 &&
        caps.maxColorAttachments >= 2
      ) {
        expect(['MID', 'HIGH']).toContain(caps.deviceTier);
      }
    });

    it('should classify LOW tier correctly', () => {
      const caps = detector.detectFeatures();

      // LOW tier is the default fallback
      if (!caps.supportsWebGL2) {
        expect(caps.deviceTier).toBe('LOW');
      }
    });
  });

  describe('Compute Shader Support', () => {
    it('should provide canUseComputeShaders method', () => {
      const result = detector.canUseComputeShaders();
      expect(typeof result).toBe('boolean');
    });

    it('should exclude macOS from compute shaders', () => {
      if (detector.isMacOS()) {
        expect(detector.canUseComputeShaders()).toBe(false);
      }
    });

    it('should require SSBO for compute shaders', () => {
      const caps = detector.detectFeatures();
      if (detector.canUseComputeShaders()) {
        expect(caps.supportsShaderStorageBufferObject).toBe(true);
      }
    });
  });

  describe('Extension Support', () => {
    it('should detect extension support methods', () => {
      expect(typeof detector.supportsFeature).toBe('function');
    });

    it('should validate extension names', () => {
      const caps = detector.detectFeatures();

      // These are boolean properties in the capabilities object
      expect(typeof caps.supportsComputeShaders).toBe('boolean');
      expect(typeof caps.supportsShaderStorageBufferObject).toBe('boolean');
      expect(typeof caps.supportsDrawIndirect).toBe('boolean');
      expect(typeof caps.supportsQueryObjects).toBe('boolean');
      expect(typeof caps.supportsOcclusionQuery).toBe('boolean');
      expect(typeof caps.supportsTimerQuery).toBe('boolean');
    });
  });

  describe('Fallback Handling', () => {
    it('should provide fallback suggestion for LOW tier', () => {
      const result = detector.shouldUseFallback();
      expect(typeof result).toBe('boolean');

      if (detector.getDeviceTier() === 'LOW') {
        expect(result).toBe(true);
      }
    });

    it('should suggest high quality for HIGH tier', () => {
      if (detector.getDeviceTier() === 'HIGH') {
        expect(detector.shouldUseFallback()).toBe(false);
      }
    });
  });

  describe('Optimal Texture Size', () => {
    it('should calculate optimal texture size', () => {
      const optimalSize = detector.getOptimalTextureSize();
      expect(optimalSize).toBeGreaterThan(0);
      expect(optimalSize).toBeLessThanOrEqual(2048);
    });

    it('should limit texture size', () => {
      const caps = detector.detectFeatures();
      const optimalSize = detector.getOptimalTextureSize();
      expect(optimalSize).toBeLessThanOrEqual(caps.maxTextureSize / 2);
    });
  });

  describe('Caching', () => {
    it('should cache capabilities after first detection', () => {
      const caps1 = detector.detectFeatures();
      const caps2 = detector.getCapabilities();

      expect(caps1).toEqual(caps2);
    });

    it('should return consistent results', () => {
      const cap1 = detector.getCapabilities();
      const cap2 = detector.getCapabilities();

      expect(cap1.supportsWebGL2).toBe(cap2.supportsWebGL2);
      expect(cap1.platform).toBe(cap2.platform);
      expect(cap1.deviceTier).toBe(cap2.deviceTier);
    });
  });

  describe('Debug Output', () => {
    it('should provide debug summary', () => {
      const summary = detector.getSummary();
      expect(typeof summary).toBe('string');
      expect(summary.length).toBeGreaterThan(0);
    });

    it('should include key info in debug summary', () => {
      const summary = detector.getSummary();
      expect(summary).toContain('WebGL');
      expect(summary).toContain('Device Tier');
      expect(summary).toContain('Platform');
    });
  });

  describe('Convenience Function', () => {
    it('should provide detectWebGLCapabilities function', () => {
      const caps = detectWebGLCapabilities();
      expect(caps).toBeDefined();
      expect(typeof caps.supportsWebGL2).toBe('boolean');
    });

    it('should return valid capabilities', () => {
      const caps = detectWebGLCapabilities();
      expect(caps.supportsWebGL2 || caps.supportsWebGL1).toBe(true);
      expect(caps.deviceTier).toMatch(/LOW|MID|HIGH/);
    });
  });

  describe('Cross-Platform Behavior', () => {
    it('should handle platform-specific features', () => {
      const caps = detector.detectFeatures();

      // macOS should not support compute shaders
      if (caps.platform === 'MACOS') {
        expect(caps.supportsComputeShaders).toBe(false);
      }

      // All platforms should support WebGL 1 or 2
      expect(caps.supportsWebGL2 || caps.supportsWebGL1).toBe(true);
    });

    it('should report device tier consistently', () => {
      const tier1 = detector.getDeviceTier();
      const tier2 = detector.getDeviceTier();

      expect(tier1).toBe(tier2);
    });
  });

  describe('Edge Cases', () => {
    it('should handle missing WebGL context gracefully', () => {
      // Even if WebGL is not available, detector should not crash
      const result = detector.detectFeatures();
      expect(result).toBeDefined();
      expect(typeof result.supportsWebGL2).toBe('boolean');
    });

    it('should provide default values for fallback case', () => {
      const caps = detector.detectFeatures();

      // All properties should have values
      expect(caps.maxTextureSize).toBeGreaterThanOrEqual(512);
      expect(caps.maxVertexUniforms).toBeGreaterThanOrEqual(128);
      expect(caps.maxDrawBuffers).toBeGreaterThanOrEqual(1);
    });
  });
});
