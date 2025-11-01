/**
 * Quality Selector: Bridge Between WebGL Features and Rendering Quality
 *
 * Integrates animation/FeatureDetector with DeviceTier to:
 * - Map WebGL capabilities to rendering quality tiers
 * - Apply quality settings during VisualizationManager initialization
 * - Provide quality adjustment hooks for runtime changes
 *
 * Works alongside VisualizationManager for automatic quality selection
 */

import type { WebGLCapabilities } from '../animation/FeatureDetector';
import { classifyDeviceTier, getQualitySummary, type RenderingQuality } from './DeviceTier';

export interface QualitySelector {
  // Selected quality settings based on WebGL capabilities
  quality: RenderingQuality;

  // Device tier (low/medium/high)
  tier: 'low' | 'medium' | 'high';

  // Performance score (0-100)
  performanceScore: number;

  // WebGL capabilities that informed selection
  capabilities: WebGLCapabilities;
}

/**
 * Select rendering quality based on WebGL capabilities
 * This bridges animation/FeatureDetector with DeviceTier quality system
 */
export function selectQualityFromWebGL(capabilities: WebGLCapabilities): QualitySelector {
  // Convert WebGL capabilities to our DeviceTier-compatible format
  const systemCapabilities = {
    gpu: {
      vendor: capabilities.gpuVendor,
      renderer: 'WebGL Device',
      maxTextureSize: capabilities.maxTextureSize,
      maxCubemapSize: capabilities.maxCubeMapTextureSize,
      maxRenderBufferSize: capabilities.maxRenderbufferSize,
      vramEstimate: estimateVRAM(capabilities.maxTextureSize),
      isIntegrated: detectIntegratedGPU(capabilities),
      supportsHighPrecision: capabilities.supportsWebGL2,
    },
    features: {
      anisotropic: false, // Will be expanded if needed
      compression: {
        s3tc: false,
        astc: false,
        bptc: false,
      },
      shadowMapping: false,
      depthTexture: false,
      drawBuffers: capabilities.supportsDrawIndirect || capabilities.maxDrawBuffers > 1,
      instancedArrays: false,
      standardDerivatives: false,
      textureHalf: false,
      textureFloat: false,
      VAO: false,
    },
    cpuCores: navigator.hardwareConcurrency || 4,
    memoryGB: (navigator as any).deviceMemory || 8,
    isLowEnd: capabilities.deviceTier === 'LOW',
  };

  const deviceTier = classifyDeviceTier(systemCapabilities);
  const performanceScore = calculatePerformanceScore(capabilities);

  return {
    quality: deviceTier.quality,
    tier: deviceTier.level,
    performanceScore,
    capabilities,
  };
}

/**
 * Estimate VRAM from max texture size (rough heuristic)
 */
function estimateVRAM(maxTextureSize: number): number {
  if (maxTextureSize >= 4096) return 2048;
  if (maxTextureSize >= 2048) return 1024;
  if (maxTextureSize >= 1024) return 512;
  return 256;
}

/**
 * Detect if GPU is integrated (low-end)
 */
function detectIntegratedGPU(capabilities: WebGLCapabilities): boolean {
  const vendor = capabilities.gpuVendor.toLowerCase();
  return (
    vendor.includes('intel') ||
    vendor.includes('mali') ||
    vendor.includes('adreno') ||
    vendor.includes('power vr')
  );
}

/**
 * Calculate performance score (0-100) from WebGL capabilities
 * Used for device tier determination
 */
function calculatePerformanceScore(capabilities: WebGLCapabilities): number {
  let score = 50;

  // WebGL version support (up to 20 points)
  if (capabilities.supportsWebGL2) score += 20;
  else if (capabilities.supportsWebGL1) score += 10;

  // Texture capabilities (up to 20 points)
  if (capabilities.maxTextureSize >= 4096) score += 20;
  else if (capabilities.maxTextureSize >= 2048) score += 15;
  else if (capabilities.maxTextureSize >= 1024) score += 10;

  // Color attachments (up to 15 points)
  if (capabilities.maxColorAttachments >= 4) score += 15;
  else if (capabilities.maxColorAttachments >= 2) score += 10;
  else score += 5;

  // Advanced features (up to 20 points)
  let featureScore = 0;
  if (capabilities.supportsComputeShaders) featureScore += 10;
  if (capabilities.supportsShaderStorageBufferObject) featureScore += 5;
  if (capabilities.supportsTimerQuery) featureScore += 5;
  score += featureScore;

  // Shader capabilities (up to 10 points)
  if (capabilities.maxVertexUniforms >= 256) score += 5;
  if (capabilities.maxFragmentUniforms >= 256) score += 5;

  return Math.min(100, score);
}

/**
 * Apply rendering quality settings to a Three.js renderer
 * Call this after creating the WebGL context in VisualizationManager
 */
export function applyQualitySettings(
  selector: QualitySelector,
  renderer: any // THREE.WebGLRenderer
): void {
  if (!renderer) return;

  const { quality } = selector;

  // Apply pixel ratio for MSAA simulation
  const pixelRatio =
    quality.msaaSamples === 1 ? 1 : quality.msaaSamples === 2 ? 1.5 : 2;
  renderer.setPixelRatio(Math.min(pixelRatio, window.devicePixelRatio));

  // Tone mapping based on quality
  renderer.toneMappingExposure = quality.postProcessing.bloom ? 1.2 : 1.0;

  // Shadow map resolution
  if (quality.shadowMapSize > 0) {
    renderer.shadowMap.enabled = true;
    renderer.shadowMap.type = 0; // BasicShadowMap for low-end, variance for high-end
  } else {
    renderer.shadowMap.enabled = false;
  }

  console.log(
    `[QualitySelector] Applied quality settings:\n${getQualitySummary(quality)}`
  );
}

/**
 * Create human-readable quality selector report
 */
export function getQualitySelectorReport(selector: QualitySelector): string {
  return `
Quality Selection Report:
  Device Tier: ${selector.tier.toUpperCase()}
  Performance Score: ${selector.performanceScore}/100

  GPU Info:
    - Max Texture Size: ${selector.capabilities.maxTextureSize}
    - Max Renderbuffer: ${selector.capabilities.maxRenderbufferSize}
    - Color Attachments: ${selector.capabilities.maxColorAttachments}
    - Draw Buffers: ${selector.capabilities.maxDrawBuffers}

  WebGL Support:
    - WebGL 2.0: ${selector.capabilities.supportsWebGL2 ? 'YES' : 'NO'}
    - Compute Shaders: ${selector.capabilities.supportsComputeShaders ? 'YES' : 'NO'}
    - Query Objects: ${selector.capabilities.supportsQueryObjects ? 'YES' : 'NO'}

${getQualitySummary(selector.quality)}
  `.trim();
}
