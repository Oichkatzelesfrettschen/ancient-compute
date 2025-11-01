/**
 * Device Tier Classification and Quality Settings
 *
 * Classifies devices into performance tiers:
 * - LOW: Integrated GPUs, < 1GB VRAM, < 4 CPU cores
 * - MEDIUM: Mid-range discrete GPUs, 1-4GB VRAM, 4-8 cores
 * - HIGH: High-end discrete GPUs, 4GB+ VRAM, 8+ cores
 *
 * Each tier defines rendering quality parameters to maintain
 * smooth animation (target 60 FPS) on that class of hardware
 */

import type { SystemCapabilities } from './FeatureDetector';

export type DeviceTierLevel = 'low' | 'medium' | 'high';

export interface RenderingQuality {
  // Shadow map resolution (0 = disabled, 512, 1024, 2048)
  shadowMapSize: number;

  // Reflection quality (none, low, medium, high)
  reflectionQuality: 'none' | 'low' | 'medium' | 'high';

  // Anti-aliasing samples (1 = none, 2, 4, 8)
  msaaSamples: number;

  // Texture filtering (linear, anisotropic)
  useAnisotropicFiltering: boolean;

  // Maximum texture resolution (512, 1024, 2048, 4096)
  maxTextureResolution: number;

  // Particle count multiplier (0.5 = half, 1.0 = normal, 2.0 = double)
  particleCountMultiplier: number;

  // Animation frame rate cap (60, 90, 120, 144)
  targetFrameRate: number;

  // Geometry detail level (0.5 = low, 1.0 = normal, 1.5 = high)
  geometryDetailLevel: number;

  // Post-processing effects enabled
  postProcessing: {
    bloom: boolean;
    chromatic: boolean;
    vignette: boolean;
    motionBlur: boolean;
  };
}

export interface DeviceTier {
  level: DeviceTierLevel;
  score: number;
  quality: RenderingQuality;
  description: string;
}

/**
 * Determine device tier from system capabilities
 */
export function classifyDeviceTier(capabilities: SystemCapabilities): DeviceTier {
  const score = calculateScore(capabilities);

  if (capabilities.isLowEnd || score < 35) {
    return {
      level: 'low',
      score,
      quality: getLowQualitySettings(capabilities),
      description:
        'Low-end device: Integrated GPU, limited VRAM. Reduced quality for stable 60 FPS.',
    };
  } else if (score < 70) {
    return {
      level: 'medium',
      score,
      quality: getMediumQualitySettings(capabilities),
      description: 'Mid-range device: Balanced quality and performance for 60 FPS.',
    };
  } else {
    return {
      level: 'high',
      score,
      quality: getHighQualitySettings(capabilities),
      description:
        'High-end device: Full quality features. Capable of 90+ FPS with advanced effects.',
    };
  }
}

/**
 * Calculate performance score (0-100)
 * Used to determine device tier thresholds
 */
function calculateScore(capabilities: SystemCapabilities): number {
  let score = 50;

  // GPU VRAM (up to 25 points)
  if (capabilities.gpu.vramEstimate >= 2048) score += 25;
  else if (capabilities.gpu.vramEstimate >= 1024) score += 20;
  else if (capabilities.gpu.vramEstimate >= 512) score += 15;
  else if (capabilities.gpu.vramEstimate >= 256) score += 5;

  // GPU Type (up to 20 points)
  if (!capabilities.gpu.isIntegrated) score += 20;

  // Max Texture Size (up to 20 points)
  if (capabilities.gpu.maxTextureSize >= 4096) score += 20;
  else if (capabilities.gpu.maxTextureSize >= 2048) score += 15;
  else if (capabilities.gpu.maxTextureSize >= 1024) score += 10;

  // Features (up to 20 points)
  let featureScore = 0;
  if (capabilities.features.anisotropic) featureScore += 5;
  if (capabilities.features.depthTexture) featureScore += 5;
  if (capabilities.features.shadowMapping) featureScore += 5;
  if (capabilities.features.instancedArrays) featureScore += 5;
  score += featureScore;

  // CPU (up to 15 points)
  if (capabilities.cpuCores >= 8) score += 15;
  else if (capabilities.cpuCores >= 4) score += 10;
  else score += 5;

  // Memory (up to 10 points)
  if (capabilities.memoryGB >= 8) score += 10;
  else if (capabilities.memoryGB >= 4) score += 7;
  else if (capabilities.memoryGB >= 2) score += 4;

  return Math.min(100, score);
}

/**
 * Low-end quality settings
 * Minimal effect: 640p textures, no shadows, minimal post-processing
 * Target: 60 FPS on integrated GPUs
 */
function getLowQualitySettings(capabilities: SystemCapabilities): RenderingQuality {
  return {
    shadowMapSize: 0, // Disabled
    reflectionQuality: 'none',
    msaaSamples: 1, // No MSAA
    useAnisotropicFiltering: false,
    maxTextureResolution: 512,
    particleCountMultiplier: 0.5,
    targetFrameRate: 60,
    geometryDetailLevel: 0.5,
    postProcessing: {
      bloom: false,
      chromatic: false,
      vignette: false,
      motionBlur: false,
    },
  };
}

/**
 * Medium quality settings
 * Balanced: 1024p textures, simple shadows, some post-processing
 * Target: 60 FPS on mid-range GPUs
 */
function getMediumQualitySettings(capabilities: SystemCapabilities): RenderingQuality {
  return {
    shadowMapSize: 512,
    reflectionQuality: 'low',
    msaaSamples: 2,
    useAnisotropicFiltering: capabilities.features.anisotropic,
    maxTextureResolution: 1024,
    particleCountMultiplier: 1.0,
    targetFrameRate: 60,
    geometryDetailLevel: 1.0,
    postProcessing: {
      bloom: true,
      chromatic: false,
      vignette: true,
      motionBlur: false,
    },
  };
}

/**
 * High-end quality settings
 * Maximum quality: 4K textures, advanced shadows, full post-processing
 * Target: 90+ FPS on high-end GPUs
 */
function getHighQualitySettings(capabilities: SystemCapabilities): RenderingQuality {
  return {
    shadowMapSize: 2048,
    reflectionQuality: 'high',
    msaaSamples: 4,
    useAnisotropicFiltering: true,
    maxTextureResolution: 4096,
    particleCountMultiplier: 2.0,
    targetFrameRate: 120,
    geometryDetailLevel: 1.5,
    postProcessing: {
      bloom: true,
      chromatic: true,
      vignette: true,
      motionBlur: true,
    },
  };
}

/**
 * Get quality settings with optional overrides
 * Allows user-controlled quality adjustment
 */
export function getQualitySettings(
  tier: DeviceTier,
  overrides?: Partial<RenderingQuality>
): RenderingQuality {
  return {
    ...tier.quality,
    ...overrides,
  };
}

/**
 * Create human-readable quality summary
 */
export function getQualitySummary(quality: RenderingQuality): string {
  const effectCount = Object.values(quality.postProcessing).filter((v) => v).length;

  return `
Rendering Quality Settings:
  Target FPS: ${quality.targetFrameRate}
  MSAA: ${quality.msaaSamples}x
  Max Texture: ${quality.maxTextureResolution}p
  Shadows: ${quality.shadowMapSize > 0 ? `${quality.shadowMapSize}p` : 'disabled'}
  Geometry Detail: ${(quality.geometryDetailLevel * 100).toFixed(0)}%
  Particles: ${(quality.particleCountMultiplier * 100).toFixed(0)}%
  Anisotropic Filtering: ${quality.useAnisotropicFiltering ? 'on' : 'off'}
  Post-Processing Effects: ${effectCount}/4
  `.trim();
}
