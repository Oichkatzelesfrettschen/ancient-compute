/**
 * Performance and Quality Adaptation Module
 *
 * Exports:
 * - CapabilityAnalyzer: Detailed GPU and system capability detection
 * - DeviceTier: Device classification and quality settings
 * - QualitySelector: Bridge between FeatureDetector and rendering quality
 */

export {
  detectGPUInfo,
  detectFeatureSupport,
  detectSystemCapabilities,
  getPerformanceScore,
  createPerformanceReport,
  type GPUInfo,
  type FeatureSupport,
  type SystemCapabilities,
} from './CapabilityAnalyzer';

export {
  classifyDeviceTier,
  getQualitySettings,
  getQualitySummary,
  type DeviceTierLevel,
  type RenderingQuality,
  type DeviceTier,
} from './DeviceTier';

export {
  selectQualityFromWebGL,
  type QualitySelector,
} from './QualitySelector';
