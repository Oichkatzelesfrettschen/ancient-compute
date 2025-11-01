/**
 * GPU Feature Detection and Capability Assessment
 *
 * Detects device hardware capabilities:
 * - GPU type and VRAM
 * - WebGL extensions (anisotropy, compression, etc.)
 * - Texture size limits
 * - CPU performance
 * - Memory availability
 *
 * Used by DeviceTier for quality adaptation decisions
 */

export interface GPUInfo {
  vendor: string;
  renderer: string;
  maxTextureSize: number;
  maxCubemapSize: number;
  maxRenderBufferSize: number;
  vramEstimate: number; // MB
  isIntegrated: boolean;
  supportsHighPrecision: boolean;
}

export interface FeatureSupport {
  anisotropic: boolean;
  compression: {
    s3tc: boolean;
    astc: boolean;
    bptc: boolean;
  };
  shadowMapping: boolean;
  depthTexture: boolean;
  drawBuffers: boolean;
  instancedArrays: boolean;
  standardDerivatives: boolean;
  textureHalf: boolean;
  textureFloat: boolean;
  VAO: boolean;
}

export interface SystemCapabilities {
  gpu: GPUInfo;
  features: FeatureSupport;
  cpuCores: number;
  memoryGB: number;
  isLowEnd: boolean;
}

/**
 * Detect GPU capabilities from WebGL context
 */
export function detectGPUInfo(gl: WebGLRenderingContext | WebGL2RenderingContext): GPUInfo {
  const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
  const vendor = debugInfo
    ? gl.getParameter((debugInfo as any).UNMASKED_VENDOR_WEBGL) || 'Unknown'
    : 'Unknown';
  const renderer = debugInfo
    ? gl.getParameter((debugInfo as any).UNMASKED_RENDERER_WEBGL) || 'Unknown'
    : 'Unknown';

  const maxTextureSize = gl.getParameter(gl.MAX_TEXTURE_SIZE) as number;
  const maxCubemapSize = gl.getParameter(gl.MAX_CUBE_MAP_TEXTURE_SIZE) as number;
  const maxRenderBufferSize = gl.getParameter(gl.MAX_RENDERBUFFER_SIZE) as number;

  // Estimate VRAM based on max texture size
  // This is a rough heuristic; actual VRAM varies
  let vramEstimate = 256;
  if (maxTextureSize >= 4096) vramEstimate = 2048;
  else if (maxTextureSize >= 2048) vramEstimate = 1024;
  else if (maxTextureSize >= 1024) vramEstimate = 512;

  // Detect if integrated GPU (common in low-end devices)
  const isIntegrated =
    renderer.toLowerCase().includes('intel') ||
    renderer.toLowerCase().includes('mali') ||
    renderer.toLowerCase().includes('adreno') ||
    renderer.toLowerCase().includes('powervr');

  // High precision support (16-bit vs 32-bit floats)
  const supportsHighPrecision =
    gl.getParameter(gl.MAX_VARYING_VECTORS) >= 8 &&
    gl.getParameter(gl.MAX_TEXTURE_IMAGE_UNITS) >= 8;

  return {
    vendor,
    renderer,
    maxTextureSize,
    maxCubemapSize,
    maxRenderBufferSize,
    vramEstimate,
    isIntegrated,
    supportsHighPrecision,
  };
}

/**
 * Detect supported WebGL extensions
 */
export function detectFeatureSupport(gl: WebGLRenderingContext | WebGL2RenderingContext): FeatureSupport {
  return {
    anisotropic: !!gl.getExtension('EXT_texture_filter_anisotropic'),
    compression: {
      s3tc: !!gl.getExtension('WEBGL_compressed_texture_s3tc'),
      astc: !!gl.getExtension('WEBGL_compressed_texture_astc'),
      bptc: !!gl.getExtension('EXT_texture_compression_bptc'),
    },
    shadowMapping: !!gl.getExtension('EXT_shadow_samplers'),
    depthTexture: !!gl.getExtension('WEBGL_depth_texture'),
    drawBuffers: !!gl.getExtension('WEBGL_draw_buffers'),
    instancedArrays: !!gl.getExtension('ANGLE_instanced_arrays'),
    standardDerivatives: !!gl.getExtension('OES_standard_derivatives'),
    textureHalf: !!gl.getExtension('OES_texture_half_float'),
    textureFloat: !!gl.getExtension('OES_texture_float'),
    VAO: !!gl.getExtension('OES_vertex_array_object'),
  };
}

/**
 * Detect system capabilities
 */
export function detectSystemCapabilities(gl: WebGLRenderingContext | WebGL2RenderingContext): SystemCapabilities {
  const gpu = detectGPUInfo(gl);
  const features = detectFeatureSupport(gl);

  // Detect CPU core count
  const cpuCores = navigator.hardwareConcurrency || 4;

  // Estimate available memory
  // Note: Not always available, defaults to reasonable estimate
  let memoryGB = 8;
  if ((navigator as any).deviceMemory) {
    memoryGB = (navigator as any).deviceMemory;
  }

  // Heuristic for low-end devices
  const isLowEnd =
    gpu.isIntegrated &&
    gpu.maxTextureSize < 2048 &&
    cpuCores < 4 &&
    !features.anisotropic &&
    !features.depthTexture;

  return {
    gpu,
    features,
    cpuCores,
    memoryGB,
    isLowEnd,
  };
}

/**
 * Get benchmark score (0-100)
 * Higher score = more powerful device
 */
export function getPerformanceScore(capabilities: SystemCapabilities): number {
  let score = 50; // Base score

  // GPU score component (up to 40 points)
  if (capabilities.gpu.maxTextureSize >= 4096) score += 30;
  else if (capabilities.gpu.maxTextureSize >= 2048) score += 20;
  else if (capabilities.gpu.maxTextureSize >= 1024) score += 10;

  if (!capabilities.gpu.isIntegrated) score += 10;

  // Features score component (up to 20 points)
  let featureCount = 0;
  if (capabilities.features.anisotropic) featureCount++;
  if (capabilities.features.depthTexture) featureCount++;
  if (capabilities.features.shadowMapping) featureCount++;
  if (capabilities.features.instancedArrays) featureCount++;
  if (capabilities.features.compression.s3tc) featureCount++;
  score += featureCount * 2;

  // CPU score component (up to 20 points)
  if (capabilities.cpuCores >= 8) score += 15;
  else if (capabilities.cpuCores >= 4) score += 10;
  else score += 5;

  if (capabilities.memoryGB >= 8) score += 5;

  return Math.min(100, score);
}

/**
 * Create performance report for debugging
 */
export function createPerformanceReport(capabilities: SystemCapabilities): string {
  const score = getPerformanceScore(capabilities);

  return `
GPU Device Report:
  Vendor: ${capabilities.gpu.vendor}
  Renderer: ${capabilities.gpu.renderer}
  Max Texture Size: ${capabilities.gpu.maxTextureSize}
  Estimated VRAM: ${capabilities.gpu.vramEstimate}MB
  Is Integrated: ${capabilities.gpu.isIntegrated}
  High Precision: ${capabilities.gpu.supportsHighPrecision}

Feature Support:
  Anisotropic Filtering: ${capabilities.features.anisotropic}
  Depth Textures: ${capabilities.features.depthTexture}
  Shadow Mapping: ${capabilities.features.shadowMapping}
  Instanced Arrays: ${capabilities.features.instancedArrays}
  S3TC Compression: ${capabilities.features.compression.s3tc}
  ASTC Compression: ${capabilities.features.compression.astc}

System:
  CPU Cores: ${capabilities.cpuCores}
  Available Memory: ${capabilities.memoryGB}GB
  Low-End Device: ${capabilities.isLowEnd}

Performance Score: ${score}/100
  `.trim();
}
