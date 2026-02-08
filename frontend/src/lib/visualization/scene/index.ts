/**
 * Scene Module - Core Three.js Infrastructure
 *
 * Exports all scene management modules:
 * - SceneManager: Core renderer and scene setup
 * - CameraController: Orbital camera control
 * - LightingSetup: Scene illumination
 * - RenderingLoop: Animation frame orchestration
 */

export { SceneManager, createSceneManager } from './SceneManager';
export type { SceneConfig, SceneSnapshot } from './SceneManager';

export { CameraController } from './CameraController';
export type { CameraControllerConfig } from './CameraController';

export { LightingSetup, createLightingSetup } from './LightingSetup';
export type { LightingConfig, LightSnapshot } from './LightingSetup';

export { RenderingLoop, createRenderingLoop } from './RenderingLoop';
export type { RenderingLoopConfig, PerformanceMetrics, UpdateCallback } from './RenderingLoop';
