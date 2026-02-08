/**
 * Visualization Manager: Complete 3D Visualization Orchestration
 *
 * Integrates:
 * - DifferenceEngineScene (3D rendering)
 * - StateAnimator (state-to-animation mapping)
 * - Timeline (animation orchestration)
 * - InteractiveCamera (user interaction)
 * - Real-time state updates from WebSocket
 *
 * Manages the complete flow: State → Diff → Animation → 3D Update → Render
 */

import * as THREE from 'three';
import type { MachineState, StateDiff } from '../state/types';
import { DifferenceEngineScene } from './DifferenceEngineScene';
import { InteractiveCamera } from './InteractiveCamera';
import { StateAnimator } from '../animation/StateAnimator';
import { Timeline } from '../animation/Timeline';
import { FeatureDetector } from '../animation/FeatureDetector';

/**
 * Visualization configuration
 */
export interface VisualizationConfig {
  canvas: HTMLCanvasElement;
  width: number;
  height: number;
  enablePerformanceMonitoring?: boolean;
  enableDebugOverlay?: boolean;
}

/**
 * Performance metrics
 */
export interface PerformanceMetrics {
  fps: number;
  deltaTime: number;
  renderTime: number;
  animationTime: number;
  totalFrames: number;
  gpuMemory?: number;
}

/**
 * Visualization manager
 */
export class VisualizationManager {
  // Core components
  private scene: DifferenceEngineScene;
  private camera: InteractiveCamera;
  private animationSystem: StateAnimator;
  private timeline: Timeline;
  private featureDetector: FeatureDetector;

  // State tracking
  private currentState: MachineState | null = null;
  private previousState: MachineState | null = null;
  private isAnimating: boolean = false;

  // Performance monitoring
  private performanceEnabled: boolean = false;
  private metrics: PerformanceMetrics = {
    fps: 0,
    deltaTime: 0,
    renderTime: 0,
    animationTime: 0,
    totalFrames: 0
  };

  // Configuration
  private config: VisualizationConfig;

  // Canvas resizing
  private resizeObserver: ResizeObserver | null = null;

  constructor(config: VisualizationConfig) {
    this.config = config;

    // Initialize feature detection
    this.featureDetector = new FeatureDetector();
    const capabilities = this.featureDetector.detectFeatures();
    console.log('[VisualizationManager] Device Tier:', capabilities.deviceTier);

    // Initialize 3D scene
    this.scene = new DifferenceEngineScene({
      canvas: config.canvas,
      width: config.width,
      height: config.height
    });

    // Initialize interactive camera
    this.camera = new InteractiveCamera(this.scene);

    // Initialize animation system
    this.timeline = new Timeline();
    this.animationSystem = new StateAnimator(this.timeline);

    // Performance monitoring
    this.performanceEnabled = config.enablePerformanceMonitoring || false;

    // Setup event handlers
    this.setupEventHandlers();

    // Start rendering
    this.start();
  }

  /**
   * Setup event handlers
   */
  private setupEventHandlers(): void {
    // Window resize
    window.addEventListener('resize', this.onWindowResize.bind(this));

    // Also setup ResizeObserver for canvas element
    if (typeof ResizeObserver !== 'undefined') {
      this.resizeObserver = new ResizeObserver(() => {
        const rect = this.config.canvas.getBoundingClientRect();
        this.onWindowResize();
      });

      this.resizeObserver.observe(this.config.canvas);
    }

    // Animation events
    this.timeline.onEvent((event) => {
      this.handleAnimationEvent(event);
    });
  }

  /**
   * Start visualization
   */
  start(): void {
    this.scene.start();
  }

  /**
   * Stop visualization
   */
  stop(): void {
    this.scene.stop();
  }

  /**
   * Update visualization with new machine state
   */
  updateState(newState: MachineState, stateDiff: StateDiff): void {
    // Store previous state
    this.previousState = this.currentState;
    this.currentState = newState;

    // Create animation sequence for state diff
    if (this.previousState) {
      this.animationSystem.animateStateDiff(
        this.previousState,
        newState,
        stateDiff,
        () => {
          this.isAnimating = false;
          console.log('[VisualizationManager] Animation complete');
        }
      );

      this.isAnimating = true;
      this.timeline.start();
    }
  }

  /**
   * Handle animation events
   */
  private handleAnimationEvent(event: any): void {
    // Route animation events to appropriate 3D updates
    const { type, targetObject, value, progress } = event;

    switch (type) {
      case 'UPDATE':
        this.updateAnimationTarget(targetObject, value, progress);
        break;

      case 'COMPLETE':
        console.log(`[VisualizationManager] Animation complete: ${targetObject}`);
        break;

      case 'START':
        console.log(`[VisualizationManager] Animation start: ${targetObject}`);
        break;
    }
  }

  /**
   * Update animation target in 3D scene
   */
  private updateAnimationTarget(targetObject: string, value: number, progress: number): void {
    // Parse target object string
    // Format: "column0wheel0", "shaft0", "carryLever0", etc.

    if (targetObject.startsWith('column')) {
      // Column wheel rotation
      const match = targetObject.match(/column(\d+)wheel(\d+)/);
      if (match) {
        const colIndex = parseInt(match[1]);
        const wheelIndex = parseInt(match[2]);

        const wheel = this.scene.getDigitWheelMesh(colIndex, wheelIndex);
        if (wheel) {
          wheel.rotation.x = value;
        }
      }
    } else if (targetObject.startsWith('shaft')) {
      // Shaft rotation
      const match = targetObject.match(/shaft(\d+)/);
      if (match) {
        const shaftIndex = parseInt(match[1]);

        const shaft = this.scene.getShaftMesh(shaftIndex);
        if (shaft) {
          shaft.rotation.z = value;
        }
      }
    } else if (targetObject.startsWith('carryLever')) {
      // Carry lever engagement
      const match = targetObject.match(/carryLever(\d+)/);
      if (match) {
        const leverIndex = parseInt(match[1]);

        const lever = this.scene.getCarryLeverMesh(leverIndex);
        if (lever) {
          lever.rotation.z = value;

          // Show/hide engagement indicator based on progress
          this.scene.setEngagementIndicatorVisible(leverIndex, progress > 0.1);
          this.scene.updateEngagementIndicator(leverIndex, progress);
        }
      }
    }
  }

  /**
   * Reset visualization to initial state
   */
  reset(): void {
    this.timeline.stop();
    this.isAnimating = false;
    this.camera.resetView();

    // Reset all animations
    // (would need to reset all mesh rotations to zero)
  }

  /**
   * Handle window resize
   */
  private onWindowResize(): void {
    const canvas = this.config.canvas;
    const width = canvas.clientWidth;
    const height = canvas.clientHeight;

    if (width > 0 && height > 0) {
      this.config.width = width;
      this.config.height = height;
      this.scene.onWindowResize(width, height);
    }
  }

  /**
   * Get feature detector
   */
  getFeatureDetector(): FeatureDetector {
    return this.featureDetector;
  }

  /**
   * Get scene
   */
  getScene(): DifferenceEngineScene {
    return this.scene;
  }

  /**
   * Get camera controller
   */
  getCamera(): InteractiveCamera {
    return this.camera;
  }

  /**
   * Get timeline
   */
  getTimeline(): Timeline {
    return this.timeline;
  }

  /**
   * Get animation system
   */
  getAnimationSystem(): StateAnimator {
    return this.animationSystem;
  }

  /**
   * Get current state
   */
  getCurrentState(): MachineState | null {
    return this.currentState;
  }

  /**
   * Is currently animating
   */
  isAnimatingNow(): boolean {
    return this.isAnimating;
  }

  /**
   * Get performance metrics
   */
  getMetrics(): Readonly<PerformanceMetrics> {
    return { ...this.metrics };
  }

  /**
   * Enable/disable performance monitoring
   */
  setPerformanceMonitoring(enabled: boolean): void {
    this.performanceEnabled = enabled;
  }

  /**
   * Get debug info string
   */
  getDebugInfo(): string {
    const sceneInfo = this.scene.getInfo();
    const timelineState = this.timeline.getState();
    const metrics = this.getMetrics();

    return `
Visualization Manager:
${sceneInfo}

Timeline State: ${timelineState}
Is Animating: ${this.isAnimating}

Performance Metrics:
- FPS: ${metrics.fps}
- Delta Time: ${metrics.deltaTime.toFixed(3)}ms
- Render Time: ${metrics.renderTime.toFixed(3)}ms
- Total Frames: ${metrics.totalFrames}

Current State: ${this.currentState ? 'ACTIVE' : 'NONE'}
    `.trim();
  }

  /**
   * Dispose resources
   */
  dispose(): void {
    this.stop();

    if (this.resizeObserver) {
      this.resizeObserver.disconnect();
      this.resizeObserver = null;
    }

    this.camera.dispose();
    this.scene.dispose();
    this.timeline.stop();
  }
}

/**
 * Factory function to create visualization manager
 */
export function createVisualizationManager(
  canvas: HTMLCanvasElement
): VisualizationManager {
  return new VisualizationManager({
    canvas,
    width: canvas.clientWidth || 1024,
    height: canvas.clientHeight || 768,
    enablePerformanceMonitoring: true,
    enableDebugOverlay: false
  });
}
