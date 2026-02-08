/**
 * RenderingLoop - Animation Frame Orchestration
 *
 * Manages the requestAnimationFrame loop and coordinates all update
 * cycles. Provides a consistent interface for attaching update callbacks
 * and ensures proper frame timing.
 *
 * Responsibilities:
 * - Drive requestAnimationFrame loop
 * - Calculate delta time between frames
 * - Coordinate camera updates, animations, and rendering
 * - Measure FPS and performance metrics
 * - Support pause/resume functionality
 */

import * as THREE from 'three';
import type { SceneManager } from './SceneManager';
import type { CameraController } from './CameraController';
import type { LightingSetup } from './LightingSetup';

export interface UpdateCallback {
  (deltaTime: number, elapsedTime: number): void;
}

export interface RenderingLoopConfig {
  sceneManager: SceneManager;
  cameraController?: CameraController;
  targetFrameRate?: number;
  measurePerformance?: boolean;
}

export interface PerformanceMetrics {
  fps: number;
  frameTime: number;
  deltaTime: number;
  frameCount: number;
  averageFps: number;
  maxFrameTime: number;
  minFrameTime: number;
}

export class RenderingLoop {
  private sceneManager: SceneManager;
  private cameraController?: CameraController;

  // Animation state
  private isRunning: boolean = false;
  private isPaused: boolean = false;
  private animationFrameId: number | null = null;

  // Timing
  private targetFrameRate: number = 60;
  private targetFrameTime: number = 1000 / 60; // milliseconds
  private lastFrameTime: number = 0;
  private deltaTime: number = 0;
  private elapsedTime: number = 0;
  private startTime: number = 0;

  // Performance measurement
  private measurePerformance: boolean = true;
  private frameCount: number = 0;
  private frameTimeHistory: number[] = [];
  private lastSecondFrames: number = 0;
  private lastSecondTime: number = 0;
  private fps: number = 60;
  private maxFrameTime: number = 0;
  private minFrameTime: Infinity = Infinity;

  // Update callbacks
  private updateCallbacks: Map<string, UpdateCallback> = new Map();

  constructor(config: RenderingLoopConfig) {
    this.sceneManager = config.sceneManager;
    this.cameraController = config.cameraController;
    this.targetFrameRate = config.targetFrameRate ?? 60;
    this.targetFrameTime = 1000 / this.targetFrameRate;
    this.measurePerformance = config.measurePerformance ?? true;
  }

  /**
   * Start the rendering loop
   */
  public start(): void {
    if (this.isRunning) {
      return;
    }

    this.isRunning = true;
    this.isPaused = false;
    this.startTime = performance.now();
    this.lastFrameTime = this.startTime;
    this.lastSecondTime = this.startTime;
    this.frameCount = 0;
    this.lastSecondFrames = 0;
    this.frameTimeHistory = [];

    this.loop();
  }

  /**
   * Stop the rendering loop
   */
  public stop(): void {
    if (!this.isRunning) {
      return;
    }

    this.isRunning = false;
    this.isPaused = false;

    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }
  }

  /**
   * Pause the rendering loop
   */
  public pause(): void {
    this.isPaused = true;
  }

  /**
   * Resume the rendering loop
   */
  public resume(): void {
    this.isPaused = false;
    this.lastFrameTime = performance.now();
  }

  /**
   * Main animation loop
   */
  private loop = (): void => {
    const now = performance.now();

    // Calculate timing
    this.deltaTime = (now - this.lastFrameTime) / 1000; // Convert to seconds
    this.elapsedTime = (now - this.startTime) / 1000;

    // Clamp delta time to prevent large jumps
    // (happens when tab is backgrounded)
    if (this.deltaTime > 0.1) {
      this.deltaTime = this.targetFrameTime / 1000;
    }

    // Update frame metrics
    if (this.measurePerformance) {
      this.updatePerformanceMetrics(now);
    }

    // Execute update callbacks only if not paused
    if (!this.isPaused) {
      // Update camera controller
      if (this.cameraController) {
        this.cameraController.update(this.deltaTime * 1000); // Convert back to ms
      }

      // Execute registered update callbacks
      this.updateCallbacks.forEach((callback) => {
        callback(this.deltaTime, this.elapsedTime);
      });

      // Render scene
      this.sceneManager.render();
    }

    this.lastFrameTime = now;
    this.frameCount++;

    // Schedule next frame
    if (this.isRunning) {
      this.animationFrameId = requestAnimationFrame(this.loop);
    }
  };

  /**
   * Update performance metrics
   */
  private updatePerformanceMetrics(now: number): void {
    // Track frame time
    const frameTime = now - this.lastFrameTime;
    this.frameTimeHistory.push(frameTime);

    // Keep only last 300 samples (5 seconds at 60fps)
    if (this.frameTimeHistory.length > 300) {
      this.frameTimeHistory.shift();
    }

    // Update min/max frame times
    if (frameTime > 0) {
      this.maxFrameTime = Math.max(this.maxFrameTime, frameTime);
      this.minFrameTime = Math.min(this.minFrameTime as any, frameTime);
    }

    // Calculate FPS every second
    this.lastSecondFrames++;
    if (now - this.lastSecondTime >= 1000) {
      this.fps = this.lastSecondFrames;
      this.lastSecondFrames = 0;
      this.lastSecondTime = now;
    }
  }

  /**
   * Register an update callback
   * Called every frame with deltaTime and elapsedTime
   */
  public addUpdateCallback(name: string, callback: UpdateCallback): void {
    this.updateCallbacks.set(name, callback);
  }

  /**
   * Remove an update callback
   */
  public removeUpdateCallback(name: string): void {
    this.updateCallbacks.delete(name);
  }

  /**
   * Get current performance metrics
   */
  public getMetrics(): PerformanceMetrics {
    const frameTimeValues = this.frameTimeHistory.filter((t) => t > 0);
    const avgFrameTime =
      frameTimeValues.length > 0
        ? frameTimeValues.reduce((a, b) => a + b, 0) / frameTimeValues.length
        : 0;

    return {
      fps: this.fps,
      frameTime: this.lastFrameTime - (this.lastFrameTime - this.deltaTime * 1000),
      deltaTime: this.deltaTime,
      frameCount: this.frameCount,
      averageFps: 1000 / avgFrameTime,
      maxFrameTime: this.maxFrameTime,
      minFrameTime: this.minFrameTime === Infinity ? 0 : this.minFrameTime
    };
  }

  /**
   * Get current FPS
   */
  public getFps(): number {
    return this.fps;
  }

  /**
   * Get frame history for charting
   */
  public getFrameHistory(samples: number = 60): number[] {
    return this.frameTimeHistory.slice(-samples);
  }

  /**
   * Get current delta time (seconds)
   */
  public getDeltaTime(): number {
    return this.deltaTime;
  }

  /**
   * Get elapsed time since start (seconds)
   */
  public getElapsedTime(): number {
    return this.elapsedTime;
  }

  /**
   * Check if rendering is active
   */
  public isActive(): boolean {
    return this.isRunning && !this.isPaused;
  }

  /**
   * Reset performance metrics
   */
  public resetMetrics(): void {
    this.frameCount = 0;
    this.frameTimeHistory = [];
    this.lastSecondFrames = 0;
    this.lastSecondTime = performance.now();
    this.fps = 0;
    this.maxFrameTime = 0;
    this.minFrameTime = Infinity;
  }

  /**
   * Set target frame rate
   */
  public setTargetFrameRate(rate: number): void {
    this.targetFrameRate = rate;
    this.targetFrameTime = 1000 / rate;
  }

  /**
   * Get target frame rate
   */
  public getTargetFrameRate(): number {
    return this.targetFrameRate;
  }

  /**
   * Dispose resources
   */
  public dispose(): void {
    this.stop();
    this.updateCallbacks.clear();
  }
}

/**
 * Factory function for creating rendering loop
 */
export function createRenderingLoop(config: RenderingLoopConfig): RenderingLoop {
  return new RenderingLoop(config);
}
