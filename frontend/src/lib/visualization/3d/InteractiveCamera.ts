/**
 * Interactive Camera: Mouse and Touch Controls
 *
 * Provides:
 * - Orbit camera controls (rotate around object)
 * - Zoom in/out
 * - Pan camera
 * - Keyboard shortcuts
 * - Touch gesture support
 * - Momentum/inertia damping
 */

import * as THREE from 'three';
import { DifferenceEngineScene } from './DifferenceEngineScene';

/**
 * Camera control state
 */
export interface CameraControlState {
  isDragging: boolean;
  isPanning: boolean;
  previousMousePosition: { x: number; y: number };
  momentum: { x: number; y: number };
  dampingFactor: number;
}

/**
 * Interactive camera controller
 */
export class InteractiveCamera {
  private scene: DifferenceEngineScene;
  private canvas: HTMLCanvasElement;
  private camera: THREE.PerspectiveCamera;
  private engineGroup: THREE.Group;

  // Control state
  private controlState: CameraControlState = {
    isDragging: false,
    isPanning: false,
    previousMousePosition: { x: 0, y: 0 },
    momentum: { x: 0, y: 0 },
    dampingFactor: 0.95
  };

  // Configuration
  private rotationSpeed: number = 1;
  private zoomSpeed: number = 0.1;
  private panSpeed: number = 0.01;
  private enableMomentum: boolean = true;
  private minZoom: number = 5;
  private maxZoom: number = 30;

  // Animation frame ID
  private momentumAnimationId: number | null = null;

  constructor(scene: DifferenceEngineScene) {
    this.scene = scene;
    this.canvas = scene.getRenderer().domElement;
    this.camera = scene.getCamera();
    this.engineGroup = scene.getEngineGroup();

    this.attachEventListeners();
  }

  /**
   * Attach event listeners to canvas
   */
  private attachEventListeners(): void {
    // Mouse events
    this.canvas.addEventListener('mousedown', this.onMouseDown.bind(this));
    this.canvas.addEventListener('mousemove', this.onMouseMove.bind(this));
    this.canvas.addEventListener('mouseup', this.onMouseUp.bind(this));
    this.canvas.addEventListener('wheel', this.onMouseWheel.bind(this), { passive: false });

    // Touch events
    this.canvas.addEventListener('touchstart', this.onTouchStart.bind(this));
    this.canvas.addEventListener('touchmove', this.onTouchMove.bind(this), { passive: false });
    this.canvas.addEventListener('touchend', this.onTouchEnd.bind(this));

    // Keyboard events
    window.addEventListener('keydown', this.onKeyDown.bind(this));

    // Double-click to reset
    this.canvas.addEventListener('dblclick', this.resetView.bind(this));
  }

  /**
   * Mouse down handler
   */
  private onMouseDown(event: MouseEvent): void {
    if (event.button === 0) {
      // Left mouse button
      this.controlState.isDragging = true;
      this.controlState.previousMousePosition = {
        x: event.clientX,
        y: event.clientY
      };

      // Cancel momentum animation if running
      if (this.momentumAnimationId !== null) {
        cancelAnimationFrame(this.momentumAnimationId);
        this.momentumAnimationId = null;
      }
    } else if (event.button === 2) {
      // Right mouse button
      this.controlState.isPanning = true;
    }
  }

  /**
   * Mouse move handler
   */
  private onMouseMove(event: MouseEvent): void {
    if (this.controlState.isDragging) {
      const deltaX = event.clientX - this.controlState.previousMousePosition.x;
      const deltaY = event.clientY - this.controlState.previousMousePosition.y;

      this.rotateView(deltaX, deltaY);

      // Update momentum
      this.controlState.momentum = {
        x: deltaX * 0.5,
        y: deltaY * 0.5
      };

      this.controlState.previousMousePosition = {
        x: event.clientX,
        y: event.clientY
      };
    } else if (this.controlState.isPanning) {
      const deltaX = event.clientX - this.controlState.previousMousePosition.x;
      const deltaY = event.clientY - this.controlState.previousMousePosition.y;

      this.panView(deltaX, deltaY);

      this.controlState.previousMousePosition = {
        x: event.clientX,
        y: event.clientY
      };
    }
  }

  /**
   * Mouse up handler
   */
  private onMouseUp(event: MouseEvent): void {
    if (event.button === 0) {
      this.controlState.isDragging = false;

      // Apply momentum damping if enabled
      if (this.enableMomentum && (this.controlState.momentum.x !== 0 || this.controlState.momentum.y !== 0)) {
        this.applyMomentum();
      }
    } else if (event.button === 2) {
      this.controlState.isPanning = false;
    }
  }

  /**
   * Mouse wheel handler (zoom)
   */
  private onMouseWheel(event: WheelEvent): void {
    event.preventDefault();

    const delta = event.deltaY > 0 ? this.zoomSpeed : -this.zoomSpeed;
    this.zoomView(delta);
  }

  /**
   * Touch start handler
   */
  private onTouchStart(event: TouchEvent): void {
    if (event.touches.length === 1) {
      this.controlState.isDragging = true;
      this.controlState.previousMousePosition = {
        x: event.touches[0].clientX,
        y: event.touches[0].clientY
      };
    } else if (event.touches.length === 2) {
      // Pinch zoom
      this.controlState.isDragging = false;
      this.recordTouchDistance(event);
    }
  }

  /**
   * Touch move handler
   */
  private onTouchMove(event: TouchEvent): void {
    event.preventDefault();

    if (event.touches.length === 1 && this.controlState.isDragging) {
      const deltaX = event.touches[0].clientX - this.controlState.previousMousePosition.x;
      const deltaY = event.touches[0].clientY - this.controlState.previousMousePosition.y;

      this.rotateView(deltaX, deltaY);

      this.controlState.previousMousePosition = {
        x: event.touches[0].clientX,
        y: event.touches[0].clientY
      };
    } else if (event.touches.length === 2) {
      // Pinch zoom
      const distance = this.getTouchDistance(event);
      const previousDistance = (event as any).previousTouchDistance || distance;

      const delta = (distance - previousDistance) * 0.01;
      this.zoomView(delta);

      (event as any).previousTouchDistance = distance;
    }
  }

  /**
   * Touch end handler
   */
  private onTouchEnd(event: TouchEvent): void {
    if (event.touches.length === 0) {
      this.controlState.isDragging = false;
    }
  }

  /**
   * Keyboard handler
   */
  private onKeyDown(event: KeyboardEvent): void {
    switch (event.key.toLowerCase()) {
      case 'r':
        this.resetView();
        break;
      case 'h':
        this.printControls();
        break;
      case 'arrowup':
      case 'w':
        this.zoomView(-this.zoomSpeed * 2);
        break;
      case 'arrowdown':
      case 's':
        this.zoomView(this.zoomSpeed * 2);
        break;
      case 'arrowleft':
      case 'a':
        this.rotateView(-10, 0);
        break;
      case 'arrowright':
      case 'd':
        this.rotateView(10, 0);
        break;
    }
  }

  /**
   * Rotate view (orbit around object)
   */
  private rotateView(deltaX: number, deltaY: number): void {
    this.scene.rotateEngine(deltaX, deltaY);
  }

  /**
   * Pan view (move camera)
   */
  private panView(deltaX: number, deltaY: number): void {
    const moveX = -deltaX * this.panSpeed;
    const moveY = deltaY * this.panSpeed;

    this.camera.position.x += moveX;
    this.camera.position.y += moveY;
  }

  /**
   * Zoom view
   */
  private zoomView(delta: number): void {
    const direction = this.camera.position.clone().normalize();
    const currentDistance = this.camera.position.length();
    const newDistance = Math.max(
      this.minZoom,
      Math.min(this.maxZoom, currentDistance + delta * 2)
    );

    this.camera.position.copy(direction.multiplyScalar(newDistance));
  }

  /**
   * Apply momentum damping
   */
  private applyMomentum(): void {
    const animate = (): void => {
      if (Math.abs(this.controlState.momentum.x) < 0.01 && Math.abs(this.controlState.momentum.y) < 0.01) {
        this.controlState.momentum = { x: 0, y: 0 };
        return;
      }

      this.rotateView(this.controlState.momentum.x, this.controlState.momentum.y);

      this.controlState.momentum.x *= this.controlState.dampingFactor;
      this.controlState.momentum.y *= this.controlState.dampingFactor;

      this.momentumAnimationId = requestAnimationFrame(animate);
    };

    animate();
  }

  /**
   * Record touch distance (for pinch zoom)
   */
  private recordTouchDistance(event: TouchEvent): void {
    if (event.touches.length === 2) {
      const touch1 = event.touches[0];
      const touch2 = event.touches[1];

      const dx = touch1.clientX - touch2.clientX;
      const dy = touch1.clientY - touch2.clientY;

      (event as any).previousTouchDistance = Math.sqrt(dx * dx + dy * dy);
    }
  }

  /**
   * Get touch distance
   */
  private getTouchDistance(event: TouchEvent): number {
    if (event.touches.length < 2) return 0;

    const touch1 = event.touches[0];
    const touch2 = event.touches[1];

    const dx = touch1.clientX - touch2.clientX;
    const dy = touch1.clientY - touch2.clientY;

    return Math.sqrt(dx * dx + dy * dy);
  }

  /**
   * Reset view to default
   */
  resetView(): void {
    this.scene.resetEngineRotation();
    this.scene.resetCameraView();
    this.controlState.momentum = { x: 0, y: 0 };
  }

  /**
   * Set rotation speed
   */
  setRotationSpeed(speed: number): void {
    this.rotationSpeed = Math.max(0.1, Math.min(5, speed));
  }

  /**
   * Set zoom speed
   */
  setZoomSpeed(speed: number): void {
    this.zoomSpeed = Math.max(0.01, Math.min(1, speed));
  }

  /**
   * Enable/disable momentum
   */
  setMomentumEnabled(enabled: boolean): void {
    this.enableMomentum = enabled;
  }

  /**
   * Print controls to console
   */
  private printControls(): void {
    console.log(`
Interactive Camera Controls:
  Mouse Left Click + Drag: Rotate view
  Mouse Right Click + Drag: Pan camera
  Mouse Wheel: Zoom in/out
  Double Click: Reset view

Keyboard:
  W/Up Arrow: Zoom in
  S/Down Arrow: Zoom out
  A/Left Arrow: Rotate left
  D/Right Arrow: Rotate right
  R: Reset view
  H: Print this help

Touch:
  Single Finger Drag: Rotate view
  Two Finger Pinch: Zoom in/out
    `);
  }

  /**
   * Get control state
   */
  getControlState(): Readonly<CameraControlState> {
    return { ...this.controlState };
  }

  /**
   * Cleanup
   */
  dispose(): void {
    if (this.momentumAnimationId !== null) {
      cancelAnimationFrame(this.momentumAnimationId);
      this.momentumAnimationId = null;
    }

    // Remove event listeners (would need to store references)
  }
}

/**
 * Factory function to create interactive camera
 */
export function createInteractiveCamera(scene: DifferenceEngineScene): InteractiveCamera {
  return new InteractiveCamera(scene);
}
