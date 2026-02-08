/**
 * CameraController - Orbital Camera Control System
 *
 * Manages camera movement with orbital rotation around the Difference Engine.
 * Supports mouse drag rotation, zoom, and keyboard shortcuts.
 *
 * Controls:
 * - Left mouse drag: Rotate around target
 * - Scroll wheel: Zoom in/out
 * - Right mouse drag: Pan camera
 * - Arrow keys: Rotate view
 * - +/- keys: Zoom
 * - R key: Reset to default view
 */

import * as THREE from 'three';

export interface CameraControllerConfig {
  camera: THREE.PerspectiveCamera;
  canvas: HTMLCanvasElement;
  target?: THREE.Vector3;
  minDistance?: number;
  maxDistance?: number;
  minPolar?: number;
  maxPolar?: number;
  minAzimuth?: number;
  maxAzimuth?: number;
  dampingFactor?: number;
  autoRotate?: boolean;
  autoRotateSpeed?: number;
}

export class CameraController {
  private camera: THREE.Camera;
  private canvas: HTMLCanvasElement;
  private target: THREE.Vector3;

  // Orbital parameters
  private radius: number = 80;
  private azimuth: number = 0;      // Horizontal rotation (radians)
  private polar: number = Math.PI / 4; // Vertical rotation (radians)

  // Constraints
  private minDistance: number;
  private maxDistance: number;
  private minPolar: number;
  private maxPolar: number;
  private minAzimuth: number;
  private maxAzimuth: number;

  // Smoothing
  private dampingFactor: number = 0.05;
  private targetAzimuth: number = 0;
  private targetPolar: number = Math.PI / 4;
  private targetRadius: number = 80;

  // Auto-rotation
  private autoRotate: boolean = false;
  private autoRotateSpeed: number = 0.5;

  // Input state
  private isRotating: boolean = false;
  private isPanning: boolean = false;
  private lastMouseX: number = 0;
  private lastMouseY: number = 0;
  private mouseVelocity: { x: number; y: number } = { x: 0, y: 0 };

  // Keyboard state
  private keys: { [key: string]: boolean } = {};

  // Event listeners (for cleanup)
  private listeners: { [key: string]: EventListener } = {};

  constructor(config: CameraControllerConfig) {
    this.camera = config.camera;
    this.canvas = config.canvas;
    this.target = config.target || new THREE.Vector3(0, 0, 0);

    // Set constraints
    this.minDistance = config.minDistance ?? 20;
    this.maxDistance = config.maxDistance ?? 150;
    this.minPolar = config.minPolar ?? Math.PI * 0.1;  // 18 degrees minimum
    this.maxPolar = config.maxPolar ?? Math.PI * 0.9;  // 162 degrees maximum
    this.minAzimuth = config.minAzimuth ?? -Infinity;
    this.maxAzimuth = config.maxAzimuth ?? Infinity;

    this.dampingFactor = config.dampingFactor ?? 0.05;
    this.autoRotate = config.autoRotate ?? false;
    this.autoRotateSpeed = config.autoRotateSpeed ?? 0.5;

    // Set initial orbital position from camera
    this.updateOrbitalFromCamera();

    // Set up event listeners
    this.attachEventListeners();
  }

  /**
   * Attach all input event listeners
   */
  private attachEventListeners(): void {
    // Mouse events
    this.listeners['mousedown'] = (e: Event) => this.onMouseDown(e as MouseEvent);
    this.listeners['mousemove'] = (e: Event) => this.onMouseMove(e as MouseEvent);
    this.listeners['mouseup'] = (e: Event) => this.onMouseUp(e as MouseEvent);
    this.listeners['wheel'] = (e: Event) => this.onMouseWheel(e as WheelEvent);

    // Keyboard events
    this.listeners['keydown'] = (e: Event) => this.onKeyDown(e as KeyboardEvent);
    this.listeners['keyup'] = (e: Event) => this.onKeyUp(e as KeyboardEvent);

    this.canvas.addEventListener('mousedown', this.listeners['mousedown']);
    this.canvas.addEventListener('mousemove', this.listeners['mousemove']);
    this.canvas.addEventListener('wheel', this.listeners['wheel'], { passive: false });

    document.addEventListener('mouseup', this.listeners['mouseup']);
    document.addEventListener('keydown', this.listeners['keydown']);
    document.addEventListener('keyup', this.listeners['keyup']);
  }

  /**
   * Remove all event listeners
   */
  private detachEventListeners(): void {
    this.canvas.removeEventListener('mousedown', this.listeners['mousedown']);
    this.canvas.removeEventListener('mousemove', this.listeners['mousemove']);
    this.canvas.removeEventListener('wheel', this.listeners['wheel']);
    document.removeEventListener('mouseup', this.listeners['mouseup']);
    document.removeEventListener('keydown', this.listeners['keydown']);
    document.removeEventListener('keyup', this.listeners['keyup']);
  }

  /**
   * Handle mouse down
   */
  private onMouseDown(event: MouseEvent): void {
    this.lastMouseX = event.clientX;
    this.lastMouseY = event.clientY;

    if (event.button === 0) {
      // Left mouse button - rotation
      this.isRotating = true;
    } else if (event.button === 2) {
      // Right mouse button - panning
      this.isPanning = true;
    }
  }

  /**
   * Handle mouse move
   */
  private onMouseMove(event: MouseEvent): void {
    const deltaX = event.clientX - this.lastMouseX;
    const deltaY = event.clientY - this.lastMouseY;

    if (this.isRotating) {
      // Rotate view based on mouse movement
      const sensitivity = 0.01;
      this.targetAzimuth -= deltaX * sensitivity;
      this.targetPolar -= deltaY * sensitivity;

      // Clamp polar angle
      this.targetPolar = Math.max(this.minPolar, Math.min(this.maxPolar, this.targetPolar));

      // Store velocity for momentum (unused for now, but useful for future)
      this.mouseVelocity.x = deltaX;
      this.mouseVelocity.y = deltaY;
    }

    if (this.isPanning) {
      // Pan camera (move target in screen plane)
      const panSensitivity = 0.1;
      const panX = -deltaX * panSensitivity;
      const panY = deltaY * panSensitivity;

      // Create pan vector in camera's local coordinate system
      const right = new THREE.Vector3();
      const up = new THREE.Vector3(0, 1, 0);
      this.camera.getWorldDirection(new THREE.Vector3());
      right.crossVectors(this.camera.getWorldDirection(new THREE.Vector3()), up).normalize();

      this.target.addScaledVector(right, panX);
      this.target.addScaledVector(up, panY);
    }

    this.lastMouseX = event.clientX;
    this.lastMouseY = event.clientY;
  }

  /**
   * Handle mouse up
   */
  private onMouseUp(event: MouseEvent): void {
    if (event.button === 0) {
      this.isRotating = false;
    } else if (event.button === 2) {
      this.isPanning = false;
    }
  }

  /**
   * Handle mouse wheel for zoom
   */
  private onMouseWheel(event: WheelEvent): void {
    event.preventDefault();

    const zoomSensitivity = 0.1;
    const zoomDelta = event.deltaY > 0 ? 1 : -1;

    this.targetRadius += zoomDelta * zoomSensitivity * this.targetRadius;
    this.targetRadius = Math.max(this.minDistance, Math.min(this.maxDistance, this.targetRadius));
  }

  /**
   * Handle keyboard down
   */
  private onKeyDown(event: KeyboardEvent): void {
    this.keys[event.key.toLowerCase()] = true;

    // Handle immediate actions
    if (event.key === 'r' || event.key === 'R') {
      this.reset();
    }
  }

  /**
   * Handle keyboard up
   */
  private onKeyUp(event: KeyboardEvent): void {
    this.keys[event.key.toLowerCase()] = false;
  }

  /**
   * Update camera position based on orbital parameters
   */
  public update(deltaTime: number = 0.016): void {
    // Handle keyboard input
    const rotationSpeed = 0.05;
    if (this.keys['arrowup']) {
      this.targetPolar -= rotationSpeed;
      this.targetPolar = Math.max(this.minPolar, Math.min(this.maxPolar, this.targetPolar));
    }
    if (this.keys['arrowdown']) {
      this.targetPolar += rotationSpeed;
      this.targetPolar = Math.max(this.minPolar, Math.min(this.maxPolar, this.targetPolar));
    }
    if (this.keys['arrowleft']) {
      this.targetAzimuth += rotationSpeed;
    }
    if (this.keys['arrowright']) {
      this.targetAzimuth -= rotationSpeed;
    }
    if (this.keys['+'] || this.keys['=']) {
      this.targetRadius -= rotationSpeed * 2;
      this.targetRadius = Math.max(this.minDistance, Math.min(this.maxDistance, this.targetRadius));
    }
    if (this.keys['-']) {
      this.targetRadius += rotationSpeed * 2;
      this.targetRadius = Math.max(this.minDistance, Math.min(this.maxDistance, this.targetRadius));
    }

    // Auto-rotation
    if (this.autoRotate) {
      this.targetAzimuth += (this.autoRotateSpeed * deltaTime) / 1000;
    }

    // Apply damping to smooth transitions
    this.azimuth += (this.targetAzimuth - this.azimuth) * this.dampingFactor;
    this.polar += (this.targetPolar - this.polar) * this.dampingFactor;
    this.radius += (this.targetRadius - this.radius) * this.dampingFactor;

    // Clamp azimuth and polar
    this.azimuth = this.clampAngle(this.azimuth, this.minAzimuth, this.maxAzimuth);
    this.polar = Math.max(this.minPolar, Math.min(this.maxPolar, this.polar));

    // Calculate camera position from spherical coordinates
    const x = this.target.x + this.radius * Math.sin(this.polar) * Math.sin(this.azimuth);
    const y = this.target.y + this.radius * Math.cos(this.polar);
    const z = this.target.z + this.radius * Math.sin(this.polar) * Math.cos(this.azimuth);

    this.camera.position.set(x, y, z);
    this.camera.lookAt(this.target);
  }

  /**
   * Update orbital parameters from current camera position
   */
  private updateOrbitalFromCamera(): void {
    const relative = new THREE.Vector3();
    relative.subVectors((this.camera as THREE.PerspectiveCamera).position, this.target);

    this.radius = relative.length();
    this.polar = Math.acos(relative.y / this.radius);
    this.azimuth = Math.atan2(relative.x, relative.z);

    this.targetRadius = this.radius;
    this.targetPolar = this.polar;
    this.targetAzimuth = this.azimuth;
  }

  /**
   * Clamp angle between min and max
   */
  private clampAngle(angle: number, min: number, max: number): number {
    if (!isFinite(min) || !isFinite(max)) {
      return angle;
    }
    return Math.max(min, Math.min(max, angle));
  }

  /**
   * Reset camera to default view
   */
  public reset(): void {
    this.targetAzimuth = 0;
    this.targetPolar = Math.PI / 4;
    this.targetRadius = 80;
    this.target.set(0, 0, 0);
  }

  /**
   * Set camera target
   */
  public setTarget(target: THREE.Vector3): void {
    this.target.copy(target);
  }

  /**
   * Get camera target
   */
  public getTarget(): THREE.Vector3 {
    return this.target.clone();
  }

  /**
   * Set auto-rotation
   */
  public setAutoRotate(enabled: boolean, speed?: number): void {
    this.autoRotate = enabled;
    if (speed !== undefined) {
      this.autoRotateSpeed = speed;
    }
  }

  /**
   * Dispose resources
   */
  public dispose(): void {
    this.detachEventListeners();
  }
}
