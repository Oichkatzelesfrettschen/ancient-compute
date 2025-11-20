/**
 * VisualizationProvider.ts
 * Abstract interface for visualization providers (Three.js, Canvas2D, etc.)
 */

export interface MachineState {
  registers: Map<string, number>;
  memory: Uint8Array;
  programCounter: number;
  accumulator: number;
  carryFlag: boolean;
  runningFlag: boolean;
  columns: ColumnState[];
  store: StoreState;
  mill: MillState;
  barrels: BarrelState[];
  cycles: number;
  timestamp: number;
}

export interface ColumnState {
  id: string;
  value: number;
  isActive: boolean;
  rotation: number; // 0-9 for display
}

export interface StoreState {
  positions: number[];
  capacity: number;
  activeIndices: number[];
}

export interface MillState {
  operation: 'idle' | 'add' | 'subtract' | 'multiply' | 'divide';
  operandA: number;
  operandB: number;
  result: number;
  progress: number; // 0.0 to 1.0
}

export interface BarrelState {
  id: string;
  programCounter: number;
  instructions: string[];
  isActive: boolean;
}

export interface VisualizationOptions {
  quality: 'low' | 'medium' | 'high';
  theme: 'victorian' | 'modern' | 'schematic';
  showAnnotations: boolean;
  animationSpeed: number; // 0.1 to 10.0
  cameraMode: 'fixed' | 'orbit' | 'free';
  enablePhysics: boolean;
  enableShadows: boolean;
  antialias: boolean;
  maxFPS: number;
}

export type StateChangeCallback = (state: MachineState) => void;
export type ErrorCallback = (error: Error) => void;

export interface VisualizationProvider {
  // Lifecycle
  initialize(canvas: HTMLCanvasElement, options: VisualizationOptions): Promise<void>;
  dispose(): void;

  // Rendering
  render(state: MachineState): void;
  resize(width: number, height: number): void;

  // Quality and Performance
  setQuality(level: 'low' | 'medium' | 'high'): void;
  getPerformanceMetrics(): PerformanceMetrics;

  // Interaction
  setCamera(position: { x: number; y: number; z: number }): void;
  resetCamera(): void;

  // State Management
  onStateChange(callback: StateChangeCallback): void;
  offStateChange(callback: StateChangeCallback): void;

  // Error Handling
  onError(callback: ErrorCallback): void;

  // Screenshot and Export
  captureScreenshot(): Promise<Blob>;
  exportState(): string;
}

export interface PerformanceMetrics {
  fps: number;
  frameTime: number;
  drawCalls: number;
  triangles: number;
  memoryUsage: number;
  gpuTime: number;
}

export abstract class BaseVisualizationProvider implements VisualizationProvider {
  protected canvas: HTMLCanvasElement | null = null;
  protected options: VisualizationOptions;
  protected stateCallbacks: Set<StateChangeCallback> = new Set();
  protected errorCallbacks: Set<ErrorCallback> = new Set();
  protected lastState: MachineState | null = null;
  protected performanceMetrics: PerformanceMetrics = {
    fps: 0,
    frameTime: 0,
    drawCalls: 0,
    triangles: 0,
    memoryUsage: 0,
    gpuTime: 0
  };

  constructor() {
    this.options = this.getDefaultOptions();
  }

  protected getDefaultOptions(): VisualizationOptions {
    return {
      quality: 'medium',
      theme: 'victorian',
      showAnnotations: true,
      animationSpeed: 1.0,
      cameraMode: 'orbit',
      enablePhysics: false,
      enableShadows: true,
      antialias: true,
      maxFPS: 60
    };
  }

  abstract initialize(canvas: HTMLCanvasElement, options: VisualizationOptions): Promise<void>;
  abstract dispose(): void;
  abstract render(state: MachineState): void;
  abstract resize(width: number, height: number): void;
  abstract setQuality(level: 'low' | 'medium' | 'high'): void;
  abstract setCamera(position: { x: number; y: number; z: number }): void;
  abstract resetCamera(): void;
  abstract captureScreenshot(): Promise<Blob>;

  getPerformanceMetrics(): PerformanceMetrics {
    return { ...this.performanceMetrics };
  }

  onStateChange(callback: StateChangeCallback): void {
    this.stateCallbacks.add(callback);
  }

  offStateChange(callback: StateChangeCallback): void {
    this.stateCallbacks.delete(callback);
  }

  onError(callback: ErrorCallback): void {
    this.errorCallbacks.add(callback);
  }

  protected notifyStateChange(state: MachineState): void {
    this.stateCallbacks.forEach(callback => callback(state));
  }

  protected notifyError(error: Error): void {
    this.errorCallbacks.forEach(callback => callback(error));
  }

  exportState(): string {
    if (!this.lastState) return '{}';
    return JSON.stringify(this.lastState, null, 2);
  }
}