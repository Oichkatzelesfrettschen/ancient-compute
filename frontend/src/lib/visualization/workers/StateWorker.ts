/**
 * StateWorker.ts
 * Web Worker for async state diff computation and non-blocking operations
 */

// Worker message types
export interface WorkerMessage {
  id: string;
  type: 'computeDiff' | 'interpolate' | 'validate' | 'optimize' | 'physics';
  payload: any;
}

export interface WorkerResponse {
  id: string;
  type: string;
  result?: any;
  error?: string;
}

// State diff types
export interface StateDiff {
  changes: StateChange[];
  timestamp: number;
  size: number;
}

export interface StateChange {
  path: string[];
  oldValue: any;
  newValue: any;
  type: 'add' | 'update' | 'delete';
}

// Main worker class (runs in worker thread)
export class StateWorkerImpl {
  private pendingTasks: Map<string, any> = new Map();

  constructor() {
    // Set up message handler
    self.addEventListener('message', (event: MessageEvent<WorkerMessage>) => {
      this.handleMessage(event.data);
    });
  }

  private async handleMessage(message: WorkerMessage): Promise<void> {
    const { id, type, payload } = message;

    try {
      let result: any;

      switch (type) {
        case 'computeDiff':
          result = await this.computeDiff(payload.oldState, payload.newState);
          break;

        case 'interpolate':
          result = await this.interpolateStates(
            payload.states,
            payload.progress,
            payload.easing
          );
          break;

        case 'validate':
          result = await this.validateState(payload.state);
          break;

        case 'optimize':
          result = await this.optimizeState(payload.state);
          break;

        case 'physics':
          result = await this.simulatePhysics(
            payload.objects,
            payload.deltaTime,
            payload.gravity
          );
          break;

        default:
          throw new Error(`Unknown message type: ${type}`);
      }

      // Send response
      const response: WorkerResponse = {
        id,
        type,
        result
      };

      self.postMessage(response);
    } catch (error) {
      // Send error response
      const response: WorkerResponse = {
        id,
        type,
        error: error instanceof Error ? error.message : String(error)
      };

      self.postMessage(response);
    }
  }

  /**
   * Compute diff between two states
   */
  private async computeDiff(oldState: any, newState: any): Promise<StateDiff> {
    const changes: StateChange[] = [];
    const visited = new Set<string>();

    // Recursive diff function
    const diff = (oldObj: any, newObj: any, path: string[] = []): void => {
      const pathStr = path.join('.');

      if (visited.has(pathStr)) return;
      visited.add(pathStr);

      // Handle primitives
      if (typeof oldObj !== 'object' || typeof newObj !== 'object') {
        if (oldObj !== newObj) {
          changes.push({
            path,
            oldValue: oldObj,
            newValue: newObj,
            type: 'update'
          });
        }
        return;
      }

      // Handle arrays
      if (Array.isArray(oldObj) && Array.isArray(newObj)) {
        const maxLength = Math.max(oldObj.length, newObj.length);

        for (let i = 0; i < maxLength; i++) {
          if (i >= oldObj.length) {
            changes.push({
              path: [...path, String(i)],
              oldValue: undefined,
              newValue: newObj[i],
              type: 'add'
            });
          } else if (i >= newObj.length) {
            changes.push({
              path: [...path, String(i)],
              oldValue: oldObj[i],
              newValue: undefined,
              type: 'delete'
            });
          } else {
            diff(oldObj[i], newObj[i], [...path, String(i)]);
          }
        }
        return;
      }

      // Handle objects
      const allKeys = new Set([
        ...Object.keys(oldObj || {}),
        ...Object.keys(newObj || {})
      ]);

      for (const key of allKeys) {
        const oldValue = oldObj?.[key];
        const newValue = newObj?.[key];

        if (!(key in oldObj)) {
          changes.push({
            path: [...path, key],
            oldValue: undefined,
            newValue,
            type: 'add'
          });
        } else if (!(key in newObj)) {
          changes.push({
            path: [...path, key],
            oldValue,
            newValue: undefined,
            type: 'delete'
          });
        } else {
          diff(oldValue, newValue, [...path, key]);
        }
      }
    };

    // Compute diff
    diff(oldState, newState);

    // Calculate size
    const size = JSON.stringify(changes).length;

    return {
      changes,
      timestamp: Date.now(),
      size
    };
  }

  /**
   * Interpolate between multiple states
   */
  private async interpolateStates(
    states: any[],
    progress: number,
    easing: 'linear' | 'ease-in' | 'ease-out' | 'ease-in-out' = 'linear'
  ): Promise<any> {
    if (states.length < 2) {
      return states[0] || {};
    }

    // Apply easing function
    const easedProgress = this.applyEasing(progress, easing);

    // Find which two states to interpolate between
    const segmentLength = 1 / (states.length - 1);
    const segmentIndex = Math.floor(easedProgress / segmentLength);
    const localProgress = (easedProgress % segmentLength) / segmentLength;

    const fromState = states[Math.min(segmentIndex, states.length - 1)];
    const toState = states[Math.min(segmentIndex + 1, states.length - 1)];

    // Interpolate numeric values
    const interpolate = (from: any, to: any, t: number): any => {
      if (typeof from === 'number' && typeof to === 'number') {
        return from + (to - from) * t;
      }

      if (typeof from === 'boolean' || typeof to === 'boolean') {
        return t < 0.5 ? from : to;
      }

      if (Array.isArray(from) && Array.isArray(to)) {
        const maxLength = Math.max(from.length, to.length);
        return Array(maxLength).fill(null).map((_, i) =>
          interpolate(from[i], to[i], t)
        );
      }

      if (typeof from === 'object' && typeof to === 'object') {
        const result: any = {};
        const allKeys = new Set([...Object.keys(from), ...Object.keys(to)]);

        for (const key of allKeys) {
          result[key] = interpolate(from[key], to[key], t);
        }

        return result;
      }

      return t < 0.5 ? from : to;
    };

    return interpolate(fromState, toState, localProgress);
  }

  /**
   * Apply easing function
   */
  private applyEasing(t: number, easing: string): number {
    t = Math.max(0, Math.min(1, t));

    switch (easing) {
      case 'ease-in':
        return t * t;

      case 'ease-out':
        return t * (2 - t);

      case 'ease-in-out':
        return t < 0.5 ? 2 * t * t : -1 + (4 - 2 * t) * t;

      case 'linear':
      default:
        return t;
    }
  }

  /**
   * Validate machine state
   */
  private async validateState(state: any): Promise<{
    valid: boolean;
    errors: string[];
    warnings: string[];
  }> {
    const errors: string[] = [];
    const warnings: string[] = [];

    // Check required fields
    if (!state.registers) {
      errors.push('Missing registers');
    }

    if (!state.memory) {
      errors.push('Missing memory');
    }

    if (typeof state.programCounter !== 'number') {
      errors.push('Invalid program counter');
    }

    if (typeof state.accumulator !== 'number') {
      errors.push('Invalid accumulator');
    }

    // Check value ranges
    if (state.programCounter < 0 || state.programCounter >= 4096) {
      warnings.push('Program counter out of typical range');
    }

    if (state.cycles && state.cycles > 1000000) {
      warnings.push('High cycle count detected');
    }

    // Check memory size
    if (state.memory && state.memory.length !== 4096) {
      warnings.push(`Unusual memory size: ${state.memory.length}`);
    }

    // Check column states
    if (state.columns) {
      state.columns.forEach((column: any, i: number) => {
        if (column.value < 0 || column.value > 9) {
          errors.push(`Column ${i} value out of range: ${column.value}`);
        }
      });
    }

    return {
      valid: errors.length === 0,
      errors,
      warnings
    };
  }

  /**
   * Optimize state for transmission/storage
   */
  private async optimizeState(state: any): Promise<{
    optimized: any;
    compression: number;
  }> {
    const original = JSON.stringify(state);

    // Remove default values
    const optimized = this.removeDefaults(state);

    // Compress arrays
    if (optimized.memory && Array.from(optimized.memory).every((v: any) => v === 0)) {
      delete optimized.memory;
      optimized.memoryEmpty = true;
    }

    // Convert sparse arrays to objects
    if (optimized.store?.positions) {
      const positions = optimized.store.positions;
      const nonZero: any = {};
      let hasNonZero = false;

      positions.forEach((value: any, index: number) => {
        if (value !== 0) {
          nonZero[index] = value;
          hasNonZero = true;
        }
      });

      if (hasNonZero) {
        optimized.store.positionsSparse = nonZero;
        delete optimized.store.positions;
      }
    }

    const optimizedStr = JSON.stringify(optimized);
    const compression = 1 - (optimizedStr.length / original.length);

    return {
      optimized,
      compression: Math.round(compression * 100) / 100
    };
  }

  /**
   * Remove default values from state
   */
  private removeDefaults(obj: any): any {
    if (typeof obj !== 'object' || obj === null) {
      return obj;
    }

    if (Array.isArray(obj)) {
      return obj.map(item => this.removeDefaults(item));
    }

    const result: any = {};

    for (const [key, value] of Object.entries(obj)) {
      // Skip default values
      if (value === 0 || value === false || value === '' || value === null) {
        continue;
      }

      // Skip empty arrays
      if (Array.isArray(value) && value.length === 0) {
        continue;
      }

      result[key] = this.removeDefaults(value);
    }

    return result;
  }

  /**
   * Simulate physics for animated components
   */
  private async simulatePhysics(
    objects: PhysicsObject[],
    deltaTime: number,
    gravity: { x: number; y: number; z: number }
  ): Promise<PhysicsObject[]> {
    return objects.map(obj => {
      // Apply gravity
      obj.velocity.x += gravity.x * deltaTime;
      obj.velocity.y += gravity.y * deltaTime;
      obj.velocity.z += gravity.z * deltaTime;

      // Apply damping
      const damping = 0.98;
      obj.velocity.x *= damping;
      obj.velocity.y *= damping;
      obj.velocity.z *= damping;

      // Update position
      obj.position.x += obj.velocity.x * deltaTime;
      obj.position.y += obj.velocity.y * deltaTime;
      obj.position.z += obj.velocity.z * deltaTime;

      // Check collisions with boundaries
      if (obj.position.y < 0) {
        obj.position.y = 0;
        obj.velocity.y = -obj.velocity.y * 0.7; // Bounce with energy loss
      }

      return obj;
    });
  }
}

// Physics object interface
interface PhysicsObject {
  id: string;
  position: { x: number; y: number; z: number };
  velocity: { x: number; y: number; z: number };
  mass: number;
}

// Client-side wrapper class (runs in main thread)
export class StateWorker {
  private worker: Worker | null = null;
  private pendingRequests: Map<string, {
    resolve: (value: any) => void;
    reject: (error: any) => void;
  }> = new Map();
  private requestId = 0;

  /**
   * Initialize the worker
   */
  async initialize(): Promise<void> {
    // Create worker from this file
    const workerCode = `
      ${StateWorkerImpl.toString()}
      ${PhysicsObject ? `// PhysicsObject interface included` : ''}
      new StateWorkerImpl();
    `;

    const blob = new Blob([workerCode], { type: 'application/javascript' });
    const workerUrl = URL.createObjectURL(blob);

    this.worker = new Worker(workerUrl);

    // Set up message handler
    this.worker.addEventListener('message', (event: MessageEvent<WorkerResponse>) => {
      this.handleResponse(event.data);
    });

    // Clean up URL
    URL.revokeObjectURL(workerUrl);
  }

  /**
   * Handle worker responses
   */
  private handleResponse(response: WorkerResponse): void {
    const pending = this.pendingRequests.get(response.id);

    if (!pending) {
      console.warn(`No pending request for response: ${response.id}`);
      return;
    }

    this.pendingRequests.delete(response.id);

    if (response.error) {
      pending.reject(new Error(response.error));
    } else {
      pending.resolve(response.result);
    }
  }

  /**
   * Send message to worker and wait for response
   */
  private async sendMessage<T>(type: string, payload: any): Promise<T> {
    if (!this.worker) {
      await this.initialize();
    }

    const id = String(this.requestId++);

    return new Promise((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });

      const message: WorkerMessage = {
        id,
        type: type as any,
        payload
      };

      this.worker!.postMessage(message);

      // Timeout after 30 seconds
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error(`Worker request timeout: ${type}`));
        }
      }, 30000);
    });
  }

  /**
   * Compute diff between two states
   */
  async computeDiff(oldState: any, newState: any): Promise<StateDiff> {
    return this.sendMessage<StateDiff>('computeDiff', { oldState, newState });
  }

  /**
   * Interpolate between states
   */
  async interpolateStates(
    states: any[],
    progress: number,
    easing: 'linear' | 'ease-in' | 'ease-out' | 'ease-in-out' = 'linear'
  ): Promise<any> {
    return this.sendMessage('interpolate', { states, progress, easing });
  }

  /**
   * Validate state
   */
  async validateState(state: any): Promise<{
    valid: boolean;
    errors: string[];
    warnings: string[];
  }> {
    return this.sendMessage('validate', { state });
  }

  /**
   * Optimize state
   */
  async optimizeState(state: any): Promise<{
    optimized: any;
    compression: number;
  }> {
    return this.sendMessage('optimize', { state });
  }

  /**
   * Simulate physics
   */
  async simulatePhysics(
    objects: any[],
    deltaTime: number,
    gravity = { x: 0, y: -9.81, z: 0 }
  ): Promise<any[]> {
    return this.sendMessage('physics', { objects, deltaTime, gravity });
  }

  /**
   * Terminate the worker
   */
  terminate(): void {
    if (this.worker) {
      this.worker.terminate();
      this.worker = null;
    }

    this.pendingRequests.clear();
  }
}