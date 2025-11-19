/**
 * EmulatorStateBridge - Connects Backend Emulator to Visualization
 * 
 * This bridge connects the WebSocket state updates from the backend
 * emulator to the visualization providers (ThreeJS/Canvas2D), handling:
 * - State synchronization with diff calculation
 * - Animation interpolation for smooth visuals
 * - Command dispatch (execute, pause, step, reset)
 * - Performance monitoring
 */

import type { MachineState, StateDiff } from '../state/types';
import { WebSocketClient, createWebSocketClient } from '../state/WebSocketClient';
import { machineStateStore } from '../state/MachineStateStore';
import type { VisualizationProvider } from '../providers/VisualizationProvider';
import { ProviderFactory } from '../providers/ProviderFactory';

export interface EmulatorStateUpdate {
  machineState: MachineState;
  timestamp: number;
  cycleNumber: number;
  phaseAngle?: number;
}

export interface BridgeConfig {
  wsEndpoint: string;
  canvas: HTMLCanvasElement;
  providerType?: 'threejs' | 'canvas2d' | 'auto';
  updateThrottleMs?: number;
  enableLogging?: boolean;
}

export interface ExecutionCommand {
  type: 'execute' | 'pause' | 'resume' | 'step' | 'reset';
  payload?: any;
}

/**
 * EmulatorStateBridge - Main integration class
 */
export class EmulatorStateBridge {
  private wsClient: WebSocketClient;
  private provider: VisualizationProvider | null = null;
  private previousState: MachineState | null = null;
  private updateQueue: EmulatorStateUpdate[] = [];
  private isProcessing = false;
  private config: Required<BridgeConfig>;
  private animationFrameId: number | null = null;
  private lastUpdateTime = 0;
  private performanceMetrics = {
    updatesReceived: 0,
    updatesProcessed: 0,
    averageUpdateTime: 0,
    droppedUpdates: 0
  };

  constructor(config: BridgeConfig) {
    this.config = {
      wsEndpoint: config.wsEndpoint,
      canvas: config.canvas,
      providerType: config.providerType ?? 'auto',
      updateThrottleMs: config.updateThrottleMs ?? 16, // 60fps
      enableLogging: config.enableLogging ?? false
    };

    // Create WebSocket client
    this.wsClient = createWebSocketClient(this.config.wsEndpoint, {
      reconnectInterval: 3000,
      maxReconnectAttempts: 10,
      heartbeatInterval: 30000
    });

    this.setupWebSocketHandlers();
  }

  /**
   * Initialize visualization provider and connect
   */
  async initialize(): Promise<void> {
    try {
      // Create visualization provider
      const factory = ProviderFactory.getInstance();
      this.provider = await factory.createProvider(
        this.config.canvas,
        {
          quality: 'medium',
          theme: 'victorian',
          showAnnotations: true,
          animationSpeed: 1.0,
          cameraMode: 'orbit',
          enablePhysics: false,
          enableShadows: true,
          antialias: true,
          maxFPS: 60
        },
        {
          preferredProvider: this.config.providerType,
          forceProvider: this.config.providerType !== 'auto'
        }
      );

      if (!this.provider) {
        throw new Error('Failed to create visualization provider');
      }

      this.log('Visualization provider initialized:', this.provider.constructor.name);

      // Connect WebSocket
      this.wsClient.connect();

      this.log('EmulatorStateBridge initialized successfully');

    } catch (error) {
      console.error('[EmulatorStateBridge] Initialization failed:', error);
      throw error;
    }
  }

  /**
   * Setup WebSocket message handlers
   */
  private setupWebSocketHandlers(): void {
    // Handle state updates
    this.wsClient.onMessage('STATE_UPDATE', (payload: any) => {
      this.performanceMetrics.updatesReceived++;

      const update: EmulatorStateUpdate = {
        machineState: payload.state,
        timestamp: payload.timestamp || Date.now(),
        cycleNumber: payload.cycleNumber || 0,
        phaseAngle: payload.phaseAngle
      };

      this.enqueueStateUpdate(update);
    });

    // Handle complete snapshots
    this.wsClient.onMessage('STATE_SNAPSHOT', (payload: any) => {
      this.log('Received state snapshot');
      if (payload.state) {
        this.enqueueStateUpdate({
          machineState: payload.state,
          timestamp: Date.now(),
          cycleNumber: 0
        });
      }
    });

    // Handle execution complete
    this.wsClient.onMessage('ANIMATION_COMPLETE', () => {
      this.log('Execution complete');
      this.handleExecutionComplete();
    });

    // Handle errors
    this.wsClient.onError((error) => {
      console.error('[EmulatorStateBridge] WebSocket error:', error);
    });

    // Handle status changes
    this.wsClient.onStatusChange((status) => {
      this.log('Connection status:', status);
      
      if (status === 'connected') {
        // Request initial state snapshot
        this.requestSnapshot();
      }
    });
  }

  /**
   * Enqueue state update for processing
   */
  private enqueueStateUpdate(update: EmulatorStateUpdate): void {
    // Drop old updates if queue is too large (prevent memory buildup)
    if (this.updateQueue.length > 100) {
      this.performanceMetrics.droppedUpdates++;
      this.updateQueue.shift();
    }

    this.updateQueue.push(update);

    if (!this.isProcessing) {
      this.processUpdateQueue();
    }
  }

  /**
   * Process update queue with throttling
   */
  private async processUpdateQueue(): Promise<void> {
    if (this.isProcessing) return;

    this.isProcessing = true;

    while (this.updateQueue.length > 0) {
      const now = performance.now();
      const timeSinceLastUpdate = now - this.lastUpdateTime;

      // Throttle updates to target frame rate
      if (timeSinceLastUpdate < this.config.updateThrottleMs) {
        // Wait for next frame
        await new Promise(resolve => {
          this.animationFrameId = requestAnimationFrame(() => resolve(undefined));
        });
        continue;
      }

      const update = this.updateQueue.shift()!;
      const startTime = performance.now();

      await this.applyStateUpdate(update);

      const updateTime = performance.now() - startTime;
      this.updatePerformanceMetrics(updateTime);
      this.lastUpdateTime = now;
    }

    this.isProcessing = false;
  }

  /**
   * Apply state update to visualization
   */
  private async applyStateUpdate(update: EmulatorStateUpdate): Promise<void> {
    const { machineState, cycleNumber, phaseAngle } = update;

    try {
      // Calculate state diff
      const diff = this.calculateStateDiff(this.previousState, machineState);

      // Update machine state store (reactive)
      machineStateStore.setState(machineState);

      // Update visualization provider
      if (this.provider) {
        await this.provider.updateState(machineState, diff);

        // Update phase angle if provided
        if (phaseAngle !== undefined && this.provider.updatePhaseAngle) {
          this.provider.updatePhaseAngle(phaseAngle);
        }
      }

      // Store for next diff
      this.previousState = machineState;

      this.performanceMetrics.updatesProcessed++;

    } catch (error) {
      console.error('[EmulatorStateBridge] Failed to apply state update:', error);
    }
  }

  /**
   * Calculate difference between two states
   */
  private calculateStateDiff(
    oldState: MachineState | null,
    newState: MachineState
  ): StateDiff {
    if (!oldState) {
      // First state - everything is new
      return {
        changedColumns: newState.columns.map((_, i) => i),
        changedCarryFlags: [0],
        changedStoreIndices: newState.store.positions.map((_, i) => i),
        millStateChanged: true,
        barrelsChanged: newState.barrels.map((_, i) => i),
        phaseChanged: true,
        angleChanged: true
      };
    }

    const diff: StateDiff = {
      changedColumns: [],
      changedCarryFlags: [],
      changedStoreIndices: [],
      barrelsChanged: []
    };

    // Check column changes
    newState.columns.forEach((column, index) => {
      const oldColumn = oldState.columns[index];
      if (!oldColumn || column.value !== oldColumn.value || column.isActive !== oldColumn.isActive) {
        diff.changedColumns.push(index);
      }
    });

    // Check carry flag
    if (newState.carryFlag !== oldState.carryFlag) {
      diff.changedCarryFlags.push(0);
    }

    // Check store changes
    newState.store.positions.forEach((value, index) => {
      if (value !== oldState.store.positions[index]) {
        diff.changedStoreIndices!.push(index);
      }
    });

    // Check mill state
    diff.millStateChanged =
      newState.mill.operation !== oldState.mill.operation ||
      newState.mill.operandA !== oldState.mill.operandA ||
      newState.mill.operandB !== oldState.mill.operandB ||
      newState.mill.result !== oldState.mill.result;

    // Check barrel changes
    newState.barrels.forEach((barrel, index) => {
      const oldBarrel = oldState.barrels[index];
      if (!oldBarrel || 
          barrel.isActive !== oldBarrel.isActive || 
          barrel.programCounter !== oldBarrel.programCounter) {
        diff.barrelsChanged!.push(index);
      }
    });

    // Check phase/angle
    diff.phaseChanged = newState.phase !== oldState.phase;
    diff.angleChanged = newState.timingAngle !== oldState.timingAngle;

    return diff;
  }

  /**
   * Execute program
   */
  execute(polynomial: number[], xRange: [number, number]): void {
    this.wsClient.send('EXECUTE', {
      coefficients: polynomial,
      x_start: xRange[0],
      x_end: xRange[1]
    });
  }

  /**
   * Pause execution
   */
  pause(): void {
    this.wsClient.send('PAUSE', {});
  }

  /**
   * Resume execution
   */
  resume(): void {
    this.wsClient.send('RESUME', {});
  }

  /**
   * Step one cycle
   */
  stepCycle(): void {
    this.wsClient.send('STEP', {});
  }

  /**
   * Reset emulator
   */
  reset(): void {
    this.wsClient.send('RESET', {});
    this.previousState = null;
    this.updateQueue = [];
    machineStateStore.reset();
    
    if (this.provider) {
      this.provider.reset();
    }
  }

  /**
   * Request state snapshot from backend
   */
  requestSnapshot(): void {
    this.wsClient.send('REQUEST_SNAPSHOT', {});
  }

  /**
   * Set breakpoint at cycle number
   */
  setBreakpoint(cycleNumber: number): void {
    this.wsClient.send('BREAKPOINT_SET', { cycleNumber });
  }

  /**
   * Remove breakpoint
   */
  removeBreakpoint(cycleNumber: number): void {
    this.wsClient.send('BREAKPOINT_REMOVE', { cycleNumber });
  }

  /**
   * Get performance metrics
   */
  getPerformanceMetrics() {
    return { ...this.performanceMetrics };
  }

  /**
   * Get WebSocket connection status
   */
  getConnectionStatus() {
    return this.wsClient.getStatus();
  }

  /**
   * Get current machine state
   */
  getCurrentState(): MachineState | null {
    return machineStateStore.getState();
  }

  /**
   * Cleanup and disconnect
   */
  dispose(): void {
    if (this.animationFrameId !== null) {
      cancelAnimationFrame(this.animationFrameId);
      this.animationFrameId = null;
    }

    this.wsClient.disconnect();

    if (this.provider) {
      this.provider.dispose();
      this.provider = null;
    }

    machineStateStore.reset();
    this.log('EmulatorStateBridge disposed');
  }

  /**
   * Handle execution complete event
   */
  private handleExecutionComplete(): void {
    // Could emit custom event or callback here
    this.log('Program execution completed');
  }

  /**
   * Update performance metrics
   */
  private updatePerformanceMetrics(updateTime: number): void {
    const alpha = 0.1; // Exponential moving average factor
    this.performanceMetrics.averageUpdateTime =
      alpha * updateTime +
      (1 - alpha) * this.performanceMetrics.averageUpdateTime;
  }

  /**
   * Logging utility
   */
  private log(...args: any[]): void {
    if (this.config.enableLogging) {
      console.log('[EmulatorStateBridge]', ...args);
    }
  }
}

/**
 * Factory function for creating bridge
 */
export function createEmulatorStateBridge(config: BridgeConfig): EmulatorStateBridge {
  return new EmulatorStateBridge(config);
}
