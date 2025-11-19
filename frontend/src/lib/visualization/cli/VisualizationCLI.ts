/**
 * VisualizationCLI.ts
 * Command-line interface for testing and debugging the visualization system
 */

import {
  MachineState,
  VisualizationProvider,
  VisualizationOptions,
  PerformanceMetrics
} from '../providers/VisualizationProvider';
import { ProviderFactory, ProviderType } from '../providers/ProviderFactory';

export interface CLICommand {
  name: string;
  description: string;
  execute: (args: string[]) => Promise<any>;
}

export interface CLIOptions {
  headless?: boolean;
  provider?: ProviderType;
  verbose?: boolean;
  recordPerformance?: boolean;
  outputFormat?: 'json' | 'text' | 'table';
}

export class VisualizationCLI {
  private provider: VisualizationProvider | null = null;
  private factory: ProviderFactory;
  private state: MachineState;
  private commands: Map<string, CLICommand> = new Map();
  private performanceLog: PerformanceMetrics[] = [];
  private options: CLIOptions;
  private canvas: HTMLCanvasElement | null = null;
  private isRunning: boolean = false;
  private stepCount: number = 0;

  constructor(options: CLIOptions = {}) {
    this.options = {
      headless: false,
      provider: 'auto',
      verbose: false,
      recordPerformance: false,
      outputFormat: 'text',
      ...options
    };

    this.factory = ProviderFactory.getInstance();
    this.state = this.createInitialState();
    this.registerCommands();
  }

  /**
   * Initialize the CLI with optional canvas
   */
  async initialize(canvas?: HTMLCanvasElement): Promise<void> {
    if (!this.options.headless && !canvas) {
      // Create offscreen canvas for headless mode
      this.canvas = document.createElement('canvas');
      this.canvas.width = 800;
      this.canvas.height = 600;
    } else if (canvas) {
      this.canvas = canvas;
    }

    if (this.canvas && !this.options.headless) {
      const visualizationOptions: VisualizationOptions = {
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

      this.provider = await this.factory.createProvider(
        this.canvas,
        visualizationOptions,
        { preferredProvider: this.options.provider }
      );

      this.provider.onStateChange((state) => {
        this.handleStateChange(state);
      });

      this.provider.onError((error) => {
        this.handleError(error);
      });
    }

    this.log('VisualizationCLI initialized', 'info');
  }

  /**
   * Register available commands
   */
  private registerCommands(): void {
    this.addCommand({
      name: 'help',
      description: 'Show available commands',
      execute: async () => this.showHelp()
    });

    this.addCommand({
      name: 'load',
      description: 'Load state from file',
      execute: async (args) => this.loadState(args[0])
    });

    this.addCommand({
      name: 'save',
      description: 'Save current state to file',
      execute: async (args) => this.saveState(args[0])
    });

    this.addCommand({
      name: 'step',
      description: 'Step through N instructions',
      execute: async (args) => this.step(parseInt(args[0] || '1'))
    });

    this.addCommand({
      name: 'run',
      description: 'Run until halt or breakpoint',
      execute: async () => this.run()
    });

    this.addCommand({
      name: 'halt',
      description: 'Halt execution',
      execute: async () => this.halt()
    });

    this.addCommand({
      name: 'reset',
      description: 'Reset machine state',
      execute: async () => this.reset()
    });

    this.addCommand({
      name: 'registers',
      description: 'Display register values',
      execute: async () => this.getRegisters()
    });

    this.addCommand({
      name: 'memory',
      description: 'Display memory contents',
      execute: async (args) => this.getMemory(
        parseInt(args[0] || '0'),
        parseInt(args[1] || '16')
      )
    });

    this.addCommand({
      name: 'breakpoint',
      description: 'Set breakpoint at address',
      execute: async (args) => this.setBreakpoint(parseInt(args[0]))
    });

    this.addCommand({
      name: 'watch',
      description: 'Watch a memory address',
      execute: async (args) => this.watchAddress(parseInt(args[0]))
    });

    this.addCommand({
      name: 'performance',
      description: 'Show performance metrics',
      execute: async () => this.getPerformance()
    });

    this.addCommand({
      name: 'screenshot',
      description: 'Capture screenshot',
      execute: async (args) => this.captureScreenshot(args[0])
    });

    this.addCommand({
      name: 'provider',
      description: 'Switch visualization provider',
      execute: async (args) => this.switchProvider(args[0] as ProviderType)
    });

    this.addCommand({
      name: 'quality',
      description: 'Set rendering quality',
      execute: async (args) => this.setQuality(args[0] as 'low' | 'medium' | 'high')
    });

    this.addCommand({
      name: 'profile',
      description: 'Start/stop performance profiling',
      execute: async (args) => this.toggleProfiling(args[0] === 'start')
    });

    this.addCommand({
      name: 'export',
      description: 'Export state and metrics',
      execute: async (args) => this.exportData(args[0])
    });
  }

  /**
   * Add a custom command
   */
  addCommand(command: CLICommand): void {
    this.commands.set(command.name, command);
  }

  /**
   * Execute a command by name
   */
  async execute(commandLine: string): Promise<any> {
    const parts = commandLine.trim().split(/\s+/);
    const commandName = parts[0];
    const args = parts.slice(1);

    const command = this.commands.get(commandName);
    if (!command) {
      throw new Error(`Unknown command: ${commandName}`);
    }

    try {
      const result = await command.execute(args);
      this.log(`Command '${commandName}' executed successfully`, 'success');
      return result;
    } catch (error) {
      this.log(`Command '${commandName}' failed: ${error}`, 'error');
      throw error;
    }
  }

  /**
   * Load machine state from JSON file
   */
  async loadState(filename: string): Promise<void> {
    try {
      const response = await fetch(filename);
      const data = await response.json();
      this.state = this.parseState(data);

      if (this.provider) {
        this.provider.render(this.state);
      }

      this.log(`State loaded from ${filename}`, 'info');
    } catch (error) {
      throw new Error(`Failed to load state: ${error}`);
    }
  }

  /**
   * Save current state to JSON file
   */
  async saveState(filename: string): Promise<void> {
    const data = JSON.stringify(this.state, null, 2);
    const blob = new Blob([data], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    // Create download link
    const a = document.createElement('a');
    a.href = url;
    a.download = filename || 'machine-state.json';
    a.click();

    URL.revokeObjectURL(url);
    this.log(`State saved to ${filename}`, 'info');
  }

  /**
   * Step through N instructions
   */
  async step(count: number = 1): Promise<void> {
    for (let i = 0; i < count; i++) {
      if (!this.state.runningFlag) {
        this.log('Machine halted', 'warning');
        break;
      }

      // Simulate instruction execution
      this.state.programCounter++;
      this.state.cycles++;
      this.stepCount++;

      // Update visualization
      if (this.provider) {
        this.provider.render(this.state);
      }

      // Record performance if enabled
      if (this.options.recordPerformance && this.provider) {
        this.performanceLog.push(this.provider.getPerformanceMetrics());
      }

      // Small delay for animation
      await this.delay(50);
    }

    this.log(`Stepped ${count} instruction(s)`, 'info');
  }

  /**
   * Run until halt
   */
  async run(): Promise<void> {
    this.isRunning = true;
    this.log('Starting execution...', 'info');

    while (this.isRunning && this.state.runningFlag) {
      await this.step(1);

      // Check for breakpoints
      if (this.hasBreakpoint(this.state.programCounter)) {
        this.log(`Breakpoint hit at ${this.state.programCounter}`, 'warning');
        break;
      }
    }

    this.isRunning = false;
    this.log('Execution stopped', 'info');
  }

  /**
   * Halt execution
   */
  halt(): void {
    this.isRunning = false;
    this.state.runningFlag = false;
    this.log('Machine halted', 'info');
  }

  /**
   * Reset machine state
   */
  reset(): void {
    this.state = this.createInitialState();
    this.stepCount = 0;

    if (this.provider) {
      this.provider.render(this.state);
    }

    this.log('Machine reset', 'info');
  }

  /**
   * Get register values
   */
  getRegisters(): Map<string, number> {
    return new Map(this.state.registers);
  }

  /**
   * Get memory contents
   */
  getMemory(start: number, length: number): Uint8Array {
    return this.state.memory.slice(start, start + length);
  }

  /**
   * Set breakpoint
   */
  private breakpoints: Set<number> = new Set();

  setBreakpoint(address: number): void {
    this.breakpoints.add(address);
    this.log(`Breakpoint set at ${address}`, 'info');
  }

  private hasBreakpoint(address: number): boolean {
    return this.breakpoints.has(address);
  }

  /**
   * Watch memory address
   */
  private watchedAddresses: Map<number, number> = new Map();

  watchAddress(address: number): void {
    this.watchedAddresses.set(address, this.state.memory[address]);
    this.log(`Watching address ${address}`, 'info');
  }

  /**
   * Get performance metrics
   */
  getPerformance(): PerformanceMetrics | null {
    if (!this.provider) return null;
    return this.provider.getPerformanceMetrics();
  }

  /**
   * Capture screenshot
   */
  async captureScreenshot(filename?: string): Promise<Blob | null> {
    if (!this.provider) {
      this.log('No visualization provider available', 'error');
      return null;
    }

    const blob = await this.provider.captureScreenshot();

    if (filename) {
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = filename;
      a.click();
      URL.revokeObjectURL(url);
    }

    this.log('Screenshot captured', 'info');
    return blob;
  }

  /**
   * Switch visualization provider
   */
  async switchProvider(type: ProviderType): Promise<void> {
    if (!this.canvas) {
      throw new Error('No canvas available');
    }

    // Dispose current provider
    if (this.provider) {
      this.provider.dispose();
    }

    // Create new provider
    const visualizationOptions: VisualizationOptions = {
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

    this.provider = await this.factory.createProvider(
      this.canvas,
      visualizationOptions,
      { preferredProvider: type, forceProvider: true }
    );

    // Render current state
    this.provider.render(this.state);

    this.log(`Switched to ${type} provider`, 'info');
  }

  /**
   * Set rendering quality
   */
  setQuality(level: 'low' | 'medium' | 'high'): void {
    if (this.provider) {
      this.provider.setQuality(level);
      this.log(`Quality set to ${level}`, 'info');
    }
  }

  /**
   * Toggle performance profiling
   */
  toggleProfiling(enable: boolean): void {
    this.options.recordPerformance = enable;

    if (enable) {
      this.performanceLog = [];
      this.log('Performance profiling started', 'info');
    } else {
      this.log(`Performance profiling stopped. ${this.performanceLog.length} samples recorded`, 'info');
    }
  }

  /**
   * Export state and metrics
   */
  async exportData(filename: string): Promise<void> {
    const data = {
      state: this.state,
      performance: this.performanceLog,
      stepCount: this.stepCount,
      timestamp: new Date().toISOString()
    };

    const json = JSON.stringify(data, null, 2);
    const blob = new Blob([json], { type: 'application/json' });
    const url = URL.createObjectURL(blob);

    const a = document.createElement('a');
    a.href = url;
    a.download = filename || 'visualization-export.json';
    a.click();

    URL.revokeObjectURL(url);
    this.log(`Data exported to ${filename}`, 'info');
  }

  /**
   * Show help information
   */
  private showHelp(): string {
    const help = Array.from(this.commands.values())
      .map(cmd => `  ${cmd.name.padEnd(15)} - ${cmd.description}`)
      .join('\n');

    const message = `Available commands:\n${help}`;
    this.log(message, 'info');
    return message;
  }

  /**
   * Create initial machine state
   */
  private createInitialState(): MachineState {
    return {
      registers: new Map([
        ['R0', 0],
        ['R1', 0],
        ['R2', 0],
        ['R3', 0]
      ]),
      memory: new Uint8Array(4096),
      programCounter: 0,
      accumulator: 0,
      carryFlag: false,
      runningFlag: true,
      columns: Array(10).fill(null).map((_, i) => ({
        id: `column_${i}`,
        value: 0,
        isActive: false,
        rotation: 0
      })),
      store: {
        positions: Array(32).fill(0),
        capacity: 32,
        activeIndices: []
      },
      mill: {
        operation: 'idle',
        operandA: 0,
        operandB: 0,
        result: 0,
        progress: 0
      },
      barrels: Array(3).fill(null).map((_, i) => ({
        id: `barrel_${i}`,
        programCounter: 0,
        instructions: [],
        isActive: false
      })),
      cycles: 0,
      timestamp: Date.now()
    };
  }

  /**
   * Parse state from JSON
   */
  private parseState(data: any): MachineState {
    return {
      ...data,
      registers: new Map(Object.entries(data.registers || {})),
      memory: new Uint8Array(data.memory || [])
    };
  }

  /**
   * Handle state change events
   */
  private handleStateChange(state: MachineState): void {
    // Check watched addresses
    this.watchedAddresses.forEach((oldValue, address) => {
      const newValue = state.memory[address];
      if (newValue !== oldValue) {
        this.log(`Watch: Address ${address} changed from ${oldValue} to ${newValue}`, 'warning');
        this.watchedAddresses.set(address, newValue);
      }
    });
  }

  /**
   * Handle error events
   */
  private handleError(error: Error): void {
    this.log(`Error: ${error.message}`, 'error');
  }

  /**
   * Logging utility
   */
  private log(message: string, level: 'info' | 'warning' | 'error' | 'success' = 'info'): void {
    if (!this.options.verbose && level === 'info') return;

    const timestamp = new Date().toISOString();
    const prefix = {
      info: '[INFO]',
      warning: '[WARN]',
      error: '[ERROR]',
      success: '[OK]'
    }[level];

    console.log(`${timestamp} ${prefix} ${message}`);
  }

  /**
   * Utility delay function
   */
  private delay(ms: number): Promise<void> {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  /**
   * Cleanup resources
   */
  dispose(): void {
    if (this.provider) {
      this.provider.dispose();
      this.provider = null;
    }

    this.factory.clearCache();
    this.commands.clear();
    this.breakpoints.clear();
    this.watchedAddresses.clear();
    this.performanceLog = [];
  }
}