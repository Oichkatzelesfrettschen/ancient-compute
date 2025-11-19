<!--
  EmulatorView.svelte
  Integrated emulator component with provider-based visualization
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import {
    ProviderFactory,
    type ProviderType
  } from '$lib/visualization/providers/ProviderFactory';
  import type {
    VisualizationProvider,
    MachineState,
    VisualizationOptions
  } from '$lib/visualization/providers/VisualizationProvider';
  import { StateWorker } from '$lib/visualization/workers/StateWorker';
  import { VisualizationCLI } from '$lib/visualization/cli/VisualizationCLI';

  // Props
  export let initialState: MachineState | null = null;
  export let providerType: ProviderType = 'auto';
  export let quality: 'low' | 'medium' | 'high' = 'medium';
  export let theme: 'victorian' | 'modern' | 'schematic' = 'victorian';
  export let enableCLI: boolean = false;

  // State
  let canvas: HTMLCanvasElement;
  let provider: VisualizationProvider | null = null;
  let factory: ProviderFactory;
  let stateWorker: StateWorker | null = null;
  let cli: VisualizationCLI | null = null;

  let isLoading = true;
  let error: string | null = null;
  let currentProvider: ProviderType = 'auto';
  let deviceCapabilities: any = null;
  let performanceMetrics: any = null;
  let showSettings = false;
  let showCLI = false;
  let cliInput = '';
  let cliOutput: string[] = [];

  // Machine state
  let machineState: MachineState = createDefaultState();

  // Animation
  let animationFrame: number | null = null;
  let isRunning = false;

  // Create default machine state
  function createDefaultState(): MachineState {
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
      runningFlag: false,
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

  // Initialize visualization on mount
  onMount(async () => {
    if (!canvas) return;

    try {
      isLoading = true;
      error = null;

      // Get factory instance
      factory = ProviderFactory.getInstance();

      // Get device capabilities
      deviceCapabilities = await factory.getDeviceCapabilities(canvas);
      console.log('Device capabilities:', deviceCapabilities);

      // Set up visualization options
      const options: VisualizationOptions = {
        quality,
        theme,
        showAnnotations: true,
        animationSpeed: 1.0,
        cameraMode: 'orbit',
        enablePhysics: false,
        enableShadows: quality !== 'low',
        antialias: quality === 'high',
        maxFPS: 60
      };

      // Create provider with auto-detection or forced type
      provider = await factory.createProvider(canvas, options, {
        preferredProvider: providerType,
        forceProvider: providerType !== 'auto'
      });

      // Determine which provider was actually used
      if (provider) {
        currentProvider = provider.constructor.name.includes('Three')
          ? 'threejs'
          : 'canvas2d';
      }

      // Set up state worker for async operations
      stateWorker = new StateWorker();
      await stateWorker.initialize();

      // Set up CLI if enabled
      if (enableCLI) {
        cli = new VisualizationCLI({
          headless: false,
          provider: currentProvider,
          verbose: true,
          recordPerformance: true
        });
        await cli.initialize(canvas);
      }

      // Use initial state if provided
      if (initialState) {
        machineState = initialState;
      }

      // Set up event handlers
      provider.onStateChange((state) => {
        handleStateChange(state);
      });

      provider.onError((err) => {
        console.error('Visualization error:', err);
        error = err.message;
      });

      // Initial render
      provider.render(machineState);

      // Start performance monitoring
      startPerformanceMonitoring();

      isLoading = false;
    } catch (err) {
      console.error('Failed to initialize visualization:', err);
      error = err instanceof Error ? err.message : String(err);
      isLoading = false;
    }
  });

  // Cleanup on destroy
  onDestroy(() => {
    stopAnimation();

    if (provider) {
      provider.dispose();
      provider = null;
    }

    if (stateWorker) {
      stateWorker.terminate();
      stateWorker = null;
    }

    if (cli) {
      cli.dispose();
      cli = null;
    }

    if (factory) {
      factory.clearCache();
    }
  });

  // Handle window resize
  function handleResize() {
    if (!canvas || !provider) return;

    const rect = canvas.getBoundingClientRect();
    canvas.width = rect.width;
    canvas.height = rect.height;

    provider.resize(rect.width, rect.height);
  }

  // Handle state changes
  async function handleStateChange(state: MachineState) {
    if (!stateWorker) return;

    // Compute diff asynchronously
    const diff = await stateWorker.computeDiff(machineState, state);
    console.log(`State diff: ${diff.changes.length} changes`);

    // Update local state
    machineState = state;

    // Update performance metrics
    if (provider) {
      performanceMetrics = provider.getPerformanceMetrics();
    }
  }

  // Start performance monitoring
  function startPerformanceMonitoring() {
    const updateMetrics = () => {
      if (provider) {
        performanceMetrics = provider.getPerformanceMetrics();
      }
      setTimeout(updateMetrics, 1000);
    };
    updateMetrics();
  }

  // Control functions
  function startAnimation() {
    if (isRunning) return;

    isRunning = true;
    machineState.runningFlag = true;

    const animate = () => {
      if (!isRunning) return;

      // Update machine state
      machineState.programCounter++;
      machineState.cycles++;

      // Simulate column rotation
      machineState.columns.forEach((col, i) => {
        col.value = (col.value + Math.random()) % 10;
        col.rotation = (col.rotation + 0.1) % (Math.PI * 2);
      });

      // Simulate mill operation
      if (Math.random() < 0.1) {
        const operations = ['idle', 'add', 'subtract', 'multiply', 'divide'];
        machineState.mill.operation = operations[
          Math.floor(Math.random() * operations.length)
        ] as any;
        machineState.mill.progress = 0;
      }

      if (machineState.mill.operation !== 'idle') {
        machineState.mill.progress = Math.min(1, machineState.mill.progress + 0.05);
        if (machineState.mill.progress >= 1) {
          machineState.mill.operation = 'idle';
        }
      }

      // Update timestamp
      machineState.timestamp = Date.now();

      // Render updated state
      if (provider) {
        provider.render(machineState);
      }

      animationFrame = requestAnimationFrame(animate);
    };

    animate();
  }

  function stopAnimation() {
    isRunning = false;
    machineState.runningFlag = false;

    if (animationFrame) {
      cancelAnimationFrame(animationFrame);
      animationFrame = null;
    }
  }

  function resetMachine() {
    stopAnimation();
    machineState = createDefaultState();

    if (provider) {
      provider.render(machineState);
    }
  }

  function stepMachine() {
    machineState.programCounter++;
    machineState.cycles++;

    if (provider) {
      provider.render(machineState);
    }
  }

  // Provider switching
  async function switchProvider(type: ProviderType) {
    if (!canvas) return;

    try {
      isLoading = true;
      error = null;

      // Dispose current provider
      if (provider) {
        provider.dispose();
      }

      // Create new provider
      const options: VisualizationOptions = {
        quality,
        theme,
        showAnnotations: true,
        animationSpeed: 1.0,
        cameraMode: 'orbit',
        enablePhysics: false,
        enableShadows: quality !== 'low',
        antialias: quality === 'high',
        maxFPS: 60
      };

      provider = await factory.createProvider(canvas, options, {
        preferredProvider: type,
        forceProvider: true
      });

      currentProvider = type;

      // Render current state
      provider.render(machineState);

      isLoading = false;
    } catch (err) {
      console.error('Failed to switch provider:', err);
      error = err instanceof Error ? err.message : String(err);
      isLoading = false;
    }
  }

  // Quality adjustment
  function setQuality(level: 'low' | 'medium' | 'high') {
    quality = level;

    if (provider) {
      provider.setQuality(level);
    }
  }

  // Screenshot capture
  async function captureScreenshot() {
    if (!provider) return;

    try {
      const blob = await provider.captureScreenshot();
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `babbage-emulator-${Date.now()}.png`;
      a.click();
      URL.revokeObjectURL(url);
    } catch (err) {
      console.error('Failed to capture screenshot:', err);
    }
  }

  // CLI command execution
  async function executeCLICommand() {
    if (!cli || !cliInput.trim()) return;

    try {
      const command = cliInput.trim();
      cliOutput = [...cliOutput, `> ${command}`];

      const result = await cli.execute(command);

      if (result) {
        cliOutput = [...cliOutput, String(result)];
      }

      cliInput = '';
    } catch (err) {
      cliOutput = [...cliOutput, `Error: ${err}`];
    }

    // Scroll to bottom
    setTimeout(() => {
      const cliConsole = document.querySelector('.cli-output');
      if (cliConsole) {
        cliConsole.scrollTop = cliConsole.scrollHeight;
      }
    }, 0);
  }

  // Keyboard handler for CLI
  function handleCLIKeydown(event: KeyboardEvent) {
    if (event.key === 'Enter') {
      executeCLICommand();
    }
  }
</script>

<svelte:window on:resize={handleResize} />

<div class="emulator-container">
  <!-- Header -->
  <div class="emulator-header">
    <h2>Babbage Analytical Engine Emulator</h2>

    <div class="header-controls">
      <button
        class="btn-settings"
        on:click={() => (showSettings = !showSettings)}
        title="Settings"
      >
        ⚙️
      </button>

      {#if enableCLI}
        <button
          class="btn-cli"
          on:click={() => (showCLI = !showCLI)}
          title="CLI"
        >
          &gt;_
        </button>
      {/if}

      <button class="btn-screenshot" on:click={captureScreenshot} title="Screenshot">
        📷
      </button>
    </div>
  </div>

  <!-- Main content -->
  <div class="emulator-content">
    <!-- Visualization canvas -->
    <div class="canvas-container" class:loading={isLoading}>
      <canvas bind:this={canvas} />

      {#if isLoading}
        <div class="loading-overlay">
          <div class="spinner" />
          <p>Initializing {currentProvider} visualization...</p>
        </div>
      {/if}

      {#if error}
        <div class="error-overlay">
          <p>Error: {error}</p>
          <button on:click={() => location.reload()}>Reload</button>
        </div>
      {/if}
    </div>

    <!-- Control panel -->
    <div class="control-panel">
      <div class="controls">
        <button
          class="btn-primary"
          on:click={isRunning ? stopAnimation : startAnimation}
          disabled={isLoading}
        >
          {isRunning ? 'Stop' : 'Run'}
        </button>

        <button class="btn-secondary" on:click={stepMachine} disabled={isLoading || isRunning}>
          Step
        </button>

        <button class="btn-secondary" on:click={resetMachine} disabled={isLoading}>
          Reset
        </button>
      </div>

      <!-- Status display -->
      <div class="status">
        <div class="status-item">
          <span class="label">PC:</span>
          <span class="value">{machineState.programCounter}</span>
        </div>

        <div class="status-item">
          <span class="label">ACC:</span>
          <span class="value">{machineState.accumulator}</span>
        </div>

        <div class="status-item">
          <span class="label">Cycles:</span>
          <span class="value">{machineState.cycles}</span>
        </div>

        <div class="status-item">
          <span class="label">Status:</span>
          <span class="value" class:running={machineState.runningFlag}>
            {machineState.runningFlag ? 'Running' : 'Halted'}
          </span>
        </div>
      </div>

      <!-- Performance metrics -->
      {#if performanceMetrics}
        <div class="metrics">
          <div class="metric">
            <span class="label">FPS:</span>
            <span class="value">{performanceMetrics.fps}</span>
          </div>

          <div class="metric">
            <span class="label">Frame:</span>
            <span class="value">{performanceMetrics.frameTime.toFixed(2)}ms</span>
          </div>

          {#if currentProvider === 'threejs'}
            <div class="metric">
              <span class="label">Draw Calls:</span>
              <span class="value">{performanceMetrics.drawCalls}</span>
            </div>

            <div class="metric">
              <span class="label">Triangles:</span>
              <span class="value">{performanceMetrics.triangles}</span>
            </div>
          {/if}
        </div>
      {/if}
    </div>
  </div>

  <!-- Settings panel -->
  {#if showSettings}
    <div class="settings-panel">
      <h3>Settings</h3>

      <div class="setting-group">
        <label>Provider:</label>
        <select bind:value={currentProvider} on:change={(e) => switchProvider(e.target.value)}>
          <option value="auto">Auto-detect</option>
          <option value="threejs">Three.js (3D)</option>
          <option value="canvas2d">Canvas2D (2D)</option>
        </select>
      </div>

      <div class="setting-group">
        <label>Quality:</label>
        <select bind:value={quality} on:change={(e) => setQuality(e.target.value)}>
          <option value="low">Low</option>
          <option value="medium">Medium</option>
          <option value="high">High</option>
        </select>
      </div>

      <div class="setting-group">
        <label>Theme:</label>
        <select bind:value={theme}>
          <option value="victorian">Victorian</option>
          <option value="modern">Modern</option>
          <option value="schematic">Schematic</option>
        </select>
      </div>

      {#if deviceCapabilities}
        <div class="device-info">
          <h4>Device Info</h4>
          <p>GPU: {deviceCapabilities.gpu}</p>
          <p>Tier: {deviceCapabilities.tier}/3</p>
          <p>WebGL2: {deviceCapabilities.webgl2 ? 'Yes' : 'No'}</p>
          <p>Mobile: {deviceCapabilities.isMobile ? 'Yes' : 'No'}</p>
        </div>
      {/if}

      <button class="btn-close" on:click={() => (showSettings = false)}>Close</button>
    </div>
  {/if}

  <!-- CLI panel -->
  {#if showCLI && enableCLI}
    <div class="cli-panel">
      <h3>Command Line Interface</h3>

      <div class="cli-output">
        {#each cliOutput as line}
          <div class="cli-line">{line}</div>
        {/each}
      </div>

      <div class="cli-input">
        <span class="prompt">&gt;</span>
        <input
          type="text"
          bind:value={cliInput}
          on:keydown={handleCLIKeydown}
          placeholder="Enter command (type 'help' for commands)"
        />
      </div>

      <button class="btn-close" on:click={() => (showCLI = false)}>Close</button>
    </div>
  {/if}
</div>

<style>
  .emulator-container {
    display: flex;
    flex-direction: column;
    height: 100vh;
    background: #1a1a1a;
    color: #ffffff;
    font-family: 'Courier New', monospace;
  }

  .emulator-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 1rem;
    background: #2a2a2a;
    border-bottom: 1px solid #3a3a3a;
  }

  .emulator-header h2 {
    margin: 0;
    font-size: 1.25rem;
    color: #b5a642;
  }

  .header-controls {
    display: flex;
    gap: 0.5rem;
  }

  .header-controls button {
    padding: 0.5rem;
    background: #3a3a3a;
    border: 1px solid #4a4a4a;
    border-radius: 4px;
    color: #ffffff;
    cursor: pointer;
    transition: background 0.2s;
  }

  .header-controls button:hover {
    background: #4a4a4a;
  }

  .emulator-content {
    flex: 1;
    display: flex;
    overflow: hidden;
  }

  .canvas-container {
    flex: 1;
    position: relative;
    background: #0a0a0a;
  }

  .canvas-container canvas {
    width: 100%;
    height: 100%;
    display: block;
  }

  .loading-overlay,
  .error-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    background: rgba(0, 0, 0, 0.9);
  }

  .spinner {
    width: 40px;
    height: 40px;
    border: 3px solid #3a3a3a;
    border-top-color: #b5a642;
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .control-panel {
    width: 300px;
    background: #2a2a2a;
    border-left: 1px solid #3a3a3a;
    padding: 1rem;
    display: flex;
    flex-direction: column;
    gap: 1rem;
  }

  .controls {
    display: flex;
    gap: 0.5rem;
  }

  .controls button {
    flex: 1;
    padding: 0.75rem;
    border: none;
    border-radius: 4px;
    font-weight: bold;
    cursor: pointer;
    transition: all 0.2s;
  }

  .btn-primary {
    background: #b5a642;
    color: #1a1a1a;
  }

  .btn-primary:hover {
    background: #c5b652;
  }

  .btn-secondary {
    background: #3a3a3a;
    color: #ffffff;
    border: 1px solid #4a4a4a;
  }

  .btn-secondary:hover {
    background: #4a4a4a;
  }

  button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .status,
  .metrics {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 0.5rem;
  }

  .status-item,
  .metric {
    display: flex;
    justify-content: space-between;
    padding: 0.5rem;
    background: #1a1a1a;
    border-radius: 4px;
    font-size: 0.875rem;
  }

  .label {
    color: #888888;
  }

  .value {
    color: #ffffff;
    font-weight: bold;
  }

  .value.running {
    color: #00ff00;
  }

  .settings-panel,
  .cli-panel {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background: #2a2a2a;
    border: 1px solid #3a3a3a;
    border-radius: 8px;
    padding: 2rem;
    min-width: 400px;
    max-height: 80vh;
    overflow-y: auto;
    z-index: 100;
  }

  .setting-group {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .setting-group select {
    padding: 0.5rem;
    background: #1a1a1a;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    color: #ffffff;
  }

  .device-info {
    margin-top: 1rem;
    padding: 1rem;
    background: #1a1a1a;
    border-radius: 4px;
  }

  .device-info h4 {
    margin: 0 0 0.5rem 0;
    color: #b5a642;
  }

  .device-info p {
    margin: 0.25rem 0;
    font-size: 0.875rem;
  }

  .cli-output {
    background: #0a0a0a;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    padding: 1rem;
    height: 300px;
    overflow-y: auto;
    margin-bottom: 1rem;
    font-family: 'Courier New', monospace;
  }

  .cli-line {
    margin: 0.25rem 0;
    white-space: pre-wrap;
  }

  .cli-input {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .cli-input .prompt {
    color: #b5a642;
  }

  .cli-input input {
    flex: 1;
    padding: 0.5rem;
    background: #1a1a1a;
    border: 1px solid #3a3a3a;
    border-radius: 4px;
    color: #ffffff;
    font-family: 'Courier New', monospace;
  }

  .btn-close {
    width: 100%;
    padding: 0.75rem;
    background: #3a3a3a;
    border: 1px solid #4a4a4a;
    border-radius: 4px;
    color: #ffffff;
    cursor: pointer;
    margin-top: 1rem;
  }

  .btn-close:hover {
    background: #4a4a4a;
  }
</style>