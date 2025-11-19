<script lang="ts">
  import { createEventDispatcher, onMount } from 'svelte';
  import { spring } from 'svelte/motion';
  import { fade, scale } from 'svelte/transition';
  import { playgroundStore } from '$lib/stores/playgroundStore';

  export let isExecuting = false;

  const dispatch = createEventDispatcher();

  let executionProgress = spring(0, { stiffness: 0.1, damping: 0.9 });
  let memoryUsage = 0;
  let cpuUsage = 0;
  let stepCount = 0;
  let isDebugging = false;
  let executionSpeed = 1; // 1x, 0.5x, 2x speed

  $: executionTime = $playgroundStore.executionTime;
  $: status = isExecuting ? 'running' : stepCount > 0 ? 'paused' : 'idle';

  // Simulate progress during execution
  $: if (isExecuting) {
    startProgressAnimation();
  } else {
    executionProgress.set(0);
  }

  function startProgressAnimation() {
    let progress = 0;
    const interval = setInterval(() => {
      if (!isExecuting || progress >= 95) {
        clearInterval(interval);
        if (!isExecuting) {
          executionProgress.set(100);
          setTimeout(() => executionProgress.set(0), 500);
        }
        return;
      }
      progress += Math.random() * 10;
      executionProgress.set(Math.min(progress, 95));
    }, 200);
  }

  function handleRun() {
    dispatch('run');
    stepCount = 0;
    isDebugging = false;
  }

  function handleStep() {
    if (!isDebugging) {
      isDebugging = true;
    }
    stepCount++;
    dispatch('step');
  }

  function handleStop() {
    dispatch('stop');
    isDebugging = false;
    stepCount = 0;
  }

  function handleReset() {
    dispatch('reset');
    isDebugging = false;
    stepCount = 0;
    memoryUsage = 0;
    cpuUsage = 0;
  }

  function handleSpeedChange(speed: number) {
    executionSpeed = speed;
    // This would be sent to the backend to control execution speed
  }

  // Keyboard shortcuts
  function handleKeydown(event: KeyboardEvent) {
    if (event.ctrlKey || event.metaKey) {
      switch (event.key) {
        case 'Enter':
          event.preventDefault();
          if (!isExecuting) handleRun();
          break;
        case '.':
          event.preventDefault();
          handleStop();
          break;
        case 'Shift':
          if (event.shiftKey) {
            event.preventDefault();
            handleReset();
          }
          break;
      }
    }

    // F10 for step
    if (event.key === 'F10') {
      event.preventDefault();
      handleStep();
    }
  }

  // Simulate resource usage (in production, this would come from the backend)
  function updateResourceUsage() {
    if (isExecuting) {
      memoryUsage = Math.min(100, memoryUsage + Math.random() * 5);
      cpuUsage = 50 + Math.random() * 50;
    } else {
      cpuUsage = Math.max(0, cpuUsage - 10);
    }
  }

  onMount(() => {
    const interval = setInterval(updateResourceUsage, 1000);
    return () => clearInterval(interval);
  });
</script>

<svelte:window on:keydown={handleKeydown} />

<div class="execution-panel">
  <div class="panel-left">
    <div class="control-buttons">
      <button
        class="control-btn primary run-btn"
        on:click={handleRun}
        disabled={isExecuting}
        title="Run code (Ctrl+Enter)"
      >
        <svg class="btn-icon" width="16" height="16" viewBox="0 0 16 16">
          {#if isExecuting}
            <rect x="5" y="4" width="2" height="8" fill="currentColor" />
            <rect x="9" y="4" width="2" height="8" fill="currentColor" />
          {:else}
            <path d="M5 3v10l7-5z" fill="currentColor" />
          {/if}
        </svg>
        <span class="btn-text">{isExecuting ? 'Running...' : 'Run'}</span>
        {#if isExecuting}
          <div class="spinner" transition:scale></div>
        {/if}
      </button>

      <button
        class="control-btn step-btn"
        on:click={handleStep}
        disabled={isExecuting}
        title="Step through code (F10)"
      >
        <svg class="btn-icon" width="16" height="16" viewBox="0 0 16 16">
          <path d="M11 3v10l-3-3v3H3V3h5v3l3-3z" fill="currentColor" />
        </svg>
        <span class="btn-text">Step</span>
        {#if stepCount > 0}
          <span class="step-count" transition:scale>{stepCount}</span>
        {/if}
      </button>

      <button
        class="control-btn stop-btn"
        on:click={handleStop}
        disabled={!isExecuting && stepCount === 0}
        title="Stop execution (Ctrl+.)"
      >
        <svg class="btn-icon" width="16" height="16" viewBox="0 0 16 16">
          <rect x="4" y="4" width="8" height="8" fill="currentColor" />
        </svg>
        <span class="btn-text">Stop</span>
      </button>

      <button
        class="control-btn reset-btn"
        on:click={handleReset}
        title="Reset environment (Ctrl+Shift+R)"
      >
        <svg class="btn-icon" width="16" height="16" viewBox="0 0 16 16">
          <path
            d="M12 8a4 4 0 1 1-4-4V2l3 3-3 3V6a2 2 0 1 0 2 2h2z"
            fill="currentColor"
          />
        </svg>
        <span class="btn-text">Reset</span>
      </button>
    </div>

    <div class="execution-status">
      <span class="status-indicator" class:running={status === 'running'} class:paused={status === 'paused'}>
        <span class="status-dot"></span>
        <span class="status-text">
          {#if status === 'running'}
            Executing
          {:else if status === 'paused'}
            Debugging
          {:else}
            Ready
          {/if}
        </span>
      </span>

      {#if isDebugging}
        <span class="debug-info" transition:fade>
          Step {stepCount}
        </span>
      {/if}
    </div>
  </div>

  <div class="panel-center">
    {#if isExecuting || $executionProgress > 0}
      <div class="progress-bar" transition:fade>
        <div class="progress-track">
          <div
            class="progress-fill"
            style="width: {$executionProgress}%"
          ></div>
        </div>
        <span class="progress-text">{Math.round($executionProgress)}%</span>
      </div>
    {/if}

    <div class="execution-metrics">
      {#if executionTime > 0}
        <div class="metric" transition:scale>
          <svg class="metric-icon" width="14" height="14" viewBox="0 0 14 14">
            <circle cx="7" cy="7" r="6" fill="none" stroke="currentColor" stroke-width="1" />
            <path d="M7 3v4l2 2" stroke="currentColor" stroke-width="1" fill="none" />
          </svg>
          <span class="metric-label">Time:</span>
          <span class="metric-value">{executionTime}ms</span>
        </div>
      {/if}

      {#if memoryUsage > 0}
        <div class="metric" transition:scale={{ delay: 50 }}>
          <svg class="metric-icon" width="14" height="14" viewBox="0 0 14 14">
            <rect x="2" y="4" width="10" height="6" fill="none" stroke="currentColor" stroke-width="1" />
            <rect x="4" y="6" width="2" height="2" fill="currentColor" />
            <rect x="8" y="6" width="2" height="2" fill="currentColor" />
          </svg>
          <span class="metric-label">Memory:</span>
          <span class="metric-value">{memoryUsage.toFixed(1)}MB</span>
        </div>
      {/if}

      {#if cpuUsage > 0}
        <div class="metric" transition:scale={{ delay: 100 }}>
          <svg class="metric-icon" width="14" height="14" viewBox="0 0 14 14">
            <rect x="3" y="3" width="8" height="8" fill="none" stroke="currentColor" stroke-width="1" />
            <rect x="5" y="5" width="4" height="4" fill="currentColor" />
          </svg>
          <span class="metric-label">CPU:</span>
          <span class="metric-value">{cpuUsage.toFixed(0)}%</span>
        </div>
      {/if}
    </div>
  </div>

  <div class="panel-right">
    <div class="speed-control">
      <span class="speed-label">Speed:</span>
      <div class="speed-buttons">
        <button
          class="speed-btn"
          class:active={executionSpeed === 0.5}
          on:click={() => handleSpeedChange(0.5)}
          title="Half speed"
        >
          0.5x
        </button>
        <button
          class="speed-btn"
          class:active={executionSpeed === 1}
          on:click={() => handleSpeedChange(1)}
          title="Normal speed"
        >
          1x
        </button>
        <button
          class="speed-btn"
          class:active={executionSpeed === 2}
          on:click={() => handleSpeedChange(2)}
          title="Double speed"
        >
          2x
        </button>
      </div>
    </div>

    <div class="shortcuts-hint">
      <button
        class="hint-btn"
        title="Keyboard shortcuts"
      >
        <svg width="14" height="14" viewBox="0 0 14 14">
          <rect x="1" y="4" width="12" height="8" rx="1" fill="none" stroke="currentColor" stroke-width="1" />
          <rect x="3" y="6" width="2" height="1" fill="currentColor" />
          <rect x="6" y="6" width="2" height="1" fill="currentColor" />
          <rect x="9" y="6" width="2" height="1" fill="currentColor" />
          <rect x="4" y="8" width="6" height="1" fill="currentColor" />
        </svg>
      </button>
    </div>
  </div>
</div>

<style>
  .execution-panel {
    display: flex;
    align-items: center;
    justify-content: space-between;
    padding: 12px 16px;
    background: var(--color-panel-bg, #252526);
    border-top: 1px solid var(--color-border, #3e3e42);
    border-bottom: 1px solid var(--color-border, #3e3e42);
    gap: 16px;
  }

  .panel-left,
  .panel-center,
  .panel-right {
    display: flex;
    align-items: center;
    gap: 12px;
  }

  .panel-center {
    flex: 1;
    justify-content: center;
  }

  .control-buttons {
    display: flex;
    gap: 8px;
  }

  .control-btn {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 6px 12px;
    background: var(--color-button-bg, #3e3e42);
    color: var(--color-text, #cccccc);
    border: 1px solid var(--color-border, #515151);
    border-radius: 4px;
    cursor: pointer;
    font-size: 13px;
    font-weight: 500;
    transition: all 0.2s;
    position: relative;
  }

  .control-btn:hover:not(:disabled) {
    background: var(--color-button-hover, #4e4e4e);
    border-color: var(--color-border-hover, #616161);
  }

  .control-btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .control-btn.primary {
    background: var(--color-primary, #0e639c);
    border-color: var(--color-primary-border, #1177bb);
    color: white;
  }

  .control-btn.primary:hover:not(:disabled) {
    background: var(--color-primary-hover, #1177bb);
  }

  .run-btn {
    min-width: 80px;
  }

  .btn-icon {
    flex-shrink: 0;
  }

  .btn-text {
    white-space: nowrap;
  }

  .spinner {
    position: absolute;
    right: 8px;
    top: 50%;
    transform: translateY(-50%);
    width: 12px;
    height: 12px;
    border: 2px solid rgba(255, 255, 255, 0.3);
    border-top-color: white;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
  }

  @keyframes spin {
    to {
      transform: translateY(-50%) rotate(360deg);
    }
  }

  .step-count {
    padding: 2px 6px;
    background: var(--color-accent, #007acc);
    color: white;
    border-radius: 10px;
    font-size: 10px;
    font-weight: 600;
    min-width: 18px;
    text-align: center;
  }

  .execution-status {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .status-indicator {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 4px 10px;
    background: var(--color-status-bg, #1e1e1e);
    border-radius: 12px;
    font-size: 12px;
  }

  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: var(--color-status-idle, #6e6e6e);
    transition: background 0.3s;
  }

  .status-indicator.running .status-dot {
    background: var(--color-status-running, #42c767);
    animation: pulse 1.5s infinite;
  }

  .status-indicator.paused .status-dot {
    background: var(--color-status-paused, #ffa500);
  }

  @keyframes pulse {
    0%, 100% {
      opacity: 1;
    }
    50% {
      opacity: 0.5;
    }
  }

  .status-text {
    color: var(--color-text-secondary, #969696);
  }

  .debug-info {
    padding: 4px 8px;
    background: var(--color-debug-bg, #4a4a4a);
    color: var(--color-debug-text, #ffa500);
    border-radius: 4px;
    font-size: 11px;
    font-weight: 500;
  }

  .progress-bar {
    display: flex;
    align-items: center;
    gap: 8px;
    min-width: 200px;
  }

  .progress-track {
    flex: 1;
    height: 4px;
    background: var(--color-progress-track, #3e3e42);
    border-radius: 2px;
    overflow: hidden;
  }

  .progress-fill {
    height: 100%;
    background: var(--color-accent, #007acc);
    transition: width 0.3s ease;
  }

  .progress-text {
    font-size: 11px;
    color: var(--color-text-secondary, #969696);
    min-width: 35px;
    text-align: right;
  }

  .execution-metrics {
    display: flex;
    gap: 16px;
  }

  .metric {
    display: flex;
    align-items: center;
    gap: 4px;
    font-size: 12px;
  }

  .metric-icon {
    color: var(--color-icon, #969696);
  }

  .metric-label {
    color: var(--color-text-secondary, #969696);
  }

  .metric-value {
    color: var(--color-text, #cccccc);
    font-weight: 500;
  }

  .speed-control {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .speed-label {
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
  }

  .speed-buttons {
    display: flex;
    gap: 2px;
    background: var(--color-speed-bg, #1e1e1e);
    border-radius: 4px;
    padding: 2px;
  }

  .speed-btn {
    padding: 4px 8px;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 2px;
    cursor: pointer;
    font-size: 11px;
    font-weight: 500;
    transition: all 0.2s;
  }

  .speed-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .speed-btn.active {
    background: var(--color-accent, #007acc);
    color: white;
  }

  .hint-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    padding: 0;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s;
  }

  .hint-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .execution-panel {
      flex-direction: column;
      gap: 12px;
      padding: 12px;
    }

    .panel-left,
    .panel-center,
    .panel-right {
      width: 100%;
      justify-content: center;
    }

    .btn-text {
      display: none;
    }

    .control-btn {
      padding: 8px;
    }

    .execution-metrics {
      flex-wrap: wrap;
      gap: 8px;
    }
  }
</style>