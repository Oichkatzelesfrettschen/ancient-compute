import { playgroundStore } from '$lib/stores/playgroundStore';

// API configuration
const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000';
const WS_BASE_URL = import.meta.env.VITE_WS_URL || 'ws://localhost:8000';

// WebSocket connection state
let ws: WebSocket | null = null;
let reconnectAttempts = 0;
const maxReconnectAttempts = 5;
const reconnectDelay = 1000; // Start with 1 second

// Execution request queue
const executionQueue: ExecutionRequest[] = [];
let currentExecutionId: string | null = null;

export interface ExecutionRequest {
  id: string;
  code: string;
  language: string;
  input?: string;
  timeout?: number;
  memoryLimit?: number;
}

export interface ExecutionResponse {
  id: string;
  type: 'stdout' | 'stderr' | 'compilation' | 'ir' | 'assembly' | 'complete' | 'error';
  data: string;
  timestamp: number;
  metadata?: {
    executionTime?: number;
    memoryUsage?: number;
    exitCode?: number;
    lineNumber?: number;
    columnNumber?: number;
  };
}

export interface LanguageCapabilities {
  id: string;
  name: string;
  version: string;
  features: string[];
  compilationSupported: boolean;
  debuggingSupported: boolean;
  irGeneration: boolean;
  assemblyGeneration: boolean;
}

// Initialize WebSocket connection
export function initWebSocket(): Promise<void> {
  return new Promise((resolve, reject) => {
    if (ws && ws.readyState === WebSocket.OPEN) {
      resolve();
      return;
    }

    const wsUrl = `${WS_BASE_URL}/ws/execute`;
    ws = new WebSocket(wsUrl);

    ws.onopen = () => {
      console.log('WebSocket connected');
      reconnectAttempts = 0;
      resolve();

      // Process any queued executions
      processExecutionQueue();
    };

    ws.onmessage = (event) => {
      try {
        const response: ExecutionResponse = JSON.parse(event.data);
        handleExecutionResponse(response);
      } catch (error) {
        console.error('Failed to parse WebSocket message:', error);
      }
    };

    ws.onerror = (error) => {
      console.error('WebSocket error:', error);
      reject(error);
    };

    ws.onclose = (event) => {
      console.log('WebSocket closed:', event.code, event.reason);
      ws = null;

      // Attempt to reconnect if not a normal closure
      if (event.code !== 1000 && reconnectAttempts < maxReconnectAttempts) {
        setTimeout(() => {
          reconnectAttempts++;
          console.log(`Reconnecting... (attempt ${reconnectAttempts})`);
          initWebSocket();
        }, reconnectDelay * Math.pow(2, reconnectAttempts));
      }
    };
  });
}

// Handle execution response
function handleExecutionResponse(response: ExecutionResponse) {
  if (response.id !== currentExecutionId) {
    // Ignore responses from old executions
    return;
  }

  switch (response.type) {
    case 'stdout':
      playgroundStore.appendStdout(response.data);
      break;

    case 'stderr':
      playgroundStore.appendStderr(response.data);
      break;

    case 'compilation':
      playgroundStore.updateOutput({ compilationLog: response.data });
      break;

    case 'ir':
      playgroundStore.updateOutput({ ir: response.data });
      break;

    case 'assembly':
      playgroundStore.updateOutput({ assembly: response.data });
      break;

    case 'complete':
      const { executionTime, memoryUsage, exitCode } = response.metadata || {};
      playgroundStore.updateOutput({ exitCode, memoryUsage });
      playgroundStore.setExecutionTime(executionTime || 0);
      playgroundStore.stopExecution();

      // Add to history
      playgroundStore.addToHistory({
        code: getCurrentCode(),
        language: getCurrentLanguage(),
        output: getCurrentOutput(),
        executionTime: executionTime || 0,
        success: exitCode === 0,
      });

      currentExecutionId = null;
      processExecutionQueue();
      break;

    case 'error':
      playgroundStore.appendStderr(`Error: ${response.data}`);
      playgroundStore.stopExecution();
      currentExecutionId = null;
      processExecutionQueue();
      break;
  }
}

// Process execution queue
function processExecutionQueue() {
  if (currentExecutionId || executionQueue.length === 0) {
    return;
  }

  const request = executionQueue.shift();
  if (!request) return;

  currentExecutionId = request.id;
  sendExecutionRequest(request);
}

// Send execution request via WebSocket
function sendExecutionRequest(request: ExecutionRequest) {
  if (!ws || ws.readyState !== WebSocket.OPEN) {
    console.error('WebSocket not connected');
    playgroundStore.appendStderr('Connection lost. Please try again.');
    playgroundStore.stopExecution();
    currentExecutionId = null;
    return;
  }

  ws.send(JSON.stringify({
    type: 'execute',
    ...request,
  }));
}

// Execute code (main API)
export async function executeCode(
  code: string,
  language: string,
  input?: string,
  options?: {
    timeout?: number;
    memoryLimit?: number;
  }
): Promise<void> {
  // Initialize WebSocket if needed
  if (!ws || ws.readyState !== WebSocket.OPEN) {
    try {
      await initWebSocket();
    } catch (error) {
      playgroundStore.appendStderr('Failed to connect to execution server.');
      playgroundStore.stopExecution();
      return;
    }
  }

  // Clear previous output and start execution
  playgroundStore.startExecution();

  // Create execution request
  const request: ExecutionRequest = {
    id: crypto.randomUUID(),
    code,
    language,
    input,
    timeout: options?.timeout || 30000, // 30 seconds default
    memoryLimit: options?.memoryLimit || 128 * 1024 * 1024, // 128MB default
  };

  // Add to queue or execute immediately
  if (currentExecutionId) {
    executionQueue.push(request);
  } else {
    currentExecutionId = request.id;
    sendExecutionRequest(request);
  }
}

// Stop execution
export function stopExecution() {
  if (!ws || ws.readyState !== WebSocket.OPEN) {
    return;
  }

  if (currentExecutionId) {
    ws.send(JSON.stringify({
      type: 'stop',
      id: currentExecutionId,
    }));
    currentExecutionId = null;
    playgroundStore.stopExecution();
  }

  // Clear the queue
  executionQueue.length = 0;
}

// REST API fallback for non-streaming execution
export async function executeCodeREST(
  code: string,
  language: string,
  input?: string
): Promise<void> {
  playgroundStore.startExecution();

  try {
    const response = await fetch(`${API_BASE_URL}/api/v1/execute/run`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({
        code,
        language,
        input_data: input,
      }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.detail || 'Execution failed');
    }

    const result = await response.json();

    playgroundStore.updateOutput({
      stdout: result.stdout || '',
      stderr: result.stderr || '',
      compilationLog: result.compilation_log || '',
      ir: result.ir_text || '',
      assembly: result.assembly_text || '',
      exitCode: result.exit_code || 0,
    });

    playgroundStore.setExecutionTime(result.compilation_time || 0);

  } catch (error) {
    playgroundStore.appendStderr(`Execution error: ${error}`);
  } finally {
    playgroundStore.stopExecution();
  }
}

// Fetch available languages and their capabilities
export async function fetchLanguages(): Promise<LanguageCapabilities[]> {
  try {
    const response = await fetch(`${API_BASE_URL}/api/v1/execute/languages`);
    if (!response.ok) {
      throw new Error('Failed to fetch languages');
    }
    return await response.json();
  } catch (error) {
    console.error('Error fetching languages:', error);
    return [];
  }
}

// Check execution service health
export async function checkHealth(): Promise<boolean> {
  try {
    const response = await fetch(`${API_BASE_URL}/api/v1/execute/health`);
    return response.ok;
  } catch (error) {
    console.error('Health check failed:', error);
    return false;
  }
}

// Helper functions to get current state
function getCurrentCode(): string {
  let code = '';
  const unsubscribe = playgroundStore.subscribe(state => {
    code = state.code;
  });
  unsubscribe();
  return code;
}

function getCurrentLanguage(): string {
  let language = '';
  const unsubscribe = playgroundStore.subscribe(state => {
    language = state.language;
  });
  unsubscribe();
  return language;
}

function getCurrentOutput() {
  let output: any = {};
  const unsubscribe = playgroundStore.subscribe(state => {
    output = state.output;
  });
  unsubscribe();
  return output;
}

// Auto-initialize WebSocket on module load
if (typeof window !== 'undefined') {
  // Initialize with a delay to ensure DOM is ready
  setTimeout(() => {
    initWebSocket().catch(error => {
      console.error('Initial WebSocket connection failed:', error);
    });
  }, 100);
}

// Clean up on page unload
if (typeof window !== 'undefined') {
  window.addEventListener('beforeunload', () => {
    if (ws) {
      ws.close(1000, 'Page unload');
    }
  });
}