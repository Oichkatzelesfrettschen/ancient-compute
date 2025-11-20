/**
 * test-utils.ts
 * Common utilities and helpers for frontend tests
 */

import type { MachineState } from '$lib/visualization/providers/VisualizationProvider';

/**
 * Create a default test machine state
 */
export function createTestMachineState(overrides?: Partial<MachineState>): MachineState {
  const defaultState: MachineState = {
    registers: new Map([
      ['R0', 0],
      ['R1', 0],
      ['R2', 0],
      ['R3', 0],
    ]),
    memory: new Uint8Array(4096),
    programCounter: 0,
    accumulator: 0,
    carryFlag: false,
    runningFlag: false,
    columns: Array(10)
      .fill(null)
      .map((_, i) => ({
        id: `column_${i}`,
        value: 0,
        isActive: false,
        rotation: 0,
      })),
    store: {
      positions: Array(32).fill(0),
      capacity: 32,
      activeIndices: [],
    },
    mill: {
      operation: 'idle',
      operandA: 0,
      operandB: 0,
      result: 0,
      progress: 0,
    },
    barrels: Array(3)
      .fill(null)
      .map((_, i) => ({
        id: `barrel_${i}`,
        programCounter: 0,
        instructions: [],
        isActive: false,
      })),
    cycles: 0,
    timestamp: Date.now(),
  };

  return { ...defaultState, ...overrides };
}

/**
 * Mock WebSocket class for testing
 */
export class MockWebSocket {
  public onopen: ((event: Event) => void) | null = null;
  public onmessage: ((event: MessageEvent) => void) | null = null;
  public onerror: ((event: Event) => void) | null = null;
  public onclose: ((event: CloseEvent) => void) | null = null;
  public readyState: number = WebSocket.CONNECTING;

  constructor(public url: string) {
    setTimeout(() => {
      this.readyState = WebSocket.OPEN;
      if (this.onopen) {
        this.onopen(new Event('open'));
      }
    }, 10);
  }

  send(data: string) {
    // Mock send - can be extended for testing
  }

  close() {
    this.readyState = WebSocket.CLOSED;
    if (this.onclose) {
      this.onclose(new CloseEvent('close'));
    }
  }

  simulateMessage(data: any) {
    if (this.onmessage) {
      this.onmessage(new MessageEvent('message', { data: JSON.stringify(data) }));
    }
  }

  simulateError() {
    if (this.onerror) {
      this.onerror(new Event('error'));
    }
  }
}

/**
 * Mock Canvas 2D context for testing
 */
export function createMockCanvas2DContext() {
  return {
    fillStyle: '',
    fillRect: vi.fn(),
    clearRect: vi.fn(),
    getImageData: vi.fn(() => ({
      data: new Uint8ClampedArray(4),
    })),
    putImageData: vi.fn(),
    createImageData: vi.fn(() => ({
      data: new Uint8ClampedArray(4),
    })),
    setTransform: vi.fn(),
    drawImage: vi.fn(),
    save: vi.fn(),
    restore: vi.fn(),
    beginPath: vi.fn(),
    moveTo: vi.fn(),
    lineTo: vi.fn(),
    closePath: vi.fn(),
    stroke: vi.fn(),
    translate: vi.fn(),
    scale: vi.fn(),
    rotate: vi.fn(),
    arc: vi.fn(),
    fill: vi.fn(),
    measureText: vi.fn(() => ({ width: 0 })),
    transform: vi.fn(),
    rect: vi.fn(),
    clip: vi.fn(),
    canvas: {
      width: 800,
      height: 600,
      toDataURL: vi.fn(() => 'data:image/png;base64,'),
      toBlob: vi.fn((callback: (blob: Blob) => void) => {
        callback(new Blob([''], { type: 'image/png' }));
      }),
    },
  };
}

/**
 * Wait for a condition to be true
 */
export async function waitForCondition(
  condition: () => boolean,
  timeout = 5000,
  interval = 100
): Promise<void> {
  const startTime = Date.now();

  while (!condition()) {
    if (Date.now() - startTime > timeout) {
      throw new Error('Timeout waiting for condition');
    }
    await new Promise((resolve) => setTimeout(resolve, interval));
  }
}

/**
 * Create a mock performance metrics object
 */
export function createMockPerformanceMetrics() {
  return {
    fps: 60,
    frameTime: 16.67,
    drawCalls: 42,
    triangles: 1024,
    programs: 2,
    geometries: 15,
    textures: 8,
    memory: {
      geometries: 1024 * 1024 * 2,
      textures: 1024 * 1024 * 4,
    },
  };
}

/**
 * Simulate animation frames
 */
export async function simulateAnimationFrames(count: number, frameTime = 16): Promise<void> {
  for (let i = 0; i < count; i++) {
    await new Promise((resolve) => setTimeout(resolve, frameTime));
  }
}

/**
 * Create mock device capabilities
 */
export function createMockDeviceCapabilities() {
  return {
    gpu: 'Mock GPU',
    tier: 2,
    webgl2: true,
    isMobile: false,
    maxTextureSize: 4096,
    maxAnisotropy: 16,
  };
}
