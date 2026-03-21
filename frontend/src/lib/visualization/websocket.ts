/**
 * WebSocket Integration Layer for State Synchronization
 *
 * Bridges WebSocketClient and machineStateStore, handling:
 * - Connection lifecycle management
 * - STATE_UPDATE message dispatch to Svelte store
 * - Connection status tracking
 * - Error handling and recovery
 * - Message acknowledgment
 *
 * Pattern: Functional integration with cleanup support
 */

import { get } from 'svelte/store';
import { WebSocketClient, createWebSocketClient } from './state/WebSocketClient';
import { machineStateStore } from './state/MachineStateStore';
import type {
  StateUpdatePayload,
  WebSocketMessage,
  WebSocketMessageType,
  ConnectionStatus as WsConnectionStatus
} from './state/types';

/**
 * Connection status tracking
 */
export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

interface WebSocketConnection {
  client: WebSocketClient;
  status: ConnectionStatus;
  errorMessage?: string;
  unsubscribers: Array<() => void>;
  isInitialized: boolean;
}

// Global connection instance
let connection: WebSocketConnection | null = null;

/**
 * Initialize WebSocket connection and state synchronization
 *
 * @param backendUrl - WebSocket server URL (e.g., ws://localhost:8000)
 * @returns Cleanup function to disconnect and unsubscribe
 */
export function initializeWebSocket(backendUrl: string): () => void {
  // Cleanup existing connection
  if (connection) {
    disconnectWebSocket();
  }

  connection = {
    client: createWebSocketClient({ url: backendUrl }),
    status: 'connecting',
    unsubscribers: [],
    isInitialized: false,
  };

  const conn = connection;
  const client = conn.client;

  /**
   * Handler for STATE_UPDATE messages from backend
   * Dispatches state changes to machineStateStore
   */
  const handleStateUpdate = (message: WebSocketMessage) => {
    const payload = message.payload as StateUpdatePayload;
    const state = payload.fullState;
    if (!state) return;

    // Update store with new state
    machineStateStore.setState(state);
  };

  /**
   * Handler for complete state snapshots
   * Used when recovering from connection loss
   */
  const handleStateSnapshot = (message: WebSocketMessage) => {
    const payload = message.payload as StateUpdatePayload;
    const state = payload.fullState;
    if (!state) return;
    machineStateStore.setState(state);
  };

  /**
   * Handler for animation completion notifications
   * Useful for coordinating multi-step animations
   */
  const handleAnimationComplete = (message: WebSocketMessage) => {
    const payload = message.payload as Record<string, unknown>;
    // Log for debugging; UI updates via store subscription
    console.debug('[WebSocket] Animation complete:', {
      timestamp: payload['timestamp'],
      animationId: payload['animationId'],
    });
  };

  /**
   * Connection status change handler
   */
  const handleStatusChange = (wsStatus: WsConnectionStatus) => {
    const statusMap: Record<WsConnectionStatus, ConnectionStatus> = {
      DISCONNECTED: 'disconnected',
      CONNECTING: 'connecting',
      CONNECTED: 'connected',
      RECONNECTING: 'connecting',
      ERROR: 'error'
    };
    conn.status = statusMap[wsStatus];
    conn.errorMessage = undefined;

    console.log('[WebSocket] Connection status:', wsStatus);

    if (wsStatus === 'CONNECTED') {
      // Request full state snapshot on reconnection
      client.send('STATE_SNAPSHOT', {});
    }
  };

  /**
   * Error handler
   */
  const handleError = (error: Error | string) => {
    const message = typeof error === 'string' ? error : error.message;
    conn.status = 'error';
    conn.errorMessage = message;
    console.error('[WebSocket] Connection error:', message);
  };

  // Register message handlers
  conn.unsubscribers.push(client.onMessage('STATE_UPDATE', handleStateUpdate));
  conn.unsubscribers.push(client.onMessage('STATE_SNAPSHOT', handleStateSnapshot));
  conn.unsubscribers.push(client.onMessage('ANIMATION_COMPLETE', handleAnimationComplete));
  conn.unsubscribers.push(client.onStatusChange(handleStatusChange));
  conn.unsubscribers.push(client.onError(handleError));

  // Establish connection
  client.connect();
  conn.isInitialized = true;

  console.log('[WebSocket] Initialized:', backendUrl);

  // Return cleanup function
  return () => disconnectWebSocket();
}

/**
 * Disconnect WebSocket and cleanup
 */
export function disconnectWebSocket(): void {
  if (!connection) return;

  // Unsubscribe all handlers
  connection.unsubscribers.forEach((unsub) => unsub());
  connection.unsubscribers = [];

  // Disconnect client
  connection.client.disconnect();

  connection = null;
  console.log('[WebSocket] Disconnected');
}

/**
 * Get current connection status
 */
export function getConnectionStatus(): ConnectionStatus {
  return connection?.status ?? 'disconnected';
}

/**
 * Get connection error message (if any)
 */
export function getConnectionError(): string | undefined {
  return connection?.errorMessage;
}

/**
 * Check if connection is established
 */
export function isConnected(): boolean {
  return getConnectionStatus() === 'connected';
}

/**
 * Get WebSocket client instance (if initialized)
 */
export function getWebSocketClient(): WebSocketClient | null {
  return connection?.client ?? null;
}

/**
 * Manual reconnect attempt
 */
export function reconnect(): void {
  const client = getWebSocketClient();
  if (client) {
    console.log('[WebSocket] Manual reconnect requested');
    client.connect();
  }
}

/**
 * Send custom message to backend
 * Useful for commands, queries, debug requests
 */
export function sendMessage(type: string, payload: unknown): void {
  const client = getWebSocketClient();
  if (client && isConnected()) {
    client.send(type as WebSocketMessageType, payload);
  } else {
    console.warn('[WebSocket] Cannot send message: not connected');
  }
}

/**
 * Request state snapshot from backend
 * Useful for manual state sync
 */
export function requestSnapshot(): void {
  sendMessage('REQUEST_SNAPSHOT', {});
}

/**
 * Get message queue length
 * Indicates how many messages are buffered during disconnection
 */
export function getQueueLength(): number {
  const client = getWebSocketClient();
  return client?.getQueueLength() ?? 0;
}

/**
 * Observable store for connection status (for reactive UI)
 * Usage: import { connectionStatusStore } from './websocket'
 *        {#if $connectionStatusStore === 'connected'} ... {/if}
 */
import { writable } from 'svelte/store';

export const connectionStatusStore = writable<ConnectionStatus>('disconnected');
export const connectionErrorStore = writable<string | undefined>(undefined);

/**
 * Internal: Update status stores when connection status changes
 * Called by initialization and status change handlers
 */
export function updateStatusStores(): void {
  connectionStatusStore.set(getConnectionStatus());
  connectionErrorStore.set(getConnectionError());
}

// Setup periodic status store updates (every 500ms)
if (typeof window !== 'undefined') {
  setInterval(() => {
    if (connection?.isInitialized) {
      updateStatusStores();
    }
  }, 500);
}
