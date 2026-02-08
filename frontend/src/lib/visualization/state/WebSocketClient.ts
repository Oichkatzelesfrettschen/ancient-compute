/**
 * WebSocket Client: Real-time State Synchronization
 *
 * Handles bidirectional WebSocket communication with:
 * - Exponential backoff reconnection (max 5 attempts)
 * - Message queuing during disconnection (100 message buffer)
 * - Heartbeat monitoring (30s interval)
 * - Message sequencing and acknowledgment
 * - Connection status tracking
 */

import type {
  WebSocketMessage,
  WebSocketConfig,
  ConnectionStatus,
  WebSocketMessageType,
  StateUpdatePayload,
  StateMetrics
} from './types';
import { machineStateStore } from './MachineStateStore';

/**
 * WebSocket client implementation
 */
export class WebSocketClient {
  private ws: WebSocket | null = null;
  private url: string;
  private config: Required<WebSocketConfig>;
  private status: ConnectionStatus = 'DISCONNECTED';
  private reconnectAttempt: number = 0;
  private reconnectTimer: NodeJS.Timeout | null = null;
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private messageQueue: WebSocketMessage[] = [];
  private messageId: number = 0;
  private sequenceNumber: number = 0;
  private lastHeartbeatTime: number = Date.now();
  private messageCallbacks: Map<string, (msg: WebSocketMessage) => void> = new Map();
  private statusCallbacks: ((status: ConnectionStatus) => void)[] = [];
  private errorCallbacks: ((error: Error) => void)[] = [];

  constructor(config: WebSocketConfig) {
    this.url = config.url;
    this.config = {
      url: config.url,
      reconnectAttempts: config.reconnectAttempts ?? 5,
      initialBackoffMs: config.initialBackoffMs ?? 1000,
      maxBackoffMs: config.maxBackoffMs ?? 16000,
      heartbeatIntervalMs: config.heartbeatIntervalMs ?? 30000,
      messageQueueSize: config.messageQueueSize ?? 100
    };
  }

  /**
   * Connect to WebSocket
   */
  async connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      try {
        this.ws = new WebSocket(this.url);

        this.ws.onopen = () => {
          console.log('[WebSocket] Connected to', this.url);
          this.setStatus('CONNECTED');
          this.reconnectAttempt = 0;
          this.lastHeartbeatTime = Date.now();
          this.startHeartbeat();
          this.flushMessageQueue();
          machineStateStore.recordReconnect();
          resolve();
        };

        this.ws.onmessage = (event: MessageEvent) => {
          this.handleMessage(event.data);
        };

        this.ws.onerror = (event: Event) => {
          const error = new Error('WebSocket error');
          console.error('[WebSocket] Error:', error);
          this.notifyError(error);
          reject(error);
        };

        this.ws.onclose = () => {
          console.log('[WebSocket] Disconnected');
          this.setStatus('DISCONNECTED');
          this.stopHeartbeat();
          this.scheduleReconnect();
        };
      } catch (error) {
        reject(error);
      }
    });
  }

  /**
   * Disconnect from WebSocket
   */
  disconnect(): void {
    this.stopHeartbeat();
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.setStatus('DISCONNECTED');
  }

  /**
   * Send message
   */
  async send<T = unknown>(
    type: WebSocketMessageType,
    payload: T
  ): Promise<void> {
    const message: WebSocketMessage<T> = {
      type,
      payload,
      messageId: `msg-${++this.messageId}`,
      timestamp: Date.now(),
      sequenceNumber: ++this.sequenceNumber
    };

    if (this.status === 'CONNECTED' && this.ws) {
      try {
        this.ws.send(JSON.stringify(message));
        machineStateStore.recordMessageSent();
      } catch (error) {
        console.error('[WebSocket] Send error:', error);
        this.queueMessage(message);
      }
    } else {
      // Queue message if not connected
      this.queueMessage(message);
    }
  }

  /**
   * Queue message for later delivery
   */
  private queueMessage<T = unknown>(message: WebSocketMessage<T>): void {
    if (this.messageQueue.length < this.config.messageQueueSize) {
      this.messageQueue.push(message as WebSocketMessage);
    } else {
      console.warn('[WebSocket] Message queue full, dropping oldest message');
      this.messageQueue.shift();
      this.messageQueue.push(message as WebSocketMessage);
      machineStateStore.recordDroppedMessage();
    }
  }

  /**
   * Handle incoming message
   */
  private handleMessage(data: string): void {
    try {
      const message: WebSocketMessage = JSON.parse(data);
      const latency = Date.now() - message.timestamp;
      machineStateStore.recordMessageReceived(latency);

      // Update last heartbeat time on any message
      this.lastHeartbeatTime = Date.now();

      // Handle different message types
      switch (message.type) {
        case 'HEARTBEAT':
          this.handleHeartbeat(message);
          break;

        case 'ACKNOWLEDGE':
          // Message was acknowledged by server
          break;

        case 'STATE_UPDATE':
        case 'STATE_SNAPSHOT':
        case 'ERROR':
        case 'ANIMATION_COMPLETE':
        case 'RECONNECT_COMPLETE':
          // Call any registered callbacks for this message type
          if (this.messageCallbacks.has(message.type)) {
            const callback = this.messageCallbacks.get(message.type);
            callback?.(message);
          }
          break;
      }

      // Send acknowledgment
      this.sendAcknowledgment(message.messageId);
    } catch (error) {
      console.error('[WebSocket] Message parse error:', error);
      this.notifyError(new Error('Failed to parse WebSocket message'));
    }
  }

  /**
   * Handle heartbeat
   */
  private handleHeartbeat(message: WebSocketMessage): void {
    this.lastHeartbeatTime = Date.now();
    // Send heartbeat acknowledgment
    this.sendAcknowledgment(message.messageId);
  }

  /**
   * Send acknowledgment
   */
  private sendAcknowledgment(messageId: string): void {
    if (this.ws && this.status === 'CONNECTED') {
      const ack: WebSocketMessage = {
        type: 'ACKNOWLEDGE',
        payload: { messageId },
        messageId: `ack-${++this.messageId}`,
        timestamp: Date.now(),
        sequenceNumber: ++this.sequenceNumber
      };
      try {
        this.ws.send(JSON.stringify(ack));
      } catch (error) {
        console.error('[WebSocket] Failed to send ACK:', error);
      }
    }
  }

  /**
   * Flush queued messages
   */
  private flushMessageQueue(): void {
    const queue = [...this.messageQueue];
    this.messageQueue = [];

    for (const message of queue) {
      if (this.ws && this.status === 'CONNECTED') {
        try {
          this.ws.send(JSON.stringify(message));
          machineStateStore.recordMessageSent();
        } catch (error) {
          console.error('[WebSocket] Failed to flush message:', error);
          this.queueMessage(message);
        }
      } else {
        this.queueMessage(message);
      }
    }
  }

  /**
   * Start heartbeat monitoring
   */
  private startHeartbeat(): void {
    this.heartbeatTimer = setInterval(() => {
      if (this.status === 'CONNECTED') {
        const timeSinceLastMessage = Date.now() - this.lastHeartbeatTime;
        if (timeSinceLastMessage > this.config.heartbeatIntervalMs) {
          // No message received in a while, send heartbeat
          const heartbeat: WebSocketMessage = {
            type: 'HEARTBEAT',
            payload: { timestamp: Date.now() },
            messageId: `heartbeat-${++this.messageId}`,
            timestamp: Date.now(),
            sequenceNumber: ++this.sequenceNumber
          };
          try {
            this.ws?.send(JSON.stringify(heartbeat));
          } catch (error) {
            console.error('[WebSocket] Failed to send heartbeat:', error);
          }
        }
      }
    }, this.config.heartbeatIntervalMs / 2); // Check twice per interval
  }

  /**
   * Stop heartbeat monitoring
   */
  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  /**
   * Schedule reconnect with exponential backoff
   */
  private scheduleReconnect(): void {
    if (this.reconnectAttempt >= this.config.reconnectAttempts) {
      console.error(
        '[WebSocket] Max reconnection attempts reached',
        this.reconnectAttempt
      );
      this.setStatus('ERROR');
      this.notifyError(
        new Error('Failed to reconnect after ' + this.reconnectAttempt + ' attempts')
      );
      return;
    }

    const backoffMs = Math.min(
      this.config.initialBackoffMs * Math.pow(2, this.reconnectAttempt),
      this.config.maxBackoffMs
    );

    console.log(
      `[WebSocket] Reconnecting in ${backoffMs}ms (attempt ${this.reconnectAttempt + 1}/${this.config.reconnectAttempts})`
    );

    this.setStatus('RECONNECTING');
    this.reconnectTimer = setTimeout(() => {
      this.reconnectAttempt += 1;
      this.connect().catch((error) => {
        console.error('[WebSocket] Reconnect failed:', error);
        this.scheduleReconnect();
      });
    }, backoffMs);
  }

  /**
   * Register callback for specific message type
   */
  onMessage(
    type: WebSocketMessageType,
    callback: (message: WebSocketMessage) => void
  ): () => void {
    this.messageCallbacks.set(type, callback);
    // Return unsubscribe function
    return () => {
      this.messageCallbacks.delete(type);
    };
  }

  /**
   * Register status change callback
   */
  onStatusChange(callback: (status: ConnectionStatus) => void): () => void {
    this.statusCallbacks.push(callback);
    // Return unsubscribe function
    return () => {
      const index = this.statusCallbacks.indexOf(callback);
      if (index >= 0) {
        this.statusCallbacks.splice(index, 1);
      }
    };
  }

  /**
   * Register error callback
   */
  onError(callback: (error: Error) => void): () => void {
    this.errorCallbacks.push(callback);
    return () => {
      const index = this.errorCallbacks.indexOf(callback);
      if (index >= 0) {
        this.errorCallbacks.splice(index, 1);
      }
    };
  }

  /**
   * Set status and notify
   */
  private setStatus(status: ConnectionStatus): void {
    if (this.status !== status) {
      this.status = status;
      this.notifyStatusChange(status);
    }
  }

  /**
   * Notify status change
   */
  private notifyStatusChange(status: ConnectionStatus): void {
    for (const callback of this.statusCallbacks) {
      try {
        callback(status);
      } catch (error) {
        console.error('[WebSocket] Status callback error:', error);
      }
    }
  }

  /**
   * Notify error
   */
  private notifyError(error: Error): void {
    for (const callback of this.errorCallbacks) {
      try {
        callback(error);
      } catch (error) {
        console.error('[WebSocket] Error callback error:', error);
      }
    }
  }

  /**
   * Get connection status
   */
  getStatus(): ConnectionStatus {
    return this.status;
  }

  /**
   * Is connected
   */
  isConnected(): boolean {
    return this.status === 'CONNECTED';
  }

  /**
   * Get message queue length
   */
  getQueueLength(): number {
    return this.messageQueue.length;
  }

  /**
   * Get metrics
   */
  getMetrics(): Partial<StateMetrics> {
    return {
      messageQueueLength: this.messageQueue.length
    };
  }
}

/**
 * Global WebSocket client instance
 */
let globalClient: WebSocketClient | null = null;

/**
 * Get or create global WebSocket client
 */
export function getWebSocketClient(config?: WebSocketConfig): WebSocketClient {
  if (!globalClient && config) {
    globalClient = new WebSocketClient(config);
  }
  return globalClient!;
}

/**
 * Create new WebSocket client
 */
export function createWebSocketClient(config: WebSocketConfig): WebSocketClient {
  return new WebSocketClient(config);
}
