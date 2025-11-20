/**
 * WebSocket Client for Emulator Backend Communication
 * 
 * Handles low-level WebSocket communication with automatic reconnection,
 * message queueing, and error recovery.
 */

import type {
  WebSocketMessage,
  MessageType,
  ConnectionStatus,
  WebSocketError
} from './types';

export interface WebSocketClientConfig {
  url: string;
  reconnectInterval?: number;
  maxReconnectAttempts?: number;
  heartbeatInterval?: number;
  messageQueueSize?: number;
}

type MessageHandler = (payload: any) => void;
type StatusHandler = (status: ConnectionStatus) => void;
type ErrorHandler = (error: WebSocketError | Error | string) => void;

/**
 * WebSocket Client with automatic reconnection and message handling
 */
export class WebSocketClient {
  private ws: WebSocket | null = null;
  private config: Required<WebSocketClientConfig>;
  private messageHandlers: Map<MessageType, Set<MessageHandler>> = new Map();
  private statusHandlers: Set<StatusHandler> = new Set();
  private errorHandlers: Set<ErrorHandler> = new Set();
  private messageQueue: WebSocketMessage[] = [];
  private reconnectAttempts = 0;
  private reconnectTimer: number | null = null;
  private heartbeatTimer: number | null = null;
  private currentStatus: ConnectionStatus = 'disconnected';
  private lastSequenceNumber = 0;

  constructor(config: WebSocketClientConfig) {
    this.config = {
      url: config.url,
      reconnectInterval: config.reconnectInterval ?? 3000,
      maxReconnectAttempts: config.maxReconnectAttempts ?? 10,
      heartbeatInterval: config.heartbeatInterval ?? 30000,
      messageQueueSize: config.messageQueueSize ?? 100
    };
  }

  /**
   * Establish WebSocket connection
   */
  public connect(): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      console.warn('[WebSocketClient] Already connected');
      return;
    }

    this.setStatus('connecting');

    try {
      this.ws = new WebSocket(this.config.url);

      this.ws.onopen = () => {
        console.log('[WebSocketClient] Connected to', this.config.url);
        this.setStatus('connected');
        this.reconnectAttempts = 0;
        this.startHeartbeat();
        this.flushMessageQueue();
      };

      this.ws.onmessage = (event) => {
        try {
          const message: WebSocketMessage = JSON.parse(event.data);
          this.handleMessage(message);
        } catch (error) {
          console.error('[WebSocketClient] Failed to parse message:', error);
          this.emitError(error as Error);
        }
      };

      this.ws.onerror = (event) => {
        console.error('[WebSocketClient] WebSocket error:', event);
        this.emitError('WebSocket connection error');
      };

      this.ws.onclose = (event) => {
        console.log('[WebSocketClient] Disconnected:', event.code, event.reason);
        this.stopHeartbeat();
        this.setStatus('disconnected');
        
        if (!event.wasClean && this.reconnectAttempts < this.config.maxReconnectAttempts) {
          this.scheduleReconnect();
        }
      };

    } catch (error) {
      console.error('[WebSocketClient] Failed to create WebSocket:', error);
      this.setStatus('error');
      this.emitError(error as Error);
    }
  }

  /**
   * Disconnect WebSocket
   */
  public disconnect(): void {
    this.stopHeartbeat();
    
    if (this.reconnectTimer !== null) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    if (this.ws) {
      this.ws.close(1000, 'Client disconnect');
      this.ws = null;
    }

    this.setStatus('disconnected');
  }

  /**
   * Send message to server
   */
  public send(type: MessageType, payload: any): void {
    const message: WebSocketMessage = {
      type,
      payload,
      timestamp: Date.now(),
      sequenceNumber: ++this.lastSequenceNumber
    };

    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      try {
        this.ws.send(JSON.stringify(message));
      } catch (error) {
        console.error('[WebSocketClient] Failed to send message:', error);
        this.queueMessage(message);
      }
    } else {
      this.queueMessage(message);
    }
  }

  /**
   * Acknowledge received message
   */
  public acknowledge(sequenceNumber: number): void {
    this.send('ANIMATION_COMPLETE', { sequenceNumber });
  }

  /**
   * Register message handler for specific message type
   */
  public onMessage(type: MessageType, handler: MessageHandler): () => void {
    if (!this.messageHandlers.has(type)) {
      this.messageHandlers.set(type, new Set());
    }
    this.messageHandlers.get(type)!.add(handler);

    // Return unsubscribe function
    return () => {
      this.messageHandlers.get(type)?.delete(handler);
    };
  }

  /**
   * Register status change handler
   */
  public onStatusChange(handler: StatusHandler): () => void {
    this.statusHandlers.add(handler);
    
    // Return unsubscribe function
    return () => {
      this.statusHandlers.delete(handler);
    };
  }

  /**
   * Register error handler
   */
  public onError(handler: ErrorHandler): () => void {
    this.errorHandlers.add(handler);
    
    // Return unsubscribe function
    return () => {
      this.errorHandlers.delete(handler);
    };
  }

  /**
   * Get current connection status
   */
  public getStatus(): ConnectionStatus {
    return this.currentStatus;
  }

  /**
   * Get message queue length
   */
  public getQueueLength(): number {
    return this.messageQueue.length;
  }

  /**
   * Handle incoming message
   */
  private handleMessage(message: WebSocketMessage): void {
    const handlers = this.messageHandlers.get(message.type);
    if (handlers) {
      handlers.forEach(handler => {
        try {
          handler(message.payload);
        } catch (error) {
          console.error('[WebSocketClient] Handler error for', message.type, error);
        }
      });
    }
  }

  /**
   * Set connection status and notify handlers
   */
  private setStatus(status: ConnectionStatus): void {
    if (this.currentStatus !== status) {
      this.currentStatus = status;
      this.statusHandlers.forEach(handler => {
        try {
          handler(status);
        } catch (error) {
          console.error('[WebSocketClient] Status handler error:', error);
        }
      });
    }
  }

  /**
   * Emit error to error handlers
   */
  private emitError(error: WebSocketError | Error | string): void {
    this.errorHandlers.forEach(handler => {
      try {
        handler(error);
      } catch (err) {
        console.error('[WebSocketClient] Error handler error:', err);
      }
    });
  }

  /**
   * Queue message for later sending
   */
  private queueMessage(message: WebSocketMessage): void {
    if (this.messageQueue.length >= this.config.messageQueueSize) {
      // Remove oldest message
      this.messageQueue.shift();
    }
    this.messageQueue.push(message);
  }

  /**
   * Flush queued messages
   */
  private flushMessageQueue(): void {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      return;
    }

    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift()!;
      try {
        this.ws.send(JSON.stringify(message));
      } catch (error) {
        console.error('[WebSocketClient] Failed to flush message:', error);
        // Re-queue if send fails
        this.messageQueue.unshift(message);
        break;
      }
    }
  }

  /**
   * Schedule reconnection attempt
   */
  private scheduleReconnect(): void {
    if (this.reconnectTimer !== null) {
      return;
    }

    this.reconnectAttempts++;
    const delay = this.config.reconnectInterval * Math.min(this.reconnectAttempts, 5);

    console.log(`[WebSocketClient] Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts}/${this.config.maxReconnectAttempts})`);

    this.reconnectTimer = window.setTimeout(() => {
      this.reconnectTimer = null;
      this.connect();
    }, delay);
  }

  /**
   * Start heartbeat to keep connection alive
   */
  private startHeartbeat(): void {
    this.stopHeartbeat();

    this.heartbeatTimer = window.setInterval(() => {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        this.send('REQUEST_SNAPSHOT', { heartbeat: true });
      }
    }, this.config.heartbeatInterval);
  }

  /**
   * Stop heartbeat timer
   */
  private stopHeartbeat(): void {
    if (this.heartbeatTimer !== null) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }
}

/**
 * Factory function for creating WebSocket client
 */
export function createWebSocketClient(url: string, config?: Partial<WebSocketClientConfig>): WebSocketClient {
  return new WebSocketClient({ url, ...config });
}
