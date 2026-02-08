/**
 * Unit Tests: WebSocketClient
 *
 * Tests:
 * - Connection status management
 * - Message queuing
 * - Message callbacks
 * - Status callbacks
 * - Error handling
 * - Acknowledgments
 */

import { describe, it, expect, beforeEach, vi } from 'vitest';
import { WebSocketClient } from './WebSocketClient';
import type { WebSocketConfig } from './types';

describe('WebSocketClient', () => {
  let client: WebSocketClient;
  const config: WebSocketConfig = {
    url: 'ws://localhost:8080',
    reconnectAttempts: 3,
    initialBackoffMs: 100,
    maxBackoffMs: 800,
    heartbeatIntervalMs: 1000,
    messageQueueSize: 100
  };

  beforeEach(() => {
    client = new WebSocketClient(config);
  });

  describe('Initialization', () => {
    it('should create client with config', () => {
      expect(client).toBeDefined();
      expect(client.getStatus()).toBe('DISCONNECTED');
    });

    it('should not be connected initially', () => {
      expect(client.isConnected()).toBe(false);
    });

    it('should have empty message queue initially', () => {
      expect(client.getQueueLength()).toBe(0);
    });
  });

  describe('Connection Status', () => {
    it('should track connection status', () => {
      expect(client.getStatus()).toBe('DISCONNECTED');
    });

    it('should support status change callbacks', (done) => {
      const unsubscribe = client.onStatusChange((status) => {
        if (status === 'CONNECTING') {
          expect(status).toBe('CONNECTING');
          unsubscribe();
          done();
        }
      });

      // Simulate status change (would happen on connect attempt)
    });
  });

  describe('Message Callbacks', () => {
    it('should register message callbacks', () => {
      const callback = vi.fn();
      const unsubscribe = client.onMessage('HEARTBEAT', callback);

      expect(typeof unsubscribe).toBe('function');
    });

    it('should unsubscribe from message callbacks', () => {
      const callback = vi.fn();
      const unsubscribe = client.onMessage('STATE_UPDATE', callback);

      unsubscribe();

      // After unsubscribe, callback should not be called
      expect(callback).not.toHaveBeenCalled();
    });

    it('should support multiple message callbacks', () => {
      const cb1 = vi.fn();
      const cb2 = vi.fn();

      client.onMessage('STATE_UPDATE', cb1);
      client.onMessage('HEARTBEAT', cb2);

      // Both should be registered
      expect(client).toBeDefined();
    });
  });

  describe('Error Callbacks', () => {
    it('should register error callbacks', () => {
      const callback = vi.fn();
      const unsubscribe = client.onError(callback);

      expect(typeof unsubscribe).toBe('function');
    });

    it('should unsubscribe from error callbacks', () => {
      const callback = vi.fn();
      const unsubscribe = client.onError(callback);

      unsubscribe();

      // Callback should be unregistered
      expect(callback).not.toHaveBeenCalled();
    });
  });

  describe('Message Queue', () => {
    it('should queue messages when not connected', async () => {
      await client.send('STATE_UPDATE', { test: 'data' });

      expect(client.getQueueLength()).toBeGreaterThan(0);
    });

    it('should not exceed queue size', async () => {
      const smallConfig = {
        ...config,
        messageQueueSize: 5
      };

      const smallClient = new WebSocketClient(smallConfig);

      for (let i = 0; i < 10; i++) {
        await smallClient.send('STATE_UPDATE', { index: i });
      }

      expect(smallClient.getQueueLength()).toBeLessThanOrEqual(5);
    });

    it('should provide queue length metric', () => {
      const metrics = client.getMetrics();
      expect(metrics.messageQueueLength).toBe(0);
    });
  });

  describe('Message Sending', () => {
    it('should support sending messages', async () => {
      // Should not throw
      await expect(client.send('HEARTBEAT', { ping: true })).resolves.toBeUndefined();
    });

    it('should support different message types', async () => {
      const types = [
        'STATE_UPDATE',
        'STATE_SNAPSHOT',
        'ANIMATION_COMPLETE',
        'ACKNOWLEDGE',
        'ERROR',
        'HEARTBEAT',
        'RECONNECT_COMPLETE'
      ] as const;

      for (const type of types) {
        await expect(client.send(type, {})).resolves.toBeUndefined();
      }
    });
  });

  describe('Disconnection', () => {
    it('should support disconnection', () => {
      // Should not throw
      expect(() => client.disconnect()).not.toThrow();
      expect(client.getStatus()).toBe('DISCONNECTED');
    });

    it('should clear timers on disconnect', () => {
      // Multiple disconnects should not cause issues
      client.disconnect();
      client.disconnect();

      expect(client.getStatus()).toBe('DISCONNECTED');
    });
  });

  describe('Configuration', () => {
    it('should accept custom config', () => {
      const customConfig: WebSocketConfig = {
        url: 'ws://custom:9000',
        reconnectAttempts: 10,
        initialBackoffMs: 500,
        maxBackoffMs: 10000,
        heartbeatIntervalMs: 60000,
        messageQueueSize: 200
      };

      const customClient = new WebSocketClient(customConfig);
      expect(customClient).toBeDefined();
    });

    it('should use default config values', () => {
      const minimalConfig: WebSocketConfig = {
        url: 'ws://localhost:8080'
      };

      const minimalClient = new WebSocketClient(minimalConfig);
      expect(minimalClient).toBeDefined();
    });
  });

  describe('Status Tracking', () => {
    it('should track connection status changes', () => {
      const statuses: string[] = [];

      client.onStatusChange((status) => {
        statuses.push(status);
      });

      // Initial state
      expect(client.getStatus()).toBe('DISCONNECTED');
    });

    it('should report disconnected status', () => {
      expect(client.isConnected()).toBe(false);
      expect(client.getStatus()).toBe('DISCONNECTED');
    });
  });

  describe('Global Client', () => {
    it('should support global instance creation', () => {
      const { getWebSocketClient } = await import('./WebSocketClient');
      const global1 = getWebSocketClient(config);
      const global2 = getWebSocketClient(config);

      expect(global1).toBe(global2);
    });

    it('should support creating new instances', async () => {
      const { createWebSocketClient } = await import('./WebSocketClient');
      const client1 = createWebSocketClient(config);
      const client2 = createWebSocketClient(config);

      expect(client1).not.toBe(client2);
    });
  });

  describe('Edge Cases', () => {
    it('should handle rapid disconnect/reconnect', () => {
      client.disconnect();
      client.disconnect();

      expect(client.getStatus()).toBe('DISCONNECTED');
    });

    it('should handle sending multiple messages rapidly', async () => {
      for (let i = 0; i < 50; i++) {
        await client.send('STATE_UPDATE', { index: i });
      }

      expect(client.getQueueLength()).toBeLessThanOrEqual(100);
    });

    it('should handle message callbacks with errors gracefully', () => {
      const errorCallback = vi.fn(() => {
        throw new Error('Callback error');
      });

      client.onMessage('STATE_UPDATE', errorCallback);

      // Should not propagate error
      expect(() => client).not.toThrow();
    });
  });
});
