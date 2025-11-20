# API Integration Guide

**Version**: 1.0.0
**Last Updated**: 2025-11-20
**Component**: Ancient Compute Backend API

---

## Table of Contents

1. [Overview](#overview)
2. [WebSocket Protocol](#websocket-protocol)
3. [REST API Endpoints](#rest-api-endpoints)
4. [Message Formats](#message-formats)
5. [Authentication](#authentication)
6. [Error Handling](#error-handling)
7. [Client Libraries](#client-libraries)
8. [Backend Integration](#backend-integration)
9. [Examples](#examples)
10. [Best Practices](#best-practices)

---

## Overview

The Ancient Compute system provides two API interfaces:

```
┌────────────────────────────────────────────────┐
│  API Architecture                              │
├────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────┐              ┌──────────┐        │
│  │  Client  │              │  Client  │        │
│  │   App    │              │   App    │        │
│  └────┬─────┘              └────┬─────┘        │
│       │                         │              │
│       │ WebSocket               │ REST         │
│       │ (Real-time)             │ (Request)    │
│       ▼                         ▼              │
│  ┌─────────────────────────────────────┐       │
│  │  FastAPI Backend                    │       │
│  ├─────────────────────────────────────┤       │
│  │  • WebSocket Endpoint (/ws)         │       │
│  │  • REST Endpoints (/api/v1/*)       │       │
│  │  • Emulator Engine                  │       │
│  │  • Compilation Services             │       │
│  └─────────────────────────────────────┘       │
│                                                 │
└────────────────────────────────────────────────┘
```

### Use Cases

**WebSocket (Real-time)**:
- Emulator state updates
- Live program execution
- Debugging sessions
- Performance monitoring

**REST API (Request/Response)**:
- Code compilation
- Program execution
- Timeline data retrieval
- User management

---

## WebSocket Protocol

### Connection

**Endpoint**: `ws://localhost:8000/ws`

**Connection Flow**:
```javascript
const ws = new WebSocket('ws://localhost:8000/ws');

ws.onopen = (event) => {
  console.log('Connected to emulator');
};

ws.onmessage = (event) => {
  const message = JSON.parse(event.data);
  handleMessage(message);
};

ws.onerror = (event) => {
  console.error('WebSocket error:', event);
};

ws.onclose = (event) => {
  console.log('Disconnected from emulator');
};
```

### Message Format

All messages are JSON with the following structure:

```typescript
interface WebSocketMessage {
  type: string;           // Message type
  sessionId?: string;     // Optional session identifier
  timestamp: number;      // Unix timestamp
  data: any;             // Message-specific data
}
```

### Message Types

#### 1. State Update (Server → Client)

Sent when emulator state changes:

```json
{
  "type": "state_update",
  "timestamp": 1700000000000,
  "data": {
    "state": {
      "registers": {
        "R0": 15,
        "R1": 5,
        "R2": 20,
        "R3": 0
      },
      "memory": [0, 0, 0, ..., 42, ...],
      "programCounter": 10,
      "accumulator": 15,
      "carryFlag": false,
      "runningFlag": true,
      "cycles": 10,
      "mill": {
        "operation": "add",
        "operandA": 10,
        "operandB": 5,
        "result": 15,
        "progress": 1.0
      },
      "columns": [...],
      "store": {...},
      "barrels": [...]
    }
  }
}
```

#### 2. Command (Client → Server)

Execute emulator command:

```json
{
  "type": "command",
  "timestamp": 1700000000000,
  "data": {
    "command": "step" | "continue" | "pause" | "reset" | "load",
    "args": {}
  }
}
```

**Commands**:
- `step`: Execute one cycle
- `continue`: Resume execution
- `pause`: Pause execution
- `reset`: Reset emulator state
- `load`: Load new program

**Example - Load Program**:
```json
{
  "type": "command",
  "timestamp": 1700000000000,
  "data": {
    "command": "load",
    "args": {
      "program": "LOAD R0, 10\nADD R0, #5\nHALT"
    }
  }
}
```

#### 3. Breakpoint (Client → Server)

Manage breakpoints:

```json
{
  "type": "breakpoint",
  "timestamp": 1700000000000,
  "data": {
    "action": "add" | "remove" | "enable" | "disable",
    "cycle": 42,
    "condition": "R0 > 10"  // Optional
  }
}
```

#### 4. Watch (Client → Server)

Add/remove watch expressions:

```json
{
  "type": "watch",
  "timestamp": 1700000000000,
  "data": {
    "action": "add" | "remove",
    "expression": "R0",
    "watchId": "watch_1"
  }
}
```

#### 5. Error (Server → Client)

Error occurred during execution:

```json
{
  "type": "error",
  "timestamp": 1700000000000,
  "data": {
    "code": "EXECUTION_ERROR",
    "message": "Invalid memory access at address 4096",
    "cycle": 42,
    "details": {...}
  }
}
```

### Reconnection Strategy

Implement exponential backoff:

```javascript
class WebSocketClient {
  constructor(url) {
    this.url = url;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 10;
    this.reconnectDelay = 3000; // Start with 3s
    this.messageQueue = [];
  }

  connect() {
    this.ws = new WebSocket(this.url);

    this.ws.onopen = () => {
      this.reconnectAttempts = 0;
      this.reconnectDelay = 3000;
      this.flushMessageQueue();
    };

    this.ws.onclose = () => {
      this.reconnect();
    };

    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
    };
  }

  reconnect() {
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      console.error('Max reconnection attempts reached');
      return;
    }

    this.reconnectAttempts++;
    console.log(`Reconnecting in ${this.reconnectDelay}ms...`);

    setTimeout(() => {
      this.connect();
      this.reconnectDelay *= 2; // Exponential backoff
    }, this.reconnectDelay);
  }

  send(message) {
    if (this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    } else {
      // Queue message for later
      this.messageQueue.push(message);
    }
  }

  flushMessageQueue() {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      this.send(message);
    }
  }
}
```

---

## REST API Endpoints

### Base URL

**Development**: `http://localhost:8000/api/v1`
**Production**: `https://api.ancient-compute.org/api/v1`

### Code Execution

#### Execute Code

**POST** `/execute/run`

Compile and execute code in any supported language:

```bash
curl -X POST http://localhost:8000/api/v1/execute/run \
  -H "Content-Type: application/json" \
  -d '{
    "code": "int main() { return 42; }",
    "language": "c",
    "input_data": ""
  }'
```

**Request**:
```typescript
interface ExecuteRequest {
  code: string;              // Source code
  language: string;          // c | python | haskell | java | lisp | idris2 | systemf | babbage-assembly
  input_data?: string;       // Optional stdin input
  timeout?: number;          // Execution timeout (ms)
}
```

**Response**:
```typescript
interface ExecuteResponse {
  stdout: string;            // Standard output
  stderr: string;            // Standard error
  exit_code: number;         // Exit code (0 = success)
  compilation_time: number;  // Compilation time (ms)
  execution_time: number;    // Execution time (ms)
  ir_text?: string;          // Intermediate representation
  assembly_text?: string;    // Babbage assembly
  machine_code?: string;     // Machine code (hex)
  error?: string;            // Error message if failed
}
```

**Example Response**:
```json
{
  "stdout": "",
  "stderr": "",
  "exit_code": 42,
  "compilation_time": 45.2,
  "execution_time": 12.8,
  "ir_text": "...",
  "assembly_text": "LOAD R0, #42\nHALT",
  "machine_code": "0x01 0x00 0x2A 0xFF"
}
```

#### Get Supported Languages

**GET** `/execute/languages`

Returns list of supported languages:

```bash
curl http://localhost:8000/api/v1/execute/languages
```

**Response**:
```json
{
  "languages": [
    {
      "name": "c",
      "version": "11",
      "compiler": "gcc 11.4.0",
      "features": ["pointers", "arrays", "functions"]
    },
    {
      "name": "python",
      "version": "3.11",
      "compiler": "cpython",
      "features": ["dynamic_typing", "list_comprehensions"]
    },
    ...
  ]
}
```

### Emulator Control

#### Load Program

**POST** `/emulator/load`

Load program into emulator:

```bash
curl -X POST http://localhost:8000/api/v1/emulator/load \
  -H "Content-Type: application/json" \
  -d '{
    "program": "LOAD R0, 10\nADD R0, #5\nHALT",
    "format": "assembly"
  }'
```

**Request**:
```typescript
interface LoadProgramRequest {
  program: string;           // Program source
  format: "assembly" | "machine_code";
  entryPoint?: number;       // Entry point address
}
```

#### Get Emulator State

**GET** `/emulator/state`

Retrieve current emulator state:

```bash
curl http://localhost:8000/api/v1/emulator/state
```

**Response**: Same as WebSocket `state_update` message.

### Timeline Data

#### Get Eras

**GET** `/timeline/eras`

Get all historical eras:

```bash
curl http://localhost:8000/api/v1/timeline/eras
```

**Response**:
```json
{
  "eras": [
    {
      "id": 1,
      "name": "Prehistory of Counting",
      "start_year": -20000,
      "end_year": -3000,
      "description": "...",
      "technologies": ["tally marks", "clay tokens"]
    },
    ...
  ]
}
```

#### Get Lesson

**GET** `/timeline/lessons/{id}`

Get lesson content:

```bash
curl http://localhost:8000/api/v1/timeline/lessons/1
```

**Response**:
```json
{
  "id": 1,
  "title": "Egyptian Multiplication",
  "content": "# Egyptian Multiplication\n\n...",
  "exercises": [...],
  "duration": 45,
  "difficulty": "beginner"
}
```

---

## Message Formats

### Standard Response Envelope

All REST API responses use this format:

```typescript
interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: {
    code: string;
    message: string;
    details?: any;
  };
  meta?: {
    requestId: string;
    timestamp: number;
    version: string;
  };
}
```

**Success Example**:
```json
{
  "success": true,
  "data": {
    "result": 42
  },
  "meta": {
    "requestId": "req_abc123",
    "timestamp": 1700000000000,
    "version": "1.0.0"
  }
}
```

**Error Example**:
```json
{
  "success": false,
  "error": {
    "code": "COMPILATION_ERROR",
    "message": "Syntax error on line 5",
    "details": {
      "line": 5,
      "column": 10,
      "token": ";"
    }
  },
  "meta": {
    "requestId": "req_xyz789",
    "timestamp": 1700000000000,
    "version": "1.0.0"
  }
}
```

---

## Authentication

### API Keys

**Development**: No authentication required
**Production**: API key required in header

```bash
curl http://localhost:8000/api/v1/execute/run \
  -H "X-API-Key: your_api_key_here" \
  -H "Content-Type: application/json" \
  -d '...'
```

### Rate Limiting

**Limits**:
- Free tier: 100 requests/hour
- Pro tier: 1000 requests/hour
- Enterprise: Unlimited

**Rate Limit Headers**:
```
X-RateLimit-Limit: 100
X-RateLimit-Remaining: 95
X-RateLimit-Reset: 1700000000
```

---

## Error Handling

### Error Codes

| Code | HTTP Status | Description |
|------|-------------|-------------|
| `COMPILATION_ERROR` | 400 | Code compilation failed |
| `EXECUTION_ERROR` | 400 | Program execution failed |
| `TIMEOUT_ERROR` | 408 | Execution timeout exceeded |
| `INVALID_REQUEST` | 400 | Malformed request |
| `UNAUTHORIZED` | 401 | Authentication required |
| `RATE_LIMIT_EXCEEDED` | 429 | Too many requests |
| `INTERNAL_ERROR` | 500 | Server error |

### Retry Strategy

Implement retries with exponential backoff:

```javascript
async function executeWithRetry(request, maxRetries = 3) {
  for (let attempt = 1; attempt <= maxRetries; attempt++) {
    try {
      const response = await fetch('/api/v1/execute/run', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(request)
      });

      if (response.ok) {
        return await response.json();
      }

      if (response.status === 429) {
        // Rate limited - wait and retry
        const delay = Math.pow(2, attempt) * 1000;
        await new Promise(resolve => setTimeout(resolve, delay));
        continue;
      }

      if (response.status >= 500) {
        // Server error - retry
        const delay = Math.pow(2, attempt) * 1000;
        await new Promise(resolve => setTimeout(resolve, delay));
        continue;
      }

      // Client error - don't retry
      throw new Error(await response.text());
    } catch (error) {
      if (attempt === maxRetries) {
        throw error;
      }
    }
  }
}
```

---

## Client Libraries

### JavaScript/TypeScript

```bash
npm install ancient-compute-client
```

```typescript
import { AncientComputeClient } from 'ancient-compute-client';

const client = new AncientComputeClient({
  baseURL: 'http://localhost:8000',
  apiKey: 'your_api_key'
});

// Execute code
const result = await client.execute({
  code: 'int main() { return 42; }',
  language: 'c'
});

console.log(result.exit_code); // 42

// WebSocket connection
const emulator = client.emulator();
emulator.on('state', (state) => {
  console.log('State:', state);
});

emulator.connect();
emulator.step();
```

### Python

```bash
pip install ancient-compute-client
```

```python
from ancient_compute import AncientComputeClient

client = AncientComputeClient(
    base_url='http://localhost:8000',
    api_key='your_api_key'
)

# Execute code
result = client.execute(
    code='int main() { return 42; }',
    language='c'
)

print(result.exit_code)  # 42

# WebSocket connection
emulator = client.emulator()

@emulator.on('state')
def handle_state(state):
    print('State:', state)

emulator.connect()
emulator.step()
```

---

## Backend Integration

### Emulator Bridge

The `EmulatorStateBridge` connects the backend emulator to frontend:

```typescript
// frontend/src/lib/integration/EmulatorStateBridge.ts

export class EmulatorStateBridge {
  private ws: WebSocket;
  private stateStore: MachineStateStore;

  constructor(wsUrl: string, stateStore: MachineStateStore) {
    this.ws = new WebSocket(wsUrl);
    this.stateStore = stateStore;

    this.ws.onmessage = (event) => {
      const message = JSON.parse(event.data);

      if (message.type === 'state_update') {
        this.stateStore.update(message.data.state);
      }
    };
  }

  sendCommand(command: string, args?: any) {
    this.ws.send(JSON.stringify({
      type: 'command',
      timestamp: Date.now(),
      data: { command, args }
    }));
  }
}
```

### State Synchronization

```typescript
// Throttle state updates to 60fps
class ThrottledStateBridge extends EmulatorStateBridge {
  private lastUpdateTime = 0;
  private updateInterval = 1000 / 60; // 16.67ms

  onMessage(event: MessageEvent) {
    const now = Date.now();

    if (now - this.lastUpdateTime < this.updateInterval) {
      return; // Skip update
    }

    this.lastUpdateTime = now;
    super.onMessage(event);
  }
}
```

---

## Examples

### Full Execution Workflow

```typescript
import { AncientComputeClient } from 'ancient-compute-client';

async function fullWorkflow() {
  const client = new AncientComputeClient({
    baseURL: 'http://localhost:8000'
  });

  // 1. Compile code
  const compileResult = await client.execute({
    code: `
      LOAD R0, 0
      LOAD R1, 10
      loop:
        ADD R0, #1
        CMP R0, R1
        JLT loop
      HALT
    `,
    language: 'babbage-assembly'
  });

  if (compileResult.error) {
    console.error('Compilation failed:', compileResult.error);
    return;
  }

  // 2. Load into emulator
  const emulator = client.emulator();
  await emulator.connect();
  await emulator.load(compileResult.machine_code);

  // 3. Set breakpoint
  await emulator.setBreakpoint(5); // Break at cycle 5

  // 4. Listen for state updates
  emulator.on('state', (state) => {
    console.log(`Cycle ${state.cycles}: R0=${state.registers.R0}`);
  });

  // 5. Run until breakpoint
  await emulator.continue();

  // 6. Step through remaining execution
  for (let i = 0; i < 5; i++) {
    await emulator.step();
  }

  // 7. Get final state
  const finalState = await emulator.getState();
  console.log('Final R0:', finalState.registers.R0); // 10
}

fullWorkflow();
```

---

## Best Practices

### 1. Connection Management

- Reuse WebSocket connections
- Implement reconnection logic
- Handle connection failures gracefully

### 2. State Updates

- Throttle updates to avoid overwhelming frontend (60fps max)
- Use state diffs to minimize data transfer
- Batch multiple small updates

### 3. Error Handling

- Always check response success field
- Implement retry logic for transient errors
- Log errors for debugging

### 4. Performance

- Use WebSocket for real-time updates (not polling)
- Cache static data (eras, lessons)
- Compress large payloads

### 5. Security

- Never expose API keys in client code
- Use HTTPS in production
- Validate all user inputs on backend

---

## Further Reading

- [DEBUGGER_USAGE.md](./DEBUGGER_USAGE.md) - Debugging features
- [PROFILER_GUIDE.md](./PROFILER_GUIDE.md) - Performance profiling
- [EMULATOR_VISUALIZATION_GUIDE.md](./EMULATOR_VISUALIZATION_GUIDE.md) - Visualization

---

**Questions or Issues?**
Report bugs at: https://github.com/ancient-compute/issues
API Documentation: https://docs.ancient-compute.org/api
