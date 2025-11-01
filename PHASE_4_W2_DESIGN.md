# Phase 4.W2 Design: Interactive Mechanical Visualization

**Date**: 2025-11-01
**Phase**: Week 15 of 52-week schedule
**Status**: PLANNING & DESIGN (Pre-Implementation)

## Executive Summary

Phase 4.W2 implements a real-time 3D mechanical visualization of Babbage's Difference Engine No. 2 in the existing SvelteKit frontend. The design balances cross-platform compatibility (macOS OpenGL 4.1 through Windows/Linux OpenGL 4.3+) with educational clarity and performance.

**Key Decisions**:
- Use Three.js (already in dependencies) for WebGL abstraction layer
- Progressive feature enhancement: Base model (all platforms) → Compute shaders (Windows/Linux only)
- Modular architecture separating scene management, animation, state sync, and rendering
- WebSocket for real-time emulator state synchronization
- Instancing and LOD for performance optimization

---

## 1. Technical Landscape Analysis

### 1.1 Current Stack

**Frontend**:
- SvelteKit 1.27.6 (Vite-based build)
- Three.js 0.158.0 (already installed, latest stable)
- TypeScript 5.3.2 (strict mode)
- Vitest for testing

**Backend**:
- FastAPI with WebSocket support capability
- Phase 3 Emulator (564 tests passing)
- DEMachine state snapshot API

**Build Target**:
- All modern browsers (Chrome, Firefox, Safari 15+)
- macOS: WebGL 2.0 (OpenGL 4.1 backend)
- Windows/Linux: WebGL 2.0 with WebGL 2.0 extensions

### 1.2 Hardware Constraints

| Platform | OpenGL | Compute Shaders | Recommendation |
|----------|--------|-----------------|-----------------|
| macOS | 4.1 (Metal) | Not via OpenGL | Use WebGL 2.0 standard features |
| Windows | 4.6+ | Yes (gl_compute) | Can use compute if beneficial |
| Linux | 4.3+ | Yes (optional) | Can use compute if beneficial |

**Three.js Strategy**:
- Three.js provides WebGL 2.0 abstraction layer
- Custom shaders for advanced effects on capable platforms
- Graceful degradation on macOS (no compute, standard rendering)
- Feature detection via `renderer.capabilities` API

### 1.3 Three.js Capability Detection

```typescript
// Runtime feature detection
const hasComputeSupport =
  renderer.capabilities.isWebGL2 &&
  gl.getExtension('GL_ARB_compute_shader') !== null;

const maxTextureSize = renderer.capabilities.maxTextureSize;
const maxRenderbufferSize = renderer.capabilities.maxRenderbufferSize;
```

---

## 2. Mechanical Model Design

### 2.1 Difference Engine Architecture (Physical)

```
┌─────────────────────────────────────────────────┐
│  Main Shaft (Center, 0-360° rotation)            │
│  ─ 8 mechanical phases at 45° intervals         │
│  ─ 65,536 positions per full rotation           │
│  ─ Drives all subsystems via cam followers      │
└─────────────────────────────────────────────────┘

┌─ Column Bank (8 columns, arranged vertically) ─┐
│                                                  │
│  Col 0   Col 1   Col 2  ...  Col 7              │
│  ┌────┐  ┌────┐  ┌────┐      ┌────┐            │
│  │ 31 │  │ 31 │  │ 31 │      │ 31 │  digits    │
│  │... │  │... │  │... │      │... │  (0-9)     │
│  │ 0 │  │ 0 │  │ 0 │      │ 0 │              │
│  └────┘  └────┘  └────┘      └────┘            │
│                                                  │
│  Each column: 31-digit wheel stack              │
│  Mechanical: carry propagation via carriages    │
└─────────────────────────────────────────────────┘

┌─ Anticipating Carriage (Overlapped carry) ─────┐
│  ─ Detects carry from column N                  │
│  ─ Prepares carry for column N+1                │
│  ─ Propagates during CARRY phase                │
│  ─ Reduces propagation latency                  │
└─────────────────────────────────────────────────┘

Mechanical Phases (360° rotation):
  0°-45°:     IDLE         (no operation)
  45°-90°:    INPUT        (load values)
  90°-135°:   ADDITION     (add & carry)
  135°-180°:  CARRY        (anticipated carry prop)
  180°-225°:  OUTPUT       (prepare results)
  225°-270°:  ADVANCE      (shift row)
  270°-315°:  RESET        (clear state)
  315°-360°:  PAUSE        (brief rest)
```

### 2.2 3D Model Specification

**Principle**: Educationally accurate but simplified for clarity

```
Scene Hierarchy:
  Scene
    └─ DifferenceEngineGroup
        ├─ MainShaft
        │   ├─ RotationIndicator (visual cue at current angle)
        │   └─ PhaseLabel (text showing ADDITION, CARRY, etc.)
        │
        ├─ ColumnBank
        │   ├─ Column[0]
        │   │   ├─ ColumnFrame (structure)
        │   │   ├─ DigitWheels (8x geometry instances)
        │   │   ├─ DigitDisplay (text for current values)
        │   │   └─ CarryIndicator (visual state)
        │   ├─ Column[1]
        │   └─ ... (7 more)
        │
        ├─ AnticipatingCarriage
        │   ├─ CarriageFrame
        │   ├─ RatchetMechanism
        │   └─ StatusIndicator
        │
        └─ ControlPanel
            ├─ ExecutionLabel
            ├─ CycleCounter
            └─ CameraControls
```

**Polygon Counts** (Target: 60 FPS on mid-range hardware):
- Main shaft: 32 sides → 128 triangles
- Column frame (8x): 24 sides each → 96 triangles × 8 = 768
- Digit wheel (8×31 instances): 16 sides → 64 triangles × 248 = 15,872
- Carriage mechanism: 256 triangles
- Text geometry: ~500 triangles

**Total target**: ~17,500 triangles (acceptable for real-time)

### 2.3 Material & Appearance Design

**Color Scheme** (matching Phase 4.W1 dark theme):

| Component | Color | Rationale |
|-----------|-------|-----------|
| Columns (static) | #667eea (blue) | Primary accent |
| Column values (active) | #ffd700 (gold) | High visibility |
| Carry signals | #ff6b6b (red) | Critical mechanic |
| Main shaft | #888888 (gray) | Neutral structure |
| Carriage | #4ecdc4 (teal) | Secondary system |
| Phase highlights | Varies by phase | Educational coding |

**Material Types**:
- Metal (brushed): Main components
- Translucent (0.3 opacity): Digit wheels (show internal mechanics)
- Emissive (phase-specific): Carry propagation path

---

## 3. Architecture & Module Organization

### 3.1 Modular Component Structure

```
frontend/src/lib/
  ├─ visualization/
  │   ├─ DifferenceEngine3D.svelte      (Main component wrapper)
  │   │
  │   └─ scene/
  │       ├─ SceneManager.ts             (THREE.Scene, camera, renderer)
  │       ├─ CameraController.ts         (Orbit camera, keyboard shortcuts)
  │       ├─ LightingSetup.ts            (Ambient, directional, spotlights)
  │       └─ RenderingLoop.ts            (requestAnimationFrame orchestrator)
  │
  │   └─ mechanics/
  │       ├─ ColumnGeometry.ts           (Column frame, wheels, text)
  │       ├─ ShaftGeometry.ts            (Main shaft with phase markers)
  │       ├─ CarriageGeometry.ts         (Anticipating carriage visuals)
  │       ├─ GeometryCache.ts            (Reusable geometries w/ instancing)
  │       └─ MaterialLibrary.ts          (Shared materials & shaders)
  │
  │   └─ animation/
  │       ├─ ShaftRotation.ts            (Smooth shaft rotation animation)
  │       ├─ CarryPropagation.ts         (Carry signal visual flow)
  │       ├─ ColumnValueAnimation.ts     (Number change tweens)
  │       ├─ StateAnimator.ts            (Coordinate animations)
  │       └─ Timeline.ts                 (Global animation timeline)
  │
  │   └─ state/
  │       ├─ MachineStateStore.ts        (Svelte store for emulator state)
  │       ├─ StateReconciler.ts          (Diff & apply state updates)
  │       ├─ WebSocketClient.ts          (Real-time sync w/ backend)
  │       └─ StateHistory.ts             (Track state changes for timeline)
  │
  │   └─ rendering/
  │       ├─ BaseRenderer.ts             (Standard WebGL 2.0 rendering)
  │       ├─ ComputeShaderRenderer.ts    (Optional: Compute-optimized)
  │       ├─ FeatureDetector.ts          (Runtime capability check)
  │       └─ PerformanceMonitor.ts       (FPS, GPU memory tracking)
  │
  │   └─ ui/
  │       ├─ VisualizationControls.svelte (Playback, camera, overlays)
  │       ├─ StateOverlay.svelte         (HUD: phase, cycle, values)
  │       └─ DebugOverlay.svelte         (Hidden by default)
  │
  │   └─ shaders/
  │       ├─ columnVertex.glsl           (Column animation vertex)
  │       ├─ columnFragment.glsl         (Column coloring fragment)
  │       ├─ shaftVertex.glsl            (Shaft rotation)
  │       ├─ carryPropagation.glsl       (Carry flow visualization)
  │       └─ compute/
  │           └─ columnUpdate.glsl       (WebGL 2.0 compute optional)
```

### 3.2 Module Responsibilities

| Module | Responsibility | Input | Output |
|--------|-----------------|-------|--------|
| SceneManager | Create/manage Three.js scene, camera, renderer | ViewportSize | Scene reference |
| ColumnGeometry | Create 3D geometry for single column | ColumnIndex, Values | THREE.Group |
| ShaftRotation | Animate main shaft based on phase | Phase, Angle, Speed | Quaternion updates |
| MachineStateStore | Svelte store for emulator state | emulator API results | Reactive state |
| WebSocketClient | Real-time state sync from backend | emulator endpoint | StateUpdate events |
| StateReconciler | Merge API state with animation timeline | OldState, NewState | InterpolationPath |
| BaseRenderer | Render with standard WebGL 2.0 | Scene + State | Frame buffer |
| PerformanceMonitor | Track FPS, GPU memory, bottlenecks | Renderer telemetry | Metrics (console) |

### 3.3 Data Flow Architecture

```
┌─────────────────────┐
│  Emulator API       │
│  (Phase 4.W1)       │
│  POST /api/execute  │
│  WebSocket state    │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────────────────┐
│ WebSocketClient                 │
│ - Connect to /ws/emulator       │
│ - Handle state updates          │
│ - Parse JSON → StateUpdate      │
└──────────┬──────────────────────┘
           │
           ▼
┌─────────────────────────────────┐
│ MachineStateStore (Svelte)      │
│ - Reactive state variable       │
│ - Subscribers notified on change│
└──────────┬──────────────────────┘
           │
    ┌──────┴──────┐
    ▼             ▼
 StateReconciler  StateAnimator
    │             │
    ├─ Diff       ├─ Interpolate
    ├─ Merge      ├─ Tween values
    ├─ Validate   └─ Timeline sync
    │
    ▼
┌──────────────────────────────────┐
│ Rendering Pipeline               │
│ - Update geometry data           │
│ - Set material colors            │
│ - Apply animations               │
│ - Render frame                   │
└──────────────────────────────────┘
```

---

## 4. Implementation Phases (Modular)

### Phase 4.W2.A: Core Scene & Geometry (Days 1-3)

**Deliverables**:
- SceneManager with basic Three.js setup
- ColumnGeometry with simple cube representation
- ShaftGeometry with rotation indicator
- Basic lighting and camera

**Tests**:
- Scene initialization tests
- Geometry creation tests
- Camera interaction tests

**Lines of Code**: ~1,200 lines

### Phase 4.W2.B: State Synchronization (Days 3-5)

**Deliverables**:
- MachineStateStore (Svelte writable store)
- WebSocketClient (connection & message handling)
- StateReconciler (diff and merge algorithm)
- Integration with Phase 4.W1 API

**Tests**:
- WebSocket connection tests
- State merge logic tests
- Event propagation tests

**Lines of Code**: ~800 lines

### Phase 4.W2.C: Animation & Rendering (Days 5-7)

**Deliverables**:
- ShaftRotation animator (smooth phase transitions)
- ColumnValueAnimation (number tweens)
- RenderingLoop (requestAnimationFrame orchestration)
- StateAnimator (coordinate multiple animations)

**Tests**:
- Animation interpolation tests
- Timing synchronization tests
- Frame rate stability tests

**Lines of Code**: ~1,000 lines

### Phase 4.W2.D: Performance & Cross-Platform (Days 7-9)

**Deliverables**:
- FeatureDetector (OpenGL capability check)
- BaseRenderer (standard WebGL 2.0)
- ComputeShaderRenderer (optional, Windows/Linux only)
- PerformanceMonitor (FPS metrics, memory tracking)

**Tests**:
- Feature detection tests (mock GPU capabilities)
- Renderer switching tests
- Performance baseline tests

**Lines of Code**: ~800 lines

### Phase 4.W2.E: UI & Polish (Days 9-10)

**Deliverables**:
- VisualizationControls (playback, camera controls)
- StateOverlay (HUD with cycle/phase/values)
- DebugOverlay (for development)
- Responsive layout adjustments

**Tests**:
- UI interaction tests
- Overlay rendering tests
- Mobile responsiveness tests

**Lines of Code**: ~600 lines

**Total Phase 4.W2**: ~4,400 lines of code + 2,000+ test lines

---

## 5. OpenGL/WebGL Strategy

### 5.1 WebGL 2.0 (All Platforms)

**Guaranteed Features**:
- 16 texture units (vs 8 in WebGL 1.0)
- Instanced rendering (critical for repeated columns)
- Custom vertex/fragment shaders
- Framebuffer objects (for post-processing)

**Implementation** (Three.js handles most):

```typescript
// BaseRenderer.ts
const renderer = new THREE.WebGLRenderer({
  antialias: true,
  precision: 'highp',
  preserveDrawingBuffer: true,  // Allow readPixels for debugging
});

// Check WebGL version
const context = renderer.getContext('webgl2');
const isWebGL2 = context instanceof WebGL2RenderingContext;

// Detect extensions
const ext = {
  instanced: context.getExtension('ANGLE_instanced_arrays'),
  standardDerivatives: context.getExtension('OES_standard_derivatives'),
  textureFloat: context.getExtension('OES_texture_float'),
  textureHalfFloat: context.getExtension('OES_texture_half_float'),
};
```

### 5.2 OpenGL 4.1 (macOS Fallback)

**Limitation**: No compute shaders, no direct shader image load/store

**Strategy**: Use compute shader features ONLY if detection passes:

```typescript
// FeatureDetector.ts - Runtime check
hasComputeSupport() {
  const canvas = document.createElement('canvas');
  const gl = canvas.getContext('webgl2');
  if (!gl) return false;

  // Check for OpenGL 4.3+ compute shader support
  const computeExt = gl.getExtension('GL_ARB_compute_shader');
  return computeExt !== null;
}
```

### 5.3 OpenGL 4.3+ (Windows/Linux Optional)

**Potential Optimizations**:
- Compute shader for column value updates (batched)
- Indirect rendering for dynamic column count
- Shader storage buffer objects (SSBO)

**Decision**: Implement ONLY if profiling shows need

```typescript
// ComputeShaderRenderer.ts (conditional)
if (capabilities.hasComputeSupport) {
  // Use compute shader for batch updates
  // Update 8 columns in single dispatch
} else {
  // Fall back to BaseRenderer
  // Per-column update in animation loop
}
```

---

## 6. Performance Strategy

### 6.1 Optimization Targets

**Target**: 60 FPS on mid-range GPU (RTX 3060, GTX 1080, Apple M1)

**Metrics**:
- Draw calls: < 50 per frame
- Triangles: < 50,000 active per frame
- Textures: < 100 MB VRAM
- Memory allocations: < 5/frame

### 6.2 Techniques

**Instancing**:
- 8 columns × 31 digit wheels = 248 identical geometries
- Use ONE geometry + instancing (8x performance improvement)
- Per-instance color via instanced vertex attribute

```typescript
// ColumnGeometry.ts - Instancing
const wheelGeometry = new THREE.CylinderGeometry(1, 1, 0.5, 16);
const material = new THREE.MeshPhongMaterial();

const positions = new Float32Array(8 * 31 * 3);  // 3 floats per position
// ... populate positions for all 248 wheels

const instancedGeometry = wheelGeometry.clone();
instancedGeometry.setAttribute(
  'instancePosition',
  new THREE.InstancedBufferAttribute(positions, 3)
);

const mesh = new THREE.Mesh(instancedGeometry, material);
mesh.instanceCount = 248;
```

**Level of Detail (LOD)**:
- Close view: Full geometry (1,000+ tris per column)
- Medium view: Simplified (200 tris per column)
- Far view: Bounding box only (48 tris per column)

**Frustum Culling**:
- Three.js automatic (built-in)
- Disable rendering for non-visible columns

### 6.3 Monitoring

```typescript
// PerformanceMonitor.ts
class PerformanceMonitor {
  private frameCount = 0;
  private lastTime = performance.now();
  private fps = 60;

  update() {
    this.frameCount++;
    const now = performance.now();
    const elapsed = now - this.lastTime;

    if (elapsed >= 1000) {
      this.fps = this.frameCount / (elapsed / 1000);
      console.log(`FPS: ${this.fps.toFixed(1)}`);
      this.frameCount = 0;
      this.lastTime = now;
    }
  }

  // Memory tracking (WebGL extension)
  getGPUMemory() {
    const ext = gl.getExtension('WEBGL_debug_renderer_info');
    if (ext) {
      return gl.getParameter(ext.UNMASKED_RENDERER_WEBGL);
    }
    return 'Unknown';
  }
}
```

---

## 7. WebSocket Real-Time Sync Strategy

### 7.1 Backend Endpoint Design

**Add to backend API**:

```python
# backend/src/api/emulator.py - New WebSocket endpoint
@app.websocket("/ws/emulator")
async def websocket_emulator(websocket: WebSocket):
    """WebSocket for real-time emulator state sync."""
    await websocket.accept()

    try:
        while True:
            # Receive commands from client
            message = await websocket.receive_json()

            if message["type"] == "step":
                # Step one cycle
                debugger.step_cycle()
                state = get_current_state()
                await websocket.send_json({
                    "type": "state_update",
                    "state": state
                })

            elif message["type"] == "continue":
                # Continue to next breakpoint
                result = debugger.continue_execution()
                state = get_current_state()
                await websocket.send_json({
                    "type": "execution_complete",
                    "cyclesRun": result.cycles_run,
                    "state": state
                })

    except WebSocketDisconnect:
        logger.info("Client disconnected")
```

### 7.2 Frontend WebSocket Client

```typescript
// WebSocketClient.ts
class WebSocketClient {
  private ws: WebSocket | null = null;
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;

  async connect() {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const url = `${protocol}//${window.location.host}/ws/emulator`;

    this.ws = new WebSocket(url);

    this.ws.onmessage = (event) => {
      const message = JSON.parse(event.data);
      this.handleMessage(message);
    };

    this.ws.onerror = () => {
      this.reconnect();
    };
  }

  private handleMessage(message: any) {
    if (message.type === 'state_update') {
      // Dispatch to store
      machineStateStore.update(message.state);
    }
  }

  private reconnect() {
    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      setTimeout(() => this.connect(), 1000 * this.reconnectAttempts);
    }
  }

  send(command: any) {
    if (this.ws?.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(command));
    }
  }
}
```

---

## 8. Testing Strategy

### 8.1 Unit Tests

**Modules**: 300+ tests across all modules

```typescript
// tests/scene/SceneManager.test.ts
describe('SceneManager', () => {
  it('should create THREE.Scene with proper setup', () => {
    const manager = new SceneManager(mockContainer);
    expect(manager.scene).toBeDefined();
    expect(manager.camera).toBeDefined();
    expect(manager.renderer).toBeDefined();
  });
});

// tests/mechanics/ColumnGeometry.test.ts
describe('ColumnGeometry', () => {
  it('should create instanced geometry for columns', () => {
    const geometry = createColumnGeometry(8);
    expect(geometry.instanceCount).toBe(31 * 8);
  });
});
```

### 8.2 Integration Tests

**Workflows**: 100+ tests for state sync and animation

```typescript
// tests/integration/WebSocketSync.test.ts
describe('WebSocket State Sync', () => {
  it('should sync state updates from backend', async () => {
    const client = new WebSocketClient();
    await client.connect();

    const statePromise = new Promise((resolve) => {
      machineStateStore.subscribe((state) => resolve(state));
    });

    // Simulate backend message
    client.handleMessage({
      type: 'state_update',
      state: mockMachineState
    });

    const state = await statePromise;
    expect(state.cycle).toBe(mockMachineState.cycle);
  });
});
```

### 8.3 E2E Tests

**Scenarios**: 50+ end-to-end tests

```typescript
// e2e/visualization.spec.ts
test('should render 3D scene and animate on execute', async ({ page }) => {
  await page.goto('http://localhost:5173/emulator');

  // Input polynomial
  const inputs = await page.locator('input[type="number"]').all();
  await inputs[0].fill('1');
  await inputs[1].fill('2');

  // Execute
  await page.click('button:has-text("Execute Polynomial")');

  // Check 3D canvas renders
  const canvas = await page.locator('canvas');
  await expect(canvas).toBeVisible();

  // Verify shaft rotation animation
  // (would need screenshot comparison or synthetic check)
});
```

---

## 9. Cross-Platform Compatibility Matrix

| Feature | macOS | Windows | Linux | Implementation |
|---------|-------|---------|-------|-----------------|
| WebGL 2.0 | ✅ Metal | ✅ Direct3D/OpenGL | ✅ OpenGL | Three.js abstraction |
| Instancing | ✅ | ✅ | ✅ | Standard WebGL 2.0 |
| Compute Shaders | ❌ (OpenGL 4.1) | ✅ (4.3+) | ✅ (4.3+) | Conditional via feature detection |
| Vertex Shaders | ✅ | ✅ | ✅ | All platforms |
| Framebuffer Objects | ✅ | ✅ | ✅ | Standard WebGL 2.0 |
| Multisampling (MSAA) | ✅ | ✅ | ✅ | Three.js antialias option |
| Texture Compression | ⚠️ PVRTC | ✅ BC/S3TC | ✅ S3TC | Platform-specific detection |

**Principle**: Progressive enhancement
- Base: All platforms get full functionality with standard WebGL 2.0
- Optional: Compute shaders IF available (perf improvement only)
- Fallback: Graceful degradation if unsupported

---

## 10. Integration with Phase 4.W1

### 10.1 API Surface

**Expected Phase 4.W1 Endpoints**:

```typescript
// Existing endpoints (used by visualization)
POST /api/execute              → Returns execution results
GET /api/state                 → Returns current machine state
POST /api/debug/step           → Step one cycle
POST /api/debug/continue       → Continue execution

// New endpoints (for Phase 4.W2)
WebSocket /ws/emulator         → Real-time state updates
GET /api/emulator/geometry     → 3D model metadata (optional)
```

### 10.2 Component Integration

**Phase 4.W1** (Control/Results):
```
[EmulatorControl] → POST /api/execute → [EmulatorState/Results]
```

**Phase 4.W2** (3D Visualization):
```
[DifferenceEngine3D] ← WebSocket /ws/emulator ← [Backend state updates]
      ↓
   [Scene Manager]
   [Animation Controller]
   [Rendering Pipeline]
```

**Combined**: Phase 4.W2 renders 3D visualization alongside Phase 4.W1 UI

---

## 11. File Structure Summary

```
frontend/src/
  ├─ lib/
  │   ├─ visualization/
  │   │   ├─ DifferenceEngine3D.svelte         (Main wrapper)
  │   │   ├─ scene/
  │   │   │   ├─ SceneManager.ts               (~250 lines)
  │   │   │   ├─ CameraController.ts           (~200 lines)
  │   │   │   ├─ LightingSetup.ts              (~150 lines)
  │   │   │   └─ RenderingLoop.ts              (~200 lines)
  │   │   ├─ mechanics/
  │   │   │   ├─ ColumnGeometry.ts             (~300 lines)
  │   │   │   ├─ ShaftGeometry.ts              (~200 lines)
  │   │   │   ├─ CarriageGeometry.ts           (~200 lines)
  │   │   │   ├─ GeometryCache.ts              (~150 lines)
  │   │   │   └─ MaterialLibrary.ts            (~200 lines)
  │   │   ├─ animation/
  │   │   │   ├─ ShaftRotation.ts              (~250 lines)
  │   │   │   ├─ CarryPropagation.ts           (~200 lines)
  │   │   │   ├─ ColumnValueAnimation.ts       (~200 lines)
  │   │   │   ├─ StateAnimator.ts              (~250 lines)
  │   │   │   └─ Timeline.ts                   (~150 lines)
  │   │   ├─ state/
  │   │   │   ├─ MachineStateStore.ts          (~150 lines)
  │   │   │   ├─ StateReconciler.ts            (~300 lines)
  │   │   │   ├─ WebSocketClient.ts            (~250 lines)
  │   │   │   └─ StateHistory.ts               (~150 lines)
  │   │   ├─ rendering/
  │   │   │   ├─ BaseRenderer.ts               (~300 lines)
  │   │   │   ├─ ComputeShaderRenderer.ts      (~200 lines, optional)
  │   │   │   ├─ FeatureDetector.ts            (~150 lines)
  │   │   │   └─ PerformanceMonitor.ts         (~200 lines)
  │   │   ├─ ui/
  │   │   │   ├─ VisualizationControls.svelte  (~300 lines)
  │   │   │   ├─ StateOverlay.svelte           (~200 lines)
  │   │   │   └─ DebugOverlay.svelte           (~150 lines)
  │   │   └─ shaders/
  │   │       ├─ columnVertex.glsl             (~80 lines)
  │   │       ├─ columnFragment.glsl           (~80 lines)
  │   │       ├─ shaftVertex.glsl              (~60 lines)
  │   │       └─ carryPropagation.glsl         (~80 lines)
  │   │
  │   └─ (existing components)
  │
  ├─ routes/
  │   ├─ emulator/
  │   │   └─ +page.svelte                      (Update to include 3D)
  │   └─ (other routes)
  │
  └─ tests/
      └─ visualization/
          ├─ scene/                            (~400 lines tests)
          ├─ mechanics/                        (~500 lines tests)
          ├─ animation/                        (~500 lines tests)
          ├─ state/                            (~400 lines tests)
          ├─ rendering/                        (~300 lines tests)
          └─ integration/                      (~600 lines tests)
```

**Total Implementation**: ~4,400 lines (code) + ~2,700 lines (tests)

---

## 12. Risk Mitigation

### Risk: Poor performance on low-end GPU

**Mitigation**:
- Early performance profiling in Week 2
- Polygon count budgeting per component
- LOD implementation for distance-based quality
- Graceful degradation (low FPS warning)

### Risk: WebGL 2.0 not available (old Safari)

**Mitigation**:
- Explicit WebGL 2.0 requirement check on startup
- Display "Browser not supported" message
- Suggest Chrome/Firefox/Safari 15+ upgrade path

### Risk: WebSocket connection drops

**Mitigation**:
- Automatic reconnection with exponential backoff
- Pause animation during disconnection
- Show "Reconnecting..." UI state
- Queue commands during downtime

### Risk: 3D canvas interferes with existing UI

**Mitigation**:
- Three.js canvas isolated in dedicated container
- Responsive layout (3D on right, controls on left)
- Mobile: Toggle between 3D and UI view
- No event bubbling between layers

---

## 13. Success Criteria

✅ **Phase 4.W2 Complete When**:

1. **Functionality**:
   - 3D scene renders Difference Engine structure
   - Shaft rotation synchronized with emulator cycles
   - Column values update in real-time
   - Carry propagation visualized

2. **Performance**:
   - 60 FPS on RTX 3060/GTX 1080/M1 (measured)
   - < 5 second load time
   - < 100 MB GPU memory
   - Zero memory leaks (24-hour test)

3. **Compatibility**:
   - macOS 11+ (Metal via WebGL 2.0)
   - Windows 10+ (DirectX via WebGL 2.0)
   - Linux with OpenGL 4.1+
   - Chrome, Firefox, Safari 15+

4. **Integration**:
   - WebSocket sync functional
   - Phase 4.W1 UI controls 3D animation
   - Debugger breakpoints visible in visualization
   - Tests: 300+ unit + 100+ integration + 50+ E2E

5. **Code Quality**:
   - 90%+ test coverage
   - Zero TypeScript errors (strict mode)
   - Zero console warnings
   - All modules documented

---

## 14. Next Steps

### Immediate (Pre-Implementation)
1. ✅ Design document (THIS DOCUMENT)
2. Peer review of architecture
3. Finalize module interfaces
4. Set up test framework

### Week 1 (Phase 4.W2.A)
- Core scene and geometry
- Basic THREE.js setup
- Rendering loop

### Week 2 (Phase 4.W2.B)
- WebSocket integration
- State synchronization
- API connection

### Week 3 (Phase 4.W2.C)
- Animation controllers
- Smooth phase transitions
- Timeline coordination

### Week 4 (Phase 4.W2.D-E)
- Performance optimization
- Cross-platform testing
- UI polish

---

**Status**: ✅ DESIGN COMPLETE - Ready for implementation review

**Architecture**: Modular, testable, cross-platform compatible
**Performance**: Targets 60 FPS with progressive enhancement
**Integration**: Seamless with Phase 4.W1 and Phase 3 emulator core
**Testing**: 300+ unit + 100+ integration + 50+ E2E tests planned
