# Phase 4.W2 Implementation Roadmap

**Date**: 2025-11-01
**Duration**: Week 15 (estimated 10 working days)
**Target Week**: Week 15 of 52-week schedule
**Previous**: Phase 4.W1 (100% complete: Frontend UI + Backend API + Tests)

## Overview

This document provides a day-by-day implementation roadmap for Phase 4.W2: Interactive Mechanical Visualization. The design separates work into 5 parallel-capable, incrementally testable modules.

---

## Sprint Structure

### Week 15, Day 1-2: Foundation (SceneManager + Geometry)

**Goal**: Establish rendering pipeline and basic 3D model

#### Day 1 Tasks:

1. **SceneManager.ts** (250 lines)
   - Initialize THREE.WebGLRenderer with quality settings
   - Set up camera (orthographic or perspective)
   - Configure basic scene lighting (ambient + directional)
   - Add event listeners (resize, devicePixelRatio)
   - Export scene, camera, renderer references

2. **CameraController.ts** (200 lines)
   - Implement orbit camera (mouse drag rotation)
   - Implement keyboard shortcuts (1-4 preset views)
   - Reset camera position button
   - Zoom controls (mouse wheel)

3. **LightingSetup.ts** (150 lines)
   - Ambient light (base illumination)
   - Directional light (main shaft highlight)
   - Optional: Spotlight on carry mechanism

4. **RenderingLoop.ts** (200 lines)
   - RequestAnimationFrame orchestrator
   - Delta time calculation
   - Frame rate independent updates
   - Animation controller scheduling

5. **Unit Tests** (300 lines)
   - Scene initialization
   - Camera setup validation
   - Lighting intensity checks
   - Frame loop timing

**Deliverables**:
- Rendering pipeline working (black scene with lights)
- Camera interactive (orbit + zoom)
- Tests passing: 15+ unit tests

**Output**: ~/Phase_4_W2_Day1_Report.md

#### Day 2 Tasks:

1. **ColumnGeometry.ts** (300 lines)
   - Create reusable wheel geometry (16-sided cylinder)
   - Create column frame geometry (rectangular structure)
   - Position 31 wheel instances per column
   - Apply materials (metallic + translucent)
   - Use THREE.InstancedBufferGeometry for efficiency

2. **ShaftGeometry.ts** (200 lines)
   - Create main shaft cylinder (center of scene)
   - Add phase markers at 45° intervals (8 total)
   - Create rotation indicator (arrow or wedge)
   - Set up shader for phase-based coloring

3. **CarriageGeometry.ts** (200 lines)
   - Create anticipating carriage frame
   - Add ratchet mechanism visualization
   - Create carry signal propagation path (line geometry)

4. **GeometryCache.ts** (150 lines)
   - Cache reusable geometries
   - Implement geometry pooling
   - Destructor for cleanup

5. **MaterialLibrary.ts** (200 lines)
   - Define materials: metal, translucent, emissive
   - Create color lookup functions (phase → color)
   - Export material instances

6. **Unit Tests** (400 lines)
   - Column instancing count verification
   - Geometry vertex count validation
   - Material property checks
   - Performance baseline (polygon count)

**Deliverables**:
- 3D geometry visible (columns, shaft, carriage)
- Material colors applied
- Instance rendering working (8 columns × 31 wheels)
- Tests passing: 25+ unit tests

**Output**: ~/Phase_4_W2_Day2_Report.md

---

### Week 15, Day 3-4: State Management (WebSocket + Synchronization)

**Goal**: Connect frontend to backend state updates in real-time

#### Day 3 Tasks:

1. **MachineStateStore.ts** (150 lines)
   - Create Svelte writable store for machine state
   - Define StateSnapshot type (cycle, phase, angle, columns, carries)
   - Implement state getter/setter
   - Subscribe mechanism for animation system

2. **WebSocketClient.ts** (250 lines)
   - Establish WebSocket connection to /ws/emulator
   - Handle connection lifecycle (connect, disconnect, reconnect)
   - Parse incoming state updates
   - Queue commands during disconnection
   - Implement exponential backoff reconnection

3. **Unit Tests** (300 lines)
   - Store creation and subscription
   - WebSocket message parsing
   - Reconnection logic simulation
   - Command queueing validation

**Deliverables**:
- WebSocket connects to backend (if available)
- State updates flowing through store
- Reconnection working on timeout
- Tests passing: 18+ unit tests

**Output**: ~/Phase_4_W2_Day3_Report.md

#### Day 4 Tasks:

1. **StateReconciler.ts** (300 lines)
   - Diff algorithm (compare old/new state)
   - Merge strategy (conflict resolution)
   - Validation (sanity checks on state values)
   - Generate interpolation paths for animation
   - Handle missing fields gracefully

2. **StateHistory.ts** (150 lines)
   - Track state changes over time (last 100 frames)
   - Timestamp each state snapshot
   - Enable timeline scrubbing (future feature)
   - Export history for debugging

3. **Integration Tests** (500 lines)
   - End-to-end state sync workflow
   - Reconciliation correctness
   - Animation path generation
   - Stress test (rapid state updates)

**Deliverables**:
- State diffs computed correctly
- No animation jumps on state update
- History tracking working
- Tests passing: 30+ integration tests

**Output**: ~/Phase_4_W2_Day4_Report.md

---

### Week 15, Day 5-6: Animation & Rendering (Timeline + Shaders)

**Goal**: Smooth mechanical animations synchronized with state

#### Day 5 Tasks:

1. **ShaftRotation.ts** (250 lines)
   - Smooth rotation animation (quaternion interpolation)
   - Phase transition timing
   - Rotation speed control (slow/normal/fast)
   - Angle-to-rotation conversion

2. **ColumnValueAnimation.ts** (200 lines)
   - Number tweening (old value → new value)
   - Digit-by-digit animation option
   - Color flash on change

3. **CarryPropagation.ts** (200 lines)
   - Visualize carry signal flow (column 0 → 7)
   - Glow effect on carry bits
   - Timing: synchronized with carry phase

4. **Timeline.ts** (150 lines)
   - Global animation timeline
   - Coordinate multiple animations
   - Time scaling (playback speed)

5. **Unit Tests** (400 lines)
   - Rotation interpolation accuracy
   - Animation timing validation
   - Carry propagation sequencing

**Deliverables**:
- Shaft rotates smoothly
- Column values tween on change
- Carry signal visualized
- Tests passing: 25+ unit tests

**Output**: ~/Phase_4_W2_Day5_Report.md

#### Day 6 Tasks:

1. **StateAnimator.ts** (250 lines)
   - Coordinate all animations
   - Subscribe to state changes
   - Trigger animation updates
   - Handle edge cases (rapid updates)

2. **Custom Shaders** (250 lines total)
   - columnVertex.glsl (80 lines): Instance position + animation
   - columnFragment.glsl (80 lines): Phase-based coloring
   - shaftVertex.glsl (60 lines): Rotation around Y-axis
   - carryPropagation.glsl (80 lines): Carry glow effect

3. **BaseRenderer.ts** (300 lines)
   - Compile and link shaders
   - Set up uniform variables
   - Render scene each frame
   - Fallback to basic materials if shaders fail

4. **Integration Tests** (500 lines)
   - Full animation workflow
   - Shader compilation
   - Rendering correctness

**Deliverables**:
- Smooth animations rendering
- Shaders applied correctly
- No visual artifacts
- Tests passing: 30+ tests

**Output**: ~/Phase_4_W2_Day6_Report.md

---

### Week 15, Day 7-8: Performance & Cross-Platform (Optimization)

**Goal**: Achieve 60 FPS across platforms, add optional compute shaders

#### Day 7 Tasks:

1. **FeatureDetector.ts** (150 lines)
   - Detect WebGL 2.0 support
   - Check for compute shader extension
   - Detect GPU capabilities (max texture size, etc.)
   - Return capability report

2. **PerformanceMonitor.ts** (200 lines)
   - FPS counter (60 target)
   - Memory profiling (GPU + JS heap)
   - Draw call counter
   - Bottleneck identification
   - Console output for debugging

3. **LOD Implementation** (200 lines)
   - Define detail levels: HIGH (all geometry), MID (50% simplified), LOW (bounding boxes)
   - Distance-based LOD selection
   - Smooth LOD transitions

4. **Benchmarking Tests** (300 lines)
   - FPS stability test (5 minute runs)
   - Memory leak detection (24 hour simulation)
   - Polygon count validation

**Deliverables**:
- 60 FPS on mid-range GPU measured
- Performance metrics dashboard
- Memory usage < 100 MB GPU VRAM
- Tests passing: 20+ performance tests

**Output**: ~/Phase_4_W2_Day7_Report.md

#### Day 8 Tasks:

1. **ComputeShaderRenderer.ts** (200 lines, optional)
   - Implement ONLY if profiling shows benefit
   - Compute shader for batch column updates
   - Conditional: feature detected + benefit measured

2. **Graceful Degradation** (100 lines)
   - Fallback from compute to standard rendering
   - Fallback from WebGL 2.0 to WebGL 1.0 (if needed)
   - Warning messages for unsupported features

3. **Cross-Platform Validation** (150 lines)
   - Test on macOS (Metal via WebGL 2.0)
   - Test on Windows (DirectX/OpenGL via WebGL 2.0)
   - Test on Linux (OpenGL via WebGL 2.0)
   - Mobile responsiveness check

4. **Integration Tests** (400 lines)
   - Feature detection accuracy
   - Renderer fallback correctness
   - Performance on low-end GPU simulation

**Deliverables**:
- Works on macOS, Windows, Linux
- Mobile responsive (60 FPS on iPad)
- Graceful degradation if features unavailable
- Tests passing: 35+ cross-platform tests

**Output**: ~/Phase_4_W2_Day8_Report.md

---

### Week 15, Day 9-10: UI & Polish (Final Integration)

**Goal**: Complete UI controls, documentation, final testing

#### Day 9 Tasks:

1. **VisualizationControls.svelte** (300 lines)
   - Playback controls (play/pause, speed)
   - Camera presets (top, front, isometric)
   - Toggle overlays (HUD, debug)
   - Screenshot/recording buttons

2. **StateOverlay.svelte** (200 lines)
   - HUD showing: cycle count, phase, angle
   - Column values (real-time update)
   - Carry signal states
   - Breakpoint indicators

3. **DebugOverlay.svelte** (150 lines)
   - FPS counter (hidden by default)
   - Memory usage (hidden by default)
   - Draw call count
   - Activation key (press D)

4. **Integration with Emulator Page** (200 lines)
   - Add 3D visualization to existing emulator page
   - Responsive layout (3D on right, UI on left)
   - Mobile toggle (switch between 3D and UI)

5. **E2E Tests** (400 lines)
   - Full user workflows
   - Camera controls
   - Overlay visibility
   - Mobile responsiveness

**Deliverables**:
- Professional UI with controls
- HUD showing all relevant information
- Mobile responsive layout
- Tests passing: 40+ E2E tests

**Output**: ~/Phase_4_W2_Day9_Report.md

#### Day 10 Tasks:

1. **Documentation** (200 lines)
   - Module-level JSDoc comments
   - Architecture diagram explanations
   - Shader comments
   - README for visualization system

2. **Bug Fixes & Polish**
   - Fix any discovered issues
   - Optimize remaining bottlenecks
   - Improve error messages

3. **Final Testing** (300 lines)
   - Comprehensive test run (all suites)
   - Performance baseline final check
   - Cross-browser compatibility
   - Mobile testing on real devices

4. **Completion & Review**
   - Generate coverage reports
   - Create final summary document
   - Prepare for merge to main

**Deliverables**:
- 100% functionality implemented
- All tests passing (300+ unit + 100+ integration + 50+ E2E)
- 90%+ code coverage
- Documentation complete
- Ready for Phase 4.W3

**Output**: PHASE_4_W2_COMPLETION.md

---

## Parallel Work Opportunities

**Days 1-2** can have sub-parallel paths:
- Path A: SceneManager + CameraController (one person)
- Path B: ColumnGeometry + ShaftGeometry (one person)
- Path C: Tests & Integration (one person)

**Days 3-4** can be parallel:
- Path A: WebSocketClient (real-time sync)
- Path B: StateReconciler (animation paths)
- Path C: Tests & Integration

**Days 5-6** can be parallel:
- Path A: ShaftRotation + ColumnValueAnimation
- Path B: Custom Shaders
- Path C: BaseRenderer + Tests

---

## Risk Checkpoints

**After Day 2**: Can render basic 3D geometry?
- YES → Continue
- NO → Debug geometry creation, shader compilation

**After Day 4**: Can receive state updates from backend?
- YES → Continue
- NO → Check WebSocket connection, API endpoint

**After Day 6**: Do animations run at 60 FPS?
- YES → Continue optimization
- NO → Profile to find bottleneck (geometry count? shader? state updates?)

**After Day 8**: Works on all three platforms?
- YES → Proceed to UI
- NO → Add fallbacks or simplifications for problem platform

---

## Testing Schedule

| Phase | Unit Tests | Integration | E2E | Total |
|-------|-----------|-------------|-----|-------|
| Day 1-2 | 15 | 0 | 0 | 15 |
| Day 3-4 | 18 | 30 | 0 | 48 |
| Day 5-6 | 25 | 30 | 0 | 55 |
| Day 7-8 | 20 | 35 | 0 | 55 |
| Day 9-10 | 10 | 20 | 40 | 70 |
| **TOTAL** | **88** | **115** | **40** | **243** |

**Target Coverage**: 90%+ across all modules

---

## Dependency Map

```
┌─────────────┐
│ Phase 4.W1  │  (Complete: Control + Results + API)
│ Emulator UI │
└──────┬──────┘
       │
       ▼
┌──────────────────────────┐
│ Phase 4.W2 Prerequisites │
├──────────────────────────┤
│ ✓ Backend API endpoints  │
│ ✓ WebSocket ready        │
│ ✓ State export format    │
└──────────────┬───────────┘
               │
    ┌──────────┴────────────┐
    ▼                       ▼
┌──────────────┐    ┌──────────────┐
│ Day 1-2      │    │ Day 3-4      │
│ Foundation   │    │ State Sync   │
│ (rendering   │    │ (WebSocket)  │
│  pipeline)   │    │              │
└──────────────┘    └──────────────┘
    │                       │
    └───────────┬───────────┘
                ▼
            ┌──────────────┐
            │ Day 5-6      │
            │ Animation    │
            │ (shaders)    │
            └──────┬───────┘
                   │
         ┌─────────┴──────────┐
         ▼                    ▼
    ┌─────────┐        ┌─────────────┐
    │ Day 7-8 │        │ Day 9-10    │
    │Perf & X │        │ UI & Polish │
    │Platform │        │ (final tests)
    └─────────┘        └─────────────┘
         │                    │
         └────────┬───────────┘
                  ▼
        ┌────────────────────┐
        │ Phase 4.W2 COMPLETE │
        │ Ready for W3        │
        └────────────────────┘
```

---

## Definition of Done (Phase 4.W2)

**Code Complete**:
- ✅ All 4,400 lines written and in version control
- ✅ All modules have JSDoc documentation
- ✅ No TypeScript errors (strict mode)
- ✅ No console.warn or console.error in production code

**Tests Complete**:
- ✅ 88+ unit tests passing
- ✅ 115+ integration tests passing
- ✅ 40+ E2E tests passing
- ✅ 90%+ code coverage
- ✅ Performance baselines established

**Quality Assurance**:
- ✅ Runs on macOS 11+ (Metal/WebGL 2.0)
- ✅ Runs on Windows 10+ (DirectX/WebGL 2.0)
- ✅ Runs on Linux (OpenGL/WebGL 2.0)
- ✅ 60 FPS on RTX 3060/GTX 1080/M1 (measured)
- ✅ < 100 MB GPU VRAM usage
- ✅ Mobile responsive (iPad/Android tablets)

**Integration**:
- ✅ WebSocket sync with Phase 4.W1 API
- ✅ Emulator control UI works with 3D viz
- ✅ Breakpoints visible in visualization
- ✅ Debugger variables highlighted

**Documentation**:
- ✅ PHASE_4_W2_COMPLETION.md (final summary)
- ✅ API documentation for visualization system
- ✅ Shader documentation
- ✅ Performance analysis report

---

## Success Metrics

**Technical**:
- 60+ FPS sustained (5-minute test)
- 0 memory leaks (24-hour test)
- 0 console errors/warnings
- 90%+ test coverage

**User Experience**:
- 3D visualization intuitive to understand
- Camera controls responsive (< 16ms input latency)
- Smooth animations (no jank or stuttering)
- Clear visual of carry propagation

**Educational**:
- Mechanical phases visible and labeled
- Column values readable in real-time
- Carry mechanism clearly demonstrated
- Historical accuracy maintained

---

## Sign-Off Checklist

- [ ] Design reviewed and approved
- [ ] Architecture validated
- [ ] Day 1-2 Foundation complete
- [ ] Day 3-4 State sync complete
- [ ] Day 5-6 Animation complete
- [ ] Day 7-8 Performance complete
- [ ] Day 9-10 Polish complete
- [ ] All tests passing
- [ ] Performance benchmarks met
- [ ] Cross-platform testing complete
- [ ] Documentation complete
- [ ] Ready for Phase 4.W3

---

**Next Document**: PHASE_4_W2_TECHNICAL_SPECS.md (detailed module specifications)

**Status**: ✅ ROADMAP COMPLETE - Ready for implementation start

**Estimated Total Effort**: 160 person-hours (10 days × 16 hours/day development + testing)
