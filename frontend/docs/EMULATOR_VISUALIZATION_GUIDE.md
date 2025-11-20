# Emulator Visualization Guide

**Version**: 1.0.0
**Last Updated**: 2025-11-20
**Component**: Ancient Compute Emulator Visualization

---

## Table of Contents

1. [Overview](#overview)
2. [Visualization Modes](#visualization-modes)
3. [3D Visualization (Three.js)](#3d-visualization-threejs)
4. [2D Visualization (Canvas2D)](#2d-visualization-canvas2d)
5. [Camera Controls](#camera-controls)
6. [Quality Settings](#quality-settings)
7. [Theme Options](#theme-options)
8. [Performance Tips](#performance-tips)
9. [Screenshot Capture](#screenshot-capture)
10. [Troubleshooting](#troubleshooting)

---

## Overview

The emulator provides **two visualization modes** for viewing the Babbage Analytical Engine:

```
┌──────────────────────────────────────────────────────┐
│  Visualization System Architecture                   │
├──────────────────────────────────────────────────────┤
│                                                       │
│  ┌─────────────┐         ┌──────────────┐           │
│  │  Provider   │◄────────│  Auto-Detect │           │
│  │   Factory   │         │  Capabilities│           │
│  └──────┬──────┘         └──────────────┘           │
│         │                                            │
│    ┌────┴─────┐                                      │
│    │          │                                      │
│ ┌──▼──────┐ ┌─▼────────┐                            │
│ │ Three.js│ │Canvas2D  │                            │
│ │  (3D)   │ │  (2D)    │                            │
│ │         │ │          │                            │
│ │ WebGL   │ │ Fallback │                            │
│ │ Render  │ │  Render  │                            │
│ └─────────┘ └──────────┘                            │
│                                                       │
└──────────────────────────────────────────────────────┘
```

### Automatic Provider Selection

The system automatically selects the best visualization provider based on:

- **WebGL Support**: Does browser support WebGL 2.0?
- **GPU Capabilities**: GPU tier (1-3 based on performance)
- **Device Type**: Desktop vs. mobile
- **User Preference**: Manual override in settings

**Selection Logic**:
```
if (WebGL 2.0 supported && GPU Tier >= 2 && !mobile):
    use Three.js (3D)
else:
    use Canvas2D (2D fallback)
```

---

## Visualization Modes

### Mode Comparison

| Feature | Three.js (3D) | Canvas2D (2D) |
|---------|---------------|---------------|
| Visual Quality | High (realistic 3D) | Medium (schematic 2D) |
| Performance | 60 FPS (good GPU) | 60 FPS (any device) |
| Requirements | WebGL 2.0 | HTML5 Canvas |
| GPU Usage | High | Low |
| Battery Impact | Higher | Lower |
| Mobile Support | Limited | Full |
| Accessibility | Lower | Higher |

### When to Use Each Mode

**Use Three.js (3D) When**:
- Desktop with good GPU
- Educational demonstration (visual impact)
- Debugging mechanical operations
- Presenting to audience

**Use Canvas2D (2D) When**:
- Mobile device
- Low-end hardware
- Battery saving needed
- Accessibility required
- Long execution sessions

### Switching Modes

**Via Settings Panel**:
1. Click ⚙️ (Settings) button
2. Select "Provider" dropdown
3. Choose:
   - `Auto-detect` (recommended)
   - `Three.js (3D)` (force 3D)
   - `Canvas2D (2D)` (force 2D)

**Via Keyboard**:
- Press `Ctrl+,` to open settings
- Use arrow keys to navigate
- Press Enter to confirm

---

## 3D Visualization (Three.js)

### Victorian Engine Rendering

The 3D visualization shows a **historically accurate** representation of Babbage's Analytical Engine:

```
        ┌─────────────────────────────────────┐
        │   Babbage Analytical Engine (3D)    │
        ├─────────────────────────────────────┤
        │                                     │
        │    ╔═══════════════════════════╗    │
        │    ║   Control Barrels (3x)    ║    │
        │    ║   [Barrel 0] [1] [2]      ║    │
        │    ╚═══════════════════════════╝    │
        │                                     │
        │    ╔═══════════════════════════╗    │
        │    ║   The Mill (Arithmetic)   ║    │
        │    ║   ⚙️ Gears rotating...     ║    │
        │    ╚═══════════════════════════╝    │
        │                                     │
        │    ╔═══════════════════════════╗    │
        │    ║   The Store (Memory)      ║    │
        │    ║   █ █ █ █ █ █ █ █ █ █    ║    │
        │    ║   10 Digit Columns        ║    │
        │    ╚═══════════════════════════╝    │
        │                                     │
        └─────────────────────────────────────┘
```

### Visual Elements

**1. Digit Columns (Store)**:
- 10 vertical brass cylinders
- Each displays 0-9 value
- Rotate during calculations
- Glow when active (green highlight)

**2. The Mill (Arithmetic Unit)**:
- Central gear mechanism
- Visible gears rotate during operations
- Different speeds for add/multiply/divide
- Result displayed on output dial

**3. Control Barrels**:
- 3 cylindrical barrels with pegs
- Represents program instructions
- Rotate as program executes
- Active barrel highlighted

**4. Carry Mechanism**:
- Animated linkages between columns
- Visible when carry propagates
- Historically accurate "anticipating carriage"

**5. Lighting**:
- Ambient Victorian-era warm lighting
- Spotlights on active components
- Shadows for depth perception
- Optional: Dynamic lighting effects

### Material Rendering

**Brass Components**:
- Physically-based rendering (PBR)
- Metallic: 0.9
- Roughness: 0.3
- Reflections from environment

**Wooden Base**:
- Oak texture
- Bump mapping for grain
- Matte finish

**Labels and Annotations**:
- 3D text labels
- Hover to show details
- Toggle in settings

---

## 2D Visualization (Canvas2D)

### Schematic View

The 2D visualization shows a **functional schematic** of the engine:

```
┌──────────────────────────────────────────┐
│  Analytical Engine - Schematic View     │
├──────────────────────────────────────────┤
│                                          │
│  Program Counter: [===>      ] 42/100   │
│                                          │
│  Registers:                              │
│  ┌──────┬──────┬──────┬──────┐          │
│  │  R0  │  R1  │  R2  │  R3  │          │
│  │  15  │   5  │  20  │   0  │          │
│  └──────┴──────┴──────┴──────┘          │
│                                          │
│  Mill:                                   │
│  ┌────────────────────────────┐         │
│  │  Operation: ADD             │         │
│  │  10 + 5 = 15                │         │
│  │  Progress: ████████░░ 80%   │         │
│  └────────────────────────────┘         │
│                                          │
│  Store (Columns):                        │
│  ┌─┬─┬─┬─┬─┬─┬─┬─┬─┬─┐                 │
│  │5│0│7│0│0│3│0│0│0│0│                 │
│  └─┴─┴─┴─┴─┴─┴─┴─┴─┴─┘                 │
│  └─Active                                │
│                                          │
└──────────────────────────────────────────┘
```

### Visual Elements

**1. Data Flow Diagram**:
- Arrows show data movement
- Animated flow during execution
- Color-coded by operation type

**2. State Indicators**:
- Green: Active/Running
- Yellow: Paused
- Red: Error
- Gray: Idle

**3. Compact Layout**:
- All components visible at once
- No camera controls needed
- Optimized for small screens

**4. Text Readability**:
- High contrast colors
- Clear fonts (Courier New)
- Adjustable text size

---

## Camera Controls

### 3D Camera (Three.js Only)

**Orbit Mode** (default):
- **Rotate**: Left-click + drag
- **Pan**: Right-click + drag (or Shift + Left-click)
- **Zoom**: Mouse wheel (or pinch on trackpad)

**Free Camera Mode**:
- **Move**: W/A/S/D keys
- **Look**: Mouse movement
- **Up/Down**: Space/Ctrl
- **Speed**: Shift (faster) / Alt (slower)

**Presets**:
- `1` key: Front view
- `2` key: Side view
- `3` key: Top view
- `4` key: Isometric view
- `0` key: Reset to default

### Camera Settings

**Field of View (FOV)**:
- Default: 50°
- Range: 30° (telephoto) - 90° (wide)
- Adjust in settings panel

**Near/Far Clipping**:
- Near: 0.1 units
- Far: 1000 units
- Auto-adjusted for scene bounds

**Auto-Frame**:
- Press `F` to frame all objects
- Useful after manual camera movement
- Maintains aspect ratio

---

## Quality Settings

### Quality Levels

**Low Quality**:
- Resolution: 720p
- Shadows: Disabled
- Anti-aliasing: None
- LOD: Aggressive
- Target: 60 FPS on low-end GPUs

**Medium Quality** (default):
- Resolution: 1080p
- Shadows: Simple
- Anti-aliasing: FXAA
- LOD: Moderate
- Target: 60 FPS on mid-range GPUs

**High Quality**:
- Resolution: 1440p+
- Shadows: High-quality
- Anti-aliasing: MSAA 4x
- LOD: Minimal
- Target: 60 FPS on high-end GPUs

### Performance Impact

| Setting | FPS Impact | Visual Impact | GPU Load |
|---------|------------|---------------|----------|
| Resolution | -30% | High | High |
| Shadows | -20% | Medium | Medium |
| Anti-aliasing | -15% | Low | Low |
| LOD | -10% | Medium | Medium |

### Adjusting Quality

1. Open Settings (Ctrl+,)
2. Select Quality dropdown
3. Choose Low/Medium/High
4. Changes apply immediately

**Auto-Quality**:
- Enable "Auto-adjust quality" in settings
- System monitors FPS
- Reduces quality if FPS drops below 30
- Increases quality if FPS stable >55

---

## Theme Options

### Victorian Theme (default)

**Colors**:
- Brass: #B5A642
- Wood: #8B4513
- Background: #1A1A1A
- Accent: #D4AF37 (gold)

**Materials**:
- Polished brass with patina
- Aged oak wood
- Victorian-era labels
- Gas lamp lighting effect

### Modern Theme

**Colors**:
- Metal: #C0C0C0 (silver)
- Plastic: #FFFFFF
- Background: #2A2A2A
- Accent: #4CAF50 (green)

**Materials**:
- Brushed aluminum
- Matte plastic
- Modern sans-serif fonts
- LED lighting

### Schematic Theme

**Colors**:
- Lines: #00FF00 (green)
- Background: #000000 (black)
- Highlights: #FFFF00 (yellow)
- Text: #00FFFF (cyan)

**Style**:
- Line-based rendering
- Minimal 3D depth
- Blueprint aesthetic
- Grid overlay

---

## Performance Tips

### Optimizing 3D Performance

**1. Reduce Draw Calls**:
- Enable "Instanced Rendering" in settings
- Merges similar geometries
- Reduces draw calls from 200+ to <50

**2. Use Level of Detail (LOD)**:
- Enable "Automatic LOD" in settings
- Distant objects use simpler geometry
- Near objects: High detail
- Far objects: Low detail

**3. Limit FPS**:
- Cap FPS at 30 if battery saving needed
- Settings > Max FPS > 30
- Reduces GPU load by 50%

**4. Disable Shadows**:
- Settings > Shadows > Off
- Significant performance improvement
- Minimal visual impact for debugging

### Optimizing 2D Performance

**1. Reduce Canvas Size**:
- Settings > Resolution Scale > 0.5x
- Renders at half resolution, scales up
- 2x performance improvement

**2. Disable Animations**:
- Settings > Animations > Essential Only
- Only animates active components
- Reduces CPU usage

**3. Batch State Updates**:
- Automatically enabled
- Groups multiple state changes
- Reduces re-renders

---

## Screenshot Capture

### Taking Screenshots

**Method 1: Button**
- Click 📷 (Camera) button in header
- Screenshot saved automatically
- Filename: `babbage-emulator-TIMESTAMP.png`

**Method 2: Keyboard**
- Press `Ctrl+Shift+S`
- Same behavior as button

### Screenshot Settings

**Resolution**:
- Default: Current canvas size
- Override: Settings > Screenshot Resolution
- Options: 720p, 1080p, 4K

**Format**:
- PNG (default): Lossless, large file
- JPEG: Lossy, smaller file
- SVG (2D only): Vector, scalable

**Include Annotations**:
- Toggle in settings
- Shows labels, values, cycle count
- Useful for documentation

---

## Troubleshooting

### Black Screen (3D Mode)

**Cause**: WebGL not supported or crashed

**Solutions**:
1. Check browser supports WebGL 2.0
2. Update graphics drivers
3. Try different browser (Chrome recommended)
4. Switch to Canvas2D mode

### Low FPS (<30)

**Cause**: GPU overloaded or low-end hardware

**Solutions**:
1. Reduce quality to Low
2. Disable shadows
3. Enable Auto-Quality
4. Switch to Canvas2D mode
5. Close other GPU-intensive applications

### Objects Not Visible

**Cause**: Camera clipping or outside view frustum

**Solutions**:
1. Press `F` to frame all objects
2. Reset camera (press `0`)
3. Check quality settings (LOD might be too aggressive)

### Blurry Rendering

**Cause**: Resolution scaling or DPI mismatch

**Solutions**:
1. Settings > Resolution Scale > 1.0x
2. Disable browser zoom (Ctrl+0)
3. Check display scaling (Windows: 100%)

### Memory Leak (Increasing RAM Usage)

**Cause**: Geometries not disposed properly

**Solutions**:
1. Restart emulator
2. Settings > History Limit > 50 (reduce)
3. Clear history manually
4. Report bug if persists

---

## Historical Accuracy

### Babbage's Original Design

Our 3D visualization is based on:

- **Science Museum Plans** (1991 reconstruction)
- **Babbage's Technical Drawings** (1840s)
- **Modern Reconstructions** (Doron Swade, 2002)

**Accurate Elements**:
✅ 10 digit columns (as Babbage specified)
✅ Anticipating carriage mechanism
✅ 3 control barrels
✅ Mill gear ratios
✅ Store capacity (1000 numbers)

**Simplified Elements**:
- ⚠️ Full engine would have 25,000+ parts (we show ~500)
- ⚠️ Size scaled down for visibility
- ⚠️ Some mechanical details abstracted

### Educational Value

The visualization helps students understand:

1. **Mechanical Computation**: How gears perform arithmetic
2. **Program Control**: How barrels encode instructions
3. **Memory Organization**: Column-based number storage
4. **Carry Propagation**: Anticipating carriage mechanism

---

## Further Reading

- [DEBUGGER_USAGE.md](./DEBUGGER_USAGE.md) - Debugging tools
- [PROFILER_GUIDE.md](./PROFILER_GUIDE.md) - Performance analysis
- [API_INTEGRATION.md](./API_INTEGRATION.md) - Programmatic control

---

**Questions or Issues?**
Report bugs at: https://github.com/ancient-compute/issues
Documentation: https://docs.ancient-compute.org
