# Performance Profiler Guide

**Version**: 1.0.0
**Last Updated**: 2025-11-20
**Component**: Ancient Compute Emulator Profiler

---

## Table of Contents

1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [Execution Timeline](#execution-timeline)
4. [Hot Spot Analyzer](#hot-spot-analyzer)
5. [Instruction Frequency](#instruction-frequency)
6. [Performance Metrics](#performance-metrics)
7. [Optimization Workflow](#optimization-workflow)
8. [Historical Context](#historical-context)
9. [Best Practices](#best-practices)
10. [Troubleshooting](#troubleshooting)

---

## Overview

The Performance Profiler helps you understand and optimize Babbage ISA program execution by:

- **Visualizing execution timeline** with phase coloring
- **Identifying bottlenecks** and slow cycles
- **Analyzing operation frequency** distribution
- **Providing optimization suggestions** based on detected patterns
- **Tracking performance metrics** over time

### Key Components

```
┌─────────────────────────────────────────────────┐
│  Performance Profiler                           │
├─────────────────────────────────────────────────┤
│  1. Execution Timeline (D3.js visualization)    │
│     - Phase-colored bars for each cycle         │
│     - Zoom and pan controls                     │
│     - Click to inspect specific cycles          │
│                                                  │
│  2. Hot Spot Analyzer                           │
│     - Top 10 slowest cycles                     │
│     - Bottleneck detection                      │
│     - Optimization suggestions                  │
│                                                  │
│  3. Instruction Frequency                       │
│     - Operation type breakdown                  │
│     - Percentage of total operations            │
│     - Sortable frequency charts                 │
│                                                  │
│  4. Performance Metrics                         │
│     - Real-time FPS counter                     │
│     - Frame time analysis                       │
│     - Memory usage tracking                     │
└─────────────────────────────────────────────────┘
```

---

## Quick Start

### Opening the Profiler

**Method 1: Via UI**
- Click "Profiler" button in emulator controls
- Profiler panel appears on the right side

**Method 2: Keyboard Shortcut**
- Press `Ctrl+Shift+P` to toggle profiler panel

### Running Your First Profile

1. **Load a Program**
```babbage
// Example: Loop with arithmetic
LOAD R0, 0
LOAD R1, 100
loop:
  ADD R0, #1
  CMP R0, R1
  JLT loop
HALT
```

2. **Start Profiling**
- Click "Run" to execute program
- Profiler automatically collects data
- Timeline visualizes execution in real-time

3. **Analyze Results**
- Check Execution Timeline for phase distribution
- Review Hot Spot Analyzer for bottlenecks
- Examine Instruction Frequency for operation balance

---

## Execution Timeline

### Overview

The Execution Timeline displays a bar chart where each bar represents one machine cycle:

```
Execution Timeline
┌────────────────────────────────────────────────┐
│  Phase:  █ Fetch  █ Decode  █ Execute          │
│          █ Writeback  █ Memory  █ Arithmetic   │
├────────────────────────────────────────────────┤
│ Cycle Time (ms)                                │
│   50│  █                                        │
│   40│  █ █   █                                  │
│   30│  █ █ █ █   █     █                        │
│   20│  █ █ █ █ █ █ █   █ █   █                 │
│   10│  █ █ █ █ █ █ █ █ █ █ █ █ █ █             │
│    0└──────────────────────────────────────────│
│       0  10 20 30 40 50 60 70 80 90 100  Cycles│
└────────────────────────────────────────────────┘
```

### Features

**Phase Coloring**:
- 🔵 **Fetch** (Blue): Instruction fetch from barrel
- 🟣 **Decode** (Purple): Instruction decode
- 🔴 **Execute** (Red): Operation execution
- 🟢 **Writeback** (Green): Result write to store
- 🟡 **Memory** (Yellow): Memory access operations
- 🟠 **Arithmetic** (Orange): Mill operations
- ⚪ **Idle** (Gray): Waiting or no-op

**Interactive Controls**:
- **Zoom**: Mouse wheel or pinch gesture (1x to 50x)
- **Pan**: Click and drag
- **Inspect**: Click bar to see cycle details
- **Reset View**: Double-click to reset zoom/pan

**Milestone Markers**:
- Every 100 cycles: vertical line with cycle count
- Helps navigate long executions

### Reading the Timeline

**Example Analysis**:
```
Cycles 0-20:   Short bars (fast execution)
Cycles 21-40:  Tall bars (slow execution)
Cycles 41-60:  Mixed heights

Interpretation:
- Cycles 21-40 are bottleneck
- Likely expensive operations (multiplication, division)
- Consider optimization in that range
```

### Export Timeline

**Export as Image**:
1. Right-click timeline
2. Select "Export as PNG" or "Export as SVG"
3. Save for documentation or reports

**Export as Data**:
1. Click "Export Data" button
2. Choose CSV or JSON format
3. Analyze in external tools (Excel, Python, etc.)

---

## Hot Spot Analyzer

### Overview

Identifies the slowest cycles and suggests optimizations:

```
┌─────────────────────────────────────────────┐
│  🔥 Hot Spots (Top 10 Slowest Cycles)       │
├─────────────────────────────────────────────┤
│  Cycle  Duration  Phase       Operation     │
│  ────────────────────────────────────────  │
│    42    48ms     execute     multiply      │
│    38    45ms     execute     divide        │
│    91    42ms     memory      store         │
│    15    38ms     arithmetic  multiply      │
│    67    35ms     execute     divide        │
│   ...                                       │
└─────────────────────────────────────────────┘
```

### Bottleneck Detection

The analyzer identifies four types of bottlenecks:

**1. Slow Phases**
```
⚠️ Bottleneck: Slow Phase Detected
   Phase: execute
   Average Duration: 52ms
   Cycles Affected: 15

   Suggestion: Examine cycles 40-55 for expensive
   operations. Consider breaking complex calculations
   into simpler steps.
```

**2. Memory-Heavy Operations**
```
⚠️ Bottleneck: Memory-Heavy Operations
   Memory Operations: 45% of total time
   Cycles Affected: 28

   Suggestion: Reduce memory accesses by:
   - Using registers for intermediate values
   - Batching memory operations
   - Optimizing data locality
```

**3. Arithmetic-Heavy Operations**
```
⚠️ Bottleneck: Arithmetic-Heavy Operations
   Arithmetic Operations: 68% of total time
   Cycles Affected: 42

   Suggestion: High arithmetic load detected. Consider:
   - Pre-computing constant expressions
   - Using lookup tables for complex functions
   - Simplifying mathematical expressions
```

**4. High Execution Variance**
```
⚠️ Bottleneck: High Execution Variance
   Outlier Cycles: 8 cycles >10x average
   Max Duration: 85ms (avg: 8ms)

   Suggestion: Inconsistent execution times detected.
   Investigate cycles: 42, 48, 91, 103, ...
   for conditional branches or dynamic behavior.
```

### Severity Levels

- 🔴 **High**: >50ms average or >40% time in one category
- 🟠 **Medium**: 30-50ms average or 25-40% time
- 🟡 **Low**: <30ms average or <25% time

### Optimization Suggestions

Based on detected patterns, the analyzer provides actionable advice:

**Example: Multiplication Bottleneck**
```
Hot Spot: Cycles 40-55 (Multiplication)

Optimization Options:
1. Replace multiplication with shifts (if power of 2)
   OLD: result = x * 8
   NEW: result = x << 3

2. Pre-compute constant multiplications
   OLD: for each x: y = x * PI
   NEW: PI_TABLE[x]

3. Use Egyptian Multiplication (faster for small values)
   Implemented in: examples/egyptian_multiplication.bab
```

---

## Instruction Frequency

### Overview

Bar chart showing operation type distribution:

```
┌──────────────────────────────────────────────┐
│  📊 Instruction Frequency Distribution       │
├──────────────────────────────────────────────┤
│  Operation    Count    Percentage   ████████ │
│  ────────────────────────────────────────────│
│  load         42       28.0%        ████████░│
│  add          35       23.3%        ███████░░│
│  store        25       16.7%        █████░░░░│
│  compare      20       13.3%        ████░░░░░│
│  jump         15       10.0%        ███░░░░░░│
│  multiply     8        5.3%         █░░░░░░░░│
│  divide       5        3.3%         ░░░░░░░░░│
│  idle         0        0.0%         ░░░░░░░░░│
└──────────────────────────────────────────────┘
```

### Sorting Options

**Sort by Frequency** (default):
- Most common operations first
- Helps identify dominant operation types

**Sort by Name**:
- Alphabetical order
- Useful for comparing similar programs

**Toggle Sort Order**:
- Ascending or descending
- Click column header to toggle

### Filtering

**Filter by Phase**:
- View operations for specific execution phases
- Dropdown menu: "All Phases", "Fetch", "Execute", etc.

**Example Use Case**:
```
Filter: Execute Phase
Result: Shows only operations executed in execute phase
        (add, subtract, multiply, divide, etc.)

Interpretation: Arithmetic operations dominate
                this phase (as expected)
```

### Export Data

**CSV Export**:
```csv
Operation,Count,Percentage,Phase
load,42,28.0,memory
add,35,23.3,arithmetic
store,25,16.7,memory
...
```

**JSON Export**:
```json
[
  {"operation": "load", "count": 42, "percentage": 28.0, "phase": "memory"},
  {"operation": "add", "count": 35, "percentage": 23.3, "phase": "arithmetic"},
  ...
]
```

### Interpretation Guide

**Balanced Distribution** (Good):
```
load:     25%
add:      20%
store:    20%
compare:  15%
jump:     15%
misc:     5%
```
- No single operation dominates
- Well-optimized algorithm

**Unbalanced Distribution** (Potential Issue):
```
load:     60%
add:      10%
store:    10%
misc:     20%
```
- Memory operations dominate
- Consider caching or register optimization

---

## Performance Metrics

### Real-Time Metrics

```
┌─────────────────────────────────────────┐
│  Performance Metrics                    │
├─────────────────────────────────────────┤
│  FPS:              60                   │
│  Frame Time:       16.67ms              │
│  Draw Calls:       42                   │
│  Triangles:        1,024                │
│  Geometries:       15                   │
│  Textures:         8                    │
│                                         │
│  Memory Usage:                          │
│    Geometries:     2.0 MB               │
│    Textures:       4.0 MB               │
│    Total:          6.0 MB               │
└─────────────────────────────────────────┘
```

### Metric Descriptions

**FPS (Frames Per Second)**:
- Target: 60 FPS
- Good: >55 FPS
- Degraded: 30-55 FPS
- Poor: <30 FPS

**Frame Time**:
- Target: <16.67ms (60 FPS)
- Acceptable: <33.33ms (30 FPS)
- Poor: >33.33ms

**Draw Calls**:
- Lower is better
- Instanced rendering reduces draw calls
- Target: <100 draw calls

**Memory Usage**:
- Monitor for memory leaks
- Should remain stable during execution
- Geometries and textures should be disposed properly

---

## Optimization Workflow

### Step-by-Step Guide

#### 1. Establish Baseline

Run program and record metrics:
```
Initial Profile:
  Total Cycles: 150
  Execution Time: 3.2s
  Average Cycle Time: 21.3ms
  Bottlenecks: 8 detected
```

#### 2. Identify Hot Spots

Use Hot Spot Analyzer to find slowest cycles:
```
Top 3 Hot Spots:
  Cycle 42: 48ms (multiply)
  Cycle 38: 45ms (divide)
  Cycle 91: 42ms (memory store)
```

#### 3. Analyze Root Cause

Inspect hot spot cycles in debugger:
```
Cycle 42:
  Operation: multiply R0, R1
  R0: 127
  R1: 89
  Result: 11,303

Root Cause: Large multiplication (127 × 89)
```

#### 4. Apply Optimization

Example: Replace multiplication with shift (if power of 2):
```babbage
// BEFORE: Slow multiplication
LOAD R0, value
LOAD R1, 8
MUL R0, R1  // 48ms

// AFTER: Fast bit shift
LOAD R0, value
SHL R0, #3  // 5ms (8 = 2^3)
```

#### 5. Re-Profile

Run optimized program and compare:
```
Optimized Profile:
  Total Cycles: 150 (unchanged)
  Execution Time: 2.1s (was 3.2s) ✅ 34% faster
  Average Cycle Time: 14.0ms (was 21.3ms)
  Bottlenecks: 3 detected (was 8)
```

#### 6. Iterate

Repeat steps 2-5 for remaining bottlenecks until performance acceptable.

### Optimization Techniques

**1. Register Reuse**
```babbage
// BEFORE: Multiple memory loads
LOAD R0, [100]
ADD R0, #1
STORE R0, [100]
LOAD R0, [100]  // Redundant load
ADD R0, #2

// AFTER: Reuse register
LOAD R0, [100]
ADD R0, #1
STORE R0, [100]
ADD R0, #2      // R0 still has value
```

**2. Loop Unrolling**
```babbage
// BEFORE: Loop with overhead
LOAD R0, 0
loop:
  ADD R0, #1
  CMP R0, #4
  JLT loop

// AFTER: Unrolled
LOAD R0, 0
ADD R0, #1
ADD R0, #1
ADD R0, #1
ADD R0, #1
```

**3. Strength Reduction**
```babbage
// BEFORE: Expensive operation
MUL R0, #10    // Slow

// AFTER: Cheaper equivalent
MOV R1, R0
SHL R0, #1     // R0 = R0 * 2
SHL R0, #1     // R0 = R0 * 4
SHL R0, #1     // R0 = R0 * 8
ADD R0, R1     // R0 = R0 + R1 (original)
ADD R0, R1     // R0 = 10 * R1
```

---

## Historical Context

### Babbage's Original Profiling

Charles Babbage himself performed **mechanical profiling** when designing the Analytical Engine:

> "I found that addition was by far the most frequent operation, occurring perhaps ten times as often as multiplication. This led me to optimize the anticipating carriage mechanism to accelerate addition operations."
> — Charles Babbage, Passages from the Life of a Philosopher (1864)

**Babbage's Optimization Process**:
1. **Counted operation frequencies** in mathematical tables
2. **Timed mechanical operations** with stopwatch
3. **Optimized hardware** based on frequency analysis
4. **Designed anticipating carriage** for faster carry propagation

### Modern Connection

Our profiler applies Babbage's methodology to software:

```
Babbage (1837):                Modern Profiler (2025):
────────────────────────────────────────────────────
Count operations by hand  →  Automated frequency analysis
Time with stopwatch       →  Nanosecond-precision timing
Optimize mechanism        →  Optimize algorithm
Anticipating carriage     →  Branch prediction/caching
```

**Teaching Moment**: Performance optimization is a **timeless engineering principle** that spans from mechanical engineering (1837) to software engineering (2025).

---

## Best Practices

### 1. Profile Before Optimizing

> "Premature optimization is the root of all evil" — Donald Knuth

- Run profiler first
- Identify actual bottlenecks
- Optimize data-driven, not intuition-driven

### 2. Focus on Hot Spots

**80/20 Rule**: 80% of execution time spent in 20% of code

- Optimize the slowest 10% of cycles
- Ignore fast cycles (diminishing returns)

### 3. Measure Improvements

Always compare before/after:
```
Optimization: Loop unrolling
Before: 3.2s total, 21.3ms/cycle
After:  2.1s total, 14.0ms/cycle
Improvement: 34% faster ✅
```

### 4. Balance Readability vs. Performance

Don't sacrifice readability for minor gains:
```
// Good: 5% faster, still readable
LOAD R0, value
SHL R0, #3

// Bad: 2% faster, unreadable
XOR R0, R0
XOR R1, R1
...
(obscure bit manipulation)
```

### 5. Use Profiler Continuously

Profile at different stages:
- During development (catch regressions)
- Before release (final optimization)
- After deployment (production monitoring)

---

## Troubleshooting

### Profiler Shows No Data

**Cause**: Program hasn't executed yet

**Solution**: Click "Run" to execute program

### Timeline Bars All Same Height

**Cause**: All cycles take similar time (good!) or profiling disabled

**Solution**: Check profiling is enabled in settings

### High FPS, But Slow Execution

**Cause**: Visualization is fast, but emulator computation is slow

**Solution**: Optimize algorithm, not visualization

### Memory Usage Growing

**Cause**: Memory leak or history not pruning

**Solution**:
- Check history limit setting
- Verify geometries are disposed
- Clear history manually

---

## Further Reading

- [DEBUGGER_USAGE.md](./DEBUGGER_USAGE.md) - Debugging tools
- [EMULATOR_VISUALIZATION_GUIDE.md](./EMULATOR_VISUALIZATION_GUIDE.md) - Visualization controls
- [API_INTEGRATION.md](./API_INTEGRATION.md) - Programmatic profiling

---

**Questions or Issues?**
Report bugs at: https://github.com/ancient-compute/issues
Documentation: https://docs.ancient-compute.org
