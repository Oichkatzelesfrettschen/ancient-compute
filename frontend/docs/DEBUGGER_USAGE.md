# Debugger Usage Guide

**Version**: 1.0.0
**Last Updated**: 2025-11-20
**Component**: Ancient Compute Emulator Debugger

---

## Table of Contents

1. [Overview](#overview)
2. [Quick Start](#quick-start)
3. [Debugger Panel](#debugger-panel)
4. [Breakpoints](#breakpoints)
5. [Step Execution](#step-execution)
6. [Watch Expressions](#watch-expressions)
7. [State Inspection](#state-inspection)
8. [Time-Travel Debugging](#time-travel-debugging)
9. [Keyboard Shortcuts](#keyboard-shortcuts)
10. [Advanced Features](#advanced-features)
11. [Troubleshooting](#troubleshooting)
12. [Best Practices](#best-practices)

---

## Overview

The Ancient Compute Emulator includes a powerful debugging system that allows you to:

- **Set breakpoints** at specific cycles or conditions
- **Step through execution** one cycle at a time
- **Watch variables** and expressions in real-time
- **Inspect state** at any point in execution
- **Travel back in time** to examine how the machine reached a state
- **Analyze performance** bottlenecks

### Key Features

- ✅ **Cycle-accurate debugging** - Examine state after each machine cycle
- ✅ **Time-travel capability** - Rewind to any previous state
- ✅ **Watch expressions** - Monitor specific values continuously
- ✅ **Conditional breakpoints** - Break only when conditions are met
- ✅ **State diff visualization** - See what changed between cycles
- ✅ **Performance profiling** - Identify slow operations
- ✅ **Keyboard shortcuts** - Fast navigation and control

---

## Quick Start

### 1. Open the Debugger

**Method 1: Via UI**
- Click the "Debugger" button in the emulator controls panel
- The debugger panel will open on the right side

**Method 2: Via Keyboard**
- Press `Ctrl+Shift+D` to toggle the debugger panel

### 2. Load a Program

```babbage
// Example: Simple addition program
LOAD R0, 10
LOAD R1, 5
ADD R0, R1
STORE R0, [100]
HALT
```

### 3. Set a Breakpoint

- Click on the cycle number where you want to break
- Or use the breakpoint button in the debugger panel
- The line will be highlighted in red

### 4. Run Until Breakpoint

- Click **Continue** (or press `F5`)
- Execution will stop at the breakpoint
- The debugger will show the current state

### 5. Step Through Execution

- Click **Step Over** (or press `F10`)
- The machine will execute one cycle
- State will update to reflect changes

---

## Debugger Panel

The debugger panel consists of several sections:

### 1. Control Buttons

```
┌─────────────────────────────────────┐
│  [Continue] [Step] [Step Into]      │
│  [Step Out] [Restart] [Stop]        │
└─────────────────────────────────────┘
```

- **Continue (F5)**: Resume execution until next breakpoint or completion
- **Step Over (F10)**: Execute one cycle and pause
- **Step Into (F11)**: Step into function calls (if applicable)
- **Step Out (Shift+F11)**: Execute until current function returns
- **Restart (Ctrl+R)**: Reset machine to initial state
- **Stop (Shift+F5)**: Stop execution immediately

### 2. Breakpoints List

Shows all active breakpoints:

```
Breakpoints:
  • Cycle 10 (conditional: R0 > 5)
  • Cycle 25
  • Cycle 100 (disabled)
```

**Actions**:
- Click checkbox to enable/disable
- Click trash icon to delete
- Click breakpoint to jump to that cycle (time-travel)

### 3. Watch Expressions

Monitor specific values:

```
Watches:
  R0: 15
  memory[100]: 0x2A
  mill.result: 42
  cycles: 10
```

**Adding a Watch**:
1. Click "+ Add Watch"
2. Enter expression: `R0`, `memory[100]`, `mill.operation`, etc.
3. Press Enter

**Expression Syntax**:
- `R0`, `R1`, `R2`, `R3` - Register values
- `memory[addr]` - Memory at address
- `programCounter` - Current PC
- `accumulator` - Accumulator value
- `mill.operation` - Current operation
- `mill.result` - Last operation result
- `carryFlag` - Carry flag state

### 4. Call Stack

Shows execution context (for programs with functions):

```
Call Stack:
  main() - Cycle 42
  ├─ calculateSum() - Cycle 35
  │  └─ multiply() - Cycle 28
  └─ [Current]
```

### 5. State Inspector

Displays complete machine state:

```
State Inspector:
  Program Counter: 10
  Accumulator: 42
  Carry Flag: false
  Running: true
  Cycles: 10

  Registers:
    R0: 15
    R1: 5
    R2: 20
    R3: 0

  Mill:
    Operation: add
    Operand A: 10
    Operand B: 5
    Result: 15
    Progress: 100%

  Store:
    Capacity: 32 columns
    Active: [0, 1, 5, 10]
    Positions: [...]
```

---

## Breakpoints

### Setting Breakpoints

**Method 1: Click Line Number**
- Click the cycle number in the execution timeline
- A red dot will appear indicating the breakpoint

**Method 2: Via Keyboard**
- Navigate to desired cycle
- Press `Ctrl+B` to toggle breakpoint

**Method 3: Via Context Menu**
- Right-click on cycle number
- Select "Add Breakpoint"

### Conditional Breakpoints

Break only when a condition is true:

1. Right-click breakpoint
2. Select "Edit Breakpoint"
3. Enter condition: `R0 > 10`, `memory[100] == 42`, etc.
4. Click Save

**Example Conditions**:
```javascript
R0 > 10                    // Register comparison
memory[100] == 0x2A        // Memory check
mill.operation == 'mul'    // Operation type
cycles > 1000              // Cycle count
accumulator != 0           // Non-zero accumulator
```

### Managing Breakpoints

**Enable/Disable**:
- Click checkbox next to breakpoint in list
- Disabled breakpoints are shown in gray

**Delete**:
- Click trash icon next to breakpoint
- Or right-click and select "Delete Breakpoint"

**Delete All**:
- Click "Clear All Breakpoints" button at top of list

---

## Step Execution

### Step Over (F10)

Execute the current cycle and pause at the next one:

```
Cycle 10: LOAD R0, 5
         ↓ [Step Over]
Cycle 11: ADD R0, R1
```

**Use When**:
- Examining cycle-by-cycle behavior
- Verifying state changes
- Debugging arithmetic operations

### Step Into (F11)

Step into function calls (if program structure supports it):

```
Cycle 15: CALL calculateSum
         ↓ [Step Into]
Cycle 16: LOAD R0, [R1]  // Inside calculateSum
```

**Use When**:
- Debugging function internals
- Tracing call hierarchy

### Step Out (Shift+F11)

Execute until current function returns:

```
Cycle 25: (inside calculateSum)
         ↓ [Step Out]
Cycle 42: (back in main, after CALL)
```

**Use When**:
- Skipping function implementation details
- Returning to caller quickly

### Continue (F5)

Resume normal execution until:
- Next breakpoint is hit
- Program completes
- Error occurs

---

## Watch Expressions

### Adding Watches

1. Click "+ Add Watch" in Watch panel
2. Enter expression
3. Press Enter

**Supported Expressions**:
```javascript
// Registers
R0, R1, R2, R3

// Memory (direct addressing)
memory[100]
memory[0x64]

// Computed memory addressing
memory[R0]
memory[R0 + 10]

// Machine state
programCounter
accumulator
carryFlag
cycles
timestamp

// Mill state
mill.operation
mill.operandA
mill.operandB
mill.result
mill.progress

// Column state
columns[0].value
columns[0].rotation
columns[0].isActive

// Store state
store.positions[0]
store.capacity
store.activeIndices.length
```

### Watch Updates

Watches update automatically after each step:

```
Watch List:
  R0: 10 → 15  (changed)
  R1: 5        (unchanged)
  memory[100]: 0 → 42  (changed)
```

**Change Indicators**:
- ✅ Green: Value increased
- 🔴 Red: Value decreased
- 🔵 Blue: Value changed (non-numeric)
- ⚪ Gray: Unchanged

### Complex Watches

Use JavaScript expressions:

```javascript
R0 + R1                    // Sum of registers
memory[R0] * 2             // Computed value
mill.operandA - mill.operandB
cycles % 100               // Modulo operation
Math.floor(accumulator / 10)
```

---

## State Inspection

### Register Inspector

View all register values in real-time:

```
┌────────────────────────┐
│  Registers             │
├────────────────────────┤
│  R0:  0x000F  (15)     │
│  R1:  0x0005  (5)      │
│  R2:  0x0014  (20)     │
│  R3:  0x0000  (0)      │
└────────────────────────┘
```

**Features**:
- Hexadecimal and decimal display
- Click to edit (when paused)
- Highlight changed values

### Memory Grid

Browse memory contents:

```
Memory [0x0000 - 0x00FF]:
┌────────────────────────────────────────┐
│ 00: 00 00 00 00 00 00 00 00  2A 00 ... │
│ 10: 00 00 00 00 00 00 00 00  00 00 ... │
│ 20: 0F 05 14 00 00 00 00 00  00 00 ... │
└────────────────────────────────────────┘
```

**Navigation**:
- Scroll to browse memory
- Click address to jump
- Right-click to set watch on address

**Display Modes**:
- Hexadecimal (default)
- Decimal
- ASCII
- Binary

### Mill Inspector

View arithmetic unit state:

```
Mill (Arithmetic Unit):
  Operation: multiply
  Operand A: 10
  Operand B: 5
  Result: 50
  Progress: ████████░░ 80%
  Status: executing
```

---

## Time-Travel Debugging

### Rewinding Execution

The debugger maintains a **history of the last 100 states**.

**Navigate History**:
- Click cycle number in timeline
- Use `Ctrl+Left` to go back one cycle
- Use `Ctrl+Right` to go forward one cycle

**Example Workflow**:
```
1. Run program to completion (Cycle 50)
2. Notice accumulator is incorrect
3. Click Cycle 25 to rewind
4. Examine state at Cycle 25
5. Step forward from Cycle 25 to find bug
```

### State Diff View

Compare current state with previous:

```
Diff: Cycle 24 → Cycle 25
┌─────────────────────────────────────┐
│ Changed:                            │
│  + R0: 10 → 15                      │
│  + accumulator: 10 → 15             │
│  + programCounter: 24 → 25          │
│  + cycles: 24 → 25                  │
│                                     │
│ Unchanged: R1, R2, R3, memory       │
└─────────────────────────────────────┘
```

**Enable Diff View**:
- Click "Show Diff" toggle in State Inspector
- Diff updates automatically when stepping

### History Limits

- Maximum history: **100 states** (configurable)
- Memory usage: ~50KB for 100 states
- Older states automatically pruned

**Adjust History Limit**:
```javascript
// In settings
historyLimit: 50  // Reduce to save memory
historyLimit: 200 // Increase for longer history
```

---

## Keyboard Shortcuts

| Shortcut | Action | Context |
|----------|--------|---------|
| `F5` | Continue execution | Debugger |
| `F10` | Step over | Debugger |
| `F11` | Step into | Debugger |
| `Shift+F11` | Step out | Debugger |
| `Ctrl+B` | Toggle breakpoint | Debugger |
| `Ctrl+Shift+D` | Toggle debugger panel | Global |
| `Ctrl+R` | Restart execution | Debugger |
| `Shift+F5` | Stop execution | Debugger |
| `Ctrl+Left` | Previous cycle (time-travel) | Debugger |
| `Ctrl+Right` | Next cycle (time-travel) | Debugger |
| `Ctrl+/` | Show help | Global |
| `Esc` | Close panels | Global |

---

## Advanced Features

### Performance Profiling Integration

The debugger integrates with the performance profiler:

**Access Profiler**:
- Click "Performance" tab in debugger panel
- Or press `Ctrl+Shift+P`

**Features**:
- Cycle time analysis
- Bottleneck detection
- Operation frequency charts

See [PROFILER_GUIDE.md](./PROFILER_GUIDE.md) for details.

### Custom Breakpoint Actions

Execute code when breakpoint is hit:

```javascript
// Breakpoint action
if (R0 > 10) {
  console.log('R0 exceeded threshold:', R0);
  // Take snapshot
  // Send notification
}
```

### Export Debug Session

Save debugging session for later analysis:

**Export Options**:
- Session history (all states)
- Breakpoint configuration
- Watch expressions
- Execution timeline

**File Format**: JSON
```json
{
  "sessionId": "debug-2025-11-20-123456",
  "program": "...",
  "breakpoints": [...],
  "watches": [...],
  "history": [...]
}
```

---

## Troubleshooting

### Breakpoint Not Hitting

**Possible Causes**:
1. Breakpoint is disabled (check checkbox)
2. Condition is never true (check condition syntax)
3. Program never reaches that cycle (verify program flow)

**Solutions**:
- Enable breakpoint
- Simplify condition
- Add unconditional breakpoint earlier in execution

### Watch Expression Error

**Common Errors**:
- `memory[R0]` when R0 is out of bounds
- Accessing undefined property

**Solutions**:
- Add bounds checking: `R0 < 4096 ? memory[R0] : 'Out of bounds'`
- Use optional chaining: `mill?.operation`

### Slow Step Execution

**Causes**:
- Too many watches
- Complex watch expressions
- Large history buffer

**Solutions**:
- Remove unused watches
- Simplify expressions
- Reduce history limit in settings

---

## Best Practices

### 1. Start with Strategic Breakpoints

Don't breakpoint every cycle. Instead:
- Break at loop entry/exit
- Break at conditional branches
- Break at function calls
- Break where bugs are suspected

### 2. Use Conditional Breakpoints

Avoid stopping unnecessarily:
```javascript
// Instead of breaking every cycle in loop
// Break only when condition met
cycles > 1000 && R0 == 0
```

### 3. Watch Key Values, Not Everything

Too many watches slow down execution:
- Watch inputs and outputs
- Watch loop counters
- Watch critical state

### 4. Leverage Time-Travel

When bug found:
1. Rewind to earlier state
2. Step forward slowly
3. Identify exact cycle bug introduced

### 5. Use Diff View

Enable diff view to quickly see:
- What changed between cycles
- Which registers were modified
- Memory updates

### 6. Export Sessions for Collaboration

Share debugging sessions with teammates:
```bash
File > Export Debug Session
# Send .json file
# Collaborator: File > Import Debug Session
```

---

## Pedagogical Connection

The Ancient Compute debugger embodies **Ada Lovelace's vision** of computational transparency:

> "The engine can arrange and combine its numerical quantities exactly as if they were letters or any other general symbols"
> — Ada Lovelace, Notes on the Analytical Engine (1843)

**Time-travel debugging** allows you to see not just *what* the state is, but *how* the machine arrived at that state—teaching computation as a **sequence of transformations** rather than static results.

This mirrors how Lovelace herself debugged Babbage's designs by manually tracing execution on paper, rewinding and replaying calculations to verify correctness.

---

## Further Reading

- [EMULATOR_VISUALIZATION_GUIDE.md](./EMULATOR_VISUALIZATION_GUIDE.md) - Visual debugging
- [PROFILER_GUIDE.md](./PROFILER_GUIDE.md) - Performance analysis
- [API_INTEGRATION.md](./API_INTEGRATION.md) - Programmatic debugging

---

**Questions or Issues?**
Report bugs at: https://github.com/ancient-compute/issues
Documentation: https://docs.ancient-compute.org
