# PHASE 3: ASSEMBLY PROCEDURES WITH DIAGRAMS
## Detailed Step-by-Step Assembly Instructions with Technical Illustrations

**Document Version**: 2.0  
**Status**: ASSEMBLY PROCEDURES & TECHNICAL DRAWINGS  
**Scope**: Five assembly phases, detailed procedures, TikZ diagrams for each  
**Target Audience**: Lead assembly technicians, supervisors, engineers  

---

## MILL ASSEMBLY PROCEDURE

### Overview

The Mill (arithmetic unit) consists of five 5-digit digit wheel stacks, mechanical carry mechanism, and control levers. This is the most complex subassembly requiring highest precision.

### Equipment Required

- Assembly bench (2 meters × 1 meter, rigid construction)
- V-blocks or shaft supports (for holding 1-meter axle)
- Precision depth gauge (±0.02 mm)
- Calipers (±0.05 mm)
- Torque wrench (0-50 N·m range)
- Magnifying glass (10×)
- Precision spirit level

### Assembly Sequence

#### Step 1: Prepare Axle & End Plates

**Purpose**: Establish the foundation structure for digit wheel stack

**Materials**:
- 1 precision shaft (10 mm diameter, 1 meter length, already manufactured)
- 2 cast iron end plates (pre-drilled for bearing mounting)
- 4 thrust washers (5 mm thick, 12 mm ID × 20 mm OD)
- 8 M8 cap screws + washers + nuts
- Light machine oil

**Procedure**:

1. **Inspect axle**:
   - Visually check for bends or surface damage
   - Measure runout on lathe spindle or precision V-blocks
   - Tolerance: Total Indicated Runout (TIR) < 0.05 mm over 1-meter length
   - If runout > 0.05 mm: REJECT and request replacement (rework not cost-effective)

2. **Clean axle**:
   - Wipe with lint-free cloth
   - Remove any manufacturing oils
   - Apply thin film of light machine oil (protective, not for operation)

3. **Mount end plates**:
   - Position first end plate horizontally on bench
   - Slip axle through center bore (should be smooth fit, no force needed)
   - Slide thrust washer onto axle (load-bearing side)
   - Set light preload with M8 cap screw (hand-tight only, will finalize after stacking)

4. **Verify alignment**:
   - Use precision level to check both end plates are horizontal
   - Check perpendicularity of end plates to axle (±0.05 mm tolerance)

**Estimated Time**: 1 hour

**Quality Checkpoints**:
- ✓ Axle runout < 0.05 mm
- ✓ End plates perpendicular to axle
- ✓ No binding when rotating axle by hand

---

#### Step 2: Stack Digit Wheels (Most Critical, Most Tedious)

**Purpose**: Position 50 digit wheels on shared axle with precise spacing

**⚠️ ATTENTION**: This is the longest single assembly task (8-10 hours per stack). Requires patience and attention to detail.

**Materials** (per stack):
- 50 digit wheels (already manufactured, inspected)
- 48 spacing gears (interlocking with digit wheels)
- 50 retaining clips (mechanical, spring-loaded)
- 100 shims (calibration spacers, 0.1 mm, 0.2 mm, 0.5 mm thicknesses)
- Light machine oil

**Procedure** (Wheel-by-wheel):

1. **Prepare wheel #1**:
   - Select first digit wheel from bin
   - Visually inspect bore (no debris, smooth finish)
   - Wipe bore with lint-free cloth
   - Manually rotate wheel on axle (should be frictionless, light resistance only)

2. **Install wheel #1 on axle**:
   - Slide wheel onto axle, position at end plate
   - Install spacing gear immediately after (interlocks with wheel)
   - Check spacing gear rotates smoothly with digit wheel (no binding)
   - Install retaining clip (locks wheel in position)
   - **Time per wheel**: 3 minutes

3. **Repeat for wheels #2-50**:
   - **Progress checkpoint every 10 wheels** (1 hour per 10 wheels):
     - Stop and measure stack height with depth gauge
     - Expected height: 10 wheels × (4 mm wheel + 2 mm spacing) = 60 mm
     - Tolerance: ±2 mm
     - If height out of tolerance: Re-examine last 5 wheels, verify spacing gears are not bent
   
   - **Manual rotation test every 5 wheels**:
     - Attempt to hand-crank axle (should rotate smoothly with light resistance)
     - If significant binding detected: Remove last wheel, inspect bore and spacing gear
     - **Do not proceed until smooth rotation confirmed**

4. **Final wheel placement**:
   - Install 50th wheel
   - Verify all wheels are present and in order (visual count, recount carefully)
   - Measure final stack height: 50 × 6 mm = 300 mm ±2 mm

5. **Install second end plate**:
   - Slide onto opposite end of axle
   - Install thrust washer
   - Tighten M8 cap screws in cross pattern (uniform pressure):
     - Screw 1 (top): Hand-tight
     - Screw 3 (bottom): Hand-tight
     - Screw 2 (left): Torque to 15 N·m
     - Screw 4 (right): Torque to 15 N·m
     - Repeat 2 & 4 to 20 N·m (final torque)
   
   - **Caution**: Over-tightening can bend shaft; under-tightening can allow wheel movement
   - Target: Snug fit with no axial movement, but wheels still rotate freely

**Estimated Time per Stack**: 8-10 hours

**Quality Checkpoints**:
- ✓ Stack height 300 mm ±2 mm
- ✓ Free hand rotation (no binding)
- ✓ All 50 wheels visible and in order
- ✓ End plates perpendicular ±0.05 mm
- ✓ No loose wheels (test by light nudging)

---

#### Step 3: Manufacture Carry Mechanism

**Purpose**: Connect digit wheels through carry logic (units → tens → hundreds, etc.)

**Carry Logic Overview**:
- When units digit transitions 9→0, it triggers tens digit to advance
- When tens digit transitions 9→0, it triggers hundreds digit, and so on
- Mechanical implementation: Lever arms, return springs, engagement surfaces

**Materials**:
- Sector wheels (8-tooth segments, 10 units)
- Pinion gears (20-tooth, 10 units)
- Shafts for intermediate axles (5 units, 300 mm length)
- Bearing supports (cast iron, 10 units)
- Lever arms (5 units, for carry mechanical logic)
- Return springs (steel, 5 units, 20 N return force each)
- Fasteners (screws, washers, 200 units)
- Lubricant

**Procedure**:

1. **Design Review** (before manufacturing):
   - Verify carry mechanism diagram (see Section 2.2 diagram)
   - Confirm engagement surfaces between digit wheel and carry lever
   - Test on mockup using 3D model or wooden prototype (if available)

2. **Mount bearing supports** (10 supports along Mill length):
   - Position at predetermined locations (100 mm spacing along Mill structure)
   - Bolt securely to main frame
   - Check perpendicularity of bore to frame (±0.1 mm tolerance)

3. **Install intermediate shafts** (5 shafts, one per digit position):
   - Thread through bearing supports
   - Lock with set screws (moderate force, not over-tight)
   - Test free rotation of each shaft

4. **Mount pinion gears** on shafts:
   - Press onto shaft with tight fit (0.02 mm interference)
   - Use arbor press with protective bushings
   - Verify gear is centered on shaft (no axial runout)

5. **Install sector wheels** at fixed positions:
   - Sector wheels interlock with pinion gears
   - Mount on fixed bracket (does not rotate)
   - Verify mesh: ~1 mm backlash, smooth engagement

6. **Test carry linkage**:
   - Manually rotate digit wheel stack
   - At position 9→0 transition, verify sector wheel engages
   - Verify next-digit wheel is nudged forward (units → tens carry)
   - Repeat for all 5 digits

**Estimated Time**: 12-15 hours

**Quality Checkpoints**:
- ✓ All gears mesh smoothly (no grinding)
- ✓ No excessive backlash (> 2 mm)
- ✓ Carry propagates through all 5 digits (test manually)
- ✓ Return springs restore carry levers after engagement

---

#### Step 4: Install Control Levers

**Purpose**: Add mechanical linkages for manual reset, testing, and control signals

**Materials**:
- Lever arms (3,000 units manufactured; use ~30 for Mill)
- Pivot pins (50 units)
- Return springs (100 units)
- Fasteners & clips
- Lubricant

**Procedure**:

1. **Mount lever pivots**:
   - Install pivot pins at predetermined locations
   - Test lever rotation (should be smooth, no binding)

2. **Connect levers to carry mechanism**:
   - Use mechanical linkage (rigid rod or flexible link) to connect carry signals
   - Verify lever position indicates carry state (visual feedback)

3. **Install return springs**:
   - Springs reset levers to neutral position after operation
   - Tension: Light (10-20 N)
   - Verify spring returns lever within 0.5 seconds

**Estimated Time**: 6-8 hours

**Quality Checkpoints**:
- ✓ All levers pivot smoothly
- ✓ Carry signals propagate correctly
- ✓ Springs return levers reliably

---

#### Step 5: Final Mill Assembly QC

**Purpose**: Validate complete Mill before integration with Store/Barrel

**Procedure**:

1. **Visual inspection**:
   - Check all fasteners are tight (no loose screws visible)
   - Verify all components secured to frame
   - No sharp edges or protrusions

2. **Hand-crank rotation test**:
   - Using handle attached to main shaft (temporary), rotate slowly (1 revolution per 30 seconds)
   - Listen for grinding or unusual sounds
   - Feel for smooth torque curve (should be relatively uniform)
   - Iterate any binding issues

3. **Carry mechanism test**:
   - Advance digit wheels manually through several complete cycles (0→9→0)
   - Verify carry propagates correctly
   - Record any anomalies

4. **Measurement validation**:
   - Final stack height: Confirm 300 mm ±2 mm per stack
   - Check all end plates perpendicular to frame
   - Verify no visible bending or deflection

**Estimated Time**: 4-5 hours (includes debugging if issues found)

---

### MILL ASSEMBLY TikZ DIAGRAM

Below is a cross-section diagram of Mill assembly showing digit wheel stack, bearing supports, and carry mechanism:

**TikZ Code for Mill Cross-Section**:

```tikz
\documentclass[tikz,border=5mm]{standalone}
\usepackage{tikz}
\usetikzlibrary{shapes,arrows,positioning}

\begin{document}

\begin{tikzpicture}[scale=1.0, every node/.style={font=\small}]

  % Draw main frame (side view)
  \draw[line width=2pt, color=darkgray] (0,0) -- (14,0);
  \draw[line width=2pt, color=darkgray] (0,6) -- (14,6);
  \draw[line width=1.5pt, color=darkgray] (0,0) -- (0,6);
  \draw[line width=1.5pt, color=darkgray] (14,0) -- (14,6);
  
  % End plates
  \draw[line width=1pt, fill=lightgray] (0.5,1) rectangle (1.5,5);
  \draw[line width=1pt, fill=lightgray] (12.5,1) rectangle (13.5,5);
  \node at (1, 0.5) {End plate};
  \node at (13, 0.5) {End plate};
  
  % Main axle
  \draw[line width=3pt, color=blue!70] (1.5,3) -- (12.5,3);
  \node at (7, 2.5) {\textbf{Main Axle (10 mm OD)}};
  
  % Digit wheels (schematic)
  \foreach \x in {2,3,4,5,6,7,8,9,10,11,12} {
    \draw[line width=0.5pt] (\x,2.5) circle (0.3);
    \draw[line width=0.5pt] (\x,3.5) circle (0.3);
  }
  
  % Labels
  \node at (7,1.7) {50 Digit Wheels (4 mm width each)};
  \node at (7,4.2) {Stack Height: 300 mm ±2 mm};
  
  % Bearing support
  \draw[line width=2pt, fill=yellow!30] (3.5,-0.5) rectangle (4.5,0.1);
  \node at (4,-0.9) {Bearing};
  
  % Carry mechanism (simplified)
  \draw[line width=1pt, color=red] (2.5,5.2) -- (2.5,5.8) -- (3.5,5.8) -- (3.5,5.2);
  \node at (3, 6.2) {Carry Lever};
  \draw[->, line width=1pt, color=red] (3,5.2) -- (3,3.3);
  
\end{tikzpicture}

\end{document}
```

---

## STORE ASSEMBLY PROCEDURE

### Overview

The Store (memory unit) consists of 2,000 digit wheel columns arranged in a vertical grid (20 columns × 100 rows). Each column holds a 50-digit number. This is the largest physical structure but simpler mechanically than Mill.

### Assembly Sequence

#### Step 1: Build Frame Structure

**Materials**:
- Steel or cast iron frame components (20 mm diameter bars)
- Angle iron for bracing
- Fasteners (bolts, M10 × 30 mm, 1,000 units)
- Lubricant

**Procedure**:

1. **Assemble frame skeleton**:
   - Weld or bolt main vertical posts (4 posts, 2 meters tall each)
   - Install horizontal crossbars (20 mm spacing for column mounting)
   - Brace structure diagonally for rigidity
   - Verify frame is square and level

2. **Mount bearing supports**:
   - Install bearing support blocks at each column location (20 columns × 100 rows = 2,000 supports)
   - Verify regular spacing (20 mm grid)
   - Check all bores are parallel (no twist)

**Estimated Time**: 20-25 hours

**Quality Checkpoints**:
- ✓ Frame is square (diagonal measurements equal within 5 mm over 2 meters)
- ✓ All bearing bores parallel to vertical axis (±0.1 mm tolerance)
- ✓ No visible bending or deflection

#### Step 2: Install Digit Wheel Stacks (2,000 stacks)

**Purpose**: Mount 2,000 preassembled digit wheel stacks into column grid

**Materials** (per stack):
- 1 preassembled digit wheel stack (from Mill assembly template, but this column may be longer if needed)
- Fasteners & lubricant

**Procedure**:

1. **Prepare stacks** (repeat from Mill assembly for each column):
   - Assemble 50-digit wheel stack following Mill procedure
   - Verify stack height 300 mm
   - Verify free rotation

2. **Insert stacks into frame**:
   - Position stack #1 in frame column position [0,0] (top-left)
   - Thread shaft through bearing supports
   - Lock with set screws (moderate force)
   - Verify free rotation

3. **Iterate for all 2,000 stacks**:
   - Row by row (left to right, top to bottom)
   - Progress checkpoint every 100 stacks (1 column completed)
   - Estimated time: 2,000 stacks × 15 minutes per stack = 30,000 minutes = 500 hours = **62 shifts = 7.75 weeks**

**Critical Issue**: This is labor-intensive. Suggest parallel teams:
- Team 1: Preassemble stacks (parallel with Store frame building)
- Team 2: Insert stacks into frame (once frame ready)

**Alternative Approach** (if time-critical):
- Subcontract 50% of stack assembly to external workshop
- Cost delta: +£0.50 per stack = +£500 total
- Time savings: Reduce to 3.9 weeks (acceptable trade-off)

**Estimated Time**: 7-8 weeks (with parallel teams) or 3.9 weeks (with subcontracting)

**Quality Checkpoints**:
- ✓ All 2,000 stacks installed and rotating freely
- ✓ No loose columns (test by hand nudging)
- ✓ Regular alignment (visual inspection every 10 columns)

#### Step 3: Install Synchronization Shafts

**Purpose**: Connect all columns for synchronized row-wise operations

**Materials**:
- 100 synchronization shafts (1 per row, 500 mm length, 8 mm diameter)
- 200 coupling hubs (link column shafts to sync shaft)
- Fasteners

**Procedure**:

1. **Install shaft #1** (top row):
   - Mount horizontally above digit wheel columns
   - Connect each column's carry mechanism to sync shaft through coupling hub
   - Verify all columns advance together when shaft rotated

2. **Repeat for all 100 rows**:
   - Install synchronization shafts one by one
   - Verify coordination between rows

**Estimated Time**: 30-40 hours

**Quality Checkpoints**:
- ✓ All columns advance synchronously when manual crank applied
- ✓ No skipping or slipping of synchronization

---

## BARREL ASSEMBLY PROCEDURE

### Overview

The Barrel (program control mechanism) is a 150-position drum that sequences operations. It resembles a music box mechanism but more complex.

### Assembly Sequence

#### Step 1: Mount Barrel Cylinder

**Materials**:
- Cast iron or hardened steel barrel (300 mm OD × 600 mm length)
- Precision centers or bearing supports (2, at each end)
- Drive coupling

**Procedure**:

1. **Mount on centers**:
   - Position barrel horizontally at fixed height (measurement: 1.5 meters above floor, predetermined)
   - Install bearing supports at both ends
   - Verify horizontal alignment with laser level (±2 mm tolerance over 600 mm length)

2. **Install drive coupling**:
   - Connect barrel to main clock mechanism (geared connection)
   - Verify 1:1 synchronization (barrel advances 1 position per clock cycle)

**Estimated Time**: 4-6 hours

**Quality Checkpoints**:
- ✓ Barrel is horizontal (level within ±2 mm)
- ✓ Free rotation (hand-rotate, should move smoothly)
- ✓ Synchronization with clock verified

#### Step 2: Mark Positions & Install Readers

**Purpose**: Establish 150 program positions, install mechanical readers

**Materials**:
- 150 position markers (engraved or pin-holes around barrel circumference)
- 6-8 reader levers (spring-loaded mechanical "fingers")
- Return springs

**Procedure**:

1. **Mark 150 positions**:
   - Divide 360° circumference into 150 equal positions (2.4° each)
   - Engrave or drill small holes at each position mark
   - Verify spacing with precision calipers (±0.5 mm tolerance)

2. **Install reader mechanism**:
   - Mount reader levers above barrel surface
   - Position levers to detect barrel position
   - When barrel position mark passes, lever engages (mechanical "feeler")
   - Lever signals next operation to perform

3. **Test synchronization**:
   - Manually rotate barrel 1 position
   - Verify reader lever engages and signals correctly
   - Repeat for several positions

**Estimated Time**: 8-10 hours

**Quality Checkpoints**:
- ✓ All 150 positions accurately marked (±0.5 mm spacing)
- ✓ Reader mechanism engages smoothly at each position
- ✓ No binding or skipping

---

## I/O ASSEMBLY PROCEDURE

### Overview

The I/O system (Input/Output) handles Hollerith punched cards for program input and result output.

### Assembly Sequence

#### Step 1: Construct Card Hopper & Feed Mechanism

**Purpose**: Load and feed punched cards one at a time into reader

**Materials**:
- Hopper frame (metal, holds ~100 cards)
- Feed pawls & ratchets (mechanical indexing, 4 units)
- Spring-loaded pressure plate
- Fasteners

**Procedure**:

1. **Mount hopper frame**:
   - Position at fixed location (predetermined, side of machine)
   - Secure to main frame

2. **Install feed mechanism**:
   - Install pawls & ratchets
   - Test manual card advance (each pump should advance 1 card)

**Estimated Time**: 6-8 hours

#### Step 2: Install Card Reader (Punched Card Input)

**Purpose**: Read 6-8 rows of holes from card (up to 80 columns of data)

**Materials**:
- Reader frame
- 6-8 reading needles (spring-loaded, 1.5 mm diameter)
- Return springs
- Fasteners

**Procedure**:

1. **Mount reader**:
   - Position to align with card holes
   - Spring tension should be light (5-10 N per needle)

2. **Test reader**:
   - Load test card with known hole pattern
   - Manually advance card through reader
   - Verify all holes are detected

**Estimated Time**: 4-6 hours

#### Step 3: Install Punch Mechanism (Results Output)

**Purpose**: Punch holes in output card to record results

**Materials**:
- Punch head with 6-8 punch pins
- Solenoid (electromagnet, for 1960s+ version) OR mechanical cam (for pure mechanical version)
- Return springs
- Fasteners

**Procedure** (mechanical version):

1. **Mount punch head**:
   - Position below output card path
   - Spring return to unengaged position

2. **Connect to control mechanism**:
   - Link punch trigger to program control signal
   - Test punch mechanism: manual trigger should punch all holes simultaneously

**Estimated Time**: 4-6 hours

#### Step 4: Final I/O Test

**Procedure**:

1. Load test card with known input (e.g., "00002" in first column)
2. Manually advance through reader
3. Trigger punch mechanism
4. Eject result card
5. Verify punch holes match expected output

**Estimated Time**: 2-3 hours

---

## FINAL INTEGRATION ASSEMBLY

### Overview

This section describes how all subassemblies (Mill, Store, Barrel, I/O) are positioned and connected into a single operational Babbage Analytical Engine.

### Integration Sequence

**Duration**: Weeks 18-24 (7 weeks total integration & testing)

#### Step 1: Main Frame Assembly (Week 18)

**Procedure**:

1. Position main base frame (cast iron or steel plate, 2 meters × 1.5 meters × 1 meter tall)
2. Bolt all subassemblies to main frame:
   - Mill unit: center position, bottom shelf (height: 0.5 m from floor)
   - Store unit: rear position, vertical installation (height: 0 to 2 m from floor)
   - Barrel unit: top position, 1.5 m height
   - I/O unit: side position (height: 0.8 m from floor)
3. Verify all units are level and secure

**Time**: 4-6 hours

#### Step 2: Drive Mechanism Installation (Week 19)

**Procedure**:

1. Install main shaft (connects all subassemblies to central clock)
2. Install gears/belts to synchronize Mill, Store, Barrel, and I/O
3. Install main clock mechanism (hand-crank or motor)
4. Test synchronized rotation: All subassemblies should advance together

**Time**: 8-10 hours

#### Step 3: Lubrication (Week 20)

**Procedure**:

1. Apply light machine oil to all bearing points (~200 mL total)
2. Apply gear lube to all gear meshes
3. Apply light oil to all pivot points

**Time**: 2-3 hours

#### Step 4: Integration Testing (Weeks 20-22)

**Procedure**:

1. Hand-crank test at low speed (1 revolution per 30 seconds)
2. Listen for grinding, binding, or unusual sounds
3. Feel for smooth torque curve
4. Debug any issues found (may require disassembly and rework)

**Time**: 12-16 hours

#### Step 5: Operational Testing (Weeks 22-24)

**Procedure**:

1. Load initial values into Store (manually set digit wheels to starting values)
2. Load program card into Hopper
3. Crank through entire program (hand-powered operation)
4. Punch results onto output card
5. Verify results against expected output

**Test program #1** (Simple Addition): 
- Input: 2 + 3
- Expected output: 5
- Pass/Fail: Record result

**Test program #2-20** (Various operations):
- Subtraction, multiplication, division, factorials, polynomials, financial calculations

**Target**: 80%+ of tests pass (16 of 20 tests correct)

**Time**: 30-40 hours (includes debugging if failures occur)

---

## ASSEMBLY TIMING SUMMARY

| Phase | Duration | Critical Tasks |
|---|---|---|
| Mill assembly | 1-2 weeks | Digit wheel stacks (8-10 hrs each × 4 stacks = 32-40 hrs); carry mechanism (12-15 hrs) |
| Store assembly | 2-3 weeks | Digit wheel stacks (2,000 stacks × 15 min = 500 hours); frame building (20-25 hrs) |
| Barrel assembly | 1 week | Position marking (8-10 hrs); reader installation (6-8 hrs) |
| I/O assembly | 1 week | Hopper, reader, punch installation (12-20 hrs total) |
| Final integration | 1-2 weeks | Frame assembly (4-6 hrs); drive mechanism (8-10 hrs); testing (42-56 hrs) |
| **TOTAL ASSEMBLY TIME** | **6-9 weeks** | **Overlaps with manufacturing weeks 12-24** |

---

## TikZ DIAGRAMS (Assembly Overview)

**Full Assembly Schematic** (bird's-eye view):

```tikz
\documentclass[tikz,border=5mm]{standalone}
\usepackage{tikz}

\begin{document}

\begin{tikzpicture}[scale=0.8]

  % Main frame
  \draw[line width=3pt, color=black] (0,0) rectangle (8,6);
  
  % Mill (center-bottom)
  \draw[line width=2pt, fill=blue!20] (3,1) rectangle (5,3);
  \node at (4,2) {\textbf{MILL}};
  \node at (4,1.5) {(Arithmetic)};
  
  % Store (rear-vertical)
  \draw[line width=2pt, fill=green!20] (5.5,1) rectangle (7.5,5);
  \node at (6.5,3) {\textbf{STORE}};
  \node at (6.5,2) {(Memory)};
  
  % Barrel (top-center)
  \draw[line width=2pt, fill=yellow!20] (2,4.5) rectangle (4,5.5);
  \node at (3,5) {\textbf{BARREL}};
  \node at (3,4.7) {(Control)};
  
  % I/O (left-side)
  \draw[line width=2pt, fill=red!20] (0.5,2) rectangle (2.5,4);
  \node at (1.5,3) {\textbf{I/O}};
  \node at (1.5,2.5) {(Input/Output)};
  
  % Connecting shafts (schematic)
  \draw[line width=1pt, color=blue, dashed] (4,3) -- (4,4.5);
  \draw[line width=1pt, color=blue, dashed] (2.5,2) -- (3,2);
  \draw[line width=1pt, color=blue, dashed] (5,2) -- (5.5,2);
  
  % Main clock (bottom-center)
  \draw[line width=1.5pt, fill=orange!20] (3.5,-0.8) circle (0.4);
  \node at (3.5,-1.3) {Clock};
  \draw[->, line width=1pt] (3.5,-0.4) -- (3.5,1);
  
  % Labels
  \node at (4,-1.8) {Synchronized Drive System};

\end{tikzpicture}

\end{document}
```

---

## END ASSEMBLY PROCEDURES DOCUMENT

