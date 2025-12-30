# Phase 4: Subassembly Integration Testing
## Testing the Five Major Subassemblies
**Document Version**: 1.0  
**Date**: October 31, 2025  
**Status**: Complete Specification for Subassembly Integration Testing

---

## EXECUTIVE SUMMARY

After all components have passed individual testing and been assembled into their respective subassemblies, each subassembly must undergo integration testing to verify that:

1. All components are correctly positioned and aligned
2. Mechanical motion is smooth and free of binding
3. Functional operations succeed (if applicable to subassembly)
4. No assembly errors (missing parts, reversed components) exist

**Testing schedule**: Weeks 36-40 (overlaps final component testing and begins system integration)

**Testing resources**:
- 2 assembly technicians (1 per subassembly pair)
- 1 QC engineer (oversight and approval)
- Test fixtures and manual operation equipment

**Pass criteria**: All 5 subassemblies demonstrate smooth mechanical operation, correct rotation speeds, and no binding

---

## SECTION 1: MILL ASSEMBLY INTEGRATION TESTING

### 1.1 Mill Assembly Overview

The Mill (Arithmetic Unit) is the computational core:
- **8-10 digit wheel stacks** (digit positions 0-9)
- **Carry mechanism** (linked to other digits)
- **Accumulator functions** (add, subtract, zero-clear)
- **Drive linkage** from main crankshaft

**Assembly completed Week 35**  
**Integration testing Week 36-37**

### 1.2 Pre-Test Inspection (Checklist)

Before mechanical operation, verify:

```
[ ] All digit wheels present (80-100 wheels total)
[ ] All carry levers properly installed (aligned, freely rotating)
[ ] All shafts straight and not bent
[ ] All bearings installed and axially secured
[ ] All locking fasteners tight (no rattle)
[ ] Digit value reset to 0000000000 (all wheels at bottom position)
[ ] No dirt, oil, or debris in mechanical paths
[ ] Test fixture in place (supports Mill on stands, prevents tipping)
```

### 1.3 Functional Tests (Sequence)

#### Test 1.3.1: Digit Wheel Rotation Smoothness

**Objective**: Verify that each digit wheel rotates freely without binding

**Procedure**:
1. Select digit stack 0 (rightmost, ones place)
2. Manually rotate the drive crank connected to this digit stack (slow, 1 revolution = 2 seconds)
3. Feel for smoothness; note any resistance or grinding sounds
4. Repeat for each digit stack (0-9)
5. Record smoothness rating: smooth (S), slight resistance (R), or binding (B)

**Pass criterion**: All digit stacks rotate smoothly with light resistance only (S or slight R)

**Failure action**: If binding (B) detected:
- Stop rotation immediately
- Inspect for bent shaft, misaligned wheels, or stuck bearing
- Disassemble if necessary to identify cause
- Correct and re-test

#### Test 1.3.2: Carry Mechanism Engagement

**Objective**: Verify that carry levers engage and disengage properly as digits overflow

**Procedure** (simulated addition causing overflow):
1. Set digit stack 0 to value 9 (wheel rotated to highest position)
2. Set carry lever arm to "ready" position (spring-loaded, waiting for engagement)
3. Manually rotate digit stack 0 one more position (simulating addition of 1 to 9)
4. Digit stack 0 should snap back to 0, and carry lever should engage
5. Carry lever should push next digit (stack 1) up by one position (simulating +1 carry)
6. Digit stack 1 should advance to 1
7. Carry lever should disengage as digit stack 0 completes its rotation
8. Repeat for digit stacks 0→1, 1→2, 2→3, etc., up to overflow position

**Pass criterion**: 
- All carry engagements smooth and positive
- Digit advancement via carry confirmed (stack N+1 advances when stack N overflows)
- All carry disengagements clean (no "drag" or delayed release)

**Failure action**: If any carry fails:
- Identify which carry lever(s) not engaging
- Check lever alignment and spring tension
- Verify pinion mesh with digit wheel teeth
- Adjust and re-test

#### Test 1.3.3: Rotation Speed Verification

**Objective**: Verify that all digit stacks rotate at correct speed (proportional to hand-crank speed)

**Procedure**:
1. Set up steady hand-crank rotation (1 crank per second = 60 RPM)
2. Observe digit wheel rotation speed visually
3. Digit wheels should rotate smoothly, synchronized with crank
4. All stacks should advance at same rate (no speed variation)
5. Record rotation speed subjectively: fast/nominal/slow relative to crank

**Pass criterion**: All digit stacks rotate at same speed, proportional to crank speed

**Failure action**: If speed variation detected:
- Check all shafts for bending
- Verify all bearings are free-rolling
- Check for misaligned gear mesh
- Adjust and re-test

#### Test 1.3.4: Position Readout Accuracy

**Objective**: Verify that position wheels display correct digit values as rotated

**Procedure**:
1. Set all digit stacks to 0
2. Manually rotate digit stack 0 exactly 1 position (1/10th of full rotation)
3. Read displayed value in position windows: should show "0000000001"
4. Repeat, advancing stack 0 to 2, 3, 4, ..., 9
5. Verify displayed values are: "0000000002", "0000000003", ..., "0000000009"
6. Test one additional stack (e.g., stack 3) with full rotation cycle (0→9→0)
7. Verify values display correctly throughout cycle

**Pass criterion**: 
- All position values display correctly
- No values skipped (e.g., going from 8 directly to 0 without showing 9)
- Values reset from 9 to 0 smoothly

**Failure action**: If position readout wrong:
- Check digit wheel teeth alignment
- Verify position window is clear (not blocked)
- Check position wheel rotation

#### Test 1.3.5: Manual Operation - Simulated Calculation

**Objective**: Verify Mill can perform basic calculation manually

**Procedure** (addition: 234 + 105 = 339):
1. Set Mill to initial value: 0000000000
2. Using digit wheels, manually set to 0000000234 (starting value)
3. Using carry mechanism and manual rotation, add 105:
   - Manually rotate stack 0 five times (4+5=9, no carry) → displays 9
   - Manually rotate stack 1 three times (3+0=3, no carry) → displays 3
   - Manually rotate stack 2 once (2+1=3, no carry) → displays 3
4. Final value should display: 0000000339
5. Compare to expected result

**Pass criterion**: Mill displays correct result (0000000339) after manual addition

**Failure action**: If result wrong:
- Verify digit wheel positions visually
- Check if carry mechanism interfered with rotation
- Re-test with simpler calculation (e.g., 1+1=2)

### 1.4 Mill Assembly Test Report Template

```
MILL ASSEMBLY INTEGRATION TEST REPORT
Test date: [date]
Test technician: [name]
Assembly engineer: [name]
Test duration: [hours]

Pre-Test Checklist: ✅ PASS / ❌ FAIL
[List any issues found]

Functional Tests:
1. Digit wheel rotation smoothness: 
   - Stack 0: [S/R/B], Stack 1: [S/R/B], ..., Stack 9: [S/R/B]
   - Result: ✅ PASS / ❌ FAIL

2. Carry mechanism engagement:
   - Carry 0→1: ✅ engage, ✅ disengage
   - Carry 1→2: ✅ engage, ✅ disengage
   - [... all carries ...]
   - Result: ✅ PASS / ❌ FAIL

3. Rotation speed verification:
   - All stacks synchronized: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

4. Position readout accuracy:
   - Stack 0: 0→1→2...9→0: ✅ PASS / ❌ FAIL
   - Stack 3: [test result]: ✅ PASS / ❌ FAIL

5. Manual operation test (234 + 105 = 339):
   - Expected: 0000000339
   - Actual: [displayed value]
   - Result: ✅ PASS / ❌ FAIL

OVERALL MILL ASSEMBLY RESULT: ✅ PASS / ❌ FAIL

Issues found:
[List all issues and resolutions]

Approved by (QC engineer): ________________  Date: ________
```

---

## SECTION 2: STORE ASSEMBLY INTEGRATION TESTING

### 2.1 Store Assembly Overview

The Store (Memory Unit) holds 1,000 digit values in 2,000-column matrix:
- **2,000 vertical columns** (each holds one decimal digit)
- **10 horizontal rows** (digit values 0-9)
- **Synchronization mechanism** (columns advance together with main crank)

**Assembly completed Week 35**  
**Integration testing Week 37**

### 2.2 Pre-Test Inspection

```
[ ] All 2,000 columns physically present and counted
[ ] All column support frames straight and not bent
[ ] All synchronization shafts in place and straight
[ ] All linking mechanism gears present and meshing
[ ] Store matrix positioned level (no tilting)
[ ] All fasteners tight (no rattle or movement)
[ ] Test rig supports Store matrix securely
[ ] Manual crank connected to synchronization drive
```

### 2.3 Functional Tests

#### Test 2.3.1: Synchronization Drive Engagement

**Objective**: Verify that all 2,000 columns advance together when crank is turned

**Procedure**:
1. Observe one column position (mark with colored tape on support frame)
2. Mark the column's initial position with indicator (pen mark on frame)
3. Manually crank the synchronization drive one full rotation
4. Observe all columns: do they all advance together or separately?
5. Repeat for 3 full rotations
6. Verify all columns stay aligned (no one column ahead or behind)

**Pass criterion**: All 2,000 columns advance synchronously; no column skips or lags

**Failure action**: If columns not synchronized:
- Check synchronization shaft for bending
- Verify all linking gears are properly meshed
- Check for broken or misaligned gears
- Adjust and re-test

#### Test 2.3.2: Column Rotation Smoothness

**Objective**: Verify that columns rotate freely without binding

**Procedure**:
1. Select one column (middle of matrix)
2. Manually rotate that column through 360° (full cycle)
3. Feel for smoothness and resistance
4. Repeat for 5 different columns across matrix (top-left, top-right, middle, bottom-left, bottom-right)
5. Record smoothness rating for each

**Pass criterion**: All sampled columns rotate smoothly

#### Test 2.3.3: Position Advancement Accuracy

**Objective**: Verify that columns advance exactly one position per crank rotation

**Procedure**:
1. Mark initial position of one column (e.g., column at row 0)
2. Manually crank synchronization drive exactly 1 full rotation
3. Measure column advancement: should be exactly 1/10th of full rotation
4. Column should now be at row 1
5. Repeat 9 more times, expecting progression: 0→1→2→...→9→0

**Pass criterion**: Column advances exactly 1 position per crank rotation, cycles correctly 0-9-0

#### Test 2.3.4: Store Matrix Stability

**Objective**: Verify that Store matrix remains rigid during operation

**Procedure**:
1. Establish baseline measurements:
   - Measure distance between support frame at top and bottom (should be constant)
   - Measure diagonal distance (corner to corner)
2. Manually crank synchronization drive for 20 full rotations
3. Re-measure frame dimensions
4. Calculate change: should be < 0.5 mm

**Pass criterion**: Frame remains within ±0.5 mm of baseline (no frame flex or warping)

#### Test 2.3.5: Load Test - Simulated Data Retention

**Objective**: Verify that Store matrix can retain and cycle through different data patterns

**Procedure** (simplified, tests a few columns):
1. Manually position 4 test columns to different values: column A→2, column B→5, column C→7, column D→9
2. Crank synchronization drive for 100 full rotations (10 complete cycles through all positions)
3. After 100 rotations, columns should have advanced 1000 positions (= 100 full cycles)
4. Verify columns have returned to original row positions
5. Values should still be at: column A→2, column B→5, column C→7, column D→9

**Pass criterion**: Columns retain position values through 100 crank rotations and return to starting values

### 2.4 Store Assembly Test Report

```
STORE ASSEMBLY INTEGRATION TEST REPORT
Test date: [date]
Test technician: [name]
Test duration: [hours]

Pre-Test Checklist: ✅ PASS / ❌ FAIL

Functional Tests:
1. Synchronization drive engagement:
   - All 2,000 columns advance together: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

2. Column rotation smoothness:
   - Column samples (5 tested): [S/R/B ratings]
   - Result: ✅ PASS / ❌ FAIL

3. Position advancement accuracy:
   - Column advances 1 position per rotation: ✅ YES / ❌ NO
   - Cycles correctly 0→9→0: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

4. Store matrix stability:
   - Baseline frame height: [mm]
   - After 20 rotations: [mm]
   - Change: [mm] (target: < 0.5 mm)
   - Result: ✅ PASS / ❌ FAIL

5. Load test (100 crank rotations):
   - Columns retained starting values: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

OVERALL STORE ASSEMBLY RESULT: ✅ PASS / ❌ FAIL

Issues found and resolutions:
[List all issues]

Approved by (QC engineer): ________________  Date: ________
```

---

## SECTION 3: BARREL ASSEMBLY INTEGRATION TESTING

### 3.1 Barrel Assembly Overview

The Barrel (Program Control Unit) contains:
- **Rotating barrel** (1-meter length, 30 cm diameter)
- **Position markers** (rows of pins indicating current operation)
- **Pin holes** (programmable card reader reads holes to determine operation)
- **Synchronization linkage** to main crankshaft

**Assembly completed Week 35**  
**Integration testing Week 37**

### 3.2 Pre-Test Inspection

```
[ ] Barrel rotates on its central axis (not stuck)
[ ] All position markers in place
[ ] No pins bent or missing
[ ] Synchronization linkage attached and tight
[ ] Card reader mechanism positioned correctly (25-50 mm above pins)
[ ] Barrel indexed to starting position (row 0)
[ ] Manual crank connected to barrel synchronization
```

### 3.3 Functional Tests

#### Test 3.3.1: Barrel Rotation Smoothness

**Objective**: Verify barrel rotates freely without binding

**Procedure**:
1. Manually rotate barrel slowly (1 full rotation = 10-15 seconds)
2. Feel for resistance and binding
3. Barrel should rotate smoothly with light bearing friction only
4. Listen for grinding sounds (indicate bearing damage)
5. Repeat 5 full rotations

**Pass criterion**: Smooth rotation with minimal bearing friction, no grinding sounds

#### Test 3.3.2: Position Marker Accuracy

**Objective**: Verify position markers accurately indicate current row

**Procedure**:
1. Start with barrel at row 0 (verified by position window or reference mark)
2. Manually rotate barrel exactly 1 position (36° per position, ~1/10 of full rotation)
3. Verify position marker indicates row 1
4. Repeat for rows 2, 3, ..., 9
5. Verify cycle back: row 9→0 with one more rotation

**Pass criterion**: All position markers display correct row numbers

#### Test 3.3.3: Card Reader Positioning

**Objective**: Verify card reader is correctly positioned relative to barrel pins

**Procedure**:
1. Insert test punched card into reader mechanism
2. Rotate barrel to position the first row of pins under card reader
3. Manually lower card reader onto pins (simulates card engagement)
4. All pins under test holes should be engaged
5. Pins NOT under holes should not be engaged (reader passes over them)
6. Repeat for 5 different positions

**Pass criterion**: Card reader correctly engages/disengages pins based on card hole pattern

#### Test 3.3.4: Synchronization with Main Crank

**Objective**: Verify barrel advances in sync with main crankshaft (1 position per crank rotation)

**Procedure**:
1. Mark barrel's current position with indicator
2. Manually rotate main crankshaft exactly 1 full rotation
3. Measure barrel advancement: should be exactly 1 position (36°)
4. Repeat for 10 crank rotations
5. Barrel should have advanced 10 positions

**Pass criterion**: Barrel advances exactly 1 position per main crank rotation

#### Test 3.3.5: Barrel Load Test (100 Rotations)

**Objective**: Verify barrel can operate for extended run without degradation

**Procedure**:
1. Manually rotate main crankshaft 100 full rotations (= 100 barrel positions = 10 complete cycles)
2. Monitor for:
   - Resistance increasing (bearing wear)
   - Unusual sounds (mechanical damage)
   - Position markers drifting (synchronization loss)
3. After 100 rotations, barrel should return to starting position
4. Recheck smoothness of rotation

**Pass criterion**: Barrel completes 100 rotations smoothly, returns to start, no degradation

### 3.4 Barrel Assembly Test Report

```
BARREL ASSEMBLY INTEGRATION TEST REPORT
Test date: [date]
Test technician: [name]
Test duration: [hours]

Pre-Test Checklist: ✅ PASS / ❌ FAIL

Functional Tests:
1. Barrel rotation smoothness:
   - Rotations 1-5: [smooth/acceptable/rough]
   - Result: ✅ PASS / ❌ FAIL

2. Position marker accuracy:
   - Positions 0-9 cycle: ✅ PASS / ❌ FAIL

3. Card reader positioning:
   - Test card engagement: ✅ PASS / ❌ FAIL

4. Synchronization with main crank:
   - 1 crank = 1 barrel position: ✅ PASS / ❌ FAIL
   - 10 cranks = 10 positions: ✅ PASS / ❌ FAIL

5. Load test (100 rotations):
   - Smoothness maintained: ✅ YES / ❌ NO
   - Returned to start position: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

OVERALL BARREL ASSEMBLY RESULT: ✅ PASS / ❌ FAIL

Issues found:
[List all issues and resolutions]

Approved by (QC engineer): ________________  Date: ________
```

---

## SECTION 4: I/O ASSEMBLY INTEGRATION TESTING

### 4.1 I/O Assembly Overview

The I/O (Input/Output Unit) includes:
- **Card hopper** (holds stack of program cards)
- **Card reader** (reads punched holes from cards)
- **Card punch mechanism** (punches output holes into blank cards)
- **Mechanical linkage** to mill/store/barrel

**Assembly completed Week 35**  
**Integration testing Week 38**

### 4.2 Pre-Test Inspection

```
[ ] Card hopper positioned and secured
[ ] Card reader mechanism complete and positioned
[ ] Card punch solenoid / mechanical linkage intact
[ ] Blank cards available for testing
[ ] Punch mechanism not jammed
[ ] Reader/punch gears engaged with main drive shaft
```

### 4.3 Functional Tests

#### Test 4.3.1: Card Hopper Operation

**Objective**: Verify card hopper can reliably feed cards one at a time

**Procedure**:
1. Load test punch cards (20 cards) into hopper
2. Manually activate card advance mechanism (crank or lever)
3. Verify first card drops into reader position
4. Advance mechanism again; first card moves to punch position, second card enters reader
5. Repeat until all 20 cards have cycled through
6. Verify no cards jammed, no double-feeds, no missed feeds

**Pass criterion**: All 20 cards cycle through correctly with 100% feed success

#### Test 4.3.2: Card Reader Operation

**Objective**: Verify card reader correctly detects hole patterns

**Procedure** (requires test punch cards with known hole patterns):
1. Insert test card with known pattern (e.g., holes in positions 1, 3, 5, 7, 9)
2. Manually lower reader over card
3. Record which reader pins engage (should match hole positions)
4. Repeat for 5 different test card patterns
5. Verify reader output matches expected pin positions for each card

**Pass criterion**: Reader correctly identifies all hole patterns in test cards

#### Test 4.3.3: Card Punch Operation

**Objective**: Verify punch mechanism can create holes in blank cards

**Procedure**:
1. Insert blank test card into punch position
2. Manually activate punch mechanism (press lever or crank)
3. Verify punch creates hole (measure hole: ~5 mm diameter, clean edges)
4. Repeat for 10 blank cards
5. Verify all holes properly formed (no incomplete punches, no torn cards)

**Pass criterion**: Punch creates clean, consistent holes in all test cards

#### Test 4.3.4: Reader-to-Mill Linkage Verification

**Objective**: Verify card reader outputs correctly control mill digit wheels

**Procedure** (integration with mill):
1. Insert test card with hole pattern (e.g., positions 0,1,2,3,4 = digits 0-4)
2. Manually operate combined system (card advance + mill operation)
3. Mill should respond to card pattern:
   - If card hole at position 0 → mill receives "add 1 to digit 0"
   - If card hole at position 5 → mill receives "add 5 to digit 0"
   - Etc.
4. Verify mill digits advance according to card pattern

**Pass criterion**: Mill correctly responds to card pattern commands (reader-to-mill linkage functional)

#### Test 4.3.5: Output Punch Verification

**Objective**: Verify punch mechanism correctly punches mill output values

**Procedure** (integration with mill):
1. Set mill to display value 12345
2. Manually activate output punch mechanism
3. Punch should create holes corresponding to values:
   - Digit 0 = 5 → reader position 5 punched
   - Digit 1 = 4 → reader position 4 punched
   - Etc.
4. Resulting card should be readable as "12345" when input to reader

**Pass criterion**: Output punch creates correct hole pattern for mill values

### 4.4 I/O Assembly Test Report

```
I/O ASSEMBLY INTEGRATION TEST REPORT
Test date: [date]
Test technician: [name]
Test duration: [hours]

Pre-Test Checklist: ✅ PASS / ❌ FAIL

Functional Tests:
1. Card hopper operation (20 cards):
   - Feed success rate: [%] (target: 100%)
   - Result: ✅ PASS / ❌ FAIL

2. Card reader operation (5 test patterns):
   - Correct patterns detected: [N]/5
   - Result: ✅ PASS / ❌ FAIL

3. Card punch operation (10 blank cards):
   - Punch success rate: [%] (target: 100%)
   - Hole quality: [good/acceptable/poor]
   - Result: ✅ PASS / ❌ FAIL

4. Reader-to-mill linkage:
   - Test card pattern correctly controls mill: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

5. Output punch verification:
   - Mill output correctly punched: ✅ YES / ❌ NO
   - Result: ✅ PASS / ❌ FAIL

OVERALL I/O ASSEMBLY RESULT: ✅ PASS / ❌ FAIL

Issues found:
[List all issues and resolutions]

Approved by (QC engineer): ________________  Date: ________
```

---

## SECTION 5: SYSTEM INTEGRATION ASSEMBLY TESTING

### 5.1 Integration Overview

After all 5 subassemblies pass individual integration tests, they are connected together to form complete system:
- Mill connects to Barrel (program control)
- Store connects to Mill (read/write operations)
- I/O connects to all three (input cards, output punching)
- Main crankshaft drives all subassemblies in sync

**Integration assembly Week 38**  
**Integration testing Week 39**

### 5.2 System Synchronization Test

#### Test 5.2.1: Crank Synchronization

**Objective**: Verify all 5 subassemblies operate in synchronized timing (1 crank = all advance 1 position)

**Procedure**:
1. Mark initial positions of: digit stack (mill), store column, barrel position, card advance, punch position
2. Manually crank main drive shaft exactly 1 full rotation
3. Verify all subassemblies advanced exactly 1 position:
   - Mill digit wheels advanced 1 position
   - Store columns advanced 1 position
   - Barrel advanced 1 position
   - Card advanced 1 position
   - Punch advanced 1 position
4. Repeat for 10 full cranks
5. All should remain synchronized throughout

**Pass criterion**: All subassemblies synchronized; 1 crank = 1 position for all

#### Test 5.2.2: Cross-Module Integration Test

**Objective**: Verify subassemblies correctly communicate (card → mill → store → output)

**Procedure** (simple addition program):
1. Insert program card: "Read value A, read value B, add A+B, output result"
2. Insert input cards with values: A=3, B=4
3. Manually crank through complete program cycle:
   - Card 1 loads: mill reads "add 3" and updates store
   - Card 2 loads: mill reads "add 4" and updates store (mill now shows 7)
   - Output initiated: punch creates result card with value 7
4. Resulting output card should show value 7

**Pass criterion**: Output card correctly shows 7 (result of 3+4)

---

## CONCLUSION

Subassembly integration testing validates that each major component works correctly and that they synchronize properly when integrated. With all subassemblies passing, the system is ready for full system-level operational testing (covered in next document).

