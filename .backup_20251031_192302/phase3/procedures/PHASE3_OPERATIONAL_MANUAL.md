# PHASE 3: OPERATIONAL MANUAL & MAINTENANCE PROCEDURES
## Complete Guide to Operating and Maintaining the Babbage Analytical Engine

**Document Version**: 1.0  
**Status**: USER & MAINTENANCE MANUAL  
**Scope**: Daily operation, troubleshooting, preventive maintenance, long-term care  
**Audience**: Museum curators, academic researchers, skilled operators  

---

## PART 1: OPERATIONAL PROCEDURES

### Chapter 1: Pre-Operation Checklist

Before each operational session (daily, if machine is used regularly):

#### 1.1 Visual Inspection (5 minutes)

- [ ] Check for visible damage, loose bolts, or bent components
- [ ] Inspect all fasteners (no loose screws visible)
- [ ] Check for spilled lubricant or debris
- [ ] Verify card hopper is loaded (if running input programs)
- [ ] Verify output card collection bin is empty and ready

#### 1.2 Lubrication Check (5 minutes)

- [ ] Inspect lubricant level in oil reservoir (should be visibly wet at all bearing points)
- [ ] Add light machine oil (ISO VG 32) if level is low (~100 mL per session)
- [ ] Apply light oil to pivots, levers, and gear meshes (3-5 drops to each bearing point)
- [ ] Wipe excess oil with cloth to prevent dust accumulation

**Note**: Over-lubrication is better than under-lubrication; dry bearings are a common failure mode

#### 1.3 Mechanical Check (5 minutes)

- [ ] Hand-rotate main shaft slowly (1-2 complete rotations)
- [ ] Listen for any grinding, clicking, or unusual sounds
- [ ] Feel for smooth torque curve (should be relatively uniform)
- [ ] Check Store columns rotate freely (spot-check 3-5 random columns)
- [ ] Verify Barrel position marker (should be at position #1 if ready for new program)
- [ ] Check digit wheels display correct initial values (visually inspect leading zeros)

**If any issue detected**: Do NOT operate; consult troubleshooting section (Chapter 3)

**Total pre-operation time**: 15 minutes

---

### Chapter 2: Operating the Engine

#### 2.1 Loading Initial Values into Store (Memory)

**Purpose**: Set the starting values for computation

**Procedure**:

1. **Stop the machine** (if running from previous operation)
   - Ensure main shaft is not rotating
   - Place brake lever in locked position (if fitted)

2. **Access Store columns**:
   - Locate the column you want to set
   - Columns are numbered 0-1999 (left to right, top to bottom in most implementations)
   - For simplicity, use first 5 columns for typical test

3. **Manually position digit wheels** (Column 0, for example):
   - To set value "12345":
     - Digit position 0 (units): Rotate wheel to position 5
     - Digit position 1 (tens): Rotate wheel to position 4
     - Digit position 2 (hundreds): Rotate wheel to position 3
     - Digit position 3 (thousands): Rotate wheel to position 2
     - Digit position 4 (ten-thousands): Rotate wheel to position 1
   
   - **Rotation method**: Gently grip the digit wheel at its edge and rotate by hand
   - **Alignment method**: Look for the numerical marking on the wheel face (if engraved)
   - **Verification**: Double-check each digit position matches intended value

4. **Record starting values** (for later verification):
   - Column 0: 12345
   - Column 1: (next value)
   - [Continue for all values used]

5. **Visual confirmation**:
   - Look at Store display (digit wheels should show intended values)
   - Photograph or document starting state (valuable for historical record)

**Estimated time**: 15-30 minutes for 5 columns of data

#### 2.2 Loading Program onto Barrel

**Purpose**: Set the sequence of operations to perform

**Two methods**:

**Method A: Pre-punched Control Cards** (Recommended, if cards are available)

1. Prepare control card deck (cards pre-punched with operation sequence)
2. Load deck into Barrel input hopper
3. Manually advance barrel and load operations one by one
4. (Details depend on specific Barrel configuration)

**Method B: Manual Barrel Setup** (If no cards available)

1. Locate Barrel (cylindrical drum at top of machine)
2. Identify 150 position markers around circumference
3. For each operation in sequence:
   - Position Barrel to next available slot
   - Install mechanical "cam" or "lever" to encode the operation
   - Lock in place
4. Verify all operations are correctly positioned

**Estimated time**: 30-60 minutes for 20-operation program

#### 2.3 Hand-Cranking the Engine

**Purpose**: Execute the program by mechanically rotating main shaft

**Safety Precautions** ⚠️:

- **Wear appropriate clothing**: Long sleeves and hair must be tied back
- **Remove jewelry**: Rings, watches, or bracelets can catch in rotating parts
- **Two-person operation**: Recommended (one operator, one observer)
- **Emergency stop**: If machine jams or exhibits unusual behavior, STOP immediately
- **Know the location of the main brake**: In case of emergency shutdown

**Procedure**:

1. **Position operator**:
   - Stand to the side of main hand-crank (typically lower right of machine)
   - Feet shoulder-width apart, knees slightly bent
   - Grip hand-crank with dominant hand, support with other hand for stability

2. **Begin cranking** (slow, steady motion):
   - Start with hand-crank at bottom position (6 o'clock)
   - Rotate upward (12 o'clock → 9 o'clock → 6 o'clock)
   - Speed: ~1 complete rotation per 30 seconds (0.5 RPM, very slow)
   - Apply steady torque (~10-20 N force comfortable for extended periods)

3. **Monitor operation**:
   - **Listen**: Should hear regular mechanical sounds (gears meshing, levers clicking)
   - **Feel**: Should feel smooth, consistent resistance (no sudden increases)
   - **Observe**: Watch for Barrel advancing position (should advance ~1 position per crank)
   - **Watch Store**: Columns should advance when their control signal activates

4. **Continue operation**:
   - Complete one full program cycle (usually 150-200 hand-crank revolutions)
   - For longer programs, take breaks every 10-15 minutes to avoid hand fatigue

5. **Completion**:
   - Finish when Barrel returns to position #1 (or program end marker)
   - Stop rotating main shaft
   - Place brake lever in locked position

**Estimated time**: 
- Simple addition (2+3): ~10-15 minutes
- Moderate calculation: 20-40 minutes
- Complex calculation: 45-90 minutes

#### 2.4 Reading Output Results

**Purpose**: Retrieve computed result from output mechanism

**Method 1: Digit Wheel Display** (Direct visual reading)

1. Locate output column (typically Result column in Store)
2. Observe digit wheels showing final values
3. Read from left to right (high-order to low-order digits)
4. Record result: Example, digits show "5" = result is 5

**Method 2: Punch Card Output** (If I/O system equipped with punch)

1. Eject output card from punch mechanism
2. Examine punched holes:
   - Hole in position means "1"
   - No hole means "0"
3. Decode hole pattern according to Hollerith format (6-8 rows, 80 columns)
4. Convert to decimal: Read row-by-row, convert binary to decimal if needed

**Method 3: Photographed/Documented Record**

1. Photograph output Store display (digit wheels showing result)
2. Record in logbook with timestamp and program name
3. Retain photograph for archival purposes

**Estimated time**: 5-10 minutes to read and document result

---

### Chapter 3: Programming Examples

#### Example 1: Simple Addition (2 + 3 = 5)

**Setup**:
1. Set Store[0] = 00002 (number 2)
2. Set Store[1] = 00003 (number 3)
3. Set Store[2] = 00000 (result location, initially empty)

**Program**:
1. Load Store[0] into Mill accumulator
2. Add Store[1] to accumulator
3. Store result to Store[2]
4. Print result

**Hand-crank**:
- Rotate main shaft slowly (~10 minutes total)
- Monitor Barrel advancement
- Watch Mill digit wheels as operations execute

**Expected result**:
- Store[2] displays 00005

**Verification**:
- Read output: "5"
- Compare to expected: 2 + 3 = 5 ✓

---

#### Example 2: Factorial (5! = 120)

**Setup**:
1. Set Store[0] = 00005 (input number)
2. Set Store[1] = 00001 (running product, starts at 1)

**Program**:
1. Load Store[1] (product)
2. Multiply by Store[0] (decrementing counter)
3. Store result back to Store[1]
4. Decrement counter in Store[0]
5. If Store[0] > 0, jump back to step 1
6. Otherwise, print result

**Hand-crank**:
- Estimate 30-40 minutes for factorial computation
- Monitor Barrel for loop advancement

**Expected result**:
- Store[1] displays 00120 (5! = 120)

**Verification**:
- 1 × 5 = 5
- 5 × 4 = 20
- 20 × 3 = 60
- 60 × 2 = 120
- 120 × 1 = 120 ✓

---

## PART 2: MAINTENANCE PROCEDURES

### Chapter 4: Preventive Maintenance Schedule

#### 4.1 Daily Maintenance (If operated daily)

**Before each session** (already covered in Chapter 1):
- [ ] Visual inspection for damage (5 min)
- [ ] Lubrication check and top-up (5 min)
- [ ] Mechanical rotation test (5 min)

**After each session**:
- [ ] Wipe excess lubricant with cloth (2 min)
- [ ] Cover machine to protect from dust (1 min)
- [ ] Document any unusual observations in logbook (5 min)

**Time commitment**: 30 minutes daily

#### 4.2 Weekly Maintenance

**Every 7 days** (or after 50+ hours of operation):

- [ ] Deep inspection of all bearing points (look for residual debris, discoloration)
- [ ] Check fastener tightness:
  - Use wrench to verify all M8 and larger fasteners
  - Torque to specification (typically 15-20 N·m)
- [ ] Inspect digit wheel bores for visible damage or warping
- [ ] Check Barrel position markers for wear
- [ ] Clean card hopper and reader mechanism of dust/debris
- [ ] Verify oil level in reservoir (top up if below visible level)

**Time commitment**: 45-60 minutes

#### 4.3 Monthly Maintenance

**Every 30 days** (or after 200+ hours of operation):

- [ ] Complete oil change:
  - Drain oil reservoir into waste container
  - Inspect drained oil for metal shavings or debris (sign of wear)
  - Refill with fresh ISO VG 32 machine oil (quantity: ~500 mL)
  - If excessive metal debris visible, escalate to engineer for inspection
  
- [ ] Inspect gear teeth:
  - Remove digit wheel stack (if accessible)
  - Examine each gear for chatter marks, pitting, or broken teeth
  - If damage visible, mark component for rework or replacement
  
- [ ] Check bearing play:
  - Gently wiggle axles at each bearing point
  - Should have minimal play (< 0.5 mm)
  - If play is excessive, bearing may be worn and need replacement
  
- [ ] Test full program cycle:
  - Run simple test program (e.g., 1+1=2)
  - Verify result is correct
  - Listen for any new sounds or vibrations

**Time commitment**: 2-3 hours

#### 4.4 Quarterly Maintenance

**Every 90 days** (or annually if not heavily used):

- [ ] Complete disassembly and inspection of one major subassembly (rotate: Mill → Store → Barrel → I/O)
  - Remove subassembly bolts carefully (preserve gaskets/shims)
  - Inspect all internal components for wear, cracks, rust
  - Clean all surfaces with cloth
  - Re-assemble with fresh lubricant
  
- [ ] Inspect control levers and linkages:
  - Check for cracks or bending
  - Verify smooth operation of all levers
  - Check return springs (should snap back smartly)
  
- [ ] Verify Barrel position indexing:
  - Manually advance Barrel through full 150-position cycle
  - Count positions to verify 150-position design
  - Check for skipped or misaligned positions
  
- [ ] Calibration verification:
  - Load known test values and verify Store displays them correctly
  - Run 5-10 test programs to verify calculation accuracy
  - Document any discrepancies

**Time commitment**: 4-6 hours

#### 4.5 Annual Comprehensive Overhaul

**Once per year**:

- [ ] Complete disassembly of all subassemblies
- [ ] Individual inspection and testing of each component
- [ ] Replacement of any worn bearings, springs, or fasteners
- [ ] Complete re-lubrication and reassembly
- [ ] Full 20-program test suite to verify all functions
- [ ] Professional engineering review (if available)

**Time commitment**: 30-40 hours over 1-2 weeks

**Estimated cost**: £500-1,000 (parts + labor, if using external technician)

---

### Chapter 5: Troubleshooting Guide

#### 5.1 Common Issues & Solutions

##### Issue 1: Machine Won't Rotate (Binding)

**Symptoms**:
- Cannot turn hand-crank at all
- Extreme resistance despite good lubrication
- No mechanical sounds

**Likely causes**:
1. Bearing seized (dry, corroded, or mechanically damaged)
2. Gear mesh too tight (teeth interfering)
3. Misaligned subassembly (bent shaft)
4. Foreign object lodged in mechanism

**Diagnosis**:
1. Stop immediately (do not force rotation)
2. Remove hand-crank handle
3. Try rotating Mill subassembly manually (can you move digit wheels?)
4. Try rotating Store columns (check if issue is specific to one subassembly)
5. Try rotating Barrel (check if issue is in control mechanism)

**Solutions**:

**If Mill is bound**:
- [ ] Lubricate all Mill bearing points (10 drops each)
- [ ] Wait 5 minutes for oil to penetrate
- [ ] Try gentle rocking motion (5-10 mm rock, not full rotation)
- [ ] If still bound, check for bent digit wheel or misaligned gear
- [ ] If misalignment confirmed, disassemble and correct

**If Store is bound**:
- [ ] Check one column at a time (isolate which is stuck)
- [ ] Lubricate that column's bearings
- [ ] If still stuck, likely bearing or shaft damage (replace bearing)

**If Barrel is bound**:
- [ ] Check Barrel can rotate freely without connected drive mechanism
- [ ] If freely rotatable, issue is in drive coupling or synchronization mechanism
- [ ] If Barrel itself is bound, bearing replacement needed

**Prevention**:
- Daily lubrication (prevents drying and seizure)
- Proper storage in climate-controlled environment (prevents rust and corrosion)

**Estimated resolution time**: 30 minutes to 2 hours (depending on severity)

---

##### Issue 2: Grinding Sound During Operation

**Symptoms**:
- Grinding or scraping sound heard during hand-cranking
- Sound increases with rotation speed
- May feel slight resistance to rotation

**Likely causes**:
1. Gear tooth chatter or damage (chipped tooth, misaligned mesh)
2. Debris (small metal shaving or dust) lodged in gear mesh
3. Bearing wear (ball races damaged, creating grinding)
4. Lubrication breakdown (gritty oil from metal particles)

**Diagnosis**:
1. Stop immediately (grinding indicates damage in progress)
2. Locate sound source:
   - Mill grinding? → Check digit wheel gears
   - Store grinding? → Check synchronization gears
   - Barrel grinding? → Check barrel drive coupling
3. Inspect affected gear teeth:
   - Remove subassembly if needed
   - Look for obvious damage (broken tooth, metal flake)
   - Check for visible metal shavings in oil

**Solutions**:

**If debris visible**:
- [ ] Drain oil completely
- [ ] Flush mechanism with clean oil (rotate gently 5-10 times)
- [ ] Drain again and refill with fresh oil
- [ ] Rotate manually to verify grinding is gone
- [ ] If grinding persists, tooth damage is likely

**If tooth damage confirmed**:
- [ ] Stop operation (continued grinding will damage other teeth)
- [ ] Remove damaged gear component
- [ ] Replace with spare (if available) or have machined
- [ ] Re-assemble and test

**Prevention**:
- Monthly oil changes (prevent metal debris accumulation)
- Quarterly inspection of all gears
- Avoid forcing rotation (indicates binding, not normal operation)

**Estimated resolution time**: 1-4 hours (depending on component accessibility)

---

##### Issue 3: Incorrect Calculation Results

**Symptoms**:
- Program executes but result is mathematically wrong
- Example: 2+3 produces 6 instead of 5
- Problem consistent across multiple test programs

**Likely causes**:
1. Digit wheel stuck (not advancing when it should)
2. Carry mechanism malfunction (carry signal not propagating)
3. Lever misalignment (control signal not reaching intended digit)
4. Mechanical linkage bent (not engaging gears properly)
5. Operator error (misread or misset initial values)

**Diagnosis**:
1. Verify initial setup (re-read Store[0] and Store[1] values manually)
2. Run simplest possible test: Set Store[0] = 1, Store[1] = 0, Compute Store[0] + Store[1]
3. Observe result carefully:
   - Should be 1 (same as Store[0])
   - If result is different, addition mechanism is broken
4. Test carry: Set Store[0] = 9, Store[1] = 1, Compute Store[0] + Store[1]
   - Should be 10 (with carry to tens digit)
   - If result is 10 instead of 11, carry mechanism failed

**Solutions**:

**If digit wheel stuck**:
- [ ] Manually rotate stuck wheel through full cycle (0→1→...→9→0)
- [ ] Check for binding or resistance
- [ ] Apply light oil to bore
- [ ] Re-test operation

**If carry mechanism fails**:
- [ ] Test manually: Advance digit wheel from 9 to 0, verify next digit advances
- [ ] If next digit doesn't advance, carry linkage is misaligned or broken
- [ ] Loosen lever fasteners, adjust position, retighten
- [ ] Re-test

**If control lever misaligned**:
- [ ] Visually inspect lever position during operation (watch it move)
- [ ] Lever should engage gear with ~1 mm clearance
- [ ] If lever is too high/low, adjust mounting bolts
- [ ] Verify lever snaps back to neutral after engagement

**Prevention**:
- Weekly mechanical inspection (check all levers move freely)
- Test carry mechanism manually every month (ensure 9→0 carry works)
- Document calculation results (compare to historical patterns)

**Estimated resolution time**: 1-3 hours

---

##### Issue 4: Slow or Stiff Rotation

**Symptoms**:
- Hand-crank requires more effort than usual (heavier torque)
- Rotation speed slows over time during operation
- No grinding sounds (rules out gear damage)

**Likely causes**:
1. Insufficient lubrication (oil depleted, viscosity wrong)
2. Bearing wear (increased friction as bearings age)
3. Gear misalignment (increasing friction)
4. Seasonal change (winter oil thickens in cold)

**Diagnosis**:
1. Check oil level (should be visibly wet at bearing points)
2. Check oil condition:
   - Drain small sample into white container
   - Should be light amber color (fresh oil = clear to light yellow)
   - If dark brown or black, oil is spent (contaminated)
3. Check lubrication points:
   - Feel bearing surfaces with finger
   - Should be slightly oily to touch
   - If dry, lubrication is depleted

**Solutions**:

**If oil is depleted**:
- [ ] Add fresh ISO VG 32 oil (~100-200 mL)
- [ ] Rotate machine 5-10 times by hand
- [ ] Check torque (should feel lighter)
- [ ] If stiffness remains, proceed to next diagnostic

**If oil is contaminated**:
- [ ] Perform complete oil change (drain old, refill fresh)
- [ ] Rotate machine 10-20 times under moderate load
- [ ] Check torque improvement

**If bearing wear is suspected**:
- [ ] Wiggle each axle at bearing points
- [ ] If play is > 1 mm, bearing is worn
- [ ] Identify worn bearing location, mark for replacement
- [ ] Continue operation (note: bearing will continue to degrade)
- [ ] Plan replacement during next quarterly maintenance

**If seasonal**:
- [ ] Switch to lighter oil in winter (ISO VG 10-22)
- [ ] Switch to standard oil in summer (ISO VG 32)
- [ ] Rotate and verify torque improves

**Prevention**:
- Monthly oil changes during regular use
- Summer/winter oil selection based on climate
- Quarterly bearing play verification

**Estimated resolution time**: 30 minutes (most cases)

---

### Chapter 6: Component Replacement Guide

#### 6.1 Digit Wheel Replacement

**When to replace**:
- Tooth broken or badly chattered
- Bore out of tolerance (> 0.05 mm runout)
- Cannot be reworked (cost exceeds replacement)

**Procedure**:

1. **Disassemble digit wheel stack** (tedious, 2-3 hours):
   - Remove end plate fasteners
   - Carefully slide wheels off axle one by one
   - Inventory wheels and spacing gears
   - Identify which wheel is damaged

2. **Remove damaged wheel**:
   - Remove retaining clip
   - Slide wheel off axle

3. **Install new wheel** (with pre-manufactured spare):
   - Slide new wheel onto axle in correct position
   - Install spacing gear
   - Install retaining clip

4. **Re-assemble digit wheel stack**:
   - Slide wheels back onto axle in reverse order
   - Install end plate and fasteners
   - Torque fasteners to 15-20 N·m
   - Verify free rotation

**Time required**: 3-4 hours

**Cost**: £5-10 per wheel (if spare available)

---

#### 6.2 Bearing Replacement

**When to replace**:
- Play > 1 mm at bearing point
- Grinding sound emanating from bearing
- Excessive friction (cannot be corrected by lubrication)

**Procedure**:

1. **Identify bearing location**:
   - Pinpoint which bearing is failing (wiggle test)
   - Note bearing specification (e.g., SKF 6001 10mm bore)

2. **Order replacement bearing**:
   - Contact SKF or local bearing supplier
   - Lead time: typically 1-2 weeks
   - Cost: £10-30 per bearing

3. **Remove subassembly** (containing failed bearing):
   - Unbolt subassembly from main frame
   - Carefully lower onto work surface

4. **Access bearing**:
   - Remove fasteners holding bearing block
   - Carefully extract bearing (may require bearing puller tool)

5. **Install new bearing**:
   - Press new bearing into bore (use bearing press or careful hammer-tap)
   - Verify bearing seats flush in bore
   - No binding when rotated by hand

6. **Re-assemble subassembly**:
   - Reinstall on main frame
   - Torque all fasteners
   - Verify subassembly rotates freely

**Time required**: 2-3 hours

**Cost**: £10-30 per bearing + bearing press tool (if not available)

---

#### 6.3 Spring Replacement

**When to replace**:
- Return spring weak (doesn't snap back smartly)
- Spring cracked or deformed
- Carrying mechanism sluggish (carry levers don't reset quickly)

**Procedure**:

1. **Identify spring location** (typically on carry lever or Barrel return)

2. **Remove old spring**:
   - Unbolt spring anchors
   - Carefully remove spring (mind tension)

3. **Measure spring specification**:
   - Free length, coil diameter, wire diameter
   - Tension: light (typically 5-20 N)

4. **Order replacement spring**:
   - Local spring maker or supplier
   - Specify free length, diameter, material (steel), tension
   - Lead time: 1-2 weeks
   - Cost: £5-15 per spring

5. **Install new spring**:
   - Bolt spring anchors to frame
   - Verify spring tension is appropriate (should return lever smoothly)
   - Test: Push lever by hand, release, verify quick return

**Time required**: 30 minutes to 1 hour

**Cost**: £5-15 per spring

---

### Chapter 7: Long-Term Storage

#### 7.1 Storage Conditions

**Environment**:
- Temperature: 15-25°C (avoid extreme heat or cold)
- Humidity: 40-60% relative humidity (avoid wet or dry extremes)
- Location: Indoors, protected from dust, dirt, and moisture
- Cover: Use dust cover when not in use

**Why**: 
- Steel rusts in wet conditions
- Oil oxidizes and becomes gummy in heat
- Lubricant thickens in cold
- Dust accumulation causes abrasion

#### 7.2 Pre-Storage Preparation (If storing > 3 months)

1. **Complete oil change**:
   - Drain old oil completely
   - Refill with fresh ISO VG 32
   - Run machine 10-20 rotations
   - This prevents oxidation of old oil during storage

2. **Coat exposed metal** (optional, recommended for museum):
   - Apply thin film of protective oil to unpainted surfaces
   - Use soft cloth to wipe excess (prevent gummy buildup)

3. **Verify all fasteners are tight**:
   - Use wrench to tighten all bolts/screws
   - Prevents vibration loosening during storage

4. **Place desiccant** (if in humid environment):
   - Silica gel packets inside machine covers
   - Replace monthly to keep humidity low

5. **Document storage date and condition**:
   - Photograph machine from multiple angles
   - Write detailed description of storage state
   - Note any known issues requiring repair

#### 7.3 Monthly Check During Storage

Even if machine is not operated, perform monthly check:

1. **Visual inspection** (5 minutes):
   - Look for rust, corrosion, or deterioration
   - Check for pest damage or debris

2. **Lubrication check** (2 minutes):
   - Verify oil is still present (not dried out)
   - If needed, apply light oil to exposed bearing surfaces

3. **Desiccant replacement** (2 minutes):
   - Replace silica gel packets if present
   - Prevents humidity from rising

#### 7.4 Reactivation After Storage

Before operating machine after storage > 3 months:

1. **Complete pre-operation checklist** (Chapter 1):
   - Visual inspection
   - Lubrication check
   - Mechanical rotation test

2. **Test run**:
   - Rotate by hand slowly (listen for any unusual sounds)
   - Run simple test program (1+1=2)
   - Verify result is correct

3. **If issues found**:
   - Consult troubleshooting guide (Chapter 5)
   - Address issues before extended operation

---

## PART 3: REFERENCE & DOCUMENTATION

### Chapter 8: Spare Parts List & Ordering

#### Critical Spares to Keep in Stock

| Part | Quantity | Lead Time | Supplier | Cost | Notes |
|---|---|---|---|---|---|
| Digit wheel (spare) | 5 | On-hand | Manufacture/stock | £5 each | Replace broken teeth |
| Ball bearing (SKF) | 5 | 1-2 weeks | SKF or distributor | £15 each | Common wear item |
| Return spring | 10 | 1-2 weeks | Spring maker | £10 each | Carries & levers |
| Gear hob (spare) | 1 | On-hand | Tooling supplier | £45 | If re-manufacturing wheels |
| Machine oil (5 L) | 1 | On-hand | Local supplier | £20 | Monthly changes |
| Fasteners (assorted) | 100 units | On-hand | Hardware supplier | £5 | Replacement bolts/screws |

**Total spare parts cost**: ~£200-300

**Storage**: Keep in dry location, labeled clearly, inventory checked quarterly

---

### Chapter 9: Logbook & Documentation

#### 9.1 Operational Logbook Template

**Date**: _________  
**Session number**: _________  
**Operator**: _________  
**Program executed**: _________  
**Initial values**: _________  
**Expected result**: _________  
**Actual result**: _________  
**Pass/Fail**: _________  
**Notes/Issues**: _________  
**Maintenance performed**: _________  
**Oil level**: [ ] Fresh [ ] Low [ ] Empty  
**Unusual observations**: _________  

#### 9.2 Maintenance Log

**Record every maintenance action**:
- Date
- Type (daily/weekly/monthly/quarterly/emergency)
- Actions performed
- Components inspected/replaced
- Operator/technician name
- Next scheduled maintenance

**Retained for**: 5 years minimum (valuable for historical documentation)

---

### Chapter 10: Useful References

#### A. Technical Specifications

- Main frame dimensions: 2 m × 1.5 m × 1 m (H)
- Total weight: ~500-800 kg
- Mill capacity: 5-digit numbers
- Store capacity: 2,000 columns × 50 digits (50-digit numbers)
- Barrel program capacity: 150 operations
- Operating speed: 0.5 RPM typical hand-crank (1 minute per complete revolution)
- Manual calculation time (average program): 20-40 minutes

#### B. Supplier Contact Information

**Steel suppliers**:
- Tata Steel (India): [Contact information]
- David Brown Ltd. (UK): [Contact information]

**Bearing suppliers**:
- SKF (Sweden): [Contact information]

**Lubricant suppliers**:
- [Local industrial oil distributor]

---

## END OPERATIONAL MANUAL

