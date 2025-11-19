# PHASE 3: QUALITY CONTROL & VALIDATION FRAMEWORK
## Complete QC Procedures, Measurement Standards, Defect Classification, and Acceptance Criteria

**Document Version**: 2.0  
**Status**: QUALITY ASSURANCE SPECIFICATION  
**Scope**: Component-level QC, assembly validation, system-level testing  
**Objective**: Achieve 80%+ system functionality with < 5% critical defects  

---

## SECTION 1: INCOMING MATERIAL INSPECTION

### Purpose
Verify all purchased materials meet supplier specifications before entering manufacturing

### Inspection Schedule

| Material | Supplier | First Lot | Ongoing | Frequency |
|---|---|---|---|---|
| Steel bar stock (AISI 1045) | Tata Steel or David Brown | 100% | 10% | Every 1,000 units |
| Fasteners (screws, bolts) | Local supplier | 100% | 5% | Every 5,000 units |
| Ball bearings (SKF) | David Brown | 100% | 10% | Every 100 units |
| Cutting tools (hobs, endmills) | Supplier | 100% | 50% | Every new tool |
| Lubricant (machine oil 32 cSt) | Local | 1 sample | 1 per barrel | Every 100-liter drum |
| Cooling fluid | Local | 1 sample | 1 per week | Weekly |

### Inspection Procedures

#### 1.1 Steel Bar Stock (Digit Wheels, Shafts, etc.)

**Acceptance Criteria**:

| Parameter | Specification | Tolerance | Measurement Method |
|---|---|---|---|
| Diameter | 12.5 mm (digit wheel) | ±0.10 mm | Caliper or micrometer |
| Diameter | 10 mm (shaft) | ±0.10 mm | Caliper or micrometer |
| Surface | No visible cracks | N/A | Visual inspection |
| Surface | No deep rust (surface rust acceptable) | N/A | Visual inspection |
| Straightness | Bars should not be visibly bent | ±5 mm per meter | Lay on flat surface, check gaps |

**First Lot Procedure** (100% inspection):
1. Measure first 10 pieces (diameter at 5 locations each)
2. Measure last 10 pieces
3. If all within tolerance, accept 100% of lot
4. If any out of tolerance, reject entire lot and notify supplier

**Ongoing Procedure** (10% sampling):
1. Randomly select 10% of delivery
2. Measure as above
3. If all in tolerance, accept lot
4. If > 1 piece out of tolerance, inspect 100% of lot

**Time per inspection**: 30-45 minutes per 1,000-unit lot

**Documentation**: Material acceptance log (retained for 2 years)

#### 1.2 Fasteners

**Acceptance Criteria**:

| Parameter | Spec | Method |
|---|---|---|
| Thread quality | Smooth, no visible damage | Go/no-go gauge, hand-screw test |
| Head size | M6 screws: 6 mm ±0.2 mm | Caliper |
| Length | Per specification ±0.5 mm | Caliper or go/no-go gage |
| Plating | Uniform, no bare steel visible | Visual inspection |

**Sampling**: 5% of delivery (minimum 50 pieces if lot < 1,000)

**Time per inspection**: 45-60 minutes per 8,000-unit delivery

#### 1.3 Bearings (SKF or equivalent)

**Acceptance Criteria**:

| Parameter | Spec | Tolerance | Method |
|---|---|---|---|
| Bore ID | 10 mm (shaft bearing) | ±0.01 mm | SKF test certificate (use supplier data) |
| Outer diameter | 20 mm | ±0.02 mm | SKF test certificate |
| Radial play | < 0.05 mm | N/A | Manual rotation test, feel for looseness |
| Rotation | Smooth, no grinding | N/A | Hand-spin bearing, listen for noise |

**Sampling**: 10% of delivery (minimum 5 bearings)

**Time per inspection**: 20-30 minutes per bearing tested

**Documentation**: SKF test certificates (typically supplied with premium bearings; request if not included)

---

## SECTION 2: IN-PROCESS QUALITY CONTROL (SPC)

### Purpose
Monitor manufacturing processes in real-time to detect drift before defects occur

### Statistical Process Control (SPC) Methodology

**Control Chart Type**: X-bar & R (mean and range chart)

**Sampling Strategy**:
- Measure first piece of each production run
- Measure every 5th-25th piece (depends on operation)
- Measure last piece before tool change
- Plot on control chart
- Action if mean drifts > 0.5 standard deviations from target

### Component-Specific SPC Procedures

#### 2.1 Digit Wheel Boring (Critical dimension: bore 12.50 mm ±0.05 mm)

**Measurement**:
- Tool: 3-point internal micrometer (precision ±0.01 mm)
- Sampling: Every 25th wheel (2% sampling rate)
- Target: 12.50 mm
- Upper control limit (UCL): 12.53 mm
- Lower control limit (LCL): 12.47 mm
- Action limit: If 2 consecutive pieces > 12.53 mm OR < 12.47 mm, STOP and adjust machine

**Procedure**:
1. Remove wheel #25 from lathe
2. Wipe clean with cloth
3. Measure bore with internal micrometer at 3 locations (top, middle, bottom of bore depth)
4. Record mean diameter
5. Plot on X-bar chart
6. If plot crosses action limit, stop production, investigate machine drift

**Frequency**: Continuous during production (one sample every ~7 minutes)

**Documentation**: SPC chart maintained daily

**Time per sample**: 5-10 minutes (measurement + recording)

#### 2.2 Shaft OD after Grinding (Target: 10.00 mm ±0.02 mm)

**Measurement**:
- Tool: Micrometer (±0.01 mm precision)
- Sampling: Every 20th shaft
- UCL: 10.02 mm
- LCL: 9.98 mm
- Action: If 2 consecutive > UCL or < LCL, adjust grinder wheel

**Procedure**: Same as digit wheel boring

#### 2.3 Sector Wheel Bore (Target: 15.00 mm ±0.02 mm)

**Measurement**:
- Tool: Bore gauge + micrometer
- Sampling: Every 50th wheel
- UCL: 15.02 mm
- LCL: 14.98 mm

#### 2.4 Bearing Bore Honing (Target: 12.00 mm ±0.02 mm) — MOST CRITICAL

**Measurement**:
- Tool: Telescopic bore gauge + precision micrometer (±0.01 mm)
- Sampling: EVERY 10TH BORE (10% inspection, higher than others)
- UCL: 12.02 mm
- LCL: 11.98 mm
- Action: If ANY piece exceeds tolerance, STOP and investigate honing stone wear

**Procedure**:
1. Remove bore from honing machine after cooling (2-minute wait)
2. Measure with telescopic bore gauge
3. Record measurement
4. Plot on X-bar chart
5. If out of tolerance, REJECT bore and scrap (honing is difficult to rework)

**Frequency**: Continuous during 133-hour bearing bore production

**Time per sample**: 8-12 minutes (includes cooling wait time)

---

## SECTION 3: FINAL COMPONENT INSPECTION

### Purpose
100% inspection before components enter assembly

### Inspection Checklist (per component type)

#### 3.1 Digit Wheels (5,000 total) — MOST CRITICAL

**Critical dimensions**:
- Bore diameter: 12.50 mm ±0.05 mm
- Bore concentricity (TIR): < 0.05 mm

**Quality parameters**:
- Gear tooth profile: No chatter marks, hob lines, or broken teeth
- Surface finish: No visible scratches or grinding damage
- Deburring: No sharp edges on gear teeth
- Rotation: Smooth when spun on test arbor (no binding)

**Inspection procedure**:
1. Visual inspection (10× magnification): Examine teeth for profile damage
2. Measure bore with precision caliper or micrometer (3 points)
3. Check concentricity on precision spindle (rotate wheel, measure bore TIR with dial indicator)
4. Spin wheel on test arbor, listen for smooth rotation (no grinding sounds)

**Acceptance criteria**:
- ✓ Bore within tolerance (12.50 mm ±0.05 mm)
- ✓ Concentricity < 0.05 mm TIR
- ✓ No visible chatter marks or broken teeth
- ✓ Smooth rotation on test arbor
- ✓ Deburring complete (no sharp edges)

**Defect categories**:
- **Critical (SCRAP)**: Broken teeth, bore out of tolerance, concentricity > 0.05 mm
- **Major (REWORK)**: Chatter marks (stoneable), minor deburring (restonable)
- **Minor (ACCEPT)**: Surface finish acceptable, no functional impact

**Expected yield**: 94-96% acceptable on first inspection; 70% of defective wheels can be reworked

**Time per wheel**: 2-3 minutes (visual + measurement)

**Inspection capacity**: 1,200 wheels per 8-hour shift (single inspector)

**Timeline**: 5,000 wheels ÷ 1,200/shift = 4.2 shifts = **0.5 weeks** (well within schedule)

#### 3.2 Shafts (600 total)

**Critical dimensions**:
- OD: 10.00 mm ±0.02 mm
- Runout (TIR): < 0.03 mm over 100 mm length

**Quality parameters**:
- Surface finish: Ra < 1.6 microns (no visible tool marks)
- Surface defects: No cracks, no deep scratches
- Straightness: No visible bending

**Inspection procedure**:
1. Measure OD with micrometer (3 locations)
2. Check runout on 4-jaw chuck on lathe spindle (rotate, measure with dial indicator)
3. Visual inspection for cracks (under magnification if needed)

**Acceptance criteria**:
- ✓ OD within tolerance
- ✓ Runout < 0.03 mm
- ✓ No surface cracks or deep scratches

**Defect handling**:
- **Critical (SCRAP)**: OD out of tolerance, runout > 0.05 mm, visible cracks
- **Major (REWORK)**: Runout 0.03-0.05 mm (can be lightly ground)
- **Minor (ACCEPT)**: Surface scratches acceptable

**Expected yield**: 97-98%

**Time per shaft**: 3-5 minutes

**Timeline**: 600 shafts ÷ (15 per shift) = 40 hours = **5 shifts = 0.6 weeks**

#### 3.3 Sector Wheels (2,000 total)

**Critical dimensions**:
- Bore ID: 15.00 mm ±0.02 mm
- Concentricity: TIR < 0.05 mm

**Inspection procedure**: Same as digit wheels (bore-critical)

**Expected yield**: 95-96%

**Time per wheel**: 2-3 minutes

**Timeline**: 2,000 ÷ 1,200/shift = 1.7 shifts = **0.2 weeks**

#### 3.4 Bearing Bores (2,000 total) — HIGHEST PRECISION

**Critical dimensions** (MOST STRINGENT):
- ID: 12.00 mm ±0.02 mm (critical for bearing fit)
- Surface finish: Ra < 0.4 microns (critical for bearing life)
- Concentricity: TIR < 0.02 mm (critical for bearing life)

**Inspection procedure**:
1. Measure ID with telescopic bore gauge + precision micrometer (±0.01 mm accuracy)
2. Assess surface finish by touch (fingernail should not scratch — Ra < 0.4 microns)
3. Check concentricity on spindle with dial indicator

**Acceptance criteria** (very strict):
- ✓ ID 12.00 mm ±0.02 mm
- ✓ Surface finish Ra < 0.4 microns
- ✓ Concentricity TIR < 0.02 mm

**Defect handling**:
- **Critical (SCRAP)**: Bore out of tolerance, finish > Ra 0.8 microns, concentricity > 0.05 mm
- **Rework**: Minimal — most out-of-tolerance bores are scrapped (rework cost > replacement)

**Expected yield**: 96-98%

**Time per bore**: 4-6 minutes (precision measurement)

**Timeline**: 2,000 ÷ (10 per shift) = 200 hours = **25 shifts = 3.1 weeks**

#### 3.5 Fasteners (8,000 total) — Sampling allowed

**Sampling strategy**: 10% random sampling (800 fasteners from batch of 8,000)

**Inspection procedure**:
1. Select 800 fasteners randomly from delivery
2. For each: Thread through go/no-go gauge (measure thread pitch diameter)
3. Measure head size with caliper
4. Measure length with caliper

**Acceptance criteria**:
- ✓ Threads pass go/no-go gauge (fit smoothly)
- ✓ Head size within ±0.1 mm
- ✓ Length within ±0.2 mm
- ✓ Plating uniform (no bare metal visible)

**Defect rate**: Target < 2% defective in sample (allows up to 16 defective fasteners in sample of 800)

**Time per fastener**: 1-2 minutes

**Timeline**: 800 fasteners ÷ 40 per shift = 20 hours = **2.5 shifts = 0.3 weeks**

---

## SECTION 4: ASSEMBLY VALIDATION

### Purpose
Verify subassemblies meet functional requirements before final integration

### 4.1 Mill Subassembly Validation

**Test procedure**:
1. Hand-crank Mill at low speed (1 revolution per 30 seconds)
2. Listen for grinding, binding, or unusual sounds
3. Feel for smooth torque curve
4. Advance wheels through several complete cycles (0→9→0)
5. Verify carry mechanism propagates correctly (units→tens→hundreds)

**Pass criteria**:
- ✓ No grinding or binding sounds
- ✓ Smooth, consistent torque
- ✓ Carry logic propagates through all 5 digits
- ✓ Free rotation confirmed

**Fail criteria**:
- ✗ Grinding or binding detected
- ✗ Carry mechanism skips or sticks
- ✗ Wheels difficult to rotate manually

**Remediation**:
- If binding: Disassemble, inspect for misaligned gears or bent levers
- If carry fails: Check sector wheel engagement surfaces
- Rework time: 4-8 hours per issue

**Time to validate**: 2-3 hours per Mill subassembly

### 4.2 Store Subassembly Validation

**Test procedure**:
1. Verify all 2,000 columns rotate freely
2. Spot-check 10 random columns: Advance through 0→9→0 cycle
3. Check synchronization: All columns advance together

**Pass criteria**:
- ✓ All 2,000 columns rotate freely (no binding)
- ✓ Spot-check columns function correctly
- ✓ Synchronization confirmed

**Time to validate**: 3-4 hours (10 columns × 20 minutes each + spot checks)

### 4.3 Barrel Subassembly Validation

**Test procedure**:
1. Manually rotate barrel through full 150-position cycle
2. At each position, verify reader mechanism engages
3. Verify 150 positions are accurately spaced

**Pass criteria**:
- ✓ Barrel rotates smoothly
- ✓ Reader engages at each position (150 times)
- ✓ No skipped or misaligned positions

**Time to validate**: 1-2 hours

### 4.4 I/O Subassembly Validation

**Test procedure**:
1. Load test card with known hole pattern
2. Manually feed through reader
3. Verify all holes detected
4. Trigger punch mechanism on output card
5. Verify punch holes match expected pattern

**Pass criteria**:
- ✓ Reader correctly detects all input holes
- ✓ Punch mechanism fires all pins simultaneously
- ✓ Output hole pattern matches input

**Time to validate**: 1-2 hours

---

## SECTION 5: SYSTEM-LEVEL TESTING

### Purpose
Verify complete Babbage Analytical Engine operates correctly

### 5.1 Functional Testing (20-Program Test Suite)

**Test program development**:

| Program # | Operation | Input | Expected Output | Test Method | Pass/Fail |
|---|---|---|---|---|---|
| 1 | Load test | Set Store[0] = 00001 | Read Store[0] = 00001 | Manual read | [ ] |
| 2 | Simple add | 2 + 3 | 5 | Punch card | [ ] |
| 3 | Subtraction | 10 - 3 | 7 | Punch card | [ ] |
| 4 | Multiplication | 5 × 6 | 30 | Punch card | [ ] |
| 5 | Division | 12 ÷ 3 | 4 | Punch card | [ ] |
| 6 | Multi-digit add | 1234 + 5678 | 6912 | Punch card | [ ] |
| 7 | Carry propagation | 9999 + 1 | 10000 (with carry) | Punch card | [ ] |
| 8 | Factorial | 5! | 120 | Punch card | [ ] |
| 9 | Polynomial | x²+2x+1 (x=3) | 16 | Punch card | [ ] |
| 10 | Financial calc. | Compound interest (P=100, r=5%, n=10) | 162.89 | Punch card | [ ] |
| 11-20 | Various | [Additional operations] | [Expected results] | Punch card | [ ] |

**Test success criteria**:
- **Pass**: Test produces expected output (exact match, or ±0.01 for financial calculations)
- **Fail**: Test produces incorrect output or machine fails to complete operation

**Overall success metric**:
- **Target**: 80% of tests pass (16 of 20 correct)
- **Acceptable**: 75%+ pass (15 of 20 correct)
- **Unacceptable**: < 75% pass (< 15 correct) — requires redesign/rework

### 5.2 Stress Testing

**Purpose**: Verify machine can handle extended operation without failure

**Procedure**:
1. Load same test program repeatedly (Program #2: 2+3=5)
2. Operate for 10 complete cycles (0.5-1 hour continuous hand-cranking)
3. Monitor for:
   - Bearing wear (increasing friction)
   - Gear wear (increasing backlash)
   - Lubrication consumption (oil level dropping)
   - Mechanical fatigue (cracks, binding)

**Success criteria**:
- ✓ All 10 cycles complete successfully
- ✓ No significant increase in friction
- ✓ No visible damage to components
- ✓ Lubrication level adequate (no dry bearings)

**Time**: 1-2 hours continuous operation + 2 hours inspection

---

## SECTION 6: DEFECT CLASSIFICATION & HANDLING

### Defect Categories

#### Critical Defects (Component-level)
- Bore out of tolerance (cannot be corrected in assembly)
- Tooth damage/missing (cannot be reworked)
- Surface cracks (safety risk)
- Runout > tolerance (cannot be corrected)

**Disposition**: SCRAP (100% replacement required)

**Rework rate**: 0% (not economical to rework)

**Cost impact**: Component cost + replacement time

#### Major Defects (Marginal, reworkable)
- Chatter marks on gear teeth (stoneable)
- Bore slightly out of tolerance but within 2× spec (can be re-honed)
- Minor deburring issues (can be redone)
- Surface scratches that don't affect function

**Disposition**: REWORK (if cost-effective)

**Rework rate**: 60-80% success (some rework attempts fail)

**Time**: 0.5-2 hours per component rework

**Cost**: Material loss ~30%; labor ~2× cost of new part typically; recommend scrap if > 5% of component cost

#### Minor Defects (Cosmetic, no functional impact)
- Surface finish slightly rough but functional
- Small deburrs on non-critical edges
- Slight discoloration/oxidation (acceptable)

**Disposition**: ACCEPT (no rework needed)

**Impact on function**: None

### Aggregate Yield Analysis

**Target yield by component**:

| Component | Critical Defects | Major (Rework) | Minor (Accept) | First-Pass Yield |
|---|---|---|---|---|
| Digit wheels | 4% | 2% | 0% | 94% |
| Shafts | 2% | 1% | 0% | 97% |
| Sector wheels | 3% | 2% | 0% | 95% |
| Levers | 6% | 2% | 0% | 92% |
| Bearing bores | 2% | 2% | 0% | 96% |
| Fasteners | 1% | 1% | 0% | 98% |

**Rework impact**:
- Major defects reworked at 70% success rate
- Final yield after rework: 96-98% across all components

**Scrap rate**: 2-4% (components that fail quality after rework)

**Schedule impact**: Rework adds 1-2 weeks contingency (built into 34-week timeline)

---

## SECTION 7: DOCUMENTATION & RECORD-KEEPING

### Quality Records (Retained for 2 years minimum)

1. **Material acceptance logs** (incoming inspection)
2. **SPC control charts** (in-process monitoring, daily)
3. **Final inspection logs** (component-by-component acceptance/rejection)
4. **Defect logs** (detailed defect records with photos if possible)
5. **Rework logs** (tracking rework attempts and results)
6. **Assembly validation records** (test results for subassemblies)
7. **System test results** (20-program test suite with pass/fail)

### Quality Metrics (Track weekly)

- Components produced vs. inspected
- First-pass yield %
- Rework completion rate
- Scrap rate
- Schedule variance (days ahead/behind critical path)
- Defects per million (DPM) for critical components

---

## SECTION 8: CONTINGENCY & ESCALATION

### Issue Escalation Procedure

**Level 1** (Machinist/Inspector):
- Issue: Out-of-tolerance component found
- Action: Quarantine, document defect
- Timeline: Immediate

**Level 2** (Lead Technician/Manufacturing Engineer):
- Issue: Defect rate > 5% on any component
- Action: Investigate machine drift, retrain operator, adjust process
- Timeline: 4-8 hours

**Level 3** (Engineering Lead):
- Issue: Defect rate > 10%, or critical defect found
- Action: Stop production, design review, corrective action plan
- Timeline: 24 hours

**Level 4** (Project Manager):
- Issue: Schedule impact > 1 week due to quality issues
- Action: Consider design change, equipment replacement, subcontracting
- Timeline: 48 hours

### Quality Gate Failures

**Gate 1** (Week 11): All non-hobbing components complete final inspection
- Success criteria: ≥ 90% of components within tolerance
- If failed: Extend schedule by 1-2 weeks, prioritize rework

**Gate 2** (Week 13): Digit wheel hobbing complete, first stack assembly tested
- Success criteria: First stack rotates smoothly, carry mechanism works
- If failed: Redesign carry mechanism, delay integration by 2-3 weeks

**Gate 3** (Week 20): All subassemblies pass individual validation
- Success criteria: Mill, Store, Barrel, I/O all function independently
- If failed: Rework subassemblies, delay integration by 2-4 weeks

**Gate 4** (Week 23): System functional testing (20-program test suite)
- Success criteria: ≥ 80% of tests pass
- If failed: Debug logic errors, adjust lever positions, reprogram control, delay by 1-2 weeks

---

## END QUALITY CONTROL & VALIDATION DOCUMENT

