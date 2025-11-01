# Phase 4: Component Test Specifications
## Integration Testing & Operational Validation
**Document Version**: 1.0  
**Date**: October 31, 2025  
**Status**: Complete Specification for Testing All 38,600+ Components

---

## EXECUTIVE SUMMARY

This document specifies comprehensive component-level testing procedures for all manufactured parts in the Babbage Analytical Engine. Testing validates that components meet Phase 3 manufacturing specifications before assembly into subassemblies.

**Key Metrics**:
- **38,600 total components** across 7 families
- **3 test levels**: In-process (manufacturing), Final (pre-assembly), Acceptance (post-assembly)
- **100% critical path components** tested before assembly
- **10% sampling of non-critical** components
- **Target pass rate**: 96% first-pass, 98% after rework
- **Test duration**: 8 weeks (overlaps manufacturing completion)

---

## SECTION 1: TESTING STRATEGY OVERVIEW

### 1.1 Three-Tier Testing Framework

#### Tier 1: In-Process Testing (During Manufacturing)
**When**: Every 5th-25th component during production  
**Who**: Manufacturing technician at workstation  
**Duration**: 30 seconds to 5 minutes per measurement  
**Action**: Immediate go/no-go decision; rework or scrap if OOT  

Components tested in-process:
- **Digit wheels** (every 50th wheel; 100 wheels = 2 wheels/week)
- **Shafts** (every 25th shaft; 160 shafts = 6.4 shafts/week)
- **Sector wheels** (every 25th wheel; 80 wheels = 3.2 wheels/week)
- **Bearing bores** (every 10th bore; 50 bores = 5 bores/week) [CRITICAL]
- **Levers** (every 50th lever; 40 levers = 0.8 levers/week)

#### Tier 2: Final Component Testing (Pre-Assembly)
**When**: After manufacturing completion, before subassembly  
**Where**: Quality Control laboratory with calibrated instruments  
**Duration**: 1-3 hours per component type  
**Action**: 100% critical components, 10% sampling non-critical  

#### Tier 3: Post-Assembly Testing (After Assembly)
**When**: After subassembly integration  
**Where**: Assembly workstation with assembly fixtures  
**Duration**: 5-15 minutes per subassembly  
**Action**: Functional testing; disassemble if failure  

### 1.2 Test Categories by Component Type

#### Critical Components (100% tested before assembly)
- Digit wheels (all 5,000)
- Bearing bores (all 50)
- Center shafts (all 20)
- Carry levers (all 40)
- Pinions (all 200)

Justification: Dimensional errors in critical components cause cascading failures or misalignment affecting entire subassembly.

#### Major Components (10% sampling before assembly)
- Sector wheels (sampling: 80 wheels = 8 tested)
- Regular shafts (sampling: 160 shafts = 16 tested)
- Fastener lots (sampling: 1 lot per 10,000 fasteners)
- Support frames (sampling: 1 of each type)

#### Minor Components (Post-assembly validation only)
- Fasteners (verified in situ)
- Lubricants and sealants (chemical verification or batch certificates)
- Misc. small parts (visual inspection)

---

## SECTION 2: DIGIT WHEEL TESTING (5,000 UNITS)

### 2.1 Critical Dimensions

| Dimension | Specification | Tolerance | Test Method | Pass/Fail |
|---|---|---|---|---|
| Outer diameter | 80 mm | ±0.05 mm | Caliper, 4 points | OD within range |
| Bore diameter | 25 mm | ±0.03 mm | Caliper + GO/NO-GO plug | Bore within range |
| Gear tooth pitch | 10 mm | ±0.1 mm | Pitch gauge or calipers | Teeth evenly spaced |
| Teeth engageable | 10-11 teeth | Functional | Mesh with pinion under light load | Smooth rotation |
| Axial runout | N/A | < 0.2 mm | Mounted on spindle, dial indicator | Runout acceptable |
| Radial runout | N/A | < 0.1 mm | Mounted on spindle, dial indicator | Runout acceptable |
| Deburring | Smooth edges | Visual + touch | Visual inspection, finger test | No sharp edges |

### 2.2 Test Sequence for Digit Wheels

**Step 1: Visual Inspection** (30 seconds)
- Surface cleanliness
- No visible cracks, chips, or deformation
- Gear teeth intact and unbent
- Bore entrance chamfered (no sharp edges)

**Step 2: Dimensional Verification** (2 minutes)
- OD measurement with caliper at 4 points (0°, 90°, 180°, 270°)
- Bore diameter with GO/NO-GO plug
- Tooth pitch with pitch gauge (measure 3 consecutive tooth spaces, average)

**Step 3: Runout Check** (2 minutes)
- Mount wheel on mandrel (bore-centered)
- Measure radial runout at OD with dial indicator
- Measure axial runout (side-to-side wobble) with dial indicator
- Record if any runout exceeds limits

**Step 4: Functional Engagement Test** (1 minute)
- Load wheel onto shaft with pinion
- Hand-rotate pinion; observe engagement
- Should rotate smoothly without binding or excessive play
- Record subjective assessment: smooth/acceptable/rough/no-go

**Step 5: Record and Disposition**
- Pass: Place in "Ready for Assembly" bin
- Marginal: Place in "Inspect Further" bin (random 5% of passes)
- Fail: Place in "Rework" bin (regrind bore or OD as needed, re-test)

### 2.3 Sampling Plan for In-Process Testing

**Testing frequency**: Every 50th wheel during manufacturing

**Schedule**:
- Week 1-4: 50 wheels/week → test 1/week = 1 wheel/week
- Week 5-10: 80 wheels/week → test 2/week
- Week 11-26: 180 wheels/week → test 4/week
- Week 27-34: 200 wheels/week → test 4/week

**Total in-process tests**: 100 wheels (2% of production)

**Action limits**:
- If 2+ consecutive samples fail same dimension → STOP production, investigate process
- If 1 sample fails any dimension → Examine next 10 units, determine if isolated defect
- If > 5% of sample fails → Restart SPC control chart, re-baseline process

---

## SECTION 3: SHAFT TESTING (160 UNITS)

### 3.1 Shaft Types and Criticality

| Shaft Type | Quantity | Criticality | Test Method |
|---|---|---|---|
| Center shaft (Mill) | 8 | **CRITICAL** | 100% before assembly |
| Input shaft (Barrel) | 4 | **CRITICAL** | 100% before assembly |
| Carry shaft (Mill) | 12 | MAJOR | 10% sampling (1-2 shafts) |
| Regular shaft (Support) | 136 | MAJOR | 10% sampling (13-14 shafts) |

### 3.2 Center Shaft Test Procedure (CRITICAL - 100% TESTED)

**Dimensions to verify**:

| Dimension | Spec | Tolerance | Test |
|---|---|---|---|
| Overall length | 280 mm | ±0.5 mm | Calipers, measure center section |
| OD main section | 15 mm | ±0.05 mm | Micrometer, 3 points along length |
| OD shoulder | 12 mm | ±0.05 mm | Micrometer, 3 points |
| Journal diameter | 8 mm | ±0.03 mm | Caliper or micrometer |
| Axial runout | N/A | < 0.15 mm | Mounted in spindle, dial indicator |
| Radial runout | N/A | < 0.1 mm | Mounted in spindle, dial indicator |

**Test sequence**:
1. Visual inspection (cracks, surface finish)
2. Length measurement (calipers at 3 points: left, center, right)
3. Diameter measurement (micrometer at 3 points along length for main section, shoulder, and journals)
4. Runout check (mounted in spindle, < 0.1 mm radial, < 0.15 mm axial)
5. Record and disposition

**Pass criteria**: All dimensions within tolerance, no runout issues

### 3.3 Regular Shaft Sampling (10% = 13-14 shafts)

**Sampling method**: Every 10th shaft off production line

**Test sequence** (abbreviated):
1. Length (calipers)
2. OD main section (micrometer)
3. Journal diameter (caliper)
4. Visual inspection

**Pass criteria**: All dimensions within tolerance

**Action if sampling shows defect**: Re-test previous 5 shafts and next 5 shafts (confidence check)

---

## SECTION 4: SECTOR WHEEL TESTING (80 UNITS)

### 4.1 Sector Wheel Specifications

**Type**: Partial gear wheels with 40-45 teeth spanning ~150° arc

| Dimension | Specification | Tolerance | Test Method |
|---|---|---|---|
| Arc length | 40-45 teeth | ±1 tooth | Tooth count |
| Outer diameter | 120 mm | ±0.1 mm | Calipers, 3 points |
| Inner radius | 50 mm | ±0.1 mm | Calipers to back surface |
| Bore diameter | 25 mm | ±0.03 mm | GO/NO-GO plug |
| Tooth engagement | Matches pinion | 0.5-1 mm gap | Mesh test with pinion |
| Radial runout | N/A | < 0.15 mm | Spindle mount, dial indicator |

### 4.2 Sampling Plan (10% = 8 wheels tested)

**Test frequency**: Every 10th sector wheel off production

**Test sequence**:
1. Visual inspection (surface, teeth, bore)
2. Tooth count (count manually, verify 40-45)
3. Bore diameter (GO/NO-GO plug)
4. Radial runout (mounted on spindle)
5. Functional mesh test (load onto output shaft with pinion, hand-rotate)

**Pass criteria**: Correct tooth count, bore within tolerance, runout acceptable, smooth engagement

---

## SECTION 5: BEARING BORE TESTING (50 UNITS - CRITICAL)

### 5.1 Bearing Bore Specifications

Bearing bores are critical because ball bearings must have precise fit to function smoothly.

| Specification | Value | Tolerance | Criticality |
|---|---|---|---|
| Bore diameter | 25 mm | ±0.02 mm | **CRITICAL** (tight tolerance) |
| Bore length | 30 mm | ±0.2 mm | MAJOR |
| Bore runout | N/A | < 0.1 mm | **CRITICAL** |
| Surface finish | Ra 0.8 μm | ±0.2 μm | MAJOR |
| Deburring | Chamfered edges | Visual | MAJOR |

### 5.2 Testing Procedure (100% BEFORE ASSEMBLY)

**All 50 bearing bores tested individually**

**Step 1: Visual Inspection** (1 minute)
- Check for cracks, chips
- Verify chamfered entrance
- Clean bore (blow out chips)

**Step 2: Bore Diameter Measurement** (2 minutes)
- Use bore gauge (precision ±0.01 mm) or limit plugs
- Measure at 3 depths: entrance, middle, exit
- Record all measurements
- Pass if all within 25.00 ± 0.02 mm (24.98 - 25.02 mm)

**Step 3: Runout Check** (3 minutes)
- Mount bore on spindle (center bore on spindle axis)
- Use dial indicator to measure radial runout
- Record maximum runout
- Pass if < 0.1 mm

**Step 4: Trial Bearing Installation** (5 minutes)
- Insert ball bearing (SKF type) into bore
- Bearing should slide in smoothly with light thumb pressure
- Rotate bearing by hand; should rotate freely without binding
- Remove bearing carefully (preserve for next test item)

**Pass criteria**: 
- Bore diameter 24.98 - 25.02 mm at all 3 depths
- Runout < 0.1 mm
- Bearing slides in and rotates smoothly

**Disposition**:
- Pass: Mark bore "READY" (do not damage entrance)
- Fail: Mark bore "REWORK" (requires honing to correct diameter)

### 5.3 Bearing Bore Quality Control

**Critical process control**:
- Boring operation uses carbide inserts (maintain sharp edge)
- Honing operation follows with ceramic stones (finish to correct diameter)
- Every 5th bore is measured in-process (see Section 1.2)
- If any bore fails final test → stop boring operation, check tool wear, re-bore previous batch

---

## SECTION 6: LEVER AND LINKAGE TESTING (40 CARRY LEVERS + 80 SUPPORT LEVERS)

### 6.1 Carry Lever Specifications (CRITICAL - 40 UNITS)

**Critical dimensions**:

| Dimension | Specification | Tolerance | Test Method |
|---|---|---|---|
| Overall length | 150 mm | ±1 mm | Calipers |
| Pivot hole diameter | 8 mm | ±0.05 mm | GO/NO-GO plug |
| Pivot hole depth | 12 mm | ±0.5 mm | Depth gauge |
| Arm straightness | N/A | < 0.5 mm | Straightedge + feeler gauge |
| Surface flatness | N/A | < 0.2 mm | Straightedge, check at 3 points |
| Deburring | Smooth edges | Visual + touch | Visual + finger test |

### 6.2 Carry Lever Test Procedure (100% TESTED BEFORE ASSEMBLY)

**All 40 carry levers tested**

**Step 1: Visual Inspection** (30 seconds)
- Check for bends, twists, cracks
- Verify smooth edges (deburred)
- Clean lever (wipe with cloth)

**Step 2: Dimensional Verification** (2 minutes)
- Length measurement (calipers at 3 points: left, center, right arm)
- Pivot hole diameter (GO/NO-GO plug; should slide in with light finger pressure)
- Arm straightness (place on flat surface, use straightedge and feeler gauge)

**Step 3: Functional Test** (1 minute)
- Insert pivot pin (8 mm diameter rod, 50 mm length) into pivot hole
- Lever should rotate freely on pin
- No binding or excessive play (< 0.1 mm gap)
- Record if smooth or rough

**Pass criteria**: All dimensions within tolerance, rotates freely on pin

---

## SECTION 7: FASTENER TESTING

### 7.1 Fastener Lot Acceptance

**Fastener types**:
- M5 screws (1,200 units)
- M4 screws (800 units)
- M6 bolts (400 units)
- Washers (2,000 units)
- Lock nuts (1,200 units)
- Cotter pins (400 units)

### 7.2 Lot Inspection Procedure

**For each lot of 1,000 fasteners**:

1. **Visual inspection**: Sample 20 fasteners
   - Check threads (no damage, proper pitch)
   - Check plating (no rust, uniform coating)
   - Check head/nut (no cracks, proper hex shape)

2. **Dimensional sampling**: Random 10 from lot
   - Measure diameter (micrometer or caliper)
   - Measure length (calipers)
   - Measure pitch (count threads in 10 mm, divide by 10)

3. **Thread function test**: Random 5 from lot
   - Screw into threaded hole in test plate
   - Should go in smoothly without stripping
   - Should hold tight without excessive force

4. **Acceptance criteria**:
   - No visible defects in visual inspection
   - All dimensions within ±0.1 mm
   - All threads function smoothly
   - **Result**: Lot ACCEPTED (stamp label) or REJECTED (return to supplier)

### 7.3 Fastener Lot Tracking

**Lot label format**:
```
FASTENER LOT CERTIFICATE
Date: [date]
Type: M5 screw, stainless steel
Quantity: 1,000
Supplier: [supplier name]
Lot #: [supplier lot number]
Test results: PASS / FAIL
Tested by: [initials]
```

---

## SECTION 8: COMPONENT TEST RESULTS TRACKING

### 8.1 Test Results Template

**File location**: `/phase4/results/component_test_results.csv`

**Columns**:
- Test date
- Component type (digit wheel, shaft, etc.)
- Component ID / serial number
- Dimensions tested (OD, bore, runout, etc.)
- Measured values
- Tolerance range
- Pass/fail status
- Rework disposition (if failed)
- Technician name
- Notes

### 8.2 Weekly Reporting

**Each Friday at 4:00 PM**, manufacturing lead submits:

```
WEEKLY COMPONENT TEST REPORT
Week of [date]

Component Test Summary:
- Digit wheels tested: [N], Pass rate: [%], Failures: [N]
- Shafts tested: [N], Pass rate: [%], Failures: [N]
- Sector wheels tested: [N], Pass rate: [%], Failures: [N]
- Bearing bores tested: [N], Pass rate: [%], Failures: [N]
- Carry levers tested: [N], Pass rate: [%], Failures: [N]

Overall pass rate: [%]
Rework rate: [%]
Scrap rate: [%]

Issues encountered:
[Describe any process issues, OOT dimensions, etc.]

Corrective actions taken:
[List any actions to address issues]

Next week's testing schedule:
[List planned testing for next week]
```

### 8.3 Quality Control Dashboard

**Real-time tracking** (updated as tests completed):

| Component Type | Total Mfg | Tested | Passed | Failed | Rework | Pass Rate |
|---|---|---|---|---|---|---|
| Digit wheels | 5,000 | [N] | [N] | [N] | [N] | [%] |
| Shafts | 160 | [N] | [N] | [N] | [N] | [%] |
| Sector wheels | 80 | [N] | [N] | [N] | [N] | [%] |
| Bearing bores | 50 | [N] | [N] | [N] | [N] | [%] |
| Carry levers | 40 | [N] | [N] | [N] | [N] | [%] |
| **OVERALL** | **5,330** | **[N]** | **[N]** | **[N]** | **[N]** | **[%]** |

---

## SECTION 9: REWORK AND SCRAP PROCEDURES

### 9.1 Rework Decision Tree

```
Component fails test
    ↓
Is failure critical (bore diameter, runout)?
    ├─ YES → Rework or scrap?
    │   ├─ Can rework in-house? (e.g., re-bore, re-hone)
    │   │   ├─ YES → Route to rework station
    │   │   └─ NO → SCRAP (cannot fix)
    │   └─ Record rework order
    │
    └─ NO (minor defect) → Assess impact
        ├─ Cosmetic only? → ACCEPT
        ├─ Minor dimension OOT? → REWORK or ACCEPT as-is?
        └─ Document and approve with QC manager
```

### 9.2 Rework Station Procedures

**For reworkable components** (e.g., digit wheel bore too large):

1. **Diagnosis**: Document why component failed
2. **Rework plan**: Determine corrective machining
3. **Rework execution**: Re-machine component per procedure
4. **Re-test**: Test reworked component with same procedure
5. **Pass/fail**:
   - Pass: Route to assembly bin
   - Fail again: SCRAP (component has been reworked once; second failure means inherent problem)

### 9.3 Scrap Procedures

**Components that cannot be reworked**:
- Mark with red "SCRAP" label
- Photograph if unusual failure
- Document failure reason
- Weigh and record (for cost tracking)
- Place in scrap bin for salvage/recycling

---

## SECTION 10: TEST EQUIPMENT CALIBRATION

### 10.1 Test Equipment Inventory

| Equipment | Specification | Calibration Interval | Next Calibration |
|---|---|---|---|
| Calipers (4x) | ±0.05 mm accuracy | Every 6 months | [Date] |
| Micrometers (3x) | ±0.01 mm accuracy | Every 6 months | [Date] |
| Dial indicators (5x) | ±0.01 mm accuracy | Every 3 months | [Date] |
| Pitch gauge | ±0.1 mm accuracy | Every 12 months | [Date] |
| GO/NO-GO plugs | ±0.02 mm accuracy | Every 12 months | [Date] |
| Straightedge (2x) | Flatness < 0.05 mm | Every 12 months | [Date] |
| Feeler gauge | ±0.05 mm accuracy | Every 12 months | [Date] |
| Spindle runout check | Reference < 0.02 mm | Every month | [Date] |

### 10.2 Calibration Procedure

**Before first use each day**:
1. Clean equipment with lint-free cloth
2. Check for visible damage (bent indicator needle, worn calipers)
3. Use reference standard (go gauge, calibration block) to verify accuracy
4. Record check in equipment log

**Every 3-6 months**:
- Send to external calibration lab (cost: £15-30 per item)
- Obtain calibration certificate
- Update equipment label with next calibration date

**Storage**:
- Keep in climate-controlled area (20-25°C, < 50% humidity)
- Store in padded cases when not in use
- Keep calipers and micrometers in protective boxes

---

## SECTION 11: COMPONENT TEST TIMELINE

**Phase 4 component testing runs in parallel with Phase 3 manufacturing completion**

| Week | Activity | Components tested | Status |
|---|---|---|---|
| Week 27-30 | Digit wheel manufacturing completion + testing | First 2,500 wheels, all 5,000 | In progress |
| Week 31-32 | Shaft manufacturing + testing | All 160 shafts | In progress |
| Week 32-33 | Sector wheel manufacturing + testing | All 80 sector wheels | In progress |
| Week 33-34 | Bearing bore manufacturing + final testing | All 50 bearing bores | In progress |
| Week 34-35 | Carry lever manufacturing + final testing | All 40 carry levers | In progress |
| Week 35-36 | Final fastener lot acceptance | All lots | In progress |
| Week 36-37 | Rework completion | Reworked components | In progress |
| Week 37-38 | Final component inspection | Random 10% verification | Complete |
| Week 38 | Component test report and sign-off | All results compiled | Final report |

---

## SECTION 12: COMPONENT TEST ACCEPTANCE CRITERIA

### 12.1 Overall Pass/Fail Criteria

**Component testing PASSES if**:
- ✅ 96%+ of components pass first-pass testing
- ✅ Rework brings 98%+ of remaining components to pass
- ✅ Overall scrap rate < 2%
- ✅ All critical components (digit wheels, bearing bores, center shafts, carry levers) have 100% pass rate or rework success
- ✅ No unexplained process issues (e.g., systematic bore diameter drift)
- ✅ Test equipment calibration current and within limits

### 12.2 Go/No-Go Decision Matrix

| Metric | Target | Actual | Status |
|---|---|---|---|
| Overall pass rate | > 96% | [%] | ✅ / ❌ |
| Critical components pass | 100% | [%] | ✅ / ❌ |
| Rework success rate | > 90% | [%] | ✅ / ❌ |
| Scrap rate | < 2% | [%] | ✅ / ❌ |
| **Component testing APPROVED?** | — | — | **✅ / ❌** |

**If APPROVED**: Proceed to integration testing (Section 2 of next document)  
**If NOT APPROVED**: Investigate failures, implement corrective actions, re-test failed components

---

## SECTION 13: TESTING ROLES AND RESPONSIBILITIES

### 13.1 Test Team Assignments

**Manufacturing Test Technician** (1 person, 34 weeks):
- Conduct in-process sampling (digit wheels, shafts, sector wheels every shift)
- Record results in test log
- Alert manufacturing lead if OOT dimensions detected
- Maintain test equipment (clean, store properly)

**QC Laboratory Technician** (1 person, 8 weeks):
- Conduct final component testing pre-assembly
- Measure critical dimensions with precision instruments
- Record all measurements in test database
- Generate weekly reports

**QC Manager** (1 person, 3 weeks):
- Review test results
- Approve rework decisions
- Sign off component testing completion
- Generate final component test report

---

## CONCLUSION

Component testing validates that all 5,330 manufactured parts meet Phase 3 specifications before assembly. With 96%+ first-pass and 98%+ final pass rates, the engine will be built from verified, high-quality components.

Next: Section 2 covers integration testing of subassemblies.

