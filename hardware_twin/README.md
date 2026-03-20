# hardware_twin/

Digital twin of the Science Museum Group (SMG) Babbage Difference Engine No. 2 (DE2).
Models the mechanical subsystems to validate emulator timing, force calculations, and
output formatting against golden traces derived from the physical machine.

## Structure

```
hardware_twin/
  __init__.py              -- package exports
  de_tabulator.py          -- tabulation controller: drives DE2 calculation columns
  printer_formatter.py     -- output formatter matching DE2 printing mechanism layout
  models/
    __init__.py
    subsystems.py          -- mechanical subsystem data classes (gears, cams, shafts)
  bom/
    bom_v0.csv             -- Bill of Materials: 25 line items, 818 total parts
  golden_traces/
    poly_table_100.csv     -- reference output: 100-row polynomial table from DE2
  tests/                   -- pytest tests for tabulator, formatter, and BOM
```

## Key Components

**de_tabulator.py**: Drives the DE2 calculation columns through a polynomial tabulation.
Implements the carry propagation and column addition sequence that matches the physical
machine's 7-axis cam-driven cycle.

**printer_formatter.py**: Formats tabulated values into the column layout produced by
DE2's printing mechanism. Validates against golden_traces/poly_table_100.csv.

**models/subsystems.py**: Data classes for gears (module, teeth, material), shafts
(diameter, length, material), cams (lift, dwell), and bearings. Used by structural.py
and kinematics.py for physics validation.

**bom/bom_v0.csv**: Machine-readable BOM. Each row has: part_id, description,
quantity, material, supplier_ref. Validated by `make bom-validate`
(tools/validate_bom.py).

## Running Tests

```bash
# From project root
make twin-verify

# Directly
cd backend && pytest ../hardware_twin/tests/ -v
```

## Golden Trace Validation

The golden trace (poly_table_100.csv) is a reference output generated from the
Science Museum DE2 operating parameters. Any change to de_tabulator.py that alters
output must be accompanied by an updated golden trace and documented justification.

## References

- Science Museum Group DE2 technical drawings (restricted access)
- Swade, D. (2001). The Cogwheel Brain. Little, Brown.
- docs/hardware/TEST_PLAN.md -- acceptance criteria for hardware twin
- docs/hardware/CAM_TIMING.md -- cam profile specifications
