"""Hardware Twin: software model for the no-electric Babbage hybrid engine.

This package provides:
- de_tabulator: Difference Engine finite-difference tabulator
- printer_formatter: Type-wheel printer output formatter
- models/: Subsystem specification dataclasses
- golden_traces/: Reference outputs for hardware acceptance testing

All golden traces produced here serve as acceptance criteria for
the physical build (see docs/hardware/TEST_PLAN.md).
"""
