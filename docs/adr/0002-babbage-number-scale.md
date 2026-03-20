# ADR 0002: BabbageNumber Fixed-Point Scale (10^40)

## Status

Accepted (2026-03-19)

## Context

The Babbage Analytical Engine specification calls for 50-digit decimal
precision. The emulator needs an exact integer arithmetic type that can
represent this range without floating-point rounding.

Alternatives considered:
- **Python Decimal**: Correct semantics but 3-5x slower in Monte Carlo
  simulations (10,000+ operations per step) due to string conversion overhead.
- **Arbitrary-precision int (no fixed scale)**: Requires explicit scaling
  at every operation boundary, error-prone, and adds per-operation overhead
  to determine magnitude.
- **Fixed-scale int with _SCALE = 10^40**: Single multiplication/division
  per arithmetic op. 50-digit values fit in Python's arbitrary-precision
  int natively. Class constants (_SCALE, _MAX_INTERNAL, _MIN_INTERNAL)
  avoid recomputation.

## Decision

BabbageNumber stores its value as `int(value * 10^40)`. The scale factor
is a class constant `_SCALE = 10**40`, with derived constants `_MAX_INTERNAL`
and `_MIN_INTERNAL` computed once at class definition time.

## Consequences

- All arithmetic is exact integer math; no floating-point rounding.
- Monte Carlo tolerance checks run at acceptable speed (~0.3s for 7 checks).
- The `to_float()` method introduces rounding only at display/export time.
- Any change to _SCALE requires updating all golden test expectations.
