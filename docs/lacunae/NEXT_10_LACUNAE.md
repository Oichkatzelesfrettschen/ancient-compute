# Next 10 Major Lacunae (Granular Build Plan)

This is the next execution tranche after the Tier-1 emulator pass.

## 1) Quipu: KFG XLSX -> normalized schema
- Goal: turn Khipu Field Guide XLSX records into machine-readable artifacts.
- Inputs: `docs/sources/cache/quipu/KFGdatafiles/QU001.xlsx`, `UR001.xlsx`
- Outputs: `docs/sources/quipu/kfg_normalized/*.json` + schema doc.
- Build steps:
  1. Inspect XLSX sheets/columns.
  2. Define `KFGRecord` schema (id, provenance, cord tree, numeric decode fields).
  3. Implement parser + normalization.
  4. Add golden fixtures from QU001/UR001.

## 2) Quipu: richer Tier-1 emulator
- Goal: replace placeholder ledger with cord/knot positional encoding model.
- Outputs: `backend/src/emulator/quipu_kfg.py` + tests.

## 3) Antikythera: acquire open primary data
- Goal: obtain gear ratios/dial mappings from open sources and cache them.
- Outputs: cached PDF(s)/dataset + citations updated.

## 4) Antikythera: Tier-2 gear-train simulator
- Goal: propagate rotations through gear trains with dial readouts.
- Outputs: `backend/src/emulator/antikythera.py` + tests.

## 5) Astrolabe: reference tables + emulator
- Goal: table-driven outputs with known latitudes and dates.
- Outputs: cached tables + `backend/src/emulator/astrolabe.py`.

## 6) Abacus/rods: worked examples
- Goal: extract 5–10 canonical problems with inputs/outputs for tests.
- Outputs: `docs/sources/abacus/worked_examples.md` + fixtures.

## 7) Pascaline: primary description/scans
- Goal: locate Pascal 1645 description and/or high-quality museum scans.
- Outputs: cached source + citations updated.

## 8) Leibniz: primary scans + 1703 binary paper
- Goal: obtain open scans (or stable museum pages) and unblock Gallica if possible.
- Outputs: cached source + citations updated.

## 9) Ada extensions beyond Note G
- Goal: extract additional explicit computation patterns into deck format.
- Outputs: additional `docs/simulation/*.yaml` decks + tests.

## 10) BOM enrichment
- Goal: replace placeholders in `docs/simulation/bom_*.csv` with sourced dimensions/tolerances.
- Outputs: BOM CSV updates + citations.
