# Next 10 Lacunae (Round 2)

Purpose: identify the next major missing primary materials and implementation gaps, with concrete, granular next actions.

Status key: missing | blocked | partial | in-progress

-------------------------------------------------------------------------------

## 1) Pascal 1645 primary text (missing)
Goal: obtain Pascal’s own description/specification of the arithmetic machine.
- Locate an open-access scan/translation of “Account of the Arithmetic Machine (1645)”.
- Cache PDF and/or text transcription under `docs/sources/cache/pascaline/`.
- Add bibliographic metadata (edition, translator, pages) to `docs/sources/SOURCE_CLASSIFICATION.md`.
- Add entry to `docs/history/CITATION_MATRIX_MISSING_SOURCES.md` (Status: local).
- Extract carry mechanism description into `docs/simulation/specs/pascaline_spec.md` (cite exact page refs).

## 2) CNAM/Arts-et-Métiers ccProxy image access (blocked)
Goal: make CNAM assets reliably downloadable (or document why not).
- Determine whether `https://collections.arts-et-metiers.net/ccProxy.ashx` exposes a public CDN alternative.
- If `imageproxy.aspx` host is internal-only, find the public object record URLs for:
  - Pascaline: C-2008-0166/0167/0168, C-2008-0253
  - Leibniz: records in `docs/sources/cache/CNAM_Leibniz_ccProxy_search.json`
- Write `tools/sources/fetch_cnam_ccproxy.py` to:
  - Resolve record pages
  - Download all referenced images/PDFs
  - Emit a manifest JSON (URL → local path → SHA256)
- Update the cache index and citation matrix.

## 3) Leibniz Archive manuscript scans (missing)
Goal: obtain primary documentation for the stepped reckoner and/or stepped drum.
- Identify Leibniz Archive (Hanover) catalog entries for calculator documents.
- Cache any open scans (PDF/images) under `docs/sources/cache/leibniz/archive/`.
- Extract stepped-drum geometry parameters (tooth profile / step heights) into `docs/simulation/specs/leibniz_spec.md`.
- Add a Tier-2 mechanical constraint model for carry in `backend/src/emulator/leibniz_reckoner.py`.

## 4) Antikythera full gear-train parameterization (partial)
Goal: replace “generic gear train” with historically sourced ratios/dial mappings.
- From arXiv PDFs (and any open museum pages), extract:
  - Tooth counts for named gears (b1, b2, c1/c2, d1/d2, e1.., k1/k2, etc.)
  - Dial mapping functions (Metonic, Saros, Callippic, draconic/anomalistic)
- Create `docs/sources/antikythera/gear_train.yaml` with:
  - gears: teeth, shaft group, mesh pairs, differential couplings
  - outputs: dial/pointer definitions and units
- Extend `backend/src/emulator/antikythera.py` with shaft-coupling + multi-stage propagation.
- Add “known cycle” tests (e.g., 223 synodic ↔ 242 draconic months) with tolerances.

## 5) Astrolabe primary tables extraction (blocked by scan/OCR)
Goal: generate structured tables (or verified formulae) backed by a primary source.
- Use `tools/astrolabe/ocr_chaucer_tables.py` to OCR candidate pages with tables.
- Identify a specific table type to normalize first (e.g., declination/altitude examples).
- Create `docs/sources/astrolabe/tables/*.json` (schema + provenance per table).
- Update `backend/src/emulator/astrolabe.py` to optionally query extracted tables for historical reproductions.
- Add tests that cross-check extracted-table results against the computed spherical model.

## 6) Note G “card-loop” semantics (partial)
Goal: model the repetitions `(13..23)` and variable-card column shifting.
- Formalize token semantics (variable versions/columns) in `docs/simulation/EMULATOR_INTERFACE.md` or a Note G appendix.
- Expand `docs/simulation/NOTE_G_TABLE_A2.yaml` to encode repetition structure explicitly.
- Implement a `CardLoop` model in `backend/src/emulator/note_g_deck.py` that:
  - Repeats operation groups as specified by cycle notation
  - Advances variable-card columns deterministically
- Add a test that computes B7 via loop semantics (and matches the Bernoulli oracle).

## 7) Quipu: complete KFG dataset ingest (partial)
Goal: scale from 2 XLSX samples to the full KFG corpus.
- Use `python-internetarchive` to list all KFG XLSX entries.
- Download all XLSX into `docs/sources/cache/quipu/KFGdatafiles/`.
- Run `tools/quipu/parse_kfg.py` across the full set to produce normalized JSON per khipu.
- Add schema evolution notes in `docs/sources/quipu/KFG_SCHEMA.md` (if new columns appear).
- Add property-based tests for parser stability (Hypothesis) on random rows.

## 8) Abacus/rod calculus: primary manuals (missing)
Goal: base algorithms on primary texts rather than modern summaries.
- Identify at least one open scan/translation of a rod calculus or suanpan manual.
- Cache source under `docs/sources/cache/abacus/`.
- Add a “worked examples” suite with citations in `docs/sources/abacus/worked_examples.md`.
- Extend `backend/src/emulator/abacus.py` with additional operations (division, roots) and golden tests.

## 9) Whitepaper: platform-level “Ancient Compute” narrative (partial)
Goal: a coherent end-to-end whitepaper spanning mechanisms + emulators + BOM.
- Add a new top-level overview document (timeline + scope + emulator tiers).
- Include a citation-backed “what counts as computing” taxonomy and methodology.
- Integrate mechanism coverage matrix + BOM schema references.
- Add a “primary sources and lacunae” appendix generated from the citation matrix.

## 10) BOM: sourced mechanical parameters (missing)
Goal: replace placeholder BOM rows with sourced dimensions/materials/tolerances.
- Identify primary/authoritative measurements for:
  - Analytical Engine (where available)
  - Pascaline / Leibniz (once sources are acquired)
  - Antikythera (gear thickness, tooth counts, plate spacing where available)
- Add provenance columns (source ID, page/figure, uncertainty bounds) to BOM CSVs.
- Create a BOM validation script that checks:
  - required fields present
  - units normalized
  - citations exist for non-placeholder rows

