# Docs Taxonomy

**Date**: 2026-02-24  
**Status**: Active  
**Purpose**: Repository-wide documentation taxonomy with ownership intent and active-vs-archive behavior.

## Core Principles

1. Keep planning canonical sources centralized under `docs/general/`.
2. Keep domain plans near domain content when they are operational and non-duplicative.
3. Archive completed/superseded planning artifacts under `docs/archive/` with explicit metadata and successor links.
4. Preserve historical evidence; do not delete context unless content-identical duplicate.

## Top-Level Documentation Domains

| Directory | Purpose | Default Status |
|---|---|---|
| `docs/general/` | Cross-project architecture, roadmap, trackers, and operator guides | active |
| `docs/archive/` | Superseded and historical records with provenance | archived |
| `docs/babbage_engine/` | Babbage-specific design, manufacturing, and emulator references | active |
| `docs/history/` | Historical methodology, sources, regional coverage, and validation | active |
| `docs/simulation/` | Simulation models, specs, extraction, and coverage artifacts | active |
| `docs/sources/` | Source index, provenance, collection workflow, and cached source artifacts | active |
| `docs/minix/` | MINIX metrics and operational runbooks | active |
| `docs/restoration/` | Physical restoration and manufacturing operations | active |
| `docs/specifications/` | Formal architecture/spec references for subsystems | active |
| `docs/requirements/` | Environment and dependency requirements | active |
| `docs/speculative/` | Clearly labeled speculative material | active (non-canonical) |
| `docs/whitepaper/` | Whitepaper source and production assets | active |
| `docs/whitepaper-arxiv/` | arXiv-focused whitepaper packaging | active |
| `docs/templates/` | Reusable documentation templates | active |
| `docs/lacunae/` | Research gap tracking | active |

## Planning Taxonomy

Planning docs are classified into three lanes:

1. `canonical-general`: project-wide strategy/execution docs.
2. `active-domain`: domain-local roadmaps and execution plans.
3. `archive`: superseded snapshots and migration artifacts.

Canonical planning map: `docs/general/PLANNING_CANONICAL_MAP.md`  
Archive planning ledger: `docs/archive/INDEX.md`

## Archive Admission Checklist

Before archiving a planning document:

1. Verify a canonical or domain-active successor exists.
2. Extract and integrate any novel constraints/checklists.
3. Add `Archive Metadata` to the document.
4. Register the document in `docs/archive/INDEX.md`.
5. Update obvious stale links to the successor.
