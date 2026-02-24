# Archive Audit Ledger

Date: 2026-02-24
Status: Active
Scope: Full inventory of legacy files under `docs/archive` (excludes governance files generated on 2026-02-24).

## Summary

- Total legacy archive files audited: 97
- Class `functional_planning`: 23
- Class `functional_design_code`: 15
- Class `historical_only`: 59
- Files with archive metadata section: 24
- Files referenced by active docs: 4
- Files with one or more broken internal links: 6
- Total broken internal links (markdown files only): 41
- Quarantine candidates (`broken_link_count >= 5`): 2

## Classification Policy

- `functional_planning`: old plans/roadmaps/audits still useful as provenance inputs for active planning.
- `functional_design_code`: archived architecture/design/implementation content still useful as technical context.
- `historical_only`: milestone/session/completion snapshots retained primarily for project history.
- Classification is generated from deterministic filename heuristics plus link-health checks; promote/demote classes manually if deeper content review disagrees.

## Why Plans Were Superseded

1. Canonical consolidation moved active planning to `docs/general/MASTER_ROADMAP.md` and `docs/general/TODO_TRACKER.md`.
2. Phase and week plans became immutable snapshots once execution completed.
3. Migration and link-repair plans were temporary by design and retained as provenance only.
4. Architecture/tooling evolved, so historical plans no longer reflect current build reality.

## Quarantine Candidates

| File | Broken Links | Canonical Successor(s) |
|---|---:|---|
| `docs/archive/WEEK_3_TASK_3_LINK_MAPPING_AND_UPDATE_PLAN.md` | 27 | `docs/general/PLANNING_CANONICAL_MAP.md;docs/archive/INDEX.md` |
| `docs/archive/STRATEGIC_ROADMAP.md` | 8 | `docs/general/MASTER_ROADMAP.md;docs/general/TODO_TRACKER.md` |

## Full Ledger

Source of truth: `docs/archive/audit_ledger.csv`

| File | Class | Reason | Metadata | Active Refs | Broken Links | Quarantine |
|---|---|---|---|---:|---:|---|
| `docs/archive/00_REVIEW_INDEX.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/ARCHITECTURAL_REVIEW_WEEK2_DAY6.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md` | `functional_design_code` | `superseded_by_current_specification` | no | 0 | 0 | no |
| `docs/archive/COMPREHENSIVE_SYNTHESIS_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/DOCUMENTATION_AND_REORGANIZATION_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/IMPLEMENTATION_ROADMAP.md` | `functional_planning` | `superseded_by_newer_canonical` | yes | 0 | 2 | no |
| `docs/archive/INDEX.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 5 | 0 | no |
| `docs/archive/LINKS_FOUND.txt` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/MIGRATION_MANIFEST.txt` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/MVP_FILE_STRUCTURE.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/NOVEMBER_1_EXECUTIVE_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/NOVEMBER_2_SESSION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/OPTION_B_IMPLEMENTATION_ROADMAP.md` | `functional_planning` | `superseded_by_newer_canonical` | yes | 0 | 0 | no |
| `docs/archive/OPTION_C_PHASE_3_VISION.md` | `historical_only` | `historical_context_retained` | no | 0 | 1 | no |
| `docs/archive/PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE2_COMPLETION_INDEX.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/PHASE2_DELIVERY_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_2_AND_3_SCOPE.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_2_IMPLEMENTATION_PLAN.md` | `functional_planning` | `phase_completed_snapshot` | yes | 0 | 0 | no |
| `docs/archive/PHASE_2_PROGRESS_STATUS.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_2_RESCOPED_BABBAGE_ISA_TARGET.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_2_SESSION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_ASSEMBLY_PROCEDURES_WITH_DIAGRAMS.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_COMPLETION_INDEX.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_COST_TRACKING_RESOURCE_ALLOCATION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_MANUFACTURING_PROCEDURES.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_OPERATIONAL_MANUAL.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/PHASE3_QUALITY_CONTROL_VALIDATION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3/README.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_ARCHITECTURE_SPECIFICATION.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_COMPENDIUM.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_IMPLEMENTATION_ROADMAP.md` | `functional_planning` | `phase_completed_snapshot` | yes | 0 | 0 | no |
| `docs/archive/PHASE_3_IMPLEMENTATION_SUMMARY.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_OVERVIEW.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_SESSION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_STATUS_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W1_3_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W1_4_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W1_5_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W2_1_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W3_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W4_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W5_W6_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_3_W7_8_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4/PHASE4_COMPLETION_INDEX.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/PHASE_4/PHASE4_COMPONENT_TEST_SPECIFICATIONS.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_4/PHASE4_HANDOFF_ACCEPTANCE_CRITERIA.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4/PHASE4_SUBASSEMBLY_INTEGRATION_TESTS.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4/PHASE4_SYSTEM_OPERATIONAL_VALIDATION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4/README.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_PLANNING.md` | `functional_planning` | `phase_completed_snapshot` | yes | 0 | 0 | no |
| `docs/archive/PHASE_4_W1_COMPLETION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W1_TEST_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_COMPLETION_AND_RESANITY_CHECK.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_D1_2_FOUNDATION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_D3_4_IMPLEMENTATION_SUMMARY.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_D3_4_STATE_MANAGEMENT_ARCHITECTURE.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_D5_6_ANIMATION_RENDERING_ARCHITECTURE.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_DESIGN.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W2_IMPLEMENTATION_ROADMAP.md` | `functional_planning` | `phase_completed_snapshot` | yes | 1 | 0 | no |
| `docs/archive/PHASE_4_W3_D10_FINAL_REPORT.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W3_D10_VALIDATION.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W3_D9_10_HANDOFF.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PHASE_4_W4_DETAILED_EXECUTION_PLAN.md` | `functional_planning` | `phase_completed_snapshot` | yes | 1 | 0 | no |
| `docs/archive/PHASE_4_W4_PLUS_SYNTHESIS_ROADMAP.md` | `functional_planning` | `phase_completed_snapshot` | yes | 1 | 0 | no |
| `docs/archive/PROJECT_COMPLETION_REPORT.txt` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PROJECT_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/PROJECT_STATUS.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/README.md` | `historical_only` | `historical_context_retained` | yes | 0 | 0 | no |
| `docs/archive/README_OUTPUT.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/REPO_AUDIT_SUMMARY.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/SESSION_2_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/SIMULATOR_RESEARCH_NOTES.md` | `functional_design_code` | `exploratory_notes_formalized` | no | 0 | 0 | no |
| `docs/archive/SOURCE_CODE_AND_BUILD_AUDIT.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/STRATEGIC_ROADMAP.md` | `functional_planning` | `superseded_by_newer_canonical` | yes | 0 | 8 | yes |
| `docs/archive/TECHNICAL_DEBT.md` | `functional_design_code` | `superseded_by_current_specification` | no | 0 | 0 | no |
| `docs/archive/TODO_REPORT.md` | `functional_planning` | `superseded_by_newer_canonical` | yes | 0 | 0 | no |
| `docs/archive/WEEKS_3_4_DOCUMENTATION_COMPLETION_PLAN.md` | `functional_planning` | `superseded_by_newer_canonical` | yes | 0 | 0 | no |
| `docs/archive/WEEK_1_CHECKLIST.md` | `functional_planning` | `phase_completed_snapshot` | yes | 0 | 0 | no |
| `docs/archive/WEEK_2_IMPLEMENTATION_PLAN.md` | `functional_planning` | `phase_completed_snapshot` | yes | 0 | 0 | no |
| `docs/archive/WEEK_3_COMPLETION_AND_STATUS_REPORT.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_3_OCTOBER_31_SESSION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_3_TASK_1_FILE_MIGRATION_AUDIT.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 0 | no |
| `docs/archive/WEEK_3_TASK_2_DIRECTORY_README_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 2 | no |
| `docs/archive/WEEK_3_TASK_3_FINAL_REPORT.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_3_TASK_3_LINK_MAPPING_AND_UPDATE_PLAN.md` | `functional_planning` | `migration_or_audit_artifact` | yes | 0 | 27 | yes |
| `docs/archive/WEEK_3_TASK_3_LINK_UPDATE_SESSION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 1 | no |
| `docs/archive/WEEK_5_C_SERVICE_IMPLEMENTATION.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/WEEK_6_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_6_LANGUAGE_SERVICES_PROGRESS.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/WEEK_7_ASSEMBLER_IMPLEMENTATION.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/WEEK_7_BABBAGE_FOUNDATION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_7_CODE_GENERATOR_IMPLEMENTATION.md` | `functional_design_code` | `implementation_snapshot_after_completion` | no | 0 | 0 | no |
| `docs/archive/WEEK_7_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_8_PHASE_1_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_8_PHASE_2_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
| `docs/archive/WEEK_8_PHASE_3_COMPLETION_SUMMARY.md` | `historical_only` | `historical_context_retained` | no | 0 | 0 | no |
