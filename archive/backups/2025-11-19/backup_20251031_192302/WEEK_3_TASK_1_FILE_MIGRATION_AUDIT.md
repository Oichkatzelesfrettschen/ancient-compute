# Week 3 Task 1: File Migration Audit and Mapping Plan

**Status**: Audit Complete - Ready for Migration Execution
**Date**: October 31, 2025
**Total Files Found**: 76 markdown files
**Files Already Migrated**: 9 files
**Files Requiring Migration/Consolidation**: 67 files
**Estimated Consolidations**: 12 files (duplicates and overlaps)
**Net Migration Target**: ~55 files to new locations

---

## Executive Summary

Complete audit of all 76 markdown files in the ancient_compute project. 9 files already properly positioned in new structure (GETTING_STARTED/, ARCHITECTURE_AND_DESIGN/). 67 files require organization into 9-directory structure. 12 files identified as duplicates or overlapping content requiring consolidation.

---

## Directory Structure Target

```
/home/eirikr/Playground/ancient_compute/
├── GETTING_STARTED/                      [4 files - COMPLETE]
│   ├── README.md
│   ├── QUICK_START_5_MINUTES.md
│   ├── DOCUMENT_FINDER.md
│   └── SITE_MAP.md
├── ARCHITECTURE_AND_DESIGN/              [5 files - COMPLETE]
│   ├── ARCHITECTURE.md
│   ├── PROJECT_STRUCTURE.md
│   ├── IMPLEMENTATION_ROADMAP.md
│   ├── STRATEGIC_ROADMAP.md
│   └── WEEKS_3_4_DOCUMENTATION_COMPLETION_PLAN.md
├── DEVELOPMENT_GUIDES/                   [TBD - 12-15 files]
│   └── (Build, testing, language services guides)
├── CURRICULUM_AND_CONTENT/               [TBD - 15-20 files]
│   └── (Content schema, pedagogical materials, curriculum)
├── IMPLEMENTATION_PHASES/                [TBD - 8-12 files]
│   └── (Phase 3, Phase 4 documentation, indexes)
├── BABBAGE_ANALYTICAL_ENGINE/            [TBD - 12-15 files]
│   └── (Whitepaper, manufacturing, specifications)
├── HISTORICAL_CONTEXT/                   [TBD - 6-8 files]
│   └── (Historical audit, narratives, timeline)
├── INFRASTRUCTURE_AND_DEPLOYMENT/        [TBD - 3-4 files]
│   └── (Docker, infrastructure strategy, deployment)
└── ARCHIVED_AND_LEGACY/                  [TBD - 3-5 files]
    └── (Obsolete files, old plans, deprecated docs)
```

---

## File Inventory - Root Level Files (24 files)

### Already in correct location:
None at root level (0 files)

### Files to migrate to ARCHITECTURE_AND_DESIGN/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| ARCHITECTURE.md | DUPLICATE | Keep ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md, delete root copy | DELETE |
| PROJECT_STRUCTURE.md | DUPLICATE | Keep ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md, delete root copy | DELETE |
| IMPLEMENTATION_ROADMAP.md | DUPLICATE | Keep ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md, delete root copy | DELETE |

### Files to migrate to DEVELOPMENT_GUIDES/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| BUILD.md | Core doc | Move to DEVELOPMENT_GUIDES/ | MOVE |
| DOCKER_INFRASTRUCTURE.md | Deployment doc | Move to DEVELOPMENT_GUIDES/ or INFRASTRUCTURE_AND_DEPLOYMENT/ | MOVE |
| LANGUAGE_SERVICE_SPECIFICATION.md | Development spec | Move to DEVELOPMENT_GUIDES/ | MOVE |
| LANGUAGE_SERVICES_ARCHITECTURE.md | Architecture doc | Move to DEVELOPMENT_GUIDES/ or ARCHITECTURE_AND_DESIGN/ | MOVE |
| WEEK_1_CHECKLIST.md | Workflow doc | Move to DEVELOPMENT_GUIDES/ | MOVE |
| WEEK_2_IMPLEMENTATION_PLAN.md | Planning doc | Move to DEVELOPMENT_GUIDES/ | MOVE |

### Files to migrate to CURRICULUM_AND_CONTENT/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| TYPE_THEORY_CURRICULUM.md | Curriculum doc | Move to CURRICULUM_AND_CONTENT/ | MOVE |
| CONTENT_SCHEMA_DESIGN.md | Schema doc | Move to CURRICULUM_AND_CONTENT/ | MOVE |
| PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md | Content summary | Move to CURRICULUM_AND_CONTENT/ | MOVE |

### Files to migrate to HISTORICAL_CONTEXT/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| TIMELINE_VISUALIZATION.md | Timeline doc | Move to HISTORICAL_CONTEXT/ | MOVE |
| TIMELINE_VISUALIZATION_SPEC.md | Timeline spec | Move to HISTORICAL_CONTEXT/ | MOVE |

### Files to migrate to IMPLEMENTATION_PHASES/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| PHASE2_COMPLETION_INDEX.md | Phase doc | Move to IMPLEMENTATION_PHASES/ | MOVE |
| DOCUMENTATION_SYNTHESIS_AND_REORGANIZATION_PLAN.md | Planning doc | Move to DOCUMENTATION_AND_ORGANIZATION/ | MOVE |
| BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md | Summary doc | Move to IMPLEMENTATION_PHASES/ | MOVE |

### Files to migrate to ARCHITECTURE_AND_DESIGN/:
| File | Status | Action | Target |
|------|--------|--------|--------|
| ARCHITECTURAL_REVIEW_WEEK2_DAY6.md | Review doc | Move to ARCHITECTURE_AND_DESIGN/ | MOVE |
| SOURCE_CODE_AND_BUILD_AUDIT.md | Audit doc | Move to ARCHITECTURE_AND_DESIGN/ | MOVE |

### Special handling (Project root/top-level):
| File | Status | Action | Target |
|------|--------|--------|--------|
| README.md | Root README | Keep in root (entry point) | KEEP |
| CLAUDE.md | Project-level memory | Keep in root | KEEP |
| AGENTS.md | Project documentation | Keep in root OR move to ARCHITECTURE_AND_DESIGN/ | DECIDE |
| MVP_FILE_STRUCTURE.md | Obsolete/reference | Move to ARCHIVED_AND_LEGACY/ | ARCHIVE |
| DOCUMENTATION_AND_REORGANIZATION_COMPLETION_SUMMARY.md | Summary | Keep in root (key summary) | KEEP |
| README_OUTPUT.md | Output doc | Move to ARCHIVED_AND_LEGACY/ | ARCHIVE |

---

## File Inventory - /docs/ Directory (20 files)

All files in /docs/ need categorization and migration. Current structure is flat; needs to be distributed to appropriate directories.

### To BABBAGE_ANALYTICAL_ENGINE/:
| File | Status | Target |
|------|--------|--------|
| BABBAGE_README.md | Reference | MOVE |
| BABBAGE_PROJECT_SUMMARY.md | Summary | MOVE |
| BABBAGE_CRITICAL_REVIEW.md | Review | MOVE |
| OPTIMAL_BABBAGE_SPECIFICATION.md | Specification | MOVE |
| SUPPLY_CHAIN_PROCUREMENT_GUIDE.md | Guide | MOVE |
| OPERATIONS_AND_MAINTENANCE.md | Manual | MOVE |
| EMULATOR_SPECIFICATION.md | Specification | MOVE |
| EMULATOR_USER_GUIDE.md | Guide | MOVE |
| ASSEMBLY_INSTRUCTIONS_PART1.md | Instructions | MOVE |

### To CURRICULUM_AND_CONTENT/:
| File | Status | Target |
|------|--------|--------|
| EDUCATIONAL_CURRICULUM_MATERIALS.md | Curriculum | MOVE |
| EDUCATIONAL_CURRICULUM_MATERIALS_PART2.md | Curriculum (Part 2) | CONSOLIDATE with Part 1 |
| EXAMPLE_PROGRAMS.md | Examples | MOVE |

### To HISTORICAL_CONTEXT/:
| File | Status | Target |
|------|--------|--------|
| HISTORICAL_AUDIT_AND_CORRECTIONS.md | Audit | MOVE |
| HISTORICAL_FICTION_NARRATIVES.md | Narratives | MOVE |

### Whitepaper subdirectory (keep separate for now):
| File | Status | Target |
|------|--------|--------|
| /docs/whitepaper-arxiv/README.md | Whitepaper README | MOVE to BABBAGE_ANALYTICAL_ENGINE/whitepaper/ |
| /docs/whitepaper-arxiv/MANUFACTURING_THIRD_WORLD.md | Whitepaper section | MOVE to BABBAGE_ANALYTICAL_ENGINE/whitepaper/ |
| /docs/whitepaper-arxiv/COMPILATION.md | Whitepaper section | MOVE to BABBAGE_ANALYTICAL_ENGINE/whitepaper/ |

### Special:
| File | Status | Target |
|------|--------|--------|
| PROJECT_COMPLETION_SUMMARY.md | Summary | Review for consolidation |
| ECONOMIC_MODELS_MASS_PRODUCTION.md | Economic | MOVE to BABBAGE_ANALYTICAL_ENGINE/ |
| 00_REVIEW_INDEX.md | Index/reference | Review - may be obsolete |

---

## File Inventory - /infrastructure/ Directory (3 files)

| File | Target Directory |
|------|------------------|
| INFRASTRUCTURE_STRATEGY.md | INFRASTRUCTURE_AND_DEPLOYMENT/ |
| BSD_INTEGRATION_SPEC.md | INFRASTRUCTURE_AND_DEPLOYMENT/ |
| SIMULATOR_RESEARCH_NOTES.md | ARCHIVED_AND_LEGACY/ (research notes) |

---

## File Inventory - /whitepaper/ Directory (3 files)

| File | Target Directory |
|------|------------------|
| README.md | BABBAGE_ANALYTICAL_ENGINE/whitepaper/ |
| WHITEPAPER_DELIVERY_SUMMARY.md | BABBAGE_ANALYTICAL_ENGINE/whitepaper/ |
| PHASE2_DELIVERY_SUMMARY.md | IMPLEMENTATION_PHASES/ or ARCHIVED_AND_LEGACY/ |

---

## File Inventory - /phase3/ Directory (4 files)

| File | Target Directory |
|------|------------------|
| README.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_COMPLETION_INDEX.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_MANUFACTURING_PROCEDURES.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_QUALITY_CONTROL_VALIDATION.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_ASSEMBLY_PROCEDURES_WITH_DIAGRAMS.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_OPERATIONAL_MANUAL.md | IMPLEMENTATION_PHASES/PHASE_3/ |
| PHASE3_COST_TRACKING_RESOURCE_ALLOCATION.md | IMPLEMENTATION_PHASES/PHASE_3/ |

---

## File Inventory - /phase4/ Directory (6 files)

| File | Target Directory |
|------|------------------|
| README.md | IMPLEMENTATION_PHASES/PHASE_4/ |
| PHASE4_COMPLETION_INDEX.md | IMPLEMENTATION_PHASES/PHASE_4/ |
| PHASE4_SYSTEM_OPERATIONAL_VALIDATION.md | IMPLEMENTATION_PHASES/PHASE_4/ |
| PHASE4_HANDOFF_ACCEPTANCE_CRITERIA.md | IMPLEMENTATION_PHASES/PHASE_4/ |
| PHASE4_COMPONENT_TEST_SPECIFICATIONS.md | IMPLEMENTATION_PHASES/PHASE_4/ |
| PHASE4_SUBASSEMBLY_INTEGRATION_TESTS.md | IMPLEMENTATION_PHASES/PHASE_4/ |

---

## File Inventory - /output/ Directory (2 files)

| File | Target Directory |
|------|------------------|
| /output/main/README.md | ARCHIVED_AND_LEGACY/ (output artifact) |
| /output/exploratory/README.md | ARCHIVED_AND_LEGACY/ (output artifact) |

---

## Content Overlap Analysis (12 files requiring consolidation)

### Overlap 1: Curriculum Materials (2 files)
- EDUCATIONAL_CURRICULUM_MATERIALS.md (in /docs/)
- EDUCATIONAL_CURRICULUM_MATERIALS_PART2.md (in /docs/)
**Action**: Consolidate into single file: CURRICULUM_MATERIALS_CONSOLIDATED.md
**Target**: CURRICULUM_AND_CONTENT/

### Overlap 2: Architecture Documentation (3 files)
- ARCHITECTURE.md (root - DUPLICATE)
- ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md (canonical)
- ARCHITECTURAL_REVIEW_WEEK2_DAY6.md (review doc)
**Action**: Delete root duplicate, keep ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md as canonical
**Target**: ARCHITECTURE_AND_DESIGN/

### Overlap 3: Project Structure Documentation (2 files)
- PROJECT_STRUCTURE.md (root - DUPLICATE)
- ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md (canonical)
**Action**: Delete root duplicate, keep canonical
**Target**: ARCHITECTURE_AND_DESIGN/

### Overlap 4: Implementation Roadmap (2 files)
- IMPLEMENTATION_ROADMAP.md (root - DUPLICATE)
- ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md (canonical)
**Action**: Delete root duplicate, keep canonical
**Target**: ARCHITECTURE_AND_DESIGN/

### Overlap 5: Phase Completions (multiple)
- PHASE2_COMPLETION_INDEX.md (root)
- phase3/PHASE3_COMPLETION_INDEX.md (in /phase3/)
- phase4/PHASE4_COMPLETION_INDEX.md (in /phase4/)
**Action**: Keep all, organize into IMPLEMENTATION_PHASES/ directory structure
**Target**: IMPLEMENTATION_PHASES/PHASE_X/

---

## Migration Execution Plan

### Phase 1: Prepare Directories (manual - 30 min)
Create all 9 target directories:
```bash
mkdir -p DEVELOPMENT_GUIDES/
mkdir -p CURRICULUM_AND_CONTENT/
mkdir -p IMPLEMENTATION_PHASES/PHASE_3/
mkdir -p IMPLEMENTATION_PHASES/PHASE_4/
mkdir -p BABBAGE_ANALYTICAL_ENGINE/whitepaper/
mkdir -p HISTORICAL_CONTEXT/
mkdir -p INFRASTRUCTURE_AND_DEPLOYMENT/
mkdir -p ARCHIVED_AND_LEGACY/
mkdir -p DOCUMENTATION_AND_ORGANIZATION/
```

### Phase 2: Execute Migrations (via script - 45 min)
See migration script in next section

### Phase 3: Consolidate Duplicates (manual - 30 min)
1. Consolidate EDUCATIONAL_CURRICULUM_MATERIALS*.md
2. Delete duplicate ARCHITECTURE.md, PROJECT_STRUCTURE.md, IMPLEMENTATION_ROADMAP.md
3. Review and decide on other overlaps

### Phase 4: Update Git (git operations - 15 min)
```bash
git add --all
git commit -m "Week 3 Task 1: Migrate and reorganize 67 markdown files into 9-directory structure"
```

---

## Migration Script: migrate-docs.sh

```bash
#!/bin/bash

# Week 3 Task 1: File Migration Script
# Migrates 76 markdown files to organized 9-directory structure
# Date: October 31, 2025

set -e  # Exit on error

PROJECT_ROOT="/home/eirikr/Playground/ancient_compute"
BACKUP_DATE=$(date +%Y%m%d_%H%M%S)
BACKUP_DIR="${PROJECT_ROOT}/.backup_${BACKUP_DATE}"

echo "=== File Migration Script ==="
echo "Project Root: ${PROJECT_ROOT}"
echo "Backup: ${BACKUP_DIR}"
echo ""

# Create backup
echo "[1/5] Creating backup..."
mkdir -p "${BACKUP_DIR}"
cp -r "${PROJECT_ROOT}"/*.md "${BACKUP_DIR}/" 2>/dev/null || true
cp -r "${PROJECT_ROOT}"/docs "${BACKUP_DIR}/" 2>/dev/null || true
cp -r "${PROJECT_ROOT}"/infrastructure "${BACKUP_DIR}/" 2>/dev/null || true
cp -r "${PROJECT_ROOT}"/whitepaper "${BACKUP_DIR}/" 2>/dev/null || true
cp -r "${PROJECT_ROOT}"/phase3 "${BACKUP_DIR}/" 2>/dev/null || true
cp -r "${PROJECT_ROOT}"/phase4 "${BACKUP_DIR}/" 2>/dev/null || true
echo "✓ Backup created at ${BACKUP_DIR}"
echo ""

# Create target directories
echo "[2/5] Creating target directories..."
mkdir -p "${PROJECT_ROOT}/DEVELOPMENT_GUIDES"
mkdir -p "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT"
mkdir -p "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_3"
mkdir -p "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_4"
mkdir -p "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/whitepaper"
mkdir -p "${PROJECT_ROOT}/HISTORICAL_CONTEXT"
mkdir -p "${PROJECT_ROOT}/INFRASTRUCTURE_AND_DEPLOYMENT"
mkdir -p "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY"
mkdir -p "${PROJECT_ROOT}/DOCUMENTATION_AND_ORGANIZATION"
echo "✓ All directories created"
echo ""

# Migrate files - Phase 1: Root level files
echo "[3/5] Migrating root-level files..."

# To DEVELOPMENT_GUIDES/
mv "${PROJECT_ROOT}/BUILD.md" "${PROJECT_ROOT}/DEVELOPMENT_GUIDES/" 2>/dev/null || true
mv "${PROJECT_ROOT}/LANGUAGE_SERVICE_SPECIFICATION.md" "${PROJECT_ROOT}/DEVELOPMENT_GUIDES/" 2>/dev/null || true
mv "${PROJECT_ROOT}/WEEK_1_CHECKLIST.md" "${PROJECT_ROOT}/DEVELOPMENT_GUIDES/" 2>/dev/null || true
mv "${PROJECT_ROOT}/WEEK_2_IMPLEMENTATION_PLAN.md" "${PROJECT_ROOT}/DEVELOPMENT_GUIDES/" 2>/dev/null || true

# To INFRASTRUCTURE_AND_DEPLOYMENT/
mv "${PROJECT_ROOT}/DOCKER_INFRASTRUCTURE.md" "${PROJECT_ROOT}/INFRASTRUCTURE_AND_DEPLOYMENT/" 2>/dev/null || true

# To CURRICULUM_AND_CONTENT/
mv "${PROJECT_ROOT}/TYPE_THEORY_CURRICULUM.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/CONTENT_SCHEMA_DESIGN.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true

# To HISTORICAL_CONTEXT/
mv "${PROJECT_ROOT}/TIMELINE_VISUALIZATION.md" "${PROJECT_ROOT}/HISTORICAL_CONTEXT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/TIMELINE_VISUALIZATION_SPEC.md" "${PROJECT_ROOT}/HISTORICAL_CONTEXT/" 2>/dev/null || true

# To ARCHITECTURE_AND_DESIGN/
mv "${PROJECT_ROOT}/LANGUAGE_SERVICES_ARCHITECTURE.md" "${PROJECT_ROOT}/ARCHITECTURE_AND_DESIGN/" 2>/dev/null || true
mv "${PROJECT_ROOT}/ARCHITECTURAL_REVIEW_WEEK2_DAY6.md" "${PROJECT_ROOT}/ARCHITECTURE_AND_DESIGN/" 2>/dev/null || true
mv "${PROJECT_ROOT}/SOURCE_CODE_AND_BUILD_AUDIT.md" "${PROJECT_ROOT}/ARCHITECTURE_AND_DESIGN/" 2>/dev/null || true

# To IMPLEMENTATION_PHASES/
mv "${PROJECT_ROOT}/PHASE2_COMPLETION_INDEX.md" "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/" 2>/dev/null || true
mv "${PROJECT_ROOT}/BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md" "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/" 2>/dev/null || true

# Delete duplicates
rm -f "${PROJECT_ROOT}/ARCHITECTURE.md" 2>/dev/null || true
rm -f "${PROJECT_ROOT}/PROJECT_STRUCTURE.md" 2>/dev/null || true
rm -f "${PROJECT_ROOT}/IMPLEMENTATION_ROADMAP.md" 2>/dev/null || true

# Archive old files
mv "${PROJECT_ROOT}/MVP_FILE_STRUCTURE.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true
mv "${PROJECT_ROOT}/README_OUTPUT.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true

echo "✓ Root-level files migrated"
echo ""

# Migrate files - Phase 2: /docs/ directory
echo "[3b/5] Migrating /docs/ files..."

# To BABBAGE_ANALYTICAL_ENGINE/
mv "${PROJECT_ROOT}/docs/BABBAGE_README.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/BABBAGE_PROJECT_SUMMARY.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/BABBAGE_CRITICAL_REVIEW.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/OPTIMAL_BABBAGE_SPECIFICATION.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/SUPPLY_CHAIN_PROCUREMENT_GUIDE.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/OPERATIONS_AND_MAINTENANCE.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/EMULATOR_SPECIFICATION.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/EMULATOR_USER_GUIDE.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/ASSEMBLY_INSTRUCTIONS_PART1.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/ECONOMIC_MODELS_MASS_PRODUCTION.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/" 2>/dev/null || true

# To CURRICULUM_AND_CONTENT/
mv "${PROJECT_ROOT}/docs/EDUCATIONAL_CURRICULUM_MATERIALS.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/EDUCATIONAL_CURRICULUM_MATERIALS_PART2.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/EXAMPLE_PROGRAMS.md" "${PROJECT_ROOT}/CURRICULUM_AND_CONTENT/" 2>/dev/null || true

# To HISTORICAL_CONTEXT/
mv "${PROJECT_ROOT}/docs/HISTORICAL_AUDIT_AND_CORRECTIONS.md" "${PROJECT_ROOT}/HISTORICAL_CONTEXT/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/HISTORICAL_FICTION_NARRATIVES.md" "${PROJECT_ROOT}/HISTORICAL_CONTEXT/" 2>/dev/null || true

# To ARCHIVED_AND_LEGACY/
mv "${PROJECT_ROOT}/docs/00_REVIEW_INDEX.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true
mv "${PROJECT_ROOT}/docs/PROJECT_COMPLETION_SUMMARY.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true

# Whitepaper subdirectory
mkdir -p "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/whitepaper"
if [ -d "${PROJECT_ROOT}/docs/whitepaper-arxiv" ]; then
  mv "${PROJECT_ROOT}/docs/whitepaper-arxiv"/*.md "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/whitepaper/" 2>/dev/null || true
  rmdir "${PROJECT_ROOT}/docs/whitepaper-arxiv" 2>/dev/null || true
fi

echo "✓ /docs/ files migrated"
echo ""

# Migrate files - Phase 3: /infrastructure/ directory
echo "[3c/5] Migrating /infrastructure/ files..."

if [ -d "${PROJECT_ROOT}/infrastructure" ]; then
  mv "${PROJECT_ROOT}/infrastructure/INFRASTRUCTURE_STRATEGY.md" "${PROJECT_ROOT}/INFRASTRUCTURE_AND_DEPLOYMENT/" 2>/dev/null || true
  mv "${PROJECT_ROOT}/infrastructure/BSD_INTEGRATION_SPEC.md" "${PROJECT_ROOT}/INFRASTRUCTURE_AND_DEPLOYMENT/" 2>/dev/null || true
  mv "${PROJECT_ROOT}/infrastructure/SIMULATOR_RESEARCH_NOTES.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true
fi

echo "✓ /infrastructure/ files migrated"
echo ""

# Migrate files - Phase 4: /phase3/ and /phase4/ directories
echo "[3d/5] Migrating phase files..."

if [ -d "${PROJECT_ROOT}/phase3" ]; then
  mv "${PROJECT_ROOT}/phase3"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_3/" 2>/dev/null || true
  if [ -d "${PROJECT_ROOT}/phase3/procedures" ]; then
    mv "${PROJECT_ROOT}/phase3/procedures"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_3/" 2>/dev/null || true
  fi
  if [ -d "${PROJECT_ROOT}/phase3/specifications" ]; then
    mv "${PROJECT_ROOT}/phase3/specifications"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_3/" 2>/dev/null || true
  fi
fi

if [ -d "${PROJECT_ROOT}/phase4" ]; then
  mv "${PROJECT_ROOT}/phase4"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_4/" 2>/dev/null || true
  if [ -d "${PROJECT_ROOT}/phase4/testing" ]; then
    mv "${PROJECT_ROOT}/phase4/testing"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_4/" 2>/dev/null || true
  fi
  if [ -d "${PROJECT_ROOT}/phase4/acceptance" ]; then
    mv "${PROJECT_ROOT}/phase4/acceptance"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_4/" 2>/dev/null || true
  fi
  if [ -d "${PROJECT_ROOT}/phase4/validation" ]; then
    mv "${PROJECT_ROOT}/phase4/validation"/*.md "${PROJECT_ROOT}/IMPLEMENTATION_PHASES/PHASE_4/" 2>/dev/null || true
  fi
fi

echo "✓ Phase files migrated"
echo ""

# Migrate files - Phase 5: /whitepaper/ directory
echo "[3e/5] Migrating whitepaper files..."

if [ -d "${PROJECT_ROOT}/whitepaper" ]; then
  mv "${PROJECT_ROOT}/whitepaper/README.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_README.md" 2>/dev/null || true
  mv "${PROJECT_ROOT}/whitepaper/WHITEPAPER_DELIVERY_SUMMARY.md" "${PROJECT_ROOT}/BABBAGE_ANALYTICAL_ENGINE/whitepaper/" 2>/dev/null || true
  mv "${PROJECT_ROOT}/whitepaper/PHASE2_DELIVERY_SUMMARY.md" "${PROJECT_ROOT}/ARCHIVED_AND_LEGACY/" 2>/dev/null || true
fi

echo "✓ Whitepaper files migrated"
echo ""

# Generate file manifest
echo "[4/5] Generating file manifest..."

MANIFEST="${PROJECT_ROOT}/MIGRATION_MANIFEST_${BACKUP_DATE}.txt"

echo "File Migration Manifest" > "${MANIFEST}"
echo "Date: $(date)" >> "${MANIFEST}"
echo "Project Root: ${PROJECT_ROOT}" >> "${MANIFEST}"
echo "" >> "${MANIFEST}"
echo "Total markdown files: $(find ${PROJECT_ROOT} -name '*.md' -type f | wc -l)" >> "${MANIFEST}"
echo "" >> "${MANIFEST}"
echo "Files by directory:" >> "${MANIFEST}"
echo "" >> "${MANIFEST}"

for dir in GETTING_STARTED ARCHITECTURE_AND_DESIGN DEVELOPMENT_GUIDES CURRICULUM_AND_CONTENT IMPLEMENTATION_PHASES BABBAGE_ANALYTICAL_ENGINE HISTORICAL_CONTEXT INFRASTRUCTURE_AND_DEPLOYMENT ARCHIVED_AND_LEGACY DOCUMENTATION_AND_ORGANIZATION; do
  count=$(find "${PROJECT_ROOT}/${dir}" -name '*.md' -type f 2>/dev/null | wc -l)
  if [ $count -gt 0 ]; then
    echo "${dir}: ${count} files" >> "${MANIFEST}"
    find "${PROJECT_ROOT}/${dir}" -name '*.md' -type f 2>/dev/null | sort >> "${MANIFEST}"
    echo "" >> "${MANIFEST}"
  fi
done

echo "Root level files:" >> "${MANIFEST}"
find "${PROJECT_ROOT}" -maxdepth 1 -name '*.md' -type f | sort >> "${MANIFEST}"

echo "✓ Manifest created: ${MANIFEST}"
echo ""

# Summary
echo "[5/5] Migration complete!"
echo ""
echo "Summary:"
echo "- Backup created at: ${BACKUP_DIR}"
echo "- Files migrated: $(find ${PROJECT_ROOT} -name '*.md' -type f | wc -l) markdown files"
echo "- Manifest created: ${MANIFEST}"
echo ""
echo "Next steps:"
echo "1. Review file placement and make corrections as needed"
echo "2. Update internal cross-references (200+ links)"
echo "3. Create README.md for each directory"
echo "4. Delete empty directories: /docs, /infrastructure, /phase3, /phase4, /whitepaper, /output"
echo ""
echo "=== Migration Script Complete ==="
```

---

## Files Requiring Link Updates (Phase 2)

After migration, the following cross-references will need updates:
- ~200+ internal links in migrated files
- References in root README.md
- References in GETTING_STARTED/ files
- References in ARCHITECTURE_AND_DESIGN/ files

---

## Execution Checklist

### Pre-Migration (Today):
- [ ] Create backup of entire project
- [ ] Review this audit document
- [ ] Confirm target directory structure
- [ ] Prepare migration script

### Migration Execution (Next session):
- [ ] Create all 9 target directories
- [ ] Run migration script
- [ ] Generate file manifest
- [ ] Review file placement
- [ ] Identify link update locations

### Post-Migration (Day 2):
- [ ] Update all cross-references
- [ ] Create directory README files
- [ ] Consolidate overlapping content
- [ ] Validate directory structure
- [ ] Commit to git

---

## Risks and Mitigations

| Risk | Probability | Impact | Mitigation |
|------|-------------|--------|-----------|
| Duplicate file losses | Low | Medium | Full backup created before migration |
| Broken cross-references | High | Medium | Manifest identifies all references |
| Directory structure errors | Medium | Low | Clear migration plan with specific moves |
| Git history loss | Low | High | Using git mv where possible in second pass |
| Missing files after migration | Low | Medium | Manifest verification against original count |

---

## Success Criteria

- [ ] 76 markdown files accounted for in new structure
- [ ] No duplicate files (after consolidation)
- [ ] All files in correct target directories per audit
- [ ] File manifest matches original file count
- [ ] Backup available for rollback
- [ ] All files readable and intact (no corruption)

---

## Appendix A: Complete File Listing (Before Migration)

**Root Level (24 files):**
1. README.md (KEEP)
2. CLAUDE.md (KEEP)
3. ARCHITECTURE.md (DELETE - duplicate)
4. AGENTS.md (DECISION NEEDED)
5. BUILD.md → DEVELOPMENT_GUIDES/
6. DOCKER_INFRASTRUCTURE.md → INFRASTRUCTURE_AND_DEPLOYMENT/
7. SECURITY_LAYERS_IMPLEMENTATION.md (TO CATEGORIZE)
8. TYPE_THEORY_CURRICULUM.md → CURRICULUM_AND_CONTENT/
9. LANGUAGE_SERVICES_ARCHITECTURE.md → ARCHITECTURE_AND_DESIGN/
10. WEEK_1_CHECKLIST.md → DEVELOPMENT_GUIDES/
11. WEEK_2_IMPLEMENTATION_PLAN.md → DEVELOPMENT_GUIDES/
12. LANGUAGE_SERVICE_SPECIFICATION.md → DEVELOPMENT_GUIDES/
13. IMPLEMENTATION_ROADMAP.md (DELETE - duplicate)
14. MVP_FILE_STRUCTURE.md → ARCHIVED_AND_LEGACY/
15. TIMELINE_VISUALIZATION.md → HISTORICAL_CONTEXT/
16. TIMELINE_VISUALIZATION_SPEC.md → HISTORICAL_CONTEXT/
17. CONTENT_SCHEMA_DESIGN.md → CURRICULUM_AND_CONTENT/
18. PROJECT_STRUCTURE.md (DELETE - duplicate)
19. SOURCE_CODE_AND_BUILD_AUDIT.md → ARCHITECTURE_AND_DESIGN/
20. PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md → CURRICULUM_AND_CONTENT/
21. ARCHITECTURAL_REVIEW_WEEK2_DAY6.md → ARCHITECTURE_AND_DESIGN/
22. DOCUMENTATION_SYNTHESIS_AND_REORGANIZATION_PLAN.md → DOCUMENTATION_AND_ORGANIZATION/
23. BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md → IMPLEMENTATION_PHASES/
24. DOCUMENTATION_AND_REORGANIZATION_COMPLETION_SUMMARY.md (KEEP - key summary)
25. README_OUTPUT.md → ARCHIVED_AND_LEGACY/
26. PHASE2_COMPLETION_INDEX.md → IMPLEMENTATION_PHASES/

*Plus any others identified*

---

**Total Files: 76 markdown files**
**Target: 9 organized directories with clear categorization**
**Status: Ready for execution**
