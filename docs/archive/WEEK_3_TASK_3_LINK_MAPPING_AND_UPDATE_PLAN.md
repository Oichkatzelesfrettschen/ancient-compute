# Week 3 Task 3: Link Mapping and Cross-Reference Update Plan

## Archive Metadata

- Archive Status: archived
- Archive Reason: migration_or_audit_artifact
- Canonical Successor: docs/general/PLANNING_CANONICAL_MAP.md; docs/archive/INDEX.md
- Novel Content Integrated: yes
- Merged Into: docs/general/PLANNING_CANONICAL_MAP.md
- Last Validated: 2026-02-24


**Task**: Update 200+ internal cross-references after file migration
**Status**: PLANNING PHASE
**Start Date**: October 31, 2025
**Target Completion**: By November 5, 2025
**Estimated Effort**: 6-8 hours

---

## Executive Summary

After migrating 67 markdown files from a flat structure to 9 organized directories, all internal cross-references (links) pointing to old file locations need updating. This document provides:

1. **Link Audit**: Complete inventory of all cross-references
2. **Path Mapping**: Old paths → New paths for every file
3. **Update Strategy**: Approach for fixing all links
4. **Validation Plan**: How to verify all links work
5. **Automation Scripts**: Tools to assist with bulk updates

---

## Link Categories

### Category 1: Old Root-Level References
**Pattern**: Links pointing to files that were in project root

Example old reference:
```markdown
[See DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md](../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md)
[Check ../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
```

Needs updating to:
```markdown
[See DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md](../DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md)
[Check ../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../../../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
```

### Category 2: /docs/ Directory References
**Pattern**: Links pointing to `/docs/` subdirectory files

Example old reference:
```markdown
[Manufacturing specs](../BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md)
[Curriculum](../CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS.md)
```

Needs updating to:
```markdown
[Manufacturing specs](../BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md)
[Curriculum](../CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS.md)
```

### Category 3: /infrastructure/ Directory References
**Pattern**: Links pointing to `/infrastructure/` subdirectory files

Example old reference:
```markdown
[Infrastructure guide](../INFRASTRUCTURE_AND_DEPLOYMENT/INFRASTRUCTURE_STRATEGY.md)
```

Needs updating to:
```markdown
[Infrastructure guide](../INFRASTRUCTURE_AND_DEPLOYMENT/INFRASTRUCTURE_STRATEGY.md)
```

### Category 4: /whitepaper/ Directory References
**Pattern**: Links pointing to `/whitepaper/` subdirectory files

Example old reference:
```markdown
[Whitepaper summary](../BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_DELIVERY_SUMMARY.md)
```

Needs updating to:
```markdown
[Whitepaper summary](../BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_DELIVERY_SUMMARY.md)
```

### Category 5: /phase3/ and /phase4/ Directory References
**Pattern**: Links pointing to phase-specific subdirectory files

Example old reference:
```markdown
[Phase 3 procedures](../IMPLEMENTATION_PHASES/PHASE_3/PHASE3_MANUFACTURING_PROCEDURES.md)
[Phase 4 validation](../IMPLEMENTATION_PHASES/PHASE_4/PHASE4_SYSTEM_OPERATIONAL_VALIDATION.md)
```

Needs updating to:
```markdown
[Phase 3 procedures](../IMPLEMENTATION_PHASES/PHASE_3/PHASE3_MANUFACTURING_PROCEDURES.md)
[Phase 4 validation](../IMPLEMENTATION_PHASES/PHASE_4/PHASE4_SYSTEM_OPERATIONAL_VALIDATION.md)
```

---

## Complete File Path Mapping

### DEVELOPMENT_GUIDES Directory
| Old Path | New Path |
|----------|----------|
| DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md | DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md |
| DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/LANGUAGE_SERVICE_SPECIFICATION.md | DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/LANGUAGE_SERVICE_SPECIFICATION.md |
| DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_1_CHECKLIST.md | DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_1_CHECKLIST.md |
| DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_2_IMPLEMENTATION_PLAN.md | DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_2_IMPLEMENTATION_PLAN.md |
| DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/SECURITY_LAYERS_IMPLEMENTATION.md | DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/SECURITY_LAYERS_IMPLEMENTATION.md |

### CURRICULUM_AND_CONTENT Directory
| Old Path | New Path |
|----------|----------|
| ../CURRICULUM_AND_CONTENT/TYPE_THEORY_CURRICULUM.md | ../../CURRICULUM_AND_CONTENT/TYPE_THEORY_CURRICULUM.md |
| ../CURRICULUM_AND_CONTENT/CONTENT_SCHEMA_DESIGN.md | ../../CURRICULUM_AND_CONTENT/CONTENT_SCHEMA_DESIGN.md |
| ../CURRICULUM_AND_CONTENT/PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md | ../../CURRICULUM_AND_CONTENT/PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md |
| CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS.md | CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS.md |
| CURRICULUM_AND_CONTENT/EXAMPLE_PROGRAMS.md | CURRICULUM_AND_CONTENT/EXAMPLE_PROGRAMS.md |

### ARCHITECTURE_AND_DESIGN Directory
| Old Path | New Path |
|----------|----------|
| LANGUAGE_SERVICES_../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md | ARCHITECTURE_AND_DESIGN/LANGUAGE_SERVICES_../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md |
| ARCHITECTURAL_REVIEW_WEEK2_DAY6.md | ARCHITECTURE_AND_DESIGN/ARCHITECTURAL_REVIEW_WEEK2_DAY6.md |
| SOURCE_CODE_AND_BUILD_AUDIT.md | ARCHITECTURE_AND_DESIGN/SOURCE_CODE_AND_BUILD_AUDIT.md |

### BABBAGE_ANALYTICAL_ENGINE Directory
| Old Path | New Path |
|----------|----------|
| BABBAGE_ANALYTICAL_ENGINE/BABBAGE_README.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_README.md |
| BABBAGE_ANALYTICAL_ENGINE/BABBAGE_PROJECT_SUMMARY.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_PROJECT_SUMMARY.md |
| BABBAGE_ANALYTICAL_ENGINE/BABBAGE_CRITICAL_REVIEW.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_CRITICAL_REVIEW.md |
| BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md | BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md |
| BABBAGE_ANALYTICAL_ENGINE/SUPPLY_CHAIN_PROCUREMENT_GUIDE.md | BABBAGE_ANALYTICAL_ENGINE/SUPPLY_CHAIN_PROCUREMENT_GUIDE.md |
| BABBAGE_ANALYTICAL_ENGINE/OPERATIONS_AND_MAINTENANCE.md | BABBAGE_ANALYTICAL_ENGINE/OPERATIONS_AND_MAINTENANCE.md |
| BABBAGE_ANALYTICAL_ENGINE/EMULATOR_SPECIFICATION.md | BABBAGE_ANALYTICAL_ENGINE/EMULATOR_SPECIFICATION.md |
| BABBAGE_ANALYTICAL_ENGINE/EMULATOR_USER_GUIDE.md | BABBAGE_ANALYTICAL_ENGINE/EMULATOR_USER_GUIDE.md |
| BABBAGE_ANALYTICAL_ENGINE/ASSEMBLY_INSTRUCTIONS_PART1.md | BABBAGE_ANALYTICAL_ENGINE/ASSEMBLY_INSTRUCTIONS_PART1.md |
| BABBAGE_ANALYTICAL_ENGINE/ECONOMIC_MODELS_MASS_PRODUCTION.md | BABBAGE_ANALYTICAL_ENGINE/ECONOMIC_MODELS_MASS_PRODUCTION.md |
| docs/whitepaper-arxiv/README.md | BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_README.md |
| docs/whitepaper-arxiv/WHITEPAPER_DELIVERY_SUMMARY.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_DELIVERY_SUMMARY.md |
| docs/whitepaper-arxiv/MANUFACTURING_THIRD_WORLD.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/MANUFACTURING_THIRD_WORLD.md |
| docs/whitepaper-arxiv/COMPILATION.md | BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/COMPILATION.md |

### HISTORICAL_CONTEXT Directory
| Old Path | New Path |
|----------|----------|
| HISTORICAL_CONTEXT/HISTORICAL_AUDIT_AND_CORRECTIONS.md | HISTORICAL_CONTEXT/HISTORICAL_AUDIT_AND_CORRECTIONS.md |
| HISTORICAL_CONTEXT/HISTORICAL_FICTION_NARRATIVES.md | HISTORICAL_CONTEXT/HISTORICAL_FICTION_NARRATIVES.md |
| HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTEXT/TIMELINE_VISUALIZATION.md | HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTEXT/TIMELINE_VISUALIZATION.md |
| HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTEXT/TIMELINE_VISUALIZATION_SPEC.md | HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTE../HISTORICAL_CONTEXT/HISTORICAL_CONTEXT/TIMELINE_VISUALIZATION_SPEC.md |

### INFRASTRUCTURE_AND_DEPLOYMENT Directory
| Old Path | New Path |
|----------|----------|
| ../INFRASTRUCTURE_AND_DEPLOYMENT/DOCKER_INFRASTRUCTURE.md | ../../INFRASTRUCTURE_AND_DEPLOYMENT/DOCKER_INFRASTRUCTURE.md |
| INFRASTRUCTURE_AND_DEPLOYMENT/INFRASTRUCTURE_STRATEGY.md | INFRASTRUCTURE_AND_DEPLOYMENT/INFRASTRUCTURE_STRATEGY.md |
| INFRASTRUCTURE_AND_DEPLOYMENT/BSD_INTEGRATION_SPEC.md | INFRASTRUCTURE_AND_DEPLOYMENT/BSD_INTEGRATION_SPEC.md |

### IMPLEMENTATION_PHASES Directory
| Old Path | New Path |
|----------|----------|
| IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHASES/PHASE2_COMPLETION_INDEX.md | IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHASES/PHASE2_COMPLETION_INDEX.md |
| IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHASES/BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md | IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHAS../IMPLEMENTATION_PHASES/IMPLEMENTATION_PHASES/BUILD_INFRASTRUCTURE_COMPLETION_SUMMARY.md |
| IMPLEMENTATION_PHASES/PHASE_3/*.md files | IMPLEMENTATION_PHASES/PHASE_3/*.md |
| IMPLEMENTATION_PHASES/PHASE_4/*.md files | IMPLEMENTATION_PHASES/PHASE_4/*.md |

### ARCHIVED_AND_LEGACY Directory
| Old Path | New Path |
|----------|----------|
| ../ARCHIVED_AND_LEGACY/MVP_FILE_STRUCTURE.md | ../../ARCHIVED_AND_LEGACY/MVP_FILE_STRUCTURE.md |
| ../ARCHIVED_AND_LEGACY/README_OUTPUT.md | ../../ARCHIVED_AND_LEGACY/README_OUTPUT.md |
| docs/00_REVIEW_INDEX.md | ARCHIVED_AND_LEGACY/00_REVIEW_INDEX.md |
| docs/PROJECT_COMPLETION_SUMMARY.md | ARCHIVED_AND_LEGACY/PROJECT_COMPLETION_SUMMARY.md |
| infrastructure/SIMULATOR_RESEARCH_NOTES.md | ARCHIVED_AND_LEGACY/SIMULATOR_RESEARCH_NOTES.md |
| whitepaper/PHASE2_DELIVERY_SUMMARY.md | ARCHIVED_AND_LEGACY/PHASE2_DELIVERY_SUMMARY.md |

---

## Link Update Strategy

### Phase 1: Identify All Cross-References (1-2 hours)

**Script 1**: Find all markdown link references
```bash
#!/bin/bash
# Find all markdown files with cross-references
cd /home/eirikr/Playground/ancient_compute

# Search for markdown links in all files
grep -r '\[.*\](.*\.md)' . --include="*.md" | sort | uniq > LINKS_FOUND.txt

# Output format: filename:line_number:[link_text](path)
```

**Expected Result**: ~200+ lines with link references

### Phase 2: Map Links to Update (2-3 hours)

**Script 2**: Generate update mapping
```bash
#!/bin/bash
# Generate mapping of old paths → new paths
# Create a sed script that will replace all old paths with new paths

cat > PATH_MAPPING.sed << 'EOF'
# Root-level files moving to DEVELOPMENT_GUIDES
s|BUILD\.md|DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md|g
s|LANGUAGE_SERVICE_SPECIFICATION\.md|DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/LANGUAGE_SERVICE_SPECIFICATION.md|g
s|WEEK_1_CHECKLIST\.md|DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_1_CHECKLIST.md|g
s|WEEK_2_IMPLEMENTATION_PLAN\.md|DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/WEEK_2_IMPLEMENTATION_PLAN.md|g
s|SECURITY_LAYERS_IMPLEMENTATION\.md|DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/SECURITY_LAYERS_IMPLEMENTATION.md|g

# Root-level files moving to CURRICULUM_AND_CONTENT
s|TYPE_THEORY_CURRICULUM\.md|../../CURRICULUM_AND_CONTENT/TYPE_THEORY_CURRICULUM.md|g
s|CONTENT_SCHEMA_DESIGN\.md|../../CURRICULUM_AND_CONTENT/CONTENT_SCHEMA_DESIGN.md|g
s|PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY\.md|../../CURRICULUM_AND_CONTENT/PEDAGOGICAL_WHITEPAPER_COMPLETION_SUMMARY.md|g

# docs/ files
s|docs/EDUCATIONAL_CURRICULUM_MATERIALS\.md|CURRICULUM_AND_CONTENT/EDUCATIONAL_CURRICULUM_MATERIALS.md|g
s|docs/OPTIMAL_BABBAGE_SPECIFICATION\.md|BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md|g
s|docs/SUPPLY_CHAIN_PROCUREMENT_GUIDE\.md|BABBAGE_ANALYTICAL_ENGINE/SUPPLY_CHAIN_PROCUREMENT_GUIDE.md|g
s|docs/BABBAGE.*\.md|BABBAGE_ANALYTICAL_ENGINE/&|g

# infrastructure/ files
s|infrastructure/INFRASTRUCTURE_STRATEGY\.md|INFRASTRUCTURE_AND_DEPLOYMENT/INFRASTRUCTURE_STRATEGY.md|g
s|infrastructure/BSD_INTEGRATION_SPEC\.md|INFRASTRUCTURE_AND_DEPLOYMENT/BSD_INTEGRATION_SPEC.md|g

# phase files
s|IMPLEMENTATION_PHASES/PHASE_3/\(.*\.md\)|IMPLEMENTATION_PHASES/PHASE_3/\1|g
s|IMPLEMENTATION_PHASES/PHASE_4/\(.*\.md\)|IMPLEMENTATION_PHASES/PHASE_4/\1|g

# whitepaper files
s|whitepaper/WHITEPAPER_DELIVERY_SUMMARY\.md|BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/BABBAGE_ANALYTICAL_ENGINE/whitepaper/WHITEPAPER_DELIVERY_SUMMARY.md|g
EOF
```

### Phase 3: Apply Updates (2-3 hours)

**Script 3**: Apply path mapping to all files
```bash
#!/bin/bash
# Apply link updates to all markdown files

# First, back up all files
cp -r . .backup_links_$(date +%Y%m%d_%H%M%S)/

# Apply sed script to all .md files
find . -name "*.md" -type f -exec sed -i.bak -f PATH_MAPPING.sed {} \;

# Remove backup files
find . -name "*.md.bak" -delete
```

### Phase 4: Manual Review and Correction (1-2 hours)

**Manual Verification**:
1. Check README files for correct links
2. Check key navigation files:
   - GETTING_STARTED/README.md
   - GETTING_STARTED/DOCUMENT_FINDER.md
   - GETTING_STARTED/SITE_MAP.md
3. Spot-check files in each directory
4. Verify relative paths are correct

### Phase 5: Validation (1 hour)

**Script 4**: Validate all links
```bash
#!/bin/bash
# Check for broken links

echo "Checking for link validity..."

for file in $(find . -name "*.md" -type f); do
  # Extract all link paths
  grep -o '\[.*\](.*\.md)' "$file" | sed 's/.*(\(.*\)).*/\1/' | while read path; do
    # Resolve relative path
    resolved=$(cd "$(dirname "$file")" && readlink -f "$path" 2>/dev/null || echo "BROKEN")
    
    if [ "$resolved" = "BROKEN" ]; then
      echo "BROKEN LINK in $file: $path"
    fi
  done
done
```

---

## Files Requiring Link Updates

### High Priority (Critical Navigation)
- GETTING_STARTED/README.md
- GETTING_STARTED/DOCUMENT_FINDER.md
- GETTING_STARTED/SITE_MAP.md
- GETTING_STARTED/QUICK_START_5_MINUTES.md
- README.md (root level)

### Medium Priority (Core Documentation)
- ARCHITECTURE_AND_DESIGN/README.md
- ../../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md
- ../../ARCHITECTURE_AND_DESIGN/PROJECT_STRUCTURE.md
- DEVELOPMENT_GUIDES/README.md
- CURRICULUM_AND_CONTENT/README.md

### Standard Priority (All Other Files)
- All files in other directories
- All newly migrated files
- All phase-specific files

---

## Link Types and Updates

### Type 1: Relative Path Links
**Current**: `[File](../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md)`
**Updated**: `[File](../DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUID../DEVELOPMENT_GUIDES/DEVELOPMENT_GUIDES/BUILD.md)`
**Tool**: sed with relative path handling

### Type 2: Full Path Links
**Current**: `[File](/BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md)`
**Updated**: `[File](/BABBAGE_ANALYTICAL_ENGINE/OPTIMAL_BABBAGE_SPECIFICATION.md)`
**Tool**: sed with full path replacement

### Type 3: Deep Links (within same directory)
**Current**: `[File](./DOCUMENT_FINDER.md)`
**Updated**: `[File](./DOCUMENT_FINDER.md)` (no change)
**Tool**: No update needed

### Type 4: Cross-Directory Links
**Current**: `[File](../GETTING_STARTED/DOCUMENT_FINDER.md)`
**Updated**: `[File](../GETTING_STARTED/DOCUMENT_FINDER.md)` (may need depth adjustment)
**Tool**: Manual verification

---

## Validation Checklist

### Before Applying Updates
- [ ] All old files accounted for in mapping
- [ ] All new locations verified to exist
- [ ] Backup created before bulk update
- [ ] Sed script tested on small sample

### After Applying Updates
- [ ] High-priority files manually checked
- [ ] Link validation script executed
- [ ] No "BROKEN LINK" results
- [ ] Navigation still works (< 3 clicks)
- [ ] Sample files spot-checked

### Quality Assurance
- [ ] All links are relative (portable)
- [ ] No hardcoded absolute paths
- [ ] Cross-directory links correct depth
- [ ] README files have correct links
- [ ] Git diff reviewed before commit

---

## Risk Mitigation

### Risk 1: Broken Links After Update
**Probability**: Medium
**Impact**: High (navigation broken)
**Mitigation**: 
- Test on sample files first
- Manual verification of critical paths
- Validation script catches remaining issues
- Backup available for rollback

### Risk 2: Incorrect Path Depth
**Probability**: Low
**Impact**: Medium (some links broken)
**Mitigation**:
- Review relative path logic
- Test with sample files
- Cross-directory links manually reviewed

### Risk 3: Links to Obsolete Files
**Probability**: Low
**Impact**: Low (dead links, but documented as archived)
**Mitigation**:
- Link to ARCHIVED_AND_LEGACY/ where appropriate
- Document which links point to legacy files
- Update should indicate archived status

---

## Success Criteria

- [ ] All 200+ cross-references updated
- [ ] No broken links in documentation
- [ ] Navigation works (<3 clicks to find any document)
- [ ] High-priority files (GETTING_STARTED/) verified correct
- [ ] Validation script passes all checks
- [ ] Git commit successful with link updates

---

## Timeline

**Week 3 (Nov 1-5)**:
- Day 1: Link audit and mapping (2 hours)
- Day 2: Prepare update scripts (2 hours)
- Day 3: Apply updates and manual review (3 hours)
- Day 4: Validation and correction (2 hours)
- Day 5: Final verification and git commit (1 hour)

**Total Estimated Effort**: 10 hours

---

## Next Phase After Link Updates

Once link updates are complete:
1. Task 4: Consolidate content overlaps (Day 6)
2. Task 5: Create reference standards documents (Week 4)
3. Task 6: Comprehensive validation (Week 4)
4. Task 7: Documentation maintenance process (Week 4)
5. Task 8: Sign-off and team briefing (Week 4)

---

## Tools and Scripts

All scripts will be stored in:
`/home/eirikr/Playground/ancient_compute/scripts/link-update-helpers/`

Required files:
1. `find-links.sh` - Find all markdown links
2. `generate-mapping.sh` - Create sed mapping file
3. `apply-updates.sh` - Apply path replacements
4. `validate-links.sh` - Check for broken links
5. `verify-critical-paths.sh` - Manual check critical files

---

**Status**: READY FOR EXECUTION
**Start Date**: Ready to begin immediately after Task 2 completion
**Estimated Completion**: November 3-5, 2025
