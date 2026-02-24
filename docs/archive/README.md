# ARCHIVED_AND_LEGACY

**Purpose**: Archive of legacy documentation, obsolete plans, research notes, and historical records that are no longer actively maintained but preserved for reference.

**Audience**: Historians, researchers, team members reviewing project evolution, anyone curious about earlier approaches

---

## What's in This Directory

This directory contains:
- **Deprecated documentation**: Plans and specs that were superseded by newer versions
- **Research notes**: Exploratory work that didn't make it to final specifications
- **Legacy specifications**: Earlier versions of components now implemented differently
- **Obsolete tools and processes**: How things used to be done (for reference only)
- **Historical records**: Early decisions, alternatives considered, lessons learned

Planning-specific archive metadata and successor mappings are tracked in:
- `docs/archive/INDEX.md`
- `docs/archive/AUDIT_LEDGER.md`
- `docs/archive/audit_ledger.csv`
- `docs/archive/QUARANTINE.md`
- `docs/general/PLANNING_CANONICAL_MAP.md`

---

## Important Note

**Files in this directory should not be used for current development.**

If you're looking for:
- **Current architecture**: See `docs/general/ARCHITECTURE.md`
- **Development guides**: See `docs/general/BUILD.md`, `docs/general/DEPLOYMENT_GUIDE.md`, and `docs/general/TROUBLESHOOTING_GUIDE.md`
- **Manufacturing specs**: See `docs/babbage_engine/`
- **Curriculum materials**: See `docs/general/TYPE_THEORY_CURRICULUM.md` and `docs/general/EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md`
- **Canonical planning docs**: See `docs/general/MASTER_ROADMAP.md` and `docs/general/TODO_TRACKER.md`

---

## Files

### ./MVP_FILE_STRUCTURE.md
Early project MVP (Minimum Viable Product) file structure plan:
- Original proposed directory layout
- Alternative organizational approaches considered
- Rationale for decisions made
- What changed and why

**Historical significance**: Shows the project's evolution from flat to 9-directory structure.

**Status**: Obsolete - superseded by current structure in `docs/general/PROJECT_STRUCTURE.md`

### ./README_OUTPUT.md
Documentation of build output structure:
- How build artifacts were organized in early phases
- Output directory layout
- Generated documentation handling

**Status**: Obsolete - build process changed in Phase 2

### 00_REVIEW_INDEX.md
Early review and index document:
- Review checklist from early phases
- Document organization notes
- Cross-reference index

**Status**: Obsolete - superseded by `docs/general/DOCUMENT_FINDER.md`

### PROJECT_COMPLETION_SUMMARY.md
Summary from earlier project phase:
- What was completed in earlier phases
- Status at that checkpoint
- Deliverables from that period

**Status**: Historical record - useful for understanding project timeline

### SIMULATOR_RESEARCH_NOTES.md
Research notes on Babbage Analytical Engine simulation:
- Exploratory work on software emulator design
- Alternative approaches considered
- Learning for eventual EMULATOR_SPECIFICATION.md

**Status**: Research phase - formalized in `docs/simulation/EMULATOR_INTERFACE.md` and `docs/simulation/specs/`

### PHASE2_DELIVERY_SUMMARY.md
Delivery summary from Phase 2:
- What was delivered in Phase 2
- Outcomes and achievements
- Status at phase end

**Status**: Phase complete - reference for understanding Phase 2 outcomes

### Output Artifacts
Various markdown files generated during build processes:
- Generated from templates
- Snapshot of state at specific times
- Not intended as primary documentation

**Status**: Build artifacts - useful for tracking documentation generation process

---

## Why We Keep This Archive

### Learning from History
Earlier approaches and decisions provide context for why current solutions were chosen. Understanding what didn't work helps avoid repeating mistakes.

### Accountability
Project decisions can be traced back to their origins. Alternative approaches are documented, showing why they were not selected.

### Continuity
New team members can see the project's evolution. Legacy documents provide onboarding context about how we got here.

### Research
Academic researchers or future projects might find these earlier approaches valuable for comparison.

### Rollback Reference
If a previously-abandoned approach becomes relevant again, documentation exists.

---

## How to Use This Archive

### If You're Implementing Something
1. Check `docs/general/BUILD.md`, `docs/general/ARCHITECTURE.md`, and `docs/general/BUILDOUT_DOC_MAP.md` for current approach
2. Check `docs/archive/` only if you need historical context
3. Do NOT use legacy documents as specification (they're outdated)

### If You're Researching Project Decisions
1. Look at `docs/general/MASTER_ROADMAP.md` and `docs/general/PLANNING_CANONICAL_MAP.md` first
2. Check `docs/archive/` for earlier alternatives and provenance
3. Use `docs/archive/INDEX.md` and `docs/archive/AUDIT_LEDGER.md` for deterministic archive mapping

### If You're New to the Project
1. Start with `docs/general/README.md`
2. Use `docs/general/DOCUMENT_FINDER.md` to locate current documentation
3. Avoid `docs/archive/` until you understand current state

### If You're Nostalgic
1. Review ./MVP_FILE_STRUCTURE.md to see early organizational thinking
2. Read PROJECT_COMPLETION_SUMMARY.md to understand phase outcomes
3. Appreciate how much the project has evolved

---

## Archive Policy

All archived planning docs must include an `Archive Metadata` section with:
- `Archive Status`
- `Archive Reason`
- `Canonical Successor`
- `Novel Content Integrated`
- `Merged Into`
- `Last Validated`

### What Gets Archived
- Superseded specifications (older version exists)
- Completed phase documentation (phase is done)
- Research notes (formalized into specs)
- Obsolete tools/processes (no longer used)
- Deprecated approaches (replaced by better solution)

### What Doesn't Get Archived
- Current specifications (stays in active directory)
- Active phase documentation (stays in active docs such as `docs/general/` and domain directories)
- Learning materials (stays in active docs such as `docs/general/` and `docs/history/`)
- Current development guides (stays in `docs/general/` and domain-specific active directories)

### Archive Criteria
A document is archived when:
1. A newer version exists and is actively maintained
2. OR the project phase for which it was created is complete
3. OR the tool/process it describes is no longer used
4. AND there is no current team member actively using it
5. AND it has historical or research value

### Retention Policy
- Keep indefinitely for historical value
- Mark as obsolete/archived in document header
- Link to current version where applicable
- Include "DO NOT USE" warning if applicable
- Record planning archive rows in `docs/archive/INDEX.md`

---

## Understanding Project Evolution

### Timeline of Major Changes

**Phase 1 (Weeks 1-2) - Foundation**
- Initial MVP structure (./MVP_FILE_STRUCTURE.md)
- Early project planning and organization
- Tool and process decisions

**Phase 2 (Weeks 3-4) - Reorganization** ← Current Phase
- Transition from flat to 9-directory structure
- Migration of 76 markdown files
- Obsoleting of MVP structure
- Consolidation and cleanup

**Future Phases (Weeks 5+)**
- Language services implementation
- Content development
- Platform expansion

### What Changed and Why

**From**: Flat structure (all files in root)
**To**: 9-directory structure (organized by content type)
**Why**: Better navigation, clearer audience targeting, scalability for growth

**From**: MVP file organization
**To**: Diátaxis framework-aligned structure
**Why**: Better pedagogical organization, clearer document purposes, industry best practices

---

## Legacy Tools and Processes

### Build System Evolution
- **Phase 1**: Simple Makefile
- **Phase 2+**: Bazel + Make (hybrid)
- Makefile still supported but Bazel is primary

### Documentation Organization
- **Phase 1**: All markdown in root directory
- **Phase 2+**: 9-directory organization (Diátaxis-aligned)
- Old flat structure deprecated

### CI/CD Pipeline
- **Phase 1**: Basic GitHub Actions
- **Phase 2+**: Enhanced with security checks
- Older workflow files in ARCHIVED_AND_LEGACY/

---

## Lessons Learned (For Posterity)

### Lesson 1: Organization Matters
Starting with a flat structure made sense initially. As the project grew (76 files), navigation became difficult. The 9-directory reorganization was necessary before Phase 2.

**Recommendation**: Plan directory structure early, based on expected growth.

### Lesson 2: Document Consolidation is Important
Having duplicate/overlapping content (EDUCATIONAL_CURRICULUM_MATERIALS in 2 parts) created confusion. Consolidation is part of Week 3 plan.

**Recommendation**: Review for overlaps quarterly, consolidate before reaching critical mass.

### Lesson 3: Cross-References Need Maintenance
200+ internal links needed updating after reorganization. Had structure been more modular initially, updates would be easier.

**Recommendation**: Keep links relative and test regularly.

### Lesson 4: Legacy is Valuable
Even "obsolete" documents provide valuable context. Archive, don't delete.

**Recommendation**: Establish archival process early.

---

## Related Documentation

For current equivalents of archived documents, see:
- [../general/PROJECT_STRUCTURE.md](../general/PROJECT_STRUCTURE.md) - Current structure (replaces `./MVP_FILE_STRUCTURE.md`)
- [../general/DOCUMENT_FINDER.md](../general/DOCUMENT_FINDER.md) - Current index (replaces `00_REVIEW_INDEX.md`)
- [../general/BUILDOUT_DOC_MAP.md](../general/BUILDOUT_DOC_MAP.md) - Current build-out source map
- [../simulation/EMULATOR_INTERFACE.md](../simulation/EMULATOR_INTERFACE.md) - Current emulator interface guidance

---

## FAQ

**Q: Why keep obsolete files?**
A: Historical context, learning, research, and accountability. They show how decisions evolved.

**Q: Can I use a legacy document?**
A: No. Legacy documents are obsolete. Use current documentation instead (see above for references).

**Q: What if I find a bug in legacy documentation?**
A: No action needed. Legacy docs are not maintained. Report bugs in current documentation instead.

**Q: Why isn't this just deleted?**
A: Because understanding why we changed provides valuable lessons for future decisions.

**Q: Can I restore a legacy approach?**
A: Only with careful consideration and tech lead approval. Legacy approaches were superseded for good reasons.

---

**Last Updated**: February 24, 2026
**Status**: Archive - Not Actively Maintained
**DO NOT USE**: For current development or implementation
**REFERENCE ONLY**: For historical understanding and research

---

**Archive Metadata**
- Created: October 31, 2025
- Last Review: February 24, 2026
- Next Review: March 31, 2026
- Archival Reason: Legacy documentation from Phase 1-2
- Current Equivalent: docs/general/ + domain-specific active docs
