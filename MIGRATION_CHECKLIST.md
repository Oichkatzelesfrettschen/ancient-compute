# Repository Reorganization - Migration Checklist

**Project**: Ancient Compute Repository Reorganization  
**Date Started**: 2025-11-19  
**Target Completion**: 2025-11-26  
**Overall Status**: Not Started

---

## Phase 1: Preparation (Day 1-2)

### Step 1.1: Team Review & Approval
- [ ] Review `REORGANIZATION_PLAN_2025.md` (1,048 lines, comprehensive)
- [ ] Review `AUDIT_EXECUTIVE_SUMMARY.md` (quick reference)
- [ ] Review `STRUCTURE_VISUALIZATION.md` (before/after comparison)
- [ ] Team discussion: Any concerns or modifications?
- [ ] Leadership approval to proceed
- [ ] **Status**: Ready to start / Not approved / Changes requested

### Step 1.2: Backup & Safety
- [ ] Create local backup of entire repository:
  ```bash
  cp -r /home/user/ancient-compute /tmp/ancient-compute.backup.2025-11-19
  ```
- [ ] Verify backup size:
  ```bash
  du -sh /tmp/ancient-compute.backup.2025-11-19
  # Should be ~13M
  ```
- [ ] Push current state to GitHub (if using remote):
  ```bash
  cd /home/user/ancient-compute
  git status
  git add .
  git commit -m "Pre-reorganization checkpoint"
  git push origin claude/consolidate-docs-01Ao7GAVBnPUSJu9d7Mduzf6
  ```
- [ ] **Status**: Backup created and verified

### Step 1.3: Document Baseline
- [ ] Record current file statistics:
  ```bash
  find . -type f | wc -l > /tmp/file_count_before.txt
  du -sh . > /tmp/repo_size_before.txt
  find . -name "*.md" | wc -l > /tmp/md_count_before.txt
  ```
- [ ] Create checksums for critical files:
  ```bash
  find docs -type f -name "*.tex" | md5sum > /tmp/critical_checksums_before.txt
  ```
- [ ] Document database/backup paths:
  ```bash
  ls -lh .backup* > /tmp/backup_inventory.txt
  ```
- [ ] **Status**: Baseline documented

### Step 1.4: Environment Setup
- [ ] Verify Git access:
  ```bash
  git status
  ```
- [ ] Check available disk space:
  ```bash
  df -h /home/user
  # Verify at least 5GB free
  ```
- [ ] Install/verify tools:
  ```bash
  which find xargs sed rsync
  ```
- [ ] **Status**: Environment ready

---

## Phase 2: Directory Structure Creation (Day 2)

### Step 2.1: Create New Directory Tree
- [ ] Create documentation structure:
  ```bash
  mkdir -p docs/{architecture,babbage,curriculum,development,getting-started,historical,infrastructure,organization,whitepaper,requirements}
  ```
- [ ] Create archive structure:
  ```bash
  mkdir -p archive/{implementation-phases,historical-records,backups,planning,audits,legacy}
  ```
- [ ] Create source code structure (if moving src/):
  ```bash
  mkdir -p src
  ```
- [ ] Create scripts structure:
  ```bash
  mkdir -p scripts/{minix,utilities,multi-repo}
  ```
- [ ] Create build structure:
  ```bash
  mkdir -p build/{dist,bazel-out}
  ```
- [ ] Verify structure:
  ```bash
  tree -L 2 -d docs archive scripts
  ```
- [ ] **Status**: Directory structure created

---

## Phase 3: Move Documentation (Day 3-4)

### Step 3.1: Consolidate Major Documentation Directories

**ARCHITECTURE_AND_DESIGN → docs/architecture/**
- [ ] Copy directory:
  ```bash
  cp -r ARCHITECTURE_AND_DESIGN/* docs/architecture/
  ls docs/architecture/ | wc -l
  # Should have 15+ files
  ```
- [ ] Verify no errors:
  ```bash
  find docs/architecture -type f | md5sum | tee /tmp/arch_after.md5
  ```
- [ ] **Status**: architecture/ populated

**BABBAGE_ANALYTICAL_ENGINE → docs/babbage/**
- [ ] Copy directory:
  ```bash
  cp -r BABBAGE_ANALYTICAL_ENGINE/* docs/babbage/
  ls -la docs/babbage/
  ```
- [ ] Verify critical files:
  ```bash
  ls docs/babbage/documentation/academic/arxiv/
  # Should have appendices, sections, data, diagrams
  ```
- [ ] **Status**: babbage/ populated

**CURRICULUM_AND_CONTENT → docs/curriculum/**
- [ ] Copy:
  ```bash
  cp -r CURRICULUM_AND_CONTENT/* docs/curriculum/
  ls docs/curriculum/ | head
  ```
- [ ] **Status**: curriculum/ populated

**DEVELOPMENT_GUIDES → docs/development/**
- [ ] Copy:
  ```bash
  cp -r DEVELOPMENT_GUIDES/* docs/development/
  ```
- [ ] **Status**: development/ populated

**GETTING_STARTED → docs/getting-started/**
- [ ] Copy:
  ```bash
  cp -r GETTING_STARTED/* docs/getting-started/
  ```
- [ ] **Status**: getting-started/ populated

**HISTORICAL_CONTEXT → docs/historical/**
- [ ] Copy:
  ```bash
  cp -r HISTORICAL_CONTEXT/* docs/historical/
  ```
- [ ] **Status**: historical/ populated

**INFRASTRUCTURE_AND_DEPLOYMENT → docs/infrastructure/**
- [ ] Copy:
  ```bash
  cp -r INFRASTRUCTURE_AND_DEPLOYMENT/* docs/infrastructure/
  ```
- [ ] **Status**: infrastructure/ populated

**DOCUMENTATION_AND_ORGANIZATION → docs/organization/**
- [ ] Copy:
  ```bash
  cp -r DOCUMENTATION_AND_ORGANIZATION/* docs/organization/
  ```
- [ ] **Status**: organization/ populated

### Step 3.2: Move Whitepaper to docs/
- [ ] Copy whitepaper directory:
  ```bash
  cp -r whitepaper/* docs/whitepaper/
  ls docs/whitepaper/
  # Should have .tex files and PROJECT_COMPLETION_REPORT.txt
  ```
- [ ] Move root-level whitepaper docs:
  ```bash
  cp docs/PEDAGOGICAL_WHITEPAPER.tex docs/whitepaper/
  cp docs/PEDAGOGICAL_GRAPHS_AND_DATA.tex docs/whitepaper/
  ```
- [ ] **Status**: whitepaper/ populated

### Step 3.3: Move Root-Level Markdown Files
- [ ] Move to docs/:
  ```bash
  mv AGENTS.md docs/agents.md
  mv PROJECT_STRUCTURE.md docs/PROJECT_STRUCTURE.md
  mv QUICK_REFERENCE.md docs/QUICK_REFERENCE.md
  mv COMPATIBILITY_MATRIX.md docs/COMPATIBILITY_MATRIX.md
  mv GEMINI.md docs/gemini.md
  mv MULTI_REPO_SUMMARY.md docs/MULTI_REPO_SUMMARY.md
  mv REPOSITORY_SPLIT_GUIDE.md docs/REPOSITORY_SPLIT_GUIDE.md
  ls docs/ | grep -E "^[A-Z]"
  ```
- [ ] **Status**: Root-level markdown moved

---

## Phase 4: Archive Phase & Historical Documents (Day 4-5)

### Step 4.1: Archive Implementation Phases
- [ ] Move Phase documentation:
  ```bash
  mv IMPLEMENTATION_PHASES/PHASE_2 archive/implementation-phases/
  mv IMPLEMENTATION_PHASES/PHASE_3 archive/implementation-phases/
  mv IMPLEMENTATION_PHASES/PHASE_4 archive/implementation-phases/
  mv IMPLEMENTATION_PHASES/*.md archive/implementation-phases/
  ls archive/implementation-phases/ | head
  ```
- [ ] Move misplaced phase directories:
  ```bash
  mv phase3/PHASE3_WHITEPAPER_MAIN.tex archive/implementation-phases/PHASE_3/
  mv phase4/PHASE4_WHITEPAPER_MAIN.tex archive/implementation-phases/PHASE_4/
  ```
- [ ] Remove empty directories:
  ```bash
  rmdir IMPLEMENTATION_PHASES phase3 phase4 whitepaper
  ls -d IMPLEMENTATION_PHASES 2>/dev/null && echo "ERROR: Not removed" || echo "OK"
  ```
- [ ] **Status**: Phases archived

### Step 4.2: Archive Historical Records
- [ ] Move historical records:
  ```bash
  mv HISTORICAL_RECORDS/* archive/historical-records/
  rmdir HISTORICAL_RECORDS
  ls archive/historical-records/ | head
  ```
- [ ] **Status**: historical-records/ populated

### Step 4.3: Archive Backups
- [ ] Create backups directory with retention policy:
  ```bash
  mkdir -p archive/backups/2025-10-31
  mv .backup_20251031_192302 archive/backups/2025-10-31/
  mv .backup_links_20251031_204331 archive/backups/2025-10-31/
  du -sh archive/backups/
  # Should show ~2.9M
  ```
- [ ] Create README for backups:
  ```bash
  cat > archive/backups/README.md << 'BACKUP_README'
# Backups Archive

This directory contains backups from previous reorganization efforts.

## Retention Policy
- Backups are kept for 90 days for reference
- Can be safely deleted after 2025-02-19
- All changes are tracked in git history for recovery
  
## Contents
- `2025-10-31/`: Backup from October 31, 2025 reorganization attempt
BACKUP_README
  ```
- [ ] **Status**: Backups archived

### Step 4.4: Archive Planning Documents
- [ ] Create archive/planning/ with obsolete documents:
  ```bash
  mkdir -p archive/planning
  mv EXECUTION_PLAN_GRANULAR.md archive/planning/
  mv REORGANIZATION_GUIDE.md archive/planning/
  mv REORGANIZATION_SUMMARY.md archive/planning/
  mv MIGRATION_PLAN.md archive/planning/
  mv MIGRATION_MANIFEST.txt archive/planning/
  mv BABBAGE_FILES_MOVED_README.md archive/planning/
  ls archive/planning/
  ```
- [ ] **Status**: planning/ populated

### Step 4.5: Archive Audit Reports
- [ ] Create archive/audits/ with this audit:
  ```bash
  mkdir -p archive/audits/2025-11-19
  mv REORGANIZATION_PLAN_2025.md archive/audits/2025-11-19/
  mv AUDIT_EXECUTIVE_SUMMARY.md archive/audits/2025-11-19/
  mv STRUCTURE_VISUALIZATION.md archive/audits/2025-11-19/
  mv MIGRATION_CHECKLIST.md archive/audits/2025-11-19/
  ls archive/audits/2025-11-19/
  ```
- [ ] **Status**: audits/ populated

---

## Phase 5: Organize Scripts & Utilities (Day 5)

### Step 5.1: Move Utility Scripts
- [ ] Create scripts/utilities/:
  ```bash
  mkdir -p scripts/utilities
  mv VALIDATE_LINKS.py scripts/utilities/
  mv COMPREHENSIVE_FIX_MALFORMED.sed scripts/utilities/fix_paths.sed
  mv FIX_MALFORMED_PATHS.sed scripts/utilities/
  mv PATH_MAPPING.sed scripts/utilities/
  ls scripts/utilities/
  ```
- [ ] **Status**: utilities/ populated

### Step 5.2: Organize MINIX Scripts
- [ ] Move existing scripts:
  ```bash
  mv scripts/minix_*.{sh,py} scripts/minix/ 2>/dev/null
  ls scripts/minix/
  # Should have: minix_metrics.sh, minix_install_interactive.sh, minix_*.py
  ```
- [ ] **Status**: minix/ populated

### Step 5.3: Move tools/ Content
- [ ] Move to scripts/utilities/:
  ```bash
  mv tools/todo_report.py scripts/utilities/
  rmdir tools
  ls scripts/utilities/ | grep python
  ```
- [ ] **Status**: tools/ removed

---

## Phase 6: Handle Duplicates (Day 5)

### Step 6.1: Delete Duplicate Whitepaper Files
- [ ] Verify files are identical:
  ```bash
  diff -q docs/whitepaper-arxiv/appendices/instruction-details.tex \
       docs/babbage/documentation/academic/arxiv/appendices/instruction-details.tex
  # Should show "identical" or nothing
  ```
- [ ] Delete duplicates:
  ```bash
  rm -rf docs/whitepaper-arxiv
  # Verify deleted:
  ls -d docs/whitepaper-arxiv 2>/dev/null && echo "ERROR: Still exists" || echo "OK"
  ```
- [ ] Create symlink for backward compatibility:
  ```bash
  ln -s babbage/documentation/academic/arxiv docs/whitepaper-arxiv
  ls -la docs/whitepaper-arxiv
  # Should show: docs/whitepaper-arxiv -> babbage/documentation/academic/arxiv
  ```
- [ ] **Status**: Duplicates eliminated, symlink created

---

## Phase 7: Clean Up Root Directory (Day 6)

### Step 7.1: Delete Obsolete Files
- [ ] Delete backup files:
  ```bash
  rm CLAUDE.md.backup.2025-11-19
  rm AGENTS.md.original
  rm LINKS_FOUND.txt
  ```
- [ ] Verify deletions:
  ```bash
  ls CLAUDE.md.backup* 2>/dev/null && echo "ERROR" || echo "Deleted"
  ```
- [ ] **Status**: Obsolete files removed

### Step 7.2: Remove Empty Directories
- [ ] Remove migrated directories:
  ```bash
  rmdir ARCHITECTURE_AND_DESIGN BABBAGE_ANALYTICAL_ENGINE \
        CURRICULUM_AND_CONTENT DEVELOPMENT_GUIDES \
        DOCUMENTATION_AND_ORGANIZATION GETTING_STARTED \
        HISTORICAL_CONTEXT INFRASTRUCTURE_AND_DEPLOYMENT \
        ARCHIVED_AND_LEGACY
  ```
- [ ] Verify removal:
  ```bash
  ls -d ARCHITECTURE_AND_DESIGN 2>/dev/null && echo "ERROR: Still exists" || echo "OK"
  ```
- [ ] **Status**: Empty directories removed

### Step 7.3: Verify Root Directory Cleanup
- [ ] Count remaining files:
  ```bash
  ls -1 | grep -v "^src$\|^docs$\|^archive$\|^scripts$\|^config$\|^build$\|^tests$\|^logs$\|^\." | wc -l
  # Should be around 12 (README.md, CLAUDE.md, CONTRIBUTING.md, etc.)
  ```
- [ ] List remaining files:
  ```bash
  ls -la | grep -v "^d" | grep -v "^total" | tail -20
  # Should only show essential files
  ```
- [ ] **Status**: Root directory clean

---

## Phase 8: Create Requirements Index (Day 6)

### Step 8.1: Create Symlinks
- [ ] Create requirements directory:
  ```bash
  mkdir -p docs/requirements
  ```
- [ ] Create symlinks to component requirements:
  ```bash
  ln -s ../../src/backend/requirements.md docs/requirements/backend.md
  ln -s ../../src/frontend/requirements.md docs/requirements/frontend.md
  ln -s ../../src/services/requirements.md docs/requirements/services.md
  ln -s ../requirements.md docs/requirements/docs.md
  ```
- [ ] Create index:
  ```bash
  cat > docs/requirements/README.md << 'REQ_README'
# Project Requirements

This directory contains pointers to all requirements documentation.

- [Backend Requirements](./backend.md) - FastAPI, Python, dependencies
- [Frontend Requirements](./frontend.md) - SvelteKit, TypeScript, npm
- [Services Requirements](./services.md) - Docker, microservices
- [Documentation Requirements](./docs.md) - LaTeX, build tools

See [Overall Requirements](../../requirements.md) for complete documentation.
REQ_README
  ```
- [ ] **Status**: requirements/ structure created

---

## Phase 9: Git Commit (Day 6)

### Step 9.1: Prepare Commit
- [ ] Check status:
  ```bash
  cd /home/user/ancient-compute
  git status
  # Should show many files added/deleted/renamed
  ```
- [ ] Add all changes:
  ```bash
  git add -A
  git status
  # Verify all changes are staged
  ```

### Step 9.2: Create Comprehensive Commit
- [ ] Commit with detailed message:
  ```bash
  git commit -m "Major repository reorganization: consolidate documentation

- Consolidate 23 documentation directories into docs/ (8 subdirectories)
- Move ARCHITECTURE_AND_DESIGN → docs/architecture/
- Move BABBAGE_ANALYTICAL_ENGINE → docs/babbage/
- Move CURRICULUM_AND_CONTENT → docs/curriculum/
- Move DEVELOPMENT_GUIDES → docs/development/
- Move GETTING_STARTED → docs/getting-started/
- Move HISTORICAL_CONTEXT → docs/historical/
- Move INFRASTRUCTURE_AND_DEPLOYMENT → docs/infrastructure/
- Move DOCUMENTATION_AND_ORGANIZATION → docs/organization/

- Archive Phase 1-4 documentation to archive/implementation-phases/
- Archive historical records to archive/historical-records/
- Archive backups to archive/backups/ with retention policy

- Move utility scripts from root to scripts/utilities/
- Move MINIX scripts to scripts/minix/
- Move tools/ content to scripts/utilities/

- Create central requirements/ index with symlinks
- Delete duplicate whitepaper files (create backward-compat symlink)
- Delete obsolete backup and generated files

- Reduce root-level files: 42 → 12 (71% reduction)
- Reduce documentation directories: 23 → 8 (65% reduction)
- Free disk space: 13M → 10.5M (2.5M savings)

Metrics:
- Files affected: 250+
- Directories consolidated: 14
- Duplicates removed: 200K
- Backups archived: 2.9M

Related documentation:
- archive/audits/2025-11-19/REORGANIZATION_PLAN_2025.md (complete 1,048-line plan)
- archive/audits/2025-11-19/AUDIT_EXECUTIVE_SUMMARY.md (quick reference)
- archive/audits/2025-11-19/STRUCTURE_VISUALIZATION.md (before/after comparison)"
  ```
- [ ] **Status**: Committed

### Step 9.3: Push to Remote (if applicable)
- [ ] Push changes:
  ```bash
  git push origin claude/consolidate-docs-01Ao7GAVBnPUSJu9d7Mduzf6
  # Or main branch if appropriate
  ```
- [ ] **Status**: Pushed

---

## Phase 10: Validation & Testing (Day 7)

### Step 10.1: Verify File Integrity
- [ ] Compare file counts:
  ```bash
  find . -type f | wc -l > /tmp/file_count_after.txt
  diff /tmp/file_count_before.txt /tmp/file_count_after.txt
  # Should show same count (moved, not new)
  ```
- [ ] Verify critical checksums:
  ```bash
  find docs -type f -name "*.tex" | md5sum > /tmp/critical_checksums_after.txt
  diff /tmp/critical_checksums_before.txt /tmp/critical_checksums_after.txt
  # Should be identical
  ```
- [ ] **Status**: Files integrity verified

### Step 10.2: Check Symlinks
- [ ] Test documentation symlinks:
  ```bash
  ls -la docs/whitepaper-arxiv
  # Should show: -> babbage/documentation/academic/arxiv
  cat docs/whitepaper-arxiv/appendices/instruction-details.tex | head -5
  # Should work without errors
  ```
- [ ] Test requirements symlinks:
  ```bash
  ls -la docs/requirements/
  cat docs/requirements/backend.md | head -10
  # Should show backend requirements content
  ```
- [ ] **Status**: Symlinks verified

### Step 10.3: Verify Repository Size
- [ ] Check new size:
  ```bash
  du -sh . > /tmp/repo_size_after.txt
  cat /tmp/repo_size_before.txt /tmp/repo_size_after.txt
  # Should show reduction from 13M to ~10.5M
  ```
- [ ] **Status**: Space savings verified

### Step 10.4: Documentation Build Test
- [ ] (If using LaTeX build):
  ```bash
  cd docs/whitepaper
  # Try building PDF or test symlink access
  ```
- [ ] (If using other doc tools):
  ```bash
  # Run documentation build system
  ```
- [ ] **Status**: Documentation builds (if applicable)

### Step 10.5: Navigation Test
- [ ] Verify easy navigation:
  ```bash
  # From root, can easily reach:
  ls docs/architecture/
  ls docs/babbage/
  ls archive/implementation-phases/PHASE_3/
  ls scripts/utilities/
  ```
- [ ] **Status**: Navigation verified

---

## Phase 11: Team Communication (Day 7)

### Step 11.1: Update Documentation
- [ ] Update root README.md:
  ```bash
  # Update any hardcoded paths from old structure
  # Example: ARCHITECTURE_AND_DESIGN/ → docs/architecture/
  ```
- [ ] Update CONTRIBUTING.md (if needed):
  ```bash
  # Update any file location references
  ```
- [ ] **Status**: Documentation updated

### Step 11.2: Notify Team
- [ ] Create summary message:
  ```
  Repository reorganization completed successfully!
  
  Summary:
  - Root files reduced: 42 → 12
  - Documentation consolidated: 23 dirs → 8
  - Disk space freed: 2.5M
  - Migration: 250+ files reorganized
  
  New structure:
  - src/ - All source code
  - docs/ - All documentation (8 subdirectories)
  - archive/ - Historical & legacy (organized by phase/date)
  - scripts/ - Build and utility scripts
  
  For details, see: archive/audits/2025-11-19/
  ```
- [ ] Send notification to team
- [ ] **Status**: Team notified

### Step 11.3: Gather Feedback
- [ ] Ask team for issues
- [ ] Monitor for CI/CD failures (if any)
- [ ] Address any broken links
- [ ] **Status**: Feedback collected

---

## Phase 12: Final Verification (Day 8)

### Step 12.1: Run Full Test Suite (if applicable)
- [ ] Backend tests:
  ```bash
  cd src/backend && pytest tests/ -v
  # Should pass or match pre-migration results
  ```
- [ ] Frontend tests:
  ```bash
  cd src/frontend && npm test
  # Should pass or match pre-migration results
  ```
- [ ] **Status**: Tests passed

### Step 12.2: Verify All Links
- [ ] Check internal documentation links:
  ```bash
  # Run link checker script if available
  python scripts/utilities/validate_links.py
  ```
- [ ] **Status**: Links verified

### Step 12.3: Create Success Report
- [ ] Document completion:
  ```
  MIGRATION COMPLETED SUCCESSFULLY
  
  Date Completed: 2025-11-26
  Total Time: ~8 days
  
  Achievements:
  ✓ Root directory cleaned: 42 → 12 files
  ✓ Documentation consolidated: 23 → 8 directories
  ✓ Backups archived and organized: 2.9M
  ✓ Duplicate files removed: 200K
  ✓ Disk space freed: 2.5M
  ✓ All files integrity verified
  ✓ All symlinks tested
  ✓ Full test suite passed
  
  New Structure Benefits:
  - Easier navigation
  - Better organization
  - Reduced cognitive load
  - Proper archival of historical docs
  - Cleaner root directory
  ```
- [ ] **Status**: Success report created

---

## Troubleshooting Guide

### If Files Are Missing
1. Check backup:
   ```bash
   ls /tmp/ancient-compute.backup.2025-11-19/
   ```
2. Restore specific files:
   ```bash
   cp /tmp/ancient-compute.backup.2025-11-19/[filename] .
   ```
3. Or restore from git:
   ```bash
   git checkout HEAD^ -- [filename]
   ```

### If Symlinks Fail
1. Check target exists:
   ```bash
   ls -la docs/babbage/documentation/academic/arxiv/
   ```
2. Remove and recreate symlink:
   ```bash
   rm docs/whitepaper-arxiv
   ln -s babbage/documentation/academic/arxiv docs/whitepaper-arxiv
   ```

### If Documentation Build Fails
1. Check paths in build scripts
2. Update any hardcoded directory references
3. Test symlink access:
   ```bash
   cat docs/whitepaper-arxiv/appendices/instruction-details.tex | head
   ```

### If Tests Fail
1. Check if paths changed in code
2. Verify imports use relative paths
3. Run tests individually to isolate issue

---

## Sign-Off

**Prepared By**: Repository Audit Team  
**Date**: 2025-11-19  
**Status**: Ready for Execution  
**Approval**: ___________________ (Team Lead)  
**Executed By**: _________________ (Migration Lead)  
**Completed Date**: ______________ (To be filled in)

---

**Notes**:
- Estimated total time: 4-6 hours active work + 2 hours testing
- Best time to execute: During low-activity period (weekday afternoon)
- Can be executed incrementally by phases
- Git history preserved throughout
- Rollback possible at any point using backup

