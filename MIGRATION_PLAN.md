# Ancient Compute: Migration to Multi-Repository Project

**Date**: November 2, 2025  
**Version**: 1.0  
**Status**: Planning Document  
**Timeline**: 2-3 days for complete migration

---

## Executive Summary

This document outlines the complete migration plan for converting Ancient Compute from a monorepo structure to a multi-repository GitHub Project organization. The migration follows industry best practices for repository splitting while preserving git history.

## Goals

1. **Modularization**: Separate major components into focused repositories
2. **Best Practices**: Follow GitHub Project best practices for multi-repo coordination
3. **History Preservation**: Maintain complete git history for each component
4. **Developer Experience**: Improve development workflows with clear separation of concerns
5. **Continuous Integration**: Maintain unbroken CI/CD throughout migration

## Migration Strategy

### Phase 1: Preparation (Day 1 Morning)

**Duration**: 2-3 hours

#### 1.1 Documentation Creation ✓ COMPLETE

- [x] Create PROJECT_STRUCTURE.md
- [x] Create CONTRIBUTING.md for multi-repo workflows
- [x] Create REPOSITORY_SPLIT_GUIDE.md with extraction steps
- [x] Create GitHub Project templates (.github/PROJECT_TEMPLATES/)
- [x] Create issue templates (.github/ISSUE_TEMPLATE/)
- [x] Create PR template (.github/PULL_REQUEST_TEMPLATE.md)
- [x] Create CODEOWNERS file
- [x] Update main README.md with multi-repo references

#### 1.2 Backup Current State

```bash
# Create backup branch
git checkout -b monorepo-backup
git push origin monorepo-backup

# Create backup tag
git tag monorepo-snapshot-2025-11-02
git push origin monorepo-snapshot-2025-11-02
```

#### 1.3 Install Required Tools

```bash
# Install git-filter-repo
pip install git-filter-repo

# Install GitHub CLI
brew install gh  # macOS
# OR
sudo apt install gh  # Ubuntu

# Login to GitHub
gh auth login
```

### Phase 2: Repository Extraction (Day 1 Afternoon)

**Duration**: 4-6 hours

Extract components in this order (dependencies first):

#### 2.1 Extract Language Services (No dependencies)

```bash
# See REPOSITORY_SPLIT_GUIDE.md section 4
./scripts/extract-repo.sh services ancient-compute-language-services
```

#### 2.2 Extract Curriculum (No dependencies)

```bash
./scripts/extract-repo.sh curriculum ancient-compute-curriculum
```

#### 2.3 Extract Documentation (No dependencies)

```bash
./scripts/extract-repo.sh docs ancient-compute-docs
```

#### 2.4 Extract Babbage Engine (No dependencies)

```bash
./scripts/extract-repo.sh babbage ancient-compute-babbage-engine
```

#### 2.5 Extract Backend (Depends on: services, babbage, curriculum)

```bash
./scripts/extract-repo.sh backend ancient-compute-backend
```

#### 2.6 Extract Frontend (Depends on: backend)

```bash
./scripts/extract-repo.sh frontend ancient-compute-frontend
```

### Phase 3: GitHub Project Configuration (Day 2 Morning)

**Duration**: 3-4 hours

#### 3.1 Create GitHub Project

```bash
# Create project via GitHub CLI
gh project create \
    --owner Oichkatzelesfrettschen \
    --title "Ancient Compute" \
    --description "12,500 years of computational history - Multi-repo educational platform"
```

Or manually via GitHub web interface:
1. Go to https://github.com/orgs/Oichkatzelesfrettschen/projects
2. Click "New Project"
3. Choose "Board" template
4. Name: "Ancient Compute"

#### 3.2 Configure Project Views

Create these views (see .github/PROJECT_TEMPLATES/README.md):
1. Overview Board (default)
2. By Repository
3. By Component
4. Roadmap (timeline view)
5. Priority Matrix

#### 3.3 Link Repositories to Project

```bash
gh project link-repo ancient-compute --project "Ancient Compute"
gh project link-repo ancient-compute-frontend --project "Ancient Compute"
gh project link-repo ancient-compute-backend --project "Ancient Compute"
gh project link-repo ancient-compute-babbage-engine --project "Ancient Compute"
gh project link-repo ancient-compute-language-services --project "Ancient Compute"
gh project link-repo ancient-compute-curriculum --project "Ancient Compute"
gh project link-repo ancient-compute-docs --project "Ancient Compute"
```

#### 3.4 Sync Labels Across All Repos

```bash
# Run label sync script
./scripts/multi-repo/sync-labels.sh
```

#### 3.5 Create Milestones in Each Repo

Milestones to create:
- Phase 2: Language Services (Due: 2025-11-30)
- Phase 3: Emulator & Tools (Due: 2026-01-31)
- Phase 4: Frontend & Visualization (Due: 2026-03-31)

### Phase 4: Core Repository Update (Day 2 Afternoon)

**Duration**: 2-3 hours

#### 4.1 Add Submodules to Core

```bash
cd /path/to/ancient-compute

# Add all repos as submodules
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-frontend.git frontend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-backend.git backend
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-babbage-engine.git babbage
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-language-services.git services
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-curriculum.git curriculum
git submodule add https://github.com/Oichkatzelesfrettschen/ancient-compute-docs.git docs

# Commit submodule additions
git add .gitmodules frontend backend babbage services curriculum docs
git commit -m "feat: add submodules for multi-repo structure"
```

#### 4.2 Update docker-compose.yml

Update docker-compose.yml to reference submodule paths:

```yaml
services:
  frontend:
    build:
      context: ./frontend
      dockerfile: Dockerfile
  
  backend:
    build:
      context: ./backend
      dockerfile: Dockerfile
  
  # ... etc
```

#### 4.3 Update Build Configuration

Update Bazel WORKSPACE and MODULE.bazel to work with submodules.

#### 4.4 Remove Extracted Directories from Main Repo

```bash
# Create cleanup branch
git checkout -b cleanup-extracted-dirs

# Remove directories that are now submodules
git rm -rf frontend/ backend/ services/ docs/ curriculum/
git rm -rf BABBAGE_ANALYTICAL_ENGINE/

# Move Babbage markdown files to legacy docs
mkdir -p legacy-docs/babbage
git mv BABBAGE_*.md legacy-docs/babbage/

# Commit cleanup
git add .
git commit -m "chore: remove extracted directories (now submodules)"
```

### Phase 5: Integration Testing (Day 3 Morning)

**Duration**: 3-4 hours

#### 5.1 Fresh Clone Test

```bash
# Test fresh clone with submodules
cd /tmp
git clone --recurse-submodules https://github.com/Oichkatzelesfrettschen/ancient-compute.git
cd ancient-compute

# Verify all submodules initialized
git submodule status

# Should show:
#  <hash> frontend (v1.0.0)
#  <hash> backend (v1.0.0)
#  <hash> babbage (v1.0.0)
#  <hash> services (v1.0.0)
#  <hash> curriculum (v1.0.0)
#  <hash> docs (v1.0.0)
```

#### 5.2 Build Test

```bash
# Test Docker Compose build
docker-compose build

# Start all services
docker-compose up -d

# Check service health
docker-compose ps
curl http://localhost:8000/health
curl http://localhost:3000
```

#### 5.3 Run Tests

```bash
# Run integration tests
make test-integration

# Run E2E tests
make test-e2e
```

#### 5.4 Verify CI/CD

```bash
# Push changes and verify CI passes
git push origin main

# Monitor GitHub Actions
gh run watch
```

### Phase 6: Documentation and Communication (Day 3 Afternoon)

**Duration**: 2-3 hours

#### 6.1 Update All Documentation

- [ ] README.md in each repository
- [ ] API documentation
- [ ] Architecture diagrams
- [ ] Deployment guides
- [ ] Troubleshooting guides

#### 6.2 Create Migration Announcement

```markdown
# Ancient Compute: Multi-Repository Migration Complete

We've migrated Ancient Compute to a multi-repository structure!

## What Changed

- Project now organized as 7 specialized repositories
- GitHub Project board for cross-repo coordination
- Improved development workflows
- Clear separation of concerns

## For Contributors

- See [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)
- See [CONTRIBUTING.md](./CONTRIBUTING.md)
- Clone with: `git clone --recurse-submodules ...`

## Benefits

- Faster CI/CD per component
- Clearer ownership
- Independent versioning
- Better modularity

## Questions?

Open an issue or discussion in the main repo.
```

#### 6.3 Update External Links

- [ ] Update links in GitHub wiki
- [ ] Update links in external documentation
- [ ] Update links in deployment scripts
- [ ] Notify contributors via GitHub Discussions

### Phase 7: Finalization (Day 3 End)

**Duration**: 1 hour

#### 7.1 Archive Old Structure (Optional)

If creating entirely new repos rather than converting:

```bash
# Archive the monorepo (if creating new repos from scratch)
gh repo archive Oichkatzelesfrettschen/ancient-compute-old

# Update repo description
gh repo edit Oichkatzelesfrettschen/ancient-compute \
    --description "Ancient Compute - Core orchestration for multi-repo project"
```

#### 7.2 Create Initial Releases

Tag v1.0.0 in all new repositories:

```bash
for repo in frontend backend babbage services curriculum docs; do
    cd /path/to/ancient-compute-$repo
    git tag v1.0.0
    git push origin v1.0.0
    gh release create v1.0.0 \
        --title "Initial standalone release" \
        --notes "Extracted from monorepo with full history preserved"
done
```

#### 7.3 Final Verification Checklist

- [ ] All repositories created and accessible
- [ ] GitHub Project configured and linked
- [ ] Submodules working in core repo
- [ ] CI/CD passing in all repos
- [ ] Documentation updated
- [ ] Labels synced
- [ ] Milestones created
- [ ] CODEOWNERS configured
- [ ] Branch protection enabled
- [ ] All tests passing

---

## Rollback Plan

If critical issues arise:

### Immediate Rollback (< 24 hours)

```bash
# Restore from backup tag
cd ancient-compute
git reset --hard monorepo-snapshot-2025-11-02
git push --force origin main

# Delete new repositories
gh repo delete Oichkatzelesfrettschen/ancient-compute-frontend --confirm
gh repo delete Oichkatzelesfrettschen/ancient-compute-backend --confirm
# ... etc
```

### Partial Rollback (> 24 hours)

Keep new repos but restore monorepo temporarily:
```bash
# Restore monorepo from backup branch
git checkout monorepo-backup
git checkout -b main-rollback
git push origin main-rollback

# Update CI to use rollback branch
# Investigate issues in new repos
# Fix and re-migrate
```

---

## Success Criteria

Migration is successful when:

1. ✅ All 7 repositories created and accessible
2. ✅ GitHub Project board operational
3. ✅ Submodules working correctly
4. ✅ All CI/CD pipelines passing
5. ✅ Integration tests passing
6. ✅ Documentation complete and accurate
7. ✅ Contributors can clone and develop
8. ✅ No broken links or references
9. ✅ Production deployment successful
10. ✅ Team onboarded to new structure

---

## Post-Migration Tasks

### Week 1

- [ ] Monitor for issues
- [ ] Gather contributor feedback
- [ ] Fix any broken workflows
- [ ] Update deployment scripts
- [ ] Create video tutorial for new structure

### Week 2

- [ ] Review and improve cross-repo workflows
- [ ] Optimize CI/CD pipelines
- [ ] Create example PRs demonstrating workflows
- [ ] Document lessons learned

### Month 1

- [ ] Evaluate migration success
- [ ] Document benefits realized
- [ ] Identify remaining improvements
- [ ] Plan next phase enhancements

---

## Resources

- **PROJECT_STRUCTURE.md** - Multi-repo architecture
- **REPOSITORY_SPLIT_GUIDE.md** - Step-by-step extraction
- **CONTRIBUTING.md** - Contributor workflows
- **.github/PROJECT_TEMPLATES/** - Project configuration templates

---

## Contact

For migration questions or issues:
- Open issue in ancient-compute repository
- Tag with `migration` label
- Mention @Oichkatzelesfrettschen

---

**Document Version**: 1.0  
**Last Updated**: November 2, 2025  
**Status**: Ready for Execution
