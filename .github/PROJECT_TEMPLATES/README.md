# GitHub Project Configuration for Ancient Compute

This directory contains templates and configuration for setting up the Ancient Compute GitHub Project.

## Overview

The Ancient Compute GitHub Project uses a multi-repository structure with coordinated issue tracking, milestones, and project boards.

## Quick Setup Guide

### 1. Create GitHub Project

1. Go to: https://github.com/orgs/Oichkatzelesfrettschen/projects (or your organization)
2. Click "New Project"
3. Choose "Board" template
4. Name: "Ancient Compute"
5. Description: "12,500 years of computational history - Multi-repo educational platform"

### 2. Configure Project Views

#### View 1: Overview Board (Default)

**Layout**: Board  
**Group by**: Status  
**Columns**:
- Backlog (items not yet scheduled)
- Todo (planned for current sprint)
- In Progress (actively being worked on)
- In Review (awaiting code review)
- Done (completed)

#### View 2: By Repository

**Layout**: Board  
**Group by**: Repository  
**Columns**:
- ancient-compute-core
- ancient-compute-frontend
- ancient-compute-backend
- ancient-compute-babbage-engine
- ancient-compute-language-services
- ancient-compute-curriculum
- ancient-compute-docs

#### View 3: By Component

**Layout**: Board  
**Group by**: Label (component)  
**Columns**:
- Frontend
- Backend
- Services
- Babbage Engine
- Curriculum
- Documentation
- Infrastructure

#### View 4: Roadmap

**Layout**: Roadmap  
**Group by**: Milestone  
**Date Field**: Due date  
**Milestones**:
- Phase 1: Foundation (Complete)
- Phase 2: Language Services (In Progress)
- Phase 3: Emulator & Tools
- Phase 4: Frontend & Visualization
- Phase 5: Content & Curriculum
- Phase 6: Documentation & Publishing
- Phase 7: Production Deployment

#### View 5: Priority Matrix

**Layout**: Table  
**Sort by**: Priority (descending), then Created date  
**Visible Fields**:
- Title
- Repository
- Status
- Priority
- Assignee
- Labels
- Milestone

### 3. Configure Automation

**Auto-add items**:
- ✓ Add newly created issues from all linked repositories
- ✓ Add newly created pull requests from all linked repositories

**Auto-archive**:
- ✓ Archive items when they are closed
- ✓ Archive items when merged (PRs)

**Status automation**:
- Move to "In Progress" when issue is assigned
- Move to "In Review" when PR is opened
- Move to "Done" when issue is closed or PR is merged

### 4. Link Repositories

Go to Project Settings → Linked repositories:
1. ancient-compute-core
2. ancient-compute-frontend
3. ancient-compute-backend
4. ancient-compute-babbage-engine
5. ancient-compute-language-services
6. ancient-compute-curriculum
7. ancient-compute-docs

### 5. Configure Labels

Apply these labels across ALL repositories for consistency.

#### Priority Labels

```yaml
- name: "priority: critical"
  color: "d73a4a"  # red
  description: "P0 - Critical priority, blocks release"

- name: "priority: high"
  color: "ff9800"  # orange
  description: "P1 - High priority, important for milestone"

- name: "priority: medium"
  color: "fbca04"  # yellow
  description: "P2 - Medium priority, normal work"

- name: "priority: low"
  color: "0e8a16"  # green
  description: "P3 - Low priority, nice to have"
```

#### Type Labels

```yaml
- name: "type: bug"
  color: "d73a4a"
  description: "Something isn't working"

- name: "type: feature"
  color: "0075ca"
  description: "New feature or request"

- name: "type: enhancement"
  color: "a2eeef"
  description: "Improvement to existing feature"

- name: "type: documentation"
  color: "0075ca"
  description: "Documentation only changes"

- name: "type: security"
  color: "d73a4a"
  description: "Security vulnerability or hardening"

- name: "type: performance"
  color: "ff9800"
  description: "Performance improvement"

- name: "type: refactor"
  color: "fbca04"
  description: "Code refactoring"

- name: "type: test"
  color: "0e8a16"
  description: "Testing related"
```

#### Component Labels

```yaml
- name: "component: frontend"
  color: "c5def5"
  description: "SvelteKit frontend"

- name: "component: backend"
  color: "c5def5"
  description: "FastAPI backend"

- name: "component: services"
  color: "c5def5"
  description: "Language execution services"

- name: "component: babbage"
  color: "c5def5"
  description: "Babbage Analytical Engine"

- name: "component: curriculum"
  color: "c5def5"
  description: "Educational content"

- name: "component: docs"
  color: "c5def5"
  description: "Documentation (LaTeX, etc.)"

- name: "component: infrastructure"
  color: "c5def5"
  description: "Build, deployment, CI/CD"
```

#### Phase Labels

```yaml
- name: "phase: 1-foundation"
  color: "bfd4f2"
  description: "Phase 1: Core infrastructure"

- name: "phase: 2-languages"
  color: "bfd4f2"
  description: "Phase 2: Language services"

- name: "phase: 3-emulator"
  color: "bfd4f2"
  description: "Phase 3: Babbage emulator"

- name: "phase: 4-frontend"
  color: "bfd4f2"
  description: "Phase 4: UI and visualization"
```

### 6. Create Milestones

Create these milestones in each repository (with consistent names):

#### Phase 1: Foundation (Complete)
- **Due date**: 2025-08-31 (completed)
- **Description**: Core infrastructure, build system, initial services
- **Linked Issues**: All Phase 1 foundation work

#### Phase 2: Language Services
- **Due date**: 2025-11-30
- **Description**: Complete all 8 language execution services
- **Progress**: 85% (3 of 7 services remaining)

#### Phase 3: Emulator & Tools
- **Due date**: 2026-01-31
- **Description**: Babbage ISA emulator, assembler, debugger

#### Phase 4: Frontend & Visualization
- **Due date**: 2026-03-31
- **Description**: Interactive timeline, 3D visualization, code playgrounds

### 7. Issue Templates

Each repository should have these issue templates in `.github/ISSUE_TEMPLATE/`:

See the files in this directory:
- `bug_report.yml`
- `feature_request.yml`
- `documentation.yml`

### 8. Pull Request Template

Each repository should have `.github/PULL_REQUEST_TEMPLATE.md`

See `PULL_REQUEST_TEMPLATE.md` in this directory.

## CLI Setup (Optional)

Use GitHub CLI to automate project setup:

```bash
# Install GitHub CLI
brew install gh  # macOS
# or
sudo apt install gh  # Ubuntu

# Login
gh auth login

# Create project
gh project create --owner Oichkatzelesfrettschen --title "Ancient Compute"

# Link repositories
gh project link-repo ancient-compute-core --project "Ancient Compute"
gh project link-repo ancient-compute-frontend --project "Ancient Compute"
# ... repeat for all repos

# Create labels (run in each repo)
cd ancient-compute-core
gh label create "priority: critical" --color d73a4a --description "P0 - Critical"
gh label create "priority: high" --color ff9800 --description "P1 - High"
# ... repeat for all labels
```

## Maintenance

### Weekly Tasks

1. Review project board, move stale items
2. Update milestone progress
3. Archive completed items
4. Triage new issues (add labels, assign, prioritize)

### Monthly Tasks

1. Review and update milestones
2. Generate progress reports
3. Update roadmap
4. Close stale issues

### Release Tasks

1. Create release milestone
2. Tag all related issues/PRs
3. Generate release notes
4. Update compatibility matrix

## Best Practices

1. **Consistent Labeling**: Use the same labels across all repos
2. **Link Issues to PRs**: Always reference related issues in PRs
3. **Update Status**: Move cards as work progresses
4. **Clear Descriptions**: Write detailed issue descriptions
5. **Regular Grooming**: Keep the board clean and organized

## References

- **GitHub Projects Docs**: https://docs.github.com/en/issues/planning-and-tracking-with-projects
- **Project Automation**: https://docs.github.com/en/issues/planning-and-tracking-with-projects/automating-your-project
- **Labels Best Practices**: https://docs.github.com/en/issues/using-labels-and-milestones-to-track-work/managing-labels

---

**Last Updated**: November 2, 2025  
**Version**: 1.0
