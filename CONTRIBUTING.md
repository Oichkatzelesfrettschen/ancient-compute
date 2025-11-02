# Contributing to Ancient Compute

Thank you for your interest in contributing to Ancient Compute! This guide covers how to contribute to our multi-repository project.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [Project Structure](#project-structure)
3. [Getting Started](#getting-started)
4. [Development Workflow](#development-workflow)
5. [Coding Standards](#coding-standards)
6. [Testing Guidelines](#testing-guidelines)
7. [Pull Request Process](#pull-request-process)
8. [Issue Guidelines](#issue-guidelines)
9. [Documentation](#documentation)
10. [Community](#community)

---

## Code of Conduct

This project adheres to a code of conduct that all contributors are expected to follow:

- **Be Respectful**: Treat everyone with respect, regardless of background or experience level
- **Be Constructive**: Focus on improving the project, not criticizing people
- **Be Collaborative**: Work together, help others, share knowledge
- **Be Professional**: Keep discussions focused on the project
- **Be Inclusive**: Welcome contributors from all backgrounds

Unacceptable behavior will not be tolerated. Please report issues to the project maintainers.

---

## Project Structure

Ancient Compute is organized as a GitHub Project with multiple repositories:

1. **ancient-compute-core** - Project orchestration and documentation
2. **ancient-compute-frontend** - SvelteKit user interface
3. **ancient-compute-backend** - FastAPI REST/WebSocket API
4. **ancient-compute-babbage-engine** - Babbage Analytical Engine emulator
5. **ancient-compute-language-services** - Docker language execution containers
6. **ancient-compute-curriculum** - Educational content and modules
7. **ancient-compute-docs** - LaTeX academic documentation

See [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md) for detailed information.

---

## Getting Started

### Prerequisites

- **Git**: Version control
- **GitHub Account**: For pull requests and issues
- **Development Environment**: See each repo's README for specific requirements

### Initial Setup

1. **Fork the Repository**

```bash
# Fork the repo you want to contribute to on GitHub
# Then clone your fork
git clone https://github.com/YOUR_USERNAME/ancient-compute-REPO_NAME.git
cd ancient-compute-REPO_NAME
```

2. **Add Upstream Remote**

```bash
git remote add upstream https://github.com/Oichkatzelesfrettschen/ancient-compute-REPO_NAME.git
git fetch upstream
```

3. **Install Dependencies**

See the specific repository's README for installation instructions.

### For Core Repository (All Submodules)

```bash
# Clone with all submodules
git clone --recurse-submodules https://github.com/Oichkatzelesfrettschen/ancient-compute-core.git
cd ancient-compute-core

# Or initialize submodules after cloning
git submodule update --init --recursive
```

---

## Development Workflow

### 1. Create a Branch

Always work on a feature branch, never on `main` or `master`:

```bash
# Sync with upstream first
git checkout main
git pull upstream main

# Create a feature branch
git checkout -b feature/your-feature-name

# Or for bug fixes
git checkout -b fix/issue-123-description
```

**Branch Naming Conventions**:
- `feature/description` - New features
- `fix/issue-123-description` - Bug fixes
- `docs/description` - Documentation only
- `refactor/description` - Code refactoring
- `test/description` - Test additions/improvements
- `chore/description` - Build, dependencies, etc.

### 2. Make Changes

Follow these principles:
- **Small, Focused Commits**: One logical change per commit
- **Descriptive Messages**: Explain what and why, not how
- **Test as You Go**: Write tests for new code
- **Lint Regularly**: Fix linting issues before committing

### 3. Commit Changes

```bash
# Stage specific files
git add path/to/changed/file.ts

# Commit with descriptive message
git commit -m "feat: add user authentication endpoint

- Implement JWT token generation
- Add password hashing with bcrypt
- Create login/logout API endpoints
- Add integration tests

Closes #123"
```

**Commit Message Format**:
```
<type>: <short description>

<optional detailed description>

<optional footer: references, breaking changes>
```

**Types**:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `style`: Formatting, no code change
- `refactor`: Code restructuring
- `test`: Adding/updating tests
- `chore`: Build, dependencies, tooling
- `perf`: Performance improvements
- `security`: Security fixes

### 4. Keep Your Branch Updated

```bash
# Regularly sync with upstream
git fetch upstream
git rebase upstream/main

# Resolve conflicts if any
# Then continue
git rebase --continue
```

### 5. Push Changes

```bash
# Push to your fork
git push origin feature/your-feature-name

# If you've rebased and need to force push
git push --force-with-lease origin feature/your-feature-name
```

---

## Coding Standards

### General Principles

1. **Code Quality Over Quantity**: Write clear, maintainable code
2. **DRY (Don't Repeat Yourself)**: Extract common functionality
3. **KISS (Keep It Simple)**: Prefer simple solutions
4. **YAGNI (You Ain't Gonna Need It)**: Don't add unnecessary features
5. **Test Coverage**: Aim for >80% code coverage

### Language-Specific Standards

#### TypeScript/JavaScript (Frontend)

```typescript
// Use TypeScript strict mode
// Prefer const over let, avoid var
// Use async/await over promises.then()
// Use functional programming patterns

// Good
const fetchUserData = async (userId: string): Promise<User> => {
  const response = await fetch(`/api/users/${userId}`);
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`);
  }
  return response.json();
};

// Bad
var getUserData = function(userId) {
  return fetch('/api/users/' + userId).then(function(response) {
    return response.json();
  });
};
```

**Linting**: ESLint with TypeScript rules
**Formatting**: Prettier (2-space indentation)
**Style Guide**: Airbnb TypeScript

#### Python (Backend)

```python
# Use type hints
# Follow PEP 8
# Use async/await for I/O
# Document with docstrings

# Good
async def get_user(user_id: str) -> User:
    """
    Fetch user by ID from database.
    
    Args:
        user_id: Unique user identifier
        
    Returns:
        User object
        
    Raises:
        UserNotFoundError: If user doesn't exist
    """
    user = await db.users.find_one({"_id": user_id})
    if not user:
        raise UserNotFoundError(f"User {user_id} not found")
    return User(**user)

# Bad
def get_user(user_id):
    user = db.users.find_one({"_id": user_id})
    return user
```

**Linting**: Ruff, mypy (type checking)
**Formatting**: Black (88 character line length)
**Style Guide**: PEP 8, Google Python Style Guide

#### Other Languages

- **C**: Follow GNU C style, use clang-format
- **Haskell**: HLint, hindent
- **LaTeX**: latexindent, chktex

### Documentation Standards

```typescript
/**
 * Calculate factorial of a number.
 * 
 * @param n - Non-negative integer
 * @returns Factorial of n
 * @throws {RangeError} If n is negative
 * 
 * @example
 * ```typescript
 * factorial(5)  // returns 120
 * factorial(0)  // returns 1
 * ```
 */
function factorial(n: number): number {
  if (n < 0) throw new RangeError("n must be non-negative");
  return n === 0 ? 1 : n * factorial(n - 1);
}
```

---

## Testing Guidelines

### Test Coverage Requirements

- **Unit Tests**: >80% coverage
- **Integration Tests**: Critical paths covered
- **E2E Tests**: Key user workflows
- **Performance Tests**: For optimization work

### Writing Tests

#### Frontend (Vitest)

```typescript
import { describe, it, expect } from 'vitest';
import { factorial } from './math';

describe('factorial', () => {
  it('should return 1 for 0', () => {
    expect(factorial(0)).toBe(1);
  });

  it('should calculate factorial correctly', () => {
    expect(factorial(5)).toBe(120);
  });

  it('should throw for negative numbers', () => {
    expect(() => factorial(-1)).toThrow(RangeError);
  });
});
```

#### Backend (pytest)

```python
import pytest
from app.services.auth import create_user, authenticate_user

class TestAuth:
    """Test authentication service."""
    
    async def test_create_user(self):
        """Should create user with hashed password."""
        user = await create_user("test@example.com", "password123")
        assert user.email == "test@example.com"
        assert user.password_hash != "password123"
    
    async def test_authenticate_valid_user(self):
        """Should authenticate with valid credentials."""
        user = await create_user("test@example.com", "password123")
        result = await authenticate_user("test@example.com", "password123")
        assert result is not None
        assert result.id == user.id
    
    async def test_authenticate_invalid_password(self):
        """Should reject invalid password."""
        await create_user("test@example.com", "password123")
        result = await authenticate_user("test@example.com", "wrongpassword")
        assert result is None
```

### Running Tests

```bash
# Frontend
cd frontend
pnpm test              # Run all tests
pnpm test:coverage     # With coverage report
pnpm test:watch        # Watch mode

# Backend
cd backend
pytest                 # Run all tests
pytest --cov           # With coverage
pytest -k test_auth    # Run specific tests
```

---

## Pull Request Process

### Before Creating a PR

1. **Run Tests**: All tests must pass
   ```bash
   make test
   ```

2. **Run Linters**: Fix all linting errors
   ```bash
   make lint
   make format
   ```

3. **Update Documentation**: If APIs or behavior changed

4. **Rebase on Latest**: Ensure your branch is up to date
   ```bash
   git fetch upstream
   git rebase upstream/main
   ```

### Creating a Pull Request

1. **Push to Your Fork**
   ```bash
   git push origin feature/your-feature-name
   ```

2. **Open PR on GitHub**
   - Go to the original repository
   - Click "New Pull Request"
   - Select your branch
   - Fill out the PR template

### PR Template

```markdown
## Description
[Describe what this PR does]

## Related Issues
Closes #123
Related to #456

## Type of Change
- [ ] Bug fix (non-breaking change fixing an issue)
- [ ] New feature (non-breaking change adding functionality)
- [ ] Breaking change (fix or feature causing existing functionality to change)
- [ ] Documentation update

## Testing
- [ ] Unit tests added/updated
- [ ] Integration tests added/updated
- [ ] E2E tests added/updated
- [ ] All tests passing

## Checklist
- [ ] Code follows project style guidelines
- [ ] Self-review completed
- [ ] Documentation updated
- [ ] No new warnings introduced
- [ ] Tests provide adequate coverage
- [ ] Changes work in development environment

## Screenshots (if applicable)
[Add screenshots for UI changes]

## Additional Context
[Any other relevant information]
```

### Review Process

1. **Automated Checks**: CI must pass (tests, linting, build)
2. **Code Review**: At least one approving review required
3. **Changes Requested**: Address feedback, push updates
4. **Approval**: Once approved, maintainer will merge

### After Merge

1. **Delete Branch**: Clean up your branch
   ```bash
   git branch -d feature/your-feature-name
   git push origin --delete feature/your-feature-name
   ```

2. **Update Local**: Sync with upstream
   ```bash
   git checkout main
   git pull upstream main
   ```

---

## Issue Guidelines

### Creating Issues

Use the appropriate issue template:
- **Bug Report**: For defects
- **Feature Request**: For new functionality
- **Documentation**: For docs improvements
- **Question**: For general questions

### Bug Report Template

```markdown
## Description
[Clear description of the bug]

## Steps to Reproduce
1. Go to '...'
2. Click on '...'
3. See error

## Expected Behavior
[What should happen]

## Actual Behavior
[What actually happens]

## Environment
- OS: [e.g., Windows 11, Ubuntu 22.04]
- Browser: [e.g., Chrome 120]
- Version: [e.g., v1.2.3]

## Additional Context
[Screenshots, logs, etc.]
```

### Feature Request Template

```markdown
## Problem Statement
[What problem does this solve?]

## Proposed Solution
[How would this work?]

## Alternatives Considered
[Other approaches you've thought about]

## Additional Context
[Mockups, examples, references]
```

### Issue Labels

Issues are labeled for organization:
- **Priority**: `critical`, `high`, `medium`, `low`
- **Type**: `bug`, `feature`, `docs`, `security`
- **Component**: `frontend`, `backend`, `services`, etc.
- **Status**: Managed by GitHub Project board

---

## Documentation

### Where to Document

1. **Code Comments**: For complex logic
2. **Docstrings/JSDoc**: For all public APIs
3. **README**: For repository overview and setup
4. **API Docs**: For REST/WebSocket APIs (OpenAPI)
5. **User Guides**: For end-user features
6. **Architecture Docs**: For system design decisions

### Documentation Style

- **Clear and Concise**: Get to the point
- **Examples**: Show, don't just tell
- **Updated**: Keep in sync with code
- **Accessible**: Written for the target audience
- **Searchable**: Use descriptive headings

### API Documentation

Use OpenAPI for REST APIs:

```yaml
/api/users/{userId}:
  get:
    summary: Get user by ID
    description: Retrieves detailed user information
    parameters:
      - name: userId
        in: path
        required: true
        schema:
          type: string
    responses:
      200:
        description: User found
        content:
          application/json:
            schema:
              $ref: '#/components/schemas/User'
      404:
        description: User not found
```

---

## Community

### Getting Help

- **GitHub Discussions**: For questions and general discussion
- **GitHub Issues**: For bug reports and feature requests
- **Pull Request Comments**: For specific code questions

### Staying Updated

- **Watch Repositories**: Get notified of new issues/PRs
- **GitHub Project Board**: Track overall project progress
- **Release Notes**: Check CHANGELOG.md for updates

### Recognition

Contributors are recognized in:
- **CONTRIBUTORS.md**: All contributors listed
- **Release Notes**: Major contributions highlighted
- **GitHub Insights**: Contribution graphs and stats

---

## License

By contributing to Ancient Compute, you agree that your contributions will be licensed under the MIT License.

---

## Questions?

If you have questions not covered here:
1. Check the [PROJECT_STRUCTURE.md](./PROJECT_STRUCTURE.md)
2. Search existing issues
3. Open a new discussion on GitHub

Thank you for contributing to Ancient Compute! 🚀

---

**Last Updated**: November 2, 2025  
**Version**: 1.0
