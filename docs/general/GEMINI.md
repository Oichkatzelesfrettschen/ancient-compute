# Gemini's Understanding of Ancient Compute

This document provides a comprehensive overview of the `ancient_compute` project, a modern, full-stack educational web application designed to teach the complete history of computation.

## Project Overview

Ancient Compute is a sophisticated platform that provides an interactive and in-depth exploration of the history of computation, from ancient tally marks to modern type theory. It features a rich, interactive frontend, a high-performance backend, and a secure, multi-language code execution environment.

- **Frontend:** The frontend is built with SvelteKit and TypeScript, featuring interactive visualizations using D3.js and Three.js, and an in-browser code editor using the Monaco Editor.
- **Backend:** The backend is powered by FastAPI (Python), providing a high-performance API for the frontend. It uses SQLite for the database and Redis for caching.
- **Build System:** The project uses Bazel for reproducible builds in a polyglot environment, managing both the Python backend and the TypeScript/SvelteKit frontend.
- **Infrastructure:** The application is containerized using Docker, with a `docker-compose.yml` file for orchestrating the various services. It uses gVisor for enhanced security in the language execution services.

## Building and Running

The project uses a `Makefile` to simplify common development tasks.

### Quick Start

1.  **Initial Setup:** Install dependencies and set up pre-commit hooks.
    ```bash
    make setup
    ```
2.  **Start Development Environment:** Start all services using Docker Compose.
    ```bash
    make docker-up
    ```
    - Frontend: `http://localhost:3000`
    - Backend: `http://localhost:8000`

### Key `make` Commands

-   `make dev`: Start all development servers (backend and frontend).
-   `make test`: Run all tests for the backend and frontend.
-   `make build`: Build all components of the project.
-   `make lint`: Run all linters to check code quality.
-f   `make format`: Format all code according to the project's style guidelines.
-   `make type-check`: Run type checkers for both the backend and frontend.
-   `make docker-down`: Stop all Docker services.
-   `make clean`: Clean all build artifacts.
-   `make help`: Show all available `make` commands.

## Development Conventions

The project enforces a strict set of development conventions to ensure code quality and consistency.

-   **Code Formatting:**
    -   **Python:** Black and isort are used for code formatting.
    -   **Frontend:** Prettier is used for formatting TypeScript, Svelte, and other frontend files.
-   **Linting:**
    -   **Python:** Pylint and Mypy are used for static analysis and type checking.
    -   **Frontend:** ESLint is used for linting the frontend codebase.
-   **Testing:**
    -   **Backend:** Pytest is used for backend testing.
    -   **Frontend:** Playwright is used for end-to-end testing of the frontend.
-   **Pre-commit Hooks:** The project uses pre-commit hooks to automatically run formatters and linters before each commit.

## Project Structure

The project is organized into the following main directories:

-   `backend/`: Contains the FastAPI backend application.
-   `frontend/`: Contains the SvelteKit frontend application.
-   `services/`: Contains the Dockerized language execution services.
-   `docs/`: Comprehensive project documentation.
    -   `general/`: General project overview, guidelines, agent docs, reports.
    -   `babbage_engine/`: Documentation specific to Babbage Analytical Engine.
    -   `requirements/`: Detailed requirements for different modules.
    -   `specifications/`: Technical specifications (e.g., language service architecture).
    -   `archive/`: Historical/superseded documents and migration reports.
-   `content/`: Educational curriculum content (modules, synthesis).
-   `scripts/`: Utility scripts for development, build, and deployment.
-   `config/`: Configuration files for various environments.
-   `tools/`: Development tools (e.g., todo_report.py).
-   `shared/`: Common utilities and libraries (e.g., common data models).

## Current Status (as of 2025-11-19)

Significant progress has been made in repository organization and documentation harmonization. The project structure has been extensively refactored to improve clarity, reduce redundancy, and establish a single source of truth for various types of information.

-   **Repository Reorganization:**
    -   Consolidated all documentation into a new, structured `docs/` directory with dedicated subdirectories for `general/`, `babbage_engine/`, `requirements/`, `specifications/`, and `minix/`.
    -   Archived outdated project summaries, historical reports, and superseded roadmap documents into `docs/archive/`.
    -   Moved all utility scripts to `scripts/` and general tools to `tools/`.
    -   Correctly identified and renamed `AGENTS.md.original` to `REPOSITORY_GUIDELINES.md` and placed it in `docs/general/`.

-   **Documentation Harmonization:**
    -   The `MASTER_ROADMAP.md` and `TODO_TRACKER.md` have been established as the primary sources of truth for project planning and task management.
    -   Consolidated multiple curriculum documents into a single `EDUCATIONAL_CURRICULUM_MATERIALS_CONSOLIDATED.md`.
    -   The various `requirements.md` files have been consolidated and reorganized into `docs/requirements/`.

-   **Build and Configuration Audit:**
    -   Audited and updated `Makefile` and `docker-compose.yml` to reflect new file paths and ensure consistency.
    -   Confirmed that `.pre-commit-config.yaml` remains correctly configured for the revised project structure.

-   **Placeholder Management:**
    -   Verified that all actionable `TODO` and `FIXME` comments are either tracked in `TODO_TRACKER.md` or are part of third-party libraries/tooling.