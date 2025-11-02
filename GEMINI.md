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
-   `docs/`: Contains LaTeX documentation for the project.
-   `content/`: Contains the educational content for the platform.
-   `scripts/`: Contains build and deployment scripts.
-   `config/`: Contains configuration files.
-   `shared/`: Contains common utilities shared between different parts of the project.

## Current Status (as of 2025-11-02)

Recent development has focused on addressing critical backend TODOs and filling in gaps in the service architecture. The following tasks have been completed:

-   **Backend Monitoring and Health Checks:**
    -   The `/ready` endpoint now performs connection checks for the database and Redis, ensuring the service is ready to handle requests.
    -   The `/metrics` endpoint now exposes Prometheus-style metrics for uptime and request counts, providing better observability into the service's health.

-   **User Authentication and Submissions:**
    -   A placeholder user system has been implemented to allow for the saving of code submissions without a full authentication system.
    -   The logic to save code submissions to the database has been implemented in the code execution endpoints.

-   **Language Service Factory:**
    -   The language service factory in `backend/src/services/languages/__init__.py` has been completed.
    -   Placeholder service classes have been created for the LISP, Idris, System F, and Java languages, allowing for future implementation without breaking the service factory.

-   **Database Seeding:**
    -   A database seeder script has been created at `backend/src/seeder.py` to populate the database with initial data.
    -   A `db-seed` command has been added to the `Makefile` to run the seeder script.