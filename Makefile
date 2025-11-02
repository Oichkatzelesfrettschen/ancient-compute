# Ancient Compute - Makefile
# Common development tasks for cross-platform development

.PHONY: help setup dev test build clean install-hooks lint format docker-up docker-down

# Default target
help:
	@echo "Ancient Compute - Development Commands"
	@echo ""
	@echo "Setup:"
	@echo "  make setup           - Initial project setup (installs dependencies)"
	@echo "  make install-hooks   - Install pre-commit hooks"
	@echo ""
	@echo "Development:"
	@echo "  make dev             - Start development servers (backend + frontend)"
	@echo "  make dev-backend     - Start backend development server only"
	@echo "  make dev-frontend    - Start frontend development server only"
	@echo ""
	@echo "Testing:"
	@echo "  make test            - Run all tests"
	@echo "  make test-backend    - Run backend tests"
	@echo "  make test-frontend   - Run frontend tests"
	@echo "  make test-coverage   - Run tests with coverage report"
	@echo ""
	@echo "Code Quality:"
	@echo "  make lint            - Run all linters"
	@echo "  make format          - Format all code"
	@echo "  make type-check      - Run type checkers"
	@echo ""
	@echo "Build:"
	@echo "  make build           - Build all components"
	@echo "  make build-backend   - Build backend with Bazel"
	@echo "  make build-frontend  - Build frontend with Vite"
	@echo "  make build-docker    - Build Docker images"
	@echo ""
	@echo "Docker:"
	@echo "  make docker-up       - Start all Docker services"
	@echo "  make docker-down     - Stop all Docker services"
	@echo "  make docker-logs     - View Docker logs"
	@echo "  make docker-clean    - Remove Docker containers and volumes"
	@echo ""
	@echo "Utilities:"
	@echo "  make clean           - Clean build artifacts"
	@echo "  make deps            - Update dependencies"
	@echo "  make minix-install   - Launch interactive MINIX installer (VNC)"
	@echo "  make minix-metrics   - Run MINIX metrics orchestrator (requires ISO path)"

# Setup
setup:
	@echo "Setting up Ancient Compute development environment..."
	@echo "Installing backend dependencies..."
	cd backend && pip install -r requirements.txt
	@echo "Installing frontend dependencies..."
	cd frontend && pnpm install
	@echo "Installing pre-commit hooks..."
	pre-commit install
	@echo "Setup complete!"

install-hooks:
	pre-commit install
	@echo "Pre-commit hooks installed"

# Development servers
dev:
	@echo "Starting development servers..."
	@echo "Backend: http://localhost:8000"
	@echo "Frontend: http://localhost:3000"
	docker-compose up

dev-backend:
	cd backend && uvicorn src.main:app --reload --host 0.0.0.0 --port 8000

dev-frontend:
	cd frontend && pnpm dev

# Testing
test:
	@echo "Running all tests..."
	$(MAKE) test-backend
	$(MAKE) test-frontend

test-backend:
	@echo "Running backend tests..."
	cd backend && pytest

test-frontend:
	@echo "Running frontend tests..."
	cd frontend && pnpm test

test-coverage:
	@echo "Running tests with coverage..."
	cd backend && pytest --cov=src --cov-report=html --cov-report=term

# Code quality
lint:
	@echo "Running linters..."
	@echo "Linting backend..."
	cd backend && pylint src/
	cd backend && mypy src/
	@echo "Linting frontend..."
	cd frontend && pnpm lint

format:
	@echo "Formatting code..."
	@echo "Formatting backend..."
	cd backend && black src/ tests/
	cd backend && isort src/ tests/
	@echo "Formatting frontend..."
	cd frontend && pnpm format

type-check:
	@echo "Running type checkers..."
	cd backend && mypy src/
	cd frontend && pnpm check

# Build
build: build-backend build-frontend

build-backend:
	@echo "Building backend..."
	bazel build //backend:all

build-frontend:
	@echo "Building frontend..."
	cd frontend && pnpm build

build-docker:
	@echo "Building Docker images..."
	docker-compose build

# Docker operations
docker-up:
	docker-compose up -d
	@echo "Docker services started"
	@echo "Backend: http://localhost:8000"
	@echo "Frontend: http://localhost:3000"
	@echo "PostgreSQL: localhost:5432"
	@echo "Redis: localhost:6379"

docker-down:
	docker-compose down
	@echo "Docker services stopped"

docker-logs:
	docker-compose logs -f

docker-clean:
	docker-compose down -v
	docker system prune -f
	@echo "Docker cleanup complete"

# Database operations
db-migrate:
	cd backend && alembic upgrade head

db-rollback:
	cd backend && alembic downgrade -1

db-reset:
	cd backend && alembic downgrade base && alembic upgrade head

db-seed:
	@echo "Seeding database..."
	python backend/src/seeder.py

# Utilities
clean:
	@echo "Cleaning build artifacts..."
	rm -rf backend/.pytest_cache
	rm -rf backend/__pycache__
	rm -rf backend/src/__pycache__
	rm -rf frontend/.svelte-kit
	rm -rf frontend/build
	rm -rf frontend/node_modules/.cache
	find . -type d -name "__pycache__" -exec rm -rf {} +
	find . -type f -name "*.pyc" -delete
	@echo "Clean complete"

deps:
	@echo "Updating dependencies..."
	cd backend && pip install -U -r requirements.txt
	cd frontend && pnpm update
	@echo "Dependencies updated"

# MINIX metrics orchestrator
minix-metrics:
	@[ -n "$$ISO" ] || (echo "Usage: make minix-metrics ISO=/path/to/minix.iso [ARCH=i386] [ITER=1]" && exit 2)
	ARCH=$${ARCH:-i386} ITER=$${ITER:-1} ./scripts/minix_metrics.sh --iso "$$ISO" --arch "$$ARCH" --iterations "$$ITER" --label make

minix-install:
	@[ -n "$$ISO" ] || (echo "Usage: make minix-install ISO=/path/to/minix.iso [ARCH=i386] [VNC=5900]" && exit 2)
	ARCH=$${ARCH:-i386} VNC=$${VNC:-5900} ./scripts/minix_install_interactive.sh --iso "$$ISO" --arch "$$ARCH" --vnc-port "$$VNC"

# Bazel operations
bazel-build:
	bazel build //...

bazel-test:
	bazel test //...

bazel-clean:
	bazel clean --expunge
