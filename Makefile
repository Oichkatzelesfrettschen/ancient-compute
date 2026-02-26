# Ancient Compute - Makefile
# Common development tasks for cross-platform development

.PHONY: help setup dev test build clean install-hooks lint format docker-up docker-down test-active test-unit test-physics verify-simulation links-check links-check-full archive-audit db-init db-migrate db-rollback db-reset verify status twin-verify bom-validate

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
	@echo "  make test-unit       - Run backend unit tests only"
	@echo "  make test-physics    - Run physics/simulation tests (requires @physics marker)"
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
	@echo "Verification:"
	@echo "  make verify          - Reproduce CI checks locally (format + tests + shellcheck)"
	@echo "  make status          - Generate project status dashboard"
	@echo "  make twin-verify     - Run hardware twin golden trace tests"
	@echo "  make bom-validate    - Validate Bill of Materials"
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
	cd backend && python3 -m venv venv && . venv/bin/activate && pip install -r requirements.txt
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
	docker compose up

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

test-unit:
	@echo "Running all unit tests (no DB required)..."
	cd backend && pytest tests/unit/ -m "not db" -q

test-physics:
	@echo "Running physics/simulation tests..."
	cd backend && pytest -m physics -q

test-active:
	@echo "Running active-contract test gate (Tier A)..."
	pytest -q \
	  backend/tests/unit/test_executors_unit.py \
	  backend/tests/unit/test_language_registry.py \
	  backend/tests/integration/test_cross_language.py \
	  backend/tests/unit/test_babbage_assembler_golden.py \
	  backend/tests/unit/test_babbage_parameter_contract.py \
	  backend/tests/unit/test_c_freestanding_subset.py \
	  backend/tests/integration/test_babbage_abi_contract.py \
	  backend/tests/integration/test_babbage_mechanical_profile.py

verify-simulation:
	@echo "Verifying Babbage simulation parameter contract..."
	python3 tools/simulation/verify_babbage_params.py

links-check:
	@echo "Validating markdown links with scoped validator..."
	python3 scripts/VALIDATE_LINKS.py --scope active

links-check-full:
	@echo "Validating markdown links across docs and archive (legacy audit)..."
	python3 scripts/VALIDATE_LINKS.py --scope all

archive-audit:
	@echo "Regenerating archive audit ledger and quarantine..."
	python3 scripts/regenerate_archive_audit.py

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
	# TODO: Bazel not configured yet; add BUILD files to enable
	@echo "Bazel build not configured. Use 'pip install -r requirements.txt' for now."

build-frontend:
	@echo "Building frontend..."
	cd frontend && pnpm build

build-docker:
	@echo "Building Docker images..."
	docker compose build

# Docker operations
docker-up:
	docker compose up -d
	@echo "Docker services started"
	@echo "Backend: http://localhost:8000"
	@echo "Frontend: http://localhost:3000"
	@echo "PostgreSQL: localhost:5432"
	@echo "Redis: localhost:6379"

docker-down:
	docker compose down
	@echo "Docker services stopped"

docker-logs:
	docker compose logs -f

docker-clean:
	docker compose down -v
	docker system prune -f
	@echo "Docker cleanup complete"

# Database operations
db-init:
	@echo "Initializing local SQLite database..."
	cd backend && DATABASE_URL=sqlite:///dev.db alembic upgrade head
	@echo "Database created at backend/dev.db"

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
	rm -f docs/whitepaper/*.pdf
	find docs/ -type f \( -name "*.aux" -o -name "*.log" -o -name "*.out" -o -name "*.toc" \) -delete
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

# Bazel operations (not yet configured -- no BUILD/WORKSPACE files)
# TODO: Add BUILD files to enable Bazel-based hermetic builds
bazel-build:
	@echo "Bazel not configured. Add BUILD files first."

bazel-test:
	@echo "Bazel not configured. Use 'make test' instead."

bazel-clean:
	@echo "Bazel not configured."

# --- Historian & Content ---
historian-index:
	@echo "Building historian index and curriculum manifest..."
	@echo "Historic sources: docs/history/sources.md" > output/historian_index.txt
	@echo "Timeline: docs/history/timeline.md" >> output/historian_index.txt
	@echo "Lacunae: docs/history/lacunae.md" >> output/historian_index.txt
	@find content/modules -maxdepth 2 -type f -name README.md | sort >> output/historian_index.txt
	@find content/modules -maxdepth 1 -type d | sort > output/modules_list.txt

verify-history:
	@echo "Verifying history claims and lacunae..."
	@grep -n "Status" docs/history/verification.md | cat
	@grep -q "pending" docs/history/verification.md && echo "Pending claims exist" || echo "All claims verified"

historian-release: historian-index verify-history
	@echo "Preparing historian release v1..."
	@echo "Sources: docs/history/sources.md"
	@echo "Scans: docs/history/SCAN_LINKS.md"
	@echo "Roadmap: docs/history/HISTORIAN_ROADMAP.md"

# --- Documentation ---
docs-validate:
	@echo "Validating documentation YAML, links, and structure..."
	@python3 -c "import yaml;yaml.safe_load(open('docs/agents.yaml'))" && echo "YAML OK" || echo "YAML ERR"
	@python3 scripts/VALIDATE_LINKS.py --scope active
	@python3 scripts/gen_doc_index.py > output/docs_index.txt
	@test -d docs/history && test -d docs/speculative && echo "Structure OK" || echo "Missing directories"



# --- Measurement & Metrics ---
measure-all:
	@echo "Running all metrics collections..."
	@python3 scripts/measure_emulator.py > output/emulator_metrics.csv
	@python3 scripts/measure_emulator_real.py > output/emulator_metrics_real.csv
	@python3 scripts/measure_emulator_resources.py
	@python3 scripts/measure_emulator_throughput.py > output/emulator_throughput.csv
	@python3 scripts/measure_emulator_clock.py > output/emulator_clock.csv
	@python3 scripts/measure_longrun.py
	@bash scripts/measure_backend.sh > output/backend_metrics.csv
	@python3 scripts/measure_backend_resources.py
	@bash scripts/measure_frontend.sh > output/frontend_metrics.csv
	@python3 scripts/benchmark_emulator_programs.py
	@bash scripts/analyze_all.sh

regression-check:
	@echo "Comparing current metrics against baselines..."
	@python3 scripts/analyze_csv.py output/emulator_metrics_real.csv avg || true
	@python3 scripts/analyze_csv.py output/backend_metrics.csv latency_s || true
	@python3 scripts/analyze_csv.py output/frontend_metrics.csv value || true
	@python3 scripts/regression_alert.py || true

metrics-snapshot: env-fingerprint measure-all regression-check
	@bash scripts/snapshot_metrics.sh
	@gnuplot scripts/plot_csv.gp || echo "gnuplot not available for plotting"

env-fingerprint:
	@bash scripts/env_fingerprint.sh > output/env_fingerprint.csv

# --- Physics & Simulation ---
physics-validate:
	@echo "Validating Babbage simulation parameter contract and physics..."
	@python3 tools/simulation/verify_babbage_params.py
	@python3 scripts/validate_emulator_physics.py
	@tools/simulation/simulate.py docs/simulation/sim_schema.yaml > output/sim_validation.txt

physics-report:
	@echo "Extracting and reporting simulation parameters..."
	@tools/simulation/extract_params.py
	@cat docs/simulation/extracted.yaml || true

physics-envelope:
	@echo "Running operational envelope sweep (RPM 10-120, 100h max)..."
	@PYTHONPATH=. python3 tools/simulation/operational_envelope.py --max-hours 100 > output/operational_envelope.csv
	@echo "Output: output/operational_envelope.csv"

# --- Verification (local CI reproduction) ---
verify:
	@echo "=== Local CI Verification ==="
	@echo ""
	@echo "[1/4] Backend formatting (black)..."
	@cd backend && python3 -m black --check --diff src/ tests/ 2>&1 || (echo "FAIL: black"; exit 1)
	@echo "[2/4] Backend import order (isort)..."
	@cd backend && python3 -m isort --check-only --diff src/ tests/ 2>&1 || (echo "FAIL: isort"; exit 1)
	@echo "[3/4] Backend tests..."
	@cd backend && python3 -m pytest tests/unit/ -q \
	  --ignore=backend/tests/integration/test_cross_language.py \
	  --ignore=backend/tests/integration/test_phase4_w1_api.py \
	  2>&1 || (echo "FAIL: pytest"; exit 1)
	@echo "[4/4] Shell scripts (shellcheck)..."
	@find . -name "*.sh" -not -path "./.git/*" -not -path "./venv/*" \
	  -exec shellcheck --severity=warning {} + 2>&1 || (echo "WARN: shellcheck issues"; true)
	@echo ""
	@echo "=== Verification PASSED ==="

# --- Status Dashboard ---
status:
	@echo "Generating status dashboard..."
	@PYTHONPATH=. python3 scripts/generate_status.py

# --- Hardware Twin ---
twin-verify:
	@echo "Running hardware twin golden trace verification..."
	@PYTHONPATH=. python3 -m pytest hardware_twin/tests/ -q

# --- BOM Validation ---
bom-validate:
	@echo "Validating Bill of Materials..."
	@PYTHONPATH=. python3 tools/validate_bom.py

