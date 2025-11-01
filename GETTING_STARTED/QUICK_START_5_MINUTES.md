# Quick Start (5 Minutes)

## What Is This Project?

Two things:

1. **Ancient Compute**: An educational platform teaching 12,500 years of computation history—from tally marks to quantum computing—across 8 programming languages.

2. **Babbage Analytical Engine Specification**: A complete engineering blueprint to manufacture a working mechanical computer using 1930s-1960s technology.

---

## I Just Want to Learn the Code Part (5 min path)

### Step 1: Run the Application (2 min)
```bash
# Navigate to project root
cd /home/eirikr/Playground/ancient_compute

# Start all services (backend, frontend, database, cache)
make dev

# Open your browser
# Frontend: http://localhost:3000
# Backend API: http://localhost:8000/docs
```

### Step 2: Explore Interactive Lessons (2 min)
- Visit http://localhost:3000
- Browse historical modules (Prehistory → Ancient → Medieval → Modern)
- Click on code examples to see them in action
- Try the interactive code playground (if available)

### Step 3: Next Steps (1 min)
- Read a lesson that interests you
- Complete an exercise
- Check out [CURRICULUM_AND_CONTENT/](../CURRICULUM_AND_CONTENT/) for full curriculum

---

## I Want to Implement/Manufacture Babbage (5 min path)

### Step 1: Understand the Scope (2 min)
- Read: [BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md](../BABBAGE_ENGINE_SPECIFICATION/00_QUICK_FACTS_AND_OVERVIEW.md)
- Key fact: **Fully functional Analytical Engine with 1930s-1960s technology**
- Feasible in India (£7,700/unit), Brazil (£10,200), Argentina (£9,500), China (£6,500 at scale)
- Timeline: 18-24 months for first unit

### Step 2: Read the Specification (2 min)
- Start: [01_COMPLETE_TECHNICAL_SPECIFICATION.md](../BABBAGE_ENGINE_SPECIFICATION/01_COMPLETE_TECHNICAL_SPECIFICATION.md)
- This covers: architecture, arithmetic, memory, operations, sourcing, feasibility

### Step 3: Next Steps (1 min)
- Choose your region/scenario
- Read [02_MANUFACTURING_PROCEDURES.md](../BABBAGE_ENGINE_SPECIFICATION/02_MANUFACTURING_PROCEDURES.md)
- Verify suppliers via [03_HISTORICAL_AUDIT_AND_CORRECTIONS.md](../BABBAGE_ENGINE_SPECIFICATION/03_HISTORICAL_AUDIT_AND_CORRECTIONS.md)
- Check [IMPLEMENTATION_PHASES/](../IMPLEMENTATION_PHASES/) for phase-by-phase details

---

## I'm a Developer/Engineer Setting Up Locally (5 min path)

### Step 1: Prerequisites (1 min)
```bash
# Check you have these installed
docker --version       # Docker & Docker Compose
python3 --version      # Python 3.9+
node --version         # Node.js 18+
make --version         # Make
```

If missing, see [DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)

### Step 2: Clone & Setup (2 min)
```bash
# Clone repo (if not already done)
git clone <repo-url> ancient_compute
cd ancient_compute

# Install dependencies and build
make setup

# This installs Python, Node, and Docker images
```

### Step 3: Start Development (1 min)
```bash
# Start all services with hot reload
make dev

# Backend API docs: http://localhost:8000/docs
# Frontend: http://localhost:3000
```

### Step 4: Build Documentation (1 min)
```bash
# Build the Babbage whitepaper
cd whitepaper
make
# Output: babbage_whitepaper_main.pdf
```

---

## I Want to Understand the Architecture (5 min path)

### Step 1: Visual Overview (1 min)
```
┌─────────────────────────────────────┐
│     Frontend (SvelteKit)            │  http://localhost:3000
│  Monaco Editor, D3.js, Three.js    │
└──────────────┬──────────────────────┘
               │
┌──────────────▼──────────────────────┐
│     Backend API (FastAPI)           │  http://localhost:8000
│  REST endpoints, WebSockets         │
└──────────────┬──────────────────────┘
               │
      ┌────────┼────────┐
      ▼        ▼        ▼
   ┌────┐  ┌────┐  ┌─────────┐
   │ DB │  │Cache│  │Languages│
   │(PG)│  │(Redis)│ (Docker) │
   └────┘  └────┘  └─────────┘
```

**Backend**: 
- REST API for content, exercises, language execution
- WebSocket for real-time code feedback
- Executes user code in sandboxed Docker containers

**Frontend**:
- Historical timeline UI
- Interactive code editor (Monaco)
- D3.js visualizations of algorithmic evolution
- Three.js 3D models of mechanical computers

### Step 2: Read the Architecture Doc (2 min)
- Read: [../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
- Details: Database schema, API endpoints, security model, content management

### Step 3: Explore the Codebase (2 min)
```bash
# Backend structure
ls -la backend/src/
# api/        - REST endpoints
# models/     - Database models
# services/   - Business logic
# security/   - Sandboxing & limits

# Frontend structure
ls -la frontend/src/
# routes/     - Page routes
# components/ - UI components
# lib/        - Utilities
```

---

## Common Tasks (Quick Reference)

### Run Tests
```bash
make test          # All tests
make test-backend  # Backend only
make test-frontend # Frontend only
```

### Code Quality
```bash
make lint          # Check code style
make format        # Auto-format code
```

### View API Docs
```bash
# After running 'make dev', visit:
http://localhost:8000/docs
```

### Build LaTeX Whitepaper
```bash
cd whitepaper
make clean
make
# Output: babbage_whitepaper_main.pdf
```

### Check Implementation Status
```bash
# Read this file
cat ARCHITECTURE_AND_DESIGN/WEEK_1_COMPLETION_STATUS.md

# See the 52-week roadmap
cat ../ARCHITECTURE_AND_DESIGN/IMPLEMENTATION_ROADMAP.md
```

---

## Where to Go Next

**Learning Path** | **Your Link**
---|---
I want to learn computation history | [CURRICULUM_AND_CONTENT/](../CURRICULUM_AND_CONTENT/)
I want to set up development | [DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md](../DEVELOPMENT_GUIDES/01_SETUP_AND_INSTALLATION.md)
I want to understand the architecture | [../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md](../../ARCHITECTURE_AND_DESIGN/ARCHITECTURE.md)
I want to manufacture Babbage | [BABBAGE_ENGINE_SPECIFICATION/](../BABBAGE_ENGINE_SPECIFICATION/)
I want to find a specific document | [DOCUMENT_FINDER.md](DOCUMENT_FINDER.md)
I'm stuck / have questions | [README.md](README.md) - "Getting Help" section

---

**Ready?** Choose your path above and dive in.

**Questions?** See [README.md](README.md) for more guidance.
