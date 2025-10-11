# Ancient Compute

A comprehensive educational platform teaching the complete 12,500-year history of computation, logic, and programming paradigms - from prehistoric tally marks to modern dependent type theory.

## Overview

Ancient Compute bridges humanity's longest continuous intellectual tradition, connecting:
- Prehistoric counting (20,000 BC) → Modern type theory (2025)
- Babylonian algorithms → Lambda calculus
- Greek logic → Haskell type classes
- Chinese I Ching binary → Digital circuits
- Medieval scholastic logic → Formal verification

### Target Audience

Developers who know C, Python, Assembly, Java, Haskell, IDRIS2, and LISP but lack the historical and theoretical context connecting these paradigms across 12 millennia of human thought.

## Features

- **Interactive Timeline**: D3.js + Three.js visualization spanning 12,500 years
- **Multi-Language Execution**: Secure sandboxed code execution for 8+ languages
- **Type Theory Curriculum**: From simply-typed lambda calculus to dependent types
- **Cross-Cultural Perspectives**: Babylonian, Greek, Indian, Islamic, Chinese contributions
- **Formal Documentation**: LaTeX + TikZ academic-quality materials
- **Progressive Learning**: Multiple paths from novice to expert

## Technology Stack

- **Frontend**: SvelteKit, TypeScript, Monaco Editor, D3.js, Three.js
- **Backend**: FastAPI, Python, SQLite, Redis
- **Language Services**: Docker containers with gVisor isolation
- **Build System**: Bazel for hermetic polyglot builds
- **Documentation**: XeLaTeX with pgfplots and TikZ
- **Security**: 5-layer isolation (Docker, gVisor, seccomp-bpf, cgroups, read-only FS)

## Supported Languages

- C (GCC/Clang)
- Python (CPython with RestrictedPython)
- Assembly (x86-64 emulator)
- Java (OpenJDK)
- Haskell (GHC)
- IDRIS2 (dependent types)
- LISP (SBCL)
- System F (custom interpreter)

## Quick Start

### Prerequisites

- Windows 11 with WSL2 or Debian Linux
- Docker Desktop (with gVisor runtime)
- Bazel 6.4.0+
- Python 3.11+
- Node.js 18+ with pnpm
- Git

### Installation

```bash
# Clone repository
git clone https://github.com/yourusername/ancient_compute.git
cd ancient_compute

# Windows setup
./scripts/setup-windows.ps1

# OR Debian setup
./scripts/setup-debian.sh

# Start development environment
docker-compose up -d

# Build project
bazel build //...

# Run tests
bazel test //...
```

## Development

```bash
# Backend development
cd backend
python -m venv venv
source venv/bin/activate  # Windows: venv\Scripts\activate
pip install -r requirements.txt
uvicorn main:app --reload

# Frontend development
cd frontend
pnpm install
pnpm dev

# Access services
# Frontend: http://localhost:3000
# Backend API: http://localhost:8000
# API Docs: http://localhost:8000/docs
```

## Project Structure

```
ancient_compute/
├── backend/           # FastAPI backend
├── frontend/          # SvelteKit frontend
├── services/          # Language execution services
│   ├── c/
│   ├── python/
│   ├── haskell/
│   ├── idris/
│   ├── lisp/
│   ├── assembly/
│   ├── java/
│   └── systemf/
├── docs/              # LaTeX documentation
├── content/           # Educational content
│   ├── modules/       # Historical modules
│   └── synthesis/     # Cross-era synthesis
├── scripts/           # Build and deployment
├── config/            # Configuration files
└── shared/            # Common utilities
```

## Educational Modules

### Module 0: Prehistory (20,000 BC - 3000 BC)
Ishango bone, clay tokens, one-to-one correspondence

### Module 1: Ancient Foundations (3000 BC - 500 AD)
Babylonian algorithms, Greek logic, Panini's grammar, I Ching

### Module 2: Medieval Transmission (500 - 1500 AD)
Islamic Golden Age, Al-Khwarizmi, scholastic logic

### Module 3: Symbolic Revolution (1500 - 1850)
Leibniz binary, Boolean algebra, Babbage/Lovelace

### Module 4: Foundations Crisis (1850 - 1940)
Frege, Russell, Godel, Church's lambda calculus, Turing

### Module 5: Electronic Age (1940 - 1980)
ENIAC, Von Neumann, LISP, structured programming

### Module 6: Type Theory Evolution (1970 - 2000)
System F, Hindley-Milner, dependent types

### Module 7: Paradigm Synthesis (1980 - 2025)
Multi-paradigm languages, modern type systems

## Security

All code execution occurs in multi-layer sandboxed environments:

1. Docker container isolation
2. gVisor runtime (userspace kernel)
3. Seccomp-bpf syscall filtering
4. Cgroups v2 resource limits
5. Read-only filesystem with tmpfs work directories

## Documentation

See the following for detailed information:

- [CLAUDE.md](./CLAUDE.md) - Development guide for AI assistants
- [ARCHITECTURE.md](./ARCHITECTURE.md) - System architecture
- [DOCKER_INFRASTRUCTURE.md](./DOCKER_INFRASTRUCTURE.md) - Container setup
- [TYPE_THEORY_CURRICULUM.md](./TYPE_THEORY_CURRICULUM.md) - Type theory content
- [WEEK_1_CHECKLIST.md](./WEEK_1_CHECKLIST.md) - Implementation guide

## Contributing

This is an educational project demonstrating the complete history of computation. Contributions welcome for:

- Historical accuracy improvements
- Additional programming language support
- Educational content enhancements
- Performance optimizations
- Security hardening

## License

MIT License - See LICENSE file

## Acknowledgments

This project synthesizes 12,500 years of human computational thought, honoring contributions from:

- Paleolithic mathematicians
- Babylonian astronomers
- Greek logicians
- Indian grammarians
- Islamic scholars
- Medieval scholastics
- Enlightenment philosophers
- Modern computer scientists

Every line of code participates in humanity's ancient dialogue between mind and mechanism.

## Contact

For questions or feedback, please open an issue on GitHub.

---

ANSI/ASCII only - No Unicode (per project requirements)
