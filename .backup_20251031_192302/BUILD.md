# Build System Documentation

## Overview

The Babbage Analytical Engine whitepaper build system provides a clean, reproducible way to generate publication-quality PDFs from LaTeX source files. The system uses XeLaTeX with professional typesetting, TikZ diagrams, and pgfplots visualizations.

## Quick Start

From the repository root, build all whitepapers:

```bash
make -f Makefile.whitepaper all
```

View the main deliverable:

```bash
make -f Makefile.whitepaper view
```

Clean build artifacts:

```bash
make -f Makefile.whitepaper clean
```

## Build Targets

### All (Default Target)

Compiles all three whitepapers with proper 2-pass XeLaTeX compilation:

```bash
make -f Makefile.whitepaper all
```

**Output files:**
- `output/main/BABBAGE_ANALYTICAL_ENGINE_COMPLETE.pdf` — Main unified deliverable (57 KB)
- `output/main/PEDAGOGICAL_CONTENT.pdf` — Teaching material (51 KB)
- `output/main/GRAPHS_AND_DATA.pdf` — Data visualizations (28 KB)
- `output/main/README.md` — Build information

**Duration:** ~30 seconds per document (2 passes each)

### View

Opens the main whitepaper PDF in your default viewer:

```bash
make -f Makefile.whitepaper view
```

Uses `xdg-open` (Linux) or `open` (macOS).

### Clean

Removes build artifacts (temporary files) while preserving generated PDFs:

```bash
make -f Makefile.whitepaper clean
```

**Removes:**
- `build/*.pdf`, `build/*.aux`, `build/*.log`, `build/*.toc`, `build/*.out`
- All LaTeX-generated temporary files

**Preserves:**
- All PDFs in `output/main/`
- Source `.tex` files

### Distclean

Complete clean rebuild — removes all outputs and build artifacts:

```bash
make -f Makefile.whitepaper distclean
```

Followed by:

```bash
make -f Makefile.whitepaper all
```

to rebuild from scratch.

### Help

Display available targets:

```bash
make -f Makefile.whitepaper help
```

## Directory Structure

```
/home/eirikr/Playground/ancient_compute/
├── Makefile.whitepaper              # Build orchestration (THIS FILE)
├── BABBAGE_COMPLETE_WHITEPAPER.tex  # Main unified source
├── PEDAGOGICAL_WHITEPAPER.tex       # Teaching material source
├── PEDAGOGICAL_GRAPHS_AND_DATA.tex  # Data visualization source
│
├── build/                           # Temporary compilation directory
│   ├── *.pdf                        # Intermediate PDFs (cleaned by 'make clean')
│   ├── *.aux, *.log, *.toc, ...     # LaTeX temporary files
│   └── (auto-created on first build)
│
├── output/                          # Final deliverables
│   ├── main/                        # PRIMARY OUTPUTS (use these)
│   │   ├── BABBAGE_ANALYTICAL_ENGINE_COMPLETE.pdf
│   │   ├── PEDAGOGICAL_CONTENT.pdf
│   │   ├── GRAPHS_AND_DATA.pdf
│   │   └── README.md
│   │
│   └── exploratory/                 # Historical/exploratory materials
│       ├── BABBAGE_PHASE2_BUILDOUT_PART1.pdf
│       ├── BABBAGE_PHASE2_BUILDOUT_PART2.pdf
│       ├── BABBAGE_EARLIER_PHASE_MAIN.pdf
│       └── README.md
│
└── whitepaper/                      # Legacy source files (for reference)
    ├── babbage_whitepaper_main.tex
    ├── babbage_phase2_buildout.tex
    ├── babbage_phase2_buildout_part2.tex
    └── ... (various metadata files)
```

## Source Files

### BABBAGE_COMPLETE_WHITEPAPER.tex (~30 KB, 1200+ lines)

**Purpose:** Unified master document combining Phases 3-4 specifications with pedagogical content.

**Content:**
- Part I: Foundations (chapters 1-2)
- Part II: System Design (chapters 3-4)
- Part III: Manufacturing (chapters 5-6)
- Part IV: Quality Assurance (chapters 7-8)
- Part V: Validation (chapters 9-10)
- Part VI: Analysis (chapters 11-13)

**Styling:** Professional typography, color-coded sections, TikZ diagrams, pgfplots graphs.

### PEDAGOGICAL_WHITEPAPER.tex (~40 KB)

**Purpose:** Teaching material organized by difficulty level.

**Levels:**
- Beginner: What and Why (first principles)
- Intermediate: How they work together (system behavior)
- Advanced: Design and implementation (engineering depth)

### PEDAGOGICAL_GRAPHS_AND_DATA.tex (~20 KB)

**Purpose:** Data visualizations and analysis from manufacturing and testing.

**Sections:**
- Manufacturing performance metrics
- Component testing and quality control
- Subassembly integration testing
- System operational validation (20-program test suite)
- Cost analysis and regional feasibility
- Mechanical reliability assessment
- Project timeline and phases

## Compilation Process

### How It Works

1. **Source Preparation**: LaTeX source file (e.g., `BABBAGE_COMPLETE_WHITEPAPER.tex`)
2. **Pass 1**: XeLaTeX reads source, generates intermediate files (`.aux`, `.toc`)
3. **Pass 2**: XeLaTeX reads source again with TOC/cross-references resolved
4. **Output**: PDF written to build directory
5. **Copy**: PDF moved to output directory with meaningful filename

### 2-Pass Compilation

The Makefile runs XeLaTeX twice per document:

```makefile
cd $(BUILD_DIR) && $(XELATEX) $(XELATEX_FLAGS) ../$< > /dev/null 2>&1
cd $(BUILD_DIR) && $(XELATEX) $(XELATEX_FLAGS) ../$< > /dev/null 2>&1
```

**Why two passes?**
- Pass 1: Generates table of contents (`.toc` file)
- Pass 2: Uses that TOC to create correct page numbers in references

This is standard practice for LaTeX documents with cross-references.

### XeLaTeX Configuration

**Engine:** XeLaTeX (Unicode-aware, system font support)

**Flags:**
- `-interaction=nonstopmode` — Continue on errors (don't prompt)
- `-halt-on-error` — Stop on fatal errors

**Packages Used:**
- `tikz` — Vector graphics and diagrams
- `pgfplots` — Data visualization and graphs
- `pgfplotstable` — Data tables and matrices
- `booktabs` — Professional table formatting
- `hyperref` — PDF hyperlinking and cross-references
- `titlesec` — Custom section styling
- `fancyhdr` — Header and footer customization
- `geometry` — Page layout and margins
- `xcolor` — Color definitions

## Modification Guide

### Editing Source Files

To modify the whitepapers:

1. Edit `.tex` source file in your preferred text editor (e.g., VSCode, Vim)
2. Rebuild: `make -f Makefile.whitepaper all`
3. View result: `make -f Makefile.whitepaper view`

**Do NOT edit PDFs directly.** Always modify `.tex` source and regenerate.

### Adding New Sections

Example: Add a new chapter to main whitepaper

```latex
% In BABBAGE_COMPLETE_WHITEPAPER.tex

\chapter{New Chapter Title}

\section{First Section}

Your content here...

\subsection{Subsection}

More content...
```

Then rebuild:

```bash
make -f Makefile.whitepaper all
```

### Updating Colors

Color definitions are in the LaTeX preamble:

```latex
\definecolor{darkblue}{HTML}{003366}
\definecolor{lightblue}{HTML}{E6F0FF}
\definecolor{gold}{HTML}{FFB900}
```

Edit any hex color code and rebuild.

### Adding Diagrams

Use TikZ for vector graphics:

```latex
\begin{tikzpicture}
  \draw (0,0) -- (2,2);
  \node at (1,1) {Label};
\end{tikzpicture}
```

Or pgfplots for data:

```latex
\begin{axis}[xlabel=X, ylabel=Y]
  \addplot coordinates {(1,2) (2,4) (3,6)};
\end{axis}
```

## Troubleshooting

### PDFs not generated

**Symptom:** `make all` runs but no PDFs appear in output/main/

**Solution:**
1. Check build directory: `ls -la build/`
2. Look for error in log: `tail -50 build/*.log`
3. Run distclean and rebuild: `make -f Makefile.whitepaper distclean && make -f Makefile.whitepaper all`

### LaTeX errors in log files

**Common error: "inputenc is not designed for xetex"**
- **Cause**: Using `\usepackage[utf-8]{inputenc}` with XeLaTeX
- **Fix**: Remove that line; XeLaTeX handles UTF-8 natively

**Common error: "Undefined color"**
- **Cause**: Using a color not defined in preamble
- **Fix**: Add `\definecolor{colorname}{HTML}{XXXXXX}` in preamble

**Common error: "Not allowed in LR mode"**
- **Cause**: Line break (`\\`) in TikZ node text
- **Fix**: Use `text width=...` and `align=center` options instead

### Build takes too long

**Optimization:**
- Build individual documents instead of all:
  ```bash
  cd build && xelatex -interaction=nonstopmode -halt-on-error ../BABBAGE_COMPLETE_WHITEPAPER.tex
  ```
- First pass is slow; second pass is faster
- Clean build artifacts if they accumulate: `make -f Makefile.whitepaper clean`

## Continuous Integration / Automated Builds

### CI/CD Integration

The Makefile can be used in CI/CD pipelines:

```bash
# In GitHub Actions, GitLab CI, or Jenkins:
make -f Makefile.whitepaper clean
make -f Makefile.whitepaper all

# Check for success
test -f output/main/BABBAGE_ANALYTICAL_ENGINE_COMPLETE.pdf && echo "✓ Build successful"
```

### Pre-commit Hook

To automatically rebuild on git commits:

```bash
# In .git/hooks/pre-commit
#!/bin/sh
make -f Makefile.whitepaper clean
make -f Makefile.whitepaper all
git add output/main/
```

## Dependencies

### Required System Packages

- `texlive-xetex` — XeLaTeX engine
- `texlive-latex-extra` — Additional packages (TikZ, pgfplots, etc.)
- `texlive-fonts-recommended` — Font support

### Installation (Arch/CachyOS)

```bash
pacman -S texlive-xetex texlive-latex texlive-fonts texlive-pictures
```

### Installation (Debian/Ubuntu)

```bash
apt-get install texlive-xetex texlive-latex-extra texlive-fonts-recommended
```

### Installation (macOS)

```bash
brew install mactex
```

## Performance Metrics

**Typical build times (per document):**
- Pass 1: 5-8 seconds
- Pass 2: 4-6 seconds
- Total per document: ~10-15 seconds
- All three documents: ~30-45 seconds

**Output file sizes:**
- BABBAGE_ANALYTICAL_ENGINE_COMPLETE.pdf: 57 KB
- PEDAGOGICAL_CONTENT.pdf: 51 KB
- GRAPHS_AND_DATA.pdf: 28 KB

**Total deliverable size: ~136 KB** (highly compressed, publication-ready)

## Version Control

### Tracking in Git

**Include in git:**
- `Makefile.whitepaper` (build orchestration)
- `*.tex` source files (easily diffed, version controlled)
- `.gitignore` entries for build artifacts

**Exclude from git:**
- `build/` directory (temporary, regenerated)
- `output/main/*.pdf` (generated from source)
- `*.aux`, `*.log`, `*.toc`, `*.out` (LaTeX temporaries)

### Suggested .gitignore

```
build/
*.pdf
*.aux
*.log
*.toc
*.out
*.fls
*.fdb_latexmk
*.bbl
*.blg
```

## Rebuilding for Distribution

Complete workflow for clean distribution build:

```bash
cd /home/eirikr/Playground/ancient_compute

# Remove all outputs and temporaries
make -f Makefile.whitepaper distclean

# Rebuild from scratch
make -f Makefile.whitepaper all

# Verify outputs
ls -lh output/main/*.pdf

# Check for completeness
file output/main/*.pdf  # Should all be "PDF 1.7"

# Archive for distribution
tar czf babbage_whitepapers_$(date +%Y%m%d).tar.gz output/main/
```

## Support and Further Assistance

For detailed content questions:
- See `output/main/BABBAGE_ANALYTICAL_ENGINE_COMPLETE.pdf`
- See `output/main/README.md` for document organization

For technical build issues:
- Check logs: `cat build/*.log | tail -100`
- Reinstall LaTeX: `pacman -S texlive-xetex`
- Run distclean rebuild: `make -f Makefile.whitepaper distclean && make -f Makefile.whitepaper all`

---

**Last Updated:** October 31, 2025  
**Build System Version:** 1.0  
**Status:** Production-ready
