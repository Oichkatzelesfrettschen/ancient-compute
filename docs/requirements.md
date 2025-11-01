# Documentation Module - Requirements and Architecture

**Module Path**: `docs/`
**Build System**: XeLaTeX + TikZ + pgfplots
**Output Format**: PDF (3,000-5,000 pages)
**Status**: Phase 1 Complete (Foundation), Phase 2+ Expanding

---

## Overview

The documentation module produces a comprehensive LaTeX-based curriculum spanning 12,500 years of computation history. It features:

- **Seven Historical Volumes**: Prehistory through Paradigm Synthesis
- **Interactive Diagrams**: TikZ illustrations of machines, algorithms, concepts
- **Function Plots**: pgfplots for visualizing mathematical evolution
- **Code Examples**: Integrated code snippets in multiple languages
- **Cross-References**: Hyperlinked connections between topics
- **Mathematical Notation**: Full LaTeX support for formal specifications

**Compilation Target**: Single unified PDF with all 7 volumes, 500+ exercises, 300+ illustrations

---

## Directory Structure

```
docs/
├── requirements.md                # This file
├── main.tex                       # Master document (compilation entry)
├── preamble.tex                  # Packages, commands, macros
├── config.tex                    # Global configuration
├── volumes/
│   ├── volume-0-prehistory/      # 20,000 BC - 3,000 BC
│   │   ├── 00-intro.tex
│   │   ├── 01-tally-marks.tex
│   │   ├── 02-clay-tokens.tex
│   │   └── 03-proto-writing.tex
│   ├── volume-1-ancient/         # 3,000 BC - 500 AD
│   │   ├── mesopotamia/
│   │   ├── egypt/
│   │   ├── greece/
│   │   ├── india/
│   │   └── china/
│   ├── volume-2-medieval/        # 500 - 1500 AD
│   │   ├── islamic-golden-age/
│   │   ├── al-khwarizmi/
│   │   ├── scholastic-logic/
│   │   └── lull-ars-magna/
│   ├── volume-3-early-modern/    # 1500 - 1850
│   │   ├── leibniz/
│   │   ├── pascal/
│   │   ├── boole/
│   │   └── babbage-lovelace/
│   ├── volume-4-foundations/     # 1850 - 1940
│   │   ├── frege-russell/
│   │   ├── godel-incompleteness/
│   │   ├── church-lambda/
│   │   └── turing-machines/
│   ├── volume-5-electronic/      # 1940 - 1980
│   │   ├── eniac/
│   │   ├── von-neumann/
│   │   ├── transistor-age/
│   │   ├── lisp-algol/
│   │   └── structured-programming/
│   ├── volume-6-type-theory/     # 1970 - 2000
│   │   ├── curry-howard/
│   │   ├── system-f/
│   │   ├── hindley-milner/
│   │   ├── martin-lof-types/
│   │   └── dependent-types/
│   └── volume-7-synthesis/       # 1980 - 2025
│       ├── paradigm-unification/
│       ├── type-system-evolution/
│       ├── modern-languages/
│       └── quantum-computing/
├── diagrams/                     # TikZ illustrations
│   ├── ancient/
│   │   ├── abacus.tikz
│   │   ├── pascaline.tikz
│   │   ├── leibniz-wheel.tikz
│   │   └── difference-engine.tikz
│   ├── mechanical/
│   │   ├── punch-card.tikz
│   │   ├── relay-logic.tikz
│   │   ├── transistor-circuit.tikz
│   │   └── integrated-circuit.tikz
│   ├── computation/
│   │   ├── turing-machine.tikz
│   │   ├── lambda-calculus.tikz
│   │   ├── state-machine.tikz
│   │   ├── type-hierarchy.tikz
│   │   └── cpu-architecture.tikz
│   └── graphs/                   # pgfplots plots
│       ├── complexity-evolution.tex
│       ├── language-paradigms.tex
│       └── processor-evolution.tex
├── exercises/                    # Programming exercises
│   ├── ancient-algorithms.tex    # Babylonian, Egyptian
│   ├── symbolic-logic.tex        # Russell, Frege exercises
│   ├── lambda-calculus.tex       # Church notation exercises
│   ├── type-theory.tex           # Polymorphism, dependent types
│   └── solutions/
│       ├── ancient-solutions.tex
│       ├── symbolic-solutions.tex
│       ├── lambda-solutions.tex
│       └── type-solutions.tex
├── code-examples/               # Multi-language code samples
│   ├── algorithms/
│   │   ├── babylonian-sqrt.c
│   │   ├── babylonian-sqrt.py
│   │   ├── babylonian-sqrt.hs
│   │   └── babylonian-sqrt.idris
│   ├── logic/
│   │   ├── syllogism.lisp
│   │   ├── truth-tables.hs
│   │   └── proof.idris
│   └── type-systems/
│       ├── polymorphism.hs
│       ├── rank2.systemf
│       └── dependent.idris
├── images/                      # Raster images (screenshots, scans)
│   ├── historical/
│   │   ├── antikythera-mechanism.jpg
│   │   ├── pascaline-original.jpg
│   │   ├── babbage-engine.jpg
│   │   ├── eniac-panel.jpg
│   │   └── early-computers.jpg
│   └── modern/
│       ├── type-inference-tree.png
│       ├── compilation-pipeline.png
│       └── cpu-schematic.png
├── bibliography/
│   ├── references.bib            # LaTeX bibliography
│   ├── primary-sources.bib       # Historical primary sources
│   └── secondary-sources.bib     # Modern references
├── styles/
│   ├── colors.tex                # Color definitions
│   ├── formatting.tex            # Typography and spacing
│   ├── code-style.tex            # Code listing formatting
│   └── theorem-styles.tex        # Proof and theorem formatting
├── build/                        # Generated files (not in git)
│   ├── main.pdf
│   ├── main.aux
│   ├── main.toc
│   └── ...
└── Makefile                      # Build automation
```

---

## LaTeX Build Requirements

### XeLaTeX Installation

**Minimum Version**: 3.14 (released 2018)

**Installation by OS**:

**Linux (Debian/Ubuntu)**:
```bash
# Full installation (3.5 GB)
apt-get install texlive-full

# Or selective (1 GB)
apt-get install \
  texlive-latex-base \
  texlive-latex-extra \
  texlive-fonts-recommended \
  texlive-xetex
```

**macOS**:
```bash
# Via Homebrew
brew install --cask mactex

# Or smaller installation
brew install mactex-no-gui
```

**Windows**:
```
1. Download: https://miktex.org/download
2. Run installer
3. Enable "Install missing packages automatically"
```

**Verification**:
```bash
xelatex --version     # Should show 3.14+
kpsewhich xelatex     # Should find binary
```

### Required LaTeX Packages

**Core Document** (automatically included):
- `fontspec` - Font selection for XeLaTeX
- `geometry` - Page layout
- `hyperref` - PDF bookmarks and links
- `babel` - Multilingual support (English, Greek, Arabic for historical quotes)

**Mathematical Typesetting**:
- `amsmath` - AMS math environments
- `amssymb` - AMS symbols
- `mathtools` - Additional math tools
- `stmaryrd` - Math symbols (lambdas, turnstiles)

**Code and Listings**:
- `listings` - Code syntax highlighting
- `minted` - Advanced code formatting (requires Python + Pygments)

**Graphics and Diagrams** (CRITICAL):
- `tikz` - Drawing framework (400+ pages of TikZ code)
- `pgfplots` - Function plotting
- `pgf` - Graphics framework (dependency of tikz)
- `xcolor` - Color definitions

**Bibliography**:
- `biblatex` - Modern bibliography management
- `biber` - Bibliography engine

**Typography**:
- `microtype` - Advanced typography
- `fontawesome` - Icon font
- `fancyhdr` - Header/footer customization

**Utilities**:
- `etoolbox` - Tools for macro programming
- `xifthen` - Conditional logic
- `ifthen` - Legacy conditional support

### Package Verification

```bash
# Check if packages are installed
kpsewhich tikz.sty          # Should find TikZ
kpsewhich pgfplots.sty      # Should find pgfplots
kpsewhich minted.sty        # Should find minted

# If not found, install via package manager
# Ubuntu: apt-get install texlive-pictures
# macOS: brew install basictex && tlmgr install tikz pgfplots
```

---

## Document Compilation

### Main Compilation File

**File**: `main.tex`

**Minimum Structure**:
```latex
\documentclass[11pt,a4paper,twoside]{book}

% ===== PREAMBLE =====
\usepackage{fontspec}
\usepackage{tikz}
\usepackage{pgfplots}
\usepackage{hyperref}
% ... 50+ more package declarations

% ===== CONFIGURATION =====
\title{Ancient Compute: 12,500 Years of Computation}
\author{Ancient Compute Collective}
\date{2025}

\begin{document}

\frontmatter
\maketitle
\tableofcontents

\mainmatter
% ===== INCLUDE VOLUMES =====
\include{volumes/volume-0-prehistory/00-intro}
\include{volumes/volume-1-ancient/mesopotamia/mesopotamia}
% ... 50+ more \include statements

\appendix
\include{exercises/ancient-algorithms}
\include{bibliography/references}

\backmatter
\printindex

\end{document}
```

### Compilation Commands

**Full Compilation** (from clean state):
```bash
cd docs

# Run 1: Build references and structure
xelatex -interaction=nonstopmode main.tex

# Run 2: Regenerate bibliography
biber main

# Run 3: Regenerate references
xelatex -interaction=nonstopmode main.tex

# Run 4: Final pass (TOC, cross-refs)
xelatex -interaction=nonstopmode main.tex

# Verify output
pdfinfo main.pdf                    # Show metadata
ls -lh main.pdf                     # Show size
```

**Quick Recompilation** (after minor changes):
```bash
xelatex -interaction=nonstopmode main.tex
```

**Verify Compilation Success**:
```bash
# No errors in output
grep "Overfull\|Underfull\|Error" main.log

# PDF generated
test -f main.pdf && echo "SUCCESS: PDF generated"

# Correct page count
pdfinfo main.pdf | grep Pages
# Expected: Pages ~3000-5000
```

### Makefile Automation

**File**: `Makefile`

```makefile
.PHONY: build clean view help

help:
	@echo "Ancient Compute Documentation Build"
	@echo "Commands:"
	@echo "  make build        - Full compilation (3-4 minutes)"
	@echo "  make quick        - Quick recompilation (10-20 seconds)"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make view         - Open PDF in default viewer"
	@echo "  make verify       - Check compilation success"

build:
	xelatex -interaction=nonstopmode main.tex
	biber main
	xelatex -interaction=nonstopmode main.tex
	xelatex -interaction=nonstopmode main.tex

quick:
	xelatex -interaction=nonstopmode main.tex

clean:
	rm -f *.aux *.bbl *.blg *.log *.out *.toc *.lof *.lot
	rm -f *.idx *.ilg *.ind *.synctex.gz
	find . -name "*.auxlock" -delete

view:
	@if [ -f main.pdf ]; then \
	  xdg-open main.pdf || open main.pdf; \
	else \
	  echo "Error: main.pdf not found. Run 'make build' first."; \
	fi

verify:
	@echo "=== Compilation Quality ==="
	@if grep -q "Overfull\\|Underfull" main.log; then \
	  echo "WARNING: Some lines are over/underful"; \
	  grep "Overfull\\|Underfull" main.log | head -10; \
	else \
	  echo "OK: All lines fit properly"; \
	fi
	@echo "=== Output Size ==="
	@ls -lh main.pdf
	@echo "=== Page Count ==="
	@pdfinfo main.pdf | grep Pages
```

**Usage**:
```bash
make build              # Full compilation
make quick              # Fast recompilation
make clean              # Remove generated files
make view               # Open PDF
make verify             # Check quality
```

---

## Document Content Structure

### Volume 0: Prehistory of Counting (20,000 - 3,000 BC)

**Chapters**:
1. Ishango Bone and Tally Systems
2. Clay Tokens and Record-Keeping
3. One-to-One Correspondence and Cardinality
4. Counting Systems Across Cultures

**Exercises**:
- Ancient number systems comparison
- Tally mark encoding/decoding
- Finger counting algorithms

**TikZ Illustrations**:
- Ishango bone reproduction
- Clay token inventory systems
- Tally mark variations

### Volume 1: Ancient Foundations (3,000 BC - 500 AD)

**Major Topics**:
- Babylonian algorithms (square root, division)
- Egyptian fraction mathematics
- Euclid's Elements and geometric algorithms
- Panini's Sanskrit grammar (formal language theory)
- I Ching binary representations
- Greek logic (Aristotle's syllogisms)

**Code Examples**:
- Babylonian square root in C, Python, Haskell
- Euclidean algorithm across languages

### Volume 2-7: Progressive Sophistication

Building through Medieval Transmission, Early Modern Revolution, Foundations Crisis, Electronic Age, Type Theory Evolution, to modern Paradigm Synthesis.

---

## TikZ Diagrams

### Mechanical Devices

**Abacus**:
```latex
\begin{tikzpicture}
  % Horizontal wooden frame
  \draw (0,0) rectangle (10,5);

  % Rods with beads
  \foreach \i in {1,2,3,4} {
    \draw (1.5 + \i*1.5, 0.5) -- (1.5 + \i*1.5, 4.5);
    \foreach \j in {0.5, 1.5, 2.5, 3.5} {
      \draw[fill=brown!50] (1.5 + \i*1.5 - 0.3, \j) circle (0.25);
    }
  }
\end{tikzpicture}
```

**Difference Engine**:
```latex
% Complex mechanical representation
% 50+ lines of TikZ showing gears, wheels, digit wheels
```

### Computational Structures

**Turing Machine**:
```latex
% Infinite tape, head, state diagram
% 100+ lines of TikZ
```

**Lambda Calculus Reduction**:
```latex
% Tree showing reduction steps
% Shows alpha-equivalence, beta-reduction, normal form
```

**Type Hierarchy**:
```latex
% Diagram showing type relationships
% Base types → Compound types → Polymorphic types → Dependent types
```

### pgfplots Visualizations

**Complexity Evolution**:
```latex
\begin{tikzpicture}
\begin{axis}[
  xlabel=Year,
  ylabel=Computational Complexity Class,
  title=Evolution of Computable Problems
]
\addplot[color=red] table {data/complexity.dat};
\end{axis}
\end{tikzpicture}
```

---

## Bibliography and Sources

### Primary Sources

**Ancient**:
- Euclid, *Elements* (300 BC)
- Diophantus, *Arithmetica* (250 AD)
- Al-Khwarizmi, *Kitab al-Mukhtasar* (820 AD)

**Early Modern**:
- Leibniz, *Binary Arithmetic* (1703)
- Boolean Algebra papers (1850s)
- Babbage, *Passages from the Life of a Philosopher* (1864)

**Foundations**:
- Frege, *Begriffsschrift* (1879)
- Russell & Whitehead, *Principia Mathematica* (1910-1913)
- Godel, *Incompleteness Theorems* papers (1931)
- Church & Turing papers (1936)

### Bibliography Management

**Tool**: BibLaTeX with Biber

**Citation Format**: Author-Year (Harvard style)

**File**: `bibliography/references.bib`

```bibtex
@book{euclid2002elements,
  author = {Euclid and Fitzpatrick, Richard},
  title = {Euclid's Elements of Geometry},
  year = {2002},
  publisher = {Lulu Press}
}

@article{turing1936,
  author = {Alan Mathison Turing},
  title = {On Computable Numbers},
  journal = {Proceedings of the London Mathematical Society},
  year = {1936},
  volume = {42},
  pages = {230--265}
}
```

---

## Code Integration

### Inline Code Examples

**LaTeX Syntax**:
```latex
\begin{lstlisting}[language=C, caption=Babylonian Square Root]
long babylonian_sqrt(long n) {
  if (n == 0) return 0;
  long x = n;
  long x_old = 0;
  while (x != x_old) {
    x_old = x;
    x = (x + n / x) / 2;
  }
  return x;
}
\end{lstlisting}
```

**Syntax Highlighting**:
- C: `language=C`
- Python: `language=Python`
- Haskell: `language=Haskell`
- LISP: `language=Lisp`

### Multi-Language Comparison

**Code Parallel Sections**:
```latex
\begin{multicols}{2}
\subsubsection*{C Implementation}
\lstinputlisting[language=C]{../code-examples/babylonian.c}

\columnbreak

\subsubsection*{Python Implementation}
\lstinputlisting[language=Python]{../code-examples/babylonian.py}
\end{multicols}
```

---

## Exercises and Solutions

### Exercise Format

```latex
\begin{exercise}[Difficulty: 2/5]
Implement the Euclidean algorithm in your preferred language.

\begin{enumerate}
\item Create a function \texttt{gcd(a, b)} that computes the GCD
\item Test with inputs: $(48, 18)$, $(100, 35)$, $(1071, 462)$
\item Verify correctness against built-in \texttt{math.gcd}
\item Measure execution time for large numbers
\end{enumerate}
\end{exercise}
```

### Solution Format

```latex
\begin{solution}[Exercise X.Y]
The Euclidean algorithm exploits the property:
\[ \gcd(a, b) = \gcd(b, a \bmod b) \]

\begin{lstlisting}[language=Python]
def gcd(a, b):
    while b:
        a, b = b, a % b
    return a
\end{lstlisting}

Time complexity: $O(\log \min(a, b))$
\end{solution}
```

---

## Compilation Performance

### Build Times

| Phase | Time | Notes |
|-------|------|-------|
| First xelatex pass | 2-3 minutes | Processes all content |
| biber (bibliography) | 30-60 seconds | Generates citations |
| Second xelatex pass | 1-2 minutes | Updates references |
| Third xelatex pass | 30-45 seconds | Finalizes TOC, indices |
| **Total** | **4-6 minutes** | From clean state |
| Quick rebuild | 30-45 seconds | After minor changes |

### Memory Requirements

- **RAM**: 512 MB minimum, 1 GB recommended
- **Disk**: 500 MB for compilation artifacts
- **Processing**: Single-threaded (xelatex doesn't parallelize)

---

## Output Quality Targets

### PDF Specifications

**Format**: PDF 1.5 (compatible with all readers)
**Compression**: Gzip for text, JPEG for images
**Size Target**: 100-150 MB (uncompressed in memory)
**Page Count**: 3,000-5,000 pages

### Typography Standards

- **Font**: Linux Libertine (default serif)
- **Monospace**: Inconsolata (code blocks)
- **Line Spacing**: 1.5 (readability)
- **Margins**: 1 inch (standard academic)
- **Widow/Orphan Control**: Active (no isolated lines)

### Accessibility

- **Bookmarks**: Full hierarchical PDF bookmarks
- **Links**: All citations and cross-references clickable
- **Color**: Accessible palette (no red/green alone for distinction)
- **OCR**: Embedded text searchable

---

## Continuous Integration

### GitHub Actions Workflow

**File**: `.github/workflows/docs.yml`

```yaml
name: Build Documentation

on:
  push:
    paths:
      - 'docs/**'
      - '.github/workflows/docs.yml'

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Install LaTeX
        run: |
          apt-get update
          apt-get install -y texlive-full

      - name: Build PDF
        working-directory: docs
        run: make build

      - name: Verify compilation
        working-directory: docs
        run: make verify

      - name: Upload artifact
        uses: actions/upload-artifact@v3
        with:
          name: ancient-compute-curriculum
          path: docs/main.pdf
```

---

## Common Issues and Solutions

### Issue: "! Package tikz Error: I cannot find the file `tikzlibraries.tex`"

**Cause**: TikZ not properly installed

**Solution**:
```bash
# Linux
apt-get install texlive-pictures texlive-fonts-recommended

# macOS
brew install basictex && tlmgr install tikz pgfplots

# Windows
MiKTeX Console → Package Manager → Search "tikz" → Install
```

### Issue: "! Undefined control sequence \foreach"

**Cause**: PGF library not loaded

**Solution**: Add to preamble:
```latex
\usepackage{tikz}
\usetikzlibrary{shapes,arrows,positioning,shadows,calc}
```

### Issue: PDF file is very large (> 500 MB)

**Cause**: Uncompressed images or inefficient TikZ

**Solution**:
1. Compress images: `convert image.png -quality 85 image-compressed.png`
2. Optimize TikZ: Use `declare function` instead of plotting points
3. Use `\pdfsuppressfontwarnings` if appropriate

### Issue: "! Mismatched environment \begin{...} \end{...}"

**Cause**: Unbalanced LaTeX blocks

**Solution**: Check all environments are properly closed:
```latex
\begin{tikzpicture}  % Must have matching \end
...
\end{tikzpicture}
```

---

## Advanced Features

### Automatic Index Generation

**Preamble**:
```latex
\usepackage{makeidx}
\makeindex
```

**Main document**:
```latex
% Mark entries for indexing
\index{Turing Machine}
\index{Lambda Calculus!Reduction}

% End of document
\printindex
```

### Conditional Content

```latex
% Include solutions only in instructor version
\usepackage{etoolbox}

\newcommand{\ifinstructor}{true}

\ifdefstring{\ifinstructor}{true}{
  % Instructor-only content
  \include{solutions/all-solutions}
}{}
```

---

## References

- **LaTeX**: https://www.latex-project.org
- **XeLaTeX**: https://tug.org/xetex/
- **TikZ/PGF**: https://sourceforge.net/projects/pgf/files/
- **BibLaTeX**: https://ctan.org/pkg/biblatex
- **pgfplots**: https://ctan.org/pkg/pgfplots

---

**End of Documentation Requirements**
