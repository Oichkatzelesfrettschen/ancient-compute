# Babbage Whitepaper: Compilation Pipeline

## Overview

This document describes the complete compilation pipeline for the Babbage Analytical Engine arxiv-style whitepaper. The pipeline synthesizes modular LaTeX files, TikZ diagrams, pgfplots data, and bibliography into a publication-ready PDF document compatible with arxiv.org submission.

## System Requirements

### LaTeX Distribution

- **TeX Live 2023+** (recommended) or **MiKTeX 22.1+**
- **PDFLaTeX** (required for arxiv compatibility)
- **Bash** or compatible shell (for pipeline automation)

### Essential Packages

All required packages are included in standard TeX Live distribution:

```
amsmath, amssymb, mathtools
geometry, setspace
booktabs, array, multirow, tabularx
graphicx, tikz, pgfplots, pgfplotstable
hyperref, cleveref, natbib
listings, xcolor
titlesec, caption
```

### Build Tools

- `pdflatex` (main compiler)
- `bibtex` (bibliography processing)
- `make` (optional, for automated builds)
- `latexmk` (optional, for intelligent recompilation)

## Directory Structure

```
whitepaper-arxiv/
├── babbage-whitepaper.tex       (main document)
├── babbage-preamble.sty         (custom LaTeX package)
├── references.bib               (bibliography database)
├── COMPILATION.md               (this file)
├── diagrams/                    (TikZ diagram sources)
│   ├── 01-system-architecture.tikz
│   ├── 02-carry-mechanism.tikz
│   └── 03-store-organization.tikz
├── sections/                    (modular content sections)
│   ├── unix-mapping.tex
│   ├── manufacturing.tex
│   └── cost-analysis.tex
├── appendices/                  (supplementary material)
│   ├── instruction-details.tex
│   ├── diagram-gallery.tex
│   ├── performance-graphs.tex
│   └── historical-sources.tex
├── data/                        (pgfplots data files)
│   ├── cost-analysis.csv
│   ├── operation-timing.csv
│   └── labour-breakdown.csv
└── build/                       (output directory, generated)
    ├── babbage-whitepaper.pdf   (final output)
    ├── babbage-whitepaper.aux
    ├── babbage-whitepaper.bbl
    └── [intermediate files]
```

## Compilation Steps

### Method 1: Direct PDFLaTeX (Sequential)

Standard compilation with bibliography:

```bash
cd whitepaper-arxiv/

# Step 1: First LaTeX pass (generate aux files)
pdflatex -interaction=nonstopmode babbage-whitepaper.tex

# Step 2: Process bibliography
bibtex babbage-whitepaper

# Step 3: Second LaTeX pass (incorporate citations)
pdflatex -interaction=nonstopmode babbage-whitepaper.tex

# Step 4: Third LaTeX pass (resolve cross-references)
pdflatex -interaction=nonstopmode babbage-whitepaper.tex
```

**Output**: `babbage-whitepaper.pdf`

### Method 2: Automated with Makefile

Create `Makefile` in whitepaper-arxiv/ directory:

```makefile
# Babbage Whitepaper Build System

MAIN = babbage-whitepaper
LATEX = pdflatex
BIBTEX = bibtex
FLAGS = -interaction=nonstopmode

.PHONY: all clean pdf view

all: pdf

pdf: $(MAIN).pdf

$(MAIN).pdf: $(MAIN).tex $(MAIN).bbl
	$(LATEX) $(FLAGS) $(MAIN).tex

$(MAIN).bbl: $(MAIN).aux references.bib
	$(BIBTEX) $(MAIN)

$(MAIN).aux: $(MAIN).tex
	$(LATEX) $(FLAGS) $(MAIN).tex

view: $(MAIN).pdf
	xdg-open $(MAIN).pdf &

clean:
	rm -f $(MAIN).aux $(MAIN).bbl $(MAIN).blg
	rm -f $(MAIN).log $(MAIN).out $(MAIN).toc
	rm -f $(MAIN).fls $(MAIN).fdb_latexmk
	rm -f *~

distclean: clean
	rm -f $(MAIN).pdf

.SILENT: view
```

Then build with:

```bash
make all      # Full compilation
make view     # Compile and open in viewer
make clean    # Remove intermediate files
make distclean # Clean everything including PDF
```

### Method 3: Intelligent Recompilation (latexmk)

If `latexmk` is installed:

```bash
latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" babbage-whitepaper.tex
```

This automatically detects changed files and recompiles only necessary passes.

## Troubleshooting

### Common Issues and Solutions

#### Issue 1: "File not found: babbage-preamble.sty"

**Cause**: Package file in wrong location or LaTeX search path doesn't include current directory.

**Solution**:
```bash
# Option A: Add current directory to TEXINPUTS
export TEXINPUTS=.:$TEXINPUTS
pdflatex babbage-whitepaper.tex

# Option B: Copy .sty file to system location (requires sudo)
sudo cp babbage-preamble.sty /usr/share/texmf/tex/latex/custom/
mktexlsr
```

#### Issue 2: "Undefined reference to [citation]"

**Cause**: Bibliography not processed, or missing reference in .bib file.

**Solution**:
```bash
bibtex babbage-whitepaper    # Process bibliography
pdflatex babbage-whitepaper.tex  # Recompile with citations
pdflatex babbage-whitepaper.tex  # Final pass for references
```

#### Issue 3: TikZ diagrams not rendering ("File not found: diagrams/XX.tikz")

**Cause**: Diagram file path incorrect or file missing.

**Solution**:
```bash
# Check file exists
ls -la diagrams/01-system-architecture.tikz

# Verify path in babbage-whitepaper.tex
grep "input{diagrams" babbage-whitepaper.tex

# If path wrong, correct in main document
# \input{diagrams/01-system-architecture.tikz}
```

#### Issue 4: pgfplots data not loading ("File not found: data/cost-analysis.csv")

**Cause**: Data file path incorrect or CSV file missing.

**Solution**:
```bash
# Check file exists
ls -la data/*.csv

# Verify path in appendix files
grep "pgfplotstable" appendices/performance-graphs.tex

# Ensure relative paths are correct from main .tex location
```

#### Issue 5: "Illegal parameter number" or "Extra \}"

**Cause**: LaTeX syntax error in .tex file (often in TikZ code).

**Solution**:
```bash
# Run pdflatex with verbose error output
pdflatex -halt-on-error babbage-whitepaper.tex

# Check line number in error message
# Review that section of code for unmatched braces/brackets
```

## Compilation Performance

Typical build times on modern systems:

| Method | Time | Notes |
|--------|------|-------|
| First pass (pdflatex) | 3--5s | Generates aux files |
| Bibliography (bibtex) | 0.5--1s | Parses .bib file |
| Second pass (pdflatex) | 3--5s | Includes citations |
| Third pass (pdflatex) | 2--4s | Resolves references |
| **Total (cold build)** | **~10--15s** | Initial compilation |
| **Incremental rebuild** | **3--5s** | After minor changes |

On slower systems (older laptop, virtual machine): multiply by 1.5--3×.

## arXiv Submission Checklist

Before submitting to arxiv.org, ensure:

### Files Included

- [ ] `babbage-whitepaper.tex` (main document)
- [ ] `babbage-preamble.sty` (package file)
- [ ] `references.bib` (bibliography)
- [ ] All diagram files in `diagrams/` subdirectory
- [ ] All section files in `sections/` subdirectory
- [ ] All data files in `data/` subdirectory (if referenced)
- [ ] `COMPILATION.md` or `README.txt` (recommended)

### PDF Format

- [ ] PDF generated with PDFLaTeX (not XeLaTeX or LuaLaTeX)
- [ ] PDF contains embedded fonts (check: `pdffonts babbage-whitepaper.pdf`)
- [ ] All figures are PDF, PNG, or JPEG (not EPS or PS)
- [ ] Figure filenames use only: a-z, A-Z, 0-9, _, -, ., =, +, ,
- [ ] Figure references match case exactly (e.g., `Fig.pdf` ≠ `fig.pdf`)

### Content

- [ ] No DVI, PS, or PDF-from-DVI files
- [ ] No scanned documents or images
- [ ] Bibliography processed with bibtex (not hand-written)
- [ ] All cross-references resolved (no "??" in PDF)
- [ ] Page numbering correct
- [ ] Margins acceptable (> 1 inch)

### Create arXiv Submission Package

```bash
# Create clean submission archive
mkdir -p arxiv-submission

# Copy essential files
cp babbage-whitepaper.tex arxiv-submission/
cp babbage-preamble.sty arxiv-submission/
cp references.bib arxiv-submission/
cp -r diagrams/ arxiv-submission/
cp -r sections/ arxiv-submission/
cp -r data/ arxiv-submission/

# Create README
cat > arxiv-submission/README << 'EOF'
Babbage Analytical Engine: A Mechanical Unix-Like System

Compilation: pdflatex babbage-whitepaper.tex && bibtex babbage-whitepaper && pdflatex babbage-whitepaper.tex && pdflatex babbage-whitepaper.tex

Author: Claude Code
Institution: Ancient Compute Educational Initiative
Date: 2025-10-31
EOF

# Create archive
tar czf babbage-whitepaper-arxiv.tar.gz arxiv-submission/

# Test compile from archive
mkdir test-extract
cd test-extract
tar xzf ../babbage-whitepaper-arxiv.tar.gz
cd arxiv-submission
pdflatex babbage-whitepaper.tex
bibtex babbage-whitepaper
pdflatex babbage-whitepaper.tex
pdflatex babbage-whitepaper.tex
ls -la babbage-whitepaper.pdf
```

## Advanced: Custom Compilation Options

### Compile to DVI (for troubleshooting)

```bash
latex babbage-whitepaper.tex  # Generates .dvi
dvipdf babbage-whitepaper.dvi babbage-whitepaper.pdf
```

**Note**: arXiv does not accept DVI-to-PDF conversion; use PDFLaTeX directly.

### Compile with Draft Mode

```bash
pdflatex -interaction=nonstopmode "\newif\ifdraft\drafttrue\input{babbage-whitepaper.tex}"
```

This skips some heavy graphics for faster preview during development.

### Compile with Debugging

```bash
pdflatex -interaction=errorstopmode babbage-whitepaper.tex
# Stops on first error, allows interactive debugging
```

## Continuous Integration (GitHub Actions Example)

For automated builds on every commit:

```yaml
name: Build Whitepaper PDF

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    
    steps:
    - uses: actions/checkout@v2
    
    - name: Install LaTeX
      run: |
        sudo apt-get update
        sudo apt-get install -y texlive-latex-base texlive-latex-extra
    
    - name: Compile PDF
      run: |
        cd docs/whitepaper-arxiv
        pdflatex -interaction=nonstopmode babbage-whitepaper.tex
        bibtex babbage-whitepaper
        pdflatex -interaction=nonstopmode babbage-whitepaper.tex
        pdflatex -interaction=nonstopmode babbage-whitepaper.tex
    
    - name: Upload Artifact
      uses: actions/upload-artifact@v2
      with:
        name: babbage-whitepaper.pdf
        path: docs/whitepaper-arxiv/babbage-whitepaper.pdf
```

## Performance Optimization Tips

### Faster Compilation During Development

1. **Comment out unused sections**: Temporarily disable appendices while editing main text.
   ```latex
   %\appendix
   %\input{appendices/...}
   ```

2. **Use `\usepackage[draft]{graphicx}`**: Replaces graphics with placeholder boxes (much faster).

3. **Disable hyperlinks during drafting**: Comment out `\usepackage{hyperref}` until final version.

4. **Use `--halt-on-error`**: Stop immediately on first error instead of continuing.

### Faster Bibliography Processing

1. **Use biblatex + biber** (modern alternative):
   ```latex
   \usepackage[backend=biber]{biblatex}
   \addbibresource{references.bib}
   ```
   Then: `pdflatex → biber → pdflatex → pdflatex`

2. **Minimize .bib entries**: Remove unused citations from references.bib.

### Faster TikZ Compilation

TikZ diagrams can be slow. Options:

1. **Use `external` library**: Cache compiled diagrams as PDFs.
   ```latex
   \usetikzlibrary{external}
   \tikzexternalize[prefix=tikz-cache/]
   ```

2. **Compile diagrams separately**: Generate individual PDFs, include as images.
   ```bash
   pdflatex --jobname=diagram-01 "\\documentclass{article}\\begin{document}\\input{diagrams/01-system-architecture.tikz}\\end{document}"
   ```

## File Manifest

| File | Purpose | Required |
|------|---------|----------|
| `babbage-whitepaper.tex` | Main document | ✓ |
| `babbage-preamble.sty` | Package configuration | ✓ |
| `references.bib` | Bibliography database | ✓ |
| `diagrams/*.tikz` | TikZ visualizations | ✓ |
| `sections/*.tex` | Content modules | ✓ |
| `appendices/*.tex` | Supplementary material | ✗ |
| `data/*.csv` | pgfplots data | ✗ (if appendices included) |
| `COMPILATION.md` | This file | ✗ |

## Support and Troubleshooting

### Online Resources

- **TeX Stack Exchange**: https://tex.stackexchange.com
- **CTAN Package Search**: https://ctan.org
- **arXiv TeX Help**: https://info.arxiv.org/help/submit
- **TikZ Manual**: https://tikz.dev
- **PGFPlots Manual**: https://pgfplots.net

### Quick Diagnostic Commands

```bash
# Check LaTeX version
pdflatex --version

# Check installed packages
tlmgr list --only-installed | grep pgfplots

# Verify TeX Live database
mktexlsr

# Check for missing fonts in PDF
pdffonts babbage-whitepaper.pdf

# Validate PDF for arXiv
pdfinfo babbage-whitepaper.pdf
```

---

**Last updated**: 2025-10-31  
**Author**: Claude Code  
**Status**: Complete and tested for arXiv submission
