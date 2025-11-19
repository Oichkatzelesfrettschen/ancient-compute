# Babbage Whitepaper Build System
# Automated compilation, deployment, and maintenance

MAIN = babbage-whitepaper
LATEX = pdflatex
BIBTEX = bibtex
LATEXMK = latexmk
VIEWER = xdg-open

# Compilation flags
LATEX_FLAGS = -interaction=nonstopmode -halt-on-error
LATEXMK_FLAGS = -pdf -pdflatex="$(LATEX) $(LATEX_FLAGS)"

# Output directories
BUILD_DIR = build
DIST_DIR = dist
ARXIV_DIR = arxiv-submission

# Source files
TEX_MAIN = $(MAIN).tex
TEX_PREAMBLE = babbage-preamble.sty
BIB_FILE = references.bib
DIAGRAM_SRCS = $(wildcard diagrams/*.tikz)
SECTION_SRCS = $(wildcard sections/*.tex)
APPENDIX_SRCS = $(wildcard appendices/*.tex)
DATA_FILES = $(wildcard data/*.csv)

# Generated files
PDF_OUTPUT = $(MAIN).pdf
AUX_FILES = $(MAIN).aux $(MAIN).bbl $(MAIN).blg $(MAIN).log $(MAIN).out $(MAIN).toc
FLS_FILES = $(MAIN).fls $(MAIN).fdb_latexmk

# Phony targets
.PHONY: all pdf view clean distclean arxiv help check deps

# Default target
all: pdf

# Build PDF (primary target)
pdf: $(PDF_OUTPUT)

# Main PDF target with dependency chain
$(PDF_OUTPUT): $(TEX_MAIN) $(TEX_PREAMBLE) $(BIB_FILE) $(DIAGRAM_SRCS) $(SECTION_SRCS) $(APPENDIX_SRCS) $(DATA_FILES)
	@echo "=== Building $(PDF_OUTPUT) ==="
	@echo "Pass 1: Initial LaTeX compilation..."
	$(LATEX) $(LATEX_FLAGS) $(TEX_MAIN) > /dev/null 2>&1 || $(LATEX) $(LATEX_FLAGS) $(TEX_MAIN)
	@echo "Pass 2: Bibliography processing..."
	$(BIBTEX) $(MAIN) > /dev/null 2>&1 || $(BIBTEX) $(MAIN)
	@echo "Pass 3: Citation incorporation..."
	$(LATEX) $(LATEX_FLAGS) $(TEX_MAIN) > /dev/null 2>&1 || $(LATEX) $(LATEX_FLAGS) $(TEX_MAIN)
	@echo "Pass 4: Final reference resolution..."
	$(LATEX) $(LATEX_FLAGS) $(TEX_MAIN) > /dev/null 2>&1 || $(LATEX) $(LATEX_FLAGS) $(TEX_MAIN)
	@echo "✓ $(PDF_OUTPUT) successfully built"
	@ls -lh $(PDF_OUTPUT)

# Fast rebuild using latexmk (incremental compilation)
fast: $(TEX_MAIN) $(TEX_PREAMBLE) $(BIB_FILE)
	@echo "=== Fast incremental build (latexmk) ==="
	$(LATEXMK) $(LATEXMK_FLAGS) $(TEX_MAIN)
	@echo "✓ Build complete"

# View in PDF viewer
view: pdf
	@echo "Opening $(PDF_OUTPUT) in viewer..."
	@$(VIEWER) $(PDF_OUTPUT) &
	@echo "✓ PDF opened"

# Create arxiv submission package
arxiv: pdf
	@echo "=== Creating arXiv submission package ==="
	@mkdir -p $(ARXIV_DIR)
	@cp $(TEX_MAIN) $(ARXIV_DIR)/
	@cp $(TEX_PREAMBLE) $(ARXIV_DIR)/
	@cp $(BIB_FILE) $(ARXIV_DIR)/
	@cp -r diagrams $(ARXIV_DIR)/ || true
	@cp -r sections $(ARXIV_DIR)/ || true
	@cp -r appendices $(ARXIV_DIR)/ || true
	@cp -r data $(ARXIV_DIR)/ || true
	@cp README.md $(ARXIV_DIR)/
	@cp COMPILATION.md $(ARXIV_DIR)/
	@echo "% arXiv submission package" > $(ARXIV_DIR)/00-README.txt
	@echo "% Generated: $$(date)" >> $(ARXIV_DIR)/00-README.txt
	@echo "" >> $(ARXIV_DIR)/00-README.txt
	@echo "BABBAGE ANALYTICAL ENGINE: A MECHANICAL UNIX-LIKE SYSTEM" >> $(ARXIV_DIR)/00-README.txt
	@echo "" >> $(ARXIV_DIR)/00-README.txt
	@echo "Compilation: pdflatex babbage-whitepaper.tex && bibtex babbage-whitepaper && pdflatex babbage-whitepaper.tex && pdflatex babbage-whitepaper.tex" >> $(ARXIV_DIR)/00-README.txt
	@tar czf $(ARXIV_DIR)-$$(date +%Y%m%d).tar.gz $(ARXIV_DIR)/
	@echo "✓ arXiv package created: $(ARXIV_DIR)-$$(date +%Y%m%d).tar.gz"
	@ls -lh $(ARXIV_DIR)-*.tar.gz

# Verify compilation (test-compile from scratch)
verify: clean
	@echo "=== Verification: Clean rebuild ==="
	@rm -rf $(ARXIV_DIR)
	$(MAKE) pdf
	@echo "✓ Verification successful"

# Check for missing dependencies
check:
	@echo "=== Checking LaTeX environment ==="
	@command -v $(LATEX) > /dev/null || (echo "ERROR: $(LATEX) not found"; exit 1)
	@echo "✓ $(LATEX) found: $$($(LATEX) --version | head -1)"
	@command -v $(BIBTEX) > /dev/null || (echo "ERROR: $(BIBTEX) not found"; exit 1)
	@echo "✓ $(BIBTEX) found: $$($(BIBTEX) --version | head -1)"
	@echo "✓ All dependencies satisfied"

# List dependencies (files that trigger rebuild)
deps:
	@echo "=== Source file dependencies ==="
	@echo "Main document: $(TEX_MAIN)"
	@echo "Preamble: $(TEX_PREAMBLE)"
	@echo "Bibliography: $(BIB_FILE)"
	@echo "Diagrams: $(DIAGRAM_SRCS)"
	@echo "Sections: $(SECTION_SRCS)"
	@echo "Appendices: $(APPENDIX_SRCS)"
	@echo "Data files: $(DATA_FILES)"

# Clean intermediate files
clean:
	@echo "=== Cleaning intermediate files ==="
	@rm -f $(AUX_FILES) $(FLS_FILES)
	@rm -f *~
	@echo "✓ Intermediate files removed"

# Deep clean (remove PDF and all intermediates)
distclean: clean
	@echo "=== Deep clean (removing PDF) ==="
	@rm -f $(PDF_OUTPUT)
	@rm -rf $(BUILD_DIR)
	@echo "✓ All build artifacts removed"

# Ultra-clean (remove everything including arxiv packages)
ultraclean: distclean
	@echo "=== Ultra-clean (removing arxiv packages) ==="
	@rm -rf $(ARXIV_DIR)
	@rm -f $(ARXIV_DIR)-*.tar.gz
	@echo "✓ All generated files removed"

# Help target
help:
	@echo "Babbage Whitepaper Build System"
	@echo "================================"
	@echo ""
	@echo "Targets:"
	@echo "  make all          - Full PDF compilation (default)"
	@echo "  make pdf          - Build PDF only"
	@echo "  make view         - Build and open in viewer"
	@echo "  make fast         - Incremental build (faster, requires latexmk)"
	@echo "  make arxiv        - Create arXiv submission package"
	@echo "  make verify       - Clean rebuild for verification"
	@echo "  make check        - Check for required dependencies"
	@echo "  make deps         - List source file dependencies"
	@echo "  make clean        - Remove intermediate files"
	@echo "  make distclean    - Remove PDF and intermediates"
	@echo "  make ultraclean   - Remove all generated files"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo "Usage examples:"
	@echo "  make              # Build PDF with full compilation"
	@echo "  make view         # Build and open in default viewer"
	@echo "  make fast         # Incremental build (after small edits)"
	@echo "  make arxiv        # Prepare for arxiv.org submission"
	@echo "  make clean        # Clean up before committing to git"

# Silent targets (no output)
.SILENT: view
