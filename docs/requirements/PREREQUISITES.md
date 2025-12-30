Prerequisite Packages

Installed and required tools (as of 2025-12-30T17:18:32Z):
- linkchecker (installed via pipx; Python 3.13.11)
  Command: linkchecker
  Manpages: man1/linkchecker.1, man5/linkcheckerrc.5
- markdownlint-cli (installed via pacman: extra/markdownlint-cli 0.47.0-1)
  Command: markdownlint
- markdownlint-cli2 (installed via pacman: extra/markdownlint-cli2 0.18.1-1)
  Command: markdownlint-cli2
- yamllint (recommend install via pacman or pipx)
  Command: yamllint
- aspell and aspell-en (spell checking)
  Commands: aspell, aspell list
- gnuplot (optional, for plotting SVG)
  Command: gnuplot

Notes
- pipx installation of markdownlint-cli failed (no matching distribution); use system package manager instead.
- If linkchecker or yamllint are missing, install via pipx or pacman.

Suggested install commands (Arch-based):
- sudo pacman -S linkchecker markdownlint-cli markdownlint-cli2 yamllint aspell aspell-en gnuplot
- Or: pipx install linkchecker; pipx install yamllint
