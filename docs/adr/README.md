# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) for the Ancient Compute project.
Each ADR documents a significant architectural choice: the context, the decision, and the
consequences. Format follows Michael Nygard's template.

## Index

| ADR | Title | Status |
|-----|-------|--------|
| [0001](0001-remove-bazel.md) | Remove Bazel, adopt Make + Docker Compose | Accepted |
| [0002](0002-babbage-number-scale.md) | BabbageNumber internal scale (10^40) | Accepted |
| [0003](0003-historical-machines-placement.md) | Historical machines in backend/src/emulator/ | Accepted |
| [0004](0004-simulation-engine-architecture.md) | Simulation engine 3-module split | Accepted |
| [0005](0005-card-compiler-vs-cli-assembler.md) | Card compiler vs CLI assembler -- two separate tools | Accepted |

## Adding a New ADR

1. Copy the structure of an existing ADR.
2. Number sequentially (0006, 0007, ...).
3. Set status to "Proposed" until discussed; change to "Accepted" or "Rejected" after decision.
4. Add a row to the index table above.
5. Reference the ADR in relevant code comments or docs where the decision has impact.

## Status Values

- **Proposed**: Under discussion.
- **Accepted**: Decision made and in effect.
- **Deprecated**: Superseded by a later ADR; kept for history.
- **Rejected**: Considered but not adopted; kept to avoid re-litigating.
