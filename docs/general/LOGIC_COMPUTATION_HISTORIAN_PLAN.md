Logic-Computation-Historian: Repository Modularization and Execution Plan

Scope
- Establish docs/history/, docs/babbage/, docs/speculative/, docs/restoration/; content/modules/, content/synthesis/; tools/emulator/ alignment; archive/ for deprecated.
- Add verification.md, perspective.md, disclaimer.md templates per unit; build curriculum index.

Execution Phases
1) Inventory (Week 1)
- Map existing files to target domains; flag speculative vs primary-source.
- Generate lacunae list (underrepresented regions/topics).

2) Restructure (Week 2)
- Move docs: Babbage primary to docs/babbage/, Unix/process/pipes to docs/speculative/, BOM/machining to docs/restoration/.
- Create docs/history/ with timeline.md, sources.md, verification.md.

3) Curriculum Build (Weeks 3-4)
- content/modules/0-7: outcomes.md, lessons/, exercises/, sources.md, verification.md, perspective.md.
- content/synthesis/: syllogisms-to-types/, abacus-to-assembly/, cross-cultural-algorithms/ with bridges.md and tasks.md.

4) Verification Pipeline (Week 5)
- templates/verification.md: source tiers, claims, status, reviewer.
- Add Makefile targets: make curriculum-index, make verify-history.

5) Emulator Alignment (Week 6)
- tools/babbage_emulator.py: align ISA to documented Analytical Engine; move Unix-like/process features to optional modules; add disclaimer in docs/speculative/.

6) Restoration Tracks (Week 7)
- docs/restoration/: BOM.md, machining_notes.md, manufacturing_tolerances.md, assembly_guides/.
- Add practical build logs and era-appropriate techniques.

7) Speculative Alt-History (Week 8)
- docs/speculative/: clearly labeled extensions (processes, pipes, bitwise ops, Hamming); disclaimers.md; rationale and feasibility.

Lacunae and Remediation
- Africa: tally/astronomy, trading computation; add module sections with sources.
- Indigenous Americas: beyond Mayan calendrics (quipu-like and Nahua counting); exercises.
- India: Ganita traditions, Pingala prosody; primary texts and translations.
- China: Nine Chapters commentaries, rod calculus; transmission analyses.
- Islamic world: algorithmic transmission networks; algebra, astronomy tables.

Deliverables
- Updated repo structure map, curriculum index, verification statuses, emulator spec alignment notes, restoration guide set, speculative module with disclaimers.

Governance
- Historical accuracy gates; reviewer assignments; errata log; no false teleology policy.
