Documentation Architect Execution Plan
- Fetch targets: Science Museum Babbage drawings, British Library/Royal Society archives, Menabrea (1842) and Lovelace (1843) scans, 19th-c. piston/valve gear references.
- Cleanroom reconstruction: materials (brass/steel), tolerances, lubrication model, gear train losses, piston/valve timing simulation.
- Physics model: rigid body + contact friction, fluid dynamics for steam drive, clocking model per TIMING_TABLE.
- Gaps to fill: exact valve gear and drive mapping, bearing friction coefficients, card feed dynamics, error rates.
- Steps: (1) curate sources; (2) extract parameters; (3) build sim YAML schema; (4) implement R/Python hybrid scripts; (5) validate vs timing; (6) iterate.
