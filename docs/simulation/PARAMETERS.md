Extraction: run make fetch-sources; pdftotext; make sim-extract; review docs/simulation/extracted.yaml
Cleanroom Parameter Extraction (mapping)
- Materials (density, friction): derive from 19th-c. handbooks and SM DE2 Technical Description
- Valve timing (lap/lead/cutoff): from Stephenson gear references; map to stroke and rpm
- Gear train ratios/efficiency: from drawings and DE2 description
- Bearings/lubrication: DE2 user manual and period treatises
- Card feed rates and jam probability: Science Museum operational notes
Validation
- Compare simulated cycles per opcode vs TIMING_TABLE and SM sources
