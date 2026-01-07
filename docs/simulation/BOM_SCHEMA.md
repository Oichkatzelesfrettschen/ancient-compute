# BOM Schema

Purpose: standardize bill of materials entries for mechanical builds.

Fields
- mechanism: name (e.g., Babbage Analytical Engine)
- subsystem: mill/store/barrel/I-O
- part_id: stable identifier
- part_name: human-readable name
- material: brass/steel/wood/etc
- qty: integer
- dimensions: LxWxH (mm)
- tolerance: +/- mm
- fabrication: cast/milled/printed
- supplier: name or TBD
- unit_cost_gbp: numeric
- notes: free text

Format
- CSV for bulk entry
- YAML for annotated lists
