Extraction Plan (DE2 Technical Description → sim_schema.yaml)
- Materials: brass/steel specs (density, strength) → materials.*
- Tolerances: gear module/backlash, shaft/bearing clearances → tolerances.*
- Valve timing: lap/lead/cutoff → valve_gear_params.*
- Lubrication: oil type, intervals → lubrication.*
Process
1) Fetch DE2 PDFs to docs/sources/cache (fetch-sources)
2) Convert PDF to text (pdftotext), place alongside
3) Run tools/simulation/extract_params.py to write docs/simulation/extracted.yaml
4) Review and merge into sim_schema.yaml; commit with citations (page refs)
