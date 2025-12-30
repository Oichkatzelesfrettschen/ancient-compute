#!/usr/bin/env python3
import re, sys, pathlib, yaml
src_dir = pathlib.Path('docs/sources/cache')
text_files = list(src_dir.glob('*.txt'))
if not text_files:
    print('No text files found in docs/sources/cache; run pdftotext on DE2 PDFs')
    sys.exit(0)
pat_map = {
  'gear_module_mm': r'module\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'backlash_mm': r'backlash\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'shaft_clearance_mm': r'shaft (?:fit|clearance)\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'bearing_clearance_mm': r'bearing (?:fit|clearance)\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'lap_mm': r'lap\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'lead_mm': r'lead\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*mm',
  'cutoff_pct': r'cut-?off\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*%|\b([0-9]{1,2})\s*%\s*cut-?off',
  'viscosity_cSt_40C': r'viscosity\s*[:=]?\s*([0-9]+\.?[0-9]*)\s*cSt',
}
found = {}
for tf in text_files:
    txt = tf.read_text(errors='ignore')
    for k,pat in pat_map.items():
        m = re.search(pat, txt, re.IGNORECASE)
        if m and k not in found:
            val = next(g for g in m.groups() if g)
            try:
                val = float(val)
            except Exception:
                pass
            found[k]=val
out = {'extracted': found, 'source_files': [str(t) for t in text_files]}
pathlib.Path('docs/simulation/extracted.yaml').write_text(yaml.safe_dump(out, sort_keys=True))
print('wrote docs/simulation/extracted.yaml with keys:', ','.join(found.keys()))
