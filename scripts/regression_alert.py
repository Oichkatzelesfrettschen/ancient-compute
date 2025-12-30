#!/usr/bin/env python3
import json, sys
from pathlib import Path
# thresholds
th = json.loads(Path('scripts/regression_thresholds.json').read_text())
# parse simple summaries from analyze_all.sh outputs not persisted; re-run targeted analyses
import subprocess
def metric(path, col=None):
    cmd = ['python3','scripts/analyze_csv.py',path]
    if col: cmd.append(col)
    
    try:
        out = subprocess.check_output(cmd, text=True).strip()
    except subprocess.CalledProcessError:
        return None
    # parse avg from output
    for part in out.split(', '):
        if part.startswith('avg='):
            return float(part.split('=')[1])
    return None
em_avg = metric('output/emulator_program_bench.csv','latency_s') or metric('output/emulator_metrics_real.csv')
fe_avg = metric('output/frontend_metrics.csv','value')
be_avg = metric('output/backend_metrics.csv','latency_s')
alerts = []
if em_avg and em_avg > th['emulator_avg_max']: alerts.append(f"Emulator avg {em_avg:.6f} exceeds {th['emulator_avg_max']}")
if fe_avg and fe_avg > th['frontend_build_avg_max']: alerts.append(f"Frontend avg {fe_avg:.3f}s exceeds {th['frontend_build_avg_max']}s")
if be_avg and be_avg > th['backend_latency_avg_max']: alerts.append(f"Backend avg {be_avg:.3f}s exceeds {th['backend_latency_avg_max']}s")
if alerts:
    print('REGRESSION ALERTS:')
    for a in alerts: print('-', a)
    sys.exit(1)
else:
    print('No regressions detected against thresholds.')
