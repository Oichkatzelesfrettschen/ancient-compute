#!/usr/bin/env python3
import sys, csv, statistics
from math import sqrt
if len(sys.argv) < 2:
    print('usage: analyze_csv.py file.csv [value_column_name]')
    sys.exit(1)
path = sys.argv[1]
val_col = sys.argv[2] if len(sys.argv) > 2 else None
vals = []
with open(path, newline='') as f:
    reader = csv.DictReader(f)
    for row in reader:
        # pick first numeric column or specified
        cols = [val_col] if val_col else reader.fieldnames
        for c in cols:
            try:
                v = float(row[c])
                vals.append(v)
                break
            except Exception:
                continue
n = len(vals)
if n == 0:
    print('no numeric data')
    sys.exit(2)
avg = sum(vals)/n
std = statistics.pstdev(vals)
# 95% CI (approx) using normal: avg ± 1.96*std/sqrt(n)
ci = 1.96*std/sqrt(n) if n>1 else 0.0
print(f'n={n}, min={min(vals):.6f}, max={max(vals):.6f}, avg={avg:.6f}, stddev={std:.6f}, ci95=±{ci:.6f}')

# percentiles
ps=sorted(vals)
get=lambda q: ps[int(q*(n-1))]
print(f"p50={get(0.50):.6f}, p95={get(0.95):.6f}, p99={get(0.99):.6f}")

# p999
print(f"p999={get(0.999):.6f}")
