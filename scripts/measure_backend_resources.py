#!/usr/bin/env python3
import csv
import os
import time
import urllib.request

import psutil

url = os.environ.get("BACKEND_URL", "http://localhost:8000/docs")
runs = 10
fields = ["run", "latency_s", "cpu_pct", "mem_mb", "ok"]
writer = csv.writer(open("output/backend_metrics.csv", "w", newline=""))
writer.writerow(fields)
for i in range(1, runs + 1):
    t0 = time.perf_counter()
    ok = 1
    try:
        urllib.request.urlopen(url, timeout=5).read(1)
    except Exception:
        ok = 0
    t1 = time.perf_counter()
    cpu = psutil.cpu_percent(interval=0.0)
    mem = psutil.virtual_memory().used / (1024**2)
    writer.writerow([i, t1 - t0, cpu, mem, ok])
