#!/usr/bin/env python3
import time, csv, os, urllib.request
url=os.environ.get('BACKEND_URL','http://localhost:8000/docs')
runs=10
proc=None
fields=['run','latency_s','cpu_pct','mem_mb','ok']
writer=csv.writer(open('output/backend_metrics.csv','w',newline=''))
writer.writerow(fields)
for i in range(1,runs+1):
    t0=time.perf_counter()
    ok=1
    try:
        import urllib.request
        urllib.request.urlopen(url, timeout=5).read(1)
    except Exception:
        ok=0
    t1=time.perf_counter()
    cpu=0.0
    mem=float(open('/proc/meminfo').read().split('
')[1].split()[1])/1024
    writer.writerow([i, t1-t0, cpu, mem, ok])
