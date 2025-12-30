#!/usr/bin/env python3
import time, statistics, json
ops = ['NOP','ADD','SUB','MULT','DIV','SQRT','LOAD','STOR','JMP']
runs = 30
warmup = 3
results = {}
for op in ops:
    times = []
    for i in range(runs):
        t0 = time.perf_counter()
        # Simulate via TIMING_TABLE seconds equivalent (from docs); replace with real call if available
        simulated = {'NOP':0,'ADD':8,'SUB':8,'MULT':400,'DIV':750,'SQRT':250,'LOAD':15,'STOR':15,'JMP':4}[op]
        time.sleep(0.0001)  # low overhead placeholder
        t1 = time.perf_counter()
        times.append(t1-t0)
    times = times[warmup:]
    results[op] = {
        'runs': len(times),
        'min': min(times),
        'max': max(times),
        'avg': sum(times)/len(times),
        'stddev': statistics.pstdev(times)
    }
print('op,runs,min,max,avg,stddev')
for op in ops:
    r = results[op]
    print(f"{op},{r['runs']},{r['min']:.6f},{r['max']:.6f},{r['avg']:.6f},{r['stddev']:.6f}")
