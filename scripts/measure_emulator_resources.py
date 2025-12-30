#!/usr/bin/env python3
import time, statistics, csv, os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import Instruction, Engine, BabbageNumber
ops=['ADD','SUB','MULT','DIV','SQRT']
runs=20
warm=3
writer=csv.writer(open('output/emulator_metrics_real.csv','w',newline=''))
writer.writerow(['op','run','latency_s','cpu_pct','mem_mb'])
for op in ops:
    for i in range(runs):
        eng=Engine()
        # init simple workload
        eng._set_register_value('A', BabbageNumber(12345))
        eng._set_register_value('B', BabbageNumber(6789))
        instr=Instruction(op, ['A','B'] if op in ['ADD','SUB','MULT','DIV'] else ['A'])
        t0=time.perf_counter()
        eng.execute_instruction(instr)
        t1=time.perf_counter()
        cpu=psutil.cpu_percent(interval=0.0)
        mem=float(open('/proc/meminfo').read().split('
')[1].split()[1])/1024
        writer.writerow([op,i+1,t1-t0,cpu,mem])
