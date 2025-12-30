#!/usr/bin/env python3
import time, csv, os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import Instruction, Engine, BabbageNumber
runs=200
writer=csv.writer(open('output/emulator_longrun.csv','w',newline=''))
writer.writerow(['run','latency_s'])
for r in range(1,runs+1):
    eng=Engine()
    eng._set_register_value('A', BabbageNumber(123456))
    eng._set_register_value('B', BabbageNumber(7890))
    instrs=[Instruction('ADD',['A','B'])]*100 + [Instruction('SUB',['A','B'])]*100
    t0=time.perf_counter()
    for ins in instrs: eng.execute_instruction(ins)
    t1=time.perf_counter()
    writer.writerow([r,t1-t0])
