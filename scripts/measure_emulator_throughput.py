#!/usr/bin/env python3
import time, os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import Instruction, Engine, BabbageNumber
ops=['ADD','SUB']
loops=10000
print('op,loops,elapsed_s,ops_per_s')
for op in ops:
    eng=Engine()
    eng._set_register_value('A', BabbageNumber(1))
    eng._set_register_value('B', BabbageNumber(2))
    instr=Instruction(op,['A','B'])
    t0=time.perf_counter()
    for i in range(loops): eng.execute_instruction(instr)
    t1=time.perf_counter()
    dt=t1-t0
    print(f"{op},{loops},{dt:.6f},{loops/dt if dt>0 else 0:.2f}")
