#!/usr/bin/env python3
import time, csv, os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import Instruction, Engine, BabbageNumber
programs={
 'arith_loop': [Instruction('LOAD',['A','[0]']), Instruction('LOAD',['B','[1]'])] + [Instruction('ADD',['A','B'])]*50,
 'sqrt_chain': [Instruction('LOAD',['A','[0]'])] + [Instruction('SQRT',['A'])]*10,
 'mult_split': [Instruction('LOAD',['A','[0]']), Instruction('LOAD',['B','[1]']), Instruction('MULT',['A','B'])]
}
runs=10
writer=csv.writer(open('output/emulator_program_bench.csv','w',newline=''))
writer.writerow(['program','run','latency_s'])
for name, instrs in programs.items():
  for r in range(1,runs+1):
    eng=Engine()
    eng.memory[0]=BabbageNumber(123456)
    eng.memory[1]=BabbageNumber(7890)
    t0=time.perf_counter()
    for ins in instrs:
      eng.execute_instruction(ins)
    t1=time.perf_counter()
    writer.writerow([name,r,t1-t0])
