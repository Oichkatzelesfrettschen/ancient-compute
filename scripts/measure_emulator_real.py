#!/usr/bin/env python3
import time, statistics, os, sys
# Ensure repo root on path
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import Instruction, Engine, BabbageNumber

def make_instruction(op):
    # Prepare a safe single-instruction with initialized state
    if op in ['ADD','SUB','MULT','DIV','CMP']:
        instr = Instruction(op, ['A','B'])
        init = lambda eng: (
            eng._set_register_value('A', BabbageNumber(2)),
            eng._set_register_value('B', BabbageNumber(1))
        )
    elif op == 'SQRT':
        instr = Instruction(op, ['A'])
        init = lambda eng: eng._set_register_value('A', BabbageNumber(4))
    elif op == 'LOAD':
        instr = Instruction(op, ['A','[0]'])
        init = lambda eng: eng.memory.__setitem__(0, BabbageNumber(7))
    elif op == 'STOR':
        instr = Instruction(op, ['A','[0]'])
        init = lambda eng: eng._set_register_value('A', BabbageNumber(3))
    elif op in ['JMP','JZ','JNZ','JLT','JGT','JLE','JGE']:
        # Jump to address 0; conditionals will not jump since ZERO flag false
        instr = Instruction(op, ['0']) if op == 'JMP' else Instruction(op, ['0'])
        init = lambda eng: None
    else:
        instr = Instruction(op, [])
        init = lambda eng: None
    return instr, init

def measure_op(opcode, runs=30, warmup=3):
    times = []
    for i in range(runs):
        eng = Engine()
        instr, init = make_instruction(opcode)
        eng.instruction_cards = [instr]
        init(eng)
        t0 = time.perf_counter()
        eng.execute_instruction(instr)
        t1 = time.perf_counter()
        times.append(t1 - t0)
    times = times[warmup:]
    times.sort()
    p50 = times[len(times)//2]
    p95 = times[int(0.95*len(times))-1]
    p99 = times[int(0.99*len(times))-1]
    return {
        'runs': len(times),
        'min': min(times),
        'max': max(times),
        'avg': sum(times)/len(times),
        'stddev': statistics.pstdev(times),
        'p50': p50,
        'p95': p95,
        'p99': p99,
    }

ops = ['NOP','ADD','SUB','MULT','DIV','SQRT','LOAD','STOR','JMP']
print('op,runs,min,max,avg,stddev,p50,p95,p99')
for op in ops:
    r = measure_op(op)
    print(f"{op},{r['runs']},{r['min']:.6f},{r['max']:.6f},{r['avg']:.6f},{r['stddev']:.6f},{r['p50']:.6f},{r['p95']:.6f},{r['p99']:.6f}")
