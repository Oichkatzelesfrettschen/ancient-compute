#!/usr/bin/env python3
import os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
try:
    from tools.babbage_emulator import Instruction, Engine, BabbageNumber
except Exception as e:
    print('error: cannot import emulator:', e)
    raise

def exec_one(eng, instr):
    if hasattr(eng, 'execute_instruction'):
        return eng.execute_instruction(instr)
    if hasattr(eng, 'step_one_instruction'):
        eng.instruction_cards = [instr]
        return eng.step_one_instruction()
    raise RuntimeError('No known execute method on Engine')

def get_clock(eng):
    if hasattr(eng, 'clock_time'):
        return eng.clock_time
    if hasattr(eng, 'clock'):
        return eng.clock
    if hasattr(eng, 'cycles'):
        return eng.cycles
    return 0

ops=['NOP','ADD','SUB','MULT','DIV','SQRT','LOAD','STOR','JMP']
print('op,engine_clock_delta')
for op in ops:
    eng=Engine()
    # initialize
    if hasattr(eng, '_set_register_value'):
        setreg = lambda r,v: eng._set_register_value(r, BabbageNumber(v))
    else:
        setreg = lambda r,v: None
    try:
        setreg('A', 2)
        setreg('B', 1)
        if op in ['ADD','SUB','MULT','DIV']:
            instr=Instruction(op, ['A','B'])
        elif op=='SQRT':
            setreg('A', 4)
            instr=Instruction(op, ['A'])
        elif op=='LOAD':
            if hasattr(eng, 'memory'):
                eng.memory[0]=BabbageNumber(7)
            instr=Instruction(op, ['A','[0]'])
        elif op=='STOR':
            instr=Instruction(op, ['A','[0]'])
        elif op=='JMP':
            instr=Instruction(op, ['0'])
        else:
            instr=Instruction(op, [])
        start=get_clock(eng)
        exec_one(eng, instr)
        delta=get_clock(eng)-start
        print(f"{op},{delta}")
    except Exception as e:
        print(f"{op},error:{e}")
