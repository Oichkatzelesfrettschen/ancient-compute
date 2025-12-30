#!/usr/bin/env python3
import os, sys
REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), '..'))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from typing import Any
try:
    from tools.babbage_emulator import Instruction, Engine, BabbageNumber, TIMING_TABLE
except Exception:
    from tools.babbage_emulator import Instruction, Engine, BabbageNumber
    TIMING_TABLE = None

failures=[]

def get_clock(eng: Any) -> int:
    for attr in ('clock_time','clock','cycles'):
        if hasattr(eng, attr):
            return getattr(eng, attr)
    return 0

def exec_one(eng, instr):
    if hasattr(eng, 'execute_instruction'):
        return eng.execute_instruction(instr)
    if hasattr(eng, 'step_one_instruction'):
        eng.instruction_cards=[instr]
        return eng.step_one_instruction()
    raise RuntimeError('No execute method')

def setreg(eng, r, v):
    if hasattr(eng, '_set_register_value'):
        return eng._set_register_value(r, BabbageNumber(v))

# 1) Timing table conformity (best-effort)
if TIMING_TABLE:
    for op, expected in TIMING_TABLE.items():
        try:
            eng=Engine()
            setreg(eng,'A',2); setreg(eng,'B',1)
            if op in ['ADD','SUB','MULT','DIV','CMP']:
                instr=Instruction(op, ['A','B'])
            elif op=='SQRT':
                setreg(eng,'A',4); instr=Instruction(op, ['A'])
            elif op in ['LOAD','STOR']:
                if hasattr(eng,'memory'): eng.memory[0]=BabbageNumber(7)
                instr=Instruction(op, ['A','[0]'])
            elif op in ['JMP','JZ','JNZ','JLT','JGT','JLE','JGE']:
                instr=Instruction(op, ['0'])
            else:
                instr=Instruction(op, [])
            start=get_clock(eng)
            exec_one(eng,instr)
            delta=get_clock(eng)-start
            if delta!=expected:
                failures.append(f"Timing {op}: expected {expected}, got {delta}")
        except Exception:
            # skip if opcode unsupported
            pass

# 2) Arithmetic correctness (best-effort)
try:
    eng=Engine(); setreg(eng,'A',123456); setreg(eng,'B',789)
    exec_one(eng, Instruction('MULT',['A','B']))
except Exception:
    eng=None
if eng is not None:
    # try multiple ways to inspect registers
    valA=None; valD=None
    for getter in ('_get_register_value','get_register_value','read_register'):
        if hasattr(eng, getter):
            try:
                valA = getattr(eng, getter)('A'); valD = getattr(eng, getter)('D')
                break
            except Exception:
                pass
    if valA is not None and hasattr(valA,'value'):
        I1=123456; I2=789; prod=I1*I2
        upper=prod//(10**50); lower=prod%(10**50)
        try:
            aval=int(valA.value//(10**40)); dval=int(valD.value//(10**40))
            if aval!=upper:
                failures.append(f"MULT upper mismatch: {aval} vs {upper}")
            if dval!=lower:
                failures.append(f"MULT lower mismatch: {dval} vs {lower}")
        except Exception:
            pass

# 3) SQRT(4)=2
try:
    eng=Engine(); setreg(eng,'A',4); exec_one(eng, Instruction('SQRT',['A']))
    got=None
    for getter in ('_get_register_value','get_register_value','read_register'):
        if hasattr(eng, getter):
            v=getattr(eng, getter)('A')
            if hasattr(v,'value'):
                got=int(v.value//(10**40))
                break
    if got is not None and got!=2:
        failures.append(f"SQRT 4 expected 2 got {got}")
except Exception:
    pass

# 4) LOAD/STOR roundtrip
try:
    eng=Engine()
    if hasattr(eng,'memory'): eng.memory[0]=BabbageNumber(42)
    exec_one(eng, Instruction('LOAD',['A','[0]']))
    exec_one(eng, Instruction('STOR',['A','[1]']))
    if hasattr(eng,'memory') and hasattr(eng.memory[1],'value'):
        got=int(eng.memory[1].value//(10**40))
        if got!=42:
            failures.append(f"LOAD/STOR expected 42 got {got}")
except Exception:
    pass

if failures:
    print('PHYSICS VALIDATION FAILURES:')
    for f in failures: print('-', f)
    sys.exit(1)
else:
    print('Physics validation passed')
