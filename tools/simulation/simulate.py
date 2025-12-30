#!/usr/bin/env python3
import sys

def list_opcodes(path):
    in_ops=False
    ops=[]
    with open(path,'r') as f:
        for line in f:
            if line.strip()=='opcodes:':
                in_ops=True
                continue
            if in_ops:
                if line and not line.startswith(' '):
                    break
                ls=line.strip()
                if ls and ':' in ls and not ls.startswith('#'):
                    key=ls.split(':',1)[0]
                    if key.isupper():
                        ops.append(key)
    return ops

if __name__ == '__main__':
    path = sys.argv[1] if len(sys.argv)>1 else 'docs/simulation/sim_schema.yaml'
    ops = list_opcodes(path)
    print('Loaded schema; opcodes:')
    for k in ops:
        print(k)
