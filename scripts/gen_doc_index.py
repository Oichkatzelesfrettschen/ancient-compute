#!/usr/bin/env python3
import os
base='docs'
print('Documentation Index')
for root,dirs,files in os.walk(base):
    for f in files:
        if f.endswith('.md') or f.endswith('.yaml'):
            p=os.path.join(root,f)
            print('-', p)
