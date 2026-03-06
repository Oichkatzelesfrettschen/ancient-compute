#!/usr/bin/env python3
import csv
import os
import sys
import time

import psutil

REPO_ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
if REPO_ROOT not in sys.path:
    sys.path.insert(0, REPO_ROOT)
from tools.babbage_emulator import BabbageNumber, Engine, Instruction

ops = ["ADD", "SUB", "MULT", "DIV", "SQRT"]
runs = 20
writer = csv.writer(open("output/emulator_metrics_real.csv", "w", newline=""))
writer.writerow(["op", "run", "latency_s", "cpu_pct", "mem_mb"])
for op in ops:
    for i in range(runs):
        eng = Engine()
        eng._set_register_value("A", BabbageNumber(12345))
        eng._set_register_value("B", BabbageNumber(6789))
        instr = Instruction(op, ["A", "B"] if op in ["ADD", "SUB", "MULT", "DIV"] else ["A"])
        t0 = time.perf_counter()
        eng.execute_instruction(instr)
        t1 = time.perf_counter()
        cpu = psutil.cpu_percent(interval=0.0)
        mem = psutil.virtual_memory().used / (1024**2)
        writer.writerow([op, i + 1, t1 - t0, cpu, mem])
