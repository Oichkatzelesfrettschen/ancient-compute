"""
Babbage Code Generator Module

Transforms Babbage Intermediate Representation (IR) to Babbage assembly language.

Components:
  - liveness: Liveness analysis for register allocation
  - regalloc: Linear scan register allocator
  - selector: Instruction selection (IR → Babbage ISA)
  - emitter: Assembly code emission
  - codegen: Main orchestrator
"""

from .codegen import CodeGenerator
from .emitter import CodeEmitter
from .liveness import LivenessAnalyzer
from .regalloc import AllocationMap, LinearScanAllocator
from .selector import InstructionSelector

__all__ = [
    "LivenessAnalyzer",
    "LinearScanAllocator",
    "AllocationMap",
    "InstructionSelector",
    "CodeEmitter",
    "CodeGenerator",
]
