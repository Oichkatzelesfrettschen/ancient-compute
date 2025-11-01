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

from .liveness import LivenessAnalyzer
from .regalloc import LinearScanAllocator, AllocationMap
from .selector import InstructionSelector
from .emitter import CodeEmitter
from .codegen import CodeGenerator

__all__ = [
    "LivenessAnalyzer",
    "LinearScanAllocator",
    "AllocationMap",
    "InstructionSelector",
    "CodeEmitter",
    "CodeGenerator",
]
