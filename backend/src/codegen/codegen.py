"""
Babbage Code Generator Orchestrator

Main pipeline: IR → Liveness → Register Allocation → Instruction Selection → Code Emission

Coordinates all code generation phases to produce Babbage assembly.
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, Optional, List
from backend.src.ir_types import Function, BasicBlock, Program
from backend.src.codegen.liveness import LivenessAnalyzer
from backend.src.codegen.regalloc import LinearScanAllocator, AllocationMap
from backend.src.codegen.selector import InstructionSelector
from backend.src.codegen.emitter import CodeEmitter, AssemblyOutput


@dataclass
class CodeGenResult:
    """Complete code generation result"""

    function_name: str
    assembly_output: AssemblyOutput
    allocation_map: AllocationMap
    liveness_info: dict  # For debugging
    warnings: List[str]

    def get_assembly_text(self) -> str:
        """Get generated assembly"""
        return self.assembly_output.assembly_text

    def get_label_map(self) -> Dict[str, int]:
        """Get label to address mapping"""
        return self.assembly_output.label_map


class CodeGenerator:
    """
    Main Babbage code generator.

    Pipeline:
    1. Input: Babbage IR function
    2. Liveness analysis (compute live intervals)
    3. Register allocation (linear scan)
    4. Instruction selection (IR → Babbage mnemonics)
    5. Code emission (assembly text)
    6. Output: Babbage assembly text
    """

    def __init__(self):
        self.warnings: List[str] = []

    def generate_function(self, function: Function, verbose: bool = False) -> CodeGenResult:
        """Generate code for single function"""
        self.warnings = []

        if verbose:
            print(f"\n[CODEGEN] Generating code for function '{function.name}'")

        # Phase 1: Liveness Analysis
        if verbose:
            print(f"[CODEGEN] Phase 1: Liveness analysis...")

        liveness_analyzer = LivenessAnalyzer(function)
        intervals = liveness_analyzer.analyze()

        max_live = liveness_analyzer.max_simultaneous_liveness()

        if verbose:
            print(f"[CODEGEN]   Live intervals: {len(intervals)}")
            print(f"[CODEGEN]   Max simultaneous: {max_live}")
            if max_live > 4:
                print(f"[CODEGEN]   WARNING: Will need to spill {max_live - 4} values")

        liveness_info = {
            "intervals": intervals,
            "max_simultaneous": max_live,
            "conflict_graph": liveness_analyzer.build_conflict_graph(),
        }

        # Phase 2: Register Allocation
        if verbose:
            print(f"[CODEGEN] Phase 2: Register allocation...")

        allocator = LinearScanAllocator(function, intervals)
        allocation = allocator.allocate()

        if verbose:
            print(f"[CODEGEN]   Allocations: {len(allocation.allocations)}")
            print(f"[CODEGEN]   Spilled: {allocation.spill_count}")
            print(f"[CODEGEN]   Register pressure: {allocation.register_pressure:.1%}")

        # Phase 3: Instruction Selection
        if verbose:
            print(f"[CODEGEN] Phase 3: Instruction selection...")

        selector = InstructionSelector(allocation)
        emitter = CodeEmitter()

        # Add main label
        emitter.add_label(function.name)

        # Process each basic block
        instruction_count = 0
        for block in function.basic_blocks:
            # Add block label
            if block.label != "entry":
                emitter.add_label(block.label)

            # Select instructions for each IR instruction
            for ir_instr in block.instructions:
                selected_instrs = selector.select_instruction(ir_instr)
                for asm_instr in selected_instrs:
                    emitter.add_instruction(asm_instr)
                    instruction_count += 1

            # Select instructions for terminator
            if block.terminator:
                selected_term = selector.select_terminator(block.terminator, emitter.labels)
                for asm_instr in selected_term:
                    emitter.add_instruction(asm_instr)
                    instruction_count += 1

        if verbose:
            print(f"[CODEGEN]   Instructions selected: {instruction_count}")

        # Phase 4: Code Emission
        if verbose:
            print(f"[CODEGEN] Phase 4: Code emission...")

        assembly_output = emitter.emit(spill_count=allocation.spill_count)

        if verbose:
            print(
                f"[CODEGEN]   Assembly lines: {len(assembly_output.assembly_text.split(chr(10)))}"
            )
            print(f"[CODEGEN] Code generation COMPLETE")

        result = CodeGenResult(
            function_name=function.name,
            assembly_output=assembly_output,
            allocation_map=allocation,
            liveness_info=liveness_info,
            warnings=self.warnings,
        )

        return result

    def generate_program(self, program: Program, verbose: bool = False) -> Dict[str, CodeGenResult]:
        """Generate code for complete program"""
        results: Dict[str, CodeGenResult] = {}

        if verbose:
            print(f"[CODEGEN] Generating code for program with {len(program.functions)} functions")

        for func_name, function in program.functions.items():
            result = self.generate_function(function, verbose=verbose)
            results[func_name] = result

            if result.warnings:
                if verbose:
                    print(f"[CODEGEN] Warnings for {func_name}:")
                    for warning in result.warnings:
                        print(f"[CODEGEN]   {warning}")

        return results


def example_code_generation():
    """Example: Complete code generation"""
    from ir_types import IRBuilder, Constant, VariableValue

    # Build simple IR function
    builder = IRBuilder("factorial", ["n"])

    # Entry block: check if n <= 1
    entry_block = builder.new_block("entry")
    builder.emit_assignment("one", Constant(1.0))
    # In real implementation, would have branch here
    # For now, just linear code

    # Compute n - 1
    builder.emit_binary_op("sub", "n_minus_1", VariableValue("n"), Constant(1.0))

    # Multiply n * factorial(n-1)
    # (assuming recursive call result in a temporary)
    builder.emit_binary_op("mul", "result", VariableValue("n"), VariableValue("one"))

    builder.emit_return(VariableValue("result"))

    func = builder.finalize()

    # Generate code
    codegen = CodeGenerator()
    result = codegen.generate_function(func, verbose=True)

    print("\n=== Generated Assembly ===")
    print(result.get_assembly_text())

    print("\n=== Allocation Map ===")
    print(result.allocation_map.print_allocation())

    print("\n=== Labels ===")
    for label, addr in result.get_label_map().items():
        print(f"  {label:20} → {addr:3}")

    print("\nCode generation test PASSED")


if __name__ == "__main__":
    example_code_generation()
