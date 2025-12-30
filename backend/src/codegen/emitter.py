"""
Code Emitter for Babbage

Emits assembly code with label resolution and address assignment.

Responsibilities:
- Map labels to instruction addresses
- Emit assembly syntax
- Handle forward references (two-pass if needed)
- Generate comments and debugging info
"""

from __future__ import annotations
from dataclasses import dataclass
from typing import List, Dict, Optional
from backend.src.codegen.selector import AsmInstruction, AsmOperand


@dataclass
class AssemblyOutput:
    """Complete assembly code output"""
    assembly_text: str
    label_map: Dict[str, int]     # label → instruction address
    instruction_count: int
    spill_count: int = 0
    comment: str = ""


class CodeEmitter:
    """
    Emits Babbage assembly code.
    
    Algorithm:
    1. Assign addresses to all labels (pass 1)
    2. Emit assembly instructions (pass 2)
    3. Generate symbol table and debugging info
    """
    
    def __init__(self):
        self.instructions: List[AsmInstruction] = []
        self.labels: Dict[str, int] = {}  # label → instruction address
        self.current_address = 0
    
    def add_instruction(self, instr: AsmInstruction) -> None:
        """Add instruction to code"""
        self.instructions.append(instr)
    
    def add_label(self, label: str) -> None:
        """Add label at current position"""
        self.labels[label] = self.current_address
    
    def emit(self, spill_count: int = 0) -> AssemblyOutput:
        """Emit complete assembly code"""
        # Pass 1: Assign addresses to labels
        self._assign_addresses()
        
        # Pass 2: Generate assembly text
        asm_lines = []
        asm_lines.append(".global main")
        asm_lines.append(".text")
        asm_lines.append("")
        
        addr = 0
        for instr in self.instructions:
            # Check if label points to this instruction
            label_at_addr = None
            for label, label_addr in self.labels.items():
                if label_addr == addr:
                    label_at_addr = label
                    break
            
            if label_at_addr:
                asm_lines.append(f"{label_at_addr}:")
            
            # Emit instruction
            instr_text = instr.to_asm_string()
            asm_lines.append(f"  {instr_text}")
            
            addr += 1
        
        assembly_text = "\n".join(asm_lines)
        
        return AssemblyOutput(
            assembly_text=assembly_text,
            label_map=self.labels,
            instruction_count=len(self.instructions),
            spill_count=spill_count,
            comment=f"Generated Babbage assembly ({len(self.instructions)} instructions)"
        )
    
    def _assign_addresses(self) -> None:
        """Assign instruction addresses (pass 1)"""
        # Update current_address for label assignment
        self.current_address = len(self.instructions)
    
    def print_assembly(self, output: AssemblyOutput) -> str:
        """Pretty-print assembly with addresses and comments"""
        lines = ["=== Babbage Assembly ===\n"]
        
        asm_lines = output.assembly_text.split("\n")
        
        # Add address column
        addr = 0
        for line in asm_lines:
            if line.startswith("."):
                lines.append(f"{line}")
            elif line.strip().endswith(":"):
                # Label line
                lines.append(f"{line}")
            elif line.strip().startswith("#"):
                # Comment
                lines.append(f"{line}")
            elif line.strip():
                # Instruction
                lines.append(f"{addr:3}  {line}")
                addr += 1
            else:
                lines.append("")
        
        lines.append(f"\nLabel Map:")
        for label, addr in sorted(output.label_map.items(), key=lambda x: x[1]):
            lines.append(f"  {label:20} → {addr:3}")
        
        lines.append(f"\nInstructions: {output.instruction_count}")
        lines.append(f"Spills: {output.spill_count}")
        
        return "\n".join(lines)


def example_code_emission():
    """Example: Code emission"""
    from selector import AsmInstruction, AsmOperand
    
    # Create emitter
    emitter = CodeEmitter()
    
    # Add instructions
    emitter.add_label("main")
    emitter.add_instruction(AsmInstruction("MOV", [
        AsmOperand("reg", "A"),
        AsmOperand("immed", "10")
    ], comment="a = 10"))
    
    emitter.add_instruction(AsmInstruction("MOV", [
        AsmOperand("reg", "B"),
        AsmOperand("immed", "5")
    ], comment="b = 5"))
    
    emitter.add_instruction(AsmInstruction("ADD", [
        AsmOperand("reg", "A"),
        AsmOperand("reg", "B")
    ], comment="a = a + b"))
    
    emitter.add_instruction(AsmInstruction("WRPRN", [
        AsmOperand("reg", "A")
    ], comment="print a"))
    
    emitter.add_instruction(AsmInstruction("RET", [], comment="return"))
    
    # Emit
    output = emitter.emit()
    
    print(emitter.print_assembly(output))
    print()
    
    # Verify
    assert output.instruction_count == 5
    assert "main" in output.label_map
    
    print("Code emission test PASSED")


if __name__ == "__main__":
    example_code_emission()
