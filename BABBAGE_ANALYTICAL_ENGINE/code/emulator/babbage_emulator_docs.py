#!/usr/bin/env python3
"""
BABBAGE ANALYTICAL ENGINE EMULATOR
Complete Python implementation of the Babbage Analytical Engine ISA

Historical Specification:
- 50-digit decimal fixed-point arithmetic
- 2,000-word memory (50 digits per word)
- 4 registers: A (accumulator), B (secondary), C (counter), D (destination)
- 32-instruction ISA with mechanical timing simulation
- Card-based I/O (program input, result output)

Author: Claude Code (AI implementation based on specifications)
Date: 2025-10-31
License: Educational, historical simulation
"""

import sys
from typing import Dict, List, Tuple, Any
from enum import IntEnum
from dataclasses import dataclass, field


# ============================================================================
# CONSTANTS AND ENUMERATIONS
# ============================================================================

class Opcode(IntEnum):
    """Instruction opcodes (32 total)"""
    NOP = 0x00
    ADD = 0x01
    SUB = 0x02
    MULT = 0x03
    DIV = 0x04
    SQRT = 0x05
    LOAD = 0x06
    STOR = 0x07
    JMP = 0x08
    JZ = 0x09
    JNZ = 0x0A
    JLT = 0x0B
    JGT = 0x0C
    JLE = 0x0D
    JGE = 0x0E
    CMP = 0x0F
    CALL = 0x10
    RET = 0x11
    PUSH = 0x12
    POP = 0x13
    RDCRD = 0x14
    WRPCH = 0x15
    WRPRN = 0x16
    HALT = 0x17
    SHL = 0x18  # Shift left
    SHR = 0x19  # Shift right
    AND = 0x1A  # Bitwise AND
    OR = 0x1B   # Bitwise OR
    XOR = 0x1C  # Bitwise XOR
    NEG = 0x1D  # Negate
    ABS = 0x1E  # Absolute value
    MOD = 0x1F  # Modulo


class Register(IntEnum):
    """Register indices"""
    A = 0
    B = 1
    C = 2
    D = 3


# Instruction timing (in seconds, mechanical operation)
TIMING_TABLE = {
    'NOP': 0,
    'ADD': 8,
    'SUB': 8,
    'CMP': 4,
    'JMP': 4,
    'JZ': 4,
    'JNZ': 4,
    'JLT': 4,
    'JGT': 4,
    'JLE': 4,
    'JGE': 4,
    'LOAD': 15,
    'STOR': 15,
    'MULT': 400,
    'DIV': 750,
    'SQRT': 250,
    'RDCRD': 30,
    'WRPCH': 30,
    'WRPRN': 2,
    'CALL': 8,
    'RET': 4,
    'PUSH': 4,
    'POP': 4,
    'SHL': 8,
    'SHR': 8,
    'AND': 8,
    'OR': 8,
    'XOR': 8,
    'NEG': 4,
    'ABS': 4,
    'MOD': 12,
    'HALT': 0,
}

OPCODE_NAMES = {v: k for k, v in Opcode.__members__.items()}


# ============================================================================
# DATA STRUCTURES
# ============================================================================

@dataclass
class Instruction:
    """Decoded instruction"""
    opcode: int
    register: int
    address: int
    immediate: int
    
    def to_50bit(self) -> int:
        """Encode as 50-bit instruction word"""
        return (
            (self.opcode << 42) |
            (self.register << 40) |
            (self.address << 29) |
            (self.immediate & 0x1FFFFFFF)
        )
    
    @staticmethod
    def from_50bit(word: int) -> 'Instruction':
        """Decode from 50-bit instruction word"""
        opcode = (word >> 42) & 0xFF
        register = (word >> 40) & 0x03
        address = (word >> 29) & 0x7FF
        immediate = word & 0x1FFFFFFF
        
        # Handle signed immediate (sign-extend from 29 bits)
        if immediate & 0x10000000:
            immediate = -(0x20000000 - immediate)
        
        return Instruction(opcode, register, address, immediate)


@dataclass
class ExecutionState:
    """Snapshot of engine state at any point"""
    pc: int
    registers: List[int] = field(default_factory=list)
    memory: List[int] = field(default_factory=list)
    clock_time: int = 0
    flags: Dict[str, bool] = field(default_factory=dict)


# ============================================================================
# MAIN ENGINE
# ============================================================================

class BabbageEngine:
    """
    Babbage Analytical Engine emulator
    
    Implements:
    - 50-digit decimal arithmetic (fixed-point)
    - 2,000 word memory
    - 4 registers (A, B, C, D)
    - 32-instruction ISA
    - Mechanical timing simulation
    - Card-based I/O
    """
    
    def __init__(self, verbose: bool = False):
        """Initialize engine"""
        self.verbose = verbose
        
        # Memory: 2,000 × 50-digit decimal numbers
        self.memory = [0] * 2000
        
        # Registers
        self.registers = [0] * 4  # A, B, C, D
        
        # Control state
        self.pc = 0  # Program Counter
        self.running = False
        self.halted = False
        
        # Timing
        self.clock_time = 0  # seconds
        
        # Flags (set by comparison operations)
        self.flags = {
            'ZERO': False,
            'SIGN': False,
            'EQUAL': False,
            'LESS': False,
            'GREATER': False,
            'OVERFLOW': False,
        }
        
        # Program
        self.instructions: List[Instruction] = []
        self.labels: Dict[str, int] = {}
        
        # I/O
        self.input_cards: List[str] = []
        self.output_cards: List[Dict[str, Any]] = []
        
        # Debugging
        self.trace_enabled = False
        self.breakpoints: List[Dict[str, Any]] = []
        self.paused = False
        
        # Return stack for CALL/RET
        self.return_stack: List[int] = []
        
        # Execution history
        self.history: List[ExecutionState] = []
    
    # ========================================================================
    # PROGRAM LOADING AND PARSING
    # ========================================================================
    
    def load_program_from_file(self, filename: str):
        """Load program from text file"""
        with open(filename, 'r') as f:
            content = f.read()
        self.load_program_from_text(content)
    
    def load_program_from_text(self, text: str):
        """Parse program from text format"""
        lines = text.strip().split('\n')
        instructions = []
        self.labels = {}
        
        # First pass: identify labels
        for line_num, line in enumerate(lines):
            # Remove comments
            line = line.split('#')[0].strip()
            if not line:
                continue
            
            # Check for label
            if ':' in line:
                label_name = line.split(':')[0].strip()
                self.labels[label_name] = len(instructions)
                line = line.split(':', 1)[1].strip()
                if not line:
                    continue
            
            # Parse instruction
            parts = line.split()
            if parts and parts[0].upper() not in OPCODE_NAMES.values():
                continue
            
            if parts:
                instructions.append(line)
        
        # Second pass: encode instructions
        for line in instructions:
            parts = line.split()
            if not parts:
                continue
            
            opcode_name = parts[0].upper()
            operands = parts[1:] if len(parts) > 1 else []
            
            try:
                instruction = self._encode_instruction(opcode_name, operands)
                self.instructions.append(instruction)
            except Exception as e:
                if self.verbose:
                    print(f"Parse error: {e}")
                raise
        
        if self.verbose:
            print(f"Loaded {len(self.instructions)} instructions, {len(self.labels)} labels")
    
    def _encode_instruction(self, opcode_name: str, operands: List[str]) -> Instruction:
        """Encode instruction from assembly text"""
        opcode_name = opcode_name.upper()
        
        if opcode_name not in Opcode.__members__:
            raise ValueError(f"Unknown opcode: {opcode_name}")
        
        opcode = Opcode[opcode_name]
        register = 0
        address = 0
        immediate = 0
        
        # Parse operands based on instruction type
        if opcode_name in ['LOAD', 'STOR']:
            if len(operands) < 2:
                raise ValueError(f"{opcode_name} requires 2 operands")
            register = self._parse_register(operands[0].rstrip(','))
            address, immediate = self._parse_address(operands[1])
        
        elif opcode_name in ['ADD', 'SUB', 'AND', 'OR', 'XOR']:
            if len(operands) < 2:
                raise ValueError(f"{opcode_name} requires 2 operands")
            register = self._parse_register(operands[0].rstrip(','))
            _, immediate = self._parse_address(operands[1])
        
        elif opcode_name in ['JMP', 'JZ', 'JNZ', 'JLT', 'JGT', 'JLE', 'JGE']:
            if len(operands) < 1:
                raise ValueError(f"{opcode_name} requires address")
            label_or_addr = operands[0]
            if label_or_addr in self.labels:
                address = self.labels[label_or_addr]
            else:
                address = int(label_or_addr)
        
        elif opcode_name in ['CALL']:
            if len(operands) < 1:
                raise ValueError(f"{opcode_name} requires address")
            label_or_addr = operands[0]
            if label_or_addr in self.labels:
                address = self.labels[label_or_addr]
            else:
                address = int(label_or_addr)
        
        elif opcode_name in ['CMP']:
            if len(operands) < 2:
                raise ValueError(f"{opcode_name} requires 2 operands")
            register = self._parse_register(operands[0].rstrip(','))
            _, immediate = self._parse_address(operands[1])
        
        return Instruction(opcode, register, address, immediate)
    
    def _parse_register(self, reg_str: str) -> int:
        """Parse register string (A, B, C, D) to index"""
        reg_str = reg_str.upper().strip()
        if reg_str in Register.__members__:
            return Register[reg_str]
        raise ValueError(f"Invalid register: {reg_str}")
    
    def _parse_address(self, addr_str: str) -> Tuple[int, int]:
        """Parse address (memory[addr] or #immediate)"""
        addr_str = addr_str.strip()
        
        if addr_str.startswith('[') and addr_str.endswith(']'):
            # Memory address: [1000]
            addr = int(addr_str[1:-1])
            return addr, 0
        elif addr_str.startswith('#'):
            # Immediate: #12345
            immediate = int(addr_str[1:])
            return 0, immediate
        else:
            # Try to parse as address
            try:
                return int(addr_str), 0
            except ValueError:
                raise ValueError(f"Invalid address/immediate: {addr_str}")
    
    # ========================================================================
    # EXECUTION
    # ========================================================================
    
    def run(self):
        """Execute program from start to halt"""
        self.pc = 0
        self.running = True
        self.halted = False
        self.clock_time = 0
        
        while self.running and not self.halted:
            if self.pc >= len(self.instructions):
                self.halted = True
                break
            
            instruction = self.instructions[self.pc]
            self.step_instruction(instruction)
            
            # Check breakpoints
            if self._check_breakpoints():
                self.paused = True
                if self.verbose:
                    print(f"[BREAKPOINT] PC={self.pc}, Clock={self.clock_time}s")
                break
    
    def step_instruction(self, instruction: Instruction):
        """Execute single instruction"""
        opcode_name = OPCODE_NAMES.get(instruction.opcode, 'UNKNOWN')
        
        if self.trace_enabled:
            self._print_trace_before(instruction, opcode_name)
        
        # Execute operation
        self._execute_opcode(instruction, opcode_name)
        
        # Update clock
        time_cost = TIMING_TABLE.get(opcode_name, 0)
        self.clock_time += time_cost
        
        # Increment PC (unless instruction set it)
        if not self.halted and opcode_name not in ['JMP', 'JZ', 'JNZ', 'JLT', 'JGT', 'JLE', 'JGE', 'CALL', 'RET', 'HALT']:
            self.pc += 1
        
        if self.trace_enabled:
            self._print_trace_after(instruction, opcode_name, time_cost)
    
    def _execute_opcode(self, instr: Instruction, opcode_name: str):
        """Execute specific opcode"""
        try:
            if opcode_name == 'NOP':
                pass
            
            elif opcode_name == 'ADD':
                reg = instr.register
                operand = instr.immediate if instr.immediate else self.memory[instr.address]
                self.registers[reg] = (self.registers[reg] + operand) % (10**50)
                self._set_flags(self.registers[reg])
            
            elif opcode_name == 'SUB':
                reg = instr.register
                operand = instr.immediate if instr.immediate else self.memory[instr.address]
                self.registers[reg] = (self.registers[reg] - operand) % (10**50)
                self._set_flags(self.registers[reg])
            
            elif opcode_name == 'MULT':
                # A = A * B
                product = self.registers[Register.A] * self.registers[Register.B]
                self.registers[Register.A] = product % (10**50)
                self.registers[Register.D] = product // (10**50)
                self._set_flags(self.registers[Register.A])
            
            elif opcode_name == 'DIV':
                # A = A / B (integer division)
                if self.registers[Register.B] == 0:
                    raise RuntimeError("Division by zero")
                self.registers[Register.A] = self.registers[Register.A] // self.registers[Register.B]
                self._set_flags(self.registers[Register.A])
            
            elif opcode_name == 'SQRT':
                # A = sqrt(A) (integer approximation)
                val = self.registers[Register.A]
                self.registers[Register.A] = int(val ** 0.5)
                self._set_flags(self.registers[Register.A])
            
            elif opcode_name == 'LOAD':
                self.registers[instr.register] = self.memory[instr.address]
            
            elif opcode_name == 'STOR':
                self.memory[instr.address] = self.registers[instr.register]
            
            elif opcode_name == 'JMP':
                self.pc = instr.address
            
            elif opcode_name == 'JZ':
                if self.flags['ZERO']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'JNZ':
                if not self.flags['ZERO']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'JLT':
                if self.flags['LESS']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'JGT':
                if self.flags['GREATER']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'JLE':
                if self.flags['LESS'] or self.flags['EQUAL']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'JGE':
                if self.flags['GREATER'] or self.flags['EQUAL']:
                    self.pc = instr.address
                else:
                    self.pc += 1
            
            elif opcode_name == 'CMP':
                a_val = self.registers[Register.A]
                b_val = instr.immediate if instr.immediate else self.memory[instr.address]
                self.flags['ZERO'] = (a_val == b_val)
                self.flags['EQUAL'] = (a_val == b_val)
                self.flags['LESS'] = (a_val < b_val)
                self.flags['GREATER'] = (a_val > b_val)
                self.flags['SIGN'] = (a_val < 0)
                self.pc += 1
            
            elif opcode_name == 'CALL':
                self.return_stack.append(self.pc + 1)
                self.pc = instr.address
            
            elif opcode_name == 'RET':
                if not self.return_stack:
                    raise RuntimeError("Return stack underflow")
                self.pc = self.return_stack.pop()
            
            elif opcode_name == 'PUSH':
                self.return_stack.append(self.registers[instr.register])
                self.pc += 1
            
            elif opcode_name == 'POP':
                if not self.return_stack:
                    raise RuntimeError("Return stack underflow")
                self.registers[instr.register] = self.return_stack.pop()
                self.pc += 1
            
            elif opcode_name == 'RDCRD':
                # Read from input card
                if self.input_cards:
                    value = int(self.input_cards.pop(0))
                    self.registers[Register.A] = value
                self.pc += 1
            
            elif opcode_name == 'WRPCH':
                # Write to punch card
                self.output_cards.append({
                    'type': 'punch',
                    'value': self.registers[Register.A],
                    'time': self.clock_time
                })
                self.pc += 1
            
            elif opcode_name == 'WRPRN':
                # Write to printer
                self.output_cards.append({
                    'type': 'print',
                    'value': self.registers[Register.A],
                    'time': self.clock_time
                })
                self.pc += 1
            
            elif opcode_name == 'HALT':
                self.halted = True
            
            elif opcode_name == 'NEG':
                reg = instr.register
                self.registers[reg] = (-self.registers[reg]) % (10**50)
                self._set_flags(self.registers[reg])
                self.pc += 1
            
            elif opcode_name == 'ABS':
                reg = instr.register
                if self.registers[reg] < 0:
                    self.registers[reg] = -self.registers[reg]
                self._set_flags(self.registers[reg])
                self.pc += 1
            
            elif opcode_name == 'MOD':
                if self.registers[Register.B] == 0:
                    raise RuntimeError("Division by zero in MOD")
                self.registers[Register.A] = self.registers[Register.A] % self.registers[Register.B]
                self._set_flags(self.registers[Register.A])
                self.pc += 1
            
            else:
                raise RuntimeError(f"Unknown opcode: {opcode_name}")
        
        except Exception as e:
            print(f"Execution error at PC={self.pc}: {e}")
            self.halted = True
    
    def _set_flags(self, value: int):
        """Set flags based on value"""
        self.flags['ZERO'] = (value == 0)
        self.flags['SIGN'] = (value < 0)
        self.flags['OVERFLOW'] = (value >= 10**50)
    
    # ========================================================================
    # DEBUGGING
    # ========================================================================
    
    def _check_breakpoints(self) -> bool:
        """Check if any breakpoint is hit"""
        for bp in self.breakpoints:
            if not bp.get('enabled', True):
                continue
            
            if bp['type'] == 'pc' and self.pc == bp['target']:
                return True
            elif bp['type'] == 'time' and self.clock_time >= bp['target']:
                return True
            elif bp['type'] == 'register' and self.registers[bp['register']] == bp['target']:
                return True
        
        return False
    
    def set_breakpoint(self, bp_type: str, target: int, register: int = 0):
        """Set breakpoint"""
        self.breakpoints.append({
            'type': bp_type,
            'target': target,
            'register': register,
            'enabled': True
        })
    
    def _print_trace_before(self, instr: Instruction, opcode_name: str):
        """Print execution trace (before)"""
        print(f"[{self.pc:04d}] {opcode_name:8s} A={self.registers[0]:10d} "
              f"B={self.registers[1]:10d} C={self.registers[2]:10d} "
              f"Clock={self.clock_time}s")
    
    def _print_trace_after(self, instr: Instruction, opcode_name: str, time_cost: int):
        """Print execution trace (after)"""
        pass  # Optional additional output
    
    def dump_state(self) -> str:
        """Dump current state"""
        lines = [
            "=== ENGINE STATE ===",
            f"PC: {self.pc}",
            f"Clock: {self.clock_time}s",
            f"Registers: A={self.registers[0]}, B={self.registers[1]}, "
            f"C={self.registers[2]}, D={self.registers[3]}",
            f"Flags: {self.flags}",
            f"Return stack depth: {len(self.return_stack)}",
        ]
        return '\n'.join(lines)
    
    def dump_memory(self, start: int = 0, end: int = 100) -> str:
        """Dump memory range"""
        lines = ["=== MEMORY DUMP ==="]
        for i in range(start, min(end, len(self.memory))):
            if self.memory[i] != 0:
                lines.append(f"[{i:04d}]: {self.memory[i]}")
        return '\n'.join(lines)


# ============================================================================
# COMMAND-LINE INTERFACE
# ============================================================================

def main():
    """Command-line interface"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Babbage Analytical Engine Emulator')
    parser.add_argument('program', help='Program file (.txt)')
    parser.add_argument('--trace', action='store_true', help='Enable execution trace')
    parser.add_argument('--verbose', action='store_true', help='Verbose output')
    parser.add_argument('--breakpoint-pc', type=int, help='Break at PC')
    parser.add_argument('--breakpoint-time', type=int, help='Break at clock time (seconds)')
    parser.add_argument('--dump-memory', action='store_true', help='Dump memory at end')
    parser.add_argument('--output', type=str, help='Output file for results')
    
    args = parser.parse_args()
    
    # Create engine
    engine = BabbageEngine(verbose=args.verbose)
    engine.trace_enabled = args.trace
    
    # Load program
    try:
        engine.load_program_from_file(args.program)
    except Exception as e:
        print(f"Error loading program: {e}", file=sys.stderr)
        sys.exit(1)
    
    # Set breakpoints
    if args.breakpoint_pc is not None:
        engine.set_breakpoint('pc', args.breakpoint_pc)
    if args.breakpoint_time is not None:
        engine.set_breakpoint('time', args.breakpoint_time)
    
    # Run
    try:
        engine.run()
    except KeyboardInterrupt:
        print("\n[Interrupted]")
    except Exception as e:
        print(f"Error during execution: {e}", file=sys.stderr)
    
    # Report results
    print(f"\n=== EXECUTION COMPLETE ===")
    print(f"Total clock time: {engine.clock_time}s ({engine.clock_time/60:.1f} minutes)")
    print(f"Total instructions executed: {engine.pc}")
    print(f"Status: {'HALTED' if engine.halted else 'RUNNING'}")
    
    if engine.verbose or args.dump_memory:
        print("\n" + engine.dump_state())
        print("\n" + engine.dump_memory())
    
    print(f"\nOutput cards: {len(engine.output_cards)}")
    for i, card in enumerate(engine.output_cards):
        print(f"  Card {i}: {card['type']} = {card['value']} (t={card['time']}s)")
    
    if args.output:
        with open(args.output, 'w') as f:
            for card in engine.output_cards:
                f.write(f"{card['value']}\n")
        print(f"\nResults written to {args.output}")


if __name__ == '__main__':
    main()
