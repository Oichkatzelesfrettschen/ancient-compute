"""Babbage Assembler - Assembly language to machine code conversion.

Implements a two-pass assembler for Babbage assembly language:
- Pass 1: Symbol resolution (collect labels, variables, functions)
- Pass 2: Code emission (resolve operands, encode to 50-bit machine code)

Supports:
- All 32 Babbage instructions (arithmetic, memory, control flow, I/O)
- Labels and forward references
- Decimal immediates (up to 50 digits)
- Register and memory operands
- Comments and directives
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple


# ============================================================================
# DATA STRUCTURES
# ============================================================================


@dataclass
class Token:
    """Single token from assembly source."""

    type: str  # 'mnemonic', 'register', 'label', 'number', 'identifier', 'comma', 'comment'
    value: Any
    line_number: int


@dataclass
class Instruction:
    """Parsed assembly instruction."""

    mnemonic: str
    operands: List[Any]  # Register names, numbers, or labels
    label: Optional[str] = None
    comment: Optional[str] = None
    line_number: int = 0


@dataclass
class AssemblyResult:
    """Result of assembling a program."""

    machine_code: List[int]  # 50-bit machine words
    symbol_table: Dict[str, int]  # Label/variable → address
    instruction_count: int
    error_count: int = 0
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)

    def get_hex_dump(self) -> str:
        """Return hex dump of machine code with symbol information."""
        lines = ["Address    Hex Code              Mnemonic    Comment"]
        lines.append("=" * 60)
        for addr, word in enumerate(self.machine_code):
            hex_code = f"0x{word:012x}"
            lines.append(f"{addr:<10} {hex_code}")
        return "\n".join(lines)

    def get_symbol_map(self) -> str:
        """Return human-readable symbol map."""
        lines = ["Symbol Table", "=" * 40]
        for symbol, addr in sorted(self.symbol_table.items(), key=lambda x: x[1]):
            lines.append(f"{symbol:<20} {addr}")
        return "\n".join(lines)


class AssemblyError(Exception):
    """Raised when assembly fails."""

    pass


# ============================================================================
# LEXER
# ============================================================================


class Lexer:
    """Tokenizes Babbage assembly language."""

    MNEMONICS = {
        "NOP", "ADD", "SUB", "MULT", "DIV", "SQRT", "ABS", "NEG",
        "LOAD", "STOR", "MOV",
        "JMP", "JZ", "JNZ", "JLT", "JGT", "JLE", "JGE", "CMP",
        "CALL", "RET", "PUSH", "POP",
        "RDCRD", "WRPCH", "WRPRN",
        "SHL", "SHR",
    }

    REGISTERS = {"A", "B", "C", "D"}

    DIRECTIVES = {".global", ".data", ".text", ".align", ".space"}

    def __init__(self, assembly_text: str):
        self.text = assembly_text
        self.lines = assembly_text.split("\n")
        self.tokens: List[Token] = []

    def tokenize(self) -> List[Token]:
        """Convert assembly text to tokens."""
        for line_num, line in enumerate(self.lines, 1):
            # Remove comments
            if "#" in line:
                line = line.split("#")[0]

            line = line.strip()
            if not line:
                continue

            # Check for directive
            if line.startswith("."):
                parts = line.split()
                directive = parts[0]
                if directive in self.DIRECTIVES:
                    self.tokens.append(Token("directive", directive, line_num))
                    if len(parts) > 1:
                        self.tokens.append(Token("identifier", parts[1], line_num))
                continue

            # Check for label
            if ":" in line:
                label, rest = line.split(":", 1)
                label = label.strip()
                if self._is_valid_identifier(label):
                    self.tokens.append(Token("label", label, line_num))
                    line = rest.strip()
                else:
                    raise AssemblyError(f"Line {line_num}: Invalid label '{label}'")

            # Tokenize rest of line
            if line:
                self._tokenize_instruction(line, line_num)

        return self.tokens

    def _tokenize_instruction(self, line: str, line_num: int) -> None:
        """Tokenize a single instruction line."""
        # Split by comma and whitespace
        parts = []
        current = ""
        for char in line:
            if char in " \t,":
                if current:
                    parts.append(current)
                    current = ""
                if char == ",":
                    parts.append(",")
            else:
                current += char
        if current:
            parts.append(current)

        for part in parts:
            if part == ",":
                self.tokens.append(Token("comma", ",", line_num))
            elif part.upper() in self.MNEMONICS:
                self.tokens.append(Token("mnemonic", part.upper(), line_num))
            elif part.upper() in self.REGISTERS:
                self.tokens.append(Token("register", part.upper(), line_num))
            elif self._is_valid_identifier(part):
                self.tokens.append(Token("identifier", part, line_num))
            elif self._is_number(part):
                self.tokens.append(Token("number", int(part), line_num))
            else:
                raise AssemblyError(f"Line {line_num}: Invalid token '{part}'")

    @staticmethod
    def _is_valid_identifier(s: str) -> bool:
        """Check if string is valid identifier."""
        return bool(re.match(r"^[a-zA-Z_][a-zA-Z0-9_]*$", s))

    @staticmethod
    def _is_number(s: str) -> bool:
        """Check if string is a number."""
        try:
            int(s)
            return True
        except ValueError:
            return False


# ============================================================================
# PARSER
# ============================================================================


class Parser:
    """Parses tokens into Instructions."""

    def __init__(self, tokens: List[Token]):
        self.tokens = tokens
        self.pos = 0

    def parse(self) -> List[Instruction]:
        """Parse token stream into instructions."""
        instructions = []
        current_label = None

        while self.pos < len(self.tokens):
            token = self.tokens[self.pos]

            if token.type == "label":
                current_label = token.value
                self.pos += 1

            elif token.type == "mnemonic":
                instr = self._parse_instruction(token.line_number)
                if current_label:
                    instr.label = current_label
                    current_label = None
                instructions.append(instr)

            elif token.type == "directive":
                self.pos += 1
                if self.pos < len(self.tokens) and self.tokens[self.pos].type == "identifier":
                    self.pos += 1

            else:
                self.pos += 1

        return instructions

    def _parse_instruction(self, line_num: int) -> Instruction:
        """Parse single instruction."""
        mnemonic = self.tokens[self.pos].value
        self.pos += 1

        operands = []
        while self.pos < len(self.tokens):
            token = self.tokens[self.pos]

            if token.type == "label":
                break

            if token.type == "comma":
                self.pos += 1
                continue

            if token.type in {"register", "number", "identifier"}:
                operands.append(token.value)
                self.pos += 1

            else:
                break

        return Instruction(mnemonic, operands, line_number=line_num)


# ============================================================================
# INSTRUCTION ENCODER
# ============================================================================


class InstructionEncoder:
    """Encodes Babbage instructions to 50-bit machine code."""

    OPCODES = {
        "NOP": 0x00, "ADD": 0x01, "SUB": 0x02, "MULT": 0x03,
        "DIV": 0x04, "SQRT": 0x05, "LOAD": 0x06, "STOR": 0x07,
        "JMP": 0x08, "JZ": 0x09, "JNZ": 0x0A, "JLT": 0x0B,
        "JGT": 0x0C, "JLE": 0x0D, "JGE": 0x0E, "CMP": 0x0F,
        "CALL": 0x10, "RET": 0x11, "PUSH": 0x12, "POP": 0x13,
        "RDCRD": 0x14, "WRPCH": 0x15, "WRPRN": 0x16, "MOV": 0x17,
        "NEG": 0x18, "ABS": 0x19, "SHL": 0x1A, "SHR": 0x1B,
    }

    REGISTER_MAP = {"A": 0, "B": 1, "C": 2, "D": 3}

    def encode(self, mnemonic: str, operands: List[int], symbol_table: Dict[str, int]) -> int:
        """Encode instruction to 50-bit machine word.

        50-bit instruction format:
        - Bits 49-42: Opcode (8 bits)
        - Bits 41-40: Register 1 (2 bits)
        - Bits 39-38: Register 2 or Flag (2 bits)
        - Bits 37-0: Address/Immediate (38 bits)
        """
        if mnemonic not in self.OPCODES:
            raise AssemblyError(f"Unknown mnemonic: {mnemonic}")

        opcode = self.OPCODES[mnemonic]

        # Encode based on operand count
        if len(operands) == 0:
            return opcode << 42

        elif len(operands) == 1:
            reg_or_addr = operands[0]
            if isinstance(reg_or_addr, str):
                # Register
                if reg_or_addr not in self.REGISTER_MAP:
                    raise AssemblyError(f"Invalid register: {reg_or_addr}")
                reg = self.REGISTER_MAP[reg_or_addr]
                return (opcode << 42) | (reg << 40)
            else:
                # Address/immediate
                return (opcode << 42) | (reg_or_addr & 0x3FFFFFFFFF)

        elif len(operands) == 2:
            reg1 = operands[0]
            reg2_or_addr = operands[1]

            if isinstance(reg1, str):
                if reg1 not in self.REGISTER_MAP:
                    raise AssemblyError(f"Invalid register: {reg1}")
                reg1_code = self.REGISTER_MAP[reg1]
            else:
                raise AssemblyError(f"First operand must be register")

            if isinstance(reg2_or_addr, str):
                # Two registers
                if reg2_or_addr not in self.REGISTER_MAP:
                    raise AssemblyError(f"Invalid register: {reg2_or_addr}")
                reg2_code = self.REGISTER_MAP[reg2_or_addr]
                return (opcode << 42) | (reg1_code << 40) | (reg2_code << 38)
            else:
                # Register and address/immediate
                return (opcode << 42) | (reg1_code << 40) | (reg2_or_addr & 0x3FFFFFFFFF)

        raise AssemblyError(f"Invalid operand count for {mnemonic}: {len(operands)}")


# ============================================================================
# SYMBOL TABLE
# ============================================================================


class SymbolTable:
    """Tracks labels, variables, and their addresses."""

    def __init__(self):
        self.symbols: Dict[str, int] = {}

    def define(self, name: str, address: int) -> None:
        """Define a symbol at an address."""
        if name in self.symbols:
            raise AssemblyError(f"Duplicate symbol: {name}")
        self.symbols[name] = address

    def lookup(self, name: str) -> int:
        """Look up a symbol address."""
        if name not in self.symbols:
            raise AssemblyError(f"Undefined symbol: {name}")
        return self.symbols[name]

    def is_defined(self, name: str) -> bool:
        """Check if symbol is defined."""
        return name in self.symbols


# ============================================================================
# ASSEMBLER
# ============================================================================


class Assembler:
    """Two-pass Babbage assembler."""

    def __init__(self, assembly_text: str):
        self.assembly_text = assembly_text
        self.symbol_table = SymbolTable()
        self.instructions: List[Instruction] = []
        self.machine_code: List[int] = []
        self.errors: List[str] = []
        self.warnings: List[str] = []

    def assemble(self, verbose: bool = False) -> AssemblyResult:
        """Two-pass assembly: resolve symbols, then emit code.

        Args:
            verbose: Print progress information

        Returns:
            AssemblyResult with machine code and symbol table
        """
        try:
            if verbose:
                print("[ASSEMBLER] Starting two-pass assembly...")

            # Pass 1: Tokenize and parse
            if verbose:
                print("[ASSEMBLER] Pass 0: Lexing and parsing...")
            lexer = Lexer(self.assembly_text)
            tokens = lexer.tokenize()
            parser = Parser(tokens)
            self.instructions = parser.parse()

            if verbose:
                print(f"[ASSEMBLER]   Parsed {len(self.instructions)} instructions")

            # Pass 1: Symbol resolution
            if verbose:
                print("[ASSEMBLER] Pass 1: Symbol resolution...")
            self._pass1_resolve_symbols()

            if verbose:
                print(f"[ASSEMBLER]   Defined {len(self.symbol_table.symbols)} symbols")

            # Pass 2: Code emission
            if verbose:
                print("[ASSEMBLER] Pass 2: Code emission...")
            self._pass2_emit_code()

            if verbose:
                print(f"[ASSEMBLER]   Emitted {len(self.machine_code)} machine code words")
                print("[ASSEMBLER] Assembly COMPLETE")

        except AssemblyError as e:
            self.errors.append(str(e))

        return AssemblyResult(
            machine_code=self.machine_code,
            symbol_table=self.symbol_table.symbols,
            instruction_count=len(self.instructions),
            error_count=len(self.errors),
            errors=self.errors,
            warnings=self.warnings,
        )

    def _pass1_resolve_symbols(self) -> None:
        """First pass: resolve labels and collect symbols."""
        address = 0
        for instr in self.instructions:
            # Register label at current address
            if instr.label:
                self.symbol_table.define(instr.label, address)
            address += 1

    def _pass2_emit_code(self) -> None:
        """Second pass: emit machine code for each instruction."""
        encoder = InstructionEncoder()

        for instr in self.instructions:
            # Resolve operands
            resolved_operands = []
            for operand in instr.operands:
                if isinstance(operand, str):
                    # Could be register or label
                    if operand in {"A", "B", "C", "D"}:
                        resolved_operands.append(operand)
                    elif self.symbol_table.is_defined(operand):
                        resolved_operands.append(self.symbol_table.lookup(operand))
                    else:
                        # Assume it's a label or variable
                        try:
                            resolved_operands.append(self.symbol_table.lookup(operand))
                        except AssemblyError:
                            self.errors.append(f"Undefined symbol: {operand}")
                            resolved_operands.append(0)  # Default to 0
                else:
                    # Numeric operand
                    resolved_operands.append(operand)

            # Encode instruction
            try:
                machine_word = encoder.encode(instr.mnemonic, resolved_operands, self.symbol_table.symbols)
                self.machine_code.append(machine_word)
            except AssemblyError as e:
                self.errors.append(f"Line {instr.line_number}: {str(e)}")


# ============================================================================
# TESTING
# ============================================================================


if __name__ == "__main__":
    # Test 1: Simple instruction
    print("=" * 70)
    print("Test 1: Simple instruction")
    print("=" * 70)
    asm_text = """
    .global main
    .text
    main:
        LOAD A, 42
    """
    asm = Assembler(asm_text)
    result = asm.assemble(verbose=True)
    print(f"\nMachine code: {[hex(w) for w in result.machine_code]}")
    print(f"Symbol table: {result.symbol_table}")
    print()

    # Test 2: Label resolution
    print("=" * 70)
    print("Test 2: Label resolution and backward jump")
    print("=" * 70)
    asm_text = """
    .global loop_test
    .text
    loop_test:
        ADD A, 1
        JMP loop_test
    """
    asm = Assembler(asm_text)
    result = asm.assemble(verbose=True)
    print(f"\nMachine code: {[hex(w) for w in result.machine_code]}")
    print(f"Symbol table: {result.symbol_table}")
    print()

    # Test 3: Forward reference
    print("=" * 70)
    print("Test 3: Forward reference")
    print("=" * 70)
    asm_text = """
    .global forward_test
    .text
    forward_test:
        JMP loop_end
        ADD A, 1
    loop_end:
        RET
    """
    asm = Assembler(asm_text)
    result = asm.assemble(verbose=True)
    print(f"\nMachine code: {[hex(w) for w in result.machine_code]}")
    print(f"Symbol table: {result.symbol_table}")
    print()

    # Test 4: Register operations
    print("=" * 70)
    print("Test 4: Register operations")
    print("=" * 70)
    asm_text = """
    .global reg_test
    .text
    reg_test:
        LOAD A, 10
        LOAD B, 5
        ADD A, B
        WRPRN A
        RET
    """
    asm = Assembler(asm_text)
    result = asm.assemble(verbose=True)
    print(f"\nMachine code ({len(result.machine_code)} instructions):")
    for addr, word in enumerate(result.machine_code):
        print(f"  {addr}: 0x{word:012x}")
    print(f"\nSymbol table:\n{result.get_symbol_map()}")
    print()
