"""
Babbage Analytical Engine Emulator

A faithful mechanical simulation of Charles Babbage's Analytical Engine (AE),
based on Menabrea/Lovelace (1842-1843) and Babbage's mechanical notation.

Architecture:
  - Store: 2,000 × 50-digit decimal numbers (memory)
  - Mill: Arithmetic unit (ADD, SUB, MULT, DIV, SQRT)
  - Ingress/Egress: Punch card I/O (operation, variable, number cards)
  - Control: Instruction sequencing via operation cards

Instruction Set:
  - Arithmetic: ADD, SUB, MULT, DIV, SQRT (with timing costs)
  - Memory: LOAD (load from store), STOR (store to store)
  - Control: JMP, JZ, JNZ, JLT, JGT, JLE, JGE, CMP, CALL, RET
  - Stack: PUSH, POP (for subroutine support)
  - I/O: RDCRD (read punch card), WRPCH (write punch card), WRPRN (print)
  - NOP (no operation)

Numbering System:
  - 50-digit decimal representation
  - Scaling: value × 10^40 (40 fractional digits internally)
  - Full arithmetic with overflow detection
  - Supports negative numbers and comparisons

Program Control:
  - Label resolution for jumps/calls
  - Return stack (max 16 levels) for subroutine support
  - Data stack for PUSH/POP operations
  - Program counter (PC) tracking
  - Breakpoint system (address, time, register, memory conditions)
  - Trace/debug support

Timing Model:
  - Each instruction has a cost in abstract time units
  - NOP: 0, ADD: 8, MULT: 400, DIV: 750, SQRT: 250
  - Load/store: 15 cycles, Jump: 4 cycles, I/O: 30 cycles
  - Used for performance analysis and historical accuracy

References:
  - Menabrea, L. F., & Lovelace, A. A. (1843). "Note on the Analytical Engine"
  - Babbage, C. (1826). "Mechanical Notation"
  - SMG Technical Description (DE2 comparison)
"""

TIMING_TABLE = {
    "NOP": 0,
    "ADD": 8,
    "SUB": 8,
    "CMP": 4,
    "JMP": 4,
    "JZ": 4,
    "LOAD": 15,
    "STOR": 15,
    "MULT": 400,
    "DIV": 750,
    "SQRT": 250,
    "RDCRD": 30,
    "WRPCH": 30,
    "WRPRN": 2,
    "CALL": 8,
    "RET": 4,
    "PUSH": 4,
    "POP": 4,
}


class BabbageNumber:
    """
    50-digit decimal fixed-point number representation.

    Internal representation: value × 10^(-40) where value is an integer.
    This provides 50 decimal digits total with 40 fractional digits.

    Maximum magnitude: ±(10^50 - 1)
    Minimum non-zero: ±10^(-40)

    Supports full arithmetic operations with overflow detection.
    """

    def __init__(self, value):
        """Initialize BabbageNumber from Python number."""
        self._overflow_flag = False
        if isinstance(value, BabbageNumber):
            self.value = value.value
        else:
            self.value = int(value * (10**40))  # Scale to 50 digits

    def to_decimal(self):
        """Convert to Python float (for display and testing)."""
        return self.value / (10**40)

    def to_card_format(self):
        """
        Convert to punch card 50-digit format.

        Hollerith cards had 50 columns for digit punching.
        This formats the number as a 50-character string.
        """
        return str(self.value).zfill(50)

    def _check_overflow(self):
        """
        Check if value exceeds 50-digit bounds.

        Returns:
            True if overflow occurred, False otherwise
            Sets internal overflow flag for later inspection
        """
        max_value = (10**50) - 1
        min_value = -(10**50) + 1
        if self.value > max_value or self.value < min_value:
            self._overflow_flag = True
            # Truncate to 50 digits
            self.value = self.value % (10**50)
            if self.value > max_value:
                self.value -= 10**50
            return True
        return False

    def __add__(self, other):
        """Addition with overflow check."""
        result = BabbageNumber(0)
        result.value = self.value + BabbageNumber(other).value
        result._check_overflow()
        return result

    def __sub__(self, other):
        """Subtraction with overflow check."""
        result = BabbageNumber(0)
        result.value = self.value - BabbageNumber(other).value
        result._check_overflow()
        return result

    def __mul__(self, other):
        """Multiplication with overflow check."""
        result = BabbageNumber(0)
        result.value = self.value * BabbageNumber(other).value
        result._check_overflow()
        return result

    def __truediv__(self, other):
        """Fixed-point division."""
        if BabbageNumber(other).value == 0:
            raise ZeroDivisionError("Division by zero")
        result = BabbageNumber(0)
        # Fixed-point division: scale numerator by 10^40 before dividing
        result.value = (self.value * (10**40)) // BabbageNumber(other).value
        result._check_overflow()
        return result

    def __eq__(self, other):
        """Equality comparison."""
        return self.value == BabbageNumber(other).value

    def __lt__(self, other):
        """Less-than comparison."""
        return self.value < BabbageNumber(other).value

    def __gt__(self, other):
        """Greater-than comparison."""
        return self.value > BabbageNumber(other).value

    def __le__(self, other):
        """Less-than-or-equal comparison."""
        return self.value <= BabbageNumber(other).value

    def __ge__(self, other):
        """Greater-than-or-equal comparison."""
        return self.value >= BabbageNumber(other).value

    def __ne__(self, other):
        """Not-equal comparison."""
        return self.value != BabbageNumber(other).value

    def __int__(self):
        """Convert to integer (internal scaled value)."""
        return self.value

    def __repr__(self):
        """String representation for debugging."""
        return f"BabbageNumber({self.to_decimal()})"


class Instruction:
    """
    Single Analytical Engine instruction.

    Opcode: Operation name (ADD, LOAD, JMP, etc.)
    Operands: List of arguments (registers, addresses, immediates)
    """

    def __init__(self, opcode, operands=None):
        self.opcode = opcode
        self.operands = operands if operands is not None else []


class Engine:
    """
    Babbage Analytical Engine emulator.

    Complete simulation of the mechanical Analytical Engine architecture:
    - 2,000 word memory (store) with 50-digit decimal numbers
    - 4 arithmetic registers (A, B, C, D) for mill operations
    - Program counter for instruction sequencing
    - Flag registers (ZERO, SIGN, OVERFLOW, comparison flags)
    - Return and data stacks for control flow
    - Breakpoint system for debugging
    - Punch card I/O simulation
    """

    def __init__(self):
        """Initialize empty engine."""
        # Memory: Store of 2,000 50-digit decimal numbers
        self.memory = [BabbageNumber(0) for _ in range(2000)]

        # Registers: Mill components for arithmetic
        self.registers = {
            "A": BabbageNumber(0),  # Accumulator
            "B": BabbageNumber(0),  # Operand register
            "C": BabbageNumber(0),  # Auxiliary
            "D": BabbageNumber(0),  # Auxiliary
        }

        # Control: Program sequencing and timing
        self.PC = 0  # Program Counter
        self.running = True  # Execution flag
        self.clock_time = 0  # Simulated elapsed time

        # I/O: Punch cards and results
        self.instruction_cards = []  # Loaded program
        self.result_cards = []  # Output results

        # Flags: Condition codes from operations
        self.flags = {
            "ZERO": False,  # Result was zero
            "SIGN": False,  # Result was negative
            "OVERFLOW": False,  # Arithmetic overflow
            "GREATER": False,  # CMP: first > second
            "LESS": False,  # CMP: first < second
            "EQUAL": False,  # CMP: first == second
        }

        # Control stacks: For subroutines and temporary storage
        self.return_stack = []  # CALL/RET return addresses (max 16)
        self.data_stack = []  # PUSH/POP temporary storage

        # Debugging: Breakpoints and tracing
        self.breakpoints = []  # Conditional breakpoints
        self.trace_enabled = False  # Trace execution to console
        self.paused = False  # Execution paused at breakpoint
        self.interactive_mode = False  # Interactive debugging mode

        # Instruction handlers: Maps opcode to execution function
        self._opcode_handlers = {
            "NOP": self._execute_NOP,
            "ADD": self._execute_ADD,
            "SUB": self._execute_SUB,
            "MULT": self._execute_MULT,
            "DIV": self._execute_DIV,
            "SQRT": self._execute_SQRT,
            "LOAD": self._execute_LOAD,
            "STOR": self._execute_STOR,
            "JMP": self._execute_JMP,
            "JZ": self._execute_JZ,
            "JNZ": self._execute_JNZ,
            "JLT": self._execute_JLT,
            "JGT": self._execute_JGT,
            "JLE": self._execute_JLE,
            "JGE": self._execute_JGE,
            "CMP": self._execute_CMP,
            "CALL": self._execute_CALL,
            "RET": self._execute_RET,
            "PUSH": self._execute_PUSH,
            "POP": self._execute_POP,
            "RDCRD": self._execute_RDCRD,
            "WRPCH": self._execute_WRPCH,
            "WRPRN": self._execute_WRPRN,
        }

    def _get_register_value(self, reg_name):
        """Get value of named register."""
        if reg_name in self.registers:
            return self.registers[reg_name]
        raise ValueError(f"Invalid register: {reg_name}")

    def _set_register_value(self, reg_name, value):
        """Set value of named register."""
        if reg_name in self.registers:
            self.registers[reg_name] = value
        else:
            raise ValueError(f"Invalid register: {reg_name}")

    def _update_flags(self, value1, value2=None, comparison_result=None):
        """Update condition flags based on operation result."""
        self.flags["ZERO"] = value1 == BabbageNumber(0)
        self.flags["SIGN"] = value1 < BabbageNumber(0)

        if comparison_result is not None:
            self.flags["GREATER"] = comparison_result > 0
            self.flags["LESS"] = comparison_result < 0
            self.flags["EQUAL"] = comparison_result == 0
        else:
            self.flags["GREATER"] = False
            self.flags["LESS"] = False
            self.flags["EQUAL"] = False

    def _get_operand_value(self, operand_str):
        """
        Resolve operand to value.

        Operand can be:
          - Register name: 'A', 'B', 'C', 'D'
          - Memory address: '[0]', '[999]', '[1999]'
          - Immediate value: '42', '-1', '0'
        """
        if operand_str in self.registers:
            return self._get_register_value(operand_str)
        elif operand_str.startswith("[") and operand_str.endswith("]"):
            mem_address = int(operand_str[1:-1])
            if 0 <= mem_address < len(self.memory):
                return self.memory[mem_address]
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            return BabbageNumber(int(operand_str))

    # ========================================================================
    # Arithmetic Operations (Mill)
    # ========================================================================

    def _execute_NOP(self):
        """No operation (1 cycle, no effect)."""
        pass

    def _execute_ADD(self, reg_dest, operand):
        """Add: reg_dest += operand (8 cycles)."""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        result = val1 + val2
        if result._check_overflow():
            self.flags["OVERFLOW"] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)

    def _execute_SUB(self, reg_dest, operand):
        """Subtract: reg_dest -= operand (8 cycles)."""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        result = val1 - val2
        if result._check_overflow():
            self.flags["OVERFLOW"] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)

    def _execute_MULT(self, reg_dest, operand):
        """
        Multiply: A = upper 50 digits, D = lower 50 digits (400 cycles).

        Babbage's multiplication produces a 100-digit result from two 50-digit
        numbers. The result is split between registers A (high 50 digits) and
        D (low 50 digits).

        Implementation:
          1. Get unscaled integer parts of operands (remove 10^40 scaling)
          2. Multiply: conceptual product is up to 100 digits
          3. Split at digit position 50
          4. Store high part in A, low part in D
        """
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)

        # Extract unscaled integer parts (conceptual 50-digit values)
        unscaled_val1_int = val1.value // (10**40)
        unscaled_val2_int = val2.value // (10**40)

        # Calculate conceptual product (up to 100 digits)
        conceptual_integer_product = unscaled_val1_int * unscaled_val2_int

        # Split into 50-digit chunks
        upper_50_digits = conceptual_integer_product // (10**50)
        lower_50_digits = conceptual_integer_product % (10**50)

        # Convert back to BabbageNumber
        A_result = BabbageNumber(upper_50_digits)
        D_result = BabbageNumber(lower_50_digits)

        if A_result._overflow_flag or D_result._overflow_flag:
            self.flags["OVERFLOW"] = True

        self._set_register_value("A", A_result)
        self._set_register_value("D", D_result)
        self._update_flags(A_result)

    def _execute_DIV(self, reg_dest, operand):
        """Divide: reg_dest /= operand with fixed-point scaling (750 cycles)."""
        val1 = self._get_register_value(reg_dest)
        val2 = self._get_operand_value(operand)
        if val2 == BabbageNumber(0):
            raise ZeroDivisionError("Division by zero")
        result = val1 / val2
        if result._check_overflow():
            self.flags["OVERFLOW"] = True
        self._set_register_value(reg_dest, result)
        self._update_flags(result)

    def _execute_SQRT(self, reg_dest):
        """
        Square root: reg_dest = sqrt(reg_dest) using Newton's method (250 cycles).

        Implementation:
          1. Use initial guess: sqrt(unscaled_value) × 10^40
          2. Newton iteration: x_new = (x + N/x) / 2
          3. Iterate until convergence (100 max iterations)
          4. Return integer result (truncates fractional part)
        """
        val = self._get_register_value(reg_dest)

        if val.value < 0:
            raise ValueError("Cannot take square root of negative number")
        if val.value == 0:
            self._set_register_value(reg_dest, BabbageNumber(0))
            self._update_flags(BabbageNumber(0))
            return

        N = val.value  # Scaled value

        # Initial guess based on unscaled value
        unscaled_N = N / (10**40)
        if unscaled_N > 0:
            x = int(unscaled_N**0.5 * (10**40))
        else:
            x = 1 * (10**40)

        if x == 0:
            x = 1

        # Newton's method iteration
        for _ in range(100):
            term_N_div_x = (N * (10**40)) // x
            x_new = (x + term_N_div_x) // 2

            if abs(x_new - x) < 10:  # Convergence threshold
                break
            x = x_new

        sqrt_val = BabbageNumber(0)
        sqrt_val.value = x

        self._set_register_value(reg_dest, sqrt_val)
        self._update_flags(sqrt_val)

    # ========================================================================
    # Memory Operations (Store)
    # ========================================================================

    def _execute_LOAD(self, reg_dest, address):
        """Load from store: reg_dest = memory[address] or immediate (15 cycles)."""
        if address.startswith("[") and address.endswith("]"):
            mem_address = int(address[1:-1])
            if 0 <= mem_address < len(self.memory):
                value = self.memory[mem_address]
                self._set_register_value(reg_dest, value)
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            # Immediate value
            value = BabbageNumber(int(address))
            self._set_register_value(reg_dest, value)
        self._update_flags(self._get_register_value(reg_dest))

    def _execute_STOR(self, reg_source, address):
        """Store to store: memory[address] = reg_source (15 cycles)."""
        if address.startswith("[") and address.endswith("]"):
            mem_address = int(address[1:-1])
            if 0 <= mem_address < len(self.memory):
                value = self._get_register_value(reg_source)
                self.memory[mem_address] = value
            else:
                raise IndexError(f"Memory address out of bounds: {mem_address}")
        else:
            raise ValueError(f"Invalid address for STOR instruction: {address}")
        self._update_flags(self._get_register_value(reg_source))

    # ========================================================================
    # Control Flow (Sequencing)
    # ========================================================================

    def _execute_JMP(self, address):
        """Jump: PC = address unconditionally (4 cycles)."""
        self.PC = int(address)
        return True  # Indicate PC was modified

    def _execute_JZ(self, address):
        """Jump if zero: PC = address if ZERO flag set (4 cycles)."""
        if self.flags["ZERO"]:
            self.PC = int(address)
            return True
        return False

    def _execute_JNZ(self, address):
        """Jump if not zero: PC = address if ZERO flag clear (4 cycles)."""
        if not self.flags["ZERO"]:
            self.PC = int(address)
            return True
        return False

    def _execute_JLT(self, address):
        """Jump if less than: PC = address if LESS flag set (4 cycles)."""
        if self.flags["LESS"]:
            self.PC = int(address)
            return True
        return False

    def _execute_JGT(self, address):
        """Jump if greater than: PC = address if GREATER flag set (4 cycles)."""
        if self.flags["GREATER"]:
            self.PC = int(address)
            return True
        return False

    def _execute_JLE(self, address):
        """Jump if less or equal: PC = address if LESS or EQUAL (4 cycles)."""
        if self.flags["LESS"] or self.flags["EQUAL"]:
            self.PC = int(address)
            return True
        return False

    def _execute_JGE(self, address):
        """Jump if greater or equal: PC = address if GREATER or EQUAL (4 cycles)."""
        if self.flags["GREATER"] or self.flags["EQUAL"]:
            self.PC = int(address)
            return True
        return False

    def _execute_CMP(self, operand1, operand2):
        """Compare: set flags based on operand1 vs operand2 (4 cycles)."""
        val1 = self._get_operand_value(operand1)
        val2 = self._get_operand_value(operand2)
        comparison_result = 0
        if val1 > val2:
            comparison_result = 1
        elif val1 < val2:
            comparison_result = -1
        self._update_flags(val1, val2, comparison_result)

    def _execute_CALL(self, address):
        """Call subroutine: push return address, jump to address (8 cycles)."""
        if len(self.return_stack) >= 16:
            raise OverflowError("Return stack overflow (max 16 levels)")
        self.return_stack.append(self.PC + 1)  # Save next instruction address
        self.PC = int(address)
        return True  # PC was modified

    def _execute_RET(self):
        """Return from subroutine: pop return address, jump to it (4 cycles)."""
        if len(self.return_stack) == 0:
            raise IndexError("Return stack underflow")
        self.PC = self.return_stack.pop()
        return True  # PC was modified

    # ========================================================================
    # Stack Operations
    # ========================================================================

    def _execute_PUSH(self, reg_source):
        """Push register onto stack (4 cycles)."""
        value = self._get_register_value(reg_source)
        self.data_stack.append(value)

    def _execute_POP(self, reg_dest):
        """Pop value from stack into register (4 cycles)."""
        if not self.data_stack:
            raise IndexError("Data stack underflow")
        value = self.data_stack.pop()
        self._set_register_value(reg_dest, value)

    # ========================================================================
    # I/O Operations (Ingress/Egress)
    # ========================================================================

    def _execute_RDCRD(self, reg_dest, value=None):
        """Read punch card: load value from input card into register (30 cycles)."""
        if value is None:
            value = 42  # Default/placeholder
        self._set_register_value(reg_dest, BabbageNumber(value))

    def _execute_WRPCH(self, reg_source):
        """Write punch card: output register value to result card (30 cycles)."""
        value = self._get_register_value(reg_source)
        self.result_cards.append(
            {
                "value": value,
                "opcode": "WRPCH",
                "operands": [reg_source],
                "clock_time": self.clock_time,
            }
        )
        print(f"WRPCH: {value.to_decimal()}")

    def _execute_WRPRN(self, reg_source):
        """Write printer: output register value to printer (2 cycles)."""
        value = self._get_register_value(reg_source)
        self.result_cards.append(
            {
                "value": value,
                "opcode": "WRPRN",
                "operands": [reg_source],
                "clock_time": self.clock_time,
            }
        )
        print(f"WRPRN: {value.to_decimal()}")

    # ========================================================================
    # Instruction Execution & Control
    # ========================================================================

    def execute_instruction(self, instruction):
        """
        Execute single instruction and update clock time.

        Parameters:
            instruction: Instruction object with opcode and operands

        Updates:
            - PC (if jump/call/return)
            - clock_time (based on opcode timing)
            - registers, memory, and flags (per opcode)
        """
        opcode_name = instruction.opcode
        operands = instruction.operands

        pc_modified = False
        handler = self._opcode_handlers.get(opcode_name)
        if handler:
            # Control flow instructions modify PC directly
            if opcode_name in ["JMP", "JZ", "JNZ", "JLT", "JGT", "JLE", "JGE", "CALL", "RET"]:
                pc_modified = handler(*operands)
            else:
                handler(*operands)
        else:
            raise NotImplementedError(f"Opcode {opcode_name} not implemented")

        # Update clock time based on instruction cost
        time_cost = TIMING_TABLE.get(opcode_name, 0)
        self.clock_time += time_cost

        # Log if tracing enabled
        if self.trace_enabled:
            print(f"Clock: {self.clock_time}s | {opcode_name} | Operands: {operands}")

        # Increment PC only if not modified by jump/call/return
        if not pc_modified:
            self.PC += 1

    def load_program(self, filename):
        """
        Load program from file with label resolution.

        File format:
          - Lines starting with # are comments
          - Labels: LABEL: at start of line
          - Instructions: OPCODE operands

        Two-pass assembly:
          Pass 1: Identify label positions
          Pass 2: Resolve label references in jump/call instructions
        """
        instructions = []
        labels = {}
        program_lines = []

        # Read and strip comments
        with open(filename, "r") as f:
            for line_num, line in enumerate(f, 0):
                original_line = line.strip()
                line = line.split("#")[0].strip()
                if not line:
                    continue
                program_lines.append((line_num, line, original_line))

        # Pass 1: Identify labels and store instructions
        current_instruction_address = 0
        for line_num, line, original_line in program_lines:
            if ":" in line:
                label_name = line.split(":")[0].strip()
                labels[label_name] = current_instruction_address
                line = line.split(":", 1)[1].strip()
                if not line:
                    continue

            parts = line.split()
            if not parts:
                continue

            opcode_name = parts[0]
            operands = parts[1:] if len(parts) > 1 else []
            instructions.append(Instruction(opcode_name, operands))
            current_instruction_address += 1

        # Pass 2: Resolve label references
        for i, instruction in enumerate(instructions):
            new_operands = []
            for operand in instruction.operands:
                if operand in labels:
                    new_operands.append(str(labels[operand]))
                else:
                    new_operands.append(operand)
            instruction.operands = new_operands

        self.instruction_cards = instructions

    def run(self):
        """Execute loaded program until completion or error."""
        self.running = True
        while self.running and self.PC < len(self.instruction_cards):
            if self.paused and self.interactive_mode:
                user_input = input("Emulator paused. Press Enter to continue or 's' to step: ")
                if user_input == "s":
                    self.paused = False
                    self.step_one_instruction()
                    self.paused = True
                else:
                    self.paused = False
            else:
                self.step_one_instruction()

    def step_one_instruction(self):
        """Execute a single instruction with breakpoint checking."""
        if self.PC >= len(self.instruction_cards):
            self.running = False
            return

        instruction = self.instruction_cards[self.PC]
        self.check_breakpoints()
        if self.paused:
            return

        try:
            self.execute_instruction(instruction)
            if instruction.opcode == "HALT":
                self.running = False
        except Exception as e:
            print(f"Runtime Error at PC {self.PC}: {e}")
            self.running = False

    def set_breakpoint(self, condition_type, target):
        """
        Set breakpoint for debugging.

        Types:
          - address: Break at specific PC
          - time: Break at clock time >= target
          - register: Break when register value matches
          - memory: Break when memory value matches
        """
        self.breakpoints.append({"type": condition_type, "target": target, "enabled": True})

    def check_breakpoints(self):
        """Check all breakpoints and pause if triggered."""
        for bp in self.breakpoints:
            if not bp["enabled"]:
                continue

            if bp["type"] == "address" and self.PC == bp["target"]:
                self.paused = True
                print(f"Breakpoint hit: PC={self.PC}")

            elif bp["type"] == "time" and self.clock_time >= bp["target"]:
                self.paused = True
                print(f"Breakpoint hit: Clock={self.clock_time}s")

            elif bp["type"] == "register" and self._get_register_value(bp["reg"]) == BabbageNumber(
                bp["target"]
            ):
                self.paused = True
                print(f"Breakpoint hit: {bp['reg']}={bp['target']}")

            elif bp["type"] == "memory" and self.memory[bp["address"]] == BabbageNumber(
                bp["target"]
            ):
                self.paused = True
                print(f"Breakpoint hit: Memory[{bp['address']}]={bp['target']}")

    def dump_state(self):
        """Return human-readable state dump for debugging."""
        state = f"""
--- EMULATOR STATE ---
PC: {self.PC}
Clock: {self.clock_time}s
Registers:
  A: {self.registers['A'].to_decimal()}
  B: {self.registers['B'].to_decimal()}
  C: {self.registers['C'].to_decimal()}
  D: {self.registers['D'].to_decimal()}
Flags: {self.flags}
Return Stack: {self.return_stack}
Memory (first 10 words):
"""
        for i in range(10):
            state += f"  [{i}]: {self.memory[i].to_decimal()}\n"
        return state

    def save_result_cards(self, filename):
        """Save result cards to file in punch card format."""
        with open(filename, "w") as f:
            for i, result_data in enumerate(self.result_cards):
                value = result_data["value"]
                opcode = result_data["opcode"]
                operands = ", ".join(result_data["operands"])
                clock_time = result_data["clock_time"]

                f.write(f"# RESULT CARD {i+1}\n")
                f.write(f"# Instruction: {opcode} {operands}\n")
                f.write(f"# Value: {value.to_decimal()}\n")
                f.write(f"# Timestamp: Clock {clock_time}s\n")
                f.write(f"\n{value.to_card_format()}\n\n")

    def save_trace(self, filename):
        """Save execution trace to file (placeholder for full implementation)."""
        print(f"Trace saving not fully implemented. Would save to {filename}")
