"""
Babbage Analytical Engine Emulator

A faithful mechanical simulation of Charles Babbage's Analytical Engine (AE),
based on Menabrea/Lovelace (1842-1843) and Babbage's mechanical notation.

Architecture:
  - Store: 2,000 x 50-digit decimal numbers (memory)
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
  - Scaling: value x 10^40 (40 fractional digits internally)
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
  - NOP: 0, ADD: 8, SUB: 8, MULT: 400, DIV: 750, SQRT: 250, RDCRD: 30, WRPCH: 30, WRPRN: 2, CALL: 8, RET: 4, PUSH: 4, POP: 4
  - Used for performance analysis and historical accuracy

References:
  - Menabrea, L. F., & Lovelace, A. A. (1843). "Note on the Analytical Engine"
  - Babbage, C. (1826). "Mechanical Notation"
  - SMG Technical Description (DE2 comparison)
"""

from typing import Optional, Any, Dict, List
from .barrels import BarrelController, MicroOp
from .types import BabbageNumber

class MechanicalFailureError(Exception):
    """Raised when the engine suffers a physical breakdown (seizure, breakage, etc.)"""
    pass

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
    "OIL": 100,
}


class Instruction:
    """Analytical Engine instruction card."""

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
        # Adjusted for scaled value
        max_internal_value = (10**50 - 1) * (10**40)
        min_internal_value = -(10**50 - 1) * (10**40)

        if self.value > max_internal_value or self.value < min_internal_value:
            self._overflow_flag = True
            # For now, let's keep the flag and not modify value on overflow here.
            return True
        return False

    def __add__(self, other):
        """Addition with overflow check."""
        result = BabbageNumber(0)
        other_babbage = BabbageNumber(other)
        result.value = self.value + other_babbage.value
        result._check_overflow()
        return result

    def __sub__(self, other):
        """Subtraction with overflow check."""
        result = BabbageNumber(0)
        other_babbage = BabbageNumber(other)
        result.value = self.value - other_babbage.value
        result._check_overflow()
        return result

    def __mul__(self, other):
        """Multiplication with overflow check."""
        result = BabbageNumber(0)
        # Multiply scaled integer values, then scale back by 10^40
        result.value = (self.value * BabbageNumber(other).value) // (10**40)
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
    - 2,000 x word memory (store) with 50-digit decimal numbers
    - 4 arithmetic registers (A, B, C, D) for mill operations
    - Program counter for instruction sequencing
    - Flag registers (ZERO, SIGN, OVERFLOW, comparison flags)
    - Return and data stacks for control flow
    - Breakpoint system for debugging
    - Punch card I/O simulation
    """

    def __init__(self, physical_engine=None):
        """Initialize empty engine."""
        self.physical_engine = physical_engine
        # Memory: Store of 2,000 50-digit decimal numbers
        self.memory = [BabbageNumber(0) for _ in range(2000)]

        # Registers: Mill components for arithmetic
        self.registers = {
            "A": BabbageNumber(0),  # Accumulator
            "B": BabbageNumber(0),  # Operand register
            "C": BabbageNumber(0),  # Auxiliary
            "D": BabbageNumber(0),  # Auxiliary
        }
        # Micro-Architectural State for Barrel Execution
        self.mill_operand_buffer = BabbageNumber(0)  # Ingress Axis
        self.mill_result_buffer = BabbageNumber(0)  # Egress Axis
        self.active_store_address: Optional[int] = None  # V.A. (Variable Axis) card
        # New Micro-Architectural State for Multiplication
        self.mill_product_accumulator = BabbageNumber(0)  # Intermediate product
        self.mill_multiplier_digit_buffer = 0  # Current multiplier digit
        self.mill_counter = 0  # Generic counter for loops
        # New Micro-Architectural State for Division
        self.mill_divisor_buffer = BabbageNumber(0)  # Holds the divisor for division ops
        self.mill_quotient_buffer = BabbageNumber(0)  # Builds the quotient during division
        self.mill_remainder_buffer = BabbageNumber(0)  # Holds the remainder during division

        # Control: Program sequencing and timing
        self.PC = 0  # Program Counter
        self.running = True  # Execution flag
        self.clock_time = 0  # Simulated elapsed time

        # I/O: Punch cards and results
        self.instruction_cards = []  # Loaded program
        self.result_cards = []  # Output results
        self.execution_trace = []  # Detailed execution trace

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

        # Barrels
        self.barrels = BarrelController()

        # Instruction handlers: Maps opcode to execution function
        self._opcode_handlers = {
            "NOP": self._execute_NOP,
            "ADD": self._execute_ADD_micro,
            "SUB": self._execute_SUB_micro,
            "MULT": self._execute_MULT_micro,  # Now using micro-programmed MULT
            "DIV": self._execute_DIV_micro,
            "SQRT": self._execute_SQRT_micro,
            "LOAD": self._execute_LOAD_micro,
            "STOR": self._execute_STOR_micro,
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
            "SHL": self._execute_SHL_micro,
            "SHR": self._execute_SHR_micro,
            "AND": self._execute_AND_micro,
            "OR": self._execute_OR_micro,
            "XOR": self._execute_XOR_micro,
            "CHKS": self._execute_CHKS_micro,
            "HALT": self._execute_HALT,
            "PLAY": self._execute_PLAY_micro,
            "SETMODE": self._execute_SETMODE_micro,
            "OIL": self._execute_OIL,
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

    def _execute_ADD_micro(self, reg_dest: str, operand_src: Any):
        """Execute ADD using micro-ops (Hybrid Tier 1/3)."""
        self.barrels.select_barrel("ADD")

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_dest, operand_src)  # Pass context

        # Final step in ADD micro-program: Transfer result from Egress to Mill Reg A
        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.mill_result_buffer)

    def _execute_SUB_micro(self, reg_dest: str, operand_src: Any):
        """Execute SUB using micro-ops."""
        self.barrels.select_barrel("SUB")

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_dest, operand_src)  # Pass context

        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.mill_result_buffer)

    def _execute_micro_op(self, op: MicroOp, reg_dest: str, operand_src: Any):
        """Execute a single physical micro-operation, modifying engine state."""
        # This translates Barrel "studs" to mechanical actions.
        if op == MicroOp.LIFT_STORE_AXIS:
            # Assume Variable Axis card is read, setting active_store_address
            # For now, let's just resolve the operand as the source address.
            if isinstance(operand_src, str) and operand_src.startswith("["):
                self.active_store_address = int(operand_src[1:-1])
            # If reg_dest is used as operand_src, it is handled via CONNECT_STORE_TO_MILL path.

        elif op == MicroOp.CONNECT_STORE_TO_MILL:
            if self.active_store_address is not None:
                if 0 <= self.active_store_address < len(self.memory):
                    self.mill_operand_buffer = self.memory[self.active_store_address]
                else:
                    raise IndexError(f"Memory address out of bounds: {self.active_store_address}")
            else:  # This path is for immediate values or other non-memory sources for now.
                # For example, if the operand_src itself is an immediate number (e.g., '10')
                self.mill_operand_buffer = self._get_operand_value(operand_src)

        elif op == MicroOp.ADVANCE_MILL:
            # This is where the actual arithmetic happens based on instruction context
            current_mill_value = self.registers[reg_dest]  # First operand is in A or specified dest

            # Check if we are in a MULT context (using mill_product_accumulator)
            if self.barrels.active_barrel == "MULT":
                # Multiplicand is in mill_operand_buffer? No, let's assume it's loaded.
                # In our MULT barrel, we LOAD_MILL_ACCUMULATOR first.
                # Let's adjust: In MULT, ADVANCE_MILL adds operand to product_accumulator
                self.mill_product_accumulator += self.mill_operand_buffer
                self.mill_result_buffer = self.mill_product_accumulator
            elif self.barrels.active_barrel == "LOAD":
                # Pure transfer from Ingress Axis to Egress Axis
                self.mill_result_buffer = self.mill_operand_buffer
            else:
                # Perform addition in the mill
                self.mill_result_buffer = current_mill_value + self.mill_operand_buffer

        elif op == MicroOp.REVERSE_MILL:
            current_mill_value = self.registers[reg_dest]
            if self.barrels.active_barrel == "DIV":
                # Only subtract if GREATER or EQUAL (set by COMPARE_MILL_BUFFERS)
                if self.flags["GREATER"] or self.flags["EQUAL"]:
                    self.mill_remainder_buffer = (
                        self.mill_remainder_buffer - self.mill_operand_buffer
                    )
            else:
                self.mill_result_buffer = current_mill_value - self.mill_operand_buffer

        elif op == MicroOp.RUN_CARRY:
            # Simulate the anticipating carriage for the mill
            # BabbageNumber already handles carry internally, so this is symbolic.
            pass

        elif op == MicroOp.LIFT_EGRESS:
            # Prepares Egress Axis to receive result
            pass

        elif op == MicroOp.CONNECT_MILL_TO_STORE:
            # Transfer from Egress to Store
            if self.active_store_address is not None:
                self.memory[self.active_store_address] = self.mill_result_buffer

        elif op == MicroOp.DROP_STORE_AXIS:
            self.active_store_address = None

        elif op == MicroOp.DROP_INGRESS:
            self.mill_operand_buffer = BabbageNumber(0)

        elif op == MicroOp.DROP_EGRESS:
            self.mill_result_buffer = BabbageNumber(0)

        elif op == MicroOp.LOAD_MILL_ACCUMULATOR:
            self.mill_product_accumulator = self.registers[reg_dest]

        elif op == MicroOp.STORE_MILL_ACCUMULATOR:
            self.registers[reg_dest] = self.mill_product_accumulator

        elif op == MicroOp.SHIFT_MILL_LEFT:
            self.mill_product_accumulator = self.mill_product_accumulator * BabbageNumber(10)

        elif op == MicroOp.SHIFT_MILL_RIGHT:
            self.mill_product_accumulator = self.mill_product_accumulator / BabbageNumber(10)

        elif op == MicroOp.GET_MULTIPLIER_DIGIT:
            # Extract current units digit and set as mill_counter
            multiplier_val = int(abs(self._get_operand_value(operand_src).to_decimal()))
            self.mill_multiplier_digit_buffer = multiplier_val % 10
            self.mill_counter = self.mill_multiplier_digit_buffer

        elif op == MicroOp.DECREMENT_COUNTER:
            print(f"DECREMENT_COUNTER: {self.mill_counter} -> {self.mill_counter - 1}")
            self.mill_counter -= 1

        elif op == MicroOp.COMPARE_MILL_BUFFERS:
            # For division, compare current dividend part (mill_result_buffer) with divisor (mill_operand_buffer)
            self.flags["GREATER"] = (
                self.mill_remainder_buffer > self.mill_operand_buffer
            )  # Compare remainder buffer with operand buffer
            self.flags["LESS"] = self.mill_remainder_buffer < self.mill_operand_buffer
            self.flags["EQUAL"] = self.mill_remainder_buffer == self.mill_operand_buffer

        elif op == MicroOp.SHIFT_MILL_DIVISOR:
            # Shift the divisor (mill_operand_buffer) to align it for subtraction
            self.mill_operand_buffer = self.mill_operand_buffer * BabbageNumber(10)

        elif op == MicroOp.INCREMENT_QUOTIENT_DIGIT:
            # Increment the current digit of the quotient being built
            # For multi-digit division, we add the current position value (e.g. 10^k)
            inc_val = getattr(self, "mill_quotient_digit_value", BabbageNumber(1))
            self.mill_quotient_buffer += inc_val


        elif op == MicroOp.RESET_REMAINDER:
            self.mill_remainder_buffer = BabbageNumber(0)

        elif op == MicroOp.JUMP_RELATIVE:
            # For simplicity, assume the 'offset' is passed via some mechanism.
            # In a real AE, this would be a mechanical linkage.
            # We'll use a fixed offset for specific use cases here.
            pass

        elif op == MicroOp.REPEAT_IF_COUNTER:
            if self.mill_counter > 0:
                # Jump back to the loop body row
                self.barrels.step_index = self.barrels.step_index - 2
            else:
                pass

        elif op == MicroOp.REPEAT_IF_GE:
            if self.flags["GREATER"] or self.flags["EQUAL"]:
                # Jump back to STEP 4 (COMPARE_MILL_BUFFERS)
                self.barrels.step_index = 4
            else:
                pass

        elif op == MicroOp.JUMP_IF_LT:
            if self.flags["LESS"]:
                # Jump forward to STEP 9 (SHIFT_MILL_DIVISOR)
                # Current step_index is 6 (after step 5). We want to go to 9.
                self.barrels.step_index = 9
            else:
                pass



        elif op == MicroOp.INIT_SQRT_GUESS:
            # First guess: x_0 = S / 2 (conceptual)
            val = self.registers[reg_dest]
            self.mill_product_accumulator = val / BabbageNumber(2)
            self.mill_operand_buffer = val # Keep original S in operand buffer

        elif op == MicroOp.DIVIDE_S_BY_X:
            # mill_result_buffer = S / x_n
            # S is in mill_operand_buffer, x_n is in mill_product_accumulator
            if self.mill_product_accumulator == BabbageNumber(0):
                self.mill_result_buffer = BabbageNumber(0)
            else:
                self.mill_result_buffer = self.mill_operand_buffer / self.mill_product_accumulator

        elif op == MicroOp.ADD_X_AND_QUOTIENT:
            # x_n = x_n + (S / x_n)
            # x_n is in mill_product_accumulator, S/x_n is in mill_result_buffer
            self.mill_product_accumulator = self.mill_product_accumulator + self.mill_result_buffer

        elif op == MicroOp.HALVE_ACCUMULATOR:
            # x_{n+1} = accumulator / 2
            self.mill_product_accumulator = self.mill_product_accumulator / BabbageNumber(2)

        elif op == MicroOp.CHECK_SQRT_CONVERGENCE:
            # Symbolic convergence check: for now, fixed 25 iterations or < epsilon
            # In a real AE, this might be a mechanical detector or just fixed iterations.
            # We'll use a counter for iterations.
            if not hasattr(self, "mill_sqrt_iterations"):
                self.mill_sqrt_iterations = 0
            self.mill_sqrt_iterations += 1
            if self.mill_sqrt_iterations < 25:
                # Jump back to step 1 (DIVIDE_S_BY_X)
                self.barrels.step_index = 1
            else:
                self.mill_sqrt_iterations = 0 # Reset for next SQRT

        elif op == MicroOp.STORE_SQRT_RESULT:
            self.registers[reg_dest] = self.mill_product_accumulator

        elif op == MicroOp.BITWISE_AND:
            val1 = self.registers[reg_dest]
            val2 = self._get_operand_value(operand_src)
            s1 = str(abs(val1.value)).zfill(50)
            s2 = str(abs(val2.value)).zfill(50)
            res = int("".join(str(int(d1) & int(d2)) for d1, d2 in zip(s1, s2)))
            self.mill_result_buffer = BabbageNumber(0)
            self.mill_result_buffer.value = res if val1.value >= 0 else -res

        elif op == MicroOp.BITWISE_OR:
            val1 = self.registers[reg_dest]
            val2 = self._get_operand_value(operand_src)
            s1 = str(abs(val1.value)).zfill(50)
            s2 = str(abs(val2.value)).zfill(50)
            res = int("".join(str(int(d1) | int(d2)) for d1, d2 in zip(s1, s2)))
            self.mill_result_buffer = BabbageNumber(0)
            self.mill_result_buffer.value = res if val1.value >= 0 else -res

        elif op == MicroOp.BITWISE_XOR:
            val1 = self.registers[reg_dest]
            val2 = self._get_operand_value(operand_src)
            s1 = str(abs(val1.value)).zfill(50)
            s2 = str(abs(val2.value)).zfill(50)
            res = int("".join(str(int(d1) ^ int(d2)) for d1, d2 in zip(s1, s2)))
            self.mill_result_buffer = BabbageNumber(0)
            self.mill_result_buffer.value = res if val1.value >= 0 else -res

        elif op == MicroOp.COMPUTE_CHECKSUM:
            val = self.registers[reg_dest]
            stored_checksum = val.value % 10
            s = str(abs(val.value // 10)).zfill(49)
            computed_c = sum(int(d) for d in s) % 10
            if stored_checksum != computed_c:
                self.flags["ERROR"] = True
                print(f"CHECKSUM ERROR: Expected {computed_c}, found {stored_checksum}")
            else:
                self.flags["ERROR"] = False

        elif op == MicroOp.PLAY_NOTE:
            note = getattr(self, "mill_note_buffer", "C4")
            duration = getattr(self, "mill_duration_buffer", "8")
            print(f"LOVELACE MUSIC ENGINE: Playing {note} for {duration} cycles")

        elif op == MicroOp.SET_MODE_SYMBOLIC:
            mode = getattr(self, "mill_mode_buffer", "numeric")
            self.mode = mode
            print(f"ENGINE MODE: {mode}")

        # Other MicroOps (Fetch, etc.) can be implemented here as needed
        # For now, FETCH_VAR_CARD is a symbolic trigger.

    def _execute_MULT_micro(self, reg_dest: str, operand_src: Any):
        """
        Execute MULT using micro-ops, leveraging repeated additions and shifts.
        """
        self.barrels.select_barrel("MULT")
        
        # Setup: multiplicand into operand_buffer
        self.mill_operand_buffer = self._get_operand_value(reg_dest)
        # Clear product accumulator (conceptual reset)
        self.mill_product_accumulator = BabbageNumber(0)

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_dest, operand_src)

        # Store result back from egress/accumulator
        self.registers[reg_dest] = self.mill_product_accumulator
        self._update_flags(self.mill_product_accumulator)

    def _execute_DIV_micro(self, reg_dest: str, operand_src: Any):
        """
        Execute DIV using micro-ops, leveraging repeated subtractions and shifts.
        """
        dividend = self._get_operand_value(reg_dest)
        divisor = self._get_operand_value(operand_src)

        if divisor == BabbageNumber(0):
            raise ZeroDivisionError("Division by zero")

        self.mill_remainder_buffer = dividend
        self.mill_quotient_buffer = BabbageNumber(0)

        # Determine initial alignment
        dividend_int = int(abs(dividend.to_decimal()))
        divisor_int = int(abs(divisor.to_decimal()))

        initial_shift = 0
        if dividend_int != 0 and divisor_int != 0:
            initial_shift = len(str(dividend_int)) - len(str(divisor_int))
            if initial_shift > 0 and dividend < (divisor * BabbageNumber(10**initial_shift)):
                initial_shift -= 1
        elif dividend_int == 0:
            initial_shift = 0

        # Outer loop for each quotient digit
        for k in range(initial_shift, -1, -1): # Integer division only
            if k < 0: break
            
            self.mill_operand_buffer = divisor * BabbageNumber(10**k)
            self.mill_quotient_digit_value = BabbageNumber(10**k)
            
            self.barrels.select_barrel("DIV")
            # We skip the first 4 steps of the DIV barrel (setup) because we did it in Python
            self.barrels.step_index = 4 

            # Micro-op execution loop for ONE digit position
            while self.barrels.active_barrel:
                ops = self.barrels.step()
                if not ops: break
                for op in ops:
                    self._execute_micro_op(op, reg_dest, operand_src)
                # If we hit step 9 (SHIFT_MILL_DIVISOR), we are done with this digit
                if self.barrels.step_index >= 9:
                    self.barrels.active_barrel = None

        # Final storage
        self.registers[reg_dest] = self.mill_quotient_buffer
        self._set_register_value("D", self.mill_remainder_buffer)
        self._update_flags(self.mill_quotient_buffer)

    def _execute_SQRT_micro(self, reg_dest):
        """Execute SQRT using micro-ops (Newton-Raphson)."""
        val = self.registers[reg_dest]
        if val.value < 0:
            raise ValueError("Cannot take square root of negative number")

        self.barrels.select_barrel("SQRT")

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_dest, None)

        self._update_flags(self.registers[reg_dest])

    def _execute_LOAD_micro(self, reg_dest, address_src):
        """Execute LOAD using micro-ops."""
        self.barrels.select_barrel("LOAD")

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_dest, address_src)

        # Final result transfer if not fully handled by micro-ops
        # In our simplified LOAD micro-op sequence, we still need to store in reg_dest
        # unless ADVANCE_MILL handled it.
        # Let's assume mill_result_buffer has it.
        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.registers[reg_dest])

    def _execute_STOR_micro(self, reg_source, address_dest):
        """Execute STOR using micro-ops."""
        if not (isinstance(address_dest, str) and address_dest.startswith("[") and address_dest.endswith("]")):
            raise ValueError(f"Invalid address for STOR instruction: {address_dest}")

        self.barrels.select_barrel("STOR")

        # Setup: source register into egress (conceptual)
        self.mill_result_buffer = self.registers[reg_source]

        # Micro-op execution loop
        while self.barrels.active_barrel:
            ops = self.barrels.step()
            for op in ops:
                self._execute_micro_op(op, reg_source, address_dest)

    def _execute_SHL_micro(self, reg_dest, n_str):
        n = int(n_str)
        self.mill_product_accumulator = self.registers[reg_dest]
        for _ in range(n):
            self.barrels.select_barrel("SHL")
            while self.barrels.active_barrel:
                for op in self.barrels.step():
                    self._execute_micro_op(op, reg_dest, None)
        self.registers[reg_dest] = self.mill_product_accumulator
        self._update_flags(self.registers[reg_dest])

    def _execute_SHR_micro(self, reg_dest, n_str):
        n = int(n_str)
        self.mill_product_accumulator = self.registers[reg_dest]
        for _ in range(n):
            self.barrels.select_barrel("SHR")
            while self.barrels.active_barrel:
                for op in self.barrels.step():
                    self._execute_micro_op(op, reg_dest, None)
        self.registers[reg_dest] = self.mill_product_accumulator
        self._update_flags(self.registers[reg_dest])

    def _execute_AND_micro(self, reg_dest, operand_src):
        self.barrels.select_barrel("AND")
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, reg_dest, operand_src)
        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.registers[reg_dest])

    def _execute_OR_micro(self, reg_dest, operand_src):
        self.barrels.select_barrel("OR")
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, reg_dest, operand_src)
        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.registers[reg_dest])

    def _execute_XOR_micro(self, reg_dest, operand_src):
        self.barrels.select_barrel("XOR")
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, reg_dest, operand_src)
        self.registers[reg_dest] = self.mill_result_buffer
        self._update_flags(self.registers[reg_dest])

    def _execute_CHKS_micro(self, reg_source):
        self.barrels.select_barrel("CHKS")
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, reg_source, None)

    def _execute_PLAY_micro(self, note, duration):
        self.barrels.select_barrel("PLAY")
        # Store params for micro-op to consume
        self.mill_note_buffer = note
        self.mill_duration_buffer = duration
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, None, None)

    def _execute_SETMODE_micro(self, mode):
        self.barrels.select_barrel("SETMODE")
        self.mill_mode_buffer = mode
        while self.barrels.active_barrel:
            for op in self.barrels.step():
                self._execute_micro_op(op, None, None)

    def _execute_HALT(self):
        self.running = False
        return False

    def _execute_OIL(self):
        """Perform maintenance: reset wear and friction states if physics is linked."""
        if self.physical_engine:
            # Reset wear volumes and reduce friction
            st = self.physical_engine.state
            st.bearing_wear_volumes_mm3 = [0.0] * len(st.bearing_wear_volumes_mm3)
            st.gear_wear_volume_mm3 = 0.0
            print("MAINTENANCE: Engine oiled, wear volumes reset.")
        else:
            print("MAINTENANCE: Engine oiled (no physical effect, physics not linked).")
        return False

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
        self._update_flags(D_result)  # Changed to D_result for flags

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
          1. Use initial guess: sqrt(unscaled_value) x 10^40
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
        """Read punch card: load value from input card into register (30 cycles).

        If called with an explicit value (from operand), uses that value.
        Otherwise, reads from the input card queue. Raises IndexError if
        the card queue is empty (simulates a jam / out-of-cards condition).
        """
        if value is not None:
            self._set_register_value(reg_dest, BabbageNumber(value))
            return

        if not hasattr(self, "input_cards"):
            self.input_cards = []

        if not self.input_cards:
            raise IndexError(
                f"RDCRD: input card queue empty at PC={self.PC}. "
                "Load cards with engine.input_cards before execution."
            )
        card_value = self.input_cards.pop(0)
        self._set_register_value(reg_dest, BabbageNumber(card_value))

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

        # Update clock time based on instruction cost (dynamic RPM/physics coupling)
        base_cycles = TIMING_TABLE.get(opcode_name, 0)
        
        if self.physical_engine:
            # Dynamic timing model:
            # time [s] = cycles / (rotations/sec) * lag
            # lag increases with shaft deflection (simulating mechanical resistance)
            rpm = self.physical_engine.config.rpm
            deflection = self.physical_engine.state.shaft_deflection_mm
            
            # 5% time penalty per mm of shaft deflection
            lag_factor = 1.0 + (0.05 * deflection)
            time_cost = (base_cycles / (rpm / 60.0)) * lag_factor
            
            # Synchronize physical engine state to match logic engine instruction step
            self.physical_engine.run(self.physical_engine.state.time_s + time_cost)
        else:
            time_cost = base_cycles

        self.clock_time += time_cost

        # Record execution trace entry
        self.execution_trace.append({
            "pc": self.PC,
            "opcode": opcode_name,
            "operands": operands,
            "clock_time": self.clock_time,
            "registers": {k: v.to_decimal() for k, v in self.registers.items()},
            "flags": dict(self.flags)
        })

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
        """Execute a single instruction with breakpoint and physical failure checking."""
        if self.PC >= len(self.instruction_cards):
            self.running = False
            return

        # Check for mechanical failure if physical engine is linked
        if self.physical_engine and self.physical_engine.failed:
            reason = self.physical_engine.failure_reason
            raise MechanicalFailureError(f"Mechanical failure: {reason}")

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

    def physics_report(self) -> dict:
        """Return physics simulation summary, or empty dict if no physics."""
        if self.physical_engine and hasattr(self.physical_engine, "physics_report"):
            return self.physical_engine.physics_report()
        return {}

    def save_trace(self, filename):
        """Save execution trace to JSON file.

        Each entry records: PC, opcode, operands, register values, flags,
        and clock_time at the moment of execution.
        """
        import json

        state_snapshot = {
            "pc": self.PC,
            "clock_time": self.clock_time,
            "registers": {k: v.to_decimal() for k, v in self.registers.items()},
            "flags": dict(self.flags),
            "result_card_count": len(self.result_cards),
            "trace": self.execution_trace,
        }

        with open(filename, "w", encoding="utf-8") as f:
            json.dump(state_snapshot, f, indent=2)
