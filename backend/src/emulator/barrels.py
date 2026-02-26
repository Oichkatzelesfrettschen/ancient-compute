"""
Analytical Engine Micro-Architecture (Barrels & Studs)

Architecture:
- Barrel: A rotating drum with rows of studs.
- Stud: A physical pin that pushes a lever.
- Lever: Actuates a mechanical component (e.g., lift axis, engage gear).
- MicroOp: A single mechanical action (e.g., "LIFT_INGRESS", "ADVANCE_MILL").

Control Flow:
1. Operation Card (e.g., ADD) selects a "Vertical" (set of barrels).
2. Barrel rotates (Steps 0..N).
3. At each step, studs push levers.
4. Levers execute mechanical actions.
"""

from dataclasses import dataclass
from enum import Enum, auto


class MicroOp(Enum):
    # Axis Control
    LIFT_INGRESS = auto()       # Lift the Ingress Axis to engage Mill
    DROP_INGRESS = auto()       # Drop the Ingress Axis to disengage
    LIFT_EGRESS = auto()        # Lift the Egress Axis to engage Mill (result)
    DROP_EGRESS = auto()        # Drop the Egress Axis
    LIFT_STORE_AXIS = auto()    # Lift a specific Store column to engage Mill
    DROP_STORE_AXIS = auto()    # Drop a specific Store column

    # Mill Control
    CONNECT_MILL_TO_STORE = auto() # Mill output to Store input
    CONNECT_STORE_TO_MILL = auto() # Store output to Mill input (Ingress Axis)
    ADVANCE_MILL = auto()       # Rotate Mill's gears to perform addition
    REVERSE_MILL = auto()       # Rotate Mill's gears in reverse for subtraction
    RUN_CARRY = auto()          # Engage Anticipating Carriage
    RESET_MILL = auto()         # Clear Mill registers
    LOAD_MILL_ACCUMULATOR = auto() # Load value into mill's internal accumulator (for product)
    STORE_MILL_ACCUMULATOR = auto() # Store value from mill's internal accumulator
    SHIFT_MILL_LEFT = auto()    # Shift mill's product accumulator left
    SHIFT_MILL_RIGHT = auto()   # Shift mill's product accumulator right
    GET_MULTIPLIER_DIGIT = auto() # Extract current digit from multiplier
    DECREMENT_COUNTER = auto()  # Decrement a counter for repeated operations
    # MicroOps for Division
    COMPARE_MILL_BUFFERS = auto() # Compare mill_result_buffer with mill_operand_buffer (or other internal values)
    SET_FLAG_GREATER = auto()   # Set a flag if comparison was A > B
    SET_FLAG_LESS = auto()      # Set a flag if comparison was A < B
    SET_FLAG_EQUAL = auto()     # Set a flag if comparison was A == B
    SHIFT_MILL_DIVISOR = auto() # Shift mill's divisor buffer
    INCREMENT_QUOTIENT_DIGIT = auto() # Increment a quotient digit
    RESET_REMAINDER = auto()    # Reset the remainder buffer

    # MicroOps for SQRT (Newton-Raphson)
    INIT_SQRT_GUESS = auto()    # Initialize x_0 = S/2 as first guess
    DIVIDE_S_BY_X = auto()      # Compute S / x_n
    ADD_X_AND_QUOTIENT = auto() # Compute x_n + S/x_n
    HALVE_ACCUMULATOR = auto()  # Divide accumulator by 2: x_{n+1} = (x_n + S/x_n)/2
    CHECK_SQRT_CONVERGENCE = auto() # Check if |x_{n+1} - x_n| < tolerance
    STORE_SQRT_RESULT = auto()  # Store converged result

    # Tier B/C and Lovelace Extensions
    BITWISE_AND = auto()        # Digit-wise AND
    BITWISE_OR = auto()         # Digit-wise OR
    BITWISE_XOR = auto()        # Digit-wise XOR
    COMPUTE_CHECKSUM = auto()   # Verify modulo-10 checksum
    SET_MODE_SYMBOLIC = auto()  # Switch to symbolic processing
    SET_MODE_NUMERIC = auto()   # Switch to numeric processing
    PLAY_NOTE = auto()          # Simulate playing a musical note

    # Control Ops
    JUMP_RELATIVE = auto()      # Relative jump in micro-program
    REPEAT_IF_COUNTER = auto()  # Jump back if mill_counter > 0
    REPEAT_IF_GE = auto()       # Jump back if GREATER or EQUAL flag is set
    JUMP_IF_LT = auto()         # Jump forward if LESS flag is set
    FETCH_OP_CARD = auto()
    FETCH_VAR_CARD = auto()

@dataclass
class BarrelRow:
    """A vertical column of studs on a barrel (activates simultaneously)."""
    studs: set[MicroOp]

class Barrel:
    """A control barrel (drum) defining a micro-program."""
    def __init__(self, name: str):
        self.name = name
        self.rows: list[BarrelRow] = []

    def add_step(self, ops: list[MicroOp]) -> None:
        self.rows.append(BarrelRow(set(ops)))

class BarrelController:
    """Orchestrates the barrels."""
    def __init__(self) -> None:
        self.barrels: dict[str, Barrel] = {}
        self.active_barrel: str | None = None
        self.step_index = 0

        self._init_standard_barrels()

    def _init_standard_barrels(self) -> None:
        # Addition Barrel
        add = Barrel("ADD")
        add.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        add.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        add.add_step([MicroOp.LIFT_INGRESS])
        add.add_step([MicroOp.ADVANCE_MILL])
        add.add_step([MicroOp.RUN_CARRY])
        add.add_step([MicroOp.DROP_INGRESS, MicroOp.LIFT_EGRESS])
        self.barrels["ADD"] = add

        # Subtraction Barrel
        sub = Barrel("SUB")
        sub.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        sub.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        sub.add_step([MicroOp.LIFT_INGRESS])
        sub.add_step([MicroOp.REVERSE_MILL])
        sub.add_step([MicroOp.RUN_CARRY])
        sub.add_step([MicroOp.DROP_INGRESS, MicroOp.LIFT_EGRESS])
        self.barrels["SUB"] = sub

        # Multiplication Barrel
        mul = Barrel("MULT")
        mul.add_step([MicroOp.GET_MULTIPLIER_DIGIT])
        mul.add_step([MicroOp.ADVANCE_MILL, MicroOp.DECREMENT_COUNTER])
        mul.add_step([MicroOp.REPEAT_IF_COUNTER])
        mul.add_step([MicroOp.STORE_MILL_ACCUMULATOR])
        self.barrels["MULT"] = mul

        # Division Barrel
        div = Barrel("DIV")
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS,
                       MicroOp.LOAD_MILL_ACCUMULATOR])
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        # Inner loop for one digit position
        div.add_step([MicroOp.COMPARE_MILL_BUFFERS])
        div.add_step([MicroOp.JUMP_IF_LT]) # Skip if remainder < divisor
        div.add_step([MicroOp.REVERSE_MILL])
        div.add_step([MicroOp.INCREMENT_QUOTIENT_DIGIT])
        div.add_step([MicroOp.REPEAT_IF_GE]) # Repeat subtract while GE
        # Next digit
        div.add_step([MicroOp.SHIFT_MILL_DIVISOR])
        div.add_step([MicroOp.RESET_REMAINDER])
        div.add_step([MicroOp.LIFT_EGRESS])
        div.add_step([MicroOp.CONNECT_MILL_TO_STORE])
        div.add_step([MicroOp.DROP_EGRESS])
        self.barrels["DIV"] = div

        # SQRT Barrel
        sqrt = Barrel("SQRT")
        sqrt.add_step([MicroOp.INIT_SQRT_GUESS])
        sqrt.add_step([MicroOp.DIVIDE_S_BY_X])
        sqrt.add_step([MicroOp.ADD_X_AND_QUOTIENT])
        sqrt.add_step([MicroOp.HALVE_ACCUMULATOR])
        sqrt.add_step([MicroOp.CHECK_SQRT_CONVERGENCE])
        sqrt.add_step([MicroOp.STORE_SQRT_RESULT])
        self.barrels["SQRT"] = sqrt

        # LOAD Barrel
        load = Barrel("LOAD")
        load.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        load.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        load.add_step([MicroOp.LIFT_INGRESS])
        load.add_step([MicroOp.ADVANCE_MILL])
        load.add_step([MicroOp.DROP_INGRESS, MicroOp.LIFT_EGRESS])
        self.barrels["LOAD"] = load

        # STOR Barrel
        stor = Barrel("STOR")
        stor.add_step([MicroOp.LIFT_EGRESS])
        stor.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        stor.add_step([MicroOp.CONNECT_MILL_TO_STORE])
        stor.add_step([MicroOp.DROP_STORE_AXIS, MicroOp.DROP_EGRESS])
        self.barrels["STOR"] = stor

        # SHL/SHR Barrels
        shl = Barrel("SHL")
        shl.add_step([MicroOp.SHIFT_MILL_LEFT])
        self.barrels["SHL"] = shl

        shr = Barrel("SHR")
        shr.add_step([MicroOp.SHIFT_MILL_RIGHT])
        self.barrels["SHR"] = shr

        # Bitwise Barrels
        and_b = Barrel("AND")
        and_b.add_step([MicroOp.BITWISE_AND])
        self.barrels["AND"] = and_b

        or_b = Barrel("OR")
        or_b.add_step([MicroOp.BITWISE_OR])
        self.barrels["OR"] = or_b

        xor_b = Barrel("XOR")
        xor_b.add_step([MicroOp.BITWISE_XOR])
        self.barrels["XOR"] = xor_b

        # Checksum Barrel
        chks = Barrel("CHKS")
        chks.add_step([MicroOp.COMPUTE_CHECKSUM])
        self.barrels["CHKS"] = chks

        # Lovelace Extension Barrels
        play = Barrel("PLAY")
        play.add_step([MicroOp.PLAY_NOTE])
        self.barrels["PLAY"] = play

        setmode = Barrel("SETMODE")
        setmode.add_step([MicroOp.SET_MODE_SYMBOLIC]) # Simplified: assume SETMODE handles toggle or param
        self.barrels["SETMODE"] = setmode

    def select_barrel(self, op_name: str) -> None:
        if op_name in self.barrels:
            self.active_barrel = op_name
            self.step_index = 0
        else:
            raise ValueError(f"Unknown barrel: {op_name}")

    def step(self) -> list[MicroOp]:
        """Execute next micro-step."""
        if not self.active_barrel:
            return []

        barrel = self.barrels[self.active_barrel]
        if self.step_index >= len(barrel.rows):
            print(f"Barrel {self.active_barrel} finished.")
            self.active_barrel = None
            return []

        ops = list(barrel.rows[self.step_index].studs)
        print(f"Step {self.step_index}: {ops}")
        self.step_index += 1
        return ops
