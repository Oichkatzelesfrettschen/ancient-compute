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

from enum import Enum, auto
from typing import List, Dict, Set, Optional
from dataclasses import dataclass

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

    # Control Ops
    JUMP_RELATIVE = auto()      # Relative jump in micro-program
    REPEAT_IF_COUNTER = auto()  # Jump back if mill_counter > 0
    FETCH_OP_CARD = auto()
    FETCH_VAR_CARD = auto()

@dataclass
class BarrelRow:
    """A vertical column of studs on a barrel (activates simultaneously)."""
    studs: Set[MicroOp]

class Barrel:
    """A control barrel (drum) defining a micro-program."""
    def __init__(self, name: str):
        self.name = name
        self.rows: List[BarrelRow] = []
        
    def add_step(self, ops: List[MicroOp]):
        self.rows.append(BarrelRow(set(ops)))

class BarrelController:
    """Orchestrates the barrels."""
    def __init__(self):
        self.barrels: Dict[str, Barrel] = {}
        self.active_barrel: Optional[str] = None
        self.step_index = 0
        
        self._init_standard_barrels()
        
    def _init_standard_barrels(self):
        # Addition Barrel (Simplified sequence based on general principles)
        add = Barrel("ADD")
        add.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS]) # Get first operand from Store
        add.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS]) # Transfer to Ingress
        add.add_step([MicroOp.LIFT_INGRESS]) # Ingress to Mill input
        add.add_step([MicroOp.ADVANCE_MILL]) # Perform addition (Ingress + Mill Primary)
        add.add_step([MicroOp.RUN_CARRY]) # Propagate carries
        add.add_step([MicroOp.DROP_INGRESS, MicroOp.LIFT_EGRESS]) # Egress gets result
        self.barrels["ADD"] = add

        # Subtraction Barrel
        sub = Barrel("SUB")
        sub.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        sub.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        sub.add_step([MicroOp.LIFT_INGRESS])
        sub.add_step([MicroOp.REVERSE_MILL]) # Perform subtraction
        sub.add_step([MicroOp.RUN_CARRY])
        sub.add_step([MicroOp.DROP_INGRESS, MicroOp.LIFT_EGRESS])
        self.barrels["SUB"] = sub

        # Multiplication Barrel (Simulated "Method of Repeated Addition")
        # Handles single-digit multiplier via repeated addition.
        # Multi-digit multiplication requires an outer loop (not modeled here).
        mul = Barrel("MULT")
        # 0: Fetch Multiplier (operand_src) and extract units digit into counter
        mul.add_step([MicroOp.GET_MULTIPLIER_DIGIT])
        # 1: Add Multiplicand to product accumulator + decrement counter (loop body)
        mul.add_step([MicroOp.ADVANCE_MILL, MicroOp.DECREMENT_COUNTER])
        # 2: Repeat step 1 if counter > 0
        mul.add_step([MicroOp.REPEAT_IF_COUNTER])
        # 3: Store result from accumulator back to register
        mul.add_step([MicroOp.STORE_MILL_ACCUMULATOR])
        self.barrels["MULT"] = mul

        # Division Barrel (restoring division algorithm)
        # Babbage's division proceeds digit-by-digit from most significant
        # to least significant, analogous to long division.
        #
        # Algorithm:
        #   1. Load dividend into mill accumulator, divisor into buffer
        #   2. For each quotient digit (50 digits, MSB first):
        #      a. Compare accumulator against shifted divisor
        #      b. If accumulator >= divisor: subtract, increment quotient digit
        #      c. Repeat until accumulator < divisor
        #      d. Shift divisor right for next digit position
        #   3. Store quotient and remainder
        div = Barrel("DIV")
        # Step 0: Fetch dividend from store to mill accumulator
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        # Step 1: Transfer dividend to mill
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS,
                       MicroOp.LOAD_MILL_ACCUMULATOR])
        # Step 2: Fetch divisor from store
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS])
        # Step 3: Transfer divisor to mill buffer
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        # Step 4: Compare accumulator vs divisor
        div.add_step([MicroOp.COMPARE_MILL_BUFFERS])
        # Step 5: Conditional subtract (if accumulator >= divisor)
        div.add_step([MicroOp.REVERSE_MILL])
        # Step 6: Increment quotient digit counter
        div.add_step([MicroOp.INCREMENT_QUOTIENT_DIGIT])
        # Step 7: Repeat subtraction while accumulator >= divisor
        div.add_step([MicroOp.REPEAT_IF_COUNTER])
        # Step 8: Shift divisor right for next quotient digit
        div.add_step([MicroOp.SHIFT_MILL_DIVISOR])
        # Step 9: Reset remainder tracking
        div.add_step([MicroOp.RESET_REMAINDER])
        # Step 10: Store quotient via egress
        div.add_step([MicroOp.LIFT_EGRESS])
        # Step 11: Transfer result to store
        div.add_step([MicroOp.CONNECT_MILL_TO_STORE, MicroOp.DROP_EGRESS])
        # Step 12: Store remainder via egress
        div.add_step([MicroOp.STORE_MILL_ACCUMULATOR])
        self.barrels["DIV"] = div

        # SQRT Barrel (Newton-Raphson iteration)
        # Algorithm: x_{n+1} = (x_n + S/x_n) / 2
        # Converges quadratically; ~25 iterations for 50-digit precision.
        sqrt = Barrel("SQRT")
        # Step 0: Initialize guess x_0 = S/2
        sqrt.add_step([MicroOp.INIT_SQRT_GUESS])
        # Step 1: Compute S / x_n
        sqrt.add_step([MicroOp.DIVIDE_S_BY_X])
        # Step 2: Compute x_n + (S/x_n)
        sqrt.add_step([MicroOp.ADD_X_AND_QUOTIENT])
        # Step 3: Halve to get x_{n+1} = (x_n + S/x_n) / 2
        sqrt.add_step([MicroOp.HALVE_ACCUMULATOR])
        # Step 4: Check convergence (loops back to step 1 if not converged)
        sqrt.add_step([MicroOp.CHECK_SQRT_CONVERGENCE])
        # Step 5: Store final result
        sqrt.add_step([MicroOp.STORE_SQRT_RESULT])
        self.barrels["SQRT"] = sqrt

    def select_barrel(self, op_name: str):
        if op_name in self.barrels:
            self.active_barrel = op_name
            self.step_index = 0
        else:
            raise ValueError(f"Unknown barrel: {op_name}")

    def step(self) -> List[MicroOp]:
        """Execute next micro-step."""
        if not self.active_barrel:
            return []
            
        barrel = self.barrels[self.active_barrel]
        if self.step_index >= len(barrel.rows):
            print(f"Barrel {self.active_barrel} finished.")
            self.active_barrel = None # Sequence complete
            return []
            
        ops = list(barrel.rows[self.step_index].studs)
        print(f"Step {self.step_index}: {ops}")
        self.step_index += 1
        return ops
