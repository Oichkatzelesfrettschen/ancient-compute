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

    # Card Reader Control (Simplified for this model)
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

        # Multiplication Barrel (Highly simplified - actual multiplication is complex repeated addition/shift)
        mul = Barrel("MULT")
        # ... (multiplication barrel definition)
        self.barrels["MULT"] = mul

        # Division Barrel (Placeholder for now)
        div = Barrel("DIV")
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS]) # Get Dividend
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        div.add_step([MicroOp.FETCH_VAR_CARD, MicroOp.LIFT_STORE_AXIS]) # Get Divisor
        div.add_step([MicroOp.CONNECT_STORE_TO_MILL, MicroOp.DROP_STORE_AXIS])
        div.add_step([MicroOp.COMPARE_MILL_BUFFERS]) # Compare
        div.add_step([MicroOp.REVERSE_MILL]) # Subtract (if possible)
        div.add_step([MicroOp.INCREMENT_QUOTIENT_DIGIT]) # Increment quotient digit
        div.add_step([MicroOp.SHIFT_MILL_DIVISOR]) # Shift divisor for next digit
        div.add_step([MicroOp.LIFT_EGRESS]) # Store quotient
        div.add_step([MicroOp.CONNECT_MILL_TO_STORE, MicroOp.DROP_EGRESS])
        self.barrels["DIV"] = div

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
            self.active_barrel = None # Sequence complete
            return []
            
        ops = list(barrel.rows[self.step_index].studs)
        self.step_index += 1
        return ops
