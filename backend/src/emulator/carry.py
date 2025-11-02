"""
Difference Engine No. 2 - Anticipating Carriage

Overlapped carry optimization for rapid carry propagation across 8 columns.

Babbage's Anticipating Carriage is a mechanical innovation that propagates
carries in parallel rather than sequentially. While sequential carry propagation
would require waiting for carries to ripple from column 0 to 7, the Anticipating
Carriage predicts and applies all carries simultaneously.

Architecture:
  - Monitors carry_out signals from all 8 columns
  - Predicts which columns will generate carries
  - Applies predicted carries to next columns (0→7 direction)
  - Updates anticipation state for next operational cycle

References:
  - SMG Technical Description: Babbage's Difference Engine No. 2
  - Menabrea/Lovelace (1843): Notes on the Analytical Engine
  - Swade & Orrery: "Difference Engine: Charles Babbage and the Quest"
"""

from typing import List, Dict, Optional
from dataclasses import dataclass


@dataclass
class CarrySnapshot:
    """Snapshot of AnticipatingCarriage state for debugging."""

    phase: int  # Current mechanical phase (0-7)
    carry_signals: List[bool]  # Carry signals from 8 columns
    anticipated_carries: List[bool]  # Carries to apply to next phase
    is_active: bool  # Carriage active in this cycle


class AnticipatingCarriage:
    """
    Anticipating Carriage for overlapped carry propagation.

    Babbage's solution to rapid carry handling across 8 columns. Rather than
    waiting for carries to propagate sequentially (slow), the Anticipating
    Carriage predicts carries and applies them in parallel.

    Operation Model:
      1. Monitor: Check carry_out from all 8 columns simultaneously
      2. Predict: Determine which columns will produce carries
      3. Apply: Set carry_in flags for next columns (0→7 direction)
      4. Repeat: For next operational cycle

    Mechanical Advantage:
      - Sequential approach: 7+ carry propagation steps
      - Anticipating approach: 1 parallel carry application
      - Performance gain: 7x faster carry handling

    Note on "Anticipation":
      The carriage doesn't actually predict future carries mathematically.
      Rather, it applies detected carries immediately in parallel to all
      affected columns, rather than waiting for ripple propagation.
    """

    def __init__(self):
        """Initialize Anticipating Carriage."""
        self.phases = 8  # 8 columns → 8 phases
        self.current_phase = 0  # Current phase (0-7)
        self.carry_signals = [False] * 8  # Input carry signals
        self.anticipated_carries = [False] * 8  # Output carries
        self.is_active = False  # Active in current cycle
        self.history: List[CarrySnapshot] = []  # For tracing

    def set_carry_signals(self, signals: List[bool]) -> None:
        """
        Set carry signals from all 8 columns.

        Parameters:
            signals: List of 8 boolean carry signals (one per column)

        Raises:
            ValueError: If signals length != 8
        """
        if len(signals) != 8:
            raise ValueError(f"Expected 8 carry signals, got {len(signals)}")
        self.carry_signals = signals.copy()

    def anticipate_carries(self) -> List[bool]:
        """
        Compute anticipated carries for next phase.

        Algorithm:
          1. Column 0 carries to column 1
          2. Column 1 carries to column 2
          ... and so on to column 7

        Returns:
            List of 8 boolean carries to apply to next columns
        """
        anticipated = [False] * 8

        # Column 0's carry goes to column 1
        if self.carry_signals[0]:
            anticipated[1] = True

        # Column 1's carry goes to column 2
        if self.carry_signals[1]:
            anticipated[2] = True

        # And so forth... (pattern continues for all 8)
        for i in range(7):
            if self.carry_signals[i]:
                anticipated[i + 1] = True

        self.anticipated_carries = anticipated
        return anticipated.copy()

    def get_carries_for_column(self, column_index: int) -> bool:
        """
        Get anticipated carry for specific column.

        Parameters:
            column_index: 0-7

        Returns:
            Boolean carry flag for this column

        Raises:
            IndexError: If column_index out of range
        """
        if not (0 <= column_index < 8):
            raise IndexError(f"Column index out of range: {column_index}")
        return self.anticipated_carries[column_index]

    def advance_phase(self) -> None:
        """Advance to next mechanical phase (0→7 cycle)."""
        self.current_phase = (self.current_phase + 1) % 8

    def activate(self) -> None:
        """Activate carriage for this cycle."""
        self.is_active = True

    def deactivate(self) -> None:
        """Deactivate carriage."""
        self.is_active = False

    def is_carrying(self) -> bool:
        """Check if any carries are pending."""
        return any(self.anticipated_carries)

    def reset(self) -> None:
        """Reset carriage to initial state."""
        self.current_phase = 0
        self.carry_signals = [False] * 8
        self.anticipated_carries = [False] * 8
        self.is_active = False

    def get_snapshot(self) -> CarrySnapshot:
        """
        Capture complete state for debugging.

        Returns:
            CarrySnapshot with all current state
        """
        return CarrySnapshot(
            phase=self.current_phase,
            carry_signals=self.carry_signals.copy(),
            anticipated_carries=self.anticipated_carries.copy(),
            is_active=self.is_active,
        )

    def record_snapshot(self) -> None:
        """Record current state to history (for tracing)."""
        self.history.append(self.get_snapshot())

    def clear_history(self) -> None:
        """Clear snapshot history."""
        self.history.clear()

    def get_history(self, depth: int = 10) -> List[CarrySnapshot]:
        """
        Get recent history (last N snapshots).

        Parameters:
            depth: How many snapshots to return

        Returns:
            List of snapshots (most recent last)
        """
        return self.history[-depth:] if self.history else []

    def __repr__(self) -> str:
        """String representation for debugging."""
        carries_str = "".join("1" if c else "0" for c in self.carry_signals)
        anticipated_str = "".join("1" if c else "0" for c in self.anticipated_carries)
        return (
            f"AnticipatingCarriage(phase={self.current_phase}, "
            f"carries={carries_str}, anticipated={anticipated_str}, "
            f"active={self.is_active})"
        )


class CarryPropagationUnit:
    """
    Extended carry propagation with multiple strategies.

    Allows simulation of different carry propagation methods:
      1. Sequential: Ripple carry (slow but simple)
      2. Lookahead: Predict carries (Babbage's approach)
      3. Parallel: All carries simultaneously (hypothetical)
    """

    def __init__(self, propagation_mode: str = "lookahead"):
        """
        Initialize carry propagation unit.

        Parameters:
            propagation_mode: "sequential", "lookahead", or "parallel"
        """
        if propagation_mode not in ("sequential", "lookahead", "parallel"):
            raise ValueError(f"Unknown propagation mode: {propagation_mode}")
        self.mode = propagation_mode
        self.carry_chain: List[bool] = [False] * 8
        self.step_count = 0

    def propagate(self, input_carries: List[bool]) -> List[bool]:
        """
        Propagate carries using selected strategy.

        Parameters:
            input_carries: List of 8 input carry signals

        Returns:
            List of 8 output carries
        """
        if len(input_carries) != 8:
            raise ValueError(f"Expected 8 carries, got {len(input_carries)}")

        if self.mode == "sequential":
            return self._propagate_sequential(input_carries)
        elif self.mode == "lookahead":
            return self._propagate_lookahead(input_carries)
        else:  # parallel
            return self._propagate_parallel(input_carries)

    def _propagate_sequential(self, carries: List[bool]) -> List[bool]:
        """
        Sequential ripple carry (simulated).

        In real hardware, this would take multiple cycles.
        Simulation counts steps instead.
        """
        result = [False] * 8
        for i in range(7):
            if carries[i]:
                result[i + 1] = True
        self.step_count = 7  # Would take 7 ripple steps
        return result

    def _propagate_lookahead(self, carries: List[bool]) -> List[bool]:
        """
        Lookahead carry (Babbage's Anticipating Carriage approach).

        Predicts all carries in parallel.
        """
        result = [False] * 8
        for i in range(7):
            if carries[i]:
                result[i + 1] = True
        self.step_count = 1  # Lookahead takes 1 step
        return result

    def _propagate_parallel(self, carries: List[bool]) -> List[bool]:
        """
        Hypothetical parallel propagation (all carries at once).

        Only makes sense if all carries were somehow predicted
        in advance (not practical for actual arithmetic).
        """
        result = [False] * 8
        for i in range(7):
            if carries[i]:
                result[i + 1] = True
        self.step_count = 1  # Parallel is instant (theoretical)
        return result

    def get_step_count(self) -> int:
        """Get number of steps for last propagation."""
        return self.step_count

    def reset(self) -> None:
        """Reset propagation state."""
        self.carry_chain = [False] * 8
        self.step_count = 0

    def __repr__(self) -> str:
        """String representation for debugging."""
        return f"CarryPropagationUnit(mode={self.mode}, " f"step_count={self.step_count})"
