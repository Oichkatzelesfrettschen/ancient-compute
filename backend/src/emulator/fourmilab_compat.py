"""Fourmilab AE card deck compatibility layer.

Fourmilab (fourmilab.ch/babbage/) is the de facto standard for Analytical
Engine card decks online. This module provides:

  parse_fourmilab_deck(source)  -- parse Fourmilab text into FourmilabCard list
  fourmilab_to_instructions(cards) -- translate to internal Instructions
  run_fourmilab_deck(source)    -- parse + translate + run, return engine state

Fourmilab card syntax (all other lines are comments):
  N000 +1234567890    # Number card: load value into column 0
  L000                # Load column 0 into result register (non-destructive)
  L000'               # Load column 0, then clear it (destructive)
  S002                # Store result register to column 2
  S002'               # Additive store (result += column 2, write back)
  +                   # Add (result = result + prior_result)
  -                   # Subtract
  *                   # Multiply
  /                   # Divide
  CF+005              # Conditional forward: skip 5 cards if result != 0
  CB-003              # Conditional backward: skip back 3 cards if result != 0
  B+010               # Unconditional forward skip 10 cards
  P                   # Print result register to output
  H                   # Halt

The register model:
  Fourmilab's engine has two implicit registers: "prior" and "result".
  We map these onto the AE engine's register A (result) and B (prior).

Translation:
  - N cards -> LOAD immediate + STOR
  - L cards -> LOAD [addr] into A  (destructive: also STOR 0 back)
  - S cards -> STOR A [addr]  (additive: LOAD B [addr], ADD B A, STOR B [addr])
  - +/-/*/  -> ADD/SUB/MULT/DIV A B
  - CF/CB   -> JNZ to computed absolute PC target
  - B       -> JMP to computed absolute PC target
  - P       -> WRPRN A
  - H       -> HALT

References:
  Walker, J. (1991). The Analytical Engine -- Online Emulator.
  fourmilab.ch/babbage/ -- accessed 2026.
"""

from __future__ import annotations

from backend.src.emulator.analytical_engine import Engine
from backend.src.emulator.cli.assembler.fourmilab import (
    FourmilabCard,
    parse_fourmilab_source,
    translate_fourmilab,
)


def parse_fourmilab_deck(source: str) -> list[FourmilabCard]:
    """Parse Fourmilab source text into a list of FourmilabCards.

    Args:
        source: Multi-line string containing Fourmilab card deck source.

    Returns:
        List of FourmilabCard dataclass instances.
    """
    return parse_fourmilab_source(source)


def fourmilab_to_instructions(cards: list[FourmilabCard]) -> list:
    """Translate a list of FourmilabCards into engine Instruction objects.

    Args:
        cards: Output from parse_fourmilab_deck().

    Returns:
        List of Instruction objects suitable for engine.instruction_cards.
    """
    return translate_fourmilab(cards)


def run_fourmilab_deck(source: str, output_callback=None) -> Engine:
    """Parse, translate, and run a Fourmilab card deck.

    Args:
        source: Fourmilab card deck source text.
        output_callback: Optional callable(str) for capturing engine output.

    Returns:
        Engine object after program completion.
        Inspect engine.result_cards for output values.
    """
    cards = parse_fourmilab_deck(source)
    instructions = fourmilab_to_instructions(cards)

    engine = Engine(output_callback=output_callback)
    engine.instruction_cards = instructions
    engine.run()

    return engine
