#!/usr/bin/env python3
"""Card compiler: source text -> encoded card bytes -> decoded ops.

Implements round-trip encoding/decoding per CARD_STANDARD.md v0.1.
Each source line maps to one card. The compiler verifies that
decode(encode(source)) == source for all valid inputs.

Usage:
    python3 tools/card_compiler.py < input.deck
    python3 tools/card_compiler.py --round-trip input.deck

Card source format (one instruction per line):
    ADD V0 V1          # Operation card
    LOAD V5            # Variable card (read)
    STORE V5           # Variable card (write)
    NUM +12345         # Number card
    HALT               # Operation card (no operands)
"""

from __future__ import annotations

import re
import sys
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import List, Optional


# --- Opcodes (must match docs/hardware/OPCODES.yaml) ---

class Opcode(Enum):
    ADD = 0x0
    SUB = 0x1
    MUL = 0x2
    DIV = 0x3
    MOV = 0x4
    CLR = 0x5
    CMPZ = 0x6
    BR = 0x7
    STEP = 0x8
    HALT = 0x9
    PRINT = 0xA
    NOP = 0xB


OPCODE_BY_NAME = {op.name: op for op in Opcode}


class CardClass(Enum):
    OPERATION = "O"
    VARIABLE = "V"
    NUMBER = "N"


# --- Card data structures ---

@dataclass(frozen=True)
class CardData:
    """Decoded card content."""
    card_class: CardClass
    # Operation cards
    opcode: Optional[Opcode] = None
    modifier: int = 0
    # Variable cards
    direction: Optional[str] = None  # "read" or "write"
    column: int = 0
    # Number cards
    sign: int = 0  # 0=positive, 1=negative
    magnitude: str = ""  # up to 50 decimal digits


# --- Encoded card (byte-level representation) ---

CARD_BYTES = 32  # Fixed card payload size (256 bits)


@dataclass(frozen=True)
class EncodedCard:
    """Raw card as bytes."""
    class_byte: int  # 0=operation, 1=variable, 2=number
    payload: bytes    # CARD_BYTES bytes


# --- Encoder ---

def encode_card(card: CardData) -> EncodedCard:
    """Encode a CardData into an EncodedCard."""
    buf = bytearray(CARD_BYTES)

    if card.card_class == CardClass.OPERATION:
        if card.opcode is None:
            raise ValueError("Operation card requires opcode")
        buf[0] = (card.opcode.value & 0x0F) | ((card.modifier & 0x0F) << 4)
        return EncodedCard(class_byte=0, payload=bytes(buf))

    elif card.card_class == CardClass.VARIABLE:
        direction_bit = 0 if card.direction == "read" else 1
        # Column address in 10 bits: buf[0] low 7 bits + buf[1] low 3 bits
        buf[0] = (direction_bit & 0x01) | ((card.column & 0x7F) << 1)
        buf[1] = (card.column >> 7) & 0x07
        return EncodedCard(class_byte=1, payload=bytes(buf))

    elif card.card_class == CardClass.NUMBER:
        buf[0] = card.sign & 0x01
        # BCD encode magnitude (up to 50 digits, 4 bits each = 25 bytes)
        mag = card.magnitude.zfill(50)[-50:]
        for i, ch in enumerate(mag):
            digit = int(ch)
            byte_idx = 1 + (i // 2)
            if byte_idx < CARD_BYTES:
                if i % 2 == 0:
                    buf[byte_idx] |= digit & 0x0F
                else:
                    buf[byte_idx] |= (digit & 0x0F) << 4
        return EncodedCard(class_byte=2, payload=bytes(buf))

    raise ValueError(f"Unknown card class: {card.card_class}")


# --- Decoder ---

def decode_card(enc: EncodedCard) -> CardData:
    """Decode an EncodedCard back to CardData."""
    buf = enc.payload

    if enc.class_byte == 0:
        opcode_val = buf[0] & 0x0F
        modifier = (buf[0] >> 4) & 0x0F
        opcode = Opcode(opcode_val)
        return CardData(
            card_class=CardClass.OPERATION,
            opcode=opcode,
            modifier=modifier,
        )

    elif enc.class_byte == 1:
        direction_bit = buf[0] & 0x01
        column = ((buf[0] >> 1) & 0x7F) | ((buf[1] & 0x07) << 7)
        return CardData(
            card_class=CardClass.VARIABLE,
            direction="read" if direction_bit == 0 else "write",
            column=column,
        )

    elif enc.class_byte == 2:
        sign = buf[0] & 0x01
        digits = []
        for i in range(50):
            byte_idx = 1 + (i // 2)
            if byte_idx < CARD_BYTES:
                if i % 2 == 0:
                    d = buf[byte_idx] & 0x0F
                else:
                    d = (buf[byte_idx] >> 4) & 0x0F
            else:
                d = 0
            digits.append(str(d))
        magnitude = "".join(digits).lstrip("0") or "0"
        return CardData(
            card_class=CardClass.NUMBER,
            sign=sign,
            magnitude=magnitude,
        )

    raise ValueError(f"Unknown class byte: {enc.class_byte}")


# --- Source text parser ---

_VAR_RE = re.compile(r"^V(\d+)$")


def parse_line(line: str) -> CardData:
    """Parse one source line into a CardData.

    Formats:
        ADD [V<n> V<n>]     -> operation card
        LOAD V<n>           -> variable card (read)
        STORE V<n>          -> variable card (write)
        NUM [+|-]<digits>   -> number card
    """
    line = line.split("#")[0].strip()
    if not line:
        return CardData(card_class=CardClass.OPERATION, opcode=Opcode.NOP)

    parts = line.split()
    mnemonic = parts[0].upper()

    # Variable cards
    if mnemonic == "LOAD":
        m = _VAR_RE.match(parts[1].upper())
        if not m:
            raise ValueError(f"Invalid variable: {parts[1]}")
        return CardData(
            card_class=CardClass.VARIABLE,
            direction="read",
            column=int(m.group(1)),
        )

    if mnemonic == "STORE":
        m = _VAR_RE.match(parts[1].upper())
        if not m:
            raise ValueError(f"Invalid variable: {parts[1]}")
        return CardData(
            card_class=CardClass.VARIABLE,
            direction="write",
            column=int(m.group(1)),
        )

    # Number cards
    if mnemonic == "NUM":
        val_str = parts[1]
        sign = 1 if val_str.startswith("-") else 0
        magnitude = val_str.lstrip("+-").lstrip("0") or "0"
        return CardData(
            card_class=CardClass.NUMBER,
            sign=sign,
            magnitude=magnitude,
        )

    # Operation cards
    if mnemonic not in OPCODE_BY_NAME:
        raise ValueError(f"Unknown mnemonic: {mnemonic}")

    opcode = OPCODE_BY_NAME[mnemonic]
    modifier = 0
    # If operands are present, encode first operand column in modifier
    if len(parts) > 1 and _VAR_RE.match(parts[1].upper()):
        modifier = int(_VAR_RE.match(parts[1].upper()).group(1)) & 0x0F

    return CardData(
        card_class=CardClass.OPERATION,
        opcode=opcode,
        modifier=modifier,
    )


def card_to_source(card: CardData) -> str:
    """Convert a CardData back to canonical source text."""
    if card.card_class == CardClass.OPERATION:
        name = card.opcode.name if card.opcode else "NOP"
        if card.modifier > 0:
            return f"{name} V{card.modifier}"
        return name

    elif card.card_class == CardClass.VARIABLE:
        prefix = "LOAD" if card.direction == "read" else "STORE"
        return f"{prefix} V{card.column}"

    elif card.card_class == CardClass.NUMBER:
        sign_str = "-" if card.sign else "+"
        return f"NUM {sign_str}{card.magnitude}"

    raise ValueError(f"Unknown card class: {card.card_class}")


# --- Deck-level operations ---

def compile_deck(source: str) -> List[EncodedCard]:
    """Compile a multi-line source into a list of encoded cards."""
    cards = []
    for line in source.splitlines():
        stripped = line.split("#")[0].strip()
        if not stripped:
            continue
        card_data = parse_line(stripped)
        cards.append(encode_card(card_data))
    return cards


def decompile_deck(encoded: List[EncodedCard]) -> str:
    """Decompile a list of encoded cards back to source text."""
    lines = []
    for enc in encoded:
        card_data = decode_card(enc)
        lines.append(card_to_source(card_data))
    return "\n".join(lines)


def round_trip(source: str) -> bool:
    """Verify source -> encode -> decode -> source round-trip.

    Returns True if recovered source matches original (after normalization).
    """
    # Normalize: strip comments, blank lines, whitespace
    def normalize(text: str) -> List[str]:
        lines = []
        for line in text.splitlines():
            stripped = line.split("#")[0].strip()
            if stripped:
                lines.append(stripped.upper())
        return lines

    original = normalize(source)
    encoded = compile_deck(source)
    recovered = decompile_deck(encoded)
    recovered_lines = normalize(recovered)

    return original == recovered_lines


# --- CLI ---

def main() -> None:
    if len(sys.argv) > 1 and sys.argv[1] == "--round-trip":
        path = Path(sys.argv[2]) if len(sys.argv) > 2 else None
        source = path.read_text() if path else sys.stdin.read()
        if round_trip(source):
            print("PASS: Round-trip verified")
        else:
            print("FAIL: Round-trip mismatch")
            sys.exit(1)
    else:
        source = sys.stdin.read()
        encoded = compile_deck(source)
        for i, enc in enumerate(encoded):
            card = decode_card(enc)
            print(f"Card {i:3d}: [{card.card_class.value}] {card_to_source(card)}")


if __name__ == "__main__":
    main()
