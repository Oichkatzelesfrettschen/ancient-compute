"""
Enigma Machine Emulator

Simulates the Wehrmacht Enigma I / M3 / M4 cipher machine.

Architecture:
  - Plugboard (Steckerbrett): Pairwise swapping of input/output letters
  - Rotors (Walzen): 3 or 4 interchangeable rotors with internal wiring
  - Reflector (Umkehrwalze): Reflects signal back through rotors
  - Keyboard/Lampboard: Input and output interface

Operation:
  - Key press closes circuit
  - Current flows through Plugboard -> Rotors (R -> M -> L) -> Reflector -> Rotors (L -> M -> R) -> Plugboard -> Lamp
  - Rightmost rotor steps before each character encipherment
  - Middle/Left rotors step based on notch positions (turnover)

Historical Context:
  - Critical to WW2 cryptography and Alan Turing's Bombe (Bletchley Park).
  - Represents the transition from mechanical calculation to electro-mechanical logic.
"""

from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass

# Rotor wiring configurations (Input A-Z maps to output string)
# Source: http://users.telenet.be/d.rijmenants/en/enigmatech.htm
ROTOR_WIRING = {
    "I":    "EKMFLGDQVZNTOWYHXUSPAIBRCJ",
    "II":   "AJDKSIRUXBLHWTMCQGZNPYFVOE",
    "III":  "BDFHJLCPRTXVZNYEIWGAKMUSQO",
    "IV":   "ESOVPZJAYQUIRHXLNFTGKDCMWB",
    "V":    "VZBRGITYUPSDNHLXAWMJQOFECK",
    "VI":   "JPGVOUMFYQBENHZRDKASXLICTW",
    "VII":  "NZJHGRCXMYSWBOUFAIVLPEKQDT",
    "VIII": "FKQHTLXOCBJSPDZRAMEWNIUYGV",
    "Beta": "LEYJVCNIXWPBQMDRTAKZGFUHOS", # M4 thin rotor
    "Gamma":"FSOKANUERHMBTIYCWLQPZXVGJD"  # M4 thin rotor
}

# Turnover notch positions (letter visible in window when turnover happens)
# E.g., 'Q' for Rotor I means when Q -> R, the next rotor steps.
ROTOR_NOTCHES = {
    "I":    "Q",
    "II":   "E",
    "III":  "V",
    "IV":   "J",
    "V":    "Z",
    "VI":   "ZM", # Two notches
    "VII":  "ZM",
    "VIII": "ZM",
}

# Reflector wiring
REFLECTOR_WIRING = {
    "A":      "EJMZALYXVBWFCRQUONTSPIKHGD",
    "B":      "YRUHQSLDPXNGOKMIEBFZCWVJAT",
    "C":      "FVPJIAOYEDRZXWGCTKUQSBNMHL",
    "B-Thin": "ENKQAUYWJICOPBLMDXZVFTHRGS", # M4 thin reflector
    "C-Thin": "RDOBJNTKVEHMLFCWZAXGYIPSUQ"  # M4 thin reflector
}

def to_int(char: str) -> int:
    return ord(char.upper()) - 65

def to_char(val: int) -> str:
    return chr((val % 26) + 65)

class Rotor:
    def __init__(self, name: str, wiring: str, notches: str, ring_setting: int = 0, position: int = 0):
        self.name = name
        # Forward wiring: Input(0..25) -> Output(0..25)
        self.forward_map = [to_int(c) for c in wiring]
        # Reverse wiring: Input(0..25) -> Output(0..25) (inverse of forward)
        self.reverse_map = [0] * 26
        for i, out in enumerate(self.forward_map):
            self.reverse_map[out] = i
            
        self.notches = [to_int(c) for c in notches] if notches else []
        self.ring_setting = ring_setting # Ringstellung (0-25)
        self.position = position # Grundstellung (0-25, current rotation)

    def step(self):
        self.position = (self.position + 1) % 26

    def is_at_notch(self) -> bool:
        return self.position in self.notches

    def encipher_forward(self, k: int) -> int:
        """Pass signal right-to-left."""
        # Shift input by offset (position - ring)
        offset = (self.position - self.ring_setting) % 26
        enter_index = (k + offset) % 26
        exit_index = self.forward_map[enter_index]
        exit_val = (exit_index - offset) % 26
        return exit_val

    def encipher_reverse(self, k: int) -> int:
        """Pass signal left-to-right (after reflection)."""
        offset = (self.position - self.ring_setting) % 26
        enter_index = (k + offset) % 26
        exit_index = self.reverse_map[enter_index]
        exit_val = (exit_index - offset) % 26
        return exit_val

class Reflector:
    def __init__(self, name: str, wiring: str):
        self.name = name
        self.map = [to_int(c) for c in wiring]

    def reflect(self, k: int) -> int:
        return self.map[k]

class Plugboard:
    def __init__(self, connections: List[str]):
        """
        connections: List of two-char strings, e.g., ["AB", "CD"]
        """
        self.map = list(range(26))
        for pair in connections:
            if len(pair) != 2:
                raise ValueError(f"Invalid plugboard pair: {pair}")
            a, b = to_int(pair[0]), to_int(pair[1])
            self.map[a] = b
            self.map[b] = a

    def swap(self, k: int) -> int:
        return self.map[k]

class EnigmaMachine:
    """
    Enigma Machine Simulator.
    Default configuration: Enigma I (Wehrmacht) with Rotors I, II, III and Reflector B.
    """
    def __init__(self, rotors: List[str] = ["I", "II", "III"], reflector: str = "B", 
                 ring_settings: List[int] = [0, 0, 0], plugboard_connections: List[str] = []):
        
        self.rotors = []
        # Rotors are typically listed Left-to-Right in config strings, but physically
        # the signal goes Right -> Middle -> Left -> Reflector
        # We will store them [Left, Middle, Right] to match convention
        # self.rotors[0] is Left (slow), self.rotors[-1] is Right (fast)
        
        for i, name in enumerate(rotors):
            wiring = ROTOR_WIRING.get(name)
            notches = ROTOR_NOTCHES.get(name, "")
            if not wiring:
                raise ValueError(f"Unknown rotor: {name}")
            
            ring = ring_settings[i] if i < len(ring_settings) else 0
            self.rotors.append(Rotor(name, wiring, notches, ring))
            
        self.reflector = Reflector(reflector, REFLECTOR_WIRING[reflector])
        self.plugboard = Plugboard(plugboard_connections)

    def set_rotor_positions(self, positions: str):
        """Set rotor start positions (Grundstellung), e.g., "ABC"."""
        if len(positions) != len(self.rotors):
            raise ValueError(f"Expected {len(self.rotors)} positions, got {len(positions)}")
        for i, char in enumerate(positions):
            self.rotors[i].position = to_int(char)

    def step_rotors(self):
        """
        Double stepping mechanism of Enigma.
        - Right rotor always steps.
        - Middle rotor steps if Right is at notch.
        - Middle rotor ALSO steps if it is at its own notch (turnover to Left).
        - Left rotor steps if Middle is at notch.
        """
        l_rotor = self.rotors[0]
        m_rotor = self.rotors[1]
        r_rotor = self.rotors[2]
        
        # Determine who steps BEFORE moving anything
        rotate_l = m_rotor.is_at_notch()
        rotate_m = r_rotor.is_at_notch() or m_rotor.is_at_notch()
        rotate_r = True
        
        if rotate_l: l_rotor.step()
        if rotate_m: m_rotor.step()
        if rotate_r: r_rotor.step()

    def encipher_char(self, char: str) -> str:
        if not char.isalpha():
            return char # Ignore non-letters
            
        k = to_int(char)
        
        # 1. Step rotors
        self.step_rotors()
        
        # 2. Plugboard
        k = self.plugboard.swap(k)
        
        # 3. Rotors Right -> Left
        for rotor in reversed(self.rotors):
            k = rotor.encipher_forward(k)
            
        # 4. Reflector
        k = self.reflector.reflect(k)
        
        # 5. Rotors Left -> Right
        for rotor in self.rotors:
            k = rotor.encipher_reverse(k)
            
        # 6. Plugboard
        k = self.plugboard.swap(k)
        
        return to_char(k)

    def process_text(self, text: str) -> str:
        """Encipher/Decipher a full string."""
        return "".join(self.encipher_char(c) for c in text)
