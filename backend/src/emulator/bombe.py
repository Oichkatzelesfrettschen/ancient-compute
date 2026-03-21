"""Turing's Bombe Emulator (Bletchley Park, 1939-1940).

Alan Turing (1912-1954) designed the Bombe in late 1939, drawing on earlier
work by Marian Rejewski and Jerzy Rozycki at the Polish Cipher Bureau (the
Bomba, 1938). Gordon Welchman added the critical "diagonal board" improvement
in 1940, which dramatically reduced false stops. The first operational Bombe
("Victory") began work at Bletchley Park in March 1940.

By war's end, 211 Bombes were operating, cracking up to 3,000-4,000 Enigma
messages per day. The Bombe is widely credited as the decisive factor enabling
the Allies to break German naval and Luftwaffe Enigma traffic throughout WW2.

Architecture:
  - Drums: Each Bombe contained multiple sets of 3 rotors (drums), each set
    simulating an Enigma scrambler (rotors + reflector) WITHOUT the plugboard.
    The Bombe had 36 scrambler units (later models had more).
  - Menu: A graph of letter connections derived from a crib (known plaintext).
    Each edge in the menu corresponds to one Enigma encryption: if crib[i]
    enciphers to cipher[i], then the scrambler at rotor-offset i connects
    crib[i] and cipher[i] on the menu.
  - Test register: An electrical circuit that detects contradictions. When the
    electrical current injected at a menu node returns to its origin with the
    same value, no contradiction has occurred -- a "stop" is registered.
  - Diagonal board (Welchman, 1940): An additional relay board exploiting the
    symmetry of the Enigma plugboard (if A-B is a Stecker pair, then B-A is
    too). This doubled the connectivity of the menu and reduced false stops by
    an order of magnitude.

Crib attack:
  The Bombe required a "crib" -- a piece of known or suspected plaintext
  aligned against ciphertext. Cribs were found by traffic analysis, standard
  message formats ("WETTER VORHERSAGE" in weather reports), or operator errors
  (e.g., re-using a key). The crib/cipher pairing generates the menu.

This emulator models:
  - BombeScrambler: One Enigma scrambler unit (rotors + reflector, no plugboard).
  - BombeMenu: The menu graph derived from a crib/cipher pair.
  - Bombe: Main class with run_single() (test one position) and run() (full
    26^3 scan).

The diagonal board is modeled implicitly: the bidirectional edge traversal in
the BFS propagation captures the same constraint that the diagonal board
enforced electrically.

References:
  - Welchman, G. (1982). The Hut Six Story: Breaking the Enigma Codes.
    McGraw-Hill. (Welchman's own account of the diagonal board.)
  - Hinsley, F. H., & Stripp, A. (Eds.). (1993). Codebreakers: The Inside
    Story of Bletchley Park. Oxford University Press.
  - Turing, A. M. (1940). Memorandum on the Bombe. (Declassified 2002,
    National Archives HW 14/2.)
  - Copeland, B. J. (Ed.). (2004). The Essential Turing. Oxford University
    Press. pp. 155-168 (Turing's own description of the Bombe principle).
"""

from __future__ import annotations

import collections
from dataclasses import dataclass
from typing import NamedTuple

from .enigma import REFLECTOR_WIRING, ROTOR_NOTCHES, ROTOR_WIRING, to_int

# ---------------------------------------------------------------------------
# Scrambler lookup computation (no object overhead in the hot path)
# ---------------------------------------------------------------------------


def _make_scrambler_lookup(
    fwd_maps: list[list[int]],
    rev_maps: list[list[int]],
    reflect_map: list[int],
    offsets: list[int],
) -> list[int]:
    """
    Compute the 26-letter substitution table for one Enigma scrambler
    (rotors + reflector, no plugboard) at the given rotor offsets.

    offsets[i] = (rotor[i].position - rotor[i].ring_setting) % 26.

    WHY: Pre-computing the full lookup table reduces each encipher() call from
    O(12 modular accesses) to O(1). Critical for the Bombe's 17,576-position
    scan loop.
    """
    lookup = [0] * 26
    n = len(fwd_maps)
    for k in range(26):
        v = k
        # Signal: right -> ... -> left (reversed rotor order in signal path)
        for i in range(n - 1, -1, -1):
            off = offsets[i]
            v = (fwd_maps[i][(v + off) % 26] - off) % 26
        # Reflector
        v = reflect_map[v]
        # Signal: left -> ... -> right
        for i in range(n):
            off = offsets[i]
            v = (rev_maps[i][(v + off) % 26] - off) % 26
        lookup[k] = v
    return lookup


# ---------------------------------------------------------------------------
# Menu
# ---------------------------------------------------------------------------


@dataclass
class MenuLink:
    """One undirected edge in the Bombe menu.

    Represents the Enigma encryption at crib position ``offset``:
    crib[offset] <-> cipher[offset] via the Enigma scrambler at that position.

    letter_a, letter_b: 0-25 (A=0 ... Z=25).
    offset:             crib index (0 = first character pair).
    """

    letter_a: int
    letter_b: int
    offset: int


@dataclass
class BombeMenu:
    """Bombe menu derived from a crib / ciphertext alignment.

    A menu is a graph where each node is a letter (0-25) and each edge is a
    MenuLink connecting two letters via the Enigma scrambler at a specific
    rotor offset. The Bombe searches for rotor positions that make the graph
    constraints consistent (no electrical contradiction).

    Best menus have a "closure" -- a cycle in the graph -- which limits false
    stops to approximately 26/26^3 = 1/676 of all positions.
    """

    links: list[MenuLink]
    crib: str
    cipher: str

    @classmethod
    def from_crib(cls, crib: str, cipher: str) -> BombeMenu:
        """Build a menu from plaintext crib and corresponding ciphertext.

        Args:
            crib:   Known or suspected plaintext (e.g., "WETTER").
            cipher: Corresponding ciphertext from the Enigma message.

        Returns:
            BombeMenu with one MenuLink per crib position.

        Raises:
            ValueError: if crib and cipher have different lengths or contain
                non-alphabetic characters after stripping spaces.
        """
        crib = crib.upper().replace(" ", "")
        cipher = cipher.upper().replace(" ", "")
        if len(crib) != len(cipher):
            raise ValueError(f"Crib length ({len(crib)}) != cipher length ({len(cipher)})")
        links: list[MenuLink] = []
        for i, (c, p) in enumerate(zip(crib, cipher, strict=True)):
            if not c.isalpha() or not p.isalpha():
                raise ValueError(f"Non-alpha character at position {i}")
            if c == p:
                # Enigma cannot encipher a letter to itself; skip invalid pairs.
                continue
            links.append(MenuLink(to_int(c), to_int(p), i))
        return cls(links=links, crib=crib, cipher=cipher)

    @property
    def letters(self) -> set[int]:
        """Return the set of all letter indices appearing in the menu."""
        result: set[int] = set()
        for link in self.links:
            result.add(link.letter_a)
            result.add(link.letter_b)
        return result

    @property
    def max_offset(self) -> int:
        """Maximum crib offset in the menu (= minimum drum count needed)."""
        return max((link.offset for link in self.links), default=0)

    def has_closure(self) -> bool:
        """Return True if the menu graph contains at least one cycle.

        A closure (cycle) is necessary for the Bombe to operate effectively.
        Without a cycle, contradictions are rare and false stops flood the
        output. Turing's original design required closures; Welchman's diagonal
        board created virtual closures from non-cyclic menus.
        """
        if len(self.links) < 2:
            return False
        # Build adjacency list and run DFS cycle detection
        adj: dict[int, list[int]] = {}
        for link in self.links:
            adj.setdefault(link.letter_a, []).append(link.letter_b)
            adj.setdefault(link.letter_b, []).append(link.letter_a)
        visited: set[int] = set()
        parent: dict[int, int] = {}

        def dfs(node: int, par: int) -> bool:
            visited.add(node)
            for nb in adj.get(node, []):
                if nb not in visited:
                    parent[nb] = node
                    if dfs(nb, node):
                        return True
                elif nb != par:
                    return True
            return False

        for start in adj:
            if start not in visited:
                parent[start] = -1
                if dfs(start, -1):
                    return True
        return False


# ---------------------------------------------------------------------------
# Stop
# ---------------------------------------------------------------------------


class BombeStop(NamedTuple):
    """A Bombe stop: a rotor position consistent with the menu constraints.

    Most stops are "false stops" caused by accidental consistency. True stops
    (genuine Enigma settings) appear in the list but require further filtering
    (called "Banburismus" or manual testing on a replica Enigma).

    left, middle, right: Rotor positions (0-25, i.e., A=0 ... Z=25).
    test_letter:         The letter from which the BFS was seeded.
    wire_value:          The wire value that produced no contradiction.
    """

    left: int
    middle: int
    right: int
    test_letter: int
    wire_value: int

    def positions_str(self) -> str:
        """Return positions as a three-character string, e.g., 'AAA'."""
        return chr(self.left + 65) + chr(self.middle + 65) + chr(self.right + 65)


# ---------------------------------------------------------------------------
# Bombe
# ---------------------------------------------------------------------------


class Bombe:
    """Turing's Bombe (Bletchley Park, 1940).

    Finds candidate Enigma rotor settings (stops) for a given crib/cipher
    menu by testing each of 17,576 (26^3) rotor positions.

    Typical usage::

        bombe = Bombe(rotor_order=["I", "II", "III"], reflector="B")
        menu = BombeMenu.from_crib("HELLO", "XMCKL")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)

    For a full 26^3 scan (may take several seconds in Python)::

        all_stops = bombe.run(menu)
    """

    def __init__(
        self,
        rotor_order: list[str] | None = None,
        reflector: str = "B",
        ring_settings: list[int] | None = None,
    ) -> None:
        """
        Args:
            rotor_order:    Three rotor names (left, middle, right), e.g.,
                            ["I", "II", "III"]. Defaults to I/II/III.
            reflector:      Reflector name, e.g., "B" (UKW-B). Default "B".
            ring_settings:  Ring settings [left, mid, right] as 0-25 ints.
                            Defaults to [0, 0, 0] (AAA).
        """
        if rotor_order is None:
            rotor_order = ["I", "II", "III"]
        if ring_settings is None:
            ring_settings = [0, 0, 0]
        if len(rotor_order) != 3:
            raise ValueError("Bombe requires exactly 3 rotors")
        for name in rotor_order:
            if name not in ROTOR_WIRING:
                raise ValueError(f"Unknown rotor: {name!r}")
        if reflector not in REFLECTOR_WIRING:
            raise ValueError(f"Unknown reflector: {reflector!r}")

        self._rotor_order = rotor_order
        self._reflector = reflector
        self._ring_settings = ring_settings

        # Precompute raw wiring arrays (avoid repeated dict lookups)
        self._fwd_maps: list[list[int]] = [
            [ord(c) - 65 for c in ROTOR_WIRING[name]] for name in rotor_order
        ]
        self._rev_maps: list[list[int]] = []
        for fwd in self._fwd_maps:
            rev = [0] * 26
            for i, o in enumerate(fwd):
                rev[o] = i
            self._rev_maps.append(rev)
        self._reflect_map: list[int] = [ord(c) - 65 for c in REFLECTOR_WIRING[reflector]]
        self._notches_m: list[int] = [to_int(c) for c in ROTOR_NOTCHES.get(rotor_order[1], "")]
        self._notches_r: list[int] = [to_int(c) for c in ROTOR_NOTCHES.get(rotor_order[2], "")]

        # Results from the last run()
        self.last_run_stops: list[BombeStop] = []
        self.last_run_tests: int = 0

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _advance_position(self, L: int, M: int, R: int, steps: int) -> tuple[int, int, int]:
        """Advance (L, M, R) by ``steps`` Enigma steps including double-stepping."""
        nm = self._notches_m
        nr = self._notches_r
        for _ in range(steps):
            rotate_l = M in nm
            rotate_m = (R in nr) or (M in nm)
            if rotate_l:
                L = (L + 1) % 26
            if rotate_m:
                M = (M + 1) % 26
            R = (R + 1) % 26
        return L, M, R

    def _build_lookups(self, base: tuple[int, int, int], max_offset: int) -> list[list[int]]:
        """Build scrambler lookup tables for offsets 0..max_offset from base."""
        L, M, R = base
        lookups: list[list[int]] = []
        for step in range(max_offset + 1):
            # +1 because Enigma steps rotors BEFORE enciphering each character.
            # The scrambler at crib offset 0 must match the Enigma at position
            # after the first step from the initial (L, M, R) setting.
            cL, cM, cR = self._advance_position(L, M, R, step + 1)
            offsets = [
                (cL - self._ring_settings[0]) % 26,
                (cM - self._ring_settings[1]) % 26,
                (cR - self._ring_settings[2]) % 26,
            ]
            lookups.append(
                _make_scrambler_lookup(self._fwd_maps, self._rev_maps, self._reflect_map, offsets)
            )
        return lookups

    def _test_position(
        self,
        menu: BombeMenu,
        base: tuple[int, int, int],
        adj: dict[int, list[MenuLink]],
    ) -> list[BombeStop]:
        """
        Test one base position for Bombe stops.

        Builds scrambler lookups once, then tries all 26 wire values for the
        test register letter. Returns a list of stops (usually 0 or 1).
        """
        if not menu.links:
            return []

        lookups = self._build_lookups(base, menu.max_offset)

        # Choose test register: letter with most menu connections (best closure)
        test_letter = max(adj, key=lambda x: len(adj[x]))

        stops: list[BombeStop] = []
        for initial_value in range(26):
            # Inject: test_letter = initial_value
            assignment: dict[int, int] = {test_letter: initial_value}
            queue: collections.deque[int] = collections.deque([test_letter])
            processed: set[int] = set()
            contradiction = False

            while queue and not contradiction:
                letter = queue.popleft()
                if letter in processed:
                    continue
                processed.add(letter)
                val = assignment[letter]

                for link in adj.get(letter, []):
                    other = link.letter_b if link.letter_a == letter else link.letter_a
                    # WHY same encipher in both directions: the Enigma scrambler
                    # (rotors + reflector, no plugboard) is self-reciprocal --
                    # S(S(k)) = k for any fixed position. So the same lookup
                    # applies regardless of which end of the edge we traverse.
                    new_val = lookups[link.offset][val]
                    if other in assignment:
                        if assignment[other] != new_val:
                            contradiction = True
                            break
                    else:
                        assignment[other] = new_val
                        queue.append(other)

            if not contradiction:
                stops.append(
                    BombeStop(
                        left=base[0],
                        middle=base[1],
                        right=base[2],
                        test_letter=test_letter,
                        wire_value=initial_value,
                    )
                )

        return stops

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def run_single(
        self,
        menu: BombeMenu,
        left: int,
        middle: int,
        right: int,
    ) -> list[BombeStop]:
        """Test a single rotor position.

        Args:
            menu:           Bombe menu from BombeMenu.from_crib().
            left:           Left rotor position (0-25).
            middle:         Middle rotor position (0-25).
            right:          Right rotor position (0-25).

        Returns:
            List of BombeStop for this position (0 or more, typically <=1).
        """
        adj: dict[int, list[MenuLink]] = {}
        for link in menu.links:
            adj.setdefault(link.letter_a, []).append(link)
            adj.setdefault(link.letter_b, []).append(link)
        return self._test_position(menu, (left, middle, right), adj)

    def run(self, menu: BombeMenu) -> list[BombeStop]:
        """Scan all 26^3 = 17,576 rotor positions for Bombe stops.

        A full scan typically takes several seconds in Python.
        Stops are also stored in ``self.last_run_stops``.

        Args:
            menu: Bombe menu from BombeMenu.from_crib().

        Returns:
            All stops found across the full 26^3 position space.
        """
        # Precompute adjacency list once for all positions
        adj: dict[int, list[MenuLink]] = {}
        for link in menu.links:
            adj.setdefault(link.letter_a, []).append(link)
            adj.setdefault(link.letter_b, []).append(link)

        all_stops: list[BombeStop] = []
        for L in range(26):
            for M in range(26):
                for R in range(26):
                    stops = self._test_position(menu, (L, M, R), adj)
                    all_stops.extend(stops)

        self.last_run_stops = all_stops
        self.last_run_tests = 26**3
        return all_stops

    def state(self) -> dict[str, object]:
        """Return a summary of the last run."""
        return {
            "rotor_order": self._rotor_order,
            "reflector": self._reflector,
            "ring_settings": self._ring_settings,
            "last_run_tests": self.last_run_tests,
            "last_run_stops": len(self.last_run_stops),
        }
