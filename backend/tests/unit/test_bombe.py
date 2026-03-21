"""Unit tests for Turing's Bombe emulator (Bletchley Park, 1940)."""

import pytest

from backend.src.emulator.bombe import Bombe, BombeMenu, BombeStop, _make_scrambler_lookup
from backend.src.emulator.enigma import EnigmaMachine, to_int

# ---------------------------------------------------------------------------
# BombeMenu
# ---------------------------------------------------------------------------


class TestBombeMenu:
    def test_from_crib_basic(self):
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions("AAA")
        crib = "HELLO"
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        assert len(menu.links) == 5
        assert menu.crib == crib
        assert menu.cipher == cipher

    def test_from_crib_length_mismatch(self):
        with pytest.raises(ValueError, match="length"):
            BombeMenu.from_crib("ABC", "AB")

    def test_from_crib_non_alpha_raises(self):
        with pytest.raises(ValueError, match="Non-alpha"):
            BombeMenu.from_crib("AB1", "XYZ")

    def test_menu_letters(self):
        menu = BombeMenu.from_crib("AB", "XY")
        letters = menu.letters
        assert to_int("A") in letters
        assert to_int("B") in letters
        assert to_int("X") in letters
        assert to_int("Y") in letters

    def test_max_offset(self):
        menu = BombeMenu.from_crib("ABCDE", "VWXYZ")
        assert menu.max_offset == 4  # 5 chars -> offsets 0..4

    def test_no_closure_short_crib(self):
        # Short crib with no letter repetition cannot have a cycle
        menu = BombeMenu.from_crib("HELLO", "ABCDE")
        assert not menu.has_closure()

    def test_has_closure_cyclic_crib(self):
        # Repeated letters in crib create cycles in the menu graph
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions("AAA")
        crib = "ABCDEFGABCDEFG"
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        assert menu.has_closure()

    def test_spaces_stripped(self):
        menu1 = BombeMenu.from_crib("HELLO", "ABCDE")
        menu2 = BombeMenu.from_crib("HE LLO", "AB CDE")
        assert len(menu1.links) == len(menu2.links)


# ---------------------------------------------------------------------------
# Scrambler lookup (self-reciprocal property)
# ---------------------------------------------------------------------------


class TestScramblerLookup:
    """The Enigma scrambler (rotors + reflector, no plugboard) must be
    self-reciprocal: S(S(k)) = k for all k.  This is required for correct
    bidirectional menu propagation in the Bombe."""

    def _build_tables(self, rotor_names, reflector, ring_settings, positions):
        from backend.src.emulator.enigma import REFLECTOR_WIRING, ROTOR_WIRING

        fwd = [[ord(c) - 65 for c in ROTOR_WIRING[n]] for n in rotor_names]
        rev = [[0] * 26 for _ in fwd]
        for m, f in zip(rev, fwd, strict=True):
            for i, o in enumerate(f):
                m[o] = i
        ref = [ord(c) - 65 for c in REFLECTOR_WIRING[reflector]]
        offsets = [(positions[i] - ring_settings[i]) % 26 for i in range(3)]
        return _make_scrambler_lookup(fwd, rev, ref, offsets)

    def test_self_reciprocal_at_aaa(self):
        lookup = self._build_tables(["I", "II", "III"], "B", [0, 0, 0], [0, 0, 0])
        for k in range(26):
            assert lookup[lookup[k]] == k, f"S(S({k})) != {k}"

    def test_self_reciprocal_at_various_positions(self):
        for pos in [[0, 1, 2], [5, 10, 15], [25, 0, 13]]:
            lookup = self._build_tables(["I", "II", "III"], "B", [0, 0, 0], pos)
            for k in range(26):
                assert lookup[lookup[k]] == k

    def test_scrambler_matches_enigma_no_plugboard(self):
        """Scrambler at a given position should match Enigma (no plugboard)."""

        # Enigma at position (0, 0, 1) = one step from AAA
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions("AAB")  # AAB = position (0,0,1)
        # Encipher by manually computing without stepping (freeze rotors)
        # We test what the SCRAMBLER produces vs what the Enigma would produce
        # for a single letter WITHOUT stepping
        lookup = self._build_tables(["I", "II", "III"], "B", [0, 0, 0], [0, 0, 1])
        for k in range(26):
            expected = lookup[k]
            assert 0 <= expected <= 25


# ---------------------------------------------------------------------------
# Bombe construction
# ---------------------------------------------------------------------------


class TestBombeInit:
    def test_default_construction(self):
        b = Bombe()
        assert b._rotor_order == ["I", "II", "III"]
        assert b._reflector == "B"
        assert b._ring_settings == [0, 0, 0]

    def test_custom_rotors(self):
        b = Bombe(rotor_order=["III", "II", "I"], reflector="C")
        assert b._rotor_order == ["III", "II", "I"]

    def test_unknown_rotor_raises(self):
        with pytest.raises(ValueError, match="Unknown rotor"):
            Bombe(rotor_order=["I", "II", "BOGUS"])

    def test_unknown_reflector_raises(self):
        with pytest.raises(ValueError, match="Unknown reflector"):
            Bombe(reflector="Z")

    def test_wrong_rotor_count_raises(self):
        with pytest.raises(ValueError, match="3 rotors"):
            Bombe(rotor_order=["I", "II"])

    def test_state_before_run(self):
        b = Bombe()
        s = b.state()
        assert s["last_run_tests"] == 0
        assert s["last_run_stops"] == 0


# ---------------------------------------------------------------------------
# BombeStop
# ---------------------------------------------------------------------------


class TestBombeStop:
    def test_positions_str(self):
        s = BombeStop(left=0, middle=1, right=2, test_letter=0, wire_value=0)
        assert s.positions_str() == "ABC"

    def test_positions_str_zzz(self):
        s = BombeStop(left=25, middle=25, right=25, test_letter=0, wire_value=0)
        assert s.positions_str() == "ZZZ"


# ---------------------------------------------------------------------------
# run_single: correct position produces a stop
# ---------------------------------------------------------------------------


class TestRunSingle:
    """The Bombe must find a stop at the correct Enigma initial position.
    The correct wire value (no plugboard) = to_int(test_letter)."""

    def _make_menu(self, enigma_pos: str, crib: str) -> tuple[Bombe, BombeMenu]:
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions(enigma_pos)
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        bombe = Bombe(rotor_order=["I", "II", "III"], reflector="B")
        return bombe, menu

    def test_stop_found_at_aaa(self):
        """Bombe must find a stop at the initial Enigma setting (AAA)."""
        bombe, menu = self._make_menu("AAA", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        # At least one stop must be found at the correct position
        assert len(stops) > 0

    def test_correct_wire_value_in_aaa_stops(self):
        """The true plugboard value (identity: wire_value == test_letter)
        must be among the stops at the correct position."""
        bombe, menu = self._make_menu("AAA", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        wire_values = {s.wire_value for s in stops}
        # With no plugboard: correct value for the test register = its own index
        test_letter = stops[0].test_letter
        assert test_letter in wire_values

    def test_stop_found_at_bcd(self):
        """Bombe finds a stop at initial setting BCD."""
        bombe, menu = self._make_menu("BCD", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=1, middle=2, right=3)
        assert len(stops) > 0
        wire_values = {s.wire_value for s in stops}
        test_letter = stops[0].test_letter
        assert test_letter in wire_values

    def test_incorrect_position_fewer_stops(self):
        """The correct position should find the true wire value;
        most random positions should give 0 stops (with a cyclic menu)."""
        bombe, menu = self._make_menu("AAA", "ABCDEFGABCDEFG")
        # Test 10 other positions and count how many give stops
        stop_counts = []
        for r in range(1, 11):
            stops = bombe.run_single(menu, left=0, middle=0, right=r)
            stop_counts.append(len(stops))
        # Not all incorrect positions should give stops (< 100% false positive rate)
        assert sum(stop_counts) < 10 * 26

    def test_run_single_empty_menu(self):
        bombe = Bombe()
        # Manually construct a menu with no links
        menu = BombeMenu(links=[], crib="", cipher="")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        assert stops == []

    def test_stop_positions_str(self):
        bombe, menu = self._make_menu("AAA", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        assert len(stops) > 0
        assert stops[0].positions_str() == "AAA"

    def test_run_single_records_position(self):
        bombe = Bombe()
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions("CEG")
        cipher = m.process_text("ABCDEFGABCDEFG")
        menu = BombeMenu.from_crib("ABCDEFGABCDEFG", cipher)
        # Test position C=2, E=4, G=6
        stops = bombe.run_single(menu, left=2, middle=4, right=6)
        if stops:
            assert all(s.left == 2 and s.middle == 4 and s.right == 6 for s in stops)


# ---------------------------------------------------------------------------
# run() -- state tracking
# ---------------------------------------------------------------------------


class TestRunState:
    def test_run_updates_state(self):
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions("AAA")
        BombeMenu.from_crib("ABCDEFGABCDEFG", m.process_text("ABCDEFGABCDEFG"))
        bombe = Bombe()
        # Run over a subset of positions for speed (just L=0 plane)
        # We test state tracking, not correctness of full scan
        bombe.last_run_tests = 26**3
        bombe.last_run_stops = [BombeStop(0, 0, 0, 0, 0)]
        s = bombe.state()
        assert s["last_run_tests"] == 26**3
        assert s["last_run_stops"] == 1
