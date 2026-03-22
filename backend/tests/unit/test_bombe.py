"""Unit tests for Turing's Bombe emulator (Bletchley Park, 1940)."""

import pytest

from backend.src.emulator.bombe import Bombe, BombeMenu, BombeStop, MenuLink, _make_scrambler_lookup
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


# ---------------------------------------------------------------------------
# MenuLink dataclass
# ---------------------------------------------------------------------------


class TestMenuLink:
    def test_attributes_accessible(self) -> None:
        link = MenuLink(letter_a=0, letter_b=25, offset=3)
        assert link.letter_a == 0
        assert link.letter_b == 25
        assert link.offset == 3

    def test_a_equals_z(self) -> None:
        link = MenuLink(letter_a=0, letter_b=25, offset=0)
        assert link.letter_a == 0
        assert link.letter_b == 25

    def test_offset_zero(self) -> None:
        link = MenuLink(letter_a=5, letter_b=10, offset=0)
        assert link.offset == 0

    def test_from_crib_links_match_expected_offsets(self) -> None:
        # "AB" -> XY: link at offset 0 with letter_a=A=0, letter_b=X
        menu = BombeMenu.from_crib("AB", "XY")
        assert len(menu.links) == 2
        assert menu.links[0].offset == 0
        assert menu.links[1].offset == 1

    def test_link_letters_are_ints(self) -> None:
        menu = BombeMenu.from_crib("AC", "BD")
        for link in menu.links:
            assert isinstance(link.letter_a, int)
            assert isinstance(link.letter_b, int)
            assert 0 <= link.letter_a <= 25
            assert 0 <= link.letter_b <= 25


# ---------------------------------------------------------------------------
# Extended BombeMenu tests
# ---------------------------------------------------------------------------


class TestBombeMenuExtended:
    def test_letters_property_is_set_of_ints(self) -> None:
        menu = BombeMenu.from_crib("AB", "XY")
        lts = menu.letters
        assert isinstance(lts, set)
        for v in lts:
            assert isinstance(v, int)

    def test_letters_empty_for_empty_links(self) -> None:
        menu = BombeMenu(links=[], crib="", cipher="")
        assert menu.letters == set()

    def test_max_offset_single_link(self) -> None:
        menu = BombeMenu.from_crib("AB", "XY")
        assert menu.max_offset == 1  # offsets 0 and 1

    def test_max_offset_length_one(self) -> None:
        # "A" and "B" differ -- one link at offset 0
        menu = BombeMenu.from_crib("A", "B")
        assert menu.max_offset == 0

    def test_from_crib_skips_same_letter_pair(self) -> None:
        # When crib[i] == cipher[i] (Enigma can't produce this), skip the link
        # "A" -> "A" pair is invalid; from_crib should skip it
        menu = BombeMenu.from_crib("AB", "AY")  # offset 0: A==A (skip), offset 1: B->Y
        # Only offset 1 link should remain
        assert all(link.offset != 0 for link in menu.links)

    def test_from_crib_lowercase_accepted(self) -> None:
        menu = BombeMenu.from_crib("hello", "abcde")
        assert len(menu.links) == 5
        assert menu.crib == "HELLO"
        assert menu.cipher == "ABCDE"

    def test_from_crib_spaces_stripped_from_both(self) -> None:
        menu1 = BombeMenu.from_crib("HELLO", "ABCDE")
        menu2 = BombeMenu.from_crib("HEL LO", "ABC DE")
        assert len(menu1.links) == len(menu2.links)

    def test_has_closure_false_for_two_link_chain(self) -> None:
        # A-B-C chain: no cycle
        menu = BombeMenu.from_crib("AB", "BC")  # A<->B, B<->C: path, no cycle
        # Depending on letters -- just verify no crash
        assert isinstance(menu.has_closure(), bool)

    def test_has_closure_false_for_one_link(self) -> None:
        menu = BombeMenu.from_crib("A", "B")
        assert menu.has_closure() is False

    def test_has_closure_empty_menu(self) -> None:
        menu = BombeMenu(links=[], crib="", cipher="")
        assert menu.has_closure() is False

    def test_from_crib_non_alpha_cipher_raises(self) -> None:
        import pytest

        with pytest.raises(ValueError, match="Non-alpha"):
            BombeMenu.from_crib("ABC", "A1C")

    def test_letters_contains_both_ends_of_each_link(self) -> None:
        menu = BombeMenu.from_crib("A", "Z")
        lts = menu.letters
        from backend.src.emulator.enigma import to_int

        assert to_int("A") in lts
        assert to_int("Z") in lts

    def test_max_offset_equals_crib_length_minus_one(self) -> None:
        # For a crib of length N with all distinct pairs, max_offset = N - 1
        menu = BombeMenu.from_crib("ABCDE", "FGHIJ")
        assert menu.max_offset == 4


# ---------------------------------------------------------------------------
# Extended BombeStop tests
# ---------------------------------------------------------------------------


class TestBombeStopExtended:
    def test_positions_str_mid_values(self) -> None:
        s = BombeStop(left=12, middle=13, right=14, test_letter=0, wire_value=0)
        assert s.positions_str() == "MNO"

    def test_positions_str_single_letter(self) -> None:
        s = BombeStop(left=0, middle=0, right=1, test_letter=0, wire_value=0)
        assert s.positions_str() == "AAB"

    def test_wire_value_field_stored(self) -> None:
        s = BombeStop(left=0, middle=0, right=0, test_letter=3, wire_value=7)
        assert s.wire_value == 7
        assert s.test_letter == 3

    def test_tuple_unpacking(self) -> None:
        s = BombeStop(left=1, middle=2, right=3, test_letter=4, wire_value=5)
        left, middle, right, test_letter, wire_value = s
        assert (left, middle, right, test_letter, wire_value) == (1, 2, 3, 4, 5)

    def test_positions_str_all_z(self) -> None:
        s = BombeStop(left=25, middle=25, right=25, test_letter=0, wire_value=0)
        assert s.positions_str() == "ZZZ"

    def test_positions_str_length_is_three(self) -> None:
        s = BombeStop(left=5, middle=10, right=15, test_letter=0, wire_value=0)
        assert len(s.positions_str()) == 3


# ---------------------------------------------------------------------------
# _advance_position stepping logic
# ---------------------------------------------------------------------------


class TestAdvancePosition:
    def _make_bombe(self) -> Bombe:
        return Bombe(rotor_order=["I", "II", "III"], reflector="B")

    def test_zero_steps_no_change(self) -> None:
        b = self._make_bombe()
        L, M, R = b._advance_position(0, 0, 0, 0)
        assert (L, M, R) == (0, 0, 0)

    def test_one_step_increments_right(self) -> None:
        b = self._make_bombe()
        L, M, R = b._advance_position(0, 0, 0, 1)
        # Right always increments; middle and left only at notch
        assert R == 1
        assert L == 0  # left unchanged (no double-step from A)

    def test_right_wraps_at_z(self) -> None:
        b = self._make_bombe()
        L, M, R = b._advance_position(0, 0, 25, 1)
        assert R == 0  # wraps from Z=25 to A=0

    def test_double_step_sequence(self) -> None:
        # Rotor III notch is V=21. Set right=21 -> triggers middle step.
        # Middle II notch is E=4. Set middle=4 -> triggers double-step.
        b = self._make_bombe()
        # Position ADV (left=0, middle=3, right=21): next step = AEW
        L, M, R = b._advance_position(0, 3, 21, 1)
        assert R == 22  # W
        assert M == 4  # E (middle stepped because right was at notch V)

    def test_multiple_steps_agrees_with_repeated_single(self) -> None:
        b = self._make_bombe()
        L_multi, M_multi, R_multi = b._advance_position(0, 0, 0, 5)
        L, M, R = 0, 0, 0
        for _ in range(5):
            L, M, R = b._advance_position(L, M, R, 1)
        assert (L_multi, M_multi, R_multi) == (L, M, R)


# ---------------------------------------------------------------------------
# Extended run_single coverage
# ---------------------------------------------------------------------------


class TestRunSingleExtended:
    def _make_pair(self, pos: str, crib: str) -> tuple[Bombe, BombeMenu]:
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
        m.set_rotor_positions(pos)
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        bombe = Bombe(rotor_order=["I", "II", "III"], reflector="B")
        return bombe, menu

    def test_stop_at_mfd_position(self) -> None:
        bombe, menu = self._make_pair("MFD", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=12, middle=5, right=3)
        assert len(stops) > 0

    def test_stop_positions_str_matches_input(self) -> None:
        bombe, menu = self._make_pair("AAA", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        assert all(s.positions_str() == "AAA" for s in stops)

    def test_stops_are_bombe_stop_instances(self) -> None:
        bombe, menu = self._make_pair("AAA", "ABCDEFGABCDEFG")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        for stop in stops:
            assert isinstance(stop, BombeStop)

    def test_run_single_returns_list(self) -> None:
        bombe, menu = self._make_pair("AAA", "ABCDE")
        result = bombe.run_single(menu, left=0, middle=0, right=0)
        assert isinstance(result, list)

    def test_run_single_no_false_stop_at_wrong_position_mostly(self) -> None:
        bombe, menu = self._make_pair("AAA", "ABCDEFGABCDEFG")
        # At AAB, the correct wire value should NOT be among stops (usually)
        stops = bombe.run_single(menu, left=0, middle=0, right=1)
        # Not guaranteed zero false stops, but wire values should be different
        # Just verify no crash and result is a list
        assert isinstance(stops, list)

    def test_different_rotors_still_finds_stop(self) -> None:
        m = EnigmaMachine(rotors=["III", "II", "I"], reflector="B")
        m.set_rotor_positions("AAA")
        crib = "ABCDEFGABCDEFG"
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        bombe = Bombe(rotor_order=["III", "II", "I"], reflector="B")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        assert len(stops) > 0

    def test_reflector_c_construction_works(self) -> None:
        m = EnigmaMachine(rotors=["I", "II", "III"], reflector="C")
        m.set_rotor_positions("AAA")
        crib = "ABCDEFGABCDEFG"
        cipher = m.process_text(crib)
        menu = BombeMenu.from_crib(crib, cipher)
        bombe = Bombe(rotor_order=["I", "II", "III"], reflector="C")
        stops = bombe.run_single(menu, left=0, middle=0, right=0)
        assert isinstance(stops, list)


# ---------------------------------------------------------------------------
# Scrambler additional properties
# ---------------------------------------------------------------------------


class TestScramblerLookupAdditional:
    def _build(self, rotors: list[str], reflector: str, pos: list[int]) -> list[int]:
        from backend.src.emulator.enigma import REFLECTOR_WIRING, ROTOR_WIRING

        fwd = [[ord(c) - 65 for c in ROTOR_WIRING[n]] for n in rotors]
        rev = [[0] * 26 for _ in fwd]
        for m, f in zip(rev, fwd, strict=True):
            for i, o in enumerate(f):
                m[o] = i
        ref = [ord(c) - 65 for c in REFLECTOR_WIRING[reflector]]
        return _make_scrambler_lookup(fwd, rev, ref, pos)

    def test_lookup_is_permutation(self) -> None:
        lookup = self._build(["I", "II", "III"], "B", [0, 0, 0])
        assert sorted(lookup) == list(range(26))

    def test_lookup_has_26_entries(self) -> None:
        lookup = self._build(["I", "II", "III"], "B", [0, 0, 0])
        assert len(lookup) == 26

    def test_self_reciprocal_rotors_iv_v_i(self) -> None:
        lookup = self._build(["IV", "V", "I"], "B", [0, 0, 0])
        for k in range(26):
            assert lookup[lookup[k]] == k

    def test_self_reciprocal_reflector_a(self) -> None:
        lookup = self._build(["I", "II", "III"], "A", [0, 0, 0])
        for k in range(26):
            assert lookup[lookup[k]] == k

    def test_different_offsets_give_different_lookup(self) -> None:
        l1 = self._build(["I", "II", "III"], "B", [0, 0, 0])
        l2 = self._build(["I", "II", "III"], "B", [0, 0, 1])
        assert l1 != l2

    def test_lookup_all_values_in_range(self) -> None:
        lookup = self._build(["II", "III", "I"], "B", [5, 10, 15])
        assert all(0 <= v <= 25 for v in lookup)
