"""Machine Registry -- central catalog of all emulated historical machines.

Each entry describes one machine: metadata, program input format, example
payload, and a factory function that returns a live (machine, adapter) pair
ready for use by the REST API.

WHY a registry (not direct imports per endpoint): the API needs to enumerate
machines, serve their manuals, and construct fresh instances on demand without
coupling the HTTP layer to individual emulator constructors.
"""

from __future__ import annotations

import importlib
from collections.abc import Callable
from dataclasses import dataclass, field
from typing import Any

from .adapter import (
    AEMachineAdapter,
    AntikytheraAdapter,
    BombeAdapter,
    ColossusAdapter,
    CurtaAdapter,
    EDSACAdapter,
    ENIACAdapter,
    EnigmaAdapter,
    GrantDEAdapter,
    HarvardMarkIAdapter,
    HollerithAdapter,
    LeibnizAdapter,
    LudgateAdapter,
    MachineAdapter,
    ManchesterBabyAdapter,
    MillionaireAdapter,
    NapierAdapter,
    OdhnerAdapter,
    PascalineAdapter,
    ScheutzAdapter,
    ThomasArithometerAdapter,
    TorresQuevedoAdapter,
    ZuseZ1Adapter,
    ZuseZ3Adapter,
)


@dataclass
class MachineEntry:
    """Catalog entry for one emulated machine."""

    id: str  # URL-safe slug, e.g. "zuse-z3"
    name: str  # Full display name
    year: int  # Year of completion / first operation
    country: str
    inventor: str
    category: str  # calculator | difference_engine | analytical_engine |
    #                cipher | stored_program | astronomical | tabulator
    brief: str  # One-sentence description
    program_input_type: str  # assembly | instructions_json | words |
    #                          crib_cipher | lorenz_tape | plaintext |
    #                          coefficients | data_deck | date | none
    manual: str  # Full manual text (from module docstring)
    example_payload: dict[str, Any]  # Default program ready to POST /load
    factory: Callable[[], tuple[Any, MachineAdapter]]  # () -> (machine, adapter)
    tags: list[str] = field(default_factory=list)


def _load_manual(module_path: str) -> str:
    """Extract the module-level docstring from an emulator module."""
    try:
        mod = importlib.import_module(module_path)
        return (mod.__doc__ or "").strip()
    except ImportError:
        return ""


# ---------------------------------------------------------------------------
# Factory helpers
# ---------------------------------------------------------------------------


def _make_pascaline() -> tuple[Any, MachineAdapter]:
    from .pascaline import PascalineEmulator

    m = PascalineEmulator(digits=6)
    return m, PascalineAdapter(m)


def _make_leibniz() -> tuple[Any, MachineAdapter]:
    from .leibniz_reckoner import LeibnizReckonerEmulator

    m = LeibnizReckonerEmulator()
    return m, LeibnizAdapter(m)


def _make_napier() -> tuple[Any, MachineAdapter]:
    from .napiers_bones import NapiersBones

    m = NapiersBones()
    m.load_number(12345)
    return m, NapierAdapter(m)


def _make_thomas() -> tuple[Any, MachineAdapter]:
    from .thomas_arithmometer import ThomasArithmometer

    m = ThomasArithmometer()
    return m, ThomasArithometerAdapter(m)


def _make_scheutz() -> tuple[Any, MachineAdapter]:
    from .scheutz import ScheutzDifferenceEngine

    m = ScheutzDifferenceEngine()
    m.load([0.0, 6.0, 2.0, 0.0])
    return m, ScheutzAdapter(m)


def _make_grant_de() -> tuple[Any, MachineAdapter]:
    from .grant_difference_engine import GrantDifferenceEngine

    m = GrantDifferenceEngine()
    m.load([0.0, 6.0, 2.0, 0.0])
    return m, GrantDEAdapter(m)


def _make_odhner() -> tuple[Any, MachineAdapter]:
    from .odhner_arithmometer import OdhnerArithmometer

    m = OdhnerArithmometer()
    return m, OdhnerAdapter(m)


def _make_millionaire() -> tuple[Any, MachineAdapter]:
    from .millionaire_calculator import MillionaireCalculator

    m = MillionaireCalculator()
    return m, MillionaireAdapter(m)


def _make_hollerith() -> tuple[Any, MachineAdapter]:
    from .hollerith_tabulator import HollerithTabulator

    m = HollerithTabulator()
    return m, HollerithAdapter(m)


def _make_antikythera() -> tuple[Any, MachineAdapter]:
    from .antikythera import AntikytheraMechanism

    m = AntikytheraMechanism()
    return m, AntikytheraAdapter(m)


def _make_ludgate() -> tuple[Any, MachineAdapter]:
    from .ludgate import LudgateMachine

    m = LudgateMachine()
    return m, LudgateAdapter(m)


def _make_torres() -> tuple[Any, MachineAdapter]:
    from .torres_quevedo import TorresQuevedo

    m = TorresQuevedo()
    return m, TorresQuevedoAdapter(m)


def _make_enigma() -> tuple[Any, MachineAdapter]:
    from .enigma import EnigmaMachine

    m = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
    m.set_rotor_positions("AAA")
    return m, EnigmaAdapter(m)


def _make_bombe() -> tuple[Any, MachineAdapter]:
    from .bombe import Bombe, BombeMenu
    from .enigma import EnigmaMachine

    # Build a default crib/cipher pair using Enigma AAA
    enigma = EnigmaMachine(rotors=["I", "II", "III"], reflector="B")
    enigma.set_rotor_positions("AAA")
    crib = "ABCDEFGABCDEFG"
    cipher = enigma.process_text(crib)
    menu = BombeMenu.from_crib(crib, cipher)
    bombe = Bombe(rotor_order=["I", "II", "III"], reflector="B")
    return bombe, BombeAdapter(bombe, menu)


def _make_zuse_z1() -> tuple[Any, MachineAdapter]:
    from .zuse_z1 import ZuseZ1

    m = ZuseZ1()
    return m, ZuseZ1Adapter(m)


def _make_zuse_z3() -> tuple[Any, MachineAdapter]:
    from .zuse_z3 import ZuseZ3

    m = ZuseZ3()
    return m, ZuseZ3Adapter(m)


def _make_colossus() -> tuple[Any, MachineAdapter]:
    from .colossus import Colossus, LorenzSZ42

    lorenz = LorenzSZ42.with_random_key(seed=42)
    colossus = Colossus(lorenz)
    return colossus, ColossusAdapter(colossus)


def _make_harvard_mark_i() -> tuple[Any, MachineAdapter]:
    from .harvard_mark_i import HarvardMarkI

    m = HarvardMarkI()
    return m, HarvardMarkIAdapter(m)


def _make_eniac() -> tuple[Any, MachineAdapter]:
    from .eniac import ENIAC

    m = ENIAC()
    return m, ENIACAdapter(m)


def _make_manchester_baby() -> tuple[Any, MachineAdapter]:
    from .manchester_baby import ManchesterBaby

    m = ManchesterBaby()
    return m, ManchesterBabyAdapter(m)


def _make_edsac() -> tuple[Any, MachineAdapter]:
    from .edsac import EDSAC

    m = EDSAC()
    return m, EDSACAdapter(m)


def _make_curta() -> tuple[Any, MachineAdapter]:
    from .curta import CurtaTypeI

    m = CurtaTypeI()
    return m, CurtaAdapter(m)


def _make_ae() -> tuple[Any, MachineAdapter]:
    from .analytical_engine import Engine

    m = Engine()
    return m, AEMachineAdapter(m)


# ---------------------------------------------------------------------------
# Registry
# ---------------------------------------------------------------------------

_BASE = "backend.src.emulator."

MACHINES: list[MachineEntry] = [
    # -----------------------------------------------------------------------
    # Astronomical / ancient
    # -----------------------------------------------------------------------
    MachineEntry(
        id="antikythera",
        name="Antikythera Mechanism",
        year=-100,
        country="Greece",
        inventor="Unknown (Rhodes workshop)",
        category="astronomical",
        brief="Oldest known analog computer; predicts astronomical positions and eclipses.",
        program_input_type="date",
        manual=_load_manual(_BASE + "antikythera"),
        example_payload={"years": 0.5},
        factory=_make_antikythera,
        tags=["analog", "gears", "astronomy", "ancient"],
    ),
    # -----------------------------------------------------------------------
    # Calculators
    # -----------------------------------------------------------------------
    MachineEntry(
        id="napiers-bones",
        name="Napier's Bones",
        year=1617,
        country="Scotland",
        inventor="John Napier",
        category="calculator",
        brief="Ivory rods enabling rapid multiplication and division via diagonal addition.",
        program_input_type="none",
        manual=_load_manual(_BASE + "napiers_bones"),
        example_payload={"number": 12345, "multiplier": 7},
        factory=_make_napier,
        tags=["manual", "multiplication", "ivory"],
    ),
    MachineEntry(
        id="pascaline",
        name="Pascaline",
        year=1642,
        country="France",
        inventor="Blaise Pascal",
        category="calculator",
        brief="World's first mechanical adding machine; carry propagates via the sautoir.",
        program_input_type="none",
        manual=_load_manual(_BASE + "pascaline"),
        example_payload={"add": 12345, "subtract": 678},
        factory=_make_pascaline,
        tags=["adding machine", "carry", "mechanical"],
    ),
    MachineEntry(
        id="leibniz-reckoner",
        name="Leibniz Stepped Reckoner",
        year=1673,
        country="Germany",
        inventor="Gottfried Wilhelm Leibniz",
        category="calculator",
        brief="First four-function mechanical calculator; uses the Staffelwalze stepped drum.",
        program_input_type="none",
        manual=_load_manual(_BASE + "leibniz_reckoner"),
        example_payload={"input": 456, "turns": 3},
        factory=_make_leibniz,
        tags=["stepped drum", "four operations", "mechanical"],
    ),
    MachineEntry(
        id="thomas-arithmometer",
        name="Thomas Arithmometer",
        year=1820,
        country="France",
        inventor="Charles Xavier Thomas de Colmar",
        category="calculator",
        brief="First commercially successful mechanical calculator; mass-produced from 1851.",
        program_input_type="none",
        manual=_load_manual(_BASE + "thomas_arithmometer"),
        example_payload={"input": 9999, "mode": "ADD", "turns": 4},
        factory=_make_thomas,
        tags=["commercial", "production", "mechanical"],
    ),
    MachineEntry(
        id="odhner-arithmometer",
        name="Odhner Arithmometer",
        year=1878,
        country="Sweden",
        inventor="Willgodt Theophil Odhner",
        category="calculator",
        brief="Pinwheel calculator; compact design became the standard for 70 years.",
        program_input_type="none",
        manual=_load_manual(_BASE + "odhner_arithmometer"),
        example_payload={"input": 7654, "mode": "ADD", "turns": 3},
        factory=_make_odhner,
        tags=["pinwheel", "compact", "mechanical"],
    ),
    MachineEntry(
        id="hollerith-tabulator",
        name="Hollerith Tabulating Machine",
        year=1890,
        country="USA",
        inventor="Herman Hollerith",
        category="tabulator",
        brief="Electromechanical punch-card tabulator; used for the 1890 US Census.",
        program_input_type="data_deck",
        manual=_load_manual(_BASE + "hollerith_tabulator"),
        example_payload={
            "cards": [
                {"row": 0, "columns": [3, 7, 12]},
                {"row": 0, "columns": [7, 15]},
                {"row": 1, "columns": [3, 12, 20]},
            ]
        },
        factory=_make_hollerith,
        tags=["punch card", "census", "electromechanical"],
    ),
    MachineEntry(
        id="millionaire-calculator",
        name="Millionaire Calculator",
        year=1893,
        country="Switzerland",
        inventor="Otto Steiger",
        category="calculator",
        brief="First direct multiplication calculator; one crank turn per digit of result.",
        program_input_type="none",
        manual=_load_manual(_BASE + "millionaire_calculator"),
        example_payload={"input": 123, "multiplier_lever": 9, "turns": 1},
        factory=_make_millionaire,
        tags=["direct multiplication", "mechanical"],
    ),
    MachineEntry(
        id="curta",
        name="Curta Type I",
        year=1948,
        country="Liechtenstein",
        inventor="Curt Herzstark",
        category="calculator",
        brief='"Math grenade" -- world\'s smallest four-function mechanical calculator.',
        program_input_type="none",
        manual=_load_manual(_BASE + "curta"),
        example_payload={"sliders": [1, 2, 3, 4, 0, 0, 0, 0], "turns": 3, "mode": "ADD"},
        factory=_make_curta,
        tags=["portable", "mechanical", "compact"],
    ),
    # -----------------------------------------------------------------------
    # Difference Engines
    # -----------------------------------------------------------------------
    MachineEntry(
        id="scheutz-de",
        name="Scheutz Difference Engine",
        year=1855,
        country="Sweden",
        inventor="Georg and Edvard Scheutz",
        category="difference_engine",
        brief="First operational difference engine; printed mathematical tables automatically.",
        program_input_type="coefficients",
        manual=_load_manual(_BASE + "scheutz"),
        example_payload={"initial_values": [0, 6, 2, 0], "steps": 10},
        factory=_make_scheutz,
        tags=["tabulation", "printing", "Babbage"],
    ),
    MachineEntry(
        id="grant-de",
        name="Grant Difference Engine",
        year=1876,
        country="USA",
        inventor="George Barnard Grant",
        category="difference_engine",
        brief="American-built difference engine; one of the largest ever constructed.",
        program_input_type="coefficients",
        manual=_load_manual(_BASE + "grant_difference_engine"),
        example_payload={"initial_values": [0.0, 6.0, 2.0, 0.0], "steps": 10},
        factory=_make_grant_de,
        tags=["tabulation", "American", "large"],
    ),
    # -----------------------------------------------------------------------
    # Analytical Engines / Programmable
    # -----------------------------------------------------------------------
    MachineEntry(
        id="ludgate",
        name="Ludgate Analytical Machine",
        year=1909,
        country="Ireland",
        inventor="Percy Ludgate",
        category="analytical_engine",
        brief="Independent analytical engine design; logarithmic index numbers for multiplication.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "ludgate"),
        example_payload={
            "program": [
                ["mult", 0, 1],
                ["print"],
            ],
            "variables": {0: 7, 1: 6},
        },
        factory=_make_ludgate,
        tags=["Irish", "logarithms", "independent"],
    ),
    MachineEntry(
        id="torres-quevedo",
        name="Torres Quevedo Electromechanical Calculator",
        year=1914,
        country="Spain",
        inventor="Leonardo Torres Quevedo",
        category="analytical_engine",
        brief="Electromechanical FP calculator; first machine with remote typewriter output.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "torres_quevedo"),
        example_payload={
            "program": [
                ["add", 0, 1, 2],
                ["print", 2],
            ],
            "registers": {0: 3.14, 1: 2.71},
        },
        factory=_make_torres,
        tags=["electromechanical", "floating point", "Spanish"],
    ),
    MachineEntry(
        id="analytical-engine",
        name="Babbage Analytical Engine",
        year=1843,
        country="United Kingdom",
        inventor="Charles Babbage / Ada Lovelace",
        category="analytical_engine",
        brief="First design for a general-purpose programmable computer; Ada wrote Note G.",
        program_input_type="assembly",
        manual=_load_manual(_BASE + "analytical_engine.engine"),
        example_payload={
            "source": (
                "LOAD A, 7\n"
                "STOR A, [0]\n"
                "LOAD A, 6\n"
                "STOR A, [1]\n"
                "LOAD A, [0]\n"
                "LOAD B, [1]\n"
                "MULT A, B\n"
                "WRPRN A\n"
                "HALT"
            )
        },
        factory=_make_ae,
        tags=["Ada Lovelace", "Turing complete", "Victorian"],
    ),
    # -----------------------------------------------------------------------
    # Cipher machines
    # -----------------------------------------------------------------------
    MachineEntry(
        id="enigma",
        name="Enigma Machine",
        year=1918,
        country="Germany",
        inventor="Arthur Scherbius",
        category="cipher",
        brief="Rotor-based electromechanical cipher; broken by Turing's Bombe at Bletchley Park.",
        program_input_type="plaintext",
        manual=_load_manual(_BASE + "enigma"),
        example_payload={
            "rotors": ["I", "II", "III"],
            "reflector": "B",
            "positions": "AAA",
            "plaintext": "HELLOWORLD",
        },
        factory=_make_enigma,
        tags=["WW2", "cipher", "rotor", "crypto"],
    ),
    MachineEntry(
        id="bombe",
        name="Turing Bombe",
        year=1940,
        country="United Kingdom",
        inventor="Alan Turing / Gordon Welchman",
        category="cipher",
        brief="Electromechanical cryptanalysis machine; defeated the Enigma by menu-graph BFS.",
        program_input_type="crib_cipher",
        manual=_load_manual(_BASE + "bombe"),
        example_payload={
            "rotors": ["I", "II", "III"],
            "reflector": "B",
            "crib": "ABCDEFGABCDEFG",
            "cipher": "BDZGOWCXLTKSBT",
        },
        factory=_make_bombe,
        tags=["WW2", "cryptanalysis", "Bletchley", "Turing"],
    ),
    MachineEntry(
        id="colossus",
        name="Colossus Mark 2",
        year=1944,
        country="United Kingdom",
        inventor="Tommy Flowers",
        category="cipher",
        brief="World's first programmable electronic computer; broke the Lorenz SZ42 Tunny cipher.",
        program_input_type="lorenz_tape",
        manual=_load_manual(_BASE + "colossus"),
        example_payload={
            "tape": [[1, 0, 1, 0, 1]] * 50,
            "function": "XOR_sum_odd",
            "wheel_index": 0,
        },
        factory=_make_colossus,
        tags=["WW2", "electronic", "Lorenz", "Bletchley"],
    ),
    # -----------------------------------------------------------------------
    # Relay / early electronic computers
    # -----------------------------------------------------------------------
    MachineEntry(
        id="zuse-z1",
        name="Zuse Z1",
        year=1938,
        country="Germany",
        inventor="Konrad Zuse",
        category="stored_program",
        brief="World's first mechanical binary computer; 22-bit FP, punched tape control.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "zuse_z1"),
        example_payload={
            "memory": {0: 2.0},
            "program": [
                {"op": "LOAD", "address": 0},
                {"op": "SQRT"},
                {"op": "PRINT"},
                {"op": "HALT"},
            ],
        },
        factory=_make_zuse_z1,
        tags=["binary", "floating point", "relay", "German"],
    ),
    MachineEntry(
        id="zuse-z3",
        name="Zuse Z3",
        year=1941,
        country="Germany",
        inventor="Konrad Zuse",
        category="stored_program",
        brief="World's first operational fully automatic programmable computer; relay-based.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "zuse_z3"),
        example_payload={
            "memory": {0: 4.0},
            "input_tape": [],
            "program": [
                {"op": "LOAD", "address": 0},
                {"op": "SQRT"},
                {"op": "PRINT"},
                {"op": "HALT"},
            ],
        },
        factory=_make_zuse_z3,
        tags=["relay", "floating point", "German"],
    ),
    MachineEntry(
        id="harvard-mark-i",
        name="Harvard Mark I (IBM ASCC)",
        year=1944,
        country="USA",
        inventor="Howard Aiken / Grace Hopper",
        category="stored_program",
        brief="First large-scale automatic sequence-controlled calculator in the US; 23-digit.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "harvard_mark_i"),
        example_payload={
            "counters": {0: 7, 1: 6},
            "program": [
                {"op": "MULT", "args": [2, 0, 1]},
                {"op": "PRINT", "args": [2]},
                {"op": "HALT"},
            ],
        },
        factory=_make_harvard_mark_i,
        tags=["IBM", "decimal", "Grace Hopper", "American"],
    ),
    MachineEntry(
        id="eniac",
        name="ENIAC",
        year=1945,
        country="USA",
        inventor="John Mauchly / J. Presper Eckert",
        category="stored_program",
        brief="First general-purpose electronic computer; 18,000 vacuum tubes, 10-digit decimal.",
        program_input_type="instructions_json",
        manual=_load_manual(_BASE + "eniac"),
        example_payload={
            "accumulators": {0: 5, 1: 3},
            "program": [
                {"op": "ADD", "src": 1, "dest": 0},
                {"op": "PRINT", "acc": 0},
                {"op": "HALT"},
            ],
        },
        factory=_make_eniac,
        tags=["vacuum tube", "electronic", "American", "first electronic"],
    ),
    MachineEntry(
        id="manchester-baby",
        name="Manchester Baby (SSEM)",
        year=1948,
        country="United Kingdom",
        inventor="Freddie Williams / Tom Kilburn",
        category="stored_program",
        brief="World's first stored-program electronic computer; Williams tube CRT store.",
        program_input_type="words",
        manual=_load_manual(_BASE + "manchester_baby"),
        example_payload={
            "store": {0: 0b_0000_0000_0000_0000_0000_0000_0110_0000},
            "program_words": [
                {"address": 0, "value": -3},
                {"address": 5, "value": 0b110_00000},
            ],
        },
        factory=_make_manchester_baby,
        tags=["CRT store", "stored program", "British"],
    ),
    MachineEntry(
        id="edsac",
        name="EDSAC",
        year=1949,
        country="United Kingdom",
        inventor="Maurice Wilkes",
        category="stored_program",
        brief="Electronic Delay Storage Automatic Calculator; first practical stored-program.",
        program_input_type="words",
        manual=_load_manual(_BASE + "edsac"),
        example_payload={
            "instructions": [
                {"address": 1, "mnemonic": "A", "operand": 10},
                {"address": 2, "mnemonic": "A", "operand": 11},
                {"address": 3, "mnemonic": "T", "operand": 12},
                {"address": 4, "mnemonic": "Z", "operand": 0},
            ],
            "data": {10: 5, 11: 3},
        },
        factory=_make_edsac,
        tags=["mercury delay line", "Cambridge", "British"],
    ),
]

# Build a lookup dict by id for O(1) access.
MACHINE_BY_ID: dict[str, MachineEntry] = {m.id: m for m in MACHINES}


def get_machine(machine_id: str) -> MachineEntry | None:
    """Return MachineEntry by slug, or None if not found."""
    return MACHINE_BY_ID.get(machine_id)


def list_machines(category: str | None = None) -> list[MachineEntry]:
    """Return all machines, optionally filtered by category."""
    if category is None:
        return list(MACHINES)
    return [m for m in MACHINES if m.category == category]


def build_machine(machine_id: str) -> tuple[Any, MachineAdapter] | None:
    """Construct a fresh (machine, adapter) pair for the given machine ID.

    Returns None if the ID is unknown.
    """
    entry = MACHINE_BY_ID.get(machine_id)
    if entry is None:
        return None
    return entry.factory()
