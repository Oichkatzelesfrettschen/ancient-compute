"""Pre-computed ASCII art frame library for mechanical animations.

Each FRAMES entry maps an animation name to a list of text frames.
Timer-driven at 8 FPS by the BabbageApp.
"""

# Rotating gear -- 8 frames (45-degree steps)
_GEAR_FRAMES = [
    r"""   ___
  / | \
 | ADD |
  \_|_/""",
    r"""   _|_
  / - \
 | ADD |
  \___/""",
    r"""   ___
  \ | /
 | ADD |
  /___\ """,
    r"""   _|_
  \ - /
 | ADD |
  /   \ """,
]

# Barrel stud pattern -- 4 frames showing rotation
_BARREL_FRAMES = [
    "[|][.][|][.][|]\n[.][|][.][|][.]\n[|][.][|][.][|]",
    "[.][|][.][|][.]\n[|][.][|][.][|]\n[.][|][.][|][.]",
    "[|][.][|][.][|]\n[.][|][.][|][.]\n[|][.][|][.][|]",
    "[.][|][.][|][.]\n[|][.][|][.][|]\n[.][|][.][|][.]",
]

# Carry propagation sweep (right to left)
_CARRY_FRAMES = [
    "[ ][ ][ ][ ][C]",
    "[ ][ ][ ][C][ ]",
    "[ ][ ][C][ ][ ]",
    "[ ][C][ ][ ][ ]",
    "[C][ ][ ][ ][ ]",
    "[ ][ ][ ][ ][ ]",
]

# Mill operation -- ADD/SUB
_MILL_ADD_FRAMES = [
    "Ingress: {ingress}\n  +\nEgress:  ---",
    "Ingress: {ingress}\n  +\nEgress:  ...",
    "Ingress: {ingress}\n  +\nEgress:  {egress}",
    "Ingress: ---\n  +\nEgress:  {egress}",
]

FRAMES = {
    "gear": _GEAR_FRAMES,
    "barrel": _BARREL_FRAMES,
    "carry": _CARRY_FRAMES,
    "mill_add": _MILL_ADD_FRAMES,
}
