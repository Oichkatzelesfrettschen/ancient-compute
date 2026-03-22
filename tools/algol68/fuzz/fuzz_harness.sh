#!/bin/sh
# fuzz_harness.sh -- afl++ fuzzing harness for a68g --check
#
# WHY: a68g is a complex Algol 68 parser/interpreter with custom error
#      recovery paths. Fuzzing the --check (syntax analysis) path finds
#      crashes, hangs, and assertion failures in the parser that would
#      be exposed by malformed user-submitted programs.
#
# WHAT: Uses afl-fuzz (black-box mode, no instrumented binary) to mutate
#       ALGOL 68 source files from corpus/ and feed them to a68g --check.
#       Crashes and hangs are written to findings/crashes/ and findings/hangs/.
#
# HOW:
#   # One-shot run (30 seconds, for CI smoke test):
#   ./fuzz_harness.sh --duration 30
#
#   # Full session (run indefinitely until Ctrl-C):
#   ./fuzz_harness.sh
#
#   # With valgrind for memory safety (much slower):
#   ./fuzz_harness.sh --valgrind
#
# Prerequisites: afl-fuzz (/usr/bin/afl-fuzz), a68g (/usr/bin/a68g)
# Output: tools/algol68/fuzz/findings/ (crashes/, hangs/)
#
# NOTE: afl-fuzz requires:
#   echo core > /proc/sys/kernel/core_pattern   (or AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1)
#   echo performance > /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor (or AFL_SKIP_CPUFREQ=1)
# These are set via environment variables below to avoid needing root.

set -eu

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CORPUS_DIR="${SCRIPT_DIR}/corpus"
FINDINGS_DIR="${SCRIPT_DIR}/findings"
A68G="/usr/bin/a68g"
AFL_FUZZ="/usr/bin/afl-fuzz"

# Defaults
DURATION=""
USE_VALGRIND=0
VALGRIND="/usr/bin/valgrind"

usage() {
  echo "Usage: $0 [--duration SECONDS] [--valgrind]"
  echo ""
  echo "  --duration N   Stop after N seconds (default: run until Ctrl-C)"
  echo "  --valgrind     Wrap a68g in valgrind for memory safety detection"
  exit 1
}

while [ "$#" -gt 0 ]; do
  case "$1" in
    --duration) DURATION="$2"; shift 2 ;;
    --valgrind) USE_VALGRIND=1; shift ;;
    --help|-h) usage ;;
    *) echo "Unknown option: $1"; usage ;;
  esac
done

# Validate prerequisites
if [ ! -x "${A68G}" ]; then
  echo "ERROR: a68g not found at ${A68G}" >&2
  exit 1
fi
if [ ! -x "${AFL_FUZZ}" ]; then
  echo "ERROR: afl-fuzz not found at ${AFL_FUZZ} (install: paru -S afl++)" >&2
  exit 1
fi
if [ ! -d "${CORPUS_DIR}" ]; then
  echo "ERROR: corpus directory missing: ${CORPUS_DIR}" >&2
  exit 1
fi
corpus_count=$(find "${CORPUS_DIR}" -maxdepth 1 -name '*.a68' | wc -l || true)
if [ "${corpus_count}" -eq 0 ]; then
  echo "ERROR: no .a68 seed files in ${CORPUS_DIR}" >&2
  exit 1
fi

mkdir -p "${FINDINGS_DIR}"

# Build the afl-fuzz command
# AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES: suppress /proc/sys/kernel/core_pattern warning
# AFL_SKIP_CPUFREQ: suppress CPU frequency governor warning
export AFL_I_DONT_CARE_ABOUT_MISSING_CRASHES=1
export AFL_SKIP_CPUFREQ=1

# a68g --check reads from a file; afl-fuzz substitutes @@ with the temp file path.
if [ "${USE_VALGRIND}" -eq 1 ]; then
  if [ ! -x "${VALGRIND}" ]; then
    echo "ERROR: valgrind not found at ${VALGRIND}" >&2
    exit 1
  fi
  TARGET_CMD="${VALGRIND} --error-exitcode=42 --quiet ${A68G} --check @@"
else
  TARGET_CMD="${A68G} --check @@"
fi

AFL_ARGS="-i ${CORPUS_DIR} -o ${FINDINGS_DIR}"
if [ -n "${DURATION}" ]; then
  AFL_ARGS="${AFL_ARGS} -V ${DURATION}"
fi

echo "=== a68g --check afl++ fuzzing session ==="
echo "Corpus:  ${CORPUS_DIR} (${corpus_count} seeds)"
echo "Output:  ${FINDINGS_DIR}"
if [ -n "${DURATION}" ]; then
  echo "Duration: ${DURATION}s"
else
  echo "Duration: unlimited (Ctrl-C to stop)"
fi
if [ "${USE_VALGRIND}" -eq 1 ]; then
  echo "Valgrind: enabled (memory safety mode)"
fi
echo ""

# shellcheck disable=SC2086
"${AFL_FUZZ}" ${AFL_ARGS} -- ${TARGET_CMD}

echo ""
echo "=== Fuzzing session complete ==="
crashes=$(find "${FINDINGS_DIR}/crashes" -name 'id:*' 2>/dev/null | wc -l || true)
hangs=$(find "${FINDINGS_DIR}/hangs" -name 'id:*' 2>/dev/null | wc -l || true)
echo "Crashes: ${crashes}"
echo "Hangs:   ${hangs}"
echo ""
if [ -d "${FINDINGS_DIR}/crashes" ]; then
  first_crash=$(find "${FINDINGS_DIR}/crashes" -name 'id:*' 2>/dev/null | head -1 || true)
  if [ -n "${first_crash}" ]; then
    echo "CRASHES found! To reproduce:"
    echo "  ${A68G} --check ${first_crash}"
    echo "  valgrind ${A68G} --check ${first_crash}"
  fi
fi
