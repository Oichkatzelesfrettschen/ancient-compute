#!/bin/bash
# Ancient Compute - C Compilation Script

set -e

# Compile C code with strict warnings
gcc -Wall -Werror -O2 -o /tmp/program main.c

# Execute program with input
/tmp/program < input.txt
