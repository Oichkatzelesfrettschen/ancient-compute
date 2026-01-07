#!/bin/bash
set -e
echo "=== ATTEMPTING BROMLEY PLAN 28 DOWNLOADS ==="
echo ""

# Try IEEE Xplore direct link
echo "1. Attempting IEEE Xplore direct PDF..."
if curl -L --fail --retry 3 --connect-timeout 15 --max-time 60 \
    -H "User-Agent: Mozilla/5.0" \
    -o "37_Bromley_Plans_28_28a_Programmer_Interface.pdf" \
    "https://ieeexplore.ieee.org/iel5/85/19196/00887986.pdf" 2>&1 | tail -5; then
    if [ -f "37_Bromley_Plans_28_28a_Programmer_Interface.pdf" ] && [ -s "37_Bromley_Plans_28_28a_Programmer_Interface.pdf" ]; then
        echo "SUCCESS: IEEE Xplore download"
        ls -lh "37_Bromley_Plans_28_28a_Programmer_Interface.pdf"
    else
        echo "FAILED: File empty or too small"
        rm -f "37_Bromley_Plans_28_28a_Programmer_Interface.pdf"
    fi
else
    echo "FAILED: IEEE Xplore direct access blocked"
fi

echo ""
echo "2. Attempting Semantic Scholar mirror..."
if curl -L --fail --retry 3 --connect-timeout 15 --max-time 60 \
    -H "User-Agent: Mozilla/5.0" \
    -o "37_Bromley_Plans_28_28a_SemanticScholar.pdf" \
    "https://www.semanticscholar.org/paper/Babbage's-Analytical-Engine-Plans-28-and-28a-The-Bromley/151b3c7aeffbaf0c3a642cfc601ebc0cb5871f90" 2>&1 | tail -5; then
    if [ -f "37_Bromley_Plans_28_28a_SemanticScholar.pdf" ] && [ -s "37_Bromley_Plans_28_28a_SemanticScholar.pdf" ]; then
        echo "SUCCESS: Semantic Scholar"
        ls -lh "37_Bromley_Plans_28_28a_SemanticScholar.pdf"
    else
        echo "NOTE: Semantic Scholar page retrieved but PDF extraction may be needed"
    fi
else
    echo "FAILED: Semantic Scholar access"
fi

echo ""
echo "3. Attempting CiteSeerX version..."
if curl -L --fail --retry 3 --connect-timeout 15 --max-time 60 \
    -o "37_Bromley_Plans_28_28a_CiteSeerX.pdf" \
    "https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=f3d4f4fb553ebb59ac0a3e48cd16f8f61f3fc983" 2>&1 | tail -5; then
    if [ -f "37_Bromley_Plans_28_28a_CiteSeerX.pdf" ] && [ -s "37_Bromley_Plans_28_28a_CiteSeerX.pdf" ]; then
        echo "SUCCESS: CiteSeerX download"
        ls -lh "37_Bromley_Plans_28_28a_CiteSeerX.pdf"
    else
        echo "FAILED: File empty or incorrect"
        rm -f "37_Bromley_Plans_28_28a_CiteSeerX.pdf"
    fi
else
    echo "FAILED: CiteSeerX access"
fi

echo ""
echo "=== STATUS ===" 
ls -1 37_Bromley* 2>/dev/null && echo "Bromley papers retrieved" || echo "No Bromley papers yet - ResearchGate/institutional access required"
