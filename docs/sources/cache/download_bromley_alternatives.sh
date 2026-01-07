#!/bin/bash
set -e
echo "=== BROMLEY PLAN 28 - ALTERNATIVE ACCESS ROUTES ==="
echo ""

# 1. Try ResearchGate direct PDF download endpoint
echo "1. Attempting ResearchGate PDF endpoint..."
if curl -L --fail --retry 2 --connect-timeout 15 --max-time 60 \
    -H "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36" \
    -o "37_Bromley_ResearchGate_Direct.pdf" \
    "https://www.researchgate.net/publication/3330758_Babbage's_Analytical_Engine_Plans_28_and_28a_-_The_Programmer's_Interface/download" 2>&1 | tail -3; then
    if [ -f "37_Bromley_ResearchGate_Direct.pdf" ] && [ -s "37_Bromley_ResearchGate_Direct.pdf" ]; then
        echo "✓ ResearchGate download successful"
        ls -lh "37_Bromley_ResearchGate_Direct.pdf"
    fi
else
    echo "ResearchGate direct download (may require account)"
    rm -f "37_Bromley_ResearchGate_Direct.pdf"
fi

# 2. Check IEEE Computer Society Digital Library (alternative IEEE endpoint)
echo ""
echo "2. Attempting IEEE Computer Society archive..."
if curl -L --fail --retry 2 --connect-timeout 15 --max-time 60 \
    -o "37_Bromley_IEEE_CSDL.pdf" \
    "https://www.computer.org/csdl/magazine/an/2000/04/man2000040005" 2>&1 | tail -3; then
    if [ -f "37_Bromley_IEEE_CSDL.pdf" ] && grep -q "PDF\|pdf" "37_Bromley_IEEE_CSDL.pdf" 2>/dev/null; then
        echo "✓ IEEE CSDL page retrieved (may be HTML)"
        ls -lh "37_Bromley_IEEE_CSDL.pdf"
    fi
else
    echo "IEEE CSDL not directly accessible"
    rm -f "37_Bromley_IEEE_CSDL.pdf"
fi

# 3. Try Scholars Portal (Canadian open access gateway)
echo ""
echo "3. Attempting Scholars Portal..."
if curl -L --fail --retry 2 --connect-timeout 15 --max-time 60 \
    -o "37_Bromley_ScholarsPortal.pdf" \
    "https://journals.scholarsportal.info/details/10586180/v22i0004/5_baep2a2tpi.xml" 2>&1 | tail -3; then
    if [ -f "37_Bromley_ScholarsPortal.pdf" ] && [ -s "37_Bromley_ScholarsPortal.pdf" ]; then
        echo "✓ Scholars Portal retrieved"
        ls -lh "37_Bromley_ScholarsPortal.pdf"
    fi
else
    echo "Scholars Portal access blocked"
    rm -f "37_Bromley_ScholarsPortal.pdf"
fi

echo ""
echo "=== DOWNLOAD ATTEMPTS COMPLETE ==="
ls -1 37_Bromley* 2>/dev/null | while read f; do
    echo "$f: $(stat -f%z "$f" 2>/dev/null || stat -c%s "$f" 2>/dev/null || echo 'unknown size') bytes"
done || echo "No downloads succeeded"
