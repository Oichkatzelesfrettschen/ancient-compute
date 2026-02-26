# Source-to-Requirement Traceability Map

**Version**: 0.1
**Date**: 2026-02-26
**Purpose**: Every engineering claim in this project traces to a source ID
defined here, or is explicitly labeled ASSUMPTION.

---

## Source Registry

Each source has a stable ID used in code comments (`# SOURCE:<ID>`) and
spec documents.

### Primary Sources

| ID | Short Title | Full Citation | URL |
|----|------------|---------------|-----|
| SMG-DE2-TECH | DE2 Technical Description | Science Museum Group, Difference Engine No. 2 Technical Description (PDF) | https://www.sciencemuseum.org.uk/sites/default/files/2023-09/DE2_Technical_Description.pdf |
| SMG-DE2-MANUAL | DE2 User Manual | Science Museum Group, Difference Engine User Manual v1.1 (PDF) | https://www.sciencemuseum.org.uk/sites/default/files/2023-08/User_Manual_PDF_Vsn_1.1_Final_SEC1.pdf |
| SMG-DRAWINGS | DE Technical Drawings | Science Museum Group, Technical Drawings Collection | https://collection.sciencemuseumgroup.org.uk/objects/co75079/technical-drawings |
| SMG-PAPERS | Babbage Papers | Science Museum Group, The Babbage Papers | https://collection.sciencemuseumgroup.org.uk/documents/aa110000003/the-babbage-papers |
| OXFORD-BABBAGE | Babbage Archive | Oxford History of Science Museum, The Babbage Archive | https://mhs.web.ox.ac.uk/the-babbage-archive |
| MENABREA-1842 | Sketch of the AE | Menabrea, L.F. (1842). "Sketch of the Analytical Engine." Trans. with notes by Ada Lovelace, Scientific Memoirs, 1843. | https://books.google.com/books?id=hPRmnQEACAAJ |
| LOVELACE-NOTES | Lovelace Notes (Fourmilab) | Fourmilab cache of Notes A through G | https://www.fourmilab.ch/babbage/sketch.html |
| LEIBNIZ-1703 | Binary Arithmetic | Leibniz, G.W. (1703). "Explication de l'Arithmetique Binaire." | https://gallica.bnf.fr/ark:/12148/bpt6k99776x/f121.image |

### Secondary Sources

| ID | Short Title | Full Citation |
|----|------------|---------------|
| SWADE-2001 | The Difference Engine | Swade, Doron (2001). "The Difference Engine: Charles Babbage and the Quest to Build the First Computer." Penguin. |
| SWADE-2000 | The Cogwheel Brain | Swade, Doron (2000). "The Cogwheel Brain." Little, Brown. |
| BROMLEY-1982 | AE 1838 | Bromley, Allan (1982). "Charles Babbage's Analytical Engine, 1838." Annals of the History of Computing 4(3):196-217. |
| BROMLEY-1990 | DE and AE | Bromley, Allan (1990). "Difference and Analytical Engines." In Aspray (ed.), Computing Before Computers. |
| GLASCHICK-2016 | Bernoulli Calc | Glaschick, R. (2016). "Ada Lovelace's Calculation of Bernoulli's Numbers." arXiv:1609.00091. |
| FUEGI-2003 | Lovelace & Babbage | Fuegi, J. & Francis, J. (2003). "Lovelace & Babbage and the Creation of the 1843 'Notes'." IEEE Annals 25(4):16-26. |
| MARTIN-2015 | Making of a CS | Martin, Rice, & Hollings (2015). "Ada Lovelace: The Making of a Computer Scientist." Bodleian Library. |

### Engineering References

| ID | Short Title | Full Citation |
|----|------------|---------------|
| HAMROCK-2004 | Fluid Film Lubrication | Hamrock, B.J., Schmid, S.R., & Jacobson, B.O. (2004). "Fundamentals of Fluid Film Lubrication." 2nd ed. Marcel Dekker. |
| ARCHARD-1953 | Wear Equation | Archard, J.F. (1953). "Contact and Rubbing of Flat Surfaces." J. Appl. Phys. 24:981-988. |
| SHIGLEY-2001 | Mech. Eng. Design | Shigley, J.E. & Mischke, C.R. (2001). "Mechanical Engineering Design." 6th ed. McGraw-Hill. |
| PETERSON-1974 | Stress Concentration | Peterson, R.E. (1974). "Stress Concentration Factors." Wiley. |
| TIMOSHENKO-1956 | Strength of Materials | Timoshenko, S.P. (1956). "Strength of Materials." Van Nostrand. |
| INCROPERA-2002 | Heat Transfer | Incropera, F.P. & DeWitt, D.P. (2002). "Fundamentals of Heat and Mass Transfer." 5th ed. Wiley. |
| CAMERON-1966 | Lubrication | Cameron, A. (1966). "The Principles of Lubrication." Longmans. |

### Historical Computing

| ID | Short Title | Full Citation |
|----|------------|---------------|
| ROBSON-2001 | Plimpton 322 | Robson, E. (2001). "Neither Sherlock Holmes nor Babylon." Historia Mathematica 28(3):167-206. |
| MANSFIELD-2017 | Plimpton Trig | Mansfield, D.F. & Wildberger, N.J. (2017). "Plimpton 322 is Babylonian exact sexagesimal trigonometry." |

---

## Requirement-to-Source Map

### Mechanical Design

| Requirement | Source | Notes |
|-------------|--------|-------|
| DE2 has 8 columns of 31 digits each | SMG-DE2-TECH | Section 2, "Machine Description" |
| Anticipating carriage looks ahead 2 positions | BROMLEY-1990 | Section on carry propagation |
| Figure wheels rotate 0-9 in decimal | SMG-DE2-TECH | -- |
| 360-degree cycle with 12 phase intervals | SMG-DE2-TECH | Timing appendix |
| Card format derived from Jacquard loom | MENABREA-1842, BROMLEY-1982 | Three card classes: operation, variable, number |
| Barrel micro-programs sequence mill operations | BROMLEY-1982 | Section 4, "The Mill" |
| Printer uses type wheels struck by hammers | SMG-DE2-MANUAL | Printer section |

### Physics Model

| Requirement | Source | Notes |
|-------------|--------|-------|
| Archard wear coefficient K = 1e-4 (dry brass) | ARCHARD-1953 | Table 2 |
| Hamrock-Dowson EHL film thickness | HAMROCK-2004 | Chapter 12, Eq. 12.7 |
| AGMA dynamic factor K_v with sqrt(V) | SHIGLEY-2001 | Eq. 14-27 |
| Peterson K_t power-law fit for stepped shafts | PETERSON-1974 | Chart 3.1 |
| Johnson-Euler buckling transition | SHIGLEY-2001 | Eq. 4-43 through 4-45 |
| Crank-Nicolson thermal solver | INCROPERA-2002 | Chapter 5, "Transient Conduction" |

### Provisional Values (ASSUMPTION)

| Parameter | Current Value | Status | Notes |
|-----------|--------------|--------|-------|
| Valve timing: lap percentage | 0.15 | ASSUMPTION | No primary-source measurement from physical DE2 |
| Valve timing: lead percentage | 0.05 | ASSUMPTION | No primary-source measurement from physical DE2 |
| Valve timing: cutoff percentage | 0.75 | ASSUMPTION | No primary-source measurement from physical DE2 |
| Steam pressure vs RPM mapping | Linear model | ASSUMPTION | Simplified; real governor is non-linear |
| 45-degree phase intervals in TimingController | 45 deg | ASSUMPTION | Approximation of SMG timing appendix |
| Manufacturing tolerance (3-sigma) | 0.05 mm | SOURCE:SWADE-2001 | "achievable precision ~0.002 inches" |
| Card dimensions: 129x56mm | 129x56mm | ASSUMPTION | Based on historical Jacquard proportions, not measured |

---

## Usage in Code

Tag engineering values in code with:

```python
# SOURCE:SHIGLEY-2001 Eq.14-27 -- AGMA dynamic factor
K_v = (A + math.sqrt(V)) / A

# ASSUMPTION: No primary measurement available; value from Swade estimate
valve_lap = 0.15
```

Tag spec documents with:

```markdown
The anticipating carriage evaluates carry at positions n and n+1
simultaneously. (SOURCE:BROMLEY-1990)
```
