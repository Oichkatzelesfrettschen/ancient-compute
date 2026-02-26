# Primary and Secondary Sources

**Date**: 2026-02-26
**Purpose**: Consolidated bibliography for all historical and technical claims.

---

## Babbage Analytical Engine / Difference Engine

### Primary Sources
- Science Museum Group: The Babbage Papers
  https://collection.sciencemuseumgroup.org.uk/documents/aa110000003/the-babbage-papers
- Science Museum: Difference Engine No.2 Technical Description (PDF)
  https://www.sciencemuseum.org.uk/sites/default/files/2023-09/DE2_Technical_Description.pdf
- Science Museum: Difference Engine User Manual (PDF)
  https://www.sciencemuseum.org.uk/sites/default/files/2023-08/User_Manual_PDF_Vsn_1.1_Final_SEC1.pdf
- Science Museum Group: Technical drawings (Difference Engine)
  https://collection.sciencemuseumgroup.org.uk/objects/co75079/technical-drawings
- Oxford History of Science Museum: The Babbage Archive
  https://mhs.web.ox.ac.uk/the-babbage-archive

### Secondary Sources
- Swade, Doron (2001). "The Difference Engine: Charles Babbage and the Quest
  to Build the First Computer." Penguin Books.
- Swade, Doron (2000). "The Cogwheel Brain." Little, Brown.
- Bromley, Allan (1982). "Charles Babbage's Analytical Engine, 1838."
  Annals of the History of Computing 4(3):196-217.
- Bromley, Allan (1990). "Difference and Analytical Engines."
  In Aspray (ed.), Computing Before Computers.

## Ada Lovelace / Note G

### Primary Sources
- Menabrea, L.F. (1842). "Sketch of the Analytical Engine Invented by
  Charles Babbage, Esq." Trans. with notes by Augusta Ada King, Countess
  of Lovelace. Scientific Memoirs, 1843.
  https://books.google.com/books?id=hPRmnQEACAAJ
- Fourmilab cache of Lovelace's Notes (A through G):
  https://www.fourmilab.ch/babbage/sketch.html

### Secondary Sources
- Glaschick, R. (2016). "Ada Lovelace's Calculation of Bernoulli's Numbers."
  arXiv:1609.00091.
- Two-Bit History (2018). "What Did Ada Lovelace's Program Actually Do?"
  https://twobithistory.org/2018/08/18/ada-lovelace-note-g.html
- Fuegi, J. & Francis, J. (2003). "Lovelace & Babbage and the Creation
  of the 1843 'Notes'." IEEE Annals of the History of Computing 25(4):16-26.
  https://www.jstor.org/stable/25171333
- Martin, Rice, & Hollings (2015). "Ada Lovelace: The Making of a Computer
  Scientist." Bodleian Library.
- Yale TAP Project -- Ada Lovelace Notes
  https://www.cs.yale.edu/homes/tap/Files/ada-lovelace-notes.html

## Leibniz / Binary Arithmetic

- Leibniz, G.W. (1703). "Explication de l'Arithmetique Binaire."
  Memoires de l'Academie Royale des Sciences.
  https://gallica.bnf.fr/ark:/12148/bpt6k99776x/f121.image

## Mechanical Engineering References (Physics Model)

### Tribology and Lubrication
- Hamrock, B.J., Schmid, S.R., & Jacobson, B.O. (2004). "Fundamentals of
  Fluid Film Lubrication." 2nd ed. Marcel Dekker.
- Archard, J.F. (1953). "Contact and Rubbing of Flat Surfaces." Journal of
  Applied Physics 24:981-988. [Archard wear equation]

### Structural and Machine Design
- Shigley, J.E. & Mischke, C.R. (2001). "Mechanical Engineering Design."
  6th ed. McGraw-Hill. [AGMA K_v, Johnson-Euler buckling, shaft analysis]
- Peterson, R.E. (1974). "Stress Concentration Factors." Wiley.
  [Notch sensitivity, K_t power-law fits]
- Timoshenko, S.P. (1956). "Strength of Materials." Van Nostrand.
  [Critical speed, deflection analysis]

### Thermodynamics
- Incropera, F.P. & DeWitt, D.P. (2002). "Fundamentals of Heat and Mass
  Transfer." 5th ed. Wiley. [Convection, radiation, Crank-Nicolson]
- Cameron, A. (1966). "The Principles of Lubrication." Longmans.
  [Oil viscosity temperature dependence]

### Historical Manufacturing Precision
- Swade, Doron. Various publications on Babbage-era manufacturing tolerances.
  Achievable precision: ~0.002 inches (0.05 mm) at 3-sigma.

## Ancient Computing Devices

### Plimpton 322 (Babylonian)
- Robson, E. (2001). "Neither Sherlock Holmes nor Babylon: A Reassessment
  of Plimpton 322." Historia Mathematica 28(3):167-206.
- Mansfield, D.F. & Wildberger, N.J. (2017). "Plimpton 322 is Babylonian
  exact sexagesimal trigonometry." Historia Mathematica 44(4):395-419.
- Note: Interpretation disputed (Robson vs right-triangle tables).

### I Ching
- Note: Binary linkage is symbolic, not computational. See errata.md.

## Provisional / Unverified Values

The following simulation parameters are provisional (not sourced from
primary measurement of physical DE2 hardware):

- Valve timing: lap, lead, cutoff percentages (see TIMING_PROVISIONAL.yaml)
- Steam pressure vs RPM mapping
- 45-degree phase intervals in TimingController

See `docs/babbage_engine/VALVE_TIMING_ANALYSIS.md` for provenance status
of each timing parameter.
