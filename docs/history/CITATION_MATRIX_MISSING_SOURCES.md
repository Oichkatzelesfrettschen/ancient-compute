# Citation Matrix: Missing Primary Sources and Access Gaps

Purpose: track missing primary materials for ancient mechanisms and record access status.
Status legend: local = cached in repo, online = reachable via HTTP, blocked = JS/login, paywalled = subscription, missing = no lead yet.

-------------------------------------------------------------------------------

Ishango Bone (tally marks)
- Primary target: RBINS Collections site (Ishango project, 3D scans)
  Access: https://collections.naturalsciences.be/projects/ishango/ (public files). Status: local
  Download: https://collections.naturalsciences.be/projects/ishango/Ishango.zip/at_download/file
  Local: docs/sources/cache/Ishango.zip
- Primary target: RBINS main museum object record (museum artifact)
  Access: URL in docs is 404 (site redesign). Status: missing
- Primary target: Heinzelin 1962 Scientific American article
  Access: likely paywalled. Status: paywalled
- Secondary: Overmann (2025) Springer chapter; UNESCO archaeoastronomy study
  Access: mixed, not cached. Status: missing

Clay tokens / bullae (early accounting)
- Primary target: Schmandt-Besserat, Before Writing (1992); How Writing Came About (1996)
  Access: books via library. Status: paywalled
- Primary target: UT Austin archives finding aids
  Access: not yet linked. Status: missing

Plimpton 322 (Babylonian algorithms)
- Primary target: Columbia RBML artifact record for Plimpton 322
  Access: not yet linked. Status: missing
- Secondary: Neugebauer & Sachs (1945), Robson (2001/2002)
  Access: likely paywalled. Status: paywalled

Nine Chapters / Liu Hui (China)
- Primary target: Chinese Text Project edition (Nine Chapters)
  Access: https://ctext.org/nine-chapters. Status: local
  Local: docs/sources/cache/ctext_nine_chapters.html
- Primary target: Critical editions or scans of Nine Chapters + Liu Hui commentary
  Access: not yet linked. Status: missing
- Secondary: Shen, Crossley, Lun (OUP, 1999)
  Access: paywalled. Status: paywalled

Sunzi Suanjing (CRT origins)
- Primary target: Chinese Text Project edition (Sunzi Suan Jing)
  Access: https://ctext.org/sunzi-suan-jing. Status: local
  Local: docs/sources/cache/ctext_sunzi_suan_jing.html
- Secondary: Britannica/JSTOR summaries
  Access: mixed. Status: paywalled

Qin Jiushao / Shu Shu Jiu Zhang (1247)
- Primary target: Qin Jiushao text or scans
  Access: not yet linked. Status: missing

I Ching binary tradition
- Primary target: Chinese Text Project edition (Book of Changes / I Ching)
  Access: https://ctext.org/book-of-changes. Status: local
  Local: docs/sources/cache/ctext_book_of_changes.html
- Secondary: Needham citations
  Access: paywalled. Status: paywalled

Abacus / counting rods
- Primary target: historical manuals (suanpan/rod calculus)
  Access: not yet linked. Status: missing
- Secondary: Needham; Li & Du
  Access: paywalled. Status: paywalled

Antikythera mechanism
- Primary target: Antikythera Mechanism Research Project (AMRP) site
  Access: JS blocked via curl; r.jina.ai returns empty. Status: blocked
- Primary target: National Archaeological Museum object record
  Access: not yet linked. Status: missing
- Museum fallback: NAM temporary exhibition page
  Access: reachable at https://www.namuseum.gr/en/temporary_exhibition/to-nayagio-ton-antikythiron-to-ploio-oi-thisayroi-o-michanismos/. Status: online
- Secondary: Freeth et al. Nature papers (2006/2008)
  Access: likely paywalled. Status: paywalled

Quipu / khipu
- Primary target: Harvard Khipu Database Project (khipudatabase.com)
  Access: DNS failed from environment. Status: blocked
- Primary target: Harvard Dataverse quipu datasets
  Access: API reachable but search results not artifact-focused. Status: partial
- Secondary: Urton (2003) Signs of the Inka Khipu
  Access: book via library. Status: paywalled

Pascaline (Pascal, 1645)
- Primary target: Pascal, Account of the Arithmetic Machine (1645)
  Access: not yet linked. Status: missing
- Museum target: Arts et Metiers Pascaline page
  Access: 404 at https://www.arts-et-metiers.net/musee/pascaline; search page shows no results via curl. Status: missing

Leibniz stepped reckoner
- Primary target: Leibniz Archive (Hanover) manuscript scans
  Access: not yet linked. Status: missing
- Primary target: Leibniz 1703 binary paper (Gallica)
  Access: Gallica triggers Altcha bot check and ARK link in docs returns HTTP 400. Status: blocked
- Secondary: Strickland/Lewis MIT Press 2022
  Access: paywalled. Status: paywalled

Jacquard loom (punch card control)
- Primary target: Jacquard patent or museum technical record
  Access: not yet linked. Status: missing
- Secondary: textile museum catalogs
  Access: not yet linked. Status: missing

Slide rule
- Primary target: Oughtred 1622 publication (slide rule origins)
  Access: not yet linked. Status: missing
- Secondary: museum collections (NMAH, Science Museum)
  Access: not yet linked. Status: missing

Astrolabe
- Primary target: historical astrolabe manuals and object records
  Access: not yet linked. Status: missing
- Secondary: museum collections (NMAH, British Museum)
  Access: not yet linked. Status: missing

Babbage Plan 28 / 28a (extensions)
- Primary target: Science Museum Babbage Papers (Plan 28/28a)
  Access: online catalog; digitized pages likely restricted. Status: partial
- Secondary: Bromley IEEE papers (Plan 28/28a)
  Access: likely paywalled; download scripts added in docs/sources/cache. Status: paywalled

Babbage/Ada core (already covered)
- Primary target: Menabrea + Lovelace Notes (1843)
  Access: cached locally in docs/sources/cache/05_Menabrea_Sketch_Analytical_Engine_1843.pdf. Status: local
- Secondary: Lovelace analyses (Robins, Stonybrook, Rod Smith)
  Access: cached locally in docs/sources/cache/*.pdf. Status: local

-------------------------------------------------------------------------------

Online access checks performed
- https://blog.plan28.org/ (reachable)
- https://www.computerhistory.org/babbage/ (reachable)
- https://www.computerhistory.org/babbage/modernsequel/ (reachable)
- https://ctext.org/nine-chapters (reachable)
- https://ctext.org/sunzi-suan-jing (reachable)
- https://ctext.org/book-of-changes (reachable)
- https://www.arts-et-metiers.net/musee/pascaline (404)
- https://dataverse.harvard.edu/api/search?q=khipu&per_page=5 (reachable, not artifact-focused)
- https://www.antikythera-mechanism.gr/ (HTTP 200 but no content via curl; JS required)
- https://www.namuseum.gr/en/?s=antikythera (reachable; search results)
- https://www.namuseum.gr/en/temporary_exhibition/to-nayagio-ton-antikythiron-to-ploio-oi-thisayroi-o-michanismos/ (reachable; exhibition page)

Next acquisition pass (recommended)
- Update Ishango museum link; capture artifact ID and page title
- Locate open scans for Nine Chapters, Sunzi, Qin Jiushao
- Locate museum object pages for Pascaline and Leibniz stepped reckoner
- Find stable Antikythera object page and AMRP PDF reports
- Identify real quipu datasets with DOIs (Harvard Dataverse or other)
- Repair Gallica ARK link for Leibniz 1703 paper
