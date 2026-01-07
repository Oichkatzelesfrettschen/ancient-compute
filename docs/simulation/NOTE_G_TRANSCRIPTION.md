# Note G Transcription (Work in progress)

Sources (local cache)
- docs/sources/cache/05_Menabrea_Sketch_Analytical_Engine_1843.pdf
- docs/sources/cache/Fourmilab_Sketch_Analytical_Engine.html
- docs/sources/cache/Fourmilab_notegt1.png
- docs/sources/cache/Fourmilab_notegt2.png
- docs/sources/cache/Fourmilab_t6clip.png
- docs/sources/cache/07_Robins_Ada_and_First_Computer.pdf

Cycle notation (from Fourmilab notegt1/notegt2 images)

From notegt1:
```
(1...7),(24,25)                    gives B1  = 1st number; (n being = 1)
(1...7),(8...12),(24,25)           gives B3  = 2nd ......; (n ..... = 2)
(1...7),(8...12),(13...23),(24,25) gives B5  = 3rd ......; (n ..... = 3)
(1...7),(8...12),2(13...23),(24,25) gives B7 = 4th ......; (n ..... = 4)
(1...7),(8...12),Sum(+1)^(n-2)(13...23),(24,25) gives B2n-1 = nth; (n = n)
```

From notegt2:
```
(1...7),(24,25), Sum(+1)^n { (1...7),(8...12), Sum(n+2)(13...23),(24,25) }
limits 1 to n                                     limits 0 to (n + 2)
```

Series definition (from Fourmilab HTML text)
- 1st Series: let n = 1, calculate (8). Result is B1.
- 2nd Series: let n = 2, substitute B1. Result is B3.
- 3rd Series: let n = 3, substitute B1, B3. Result is B5.
- Continue to any extent.

Table A.2 / diagram sources
- Menabrea PDF page 55 contains Table A.2 (raster).
- Robins PDF page 4 contains a high-contrast reprint (raster).

Open gaps
- Table A.2 row-by-row transcription still missing.
- T6 clip (Fourmilab_t6clip.png) is not OCRable; manual transcription needed.
