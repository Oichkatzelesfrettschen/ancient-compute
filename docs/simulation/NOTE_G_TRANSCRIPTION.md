# Note G Transcription (Work in progress)

Sources (local cache)
- docs/sources/cache/05_Menabrea_Sketch_Analytical_Engine_1843.pdf
- docs/sources/cache/Fourmilab_Sketch_Analytical_Engine.html
- docs/sources/cache/Fourmilab_notegt1.png
- docs/sources/cache/Fourmilab_notegt2.png
- docs/sources/cache/Fourmilab_t6clip.png
- docs/sources/cache/07_Robins_Ada_and_First_Computer.pdf
Canonical deck spec
- docs/simulation/NOTE_G_DECK.yaml

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
- Fourmilab provides a higher-resolution diagram (menat6_1-5k.png, menat6_2k.png) and PostScript (menat6ps.zip).

Known errata (from Robins / secondary notes)
- Operation 21 denominator should be 4, not 3 (Ada's program note).
- Variable tracking: the deck does not carry an explicit iteration counter; V10 is reused.

Open gaps
- Table A.2 transcription captured; next step is executable deck runner.
- T6 clip (Fourmilab_t6clip.png) is not OCRable; manual transcription needed.

Working transcription: Table A.2 (rows 1-25)
Source: docs/sources/cache/Fourmilab_menat6_2k.png
Columns: Op#, Op, Variables acted upon, Variables receiving results, Statement of results

1  | x | 1V2 x 1V3       | 1V4, 1V5, 1V6 | = 2n
2  | - | 1V4 - 1V1       | 2V4          | = 2n - 1
3  | + | 1V5 + 1V1       | 2V5          | = 2n + 1
4  | ÷ | 2V5 ÷ 2V4       | 1V11         | = (2n - 1) / (2n + 1)
5  | ÷ | 1V11 ÷ 1V2      | 2V11         | = (1/2) * ((2n - 1) / (2n + 1))
6  | - | 0V13 - 2V11     | 1V13         | = -(1/2) * ((2n - 1) / (2n + 1)) = A0
7  | - | 1V3 - 1V1       | 1V10         | = n - 1 (= 3)
8  | + | 1V2 + 0V7       | 1V7          | = 2 + 0 = 2
9  | ÷ | 1V6 ÷ 1V7       | 3V11         | = (2n / 2) = A1
10 | x | 1V21 x 3V11     | 1V12         | = B1 * (2n/2) = B1 A1
11 | + | 1V12 + 1V13     | 2V13         | = -(1/2)*((2n-1)/(2n+1)) + B1*(2n/2)
12 | - | 1V10 - 1V1      | 2V10         | = n - 2 (= 2)
13 | - | 1V6 - 1V1       | 2V6          | = 2n - 1
14 | + | 1V1 + 1V7       | 2V7          | = 2 + 1 = 3
15 | ÷ | 2V6 ÷ 2V7       | 1V8          | = (2n - 1) / 3
16 | x | 1V8 x 3V11      | 4V11         | = (2n/2) * ((2n - 1)/3)
17 | - | 2V6 - 1V1       | 3V6          | = 2n - 2
18 | + | 1V1 + 2V7       | 3V7          | = 3 + 1 = 4
19 | ÷ | 3V6 ÷ 3V7       | 1V9          | = (2n - 2) / 4
20 | x | 1V9 x 4V11      | 5V11         | = (2n/2) * ((2n - 1)/3) * ((2n - 2)/4) = A3
21 | x | 1V22 x 5V11     | 0V12         | = B3 * (2n/2) * ((2n - 1)/3) * ((2n - 2)/4) = B3 A3
22 | + | 2V12 + 2V13     | 3V13         | = A0 + B1 A1 + B3 A3
23 | - | 2V10 - 1V1      | 3V10         | = n - 3 (= 1)
24 | + | 4V13 + 0V24     | 1V24         | = B7
25 | + | 1V1 + 1V3       | 1V3          | = n + 1 = 4 + 1 = 5; by a Variable-card
    |   |               |              | reset indices: 1V1=1V1, 1V3=1V3, 5V6=0V6, 5V7=0V7
