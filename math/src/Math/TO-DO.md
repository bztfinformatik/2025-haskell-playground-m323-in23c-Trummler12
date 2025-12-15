# TO-DO after diving into Polytopes

I've asked Haskell GPT (5.2) what our next stept might be:
```text
Please again ultrathink hard on this:
So, a fully normalized formula for any polygon's projection of any dimension could be called "polytopical" with "polytopicNumber" and "politopischeZahl" as sugar;
BUT, we'd also need an extra argument for the number of faces a Vertex shares (for most, it's 3, but for the octahedron and orthoplexes (only full family with non-flat examples), it's 4, and the icosaedron (as far as I'm concerned the only case with non-flat examples) even has 5; So we need not forget to account for those cases.
So I'd suggest the following: Before we dive into the most generalized cases, we first build from the most non-generalized cases upwards since we want to add a name for every common series either way (with infinite-family-upper-bounds of 8).

So far, we've got the following:
```haskell
module Math.Series where

-- cube root helper
realCbrt :: (Ord a, Floating a) => a -> a
realCbrt x = signum x * abs x ** (1/3)

-- nd number
simplical d n = product [n .. n + max 0 d - 1] / product [1 .. max 0 d]; simplexNumber = simplical; simplexZahl = simplical
hypercubical d n = n ** fromIntegral d; hypercubeNumber = hypercubical; hypercubeZahl = hypercubical
-- orthoplecal d n = 

-- nd inverted number
simplicalRoot d x = (\y -> let r = fromIntegral (round y) in if abs (y - r) < 1e-6 then r else y) (let (l,h) = until (\(l,h)->h-l<1e-6) (\(l,h)->let m=(l+h)/2 in if simplical d m > x then (l,m) else (m,h)) (0, max 1 (x+1)) in (l+h)/2)
simplexRoot = simplicalRoot; simplexWurzel = simplicalRoot
hypercubicalRoot d x = (\y -> let r = fromIntegral (round y) in if abs (y - r) < 1e-6 then r else y) (x ** (1 / fromIntegral d))
hypercubeRoot = hypercubicalRoot; hypercubeWurzel = hypercubicalRoot
-- orthoplecalRoot d x =

-- 2d number
polygonal s n = n * ((s - 2) * n - (s - 4)) / 2
ngonal = polygonal; anygonal = polygonal

triangular = polygonal 3; dreieckszahl = triangular
square = polygonal 4; quadratzahl = square; viereckszahl = square
pentagonal = polygonal 5; pentagonalzahl = pentagonal; fünfeckszahl = pentagonal
hexagonal = polygonal 6; hexagonalzahl = hexagonal; sechseckszahl = hexagonal
heptagonal = polygonal 7; septagonal = heptagonal; heptagonalzahl = heptagonal; siebeneckszahl = heptagonal
octagonal = polygonal 8; octagonalzahl = octagonal; achteckszahl = octagonal

-- 2d number series
polygonals s = map (polygonal s) [1..]
ngonals = polygonals; anygonals = polygonals

naturals = [1..]; natürlicheZahlen = naturals
triangulars = polygonals 3; dreieckszahlen = triangulars
squares = polygonals 4; quadratzahlen = squares; viereckszahlen = squares
pentagonals = polygonals 5; pentagonalzahlen = pentagonals; fünfeckszahlen = pentagonals
hexagonals = polygonals 6; hexagonalzahlen = hexagonals; sechseckszahlen = hexagonals
heptagonals = polygonals 7; septagonals = heptagonals; heptagonalzahlen = heptagonals; siebeneckszahlen = heptagonals
octagonals = polygonals 8; octagonalzahlen = octagonals; achteckszahlen = octagonals

-- 2d inverted number
polygonalRoot s x = (sqrt (8 * (s - 2) * x + ((s - 4) ^ 2)) + (s - 4)) / (2 * (s - 2))
ngonalRoot = polygonalRoot; anygonalRoot = polygonalRoot

naturalRoot x = x; natürlicheWurzel = naturalRoot
triangularRoot = polygonalRoot 3; dreiecksWurzel = triangularRoot
squareRoot = polygonalRoot 4; quadratWurzel = squareRoot; vierecksWurzel = squareRoot
pentagonalRoot = polygonalRoot 5; pentagonalWurzel = pentagonalRoot; fünfecksWurzel = pentagonalRoot
hexagonalRoot = polygonalRoot 6; hexagonalWurzel = hexagonalRoot; sechsecksWurzel = hexagonalRoot
heptagonalRoot = polygonalRoot 7; septagonalWurzel = heptagonalRoot; heptagonalWurzel = heptagonalRoot; siebenecksWurzel = heptagonalRoot
octagonalRoot = polygonalRoot 8; octagonalWurzel = octagonalRoot; achtecksWurzel = octagonalRoot

-- 2d inverted number series
polygonalRoots s = map (polygonalRoot s) [1..]
ngonalRoots = polygonalRoots; anygonalRoots = polygonalRoots

naturalRoots = [1..]; natürlicheWurzeln = naturalRoots
triangularRoots = polygonalRoots 3; dreiecksWurzeln = triangularRoots
squareRoots = polygonalRoots 4; quadratWurzeln = squareRoots; vierecksWurzeln = squareRoots
pentagonalRoots = polygonalRoots 5; pentagonalWurzeln = pentagonalRoots; fünfecksWurzeln = pentagonalRoots
hexagonalRoots = polygonalRoots 6; hexagonalWurzeln = hexagonalRoots; sechsecksWurzeln = hexagonalRoots
heptagonalRoots = polygonalRoots 7; septagonalWurzeln = heptagonalRoots; heptagonalWurzeln = heptagonalRoots; siebenecksWurzeln = heptagonalRoots
octagonalRoots = polygonalRoots 8; octagonalWurzeln = octagonalRoots; achtecksWurzeln = octagonalRoots


-- 3d number
pyramidal s n = n * (n + 1) * ((s - 2) * n - (s - 5)) / 6
ngonalPyramidal = pyramidal; anygonalPyramidal = pyramidal; pyramidalzahl = pyramidal; pyramidenzahl = pyramidal
tetrahedral = pyramidal 3; triangularPyramidal = tetrahedral; tetraederzahl = tetrahedral; dreiecksPyramidalzahl = tetrahedral; dreiecksPyramidenZahl = tetrahedral
squarePyramidal = pyramidal 4; quadratPyramidalzahl = squarePyramidal; quadratPyramidenZahl = squarePyramidal; vierecksPyramidalzahl = squarePyramidal; vierecksPyramidenZahl = squarePyramidal
pentagonalPyramidal = pyramidal 5; fünfecksPyramidalzahl = pentagonalPyramidal; fünfecksPyramidenZahl = pentagonalPyramidal
hexagonalPyramidal = pyramidal 6; sechsecksPyramidalzahl = hexagonalPyramidal; sechsecksPyramidenZahl = hexagonalPyramidal
heptagonalPyramidal = pyramidal 7; septagonalPyramidal = heptagonalPyramidal; siebenecksPyramidalzahl = heptagonalPyramidal; siebenecksPyramidenZahl = heptagonalPyramidal
octagonalPyramidal = pyramidal 8; achtecksPyramidalzahl = octagonalPyramidal; achtecksPyramidenZahl = octagonalPyramidal
octahedral n = n * (2 * n * n + 1) / 3; oktaederzahl = octahedral
cubical n = n * n * n; cubicNumber = cubical; cubeNumber = cubical; kubikzahl = cubical; würfelzahl = cubical
icosahedral n = n * (5 * n * n - 5 * n + 2) / 2; ikosaederzahl = icosahedral
dodecahedral n = 3 * n * ((3 * n - 1) * (3 * n - 2)) / 6; dodekaederzahl = dodecahedral

-- 3d number series
pyramidals s = map (pyramidal s) [1..]
ngonalPyramidals = pyramidals; anygonalPyramidals = pyramidals; pyramidalzahlen = pyramidals; pyramidenzahlen = pyramidals
tetrahedrals = pyramidals 3; triangularPyramidals = tetrahedrals; tetraederzahlen = tetrahedrals; dreiecksPyramidalzahlen = tetrahedrals; dreiecksPyramidenZahlen = tetrahedrals
squarePyramidals = pyramidals 4; quadratPyramidalzahlen = squarePyramidals; quadratPyramidenZahlen = squarePyramidals; vierecksPyramidalzahlen = squarePyramidals; vierecksPyramidenZahlen = squarePyramidals
pentagonalPyramidals = pyramidals 5; fünfecksPyramidalzahlen = pentagonalPyramidals; fünfecksPyramidenZahlen = pentagonalPyramidals
hexagonalPyramidals = pyramidals 6; sechsecksPyramidalzahlen = hexagonalPyramidals; sechsecksPyramidenZahlen = hexagonalPyramidals
heptagonalPyramidals = pyramidals 7; septagonalPyramidals = heptagonalPyramidals; siebenecksPyramidalzahlen = heptagonalPyramidals; siebenecksPyramidenZahlen = heptagonalPyramidals
octagonalPyramidals = pyramidals 8; achtecksPyramidalzahlen = octagonalPyramidals; achtecksPyramidenZahlen = octagonalPyramidals
octahedrals = map octahedral [1..]; oktaederzahlen = octahedrals
cubicals = map cubical [1..]; cubeNumbers = cubicals; kubikzahlen = cubicals; würfelzahlen = cubicals
icosahedrals = map icosahedral [1..]; ikosaederzahlen = icosahedrals
dodecahedrals = map dodecahedral [1..]; dodekaederzahlen = dodecahedrals

-- 3d inverted number
cardanoRoot a b c d = let d0 = b*b - 3*a*c; d1 = 2*b*b*b - 9*a*b*c + 27*a*a*d; sqrtTerm = sqrt (d1*d1 - 4*d0*d0*d0); cBig = realCbrt ((d1 + sqrtTerm) / 2) in (- (1 / (3 * a))) * (b + cBig + d0 / cBig)

pyramidalRoot s x = cardanoRoot (s - 2) 3 (5 - s) (- (6 * x))
ngonalPyramidalRoot = pyramidalRoot; anygonalPyramidalRoot = pyramidalRoot; pyramidalwurzel = pyramidalRoot; pyramidenwurzel = pyramidalRoot
tetrahedralRoot = pyramidalRoot 3; triangularPyramidalRoot = tetrahedralRoot; tetraederwurzel = tetrahedralRoot; dreiecksPyramidalwurzel = tetrahedralRoot; dreiecksPyramidenwurzel = tetrahedralRoot
squarePyramidalRoot = pyramidalRoot 4; quadratPyramidalwurzel = squarePyramidalRoot; quadratPyramidenwurzel = squarePyramidalRoot; vierecksPyramidalwurzel = squarePyramidalRoot; vierecksPyramidenwurzel = squarePyramidalRoot
pentagonalPyramidalRoot = pyramidalRoot 5; fünfecksPyramidalwurzel = pentagonalPyramidalRoot; fünfecksPyramidenwurzel = pentagonalPyramidalRoot
hexagonalPyramidalRoot = pyramidalRoot 6; sechsecksPyramidalwurzel = hexagonalPyramidalRoot; sechsecksPyramidenwurzel = hexagonalPyramidalRoot
heptagonalPyramidalRoot = pyramidalRoot 7; septagonalPyramidalRoot = heptagonalPyramidalRoot; siebenecksPyramidalwurzel = heptagonalPyramidalRoot; siebenecksPyramidenwurzel = heptagonalPyramidalRoot
octagonalPyramidalRoot = pyramidalRoot 8; achtecksPyramidalwurzel = octagonalPyramidalRoot; achtecksPyramidenwurzel = octagonalPyramidalRoot

octahedralRoot x = cardanoRoot 2 0 1 (- (3 * x))
oktaederwurzel = octahedralRoot

cubicalRoot x = x ** (1 / 3)
cubeNumberRoot = cubicalRoot; kubikwurzel = cubicalRoot; würfelwurzel = cubicalRoot

icosahedralRoot x = cardanoRoot 5 (-5) 2 (- (2 * x))
ikosaederwurzel = icosahedralRoot

dodecahedralRoot x = cardanoRoot 27 (-27) 6 (- (6 * x))
dodekaederwurzel = dodecahedralRoot

-- 3d inverted number series
pyramidalRoots s = map (pyramidalRoot s) [1..]
ngonalPyramidalRoots = pyramidalRoots; anygonalPyramidalRoots = pyramidalRoots; pyramidalwurzeln = pyramidalRoots; pyramidenwurzeln = pyramidalRoots

tetrahedralRoots = pyramidalRoots 3; triangularPyramidalRoots = tetrahedralRoots; tetraederwurzeln = tetrahedralRoots; dreiecksPyramidalwurzeln = tetrahedralRoots; dreiecksPyramidenwurzeln = tetrahedralRoots
squarePyramidalRoots = pyramidalRoots 4; quadratPyramidalwurzeln = squarePyramidalRoots; quadratPyramidenwurzeln = squarePyramidalRoots; vierecksPyramidalwurzeln = squarePyramidalRoots; vierecksPyramidenwurzeln = squarePyramidalRoots
pentagonalPyramidalRoots = pyramidalRoots 5; fünfecksPyramidalwurzeln = pentagonalPyramidalRoots; fünfecksPyramidenwurzeln = pentagonalPyramidalRoots
hexagonalPyramidalRoots = pyramidalRoots 6; sechsecksPyramidalwurzeln = hexagonalPyramidalRoots; sechsecksPyramidenwurzeln = hexagonalPyramidalRoots
heptagonalPyramidalRoots = pyramidalRoots 7; septagonalPyramidalRoots = heptagonalPyramidalRoots; siebenecksPyramidalwurzeln = heptagonalPyramidalRoots; siebenecksPyramidenwurzeln = heptagonalPyramidalRoots
octagonalPyramidalRoots = pyramidalRoots 8; achtecksPyramidalwurzeln = octagonalPyramidalRoots; achtecksPyramidenwurzeln = octagonalPyramidalRoots

octahedralRoots = map octahedralRoot [1..]; oktaederwurzeln = octahedralRoots
cubicalRoots = map cubicalRoot [1..]; cubeNumberRoots = cubicalRoots; kubikwurzeln = cubicalRoots; würfelwurzeln = cubicalRoots
icosahedralRoots = map icosahedralRoot [1..]; ikosaederwurzeln = icosahedralRoots
dodecahedralRoots = map dodecahedralRoot [1..]; dodekaederwurzeln = dodecahedralRoots
```

So now, we want to figure out the most simpel cases (until the 8-limit) we haven't covered yet, add functions for those cases and then normalize downwards afterwards.
So when you analyze what we currently have, what would you conclude for our next steps?
```

ANSWER:
```text
What you have now is already “most of the big easy wins”. If you want to keep climbing *upwards from the least-general cases* (and keep the “regular-polytope family upper bound” idea), the next steps are pretty clear from the standard regular-polytope classification:

* In every dimension (d\ge 2): **simplex**, **orthoplex**, **hypercube** (orthotope). ([OEIS][1])
* Extra only in **3D**: icosahedron + dodecahedron (you already have). ([OEIS][1])
* Extra only in **4D**: 24-cell, 600-cell, 120-cell. ([OEIS][1])

So the immediate “missing pillar” in your module is:

## 1) Add the orthoplex family: `orthoplecal d n`

The OEIS taxonomy calls these **orthoplicial polytopic numbers / orthoplex numbers / cross-polytope numbers** (βₙ family). ([OEIS][2])

A standard closed form for the layered cross-polytope numbers is (paper’s notation (T(n,k))):

[
T(n,k)=\sum_{m=0}^{n-1} \binom{n-1}{m}\binom{k+m}{n}
]
([FQ Math][3])

Mapping that to your style (`d` = dimension, `n` = layer index), one clean implementation is:

```haskell
orthoplecal d n = sum [ choose (d-1) m * choose (n+m) d | m <- [0..d-1] ]
```

and then you get the expected overlaps:

* `orthoplecal 2 n = n^2` (squares)
* `orthoplecal 3 n = n(2n^2+1)/3` (octahedral) — matches your `octahedral`. ([OEIS][2])

**Root:** do exactly what you did for `simplicalRoot`: bisection + snap-to-integer. (No nice “closed radical” formula in general because it’s degree `d` in `n`.)

## 2) Factor out the shared “root machinery”

You’ve repeated three ideas a lot:

* “snap within epsilon”
* “bisection on a monotone function”
* “Cardano cubic root with real cbrt”

Next step (before adding more shapes) is to normalize those into tiny helpers, because once you add 4D special polychora you’ll want the same root strategy again.

Conceptually:

```haskell
snap eps y = ...
bisect eps f (lo,hi) = ...
rootMonotone eps f x = snap eps (bisect eps (\n -> f n > x) ...)
```

Then `simplicalRoot`, `orthoplecalRoot`, and any “polytopeRoot” become one-liners that just plug in `f`.

## 3) Then add the *4D-only* regular polychoron number families

Per OEIS/Wikipedia naming, these are commonly referred to as:

* `cell24Number` / `polychoron24Number` (24-cell numbers)
* `cell600Number` (600-cell / “hypericosahedral” numbers)
* `cell120Number` (120-cell / “hyperdodecahedral” numbers)

Those three are explicitly called out as the 4D exceptions. ([OEIS][1])

Implementation-wise, you’ll either:

* add their known closed-form polynomials (degree 4 in `n`), plus bisection roots, **or**
* derive them from a general “regular polychoron number” scheme if you want to go full-framework (OEIS wiki discusses “regular polychoron numbers” as layered dot-counts by polychoron type). ([OEIS][4])

Given your “start from simplest cases upward” plan, I’d do **explicit formulas first** (like you did for octahedral/icosahedral/dodecahedral), *then* normalize them later behind a `polytopical` dispatcher.

## 4) About your “extra argument: faces meeting at a vertex”

That parameter is real (e.g. tetra/cube/dodeca have 3; octa has 4; icosa has 5), but if you’re restricting yourself to the **regular Euclidean polytope families**, you usually don’t pass it as an argument—because it’s part of the *type* (encoded by the Schläfli symbol / which family you’re in). The OEIS “regular polytope numbers” page even phrases each family by “how many facets meet at the shared vertex” (simplicial share 3 facets, orthotopes share d facets, orthoplices share (2d-1) facets, etc.). ([OEIS][1])

So my suggested next steps in *your exact direction*:

1. **Implement `orthoplecal` + `orthoplecalRoot`** (bisection + snap), using the binomial-sum closed form above. ([FQ Math][3])
2. **Refactor** snap + bisection into helpers (so adding new families is cheap).
3. **Add 4D exceptions**: 24-cell, 600-cell, 120-cell (explicit formulas + roots via bisection). ([OEIS][4])
4. Only after that, introduce your “normalized” `polytopical` front-end that dispatches by a small `data RegularFamily = Simplex | Hypercube | Orthoplex | Icosa | Dodeca | Cell24 | Cell120 | Cell600` (that’s your “8-ish” list).

If you want, I can write the exact `orthoplecal`/`orthoplecalRoot` code in *your current one-liner style* (and in a way that works sanely for `Double` without accidentally using `(^)` on fractional exponents).

[1]: https://oeis.org/wiki/Regular_polytope_numbers "Regular polytope numbers - OeisWiki"
[2]: https://oeis.org/wiki/Classifications_of_figurate_numbers "Classifications of figurate numbers - OeisWiki"
[3]: https://www.fq.math.ca/Papers1/54-3/EdwardsGriffiths07172016.pdf "EdwardsGriffiths07172016.dvi"
[4]: https://oeis.org/wiki/Regular_polychoron_numbers "Regular polychoron numbers - OeisWiki"
```