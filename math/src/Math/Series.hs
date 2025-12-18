module Math.Series where

-- SECTIONS:
--   nd numbers
--   2d numbers
--   3d numbers
--     3d pyramidal numbers
--     (TO-DO: 3d Prism numbers)
-- Subsections for each Section: singular, series, singularRoot, series of roots

-- cube root helper
realCbrt :: (Ord a, Floating a) => a -> a
realCbrt x = signum x * abs x ** (1/3)

-- nd number
simplical d n = product [n .. n + max 0 d - 1] / product [1 .. max 0 d]; simplexNumber = simplical; simplexZahl = simplical
hypercubical d n = n ** fromIntegral d; hypercubeNumber = hypercubical; hypercubeZahl = hypercubical
orthoplecal d n = sum [ (2 ** fromIntegral (k-1)) * (fromIntegral (product [d-k+1 .. d-1]) / fromIntegral (product [1 .. k-1])) * (product [n-fromIntegral k+1 .. n] / product [1 .. fromIntegral k]) | k <- [1 .. max 0 d] ]
orthoplexNumber = orthoplecal; orthoplexZahl = orthoplecal

-- nd inverted number
simplicalRoot d x = (\y -> let r = fromIntegral (round y) in if abs (y - r) < 1e-6 then r else y) (let (l,h) = until (\(l,h)->h-l<1e-6) (\(l,h)->let m=(l+h)/2 in if simplical d m > x then (l,m) else (m,h)) (0, max 1 (x+1)) in (l+h)/2)
simplexRoot = simplicalRoot; simplexWurzel = simplicalRoot
hypercubicalRoot d x = (\y -> let r = fromIntegral (round y) in if abs (y - r) < 1e-6 then r else y) (x ** (1 / fromIntegral d))
hypercubeRoot = hypercubicalRoot; hypercubeWurzel = hypercubicalRoot
orthoplecalRoot d x = (\y->let r=fromIntegral (round y) in if abs (y-r)<1e-6 then r else y) (let f n = sum [ (2 ** fromIntegral (k-1)) * (fromIntegral (product [d-k+1 .. d-1]) / fromIntegral (product [1 .. k-1])) * (product [n-fromIntegral k+1 .. n] / product [1 .. fromIntegral k]) | k <- [1 .. max 0 d] ]; (l,h)=until (\(l,h)->h-l<1e-6) (\(l,h)->let m=(l+h)/2 in if f m > x then (l,m) else (m,h)) (0, max 1 (x+1)) in (l+h)/2)
orthoplexRoot = orthoplecalRoot; orthoplexWurzel = orthoplecalRoot

-- nd number series
simplicals d = map (simplical d) [1..]; simplexNumbers = simplicals; simplexZahlen = simplicals
hypercubicals d = map (hypercubical d) [1..]; hypercubeNumbers = hypercubicals; hypercubeZahlen = hypercubicals
orthoplecals d = map (orthoplecal d) [1..]; orthoplexNumbers = orthoplecals; orthoplexZahlen = orthoplecals

-- nd inverted number series
simplicalRoots d = map (simplicalRoot d) [1..]; simplexRoots = simplicalRoots; simplexWurzeln = simplicalRoots
hypercubicalRoots d = map (hypercubicalRoot d) [1..]; hypercubeRoots = hypercubicalRoots; hypercubeWurzeln = hypercubicalRoots
orthoplecalRoots d = map (orthoplecalRoot d) [1..]; orthoplexRoots = orthoplecalRoots; orthoplexWurzeln = orthoplecalRoots

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
