module NumLang.NumLang (numlang) where

import qualified Data.Map.Strict as M
import GHC.IO.Unsafe ( unsafePerformIO )
import Data.Ratio ( (%) , numerator, denominator )
import Data.List (intercalate, isPrefixOf)
import Data.Char (toUpper, isDigit, isSpace)
import Text.Read (readMaybe)
import Data.Time ( getZonedTime, LocalTime(localTimeOfDay),
                   TimeOfDay(TimeOfDay), ZonedTime(zonedTimeToLocalTime) )

-- Set to False if the terminal cannot display umlauts
supportsUmlauts :: Bool
supportsUmlauts = False

-- Limit OUTPUT Decimal Places (doesn't affect accuracy)
limitDecimals :: Int
limitDecimals = 50

supportedLangs :: [String]
-- Languages are grouped by family and sorted by approximate number
-- of native speakers within each group. German is listed first as the
-- primary language for this project.
supportedLangs =
  [ "de", "en"             -- Germanic
  , "es", "pt", "fr", "it" -- Romance
  , "tr"                   -- Turkic
  , "jp2"                  -- Japonic (_2 = spoken)
  ]

-- numlang wird jetzt über eine Typeclass überladen
class NumLang a where
  numlang :: a -> String -> String

-- Integer: wie bisher
instance NumLang Integer where
  numlang = numlangInt

instance NumLang Rational where
  numlang = numlangRat

-- Dezimalzahlen (einfach gehalten, wie beschrieben)
instance NumLang Double where
  numlang = numlangFromRealFrac
instance NumLang Float where
  numlang = numlangFromRealFrac

instance NumLang String where
  numlang s lang =
    case parseToRational s of
      Just r  -> numlangRat r lang
      Nothing -> unsupported s "String"

-- ========= Input Handling Helpers =========

-- entferne alle Spaces (vereinfacht unser Parsen deutlich)
stripSpaces :: String -> String
stripSpaces = filter (not . isSpace)

-- entferne wiederholt ein einziges äußeres, BALANCIERTES Klammernpaar
stripOuterParens :: String -> String
stripOuterParens = go . stripSpaces
  where
    go s
      | length s >= 2
      , head s == '('
      , last s == ')'
      , balanced 0 (init (tail s)) = go (init (tail s))
      | otherwise = s

    balanced :: Int -> String -> Bool
    balanced 0 [] = True
    balanced _ [] = False
    balanced d (c:cs)
      | c == '('  = balanced (d+1) cs
      | c == ')'  = d > 0 && balanced (d-1) cs
      | otherwise = balanced d cs

-- a) Dispatcher for  Double/Float
numlangFromRealFrac :: (RealFrac a, Show a) => a -> String -> String
numlangFromRealFrac x lang =
  let shown   = show x
      hasExpo = any (`elem` "eE") shown
      decs    = decimalsShown shown
      digits  = length (filter isDigit shown)
  in if hasExpo || digits > 15 || decs > 15
      then "Please send your number as a String (in quotes) to avoid floating-point inaccuracies (exponent or too many digits)."
      else case parseToRational shown of
              Just r  -> numlangRat r lang
              Nothing -> "Please send your Number as a String (using quotation marks)."

-- count total Decimals
decimalsShown :: String -> Int
decimalsShown s =
  case dropWhile (/='.') s of
    []       -> 0
    (_:rest) -> length (takeWhile isDigit rest)

unsupported :: String -> String -> String
unsupported arg1 _typ =
  "EXCEPTION: Unsupported number input: \"" ++ arg1 ++
  "\"\nAccepted forms:\n" ++
  "  - Integer             (e.g. 123, \"123\", \"123'456'789\", \"123.456.789\")\n" ++
  "  - Integer Expressions (e.g. (10^1234 + 42*69) or (Any Integer Result Expression in Brackets))\n" ++
  "  - Fraction as \"a/b\"   (e.g. \"1/47\", \"123456/7\")\n" ++
  "  - Decimal string      (e.g. \"12.345\", \"1234,5678\", \"123'456.789\", \"420.420.420,1337\")\n"

parseToRational :: String -> Maybe Rational
parseToRational raw =
  let string = dropWhile (=='+') . stripOuterParens . normalizeNumberString $ raw
  in case break (=='/') string of
       (a, '/':b) -> do
         na <- readMaybe a :: Maybe Integer
         nb <- readMaybe b :: Maybe Integer
         if nb == 0 then Nothing else Just (na % nb)
       _ | '.' `elem` string ->
           let (a, _:b) = break (=='.') string
               sign     = if take 1 a == "-" then -1 else 1
               a'       = if sign == -1 then drop 1 a else a
           in do
              n <- readMaybe a' :: Maybe Integer
              -- 3×limitDecimals „hart“ abschneiden (ohne Runden)
              let fracDigitsFull = filter isDigit b
                  fracDigits     = take (3 * limitDecimals) fracDigitsFull
                  k              = length fracDigits
              m <- readMaybe fracDigits :: Maybe Integer
              let num = sign * (n * (10 ^ k) + m)
              Just (num % (10 ^ k))
         | otherwise ->
             (% 1) <$> (readMaybe string :: Maybe Integer)

-- robustes Normalisieren von Zahlenstrings:
-- - entfernt ' und _   (Tausender)
-- - bestimmt Dezimaltrenner per *letztem* '.' oder ','
-- - ersetzt Dezimal ',' -> '.'; alle übrigen Trenner werden entfernt
normalizeNumberString :: String -> String
normalizeNumberString s0 =
  let s1 = filter (`notElem` " _'") s0
      lastPos :: Char -> String -> Int
      lastPos ch = go 0 (-1)
        where
          go _ acc []     = acc
          go i acc (c:cs) = go (i+1) (if c==ch then i else acc) cs
      dDot   = lastPos '.' s1
      dComma = lastPos ',' s1
  in case compare dDot dComma of
       GT -> filter (/= ',') s1
       LT -> map (\c -> if c==',' then '.' else c) (filter (/='.') s1)
       EQ -> s1  -- keiner/gleich -> nichts forcieren; nachgelagerte Logik entscheidet

-- (Ganzzahlteil, pre-Ziffern, period-Ziffern); alles EXAKT
decomposeRational :: Rational -> (Integer, [Int], [Int])
decomposeRational q
  | q == 0    = (0, [], [])
  | otherwise =
      let r        = abs q
          n        = numerator r
          den      = denominator r
          (i, r0)  = quotRem n den
          applySign x = if q < 0 then negate x else x
          go pos rem' seen accRev
            | rem' == 0 = (applySign i, reverse accRev, [])
            | Just p <- M.lookup rem' seen =
                let pre = reverse accRev
                    -- `p` marks the index where this remainder first
                    -- appeared. Digits before `p` form the non-repeating
                    -- prefix, digits from `p` onward make up the repeating
                    -- cycle.
                    (pre', per') = splitAt p pre
                in (applySign i, pre', per')
            | otherwise =
                let rem10 = rem' * 10
                    dgt   = fromIntegral (rem10 `quot` den) :: Int
                    rem'' = rem10 `rem` den
                in go (pos+1) rem'' (M.insert rem' pos seen) (dgt:accRev)
      in if r0 == 0
            then (applySign i, [], [])
            else go 0 r0 M.empty []

trimForDisplay :: Int -> [Int] -> [Int] -> ([Int],[Int])
trimForDisplay lim pre per
  | null per  = (take lim pre, [])
  | otherwise = (take lim pre, per)

-- ========= Decimal Helpers =========

sayDigits :: [Int] -> String -> String
sayDigits ds lang =
  unwords (if null ds then [numlangUnit 0 lang] else map (`numlangUnit` lang) ds)

sayDecimalWords :: [Int] -> [Int] -> String -> String
sayDecimalWords pre per lang
  | null per      = sayDigits pre lang
  | null pre      = unwords [ repName lang
                            , sayDigits per lang ]
  | otherwise     = unwords [ sayDigits pre lang
                            , repName lang
                            , sayDigits per lang ]

minusName :: String -> String
minusName "de" = "minus "
minusName "en" = "minus "
minusName "es" = "menos "
minusName "pt" = "menos "
minusName "fr" = "moins "
minusName "it" = "meno "
minusName "tr" = "eksi "
minusName "jp2" = "mainasu "
minusName _    = "minus "

decPointName :: String -> String
decPointName "de" = "Komma"
decPointName "en" = "point"
decPointName "es" = "coma"
decPointName "pt" = "vírgula"
decPointName "fr" = "virgule"
decPointName "it" = "virgola"
decPointName "tr" = "virgül"
decPointName "jp2" = "shousuten"
decPointName _    = "point"

repName :: String -> String
repName "de" = "Periode"
repName "en" = "repeating"
repName "es" = "periódico"
repName "pt" = "periódico"
repName "fr" = "période"
repName "it" = "periodico"
repName "tr" = "devirli"
repName "jp2" = "junkan"
repName _    = "repeating"

numlangUnit :: Int -> String -> String
numlangUnit d lang =
  let w = case lang of
            "de" -> unitsDE !! d
            "en" -> unitsEN !! d
            "es" -> unitsES !! d
            "pt" -> unitsPT !! d
            "fr" -> unitsFR !! d
            "it" -> unitsIT !! d
            "tr" -> unitsTR !! d
            "jp2" -> unitsJP2 !! d
            _    -> unitsEN !! d
  in if supportsUmlauts then w else replaceSpecials w

-- ========= Core Functionality =========

numlangInt :: Integer -> String -> String
numlangInt n lang =
      let result = case lang of
            "de" -> numlangDE n
            "en" -> numlangEN n
            "es" -> numlangES n
            "pt" -> numlangPT n
            "fr" -> numlangFR n
            "it" -> numlangIT n
            "tr" -> numlangTR n
            "jp2" -> numlangJP2 n hourZero
            _    -> error $ "Unsupported language: " ++ lang ++
                          ". Supported languages: " ++ intercalate ", " supportedLangs
      in if supportsUmlauts then result else replaceSpecials result

numlangRat :: Rational -> String -> String
numlangRat q lang =
  let (i, pre, per)   = decomposeRational q
      (pre', per')    = trimForDisplay limitDecimals pre per
      signPrefix      = if q < 0 && i == 0 then minusName lang else ""  -- z.B. -0.125
      headPart        = numlangInt i lang
  in if null pre' && null per'
        then headPart                                         -- ganze Zahl
        else signPrefix ++ unwords [ headPart
                                   , decPointName lang
                                   , sayDecimalWords pre' per' lang ]

-- split number into groups of 1000
groups :: Integer -> Int -> [Integer]
groups 0 n = []
groups x n = let (q, r) = x `divMod` (10^n) in r : groups q n

-- ======== LANGUAGES =========
-- German number names
numlangDE :: Integer -> String
numlangDE x
  | x < 0 = "minus " ++ numlangDE (-x)
  | x == 0 = "null"
  | otherwise = unwords . reverse $
      [groupWords g i | (g, i) <- zip (groups x 3) [0..], g > 0]
  where
    groupWords g 0 = convertGroupDE False g
    groupWords g 1 = if g == 1 then "tausend" else convertGroupDE True g ++ "tausend"
    groupWords g i =
      let (sing, plur) = scalesDE !! fromIntegral (i - 2)
      in if g == 1
         then "eine " ++ sing
         else convertGroupDE False g ++ " " ++ plur

convertGroupDE :: Bool -> Integer -> String
convertGroupDE _ 0 = ""
convertGroupDE isScale n
  | n < 20 = case fromIntegral n of
      1 -> if isScale then "ein" else "eins"
      k -> unitsDE !! k
  | n < 100 =
      let (t, u) = n `divMod` 10
          tensPart = tensDE !! fromIntegral t
          unitPart = case fromIntegral u of
            0 -> ""
            1 -> "ein"
            k -> unitsDE !! k
      in if u > 0 then unitPart ++ "und" ++ tensPart else tensPart
  | otherwise =
      let (h, r) = n `divMod` 100
          hPart = if h == 1 then "ein" else unitsDE !! fromIntegral h
      in hPart ++ "hundert" ++ convertGroupDE False r

unitsDE :: [String]
unitsDE = ["null", "eins", "zwei", "drei", "vier", "fünf", "sechs", "sieben", "acht", "neun", "zehn",
           "elf", "zwölf", "dreizehn", "vierzehn", "fünfzehn", "sechzehn", "siebzehn", "achtzehn", "neunzehn"]

tensDE :: [String]
tensDE = ["", "", "zwanzig", "dreißig", "vierzig", "fünfzig", "sechzig", "siebzig", "achtzig", "neunzig"]

scalesDE :: [(String, String)]
scalesDE = concatMap (makePairs . capitalize . illionName) [1..]
  where
    makePairs base =
      let singular = base
          plural   = base ++ "en"
          arde     = makeArde base
          ardePl   = arde ++ "n"
      in [(singular, plural), (arde, ardePl)]


-- English number names
numlangEN :: Integer -> String
numlangEN n
  | n < 0 = "minus " ++ numlangEN (-n)
  | n == 0 = "zero"
  | otherwise = unwords . reverse $
      [groupWords g i | (g, i) <- zip (groups n 3) [0..], g > 0]
  where
    groupWords g i =
      let scale = scalesEN !! fromIntegral i
          grp   = convertGroupEN g
      in if null scale then grp else grp ++ " " ++ scale

convertGroupEN :: Integer -> String
convertGroupEN n
  | n < 20 = unitsEN !! fromIntegral n
  | n < 100 =
      let (t, u) = n `divMod` 10
          tensPart = tensEN !! fromIntegral t
      in tensPart ++ if u > 0 then "-" ++ convertGroupEN u else ""
  | otherwise =
      let (h, r) = n `divMod` 100
          hPart = unitsEN !! fromIntegral h ++ " hundred"
      in hPart ++ if r > 0 then " and " ++ convertGroupEN r else ""

unitsEN :: [String]
unitsEN = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
          "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tensEN :: [String]
tensEN = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scalesEN :: [String]
scalesEN = "" : "thousand" : map illionName [1..]



-- Spanish number names
numlangES :: Integer -> String
numlangES n
  | n < 0 = "menos " ++ numlangES (-n)
  | n == 0 = "cero"
  | otherwise = unwords . reverse $
      [groupWords g i | (g,i) <- zip (groups n 3) [0..], g > 0]
  where
    groupWords g 0 = convertGroupES g
    groupWords g 1 = if g==1 then "mil" else contractUnoES (convertGroupES g) ++ " mil"
    groupWords g i =
      let (sing,plur) = scalesES !! fromIntegral (i - 2)
          noun = if g==1 then sing else plur
          left = if g==1 then "un " else contractUnoES (convertGroupES g) ++ " "
      in left ++ noun

contractUnoES :: String -> String
contractUnoES = replace " veintiuno" " veintiún"
  . replace " y uno"   " y un"
  . replace " uno"     " un"

convertGroupES :: Integer -> String
convertGroupES n
  | n < 30 =
      if n <= 20 then unitsES !! fromIntegral n
      else let u = fromIntegral (n - 20) :: Int
           in case u of
                2 -> "veintidós"
                3 -> "veintitrés"
                6 -> "veintiséis"
                _ -> "veinti" ++ unitsES !! u
  | n < 100 =
      let (t,u) = n `divMod` 10
          tensPart = tensES !! fromIntegral t
      in if u == 0 then tensPart else tensPart ++ " y " ++ convertGroupES u
  | n == 100 = "cien"
  | n < 200 = "ciento " ++ convertGroupES (n - 100)
  | n < 1000 =
      let (h,r) = n `divMod` 100
          hPart = hundredsES !! fromIntegral h
      in if r == 0 then hPart else hPart ++ " " ++ convertGroupES r
  | otherwise = "" -- unreachable

unitsES :: [String]
unitsES = ["cero","uno","dos","tres","cuatro","cinco","seis","siete","ocho","nueve","diez",
           "once","doce","trece","catorce","quince","dieciséis","diecisiete","dieciocho","diecinueve"]

tensES :: [String]
tensES = ["","","veinte","treinta","cuarenta","cincuenta","sesenta","setenta","ochenta","noventa"]

hundredsES :: [String]
hundredsES = ["","ciento","doscientos","trescientos","cuatrocientos","quinientos","seiscientos","setecientos","ochocientos","novecientos"]

scalesES :: [(String,String)]
scalesES = concatMap makePair [1..]
  where
    makePair n =
      let base = illionName n
          stem = take (length base - 3) base
          singular = stem ++ "ón"
          plural = stem ++ "ones"
          milForm = "mil " ++ plural
      in [(singular, plural), (milForm, milForm)]


-- Portuguese number names
numlangPT :: Integer -> String
numlangPT n
  | n < 0     = "menos " ++ numlangPT (-n)
  | n == 0    = "zero"
  | otherwise =
      let gis = [(g,i) | (g,i) <- zip (groups n 3) [0..], g > 0]
          toks = build gis
      in unwords (reverse toks)
  where
    build ((g0,0):(g1,1):rest)
      | g1 > 0 && g0 < 100 =
          groupWords g0 0 : ("e" : groupWords g1 1 : build rest)
    build ((g,i):rest) = groupWords g i : build rest
    build [] = []
    groupWords g 0 = convertGroupPT g
    groupWords g 1 = if g == 1 then "mil" else convertGroupPT g ++ " mil"
    groupWords g i =
      let (sing,plur) = scalesPT !! fromIntegral (i - 2)
      in if sing == plur
            then if g == 1 then sing else convertGroupPT g ++ " " ++ plur
            else if g == 1 then "um " ++ sing else convertGroupPT g ++ " " ++ plur

convertGroupPT :: Integer -> String
convertGroupPT n
  | n < 20 = unitsPT !! fromIntegral n
  | n < 100 =
      let (t,u) = n `divMod` 10
          tensPart = tensPT !! fromIntegral t
      in if u == 0 then tensPart else tensPart ++ " e " ++ convertGroupPT u
  | n == 100 = "cem"
  | n < 200 = "cento" ++ if r > 0 then " e " ++ convertGroupPT r else ""
  | n < 1000 =
      let (h,r) = n `divMod` 100
          hPart = hundredsPT !! fromIntegral h
      in if r == 0 then hPart else hPart ++ " e " ++ convertGroupPT r
  | otherwise = "" -- unreachable
  where r = n - 100

unitsPT :: [String]
unitsPT = ["zero","um","dois","três","quatro","cinco","seis","sete","oito","nove","dez",
           "onze","doze","treze","catorze","quinze","dezasseis","dezassete","dezoito","dezanove"]

tensPT :: [String]
tensPT = ["","","vinte","trinta","quarenta","cinquenta","sessenta","setenta","oitenta","noventa"]

hundredsPT :: [String]
hundredsPT = ["","cento","duzentos","trezentos","quatrocentos","quinhentos","seiscentos","setecentos","oitocentos","novecentos"]

scalesPT :: [(String,String)]
scalesPT = concatMap makePair [1..]
  where
    makePair n =
      let pref = illionPrefix n
          singular = if n == 1 then "milhão" else pref ++ "ilião"
          plural   = if n == 1 then "milhões" else pref ++ "iliões"
          milForm  = "mil " ++ plural
      in [(singular, plural), (milForm, milForm)]


-- French number names
numlangFR :: Integer -> String
numlangFR n
  | n < 0 = "moins " ++ numlangFR (-n)
  | n == 0 = "zéro"
  | otherwise = unwords . reverse $
      [groupWords g i | (g,i) <- zip (groups n 3) [0..], g > 0]
  where
    groupWords g 0 = convertGroupFR g
    groupWords g 1 = if g == 1 then "mille" else convertGroupFR g ++ " mille"
    groupWords g i =
      let (sing,plur) = scalesFR !! fromIntegral (i - 2)
      in if g == 1 then "un " ++ sing else convertGroupFR g ++ " " ++ plur

convertGroupFR :: Integer -> String
convertGroupFR n
  | n < 20 = unitsFR !! fromIntegral n
  | n < 70 =
      let (t,u) = n `divMod` 10
          tensPart = tensFR !! fromIntegral t
      in tensPart ++ case u of
                       0 -> ""
                       1 -> " et un"
                       _ -> "-" ++ convertGroupFR u
  | n < 80 = "soixante" ++
      if n == 71 then " et onze" else "-" ++ convertGroupFR (n-60)
  | n < 100 =
      let rest = n - 80
          base = "quatre-vingt"
      in base ++ if rest == 0 then "s"
                 else if rest == 1 then "-un"
                 else "-" ++ convertGroupFR rest
  | n == 100 = "cent"
  | n < 200 = "cent" ++ if r > 0 then " " ++ convertGroupFR r else ""
  | otherwise =
      let (h,r) = n `divMod` 100
          hPart = unitsFR !! fromIntegral h ++ " cent" ++ (if h > 1 && r == 0 then "s" else "")
      in hPart ++ if r > 0 then " " ++ convertGroupFR r else ""
  where r = n - 100

unitsFR :: [String]
unitsFR = ["zéro","un","deux","trois","quatre","cinq","six","sept","huit","neuf","dix",
           "onze","douze","treize","quatorze","quinze","seize","dix-sept","dix-huit","dix-neuf"]

tensFR :: [String]
tensFR = ["","","vingt","trente","quarante","cinquante","soixante","soixante","quatre-vingt","quatre-vingt"]

scalesFR :: [(String,String)]
scalesFR = concatMap makePairs [1..]
  where
    makePairs n =
      let base = illionName n
          singular = base
          plural = base ++ "s"
          iard = makeIard base
          iardPl = iard ++ "s"
      in [(singular, plural), (iard, iardPl)]

makeIard :: String -> String
makeIard name = take (length name - 3) name ++ "iard"


-- Italian number names
numlangIT :: Integer -> String
numlangIT n
  | n < 0 = "meno " ++ numlangIT (-n)
  | n == 0 = "zero"
  | otherwise = unwords . reverse $
      [groupWords g i | (g,i) <- zip (groups n 3) [0..], g > 0]
  where
    groupWords g 0 = convertGroupIT g
    groupWords g 1 = if g == 1 then "mille" else convertGroupIT g ++ "mila"
    groupWords g i =
      let (sing,plur) = scalesIT !! fromIntegral (i - 2)
      in if g == 1 then "un " ++ sing else convertGroupIT g ++ " " ++ plur

convertGroupIT :: Integer -> String
convertGroupIT n
  | n < 20 = unitsIT !! fromIntegral n
  | n < 100 =
      let (t,u) = n `divMod` 10
          tensPart = tensIT !! fromIntegral t
          tensAdj = if u == 1 || u == 8 then dropLastVowel tensPart else tensPart
      in tensAdj ++ if u > 0 then unitsIT !! fromIntegral u else ""
  | otherwise =
      let (h,r) = n `divMod` 100
          hPart = hundredsIT !! fromIntegral h
          hAdj = if r == 8 || (r >= 80 && r < 90) then dropLastVowel hPart else hPart
      in hAdj ++ if r > 0 then convertGroupIT r else ""

dropLastVowel :: String -> String
dropLastVowel s
  | null s = s
  | last s `elem` "aeiou" = init s
  | otherwise = s

unitsIT :: [String]
unitsIT = ["zero","uno","due","tre","quattro","cinque","sei","sette","otto","nove","dieci",
           "undici","dodici","tredici","quattordici","quindici","sedici","diciassette","diciotto","diciannove"]

tensIT :: [String]
tensIT = ["","","venti","trenta","quaranta","cinquanta","sessanta","settanta","ottanta","novanta"]

hundredsIT :: [String]
hundredsIT = ["","cento","duecento","trecento","quattrocento","cinquecento","seicento","settecento","ottocento","novecento"]

scalesIT :: [(String,String)]
scalesIT = concatMap makePairs [1..]
  where
    makePairs n =
      let pref = illionPrefix n
          singular = pref ++ "ilione"
          plural = pref ++ "ilioni"
          iard = pref ++ "iliardo"
          iardPl = pref ++ "iliardi"
      in [(singular, plural), (iard, iardPl)]


-- Turkish number names
numlangTR :: Integer -> String
numlangTR n
  | n < 0 = "eksi " ++ numlangTR (-n)
  | n == 0 = "sıfır"
  | otherwise = unwords . reverse $
      [groupWords g i | (g,i) <- zip (groups n 3) [0..], g > 0]
  where
    groupWords g i =
      let scale = scalesTR !! fromIntegral i
          grp = convertGroupTR g
      in if null scale
           then grp
           else if g == 1 && scale == "bin"
                  then scale
                  else if g == 1
                         then "bir " ++ scale
                         else grp ++ " " ++ scale

convertGroupTR :: Integer -> String
convertGroupTR 0 = ""
convertGroupTR n
  | n < 20 = unitsTR !! fromIntegral n
  | n < 100 =
      let (t,u) = n `divMod` 10
          tensPart = tensTR !! fromIntegral t
      in if u == 0 then tensPart else tensPart ++ " " ++ unitsTR !! fromIntegral u
  | otherwise =
      let (h,r) = n `divMod` 100
          hPart = if h == 1 then "yüz" else unitsTR !! fromIntegral h ++ " yüz"
      in if r == 0 then hPart else hPart ++ " " ++ convertGroupTR r

unitsTR :: [String]
unitsTR = ["sıfır","bir","iki","üç","dört","beş","altı","yedi","sekiz","dokuz","on",
           "on bir","on iki","on üç","on dört","on beş","on altı","on yedi","on sekiz","on dokuz"]

tensTR :: [String]
tensTR = ["","","yirmi","otuz","kırk","elli","altmış","yetmiş","seksen","doksan"]

scalesTR :: [String]
scalesTR = "" : "bin" : "milyon" : "milyar" : map turkishIllion [3..]
  where
    turkishIllion n = foldl (\acc (o,nw) -> replace o nw acc) (illionName n) reps
    reps = [("illion","ilyon"),("quadr","katr"),("quint","kent"),("sext","seks"),
            ("oct","ok"),("dec","des"),("undec","undes"),("duodec","duodes"),
            ("tredec","tredes"),("quattuordec","kattordes"),("quindec","kindes"),
            ("sexdec","seksdes"),("septendec","septendes"),("octodec","oktodes"),
            ("novemdec","novemdes")]


-- Japanese number names (romaji)
hourZero :: Bool
hourZero =
  let TimeOfDay hour _ _ = localTimeOfDay . zonedTimeToLocalTime $ unsafePerformIO getZonedTime
  in hour `mod` 12 == 0

numlangJP2 :: Integer -> Bool -> String
numlangJP2 n hourZero
  | n < 0     = "mainasu " ++ numlangJP2 (-n) hourZero
  | n == 0    = if hourZero then "rei" else "zero"
  | otherwise =
      let gs            = [(g,i) | (g,i) <- zip (groups n 4) [0..], g > 0]
          maxNamedIx    = length scalesJP2 - 1
          (low, high)   = span (\(_g,i) -> i <= maxNamedIx) gs
      in case reverse high of
          [] -> unwords . reverse $
                  [ groupWords g i | (g,i) <- gs ]
          _  -> let str         = show n
                    digits      = length str
                    e           = digits - 1
                    coeffLen    = 1 + ((digits - 1) `mod` 4)
                    coeffVal    = read (take coeffLen str) :: Integer
                    coeffRead   = convertGroupJP2 coeffVal
                    tailWords   = unwords . reverse $
                                    [ groupWords g i | (g,i) <- low ]
                    headWords   = coeffRead ++ " kakeru juu no " ++ numlangJP2 (fromIntegral e) hourZero ++ " jou"
                in if null tailWords then headWords else headWords ++ " purasu " ++ tailWords
  where
    groupWords g i =
      let scale = if i < length scalesJP2 then scalesJP2 !! fromIntegral i else ""
          grp   = convertGroupJP2 g
      in if null scale then grp else grp ++ " " ++ scale

convertGroupJP2 :: Integer -> String
convertGroupJP2 0 = ""
convertGroupJP2 n =
  let (th, r1) = n `divMod` 1000
      (h, r2)  = r1 `divMod` 100
      (t, u)   = r2 `divMod` 10
      parts = [thPart th, hPart h, tPart t, uPart u]
  in unwords (filter (not . null) parts)
  where
    thPart 0 = ""
    thPart 1 = "sen"
    thPart 3 = "san-zen"
    thPart 8 = "has-sen"
    thPart k = unitsJP2 !! fromIntegral k ++ "-sen"
    hPart 0 = ""
    hPart 1 = "hyaku"
    hPart 3 = "san-byaku"
    hPart 6 = "rop-pyaku"
    hPart 8 = "hap-pyaku"
    hPart k = unitsJP2 !! fromIntegral k ++ "-hyaku"
    tPart 0 = ""
    tPart 1 = "juu"
    tPart k = unitsJP2 !! fromIntegral k ++ "-juu"
    uPart 0 = ""
    uPart k = unitsJP2 !! fromIntegral k

unitsJP2 :: [String]
unitsJP2 =
  [ "zero", "ichi", "ni", "san", "yon", "go", "roku", "nana", "hachi", "kyuu"]

scalesJP2 :: [String]
scalesJP2 = base
  where
    base = ["", "man", "oku", "chou", "kei", "gai", "jo", "jou", "kou", "kan", "sei",
            "sai", "goku", "gougasha", "asougi", "nayuta", "fukashigi", "muryoutaisuu"]


-- ========= LARGE NUMBERS =========

-- large number names
illionName :: Integer -> String
illionName n = illionPrefix n ++ "illion"

illionPrefix :: Integer -> String
illionPrefix n
  | n < 20    = base !! fromIntegral n
  | n < 100   = units !! fromIntegral (n `mod` 10) ++ tens !! fromIntegral (n `div` 10)
  | n < 1000  = hundreds !! fromIntegral (n `div` 100) ++ illionPrefix (n `mod` 100)
  | otherwise =
      let (q, r) = n `divMod` 1000
          qPart  = if q == 1 then "millin" else illionPrefix q ++ "millia"
          rPart  = illionPrefix r
      in qPart ++ rPart
  where
    base = ["","m","b","tr","quadr","quint","sext","sept","oct","non","dec",
            "undec","duodec","tredec","quattuordec","quindec","sexdec","septendec","octodec","novemdec"]
    units = ["","un","duo","tre","quattuor","quin","sex","septen","octo","novem"]
    tens = ["","dec","vigint","trigint","quadragint","quinquagint","sexagint","septuagint","octogint","nonagint"]
    hundreds = ["","cent","ducent","trecent","quadringent","quingent","sescent","septingent","octingent","nongent"]

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

makeArde :: String -> String
makeArde name = take (length name - 3) name ++ "iarde"

-- ========= UTILITIES =========

-- simple string replacement
replace :: String -> String -> String -> String
replace old new = go
  where
    go s | old `isPrefixOf` s = new ++ go (drop (length old) s)
    go (c:cs) = c : go cs
    go [] = []

-- fix terminal output for umlauts
-- Only map characters here if they are used in `numlang` and confirmed to be
-- unsupported by the terminal in use.
replaceSpecials :: String -> String
replaceSpecials = concatMap repl
  where
    repl 'ä' = "ae"; repl 'ö' = "oe"; repl 'ü' = "ue"
    repl 'Ä' = "Ae"; repl 'Ö' = "Oe"; repl 'Ü' = "Ue"
    repl 'ß' = "ss"
    -- repl 'ã' = "a"
    repl 'õ' = "o"
    -- repl 'é' = "e"
    repl 'ê' = "e"
    -- repl 'í' = "i"
    -- repl 'ó' = "o"
    -- repl 'Ã' = "A"
    -- repl 'Õ' = "O"
    -- repl 'É' = "E"
    -- repl 'Ê' = "E"
    -- repl 'Í' = "I"
    -- repl 'Ó' = "O"
    repl 'ç' = "c"
    -- repl 'ğ' = "g"
    repl 'ı' = "i"
    repl 'ş' = "s"
    -- repl 'Ç' = "C"
    -- repl 'Ğ' = "G"
    -- repl 'İ' = "I"
    -- repl 'Ş' = "S"
    repl c   = [c]

