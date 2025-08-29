import Data.List (intercalate)
import Data.Char (toUpper)

-- Set to False if the terminal cannot display umlauts
supportsUmlauts :: Bool
supportsUmlauts = False

supportedLangs :: [String]
supportedLangs = ["de", "en"]

numlang :: Integer -> String -> String
numlang n lang =
      let result = case lang of
            "de" -> numlangDE n
            "en" -> numlangEN n
            _    -> error $ "Unsupported language: " ++ lang ++
                          ". Supported languages: " ++ intercalate ", " supportedLangs
      in if supportsUmlauts then result else replaceSpecials result


-- split number into groups of 1000
groups :: Integer -> [Integer]
groups 0 = []
groups x = let (q, r) = x `divMod` 1000 in r : groups q


-- English number names
numlangEN :: Integer -> String
numlangEN n
  | n < 0 = "minus " ++ numlangEN (-n)
  | n == 0 = "zero"
  | otherwise = unwords . reverse $
      [groupWords g i | (g, i) <- zip (groups n) [0..], g > 0]
  where
    groupWords g i =
      let scale = scalesEN !! fromIntegral i
          grp   = convertGroupEN g
      in if null scale then grp else grp ++ " " ++ scale

convertGroupEN :: Integer -> String
convertGroupEN n
  | n < 20 = onesEN !! fromIntegral n
  | n < 100 =
      let (t, u) = n `divMod` 10
          tensPart = tensEN !! fromIntegral t
      in tensPart ++ if u > 0 then "-" ++ convertGroupEN u else ""
  | otherwise =
      let (h, r) = n `divMod` 100
          hPart = onesEN !! fromIntegral h ++ " hundred"
      in hPart ++ if r > 0 then " and " ++ convertGroupEN r else ""

onesEN :: [String]
onesEN = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
          "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tensEN :: [String]
tensEN = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

scalesEN :: [String]
scalesEN = "" : "thousand" : map illionName [1..]


-- German number names
numlangDE :: Integer -> String
numlangDE x
  | x < 0 = "minus " ++ numlangDE (-x)
  | x == 0 = "null"
  | otherwise = unwords . reverse $
      [groupWords g i | (g, i) <- zip (groups x) [0..], g > 0]
  where
    groupWords g 0 = convertGroupDE False g
    groupWords g 1 = convertGroupDE True g ++ "tausend"
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


-- fix terminal output for umlauts
replaceSpecials :: String -> String
replaceSpecials = concatMap repl
  where
    repl 'ä' = "ae"
    repl 'ö' = "oe"
    repl 'ü' = "ue"
    repl 'Ä' = "Ae"
    repl 'Ö' = "Oe"
    repl 'Ü' = "Ue"
    repl 'ß' = "ss"
    repl c   = [c]

