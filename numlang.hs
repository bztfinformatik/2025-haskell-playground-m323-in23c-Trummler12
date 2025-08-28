numlang :: Int -> String -> String
numlang n lang = case lang of
  "de" -> numlangDE (toInteger n)
  "en" -> numlangEN (toInteger n)
  _    -> show n

-- English number names
numlangEN :: Integer -> String
numlangEN x
  | x < 0 = "minus " ++ numlangEN (-x)
  | x == 0 = "zero"
  | otherwise = unwords . reverse $
      [groupWords g i | (g, i) <- zip (groups x) [0..], g > 0]
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

scalesEN :: [String]
scalesEN = ["", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion"]

onesEN :: [String]
onesEN = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
          "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tensEN :: [String]
tensEN = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

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

scalesDE :: [(String, String)]
scalesDE = [("Million", "Millionen"), ("Milliarde", "Milliarden"), ("Billion", "Billionen"),
            ("Billiarde", "Billiarden"), ("Trillion", "Trillionen")]

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

-- split number into groups of 1000
groups :: Integer -> [Integer]
groups 0 = []
groups x = let (q, r) = x `divMod` 1000 in r : groups q

