-- Vektor-Modul; Die Übung sieht vor, dass wir stets 2-dimensionale Vektoren verwenden, ich möchte die Dimensionalität jedoch nach oben offen halten;
-- Zudem will ich keinen bereits vorgefertigten Datentyp "Vektor" verwenden, sondern einen eigenen definieren mit Namen "vec"
-- Mögliche Eingaben: "vprint vsub (Vec 2 4 7) (Vec 1 2 3)" => "[1 2 4]"; "vprint vlength (Vec 3 4)" => "5.0"; "vprint vscale 2 (Vec 1 2 3)" => "[2 4 6]"

module Vector where

-- wir starten als Übung mal rein mit 2-dimensionalen Vektoren

data Vec where
  -- Vec soll eine x-beliebige Zahl an Argumenten erhalten dürfen, die alle vom Typ Double sind
  Vec :: [Double] -> Vec

  deriving (Show)

vadd :: Vec -> Vec -> Vec
vadd (Vec xs) (Vec ys) = Vec (zipWith (+) xs ys)

vsub :: Vec -> Vec -> Vec
vsub (Vec xs) (Vec ys) = Vec (zipWith (-) xs ys)

add2 :: [Double] -> [Double] -> [Double]
add2 xs ys = zipWith (+) xs ys
