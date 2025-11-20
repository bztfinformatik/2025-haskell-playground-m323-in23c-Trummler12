module Main (main) where

-- Return the third element of a triple
-- Elements of triple can be of different types
third :: (a ,b, c) -> c
third (_, _, x) = x

-- Return the third element of a triple
-- Elements of triple must be of the same type
third' :: (a ,a, a) -> a
third' (_, _, x) = x

-- Calculate the area of a rectangle
-- Length and width can be of any numeric type
rectangleArea :: Num a => a -> a -> a
rectangleArea length width = length * width


-- Calculate the area of a triangle
-- Base and height cannot be of type class Num, since Num does not implement the division operator (/)
triangleArea :: Fractional a => a -> a -> a
triangleArea base height = base * height / 2

-- Increment a number by 1
-- Does work only for Int
incrementBy1 :: Int -> Int
incrementBy1 x = x + 1

-- Does not work for any type, since (+) is not defined for all types
-- incrementBy1' :: a -> a
-- incrementBy1 x = x + 1


-- Increment a number by 1
-- Works for any type that is an instance of the Num type class
incrementBy1'' :: Num a => a -> a
incrementBy1'' x = x + 1

-- Convert a number to its German word representation using pattern matching
-- falls back to showing the number if no match is found
num2word :: Int -> String
num2word 1 = "eins"
num2word 2 = "zwei"
num2word 3 = "drei"
num2word n = show n


-- Convert a number to its German word representation using guards
-- falls back to showing the number if no match is found
num2word' :: Int -> String
num2word' n
  | n == 1 = "eins"
  | n == 2 = "zwei"
  | n == 3 = "drei"
  | otherwise = show n

-- Convert a number to its German or English word representation using pattern matching
-- falls back to showing the number if no match is found
num2wordDeOrEn :: Int -> String -> String
num2wordDeOrEn 1 "de" = "eins"
num2wordDeOrEn 2 "de" = "zwei"
num2wordDeOrEn 3 "de" = "drei"
num2wordDeOrEn 1 "en" = "one"
num2wordDeOrEn 2 "en" = "two"
num2wordDeOrEn 3 "en" = "three"
num2wordDeOrEn n _ = show n  

main :: IO ()
main = pure ()
