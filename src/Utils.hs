module Utils where

import           Data.List

-- Funkcja 'replaceAt' zamienia element listy.
-- Pierwszy argument: lista, drugi argument: nowy element, trzeci argument: numer porzadkowy
replaceAt :: [a] -> a -> Int -> [a]
replaceAt [] _ _ = []
replaceAt l e n =
  let (xs, y:ys) = splitAt n l
   in xs ++ [e] ++ ys

-- 1st arg: lista dwuwymiarowa
-- 2nd arg: nowy element
-- 3rd arg: numer kolumny (od 0)
-- 4th arg: numer wiersza (od 0)
replaceAt2 :: [[a]] -> a -> Int -> Int -> [[a]]
replaceAt2 [] _ _ _ = []
replaceAt2 l e c r =
  let row = replaceAt (l !! r) e c
   in replaceAt l row r

-- Funkcja zwracajaca listę n takich samych elementow
generate :: a -> Int -> [a]
generate _ 0 = []
generate x n = x : generate x (n - 1)

-- Wypisuje kazdy wiersz tablicy dwuwymiarowej w nowej linii
prettyPrint :: Show a => [[a]] -> IO ()
prettyPrint x = mapM_ print x
