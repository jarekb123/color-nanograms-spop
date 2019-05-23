module Solver where

-- Import modułu z modelami danych, tj. kolor, blok, itp.
import Model
import Utils

-- Nanogram - typ reprezentujacy nanogram z zamalowanymi kratkami (boxami)
type Nanogram = [[Color]]

emptyNanogram :: Int -> Int -> Nanogram
emptyNanogram cols rows = generate (generate Empty cols) rows

{-|
  Funkcja 'putColor' zamalowuje kratkę (box) nanogramu.
  Przyjmuje trzy argumenty: Nanogram, numer kolumny ('Int'), numer wiersza ('Int').
  Zwraca 'Nanogram' z zamalowaną kratka
-}
putColor :: Nanogram -> Color -> Int -> Int -> Nanogram
putColor n color col row = replaceAt2 n color col row