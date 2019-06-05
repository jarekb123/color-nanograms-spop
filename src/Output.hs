module Output where

import qualified System.Console.ANSI           as Console
import           Model

translateColor :: Color -> Console.Color
translateColor color | color == Empty  = Console.White
                     | color == Black  = Console.Black
                     | color == Red    = Console.Red
                     | color == Green  = Console.Green
                     | color == Yellow = Console.Yellow
                     | color == Orange = Console.Magenta
                     | -- brak pomarańczowego w System.Console.Ansi
                       color == Cyan   = Console.Cyan
                     | color == Blue   = Console.Blue

testNanogram =
    [[Empty, Black, Red], [Green, Yellow, Orange], [Cyan, Red, Black]]

putCharWithColor :: Char -> Console.Color -> IO ()
putCharWithColor x c = do
    Console.setSGR [Console.SetColor Console.Background Console.Vivid c]
    putChar x
    Console.setSGR [Console.Reset]

outputRow :: [Color] -> IO ()
outputRow []       = putChar '\n'
outputRow (x : xs) = do
    (putCharWithColor ' ') (translateColor x)
    outputRow xs

outputNanogram :: Nanogram -> IO ()
outputNanogram []           = putChar '\n'
outputNanogram (row : rows) = do
    outputRow row
    outputNanogram rows
