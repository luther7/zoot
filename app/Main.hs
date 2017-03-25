module Main where


import Parser


import System.Environment


process :: [String] -> IO ()
process [] = print "Nothing."
process (x:xs) = do
  let result = parseQueryExpression x
  case result of
    Left error -> print error
    Right expression -> print $ show expression
  process xs


main :: IO ()
main = do
  command <- getArgs
  process command
