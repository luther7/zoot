module Main where


import System.Environment


import Parser


main :: IO ()
main = do
  command <- getArgs
  run $ parseCommand command

run :: Either ParseError Command -> IO ()
run command = print . show $ command
