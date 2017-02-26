module Main where


import System.Environment


import Parser


main :: IO ()
main = do
  command <- getArgs
  run $ parseCommand command

run :: Either ParseError String -> IO ()
run command = print . show $ command
