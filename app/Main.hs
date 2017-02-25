module Main where


import Options.Applicative
import ZootOptions


main :: IO ()
main = run =<< execParser
    (parseOptions `withInfo` "Zoot")


run :: Options -> IO ()
run opts = undefined
