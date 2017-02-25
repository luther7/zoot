module ZootOptions
    ( withInfo,
      parseOptions,
      Options
    ) where


import Options.Applicative
import Data.Monoid


type OptionExample = String
type CommandExample1 = String
type CommandExample2 = String

data Command
    = CommandExample1 CommandExample2
    | CommandExample2 CommandExample1

data Options = Options OptionExample Command


parseOptionExample :: Parser OptionExample
parseOptionExample = strOption $
    short 'o' <> long "option" <> metavar "OPTION-EXAMPLE" <>
    help "Option example"


parseCommandExample1 :: Parser Command
parseCommandExample1 = CommandExample1 <$> argument str (metavar "COMMAND-EXAMPLE-1")


parseCommandExample2 :: Parser Command
parseCommandExample2 = CommandExample2 <$> argument str (metavar "COMMAND-EXAMPLE-2")


parseCommand :: Parser Command
parseCommand = subparser $
    command "command-example-1" (parseCommandExample1 `withInfo` "Command example 1") <>
    command "command-example-2" (parseCommandExample2 `withInfo` "Command example 2")


parseOptions :: Parser Options
parseOptions = Options <$> parseOptionExample <*> parseCommand


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
