module Main (main) where

import           Prelude hiding (lex)
import           Data.Foldable (traverse_)
import qualified PrettyPrinter as PP
import           Lexer (lex)
import           Parser (parse)
import           Number (number)
import           System.Environment (getArgs)
import qualified GoToDef
import qualified System.IO.Strict as SIO

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lex"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nTokens:"
      print $ lex contents
    ["parse"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ PP.prettyPrint $ parse $ lex contents
    ["number"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ PP.prettyPrint $ number $ parse $ lex contents
    ["go-to-def", mode] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      let ast = number $ parse $ lex contents
      putStrLn $ PP.prettyPrint ast
      putStrLn $ "\n\nGo-to-def (" <> mode <> ") table:"
      traverse_ (\(id1, id2) -> putStrLn $ show id1 <> " -> " <> show id2)
        $ GoToDef.tabulate
        $ GoToDef.goToDef (read mode) ast
    _ -> error "dunno what's happening"

readProgramSTDIN :: IO String
readProgramSTDIN = do
  putStrLn "Reading input..."
  SIO.getContents