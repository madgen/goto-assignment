module Main (main) where

import           Prelude hiding (lex, id)
import           Data.Foldable (traverse_)
import qualified AST as AST
import           Lexer (lex)
import           Parser (parse)
import qualified Span as Span
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
      putStrLn $ AST.pp $ parse $ lex contents
    ["number"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ AST.pp $ number $ parse $ lex contents
    ["go-to-def", mode] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      let ast = parse $ lex contents
      putStrLn $ AST.pp ast
      putStrLn $ "\n\nGo-to-def (" <> mode <> ") table:"
      traverse_ (\(id, ids) -> putStrLn $ Span.pp id <> " -> " <> Span.pps ids)
        $ GoToDef.tabulate
        $ GoToDef.goToDef (read mode) ast
    _ -> error "dunno what's happening"

readProgramSTDIN :: IO String
readProgramSTDIN = do
  putStrLn "Reading input..."
  SIO.getContents