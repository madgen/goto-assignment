{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Prelude hiding (lex)
import           Data.Foldable (traverse_)
import           PrettyPrinter (pp)
import           Lexer (lex)
import           Parser (parse)
import           Number (number)
import           System.Environment (getArgs)
import qualified GoToDef

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lex"] -> do
      !contents <- readProgramSTDIN
      putStrLn "\n\nTokens:"
      print $ lex contents
    ["parse"] -> do
      !contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ pp $ parse $ lex contents
    ["number"] -> do
      !contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ pp $ number $ parse $ lex contents
    ["go-to-first-def"] -> do
      !contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      let ast = number $ parse $ lex contents
      putStrLn $ pp ast
      putStrLn "\n\nGo-to-def (first) table:"
      traverse_ (\(id1, id2) -> putStrLn $ show id1 <> " -> " <> show id2)
        $ GoToDef.tabulate
        $ GoToDef.goToFirstDef ast
    _ -> error "dunno what's happening"

readProgramSTDIN :: IO String
readProgramSTDIN = do
  putStrLn ""
  putStrLn "Reading input..."
  getContents