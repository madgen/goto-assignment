{-# LANGUAGE LambdaCase #-}

module PrettyPrinter (pp) where

import           AST
import qualified Data.List as List
import           Prelude hiding (id)

pp :: Show id => Program id -> String
pp = ppBlock 0

ppBlock :: Show id => Int -> Block id -> String
ppBlock indent = List.intercalate "\n" . List.map (ppStatement indent)

ppStatement :: Show id => Int -> Statement id -> String
ppStatement indent = \case
  (Assignment var e)
    -> List.replicate indent ' ' <> ppVariable var <> " := " <> ppExpression e
  (Ite e th el)      -> List.replicate indent ' '
    <> "if "
    <> ppExpression e
    <> " {\n"
    <> ppBlock (indent + 2) th
    <> "\n"
    <> List.replicate indent ' '
    <> "} else {\n"
    <> ppBlock (indent + 2) el
    <> "\n"
    <> List.replicate indent ' '
    <> "}"
  (While e body)     -> List.replicate indent ' '
    <> "while "
    <> ppExpression e
    <> " {\n"
    <> ppBlock (indent + 2) body
    <> "\n"
    <> List.replicate indent ' '
    <> "}"

ppExpression :: Show id => Expression id -> String
ppExpression (EConst i) = show i
ppExpression (EVar var) = ppVariable var
ppExpression (EPlus e1 e2) = ppExpression e1 <> " + " <> ppExpression e2

ppVariable :: Show id => Variable id -> String
ppVariable (Variable id str) = str <> "(" <> show id <> ")"