{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE InstanceSigs #-}

module PrettyPrinter (prettyPrint) where

import           AST
import           Span (Span(..))
import qualified Data.List as List
import           Prelude hiding (id)

prettyPrint :: Pretty id => Program id -> String
prettyPrint = pp 0

class Pretty a where
  pp :: Int -> a -> String

instance Pretty id => Pretty (Block id) where
  pp indent = List.intercalate "\n" . List.map (pp indent)

instance Pretty id => Pretty (Statement id) where
  pp indent = \case
    (Assignment var e)
      -> List.replicate indent ' ' <> pp indent var <> " := " <> pp indent e
    (Ite e th el)      -> List.replicate indent ' '
      <> "if "
      <> pp indent e
      <> " {\n"
      <> pp (indent + 2) th
      <> "\n"
      <> List.replicate indent ' '
      <> "} else {\n"
      <> pp (indent + 2) el
      <> "\n"
      <> List.replicate indent ' '
      <> "}"
    (While e body)     -> List.replicate indent ' '
      <> "while "
      <> pp indent e
      <> " {\n"
      <> pp (indent + 2) body
      <> "\n"
      <> List.replicate indent ' '
      <> "}"

instance Pretty id => Pretty (Expression id) where
  pp _ (EConst i) = show i
  pp indent (EVar var) = pp indent var
  pp indent (EPlus e1 e2) = pp indent e1 <> " + " <> pp indent e2

instance Pretty id => Pretty (Variable id) where
  pp indent (Variable id str) = str <> "(" <> pp indent id <> ")"

instance Pretty Span where
  pp _ (Span { .. }) = show _line <> ":" <> show _column

instance Pretty Int where
  pp _ = show

instance Pretty () where
  pp :: Int -> () -> String
  pp _ () = ""