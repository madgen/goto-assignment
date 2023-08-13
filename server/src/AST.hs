{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AST
    ( Program
    , Block
    , Statement(..)
    , Expression(..)
    , Variable(..)
    , pp) where

import           Prelude hiding (id)
import qualified PrettyPrinter as PP
import           Data.List (intercalate)

type Program id = Block id

type Block id = [Statement id]

data Statement id where
  Assignment :: Variable id -> Expression id -> Statement id
  Ite :: Expression id -> Block id -> Block id -> Statement id
  While :: Expression id -> Block id -> Statement id
  deriving Show

data Expression id = EConst Int
                   | EVar (Variable id)
                   | EPlus (Expression id) (Expression id)
  deriving Show

data Variable id = Variable id String
  deriving Show

pp :: PP.Pretty id => Program id -> String
pp = PP.pp 0

instance PP.Pretty id => PP.Pretty (Block id) where
  pp indent = intercalate "\n" . map (PP.pp indent)

instance PP.Pretty id => PP.Pretty (Statement id) where
  pp indent = \case
    (Assignment var e)
      -> replicate indent ' ' <> PP.pp indent var <> " := " <> PP.pp indent e
    (Ite e th el)      -> replicate indent ' '
      <> "if "
      <> PP.pp indent e
      <> " {\n"
      <> PP.pp (indent + 2) th
      <> "\n"
      <> replicate indent ' '
      <> "} else {\n"
      <> PP.pp (indent + 2) el
      <> "\n"
      <> replicate indent ' '
      <> "}"
    (While e body)     -> replicate indent ' '
      <> "while "
      <> PP.pp indent e
      <> " {\n"
      <> PP.pp (indent + 2) body
      <> "\n"
      <> replicate indent ' '
      <> "}"

instance PP.Pretty id => PP.Pretty (Expression id) where
  pp _ (EConst i) = show i
  pp indent (EVar var) = PP.pp indent var
  pp indent (EPlus e1 e2) = PP.pp indent e1 <> " + " <> PP.pp indent e2

instance PP.Pretty id => PP.Pretty (Variable id) where
  pp indent (Variable id str) = str <> PP.pp indent id
