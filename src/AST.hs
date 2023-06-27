{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module AST (Program, Block, Statement(..), Expression(..), Variable(..)) where

import           Prelude hiding (id)

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