{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PrettyPrinter (Pretty(..)) where

import           Prelude hiding (id)

class Pretty a where
  pp :: Int -> a -> String

instance Pretty Int where
  pp _ i = "(" <> show i <> ")"

instance Pretty () where
  pp :: Int -> () -> String
  pp _ () = ""