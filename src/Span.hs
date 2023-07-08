{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Span (Span(..), pp, pps) where

import           Data.List (intercalate)
import qualified PrettyPrinter as PP

data Span =
  Span { _offset :: Int, _line :: Int, _column :: Int, _length :: Int }
  deriving Show

endOffset :: Span -> Int
endOffset s = _offset s + _length s

instance Eq Span where
  s1 == s2 = _offset s1 == _offset s2 && _length s1 == _length s2

instance Ord Span where
  s1 `compare` s2
    | s1 == s2 = EQ
    | s1 `isWithin` s2 = LT
    | s2 `isWithin` s1 = GT
    | s1 `isBefore` s2 = LT
    | s2 `isBefore` s1 = GT
    | otherwise = error "Oh no, intersecting spans!"

isWithin :: Span -> Span -> Bool
s1 `isWithin` s2 = _offset s1 <= _offset s2 && endOffset s2 <= endOffset s1

isBefore :: Span -> Span -> Bool
s1 `isBefore` s2 = _offset s1 <= _offset s2 && endOffset s1 <= endOffset s2

pp :: Span -> String
pp = PP.pp 0

pps :: [Span] -> String
pps spans = "{" <> intercalate "," (pp <$> spans) <> "}"

instance PP.Pretty Span where
  pp _ (Span { .. }) = "(" <> show _line <> ":" <> show _column <> ")"