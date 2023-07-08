module Span (Span(..)) where

data Span = Span { _line :: Int, _column :: Int, _length :: Int }
  deriving (Eq, Show)