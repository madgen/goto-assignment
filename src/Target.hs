module Target (Target(..)) where

data Target = Target { _line :: Int, _column :: Int }