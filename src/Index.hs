module Index (build, lookup) where

import           Prelude hiding (lookup)
import           Data.List (sort)
import           Span (Span, isTargetWithin)
import           Target (Target)
import           AST

newtype Index = Index [Span]

{-| The index relies on the ordering placing the most deeply nested entities
 - first. -}
build :: Program Span -> Index
build = Index . sort . buildBl
  where
    buildBl :: Block Span -> [Span]
    buildBl = concatMap buildSt

    buildSt :: Statement Span -> [Span]
    buildSt (Assignment var rhs) = buildVar var:buildExp rhs
    buildSt (Ite cond th el) = buildExp cond <> buildBl th <> buildBl el
    buildSt (While cond body) = buildExp cond <> buildBl body

    buildVar :: Variable Span -> Span
    buildVar (Variable sp _) = sp

    buildExp :: Expression Span -> [Span]
    buildExp EConst {} = []
    buildExp (EVar var) = [buildVar var]
    buildExp (EPlus e1 e2) = buildExp e1 <> buildExp e2

lookup :: Target -> Index -> Maybe Span
lookup target (Index spans) = go spans
  where
    go [] = Nothing
    go (sp:sps)
      | target `Span.isTargetWithin` sp = Just sp
      | otherwise = go sps