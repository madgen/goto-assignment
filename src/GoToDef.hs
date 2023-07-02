{-# LANGUAGE ScopedTypeVariables #-}

module GoToDef (Mode(..), goToDef, tabulate) where

import qualified Data.Map.Strict as M
import           AST
import           Prelude hiding (id)

type GoToDef id = M.Map id [id]

type Env id = (M.Map String id, GoToDef id)

data Mode = First
          | Last
  deriving Read

goToDef :: Ord id => Mode -> Program id -> GoToDef id
goToDef First = goToDefSimple (\_ y -> y)
goToDef Last = goToDefSimple const

goToDefSimple
  :: forall id. Ord id => (id -> id -> id) -> Program id -> GoToDef id
goToDefSimple choose pr = snd $ goBlock (M.empty, M.empty) pr
  where
    goBlock :: Env id -> Program id -> Env id
    goBlock = foldl goSt

    goSt :: Env id -> Statement id -> Env id
    goSt env@(lastOcc, _) (Assignment var@(Variable id str) expr) =
      (lastOcc', goToDefTbl')
      where
        lastOcc' = M.insertWith choose str id lastOcc

        env' = (lastOcc', goExpr env expr)

        goToDefTbl' = goVar env' var
    goSt env@(lastOcc, _) (Ite e th el) =
      (M.unionWith choose lastOccEl lastOccTh, goToDefTbl'')
      where
        goToDefTbl = goExpr env e

        (lastOccTh, goToDefTbl') = foldl goSt (lastOcc, goToDefTbl) th

        (lastOccEl, goToDefTbl'') = foldl goSt (lastOcc, goToDefTbl') el
    goSt env@(lastOcc, _) (While e body) =
      foldl goSt (lastOcc, goExpr env e) body

    goExpr :: Env id -> Expression id -> GoToDef id
    goExpr (_, goToDefTbl) EConst {} = goToDefTbl
    goExpr env (EVar var) = goVar env var
    goExpr env@(lastOcc, _) (EPlus e1 e2) = goExpr (lastOcc, goExpr env e1) e2

    goVar (lastOcc, goToDefTbl) (Variable id str) = maybe
      goToDefTbl
      (\v -> M.insert id [v] goToDefTbl)
      (M.lookup str lastOcc)

tabulate :: GoToDef id -> [(id, [id])]
tabulate = M.assocs