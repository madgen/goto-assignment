{-# LANGUAGE ScopedTypeVariables #-}

module GoToDef (goToFirstDef, tabulate) where

import qualified Data.Map.Strict as M
import           AST
import           Prelude hiding (id)

type GoToDef id = M.Map id [ id ]

type Env id = (M.Map String id, GoToDef id)

goToFirstDef :: forall id. Ord id => Program id -> GoToDef id
goToFirstDef pr = snd $ goBlock (M.empty, M.empty) pr
  where
    goBlock :: Env id -> Program id -> Env id
    goBlock = foldl goSt

    goSt :: Env id -> Statement id -> Env id
    goSt env@(lastOcc, _) (Assignment var@(Variable id str) expr) =
      (lastOcc', goToDef')
      where
        lastOcc' = M.insertWith (\_ old -> old) str id lastOcc

        env' = (lastOcc', goExpr env expr)

        goToDef' = goVar env' var
    goSt env@(lastOcc, _) (Ite e th el) =
      (M.union lastOcc' lastOcc'', goToDef'')
      where
        goToDef = goExpr env e

        (lastOcc', goToDef') = foldl goSt (lastOcc, goToDef) th

        (lastOcc'', goToDef'') = foldl goSt (lastOcc, goToDef') el
    goSt env@(lastOcc, _) (While e body) =
      foldl goSt (lastOcc, goExpr env e) body

    goExpr :: Env id -> Expression id -> GoToDef id
    goExpr (_, goToDef) EConst {} = goToDef
    goExpr env (EVar var) = goVar env var
    goExpr env@(lastOcc, _) (EPlus e1 e2) = goExpr (lastOcc, goExpr env e1) e2

    goVar (lastOcc, goToDef) (Variable id str) =
      maybe goToDef (\v -> M.insert id [ v ] goToDef) (M.lookup str lastOcc)
      
tabulate :: GoToDef id -> [(id, [ id ])]
tabulate = M.assocs