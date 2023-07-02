{-# LANGUAGE ScopedTypeVariables #-}

module GoToDef (Mode(..), goToDef, tabulate) where

import qualified Data.Map.Strict as M
import           AST
import           Prelude hiding (id)

type GoToDef id = M.Map id [id]

type LastOcc id = M.Map String [id]

type Env id = (LastOcc id, GoToDef id)

data Mode = First
          | Last
  deriving Read

goToDef :: Ord id => Mode -> Program id -> GoToDef id
goToDef First = goToDefSimple (\_ y -> y)
goToDef Last = goToDefSimple const

goToDefSimple
  :: forall id. Ord id => ([id] -> [id] -> [id]) -> Program id -> GoToDef id
goToDefSimple choose pr = snd $ goBlock (M.empty, M.empty) pr
  where
    goBlock :: Env id -> Program id -> Env id
    goBlock = foldl goSt

    goSt :: Env id -> Statement id -> Env id
    goSt env@(lastDef, _) (Assignment var@(Variable id str) expr) =
      (lastDef', goToDefTbl')
      where
        lastDef' = M.insertWith choose str [id] lastDef

        env' = (lastDef', goExpr env expr)

        goToDefTbl' = goVar env' var
    goSt env@(lastDef, _) (Ite e th el) =
      (M.unionWith choose lastDefEl lastDefTh, goToDefTbl'')
      where
        goToDefTbl = goExpr env e

        (lastDefTh, goToDefTbl') = foldl goSt (lastDef, goToDefTbl) th

        (lastDefEl, goToDefTbl'') = foldl goSt (lastDef, goToDefTbl') el
    goSt env@(lastDef, _) (While e body) =
      foldl goSt (lastDef, goExpr env e) body

    goExpr :: Env id -> Expression id -> GoToDef id
    goExpr (_, goToDefTbl) EConst {} = goToDefTbl
    goExpr env (EVar var) = goVar env var
    goExpr env@(lastDef, _) (EPlus e1 e2) = goExpr (lastDef, goExpr env e1) e2

    goVar :: Env id -> Variable id -> GoToDef id
    goVar (lastDef, goToDefTbl) (Variable id str) =
      maybe goToDefTbl (\v -> M.insert id v goToDefTbl) (M.lookup str lastDef)

tabulate :: GoToDef id -> [(id, [id])]
tabulate = M.assocs