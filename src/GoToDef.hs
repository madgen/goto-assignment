{-# LANGUAGE ScopedTypeVariables #-}

module GoToDef (Mode(..), goToDef, tabulate) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           AST
import           Prelude hiding (id)
import           Control.Arrow (second)

type GoToDef id = M.Map id (S.Set id)

type DefsInEffect id = M.Map String (S.Set id)

type Env id = (DefsInEffect id, GoToDef id)

data Mode = First
          | Last
  deriving Read

goToDef :: Ord id => Mode -> Program id -> GoToDef id
goToDef First = goToDefSimple (\_ y -> y)
goToDef Last = goToDefSimple const

goToDefSimple :: forall id.
              Ord id
              => (S.Set id -> S.Set id -> S.Set id)
              -> Program id
              -> GoToDef id
goToDefSimple choose pr = snd $ goBlock (M.empty, M.empty) pr
  where
    goBlock :: Env id -> Program id -> Env id
    goBlock = foldl goSt

    goSt :: Env id -> Statement id -> Env id
    goSt env@(defsInEffect, _) (Assignment var@(Variable id str) expr) =
      (defsInEffect', goToDefTbl')
      where
        defsInEffect' = M.insertWith choose str (S.singleton id) defsInEffect

        env' = (defsInEffect', goExpr env expr)

        goToDefTbl' = goVar env' var
    goSt env@(defsInEffect, _) (Ite e th el) =
      (M.unionWith choose defsInEffectEl defsInEffectTh, goToDefTbl'')
      where
        goToDefTbl = goExpr env e

        (defsInEffectTh, goToDefTbl') = goBlock (defsInEffect, goToDefTbl) th

        (defsInEffectEl, goToDefTbl'') = goBlock (defsInEffect, goToDefTbl') el
    goSt env@(defsInEffect, _) (While e body) =
      goBlock (defsInEffect, goExpr env e) body

    goExpr :: Env id -> Expression id -> GoToDef id
    goExpr (_, goToDefTbl) EConst {} = goToDefTbl
    goExpr env (EVar var) = goVar env var
    goExpr env@(defsInEffect, _) (EPlus e1 e2) =
      goExpr (defsInEffect, goExpr env e1) e2

    goVar :: Env id -> Variable id -> GoToDef id
    goVar (defsInEffect, goToDefTbl) (Variable id str) = maybe
      goToDefTbl
      (\v -> M.insert id v goToDefTbl)
      (M.lookup str defsInEffect)

tabulate :: GoToDef id -> [(id, [id])]
tabulate = map (second S.toAscList) . M.assocs