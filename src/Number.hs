module Number (number) where

import           AST
import qualified Control.Monad.Trans.State as St
import           Prelude hiding (id)

type NumberM a = St.State Int a

number :: Program id -> Program Int
number pr = St.evalState (numberPr pr) 0

numberPr :: Program id -> NumberM (Program Int)
numberPr = numberBlock

numberBlock :: Block id -> NumberM (Block Int)
numberBlock = traverse numberSt

numberSt :: Statement id -> NumberM (Statement Int)
numberSt (Assignment var e) = do
  e' <- numberExpr e
  var' <- numberVar var
  pure $ Assignment var' e'
numberSt (Ite e th el) =
  Ite <$> numberExpr e <*> numberBlock th <*> numberBlock el
numberSt (While e body) = While <$> numberExpr e <*> numberBlock body

numberExpr :: Expression id -> NumberM (Expression Int)
numberExpr (EConst i) = pure $ EConst i
numberExpr (EVar var) = EVar <$> numberVar var
numberExpr (EPlus e1 e2) = EPlus <$> numberExpr e1 <*> numberExpr e2

numberVar :: Variable id -> NumberM (Variable Int)
numberVar (Variable _ str) = (`Variable` str) <$> freshId

freshId :: NumberM Int
freshId = do
  id <- St.get
  St.put (id + 1)
  pure id