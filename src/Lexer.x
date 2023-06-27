{
module Lexer (lex, Token(..)) where

import Prelude hiding (lex)
}

%wrapper "basic"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                        ;
  :=                             { \_ -> TAssign }
  if                             { \_ -> TIf }
  else                           { \_ -> TElse }
  while                          { \_ -> TWhile }
  $digit+                        { \s -> TInt (read s) }
  \+                             { \_ -> TPlus }
  \;                             { \_ -> TSemiColon }
  \{                             { \_ -> TLeftCurly }
  \}                             { \_ -> TRightCurly }
  $alpha [$alpha $digit \_ \']*  { \s -> TVar s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token
  = TAssign
  | TIf
  | TElse
  | TWhile
  | TPlus
  | TSemiColon
  | TLeftCurly
  | TRightCurly
  | TVar String
  | TInt Int
  deriving (Eq, Show)

lex = alexScanTokens
}