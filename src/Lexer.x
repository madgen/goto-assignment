{
{-# LANGUAGE RecordWildCards #-}
module Lexer (lex, Token(..)) where

import Prelude hiding (lex)
import Span(Span(..))
}

%wrapper "posn"

$digit = 0-9            -- digits
$alpha = [a-zA-Z]       -- alphabetic characters

tokens :-
  $white+                        ;
  :=                             { convert $ \_ _ -> TAssign }
  if                             { convert $ \_ _ -> TIf }
  else                           { convert $ \_ _ -> TElse }
  while                          { convert $ \_ _ -> TWhile }
  $digit+                        { convert $ \_ s -> TInt (read s) }
  \+                             { convert $ \_ _ -> TPlus }
  \;                             { convert $ \_ _ -> TSemiColon }
  \{                             { convert $ \_ _ -> TLeftCurly }
  \}                             { convert $ \_ _ -> TRightCurly }
  $alpha [$alpha $digit \_ \']*  { convert $ \p s -> TVar p s }

{
data Token
  = TAssign
  | TIf
  | TElse
  | TWhile
  | TPlus
  | TSemiColon
  | TLeftCurly
  | TRightCurly
  | TVar Span String
  | TInt Int
  deriving (Eq, Show)

convert :: (Span -> String -> Token) -> (AlexPosn -> String -> Token)
convert mk apos str = mk (toSpan apos (length str)) str

toSpan :: AlexPosn -> Int -> Span
toSpan (AlexPn _ line col) len =
  Span {_line = line, _column = col, _length = len}

lex = alexScanTokens
}