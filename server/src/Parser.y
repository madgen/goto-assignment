{
module Parser (parse) where

import AST
import Lexer
}
%name parse1
%tokentype { Token }
%error { parseError }
%left '+'

%token
      ':='            { TAssign }
      if              { TIf }
      else            { TElse }
      while           { TWhile }
      '+'             { TPlus }
      ';'             { TSemiColon }
      '{'             { TLeftCurly }
      '}'             { TRightCurly }
      var             { TVar _ _ }
      int             { TInt $$ }
      
%%

Bl      : St                                     { [ $1 ] }
        | Bl St                                  { $2 : $1 }

St      : Var ':=' Exp                           { Assignment $1 $3 }
        | if Exp '{' Bl '}' else '{' Bl '}'      { Ite $2 (reverse $4) (reverse $8) }
        | while Exp '{' Bl '}'                   { While $2 (reverse $4) }

Exp     : int                                    { EConst $1 }
        | Var                                    { EVar $1 }
        | Exp '+' Exp                            { EPlus $1 $3 }
    
Var     : var                                    { let TVar span val = $1 in Variable span val }
{
parseError :: [Token] -> a
parseError _ = error "Parse error"

parse = reverse . parse1
}