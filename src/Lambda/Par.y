-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Lambda.Par
  ( happyError
  , myLexer
  , pTerm5
  , pTerm4
  , pTerm3
  , pTerm2
  , pTerm1
  , pTerm
  , pBinding
  ) where

import Prelude

import qualified Lambda.Abs
import Lambda.Lex

}

%name pTerm5 Term5
%name pTerm4 Term4
%name pTerm3 Term3
%name pTerm2 Term2
%name pTerm1 Term1
%name pTerm Term
%name pBinding Binding
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('      { PT _ (TS _ 1) }
  '()'     { PT _ (TS _ 2) }
  ')'      { PT _ (TS _ 3) }
  '='      { PT _ (TS _ 4) }
  'in'     { PT _ (TS _ 5) }
  'let'    { PT _ (TS _ 6) }
  'λ'      { PT _ (TS _ 7) }
  'μ'      { PT _ (TS _ 8) }
  L_Ident  { PT _ (TV $$)  }
  L_integ  { PT _ (TI $$)  }

%%

Ident :: { Lambda.Abs.Ident }
Ident  : L_Ident { Lambda.Abs.Ident $1 }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Term5 :: { Lambda.Abs.Term }
Term5
  : Integer { Lambda.Abs.Var $1 }
  | '()' { Lambda.Abs.Unit }
  | '(' Term ')' { $2 }

Term4 :: { Lambda.Abs.Term }
Term4 : Ident { Lambda.Abs.Named $1 } | Term5 { $1 }

Term3 :: { Lambda.Abs.Term }
Term3 : Term3 Term4 { Lambda.Abs.App $1 $2 } | Term4 { $1 }

Term2 :: { Lambda.Abs.Term }
Term2 : 'λ' Term2 { Lambda.Abs.Lam $2 } | Term3 { $1 }

Term1 :: { Lambda.Abs.Term }
Term1 : 'μ' Term1 { Lambda.Abs.Mu $2 } | Term2 { $1 }

Term :: { Lambda.Abs.Term }
Term
  : 'let' Binding 'in' Term { Lambda.Abs.Let $2 $4 }
  | 'let' Binding { Lambda.Abs.LetDef $2 }
  | Term1 { $1 }

Binding :: { Lambda.Abs.Binding }
Binding : Ident '=' Term { Lambda.Abs.BindPair $1 $3 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}
