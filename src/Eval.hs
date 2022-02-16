{-# LANGUAGE ViewPatterns #-}


module Eval where

import           Control.Monad                  ( (<=<)
                                                , when
                                                )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                , runExcept
                                                , runExceptT
                                                )
import           Control.Monad.State.Lazy       ( StateT(runStateT)
                                                , gets
                                                , modify
                                                )
import           Data.Map.Lazy                  ( Map
                                                , insert
                                                , lookup
                                                )
import qualified Data.Map.Lazy                 as Map
import           Lambda.Abs
import           Prelude                 hiding ( lookup )

data LangError = ValueExpected
               | UnboundVariable
               | OpenTerm
               deriving (Show)

type LangM m = StateT (Map String Term) (ExceptT LangError m)

pureShift :: Int -> Term -> Term
pureShift s = shift' s 0
  where
    shift' :: Int -> Int -> Term -> Term
    shift' s c x@(Var (fromIntegral -> x')) | x' < c    = x
                                            | otherwise = Var $ toInteger (x' + s)
    shift' s c Unit                       = Unit
    shift' s c (Named id                ) = Named id
    shift' s c (App te te'              ) = App (shift' s c te) (shift' s c te')
    shift' s c (Lam te                  ) = Lam (shift' s (c + 1) te)
    shift' s c (Mu  te                  ) = Mu (shift' s (c + 1) te)
    shift' s c (Let (BindPair id te') te) = Let (BindPair id (shift' s c te')) (shift' s c te)
    shift' s c (LetDef (BindPair id te) ) = LetDef (BindPair id (shift' s c te))

shift :: Monad m => Int -> Term -> LangM m Term
shift = flip shift' 0
  where
    shift' :: Monad m => Int -> Int -> Term -> LangM m Term
    shift' s c (Var x@(fromIntegral -> x')) | x' < c    = return $ Var x
                                            | otherwise = return $ Var (toInteger (x' + s))
    shift' s c (Lam t      ) = Lam <$> shift' s (c + 1) t
    shift' s c (Mu  t      ) = Mu <$> shift' s (c + 1) t
    shift' s c (App a    b ) = App <$> shift' s c a <*> shift' s c b
    shift' s c (Let bind te) = Let bind <$> shift' s c te
    shift' s c (LetDef bind) = return $ LetDef bind
    shift' s c (Named  id  ) = return $ Named id
    shift' s c Unit          = return Unit

isClosedTerm :: Monad m => Term -> LangM m Bool
isClosedTerm = go 0
  where
    go :: Monad m => Int -> Term -> LangM m Bool
    go c (Var (fromIntegral -> n)) | n < c     = return True
                                   | otherwise = return False
    go c (App te te' ) = (&&) <$> go c te <*> go c te'
    go c (Lam te     ) = go (c + 1) te
    go c (Mu  te     ) = go (c + 1) te
    go c (Let bind te) = go c te
    go c (LetDef bind) = return True
    go c (Named  id  ) = return True
    go c Unit          = return True

-- >>> runExcept (runStateT (shift 1 (Lam (Var 1))) Map.empty)
-- Right (Lam (Var 2),fromList [])

subst :: Monad m => Term -> Term -> LangM m Term
subst a = shift (-1) <=< subst' 0 a <=< shift 1
  where
    subst' :: Monad m => Int -> Term -> Term -> LangM m Term
    subst' i x@(Var (fromIntegral -> x')) b | x' == i   = return b
                                            | otherwise = return x
    subst' i (App a b                 ) c = App <$> subst' i a c <*> subst' i b c
    subst' i (Lam t                   ) b = Lam <$> subst' (i + 1) t b
    subst' i (Mu  t                   ) b = Mu <$> subst' (i + 1) t b
    subst' i (Let (BindPair id te') te) b = Let . BindPair id <$> subst' i te' b <*> subst' i te b
    subst' i (LetDef (BindPair id te) ) b = LetDef . BindPair id <$> subst' i te b
    subst' i (Named  id               ) b = return $ Named id
    subst' i Unit                       b = return Unit

occurs :: String -> Term -> Bool
occurs defname (Var n)           = False
occurs defname Unit              = False
occurs defname (Named (Ident s)) = defname == s
occurs defname (App te te'     ) = occurs defname te || occurs defname te'
occurs defname (Lam te         ) = occurs defname te
occurs defname (Mu  te         ) = occurs defname te
occurs defname (Let (BindPair (Ident s) te') te) | defname == s = False
                                                 | otherwise    = occurs defname te || occurs defname te'
occurs defname (LetDef (BindPair (Ident s) te)) | defname == s = False
                                                | otherwise    = occurs defname te

-- | turn a recursively defined term into a mu term
fixpoint :: String -> Term -> Term
fixpoint defname = go 0 . pureShift 1
  where
    go :: Int -> Term -> Term
    go depth (Var n) = Var n
    go depth Unit    = Unit
    go depth (Named (Ident s)) | defname == s = Var (toInteger depth)
                               | otherwise    = Named (Ident s)
    go depth (App te te') = App (go depth te) (go depth te')
    go depth (Lam te    ) = Lam (go (depth + 1) te)
    go depth (Mu  te    ) = Lam (go (depth + 1) te)
    go depth (Let (BindPair (Ident s) te') te)
        | defname == s = Let (BindPair (Ident s) (if occurs s te' then Mu (fixpoint s te') else te')) te
        | otherwise    = Let (BindPair (Ident s) (if occurs s te' then go depth (Mu (fixpoint s te')) else go depth te')) (go depth te)
    go depth (LetDef (BindPair (Ident s) te'))
        | defname == s = if occurs s te' then LetDef (BindPair (Ident s) (Mu (fixpoint s te'))) else LetDef (BindPair (Ident s) te')
        | otherwise    = if occurs s te' then LetDef (BindPair (Ident s) (go (depth + 1) (fixpoint s te'))) else LetDef (BindPair (Ident s) (go depth te'))

eval :: Monad m => Term -> LangM m Term
eval (Var n     ) = return $ Var n
eval (App te te') = do
    t <- eval te
    case t of
        Lam te2 -> subst te2 te'
        _       -> return $ App t te'
eval (Lam te                          ) = return $ Lam te
eval (Mu  te                          ) = subst te (Mu te)
eval (Let (BindPair (Ident id) te') te) = do
    no <- not <$> isClosedTerm te'
    when no (throwError OpenTerm)
    oldDef <- gets $ lookup id
    modify (insert id (if occurs id te then Mu (fixpoint id te) else te))
    te' <- eval te
    modify $ Map.alter (const oldDef) id
    return te'
eval (LetDef (BindPair (Ident s) te)) = isClosedTerm te >>= \yes -> if yes
    then do
        modify . insert s =<< eval (if occurs s te then Mu (fixpoint s te) else te)
        return Unit
    else throwError OpenTerm
eval (Named (Ident s)) = gets (lookup s) >>= maybe (throwError UnboundVariable) eval
eval Unit              = return Unit

prettyprintTopLevel :: Monad m => Term -> LangM m String
prettyprintTopLevel (Var n)                            = return $ show n
prettyprintTopLevel Unit                               = return "()"
prettyprintTopLevel (Named (Ident id)                ) = gets (lookup id) >>= maybe (throwError UnboundVariable) prettyprintTopLevel
prettyprintTopLevel (App te te'@(Var _)              ) = return $ prettyprint te ++ " " ++ prettyprint te'
prettyprintTopLevel (App te te'@Unit                 ) = return $ prettyprint te ++ " " ++ prettyprint te'
prettyprintTopLevel (App te te'                      ) = return $ prettyprint te ++ " (" ++ prettyprint te' ++ ")"
prettyprintTopLevel (Lam te                          ) = return $ "λ " ++ prettyprint te
prettyprintTopLevel (Mu  te                          ) = return $ "μ " ++ prettyprint te
prettyprintTopLevel (Let (BindPair (Ident id) te') te) = return $ "let " ++ id ++ " = " ++ prettyprint te' ++ " in " ++ prettyprint te
prettyprintTopLevel (LetDef (BindPair (Ident s) te)  ) = return $ "let " ++ s ++ " = " ++ prettyprint te

prettyprint :: Term -> String
prettyprint (Var n)                            = show n
prettyprint Unit                               = "()"
prettyprint (Named (Ident id)                ) = id
prettyprint (App te te'@(Var _)              ) = prettyprint te ++ " " ++ prettyprint te'
prettyprint (App te te'@Unit                 ) = prettyprint te ++ " " ++ prettyprint te'
prettyprint (App te te'@(Named _)            ) = prettyprint te ++ " " ++ prettyprint te'
prettyprint (App te te'                      ) = prettyprint te ++ " (" ++ prettyprint te' ++ ")"
prettyprint (Lam te                          ) = "λ " ++ prettyprint te
prettyprint (Mu  te                          ) = "μ " ++ prettyprint te
prettyprint (Let (BindPair (Ident id) te') te) = "let " ++ id ++ " = " ++ prettyprint te' ++ " in " ++ prettyprint te
prettyprint (LetDef (BindPair (Ident s) te)  ) = "let " ++ s ++ " = " ++ prettyprint te

{-

>>> zero = Lam $ Lam $ Var 0
>>> one = Lam $ Lam $ App (Var 1) (Var 0)
>>> tru = Lam $ Lam $ Var 1
>>> fls = zero
>>> isZero = Lam $ App (App (Var 0) (Lam $ fls)) tru
>>> suc = Lam $ Lam $ Lam $ App (Var 1) (App (App (Var 2) (Var 1)) (Var 0))

>>> suc
Lam (Lam (Lam (App (Var 1) (App (App (Var 2) (Var 1)) (Var 0)))))

>>> eval $ App (App one suc) zero
Lam (Lam (App (Var 1) (App (App (Lam (Lam (Var 0))) (Var 1)) (Var 0))))

>>> eval $ App isZero zero
Lam (Lam (Var 1))

>>> eval $ App isZero one
Lam (Lam (Var 0))

-}

-- >>> runExcept $ runStateT (eval $ Lam (App (Lam (Var 0)) (Var 0))) Map.empty
-- Right (Lam (App (Lam (Var 0)) (Var 0)),fromList [])

-- >>> runExcept $ runStateT (eval $ App (Lam $ Lam $ Var 1) (Var 5)) Map.empty
-- Right (Lam (Var 5),fromList [])

-- >>> runExcept $ runStateT (eval $ Mu (Lam $ App (Var 1) (Var 0))) Map.empty
-- Right (Lam (App (Mu (Lam (App (Var 1) (Var 0)))) (Var 0)),fromList [])

