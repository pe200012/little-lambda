{-# LANGUAGE ViewPatterns #-}


module Eval where

import           Control.Monad                  ( (<=<) )
import           Control.Monad.Except           ( ExceptT
                                                , MonadError(throwError)
                                                , runExceptT
                                                )
import           Control.Monad.Identity         ( Identity(runIdentity) )
import           Control.Monad.State.Lazy       ( StateT(runStateT)
                                                , evalStateT
                                                , gets
                                                , modify
                                                )
import           Data.Map.Lazy                  ( Map
                                                , empty
                                                , insert
                                                , lookup
                                                )
import           Lambda.Abs
import           Prelude                 hiding ( lookup )

data LangError = ValueExpected
               | UnboundVariable String
               | OpenTerm Term
               deriving (Show)

type LangM m = StateT (Map String Term) (ExceptT LangError m)

runLangMT :: Monad m => LangM m a -> m (Either LangError a)
runLangMT = runExceptT . flip evalStateT empty

runLangM :: LangM Identity a -> Either LangError a
runLangM = runIdentity . runExceptT . flip evalStateT empty

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
    go depth (Mu  te    ) = Mu (go (depth + 1) te)
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
        Lam te2 -> eval =<< subst te2 te'
        _       -> return $ App t te'
eval (Lam te                          ) = return $ Lam te
eval (Mu  te                          ) = subst te (Mu te)
eval (Let (BindPair (Ident id) te') te) = replace id te (if occurs id te' then Mu (fixpoint id te') else te')
  where
    replace :: Monad m => String -> Term -> Term -> LangM m Term
    replace name (Var n) b = return $ Var n
    replace name Unit    b = return Unit
    replace name (Named (Ident s)) b | name == s = return b
                                     | otherwise = return $ Named (Ident s)
    replace name (App te2 te3) b = App <$> replace name te2 b <*> replace name te3 b
    replace name (Lam te2    ) b = Lam <$> replace name te2 b
    replace name (Mu  te2    ) b = Mu <$> replace name te2 b
    replace name o@(Let (BindPair (Ident s) te3) te2) b =
        let x = if name == s then return o else Let <$> (BindPair (Ident s) <$> replace name te3 b) <*> pure te2 in eval =<< x

    replace name x@(LetDef (BindPair (Ident s) te2)) b = throwError ValueExpected
eval (LetDef (BindPair (Ident s) te)) = do
    modify (insert s (if occurs s te then Mu (fixpoint s te) else te))
    return Unit
eval (Named (Ident s)) = gets (lookup s) >>= maybe
    (throwError $ UnboundVariable s)
    (\x -> do
        t <- eval x
        modify (insert s t)
        return t
    )
eval Unit = return Unit

eval' :: Monad m => Int -> Term -> LangM m Term
eval' gas x | gas <= 0 = return x
eval' gas (Var n     ) = return $ Var n
eval' gas (App te te') = do
    t <- eval' gas te
    case t of
        Lam te2 -> eval' (gas - 1) =<< subst te2 te'
        _       -> return $ App t te'
eval' gas (Lam te                          ) = Lam <$> eval' (gas - 1) te
eval' gas (Mu  te                          ) = eval' (gas - 1) =<< subst te (Mu te)
eval' gas (Let (BindPair (Ident id) te') te) = eval' gas =<< replace id te (if occurs id te' then Mu (fixpoint id te') else te')
  where
    replace :: Monad m => String -> Term -> Term -> LangM m Term
    replace name (Var n) b = return $ Var n
    replace name Unit    b = return Unit
    replace name (Named (Ident s)) b | name == s = return b
                                     | otherwise = return $ Named (Ident s)
    replace name (App te2 te3) b = App <$> replace name te2 b <*> replace name te3 b
    replace name (Lam te2    ) b = Lam <$> replace name te2 b
    replace name (Mu  te2    ) b = Mu <$> replace name te2 b
    replace name o@(Let (BindPair (Ident s) te3) te2) b =
        let x = if name == s then return o else Let <$> (BindPair (Ident s) <$> replace name te3 b) <*> replace name te2 b in eval' gas =<< x

    replace name o@(LetDef (BindPair (Ident s) te2)) b =
        let x = if name == s then return o else LetDef <$> (BindPair (Ident s) <$> replace name te2 b) in eval' gas =<< x
eval' gas (LetDef (BindPair (Ident s) te)) = do
    tt <- eval' gas (if occurs s te then Mu (fixpoint s te) else te)
    modify (insert s tt)
    return Unit
eval' gas (Named (Ident s)) = gets (lookup s) >>= maybe
    (throwError $ UnboundVariable s)
    (\x -> do
        t <- eval' gas x
        modify (insert s t)
        return t
    )
eval' gas Unit = return Unit

eval'TopLevel :: Monad m => Int -> Term -> LangM m Term
eval'TopLevel gas t = do
    t'  <- eval' gas t
    yes <- isClosedTerm t'
    if yes then return t' else throwError $ OpenTerm t'

prettyprintTopLevel :: Monad m => Term -> LangM m String
prettyprintTopLevel (Var n)            = return $ show n
prettyprintTopLevel Unit               = return "()"
prettyprintTopLevel (Named (Ident id)) = gets (lookup id) >>= maybe (throwError $ UnboundVariable id) prettyprintTopLevel
prettyprintTopLevel (App te te'      ) = do
    let s = case te of
            Named _ -> prettyprint te
            Var   _ -> prettyprint te
            Unit    -> prettyprint te
            App _ _ -> prettyprint te
            _       -> "(" ++ prettyprint te ++ ")"
    let s' = case te' of
            Named _ -> prettyprint te'
            Var   _ -> prettyprint te'
            Unit    -> prettyprint te'
            _       -> "(" ++ prettyprint te' ++ ")"
    return $ s ++ " " ++ s'
prettyprintTopLevel (Lam te                          ) = return $ "λ " ++ prettyprint te
prettyprintTopLevel (Mu  te                          ) = return $ "μ " ++ prettyprint te
prettyprintTopLevel (Let (BindPair (Ident id) te') te) = return $ "let " ++ id ++ " = " ++ prettyprint te' ++ " in " ++ prettyprint te
prettyprintTopLevel (LetDef (BindPair (Ident s) te)  ) = return $ "let " ++ s ++ " = " ++ prettyprint te

prettyprint :: Term -> String
prettyprint (Var n)            = show n
prettyprint Unit               = "()"
prettyprint (Named (Ident id)) = id
prettyprint (App te te') =
    let s = case te of
            Named _ -> prettyprint te
            Var   _ -> prettyprint te
            Unit    -> prettyprint te
            App _ _ -> prettyprint te
            _       -> "(" ++ prettyprint te ++ ")"
        s' = case te' of
            Named _ -> prettyprint te'
            Var   _ -> prettyprint te'
            Unit    -> prettyprint te'
            _       -> "(" ++ prettyprint te' ++ ")"
    in  s ++ " " ++ s'
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

