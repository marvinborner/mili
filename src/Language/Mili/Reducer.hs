-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Mili.Reducer
  ( nf
  ) where

import           Data.Mili                      ( Nat(..)
                                                , Term(..)
                                                , shift
                                                )

data Singleton = TermTag Term | RecTag Term Term Term Term
  deriving Show

-- TODO: There will only ever be one substitution, so don't iterate the entire tree!
-- Otherwise replace with fold
subst :: Int -> Term -> Term -> Term
subst l (Abs d m) s = Abs d $ subst l m s
subst l (App a b) s = App (subst l a s) (subst l b s)
subst l (Lvl i) s | l == i    = s
                  | otherwise = (Lvl i)
subst _ (Num Z    ) _ = Num Z
subst l (Num (S m)) s = Num $ S $ subst l m s
subst l (Rec (t1, t2) u v w) s =
  Rec (subst l t1 s, t2) (subst l u s) (subst l v s) (subst l w s)

machine :: Term -> [Singleton] -> (Term, [Singleton])
machine (App a b) s = (a, TermTag b : s)
machine (Abs l u) (TermTag t : s) = (shift (-1) (subst l u t), s)
machine (Rec (t1, t2) u v w) s = (t1, RecTag (Num t2) u v w : s)
machine (Num Z) (RecTag _ u _ _ : s) = (u, s)
machine (Num (S t1)) ((RecTag t2 u v w) : s) =
  (v, RecTag (App (App w t1) t2) u v w : s)
machine t s = error $ show t <> show s

runMachine :: [Singleton] -> Term -> Term
runMachine s t = case machine t s of
  (t', []) -> t'
  (t', s') -> runMachine s' t'

-- | Reduce term to normal form
nf :: Term -> Term
nf = runMachine []
