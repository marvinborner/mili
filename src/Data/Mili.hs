-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Mili
  ( Term(..)
  , Nat(..)
  , fold
  , shift
  ) where

import           Prelude                 hiding ( abs
                                                , min
                                                )

data Nat = Z | S Term

data Term = Abs Term                       -- | Abstraction
          | App Term Term                  -- | Application
          | Lvl Int                        -- | de Bruijn level
          | Num Nat                        -- | Peano numeral
          | Rec (Term, Nat) Term Term Term -- | Unbounded iteration

instance Show Nat where
  show Z     = "Z"
  show (S t) = "S " <> show t

instance Show Term where
  showsPrec _ (Abs m) = showString "[" . shows m . showString "]"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Lvl i) = shows i
  showsPrec _ (Num n) = shows n
  showsPrec _ (Rec (n, t) u v w) =
    showString "REC ("
      . shows n
      . showString ", "
      . shows t
      . showString "), "
      . shows u
      . showString ", "
      . shows v
      . showString ", "
      . shows w

fold
  :: (Term -> Term)
  -> (Term -> Term -> Term)
  -> (Int -> Term)
  -> (Nat -> Term)
  -> ((Term, Nat) -> Term -> Term -> Term -> Term)
  -> Term
  -> Term
fold abs app lvl num rec (Abs m) = abs $ fold abs app lvl num rec m
fold abs app lvl num rec (App a b) =
  app (fold abs app lvl num rec a) (fold abs app lvl num rec b)
fold _   _   lvl _   _   (Lvl n           ) = lvl n
fold _   _   _   num _   (Num Z           ) = num Z
fold abs app lvl num rec (Num (S t)) = num $ S $ fold abs app lvl num rec t
fold abs app lvl num rec (Rec (t, n) u v w) = rec
  (fold abs app lvl num rec t, n) -- TODO: t'
  (fold abs app lvl num rec u)
  (fold abs app lvl num rec v)
  (fold abs app lvl num rec w)

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold Abs App (\l -> Lvl $ l + n) Num Rec
