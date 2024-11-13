module Data.Mili
  ( Term(..)
  , fold
  , shift
  ) where

import           Prelude                 hiding ( abs
                                                , min
                                                )

data Nat = Z | S Nat

data Term = Abs Term          -- | Abstraction
          | App Term Term     -- | Application
          | Lvl Int           -- | de Bruijn level
          | Num Nat           -- | Peano numeral
          | Min Nat Term Term -- | Unbounded minimizer
          | Itr Nat Term Term -- | Bounded iterator

instance Show Nat where
  show Z     = "0"
  show (S n) = show (read (show n) + 1 :: Integer)

instance Show Term where
  showsPrec _ (Abs m) = showString "[" . shows m . showString "]"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Lvl i) = shows i
  showsPrec _ (Num n) = shows n
  showsPrec _ (Min n u f) =
    showString "μ"
      . shows n
      . showString " "
      . shows u
      . showString " "
      . shows f
  showsPrec _ (Itr n u v) =
    showString "iter "
      . shows n
      . showString " "
      . shows u
      . showString " "
      . shows v

fold
  :: (t -> t)
  -> (t -> t -> t)
  -> (Int -> t)
  -> (Nat -> t)
  -> (Nat -> t -> t -> t)
  -> (Nat -> t -> t -> t)
  -> Term
  -> t
fold abs app lvl num min itr (Abs m) = abs (fold abs app lvl num min itr m)
fold abs app lvl num min itr (App a b) =
  app (fold abs app lvl num min itr a) (fold abs app lvl num min itr b)
fold _ _ lvl _   _ _ (Lvl n) = lvl n
fold _ _ _   num _ _ (Num n) = num n
fold abs app lvl num min itr (Min n u f) =
  min n (fold abs app lvl num min itr u) (fold abs app lvl num min itr f)
fold abs app lvl num min itr (Itr n u v) =
  min n (fold abs app lvl num min itr u) (fold abs app lvl num min itr v)

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold Abs App (\l -> Lvl $ l + n) Num Min Itr
