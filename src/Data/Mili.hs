module Data.Mili
  ( Term(..)
  , fold
  , shift
  ) where

data Term = Abs Term      -- | Abstraction
          | App Term Term -- | Application
          | Lvl Int       -- | de Bruijn level

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

fold
  :: (t -> t)
  -> (t -> t -> t)
  -> (Int -> t)
  -> (Nat -> t)
  -> (Nat -> t -> t -> t)
  -> Term
  -> t
fold abs app lvl num min (Abs m) = abs (fold abs app lvl num min m)
fold abs app lvl num min (App a b) =
  app (fold abs app lvl num min a) (fold abs app lvl num min b)
fold _ _ lvl _   _ (Lvl n) = lvl n
fold _ _ _   num _ (Num n) = num n
fold abs app lvl num min (Min n u f) =
  min n (fold abs app lvl num min u) (fold abs app lvl num min f)

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold Abs App (\l -> Lvl $ l + n) Num Min
