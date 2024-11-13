module Data.Mili
  ( Term(..)
  ) where

data Term = Abs Term      -- | Abstraction
          | App Term Term -- | Application
          | Lvl Int       -- | de Bruijn level

instance Show Term where
  showsPrec _ (Abs m) = showString "[" . shows m . showString "]"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Lvl i) = shows i
