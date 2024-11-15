-- MIT License, Copyright (c) 2024 Marvin Borner

module Data.Mili
  ( Term(..)
  , Nat(..)
  , foldM
  , fold
  , shift
  ) where

import           Data.Functor.Identity          ( runIdentity )
import           Prelude                 hiding ( abs
                                                , min
                                                )

data Nat = Z | S Term

data Term = Abs Int Term                 -- | Abstraction with level
          | App Term Term                -- | Application
          | Lvl Int                      -- | de Bruijn level
          | Num Nat                      -- | Peano numeral
          | Rec Term Term Term Term Term -- | Unbounded iteration

instance Show Nat where
  show Z     = "Z"
  show (S t) = "S(" <> show t <> ")"

instance Show Term where
  showsPrec _ (Abs l m) =
    showString "(\\" . shows l . showString "." . shows m . showString ")"
  showsPrec _ (App m n) =
    showString "(" . shows m . showString " " . shows n . showString ")"
  showsPrec _ (Lvl i) = shows i
  showsPrec _ (Num n) = shows n
  showsPrec _ (Rec t1 t2 u v w) =
    showString "REC ("
      . shows t1
      . showString ", "
      . shows t2
      . showString "), "
      . shows u
      . showString ", "
      . shows v
      . showString ", "
      . shows w

foldM
  :: Monad m
  => (Int -> Term -> m Term)
  -> (Term -> Term -> m Term)
  -> (Int -> m Term)
  -> (Nat -> m Term)
  -> (Term -> Term -> Term -> Term -> Term -> m Term)
  -> Term
  -> m Term
foldM abs app lvl num rec (Abs d m) = foldM abs app lvl num rec m >>= abs d
foldM abs app lvl num rec (App a b) = do
  a' <- foldM abs app lvl num rec a
  b' <- foldM abs app lvl num rec b
  app a' b'
foldM _   _   lvl _   _   (Lvl i          ) = pure i >>= lvl
foldM _   _   _   num _   (Num Z          ) = pure Z >>= num
foldM abs app lvl num rec (Num (S m)) = foldM abs app lvl num rec m >>= num . S
foldM abs app lvl num rec (Rec t1 t2 u v w) = do
  t1' <- foldM abs app lvl num rec t1
  t2' <- foldM abs app lvl num rec t2
  u'  <- foldM abs app lvl num rec u
  v'  <- foldM abs app lvl num rec v
  w'  <- foldM abs app lvl num rec w
  rec t1' t2' u' v' w'

fold
  :: (Int -> Term -> Term)
  -> (Term -> Term -> Term)
  -> (Int -> Term)
  -> (Nat -> Term)
  -> (Term -> Term -> Term -> Term -> Term -> Term)
  -> Term
  -> Term
fold abs app lvl num rec term = runIdentity $ foldM
  (\d m -> pure $ abs d m)
  (\a b -> pure $ app a b)
  (pure . lvl)
  (pure . num)
  (\t1 t2 u v w -> pure $ rec t1 t2 u v w)
  term

shift :: Int -> Term -> Term
shift 0 = id
shift n = fold (\l m -> Abs (l + n) m) App (\l -> Lvl $ l + n) Num Rec
