module Language.Mili.Reducer
  ( nf
  ) where

import           Data.Mili                      ( Term(..) )

-- | Reduce term to normal form
nf :: Term -> Term
nf m = m
